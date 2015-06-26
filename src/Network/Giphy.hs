{-# LANGUAGE OverloadedStrings #-}

module Network.Giphy where

import Network.HTTP.Conduit
import Network (withSocketsDo)
import Network.URL
import Control.Monad
import Control.Applicative
import Data.Text
import Data.ByteString.Lazy (ByteString, unpack)
import Data.ByteString.Lazy.Char8
import Data.Maybe (fromJust)
import Data.Aeson
import Data.Default
import Data.Time ( parseTimeOrError
                 , defaultTimeLocale
                 , UTCTime )

instance FromJSON URL where
    parseJSON (String s) = return $ fromJust $ importURL $ Data.Text.unpack s
    parseJSON _          = mzero

type Token = String

data Rating = Y | G | PG | PG13 | R deriving (Eq, Show)

data SearchParameters = SearchParameters
    { search_q :: String
    , search_limit :: Int
    , search_offset :: Int
    , search_rating :: Rating
    } deriving Show

instance Default SearchParameters where
    def = SearchParameters
          { search_q = ""
          , search_limit = 25
          , search_offset = 0
          , search_rating = R
          }

instance FromJSON Rating where
    parseJSON (String s) = case s of
        "y"     -> return Y
        "g"     -> return G
        "pg"    -> return PG
        "pg-13" -> return PG13
        "r"     -> return R
        _       -> mzero
    parseJSON _ = mzero

instance ToJSON Rating where
    toJSON Y = String "y"
    toJSON G = String "g"
    toJSON PG = String "pg"
    toJSON PG13 = String "pg-13"
    toJSON R = String "r"

data Image = Image
    { image_url :: URL
    , width :: Int
    , height :: Int
    , size :: Maybe Int
    , frames :: Maybe Int
    , mp4 :: Maybe URL
    , mp4_size :: Maybe Int
    , webp :: Maybe URL
    , webp_size :: Maybe Int
    } deriving Show

instance FromJSON Image where
    parseJSON (Object v) = Image <$>
                           v .: "url" <*>
                           (read <$> v .: "width") <*>
                           (read <$> v .: "height") <*>
                           (maybeRead <$> v .:? "size") <*>
                           (maybeRead <$> v .:? "frames") <*>
                           v .:? "mp4" <*>
                           (maybeRead <$> v .:? "mp4_size") <*>
                           v .:? "webp" <*>
                           (maybeRead <$> v .:? "webp_size")
    parseJSON _          = mzero

data Images = Images
    { fixed_height :: Image
    , fixed_height_still :: Image
    , fixed_height_downsampled :: Image
    , fixed_width :: Image
    , fixed_width_still :: Image
    , fixed_width_downsampled :: Image
    , fixed_height_small :: Image
    , fixed_height_small_still :: Image
    , fixed_width_small :: Image
    , fixed_width_small_still :: Image
    , downsized :: Image
    , downsized_still :: Image
    , downsized_large :: Image
    , original :: Image
    } deriving Show

instance FromJSON Images where
    parseJSON (Object v) = Images <$>
                           v .: "fixed_height" <*>
                           v .: "fixed_height_still" <*>
                           v .: "fixed_height_downsampled" <*>
                           v .: "fixed_width" <*>
                           v .: "fixed_width_still" <*>
                           v .: "fixed_width_downsampled" <*>
                           v .: "fixed_height_small" <*>
                           v .: "fixed_height_small_still" <*>
                           v .: "fixed_width_small" <*>
                           v .: "fixed_width_small_still" <*>
                           v .: "downsized" <*>
                           v .: "downsized_still" <*>
                           v .: "downsized_large" <*>
                           v .: "original"

data GiphyResult = GiphyResult
    { result_type :: String
    , id :: String
    , result_url :: URL
    , bitly_gif_url :: URL
    , bitly_url :: URL
    , embed_url :: URL
    , username :: String
    , source :: URL
    , rating :: Rating
    , caption :: String
    , content_url :: URL
    , import_datetime :: UTCTime
    , trending_datetime :: UTCTime
    , images :: Images
    } deriving Show

instance FromJSON GiphyResult where
    parseJSON (Object v) = GiphyResult <$>
                           v .: "type" <*>
                           v .: "id" <*>
                           v .: "url" <*>
                           v .: "bitly_gif_url" <*>
                           v .: "bitly_url" <*>
                           v .: "embed_url" <*>
                           v .: "username" <*>
                           v .: "source" <*>
                           v .: "rating" <*>
                           v .: "caption" <*>
                           v .: "content_url" <*>
                           (parseTime <$> v .: "import_datetime") <*>
                           (parseTime <$> v .: "trending_datetime") <*>
                           v .: "images"

data Meta = Meta
    { msg :: String
    , status :: Int
    } deriving Show

instance FromJSON Meta where
    parseJSON (Object v) = Meta <$>
                           v .: "msg" <*>
                           v .: "status"

data Pagination = Pagination
    { total_count :: Int
    , count :: Int
    , offset :: Int
    } deriving Show

instance FromJSON Pagination where
    parseJSON (Object v) = Pagination <$>
                           (v .: "total_count") <*>
                           (v .: "count") <*>
                           (v .: "offset")

data PaginatedResult = PaginatedResult
    { result :: [GiphyResult]
    , meta :: Meta
    , pagination :: Pagination } deriving Show

instance FromJSON PaginatedResult where
    parseJSON (Object v) = PaginatedResult <$>
                           v .: "data" <*>
                           v .: "meta" <*>
                           v .: "pagination"

maybeRead :: Read a => Maybe String -> Maybe a
maybeRead ms = case ms of
    Nothing -> Nothing
    Just s -> Just $ read s

publicToken :: Token
publicToken = "dc6zaTOxFJmzC"

timeFormat :: String
timeFormat = "%Y-%m-%d %H:%M:%S"

getOriginalImageURL :: GiphyResult -> URL
getOriginalImageURL g = image_url $ original $ images g

getFirstGiphy :: PaginatedResult -> GiphyResult
getFirstGiphy pr = Prelude.head . result $ pr

getFirstResultURL :: PaginatedResult -> URL
getFirstResultURL pr = getOriginalImageURL . getFirstGiphy $ pr

parseTime :: String -> UTCTime
parseTime t = parseTimeOrError True defaultTimeLocale timeFormat t

baseURL :: URL
baseURL = fromJust $ importURL "http://api.giphy.com"

addToPath :: String -> URL -> URL
addToPath s url = let path = url_path url
                  in url { url_path = path ++ s}

addParam :: (String, String) -> URL -> URL
addParam = flip add_param

makeRequest :: URL -> IO ByteString
makeRequest url = withSocketsDo $ simpleHttp $ exportURL url

searchURL :: Token -> SearchParameters -> URL
searchURL t params =
    addToPath relativePath $
    addParam ("q", q) $
    addParam ("limit", limit) $
    addParam ("offset", offset) $
    addParam ("rating", rating) $
    addParam ("api_key", t) $
    baseURL
    where relativePath = "/v1/gifs/search"
          q = search_q params
          limit = show $ search_limit params
          offset = show $ search_offset params
          rating = Prelude.filter (/= '"') .
                   Data.ByteString.Lazy.Char8.unpack .
                   encode .
                   search_rating $
                   params

search :: Token -> SearchParameters -> IO (Maybe PaginatedResult)
search t params = do
    res <- makeRequest $ searchURL t params
    return $ decode res
