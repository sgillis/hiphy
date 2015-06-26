# Hiphy

Haskell giphy.com API client

## Usage

    import Network.Giphy
    import Network.URL
    import Data.Default
    import Data.Maybe

    main :: IO ()
    main = do
        result <- search publicToken (def { search_q = "haskell" })
        url <- return . exportURL . getFirstResultURL . fromJust $ result
        putStrLn url

    > http://media1.giphy.com/media/8SQFi5OjfGRJ6/giphy.gif
