import Control.Concurrent (threadDelay)
import System.IO
main = do
    hSetBuffering stdout NoBuffering
    putStr "."
    threadDelay $ 1*(10^6)
    putStr "."
    putStrLn "."
    l <- getLine
    putStrLn l
    if (reverse l=="exit") then return () else main