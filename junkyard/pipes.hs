import System.IO
import System.Posix.IO
import System.Posix.Process 
import qualified System.Process as P
import Control.Concurrent (forkIO, killThread)
import Control.Monad
import Control.Monad.Loops

main :: IO ()
main = do
    --h <- fdToHandle stdInput
    (Just hIn, Just hOut, _, p) <-
        P.createProcess (P.shell "./echoes"){ P.std_in = P.CreatePipe
                                            , P.std_out = P.CreatePipe }
    

    --r <- fdToHandle rPipe
    hSetBuffering hIn NoBuffering
    hSetBuffering hOut NoBuffering

    tid0 <- forkIO $ getInput hIn
    tid1 <- forkIO $ getOutput hOut
    e <- P.waitForProcess p
    killThread tid1
    killThread tid0
    print e
    return ()

getInput hIn = do  
    forever $ do 
        l <- getLine
        hPutStrLn hIn $ unwords . map reverse $ words l

getOutput hOut = do  
    forever $ do 
        putStrLn "Getting input"
        hGetContents hOut >>= putStrLn
        --(l, _) <- fdRead hOut 100
        --print l 
