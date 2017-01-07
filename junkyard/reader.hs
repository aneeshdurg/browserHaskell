import Control.Monad
import System.IO
import System.Posix.Files
import Control.Concurrent (forkIO, threadDelay, killThread)
  
pipeString = "/tmp/dld.fifo"

main :: IO ()
main = do
    createNamedPipe pipeString $ unionFileModes ownerReadMode ownerWriteMode
    pipe <- openFile pipeString ReadMode
    tid <- forkIO $ writeStuff "asdf"
    forever (hGetLine pipe >>= putStrLn)

writeStuff :: [Char] -> IO()
writeStuff x = do 
    pipe <- openFile pipeString WriteMode
    forever (getLine >>= (hPutStr pipe) )