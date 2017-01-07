import Control.Monad
import System.IO
import System.Posix.Files
 
pipeString = "/tmp/dld.fifo"
 
main :: IO ()
main = do
       createNamedPipe pipeString $ unionFileModes ownerReadMode ownerWriteMode
       pipe <- openFile pipeString WriteMode
       forever $ do
            l <- getLine
            hPutStr pipe l