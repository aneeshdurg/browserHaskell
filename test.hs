import           Data.Text (Text, fun)
import qualified Data.Text as T

main = do 
		 x <- getLine
			 putStr $ show someFunc x
	         putStrLn "done!"

someFunc :: a -> Int
someFunc _ = [0,1,2,3,4,5,6,7,8,9,10]