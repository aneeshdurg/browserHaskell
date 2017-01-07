module ParseHaskell where

whiteList :: [String]
whiteList = [ "Control.Monad"
            , "Data.List"
            , "Data.Array" ]
noImports :: String -> Bool
noImports = not . ("import" `elem`) . words