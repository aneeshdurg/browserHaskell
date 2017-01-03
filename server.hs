{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Monad (msum)
import Happstack.Server
import Happstack.Server.Types (Response, addHeader)
import qualified Data.Text.Lazy as L
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = do
    putStrLn "Now running on http://127.0.0.1:8000/"
    simpleHTTP nullConf $ handlers

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 10000 10000)

handlers :: ServerPart Response
handlers =
    do decodeBody myPolicy
       msum [ dir "process" $ processRequest 
            , nullDir >> page
            ]

processRequest :: ServerPart Response
processRequest =
    do methodM POST
       code <- look "code"
       let r = toResponse $ reverse newStr
       ok $ addHeader "Access-Control-Allow-Origin" "*" r

page :: ServerPart Response
page = ok $ toResponse $
            H.html $ do
                H.head $ do
                    H.title $ "Haskell Editor"
                H.body $ do
                    svgDoc
                    H.p ! A.id "state" ! A.hidden "true" $ "[5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]"
                    H.br
                    H.button ! A.onclick "fetch()" $ "Step Forward"
                    H.button ! A.onclick "changeSpeed(50)" $ "Speed-"
                    H.button ! A.onclick "changeSpeed(-50)" $ "Speed+"
                    H.button ! A.id "pausebtn" ! A.onclick "togglePause()" $ "Pause"
                    H.br
                    H.button ! A.onclick "changeGrid(-1)" $ "grid-"
                    H.button ! A.onclick "changeGrid(1)" $ "grid+"

svgDoc = S.svg ! SA.version "1.1" ! SA.width "150" ! SA.height "100" $ ""
                   