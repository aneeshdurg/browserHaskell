{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
  TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
  FlexibleContexts #-}

import Yesod
import Yesod.Core
import Yesod.WebSockets
import Conduit

--import Data.Conduit
import Yesod.Form.Jquery
import Control.Concurrent.Chan (Chan, dupChan, writeChan, newChan)
import Control.Concurrent (forkOS, forkIO, threadDelay, killThread)
import Data.Text (Text, pack, unpack)
import Text.Julius (rawJS)
import Blaze.ByteString.Builder.ByteString
import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Data.Monoid ((<>))

import Control.Monad.Trans.Resource (runResourceT, register)
import Control.Monad
import Control.Monad.Loops

import System.IO
import System.Posix.IO
import System.Posix.Process 
import qualified System.Process as P

data App = App (Chan ServerEvent)

pageTitleText = "Browser Haskell" :: Text

mkYesod "App" [parseRoutes|
/localhost LocalR GET
/recv ReceiveR GET
/editor EditorR GET POST
|]

instance Yesod App where
  defaultLayout widget = do
      appFoundation <- getYesod
      pageContent <- widgetToPageContent $ do
                       widget
                       addScriptEither (urlJqueryJs appFoundation)
      mmsg <- getMessage
      withUrlRenderer [hamlet| $newline never
                       $doctype 5
                       <html>
                         <head>
                           <title>#{pageTitle pageContent}
                           ^{pageHead pageContent}
                         <body>
                           $maybe msg <- mmsg
                            <p .message>#{msg}
                           ^{pageBody pageContent}
                       |]

-- We want a more recent jQuery version than the default.
instance YesodJquery App where
    urlJqueryJs _ = Right $ "http://ajax.googleapis.com/"
                    <>"ajax/libs/jquery/1.8.3/jquery.min.js"

data CodeResponse = CodeResponse 
     { codeStr :: Text
     , append :: Bool
     } deriving Show

--Allows client to check if server is running locally
getLocalR :: Handler TypedContent
getLocalR = do
  addHeader "Access-Control-Allow-Origin" "*"
  return $ TypedContent "text" $ toContent $ 
    ("browserHaskell-localhost" :: [Char])

--todo remove this function, no need for get here
getReceiveR :: Handler TypedContent
getReceiveR = do
  chan0 <- liftIO $ newChan
  tid <- liftIO $ forkOS $ talk chan0 0
  register . liftIO $ do 
    putStrLn "Connection closed by client"
    killThread tid
  sendWaiApplication $ eventSourceAppChan chan0

runCode :: WebSocketsT Handler ()
runCode = do
  --chan0 <- liftIO $ newChan
  sendTextData ("Starting proces..." :: Text)
  
  code <- receiveData
  --todo actually get the right data from the request
  let inpStr = unpack code
  liftIO $ putStr inpStr
  --todo make a function to modify input str such that
  --  Only certain libraries are imported
  --  stdout is not buffered
  --todo make a uniqe file name perclient
  h <- liftIO $ openFile "needsUniqueFile.hs" WriteMode
  liftIO $ hPutStr h inpStr
  liftIO $ hClose h

  (_, Just hOut, _, p) <- 
    liftIO $ P.createProcess (P.shell "echo testing"){ P.std_out = P.CreatePipe }

  liftIO $ hSetBuffering hOut NoBuffering

  --race_ ( do $ e <- P.waitForProcess p ) ( forever $ hGetChar hOut >>= (sendTextData . pack) )
  forever $ do
    l <- liftIO $ hGetChar hOut
    sendTextData $ pack (l:[])

  --tid <- liftIO $ forkIO $ getOutput chan0 hOut
  ----todo extract code from the request
  ----todo use something likes pipes.hs to process code
  ----todo figure out how get input...need to make sure that I can pass this pipe on somwhere
  --register . liftIO $ do 
  --  putStrLn "Connection closed by client"
  --  killThread tid
    --todo clean up process spawned
    --  e <- P.waitForProcess p
    --  p.killprocess p
  --sendWaiApplication $ eventSourceAppChan chan0

getOutput :: Chan ServerEvent -> Handle -> IO ()
getOutput ch hOut= do
  forever $ do
    l <- hGetChar hOut
    writeChan ch $ 
      ServerEvent (Just $ fromText "result") (Just $ fromText "0") [(fromString (l:[]) <> fromString "0")]


postEditorR :: Handler Html
postEditorR = do
  defaultLayout $ do
              setTitle $ toHtml pageTitleText
              (rb,_) <- runRequestBody
              let strData = map (\(x, y) -> (unpack x, unpack y)) rb
              liftIO $ print strData
              if fst (head strData) == "code" then 
                eventSourceW $ snd . head $ strData 
              else eventSourceW ("--Enter Code Here!" :: [Char])

getEditorR :: Handler Html
getEditorR = do
  webSockets runCode
  defaultLayout $ do
              setTitle $ toHtml pageTitleText
              eventSourceW ("--Enter Code Here!" :: [Char])

--todo modify the following html/css/js to make it 
--suitable for the application
onlyEventName :: Text
onlyEventName = "newFib"

eventSourceW str = do
  receptacle0 <- newIdent -- css id for output div 0
  btn0 <- newIdent -- css id for output div 1
  [whamlet| $newline never
            <div ##{receptacle0}>
            <textarea #input>#{str}
            <br>
            <button ##{btn0}>Send Code.|]

  -- the JavaScript ServerEvent handling code
  toWidget [julius|
            var url = document.URL;
            var input = document.getElementById('input');

            url = url.replace("http", "ws");
            console.log(url);
            var conn = new WebSocket(url);

            $(window).on('beforeunload', function(){
                conn.close();
            });
            conn.onmessage = function(e) {
              $('##{rawJS receptacle0}').append(e.data);
            }
            
            $('##{rawJS btn0}').on('click', function(){
                conn.send(input.value);
                input.value = '';
                console.log("clicked");
              });
            |]

--todo get rid of the following 'placeholder' fuctions
fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

talk :: Chan ServerEvent -> Int -> IO ()
talk ch n = do
  forM_ (zip [1..] fibs) $ \fib -> do
    writeChan ch $ mkServerEvent "newFib" (snd fib) $ pack ("Fib"++(show . fst) fib++": ")
    threadDelay micros
    where micros = 1*(10^6)
          mkServerEvent evName evId evData =
              let mEvName = case evName of
                                 "" -> Nothing
                                 _  -> (Just $ fromText evName)
                  mEvId   = Just $ fromString $ show evId
                  evPayload = [(fromText evData <> fromString (show evId))]
              in ServerEvent mEvName mEvId evPayload

--todo figure out if I can usethe global channel for something
main = do
    ch <- newChan
    putStrLn "Now running on http://127.0.0.1:3000/editor"
    warp 3000 $ App ch
