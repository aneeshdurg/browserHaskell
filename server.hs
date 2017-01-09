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
import System.Exit ( ExitCode(ExitFailure) )
import qualified System.Process as P

data App = App (Chan ServerEvent)

pageTitleText = "Browser Haskell" :: Text

mkYesod "App" [parseRoutes|
/localhost LocalR GET
/editor EditorR GET POST
/ HomeR GET
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

runCode :: WebSocketsT Handler ()
runCode = do
  sendTextData ("Waiting for code..." :: Text)
  
  code <- receiveData
  sendTextData ("{*clear*}" :: Text)
  
  let inpStr = unpack code
  liftIO $ putStr inpStr
  --todo make a function to modify input str (maybe do this with js) such that
  --  Only certain libraries are imported
  --  stdout is not buffered

  --todo make a uniqe file name perclient
  liftIO $ do
    h <- openFile "needsUniqueFile.hs" WriteMode
    hPutStr h inpStr
    hClose h

  (_, Just hOut, _, p) <- 
    liftIO $ P.createProcess (P.shell "timeout 5 runhaskell needsUniqueFile.hs"){ P.std_out = P.CreatePipe }

  liftIO $ hSetBuffering hOut NoBuffering

  --todo, get input from websocket
  --todo display results differently
  --todo catch eof
  race_
        (forever $ do
          l <- liftIO $ hGetChar hOut
          sendTextData $ pack (l:[]))
        (do 
          e <- liftIO $ P.waitForProcess p
          case e of 
            ExitFailure 124 -> do
              sendTextData ("{*clear*}" :: Text)
              sendTextData ("Program timed out! (>5s)\n" :: Text)
            ExitFailure n -> do
              sendTextData ("{*clear*}" :: Text)
              sendTextData $ pack ("Program failed with exit code: "++show n++"\n")
            _ -> sendTextData ("  --Done\n" :: Text)) 

  runCode

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

getHomeR :: Handler Html
getHomeR = do 
  defaultLayout $ do
    setTitle $ toHtml pageTitleText
    homePage

homePage = do
  [whamlet| $newline never
            Welcome to browserHaskell!|]


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
            var isCode = false;
            var url = document.URL;
            var input = document.getElementById('input');

            url = url.replace("http", "ws");
            console.log(url);
            var conn = new WebSocket(url);

            $(window).on('beforeunload', function(){
                conn.close();
            });
            conn.onmessage = function(e) {
              if(e.data=="{*clear*}")
                $('##{rawJS receptacle0}').text("");
              else{
                $('##{rawJS receptacle0}').append(e.data.replace("\n", "<br>"));
              }
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
