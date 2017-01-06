{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
  TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
  FlexibleContexts #-}

import Yesod
import Yesod.Core
import Data.Conduit
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
/recv ReceiveR GET POST
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
getLocalR = return $ TypedContent "text" $ toContent $ 
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

postReceiveR :: Handler TypedContent
postReceiveR = do
  chan0 <- liftIO $ newChan
  (rb,_) <- runRequestBody
  --todo actually get the right data from the request
  let strData = map (\(x, y) -> (unpack x, unpack y)) rb
  let inpStr = snd . head $ strData
  --todo make a function to modify input str such that
  --  Only certain libraries are imported
  --  stdout is not buffered
  --todo make a uniqe file name perclient
  h <- liftIO $ openFile "needsUniqueFile.hs" WriteMode
  liftIO $ hPutStr h inpStr
  liftIO $ hClose h

  (_, Just hOut, _, p) <- 
    liftIO $ P.createProcess (P.shell "runhaskell needsUniqueFile.hs"){ P.std_out = P.CreatePipe }

  liftIO $ hSetBuffering hOut NoBuffering

  tid <- liftIO $ forkIO $ getOutput chan0 hOut
  --todo extract code from the request
  --todo use something likes pipes.hs to process code
  --todo figure out how get input...need to make sure that I can pass this pipe on somwhere
  register . liftIO $ do 
    putStrLn "Connection closed by client"
    killThread tid
    --todo clean up process spawned
    --  e <- P.waitForProcess p
    --  p.killprocess p
  sendWaiApplication $ eventSourceAppChan chan0

getOutput :: Chan ServerEvent -> Handle -> IO ()
getOutput ch hOut= do
  forever $ do
  l <- hGetContents hOut
  writeChan ch $ 
    ServerEvent (Just $ fromText "result") (Just $ fromText "0") [(fromString l <> fromString "0")]


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
            <div ##{receptacle0}>Results.
            <textarea>#{str}
            <br>
            <button ##{btn0}>Send Code.|]

  -- the JavaScript ServerEvent handling code
  toWidget [julius|
            // setup the EventSource itself
            var source = new EventSource('/recv');
            $('##{rawJS btn0}').on('click', function(){
                console.log("clicked");
              });
            // listener for first type of events
            source.addEventListener(#{toJSON onlyEventName}, function(event)
              {
                $('##{rawJS receptacle0}')
                     .html(makeEventString('Events, type 1', event));
              }, false);

            // listener for unclassified events
            source.onmessage = function(event)
              {

                $('##{rawJS receptacle0}')
                     .html(makeEventString('Events, unclassified', event));
              };

            // just an output helper function
            function makeEventString(str, event)
              {
                return '<strong>' +
                       event.data + ' </strong><br>';
              }
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
