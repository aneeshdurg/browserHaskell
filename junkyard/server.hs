{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
  TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
  FlexibleContexts #-}

import Yesod
import Yesod.Core
import Data.Conduit
import Yesod.Form.Jquery
import Control.Concurrent.Chan (Chan, dupChan, writeChan, newChan)
import Control.Concurrent (forkOS, threadDelay, killThread)
import Data.Text (Text, pack)
import Text.Julius (rawJS)
import Blaze.ByteString.Builder.ByteString
import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Data.Monoid ((<>))
import Control.Monad.Trans.Resource (runResourceT, register)
import Control.Monad (forM_)

data App = App (Chan ServerEvent)

pageTitleText = "Browser Haskell" :: Text

mkYesod "App" [parseRoutes|
/localhost LocalR GET
/recv ReceiveR GET 
/setup SetupR GET POST
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

getLocalR :: Handler TypedContent
getLocalR = return $ TypedContent "text" $ toContent $ ("browserHaskell-localhost" :: [Char])

getReceiveR :: Handler TypedContent
getReceiveR = do
  chan0 <- liftIO $ newChan
  tid <- liftIO $ forkOS $ talk chan0 0
  register . liftIO $ do 
    putStrLn "Connection closed by client"
    killThread tid
  sendWaiApplication $ eventSourceAppChan chan0

--postReceiveR :: Handler TypedContent
--postReceiveR = do
--  (rb, _) <- runRequstBody
--  codeStr <- getCodeStrFromBody rb
--  chan0 <- liftIO $ newChan
--  tid <- liftIO $ forkIO $ runCode chan0 codeStr
--  register . liftIO $ killThread tid
--  sendWaiApplication $ eventSourceAppChan chan0

postSetupR :: Handler TypedContent
postSetupR = respondSource typePlain $ do 
  --wr <- waiRequest
  (rb,_) <- runRequestBody
  forM_ rb $ \e -> do
    sendChunkText $ fst $ e
    sendChunkText $ "="
    sendChunkText $ snd $ e
    sendChunkText $ "\n"
  yield Flush
  sendFlush
  --todo put the request into an <input> field

getSetupR :: Handler Html
getSetupR = do
  defaultLayout $ do
              setTitle $ toHtml pageTitleText
              eventSourceW ("test" :: [Char])

onlyEventName :: Text
onlyEventName = "newFib"

eventSourceW str = do
  receptacle0 <- newIdent -- css id for output div 0
  btn0 <- newIdent -- css id for output div 1
  [whamlet| $newline never
            <div ##{receptacle0}>Default text.
            <button ##{btn0}>Click Here.
            <br>
            <textarea>#{str}|]

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

main = do
    ch <- newChan
    putStrLn "http://127.0.0.1:3000/setup"
    warp 3000 $ App ch
