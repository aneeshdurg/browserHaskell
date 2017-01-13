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
import Network.Wai (remoteHost)
import Network.Socket.Internal (SockAddr(..))
import Data.IP 

import Data.Monoid ((<>))

import Control.Monad.Trans.Resource (runResourceT, register)
import Control.Monad
import Control.Monad.Loops

import System.IO
import System.Posix.IO
import System.Posix.Process 
import System.Exit ( ExitCode(ExitFailure) )
import qualified System.Process as P

import System.Directory
import qualified Control.Concurrent.Async.Lifted as Async

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

--thanks stackoverflow:
--http://stackoverflow.com/questions/33536054/yesod-get-client-ipv4-ipv6-adress
sockAddrToIP :: SockAddr -> IP
sockAddrToIP sockAddr = case sockAddr of
    SockAddrInet _port hostAddress -> IPv4 $ fromHostAddress hostAddress
    SockAddrInet6 _port _flowInfo hostAddress6 _scopeID -> IPv6 $ fromHostAddress6 hostAddress6

runCode :: String -> WebSocketsT Handler ()
runCode ip = do
  sendTextData ("Waiting for code..." :: Text)
  
  code <- receiveData
  sendTextData ("{*clear*}" :: Text)
  
  let inpStr = unpack code
  let cleanIP = filter (`elem` ['0'..'9']) ip
  liftIO $ putStrLn $ cleanIP
  liftIO $ putStr inpStr
  --todo make a function to modify input str (maybe do this with js) such that
  --  stdout is not buffered

  --todo make a uniqe file name perclient
  liftIO $ do
    h <- openFile ("temp/"++cleanIP++".hs") WriteMode
    hPutStr h inpStr
    hClose h
  --todo programitcally set temp path
  let command = "docker run -iv ~/Desktop/browserHaskell/temp:/workspace docker-haskell runhaskell /workspace/"++cleanIP++".hs" :: String
  (Just hIn, Just hOut, _, p) <- 
    liftIO $ P.createProcess (P.shell command){ P.std_out = P.CreatePipe
                                              , P.std_in = P.CreatePipe }

  liftIO $ hSetBuffering hOut NoBuffering
  liftIO $ hSetBuffering hIn NoBuffering

  --todo support for stderr
  reader <- Async.async (whileM_ (fmap not $ liftIO . hIsEOF $ hOut) $ do
          l <- liftIO $ hGetChar hOut
          sendTextData $ pack (l:[])
          return ()) 
  writer <- Async.async $ myInp hIn  

  Async.wait reader

  e <- liftIO $ P.waitForProcess p
  case e of 
    ExitFailure 124 -> do
      sendTextData ("{*clear*}" :: Text)
      sendTextData ("Program timed out! (>5s)\n" :: Text)
    ExitFailure n -> do
      sendTextData ("{*clear*}" :: Text)
      sendTextData $ pack ("Program failed with exit code: "++show n++"\n")
    _ -> do 
      return ()
  sendTextData ("  --Done\n" :: Text) 

  Async.wait writer

  liftIO $ removeFile $ "temp/"++cleanIP++".hs"
  runCode ip

myInp hIn = do 
  newInp <- receiveData
  let newInpStr = unpack newInp
  case newInpStr of
    "{*--EOF--*}" -> return()
    _ -> do
      liftIO $ putStrLn newInpStr
      liftIO $ hPutStrLn hIn newInpStr
      myInp hIn

postEditorR :: Handler Html
postEditorR = do
  defaultLayout $ do
              setTitle $ toHtml pageTitleText
              (rb,_) <- runRequestBody
              let strData = map (\(x, y) -> (unpack x, unpack y)) rb
              liftIO $ print strData
              if fst (head strData) == "code" then 
                eventSourceW $ snd . head $ strData 
              else eventSourceW defaultMessage

getEditorR :: Handler Html
getEditorR = do
  host <- remoteHost <$> waiRequest
  let ip = show $ sockAddrToIP host
  webSockets $ runCode ip
  defaultLayout $ do
              setTitle $ toHtml pageTitleText
              eventSourceW defaultMessage

defaultMessage :: String
defaultMessage = "import System.IO\nmain = do\n    hSetBuffering stdout NoBuffering\n    --Enter Code Here!"

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
            <form #form>
              <input #ioinput>
            <textarea #input style=resize:none cols=90 rows=50>#{str}
            <br>
            <button ##{btn0}>Run
              |]

  -- the JavaScript ServerEvent handling code
  toWidget [julius|
            var isCode = false;
            var url = document.URL;
            var form = document.getElementById('form');
            var io = document.getElementById('ioinput');
            var input = document.getElementById('input');
            input.value = decodeURIComponent(input.value);

            var sent = false;

            url = url.replace("http", "ws");
            console.log(url);
            var conn = new WebSocket(url);

            $(window).on('beforeunload', function(){
                conn.close();
            });
            conn.onmessage = function(e) {
              if(e.data=="  --Done\n"){
                sent = false;
                conn.send("{*--EOF--*}");
                $('##{rawJS btn0}').prop('disabled', false);
              }
              if(e.data=="{*clear*}")
                $('##{rawJS receptacle0}').text("");
              else{
                $('##{rawJS receptacle0}').append(e.data.replace("\n", "<br>"));
              }
            }
            
            form.addEventListener("submit", function(e){
                e.preventDefault();
                if(sent){ 
                  conn.send(io.value);
                  io.value = '';
                }
            });

            $('##{rawJS btn0}').on('click', function(){
                sent = true;
                conn.send(input.value);
                input.value = "--submitted code\n"+input.value;
                console.log("clicked");
                $('##{rawJS btn0}').prop('disabled', true);
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
