{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
  TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
  FlexibleContexts #-}

import Yesod
import Yesod.Core
import Yesod.WebSockets 
import Conduit 

import Yesod.Form.Jquery
import Data.Text (Text, pack, unpack)
import Text.Julius (rawJS)
import Blaze.ByteString.Builder.ByteString
import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)
import Network.Wai (remoteHost)
import Network.Socket.Internal (SockAddr(..))
import Data.IP 

import Data.Monoid ((<>))

import Control.Monad
import Control.Monad.Loops (whileM_)

import System.IO
import System.Posix.Process 
import System.Exit ( ExitCode(ExitFailure) )
import qualified System.Process as P

import System.Directory
import qualified Control.Concurrent.Async.Lifted as Async

data App = App

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

getNewCode = do
  code <- receiveData
  let inpStr = unpack code
  if (inpStr=="{*--EOF--*}")
    then getNewCode
    else return inpStr

runCode :: String -> WebSocketsT Handler ()
runCode ip = do
  sendTextData ("Waiting for code..." :: Text)
  
  inpStr <- getNewCode

  sendTextData ("{*clear*}" :: Text)

  let cleanIP = filter (`elem` ['0'..'9']) ip
  here <- liftIO $ getCurrentDirectory  
  let tempFolder = here++"/temp"
  let fileName = cleanIP++".hs"

  liftIO $ do
    h <- openFile (tempFolder++"/"++fileName) WriteMode
    hPutStr h inpStr
    hClose h

  let command = "timeout 300 docker run -iv "++tempFolder++":/workspace docker-haskell runhaskell /workspace/"++fileName :: String
  (Just hIn, Just hOut, Just hErr, p) <- 
    liftIO $ P.createProcess (P.shell command){ P.std_out = P.CreatePipe
                                              , P.std_in = P.CreatePipe 
                                              , P.std_err = P.CreatePipe }

  liftIO $ hSetBuffering hOut NoBuffering
  liftIO $ hSetBuffering hErr NoBuffering
  liftIO $ hSetBuffering hIn NoBuffering

  reader <- Async.async (whileM_ (fmap not $ liftIO . hIsEOF $ hOut) $ do
          l <- liftIO $ hGetChar hOut
          sendTextData $ pack (l:[])) 
  errReader <- Async.async (whileM_ (fmap not $ liftIO . hIsEOF $ hErr) $ do
          l <- liftIO $ hGetLine hErr
          sendTextData $ pack ("<p style='color:red'>"++l++"</p>"))
  writer <- Async.async $ myInp hIn  

  Async.wait reader
  Async.wait errReader

  e <- liftIO $ P.waitForProcess p
  case e of 
    ExitFailure n -> do
      sendTextData $ pack ("Program failed with exit code: "++show n++"\n")
    _ -> do 
      return ()
  sendTextData ("  --Done\n" :: Text) 

  Async.wait writer

  liftIO $ removeFile $ "temp/"++cleanIP++".hs"
  runCode ip

myInp :: Handle -> WebSocketsT Handler ()
myInp hIn = do 
  newInp <- receiveData
  let newInpStr = unpack newInp
  case newInpStr of
    "{*--EOF--*}" -> liftIO $ hClose hIn
    _ -> do
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
                editorSource $ defaultMessage ++"\n" ++ (snd . head $ strData) 
              else editorSource defaultMessage

getEditorR :: Handler Html
getEditorR = do
  host <- remoteHost <$> waiRequest
  let ip = show $ sockAddrToIP host
  webSockets $ runCode ip
  defaultLayout $ do
              setTitle $ toHtml pageTitleText
              editorSource defaultMessage

defaultMessage :: String
defaultMessage = "import System.IO\nmain = do\n    hSetBuffering stdout NoBuffering\n    hSetBuffering stderr NoBuffering\n    --Enter Code Here!"

editorSource str = do
  receptacle0 <- newIdent
  receptacle1 <- newIdent
  btn0 <- newIdent 
  [whamlet| $newline never
            <div ##{receptacle1} .outdiv>
              <textarea #input autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false" style=resize:none cols=90 rows=50>#{str}
              <br>
              <button ##{btn0}>Run
            <div #container .console>
              <div #textcontainer .consolet>
                <code #output>
                <div ##{receptacle0}>
              <form #form .down>
                <input #ioinput autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false">
              <button #close disabled .downbtn> Close input
              |]

  toWidget [lucius|
           .outdiv
           {
             float:left;
             display: inline-block;
             width:800px;
           }
           .console
           {
             float:left;
             display: inline-block;
             width:800px;
             height:750px;
             background-color: black;
             color:gray;
             font-size:20px;
             position:relative;
           }
           .consolet
           {
             float:left;
             display: inline-block;
             width:100%;
             height:700px;
             background-color: black;
             color:gray;
             overflow:auto;
             font-size:20px;
             position:relative;
           }
           .down{
            position:absolute;
            bottom:10px;
            left:10px;
          }
          .downbtn{
            position:absolute;
            bottom:10px;
            left:225px;
          }

           html, body { 
             margin:0; 
             padding:0; 
             height:100%; 
           }
         |]

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
                sendEof();
                $('##{rawJS btn0}').prop('disabled', false);
                $('#close').prop('disabled', true);
              }
              if(e.data=="{*clear*}"){
                $('#output').text("");
                $('##{rawJS receptacle0}').text("");
              }
              else{
                if(sent)
                  $('#output').append(e.data.replace("\n", "<br>"));
                else
                  $('##{rawJS receptacle0}').append(e.data.replace("\n", "<br>"));
                var elem = document.getElementById('textcontainer');
                elem.scrollTop = elem.scrollHeight;
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
                $('##{rawJS btn0}').prop('disabled', true);
                $('#close').prop('disabled', false);
              });

            $('#close').on('click', function(){
                if(sent){
                  sendEof();
                }
              });

            function sendEof(){
              console.log('closing stdin');
              conn.send("{*--EOF--*}");
            }
            |]

getHomeR :: Handler Html
getHomeR = do 
  defaultLayout $ do
    setTitle $ toHtml pageTitleText
    homePage

homePage = do
  [whamlet| $newline never
            Welcome to browserHaskell!|]

main = do
    putStrLn "Now running on http://127.0.0.1:3000/editor"
    warp 3000 $ App
