//console.log("here");
chrome.extension.onMessage.addListener(function(request, sender, sendResponse) {
	console.log("got request:"+request.method);
	//credits to:
	//http://stackoverflow.com/questions/2626859/chrome-extension-how-to-capture-selected-text-and-send-to-a-web-service
    if (request.method == "getSelection"){
      console.log("sending"+window.getSelection().toString());
      sendResponse({data: window.getSelection().toString()});
    }
    else
      sendResponse({}); // snub them.
});