function sendCode(info, append)
{
  var searchstring = encodeURIComponent(info);
  fetch(searchstring, append);
}

var siteUrl = "http://35.160.216.80:3000/editor"
var iter = 0;
function fetch(data, append) {
	//var a = append ? encodeURIComponent("{**haskAppend=true**}"):"";

    //check if the server is running on localhost
    console.log("sending to server");
    var form = $('<form></form>');
    form.attr("method", "post")
    form.attr("action", siteUrl);
    //open new tab
    form.attr("target", "_blank");
    //append data
    var field = $('<input></input>');
    field.attr("type", "hidden");
    field.attr("name", "code");
    field.attr("value", data);
    //submit form
    form.append(field);
    $(form).appendTo("body").submit();
}
//function send(){
//	var text = document.getElementById('text').value;
//	document.getElementById('text').value = "";
//}

chrome.runtime.onInstalled.addListener(function() {
    chrome.contextMenus.create({
        title: 'Haskell RC',
        id: 'haskMenu1', // you'll use this in the handler function to identify this context menu item
        contexts: ['all'],
    });
    setInterval(function(){
        $.ajax({
          type: "GET",
          url: "http://127.0.0.1:3000/localhost",
          success: function(valid){    
            if ( valid == "browserHaskell-localhost" ){ 
                console.log("found local"); 
                siteUrl = "http://127.0.0.1:3000/editor";
            }
          },
        });
    }, 5000);
});

chrome.contextMenus.onClicked.addListener(function(info, tab) {
    if (info.menuItemId === "haskMenu1") {
    	chrome.tabs.sendMessage(tab.id, {method: "getSelection"}, function(response){
            console.log(response);
            sendCode(response.data);
        });
    }
});