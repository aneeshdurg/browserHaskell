function sendCode(info, append)
{
  var searchstring = encodeURIComponent(info);
  fetch(searchstring, append);
}

function fetch(data, append) {
	//var a = append ? encodeURIComponent("{**haskAppend=true**}"):"";

    //check if the server is running on localhost
    $.ajax({
	  type: "GET",
	  url: "http://127.0.0.1:3000/localhost",
	  success: function(valid){
 		
        if ( valid == "browserHaskell-localhost" ){  
            //create form
            var form = $('<form></form>');
            form.attr("method", "post")
            form.attr("action", "http://127.0.0.1:3000/editor");
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
	  }
	});
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
});

chrome.runtime.onInstalled.addListener(function() {
    chrome.contextMenus.create({
        title: 'Append to Haskell RC',
        id: 'haskMenu2', // you'll use this in the handler function to identify this context menu item
        contexts: ['all'],
    });
});

chrome.contextMenus.onClicked.addListener(function(info, tab) {
    if (info.menuItemId === "haskMenu1") {
    	chrome.tabs.sendMessage(tab.id, {method: "getSelection"}, function(response){
            sendCode(response.data);
        });
    }
    if (info.menuItemId === "haskMenu2") {
    	sendCode(info, true);
    }
});