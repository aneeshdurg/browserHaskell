function sendCode(info, append)
{
  var searchstring = encodeURIComponent(info.selectionText);
  fetch(searchstring, append);
}

function fetch(data, append) {
	var a = append ? encodeURIComponent("{**haskAppend=true**}"):"";
	$.ajax({
	  type: "POST",
	  data: "code="+a+data,
	  url: "http://127.0.0.1:5000/editor",
	  success: function(data){
 		chrome.tabs.create({url: "http://127.0.0.1:5000/editor"})
	  	//document.getElementById('status').innerHTML = data;
	  	//fetch();
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
    	sendCode(info);
    }
    if (info.menuItemId === "haskMenu2") {
    	sendCode(info, true);
    }
});