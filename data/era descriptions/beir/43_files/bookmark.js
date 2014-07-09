var note="";

function saveNote(r,p){
var request;
note="";
//str = str.replace(/\n/g, '<br />');


var n=document.getElementById("note").value.replace(/\n/g, '<br>');

var url="/napsave.php?type=note&action=add&r="+ r + "&p=" + p +"&n=" + n;

note=n;

httpRequest("get", url, true, processNote);

}


function processNote(){
if (request.readyState == 4 &&request.status == 200 ) {

doc=request.responseText;

var func = new Function("return "+doc);
var objt=func();

//error
if (objt.status == "1"){
alert("Note has been saved!");

var nd="note_"+objt.record_id + "-" + objt.page;
//alert(nd);
//document.getElementById("note_"+objt.record_id + "-" + objt.page).innerHTML=note;
parent.parent.window.location.reload();
parent.parent.GB_hide();  
}
else{
alert("error" + objt.error);
}

}

}

function deleteNote(r,p){
var request;

var url="/napsave.php?type=note&r="+ r + "&p=" + p +"&action=delete";

httpRequest("get", url, true, processDeleteNote);

}


function processDeleteNote(){
if (request.readyState == 4 &&request.status == 200 ) {

doc=request.responseText;

var func = new Function("return "+doc);
var objt=func();

//error
if (objt.status == "1"){
alert("Note has been deleted!");
parent.parent.window.location.reload(true);
parent.parent.GB_hide();

}
else{
alert("error" + objt.error);
}

}

}


function deleteBookmark(r,p){
var request;

var url="/napsave.php?type=bookmark&r="+ r + "&p=" + p +"&action=delete";

httpRequest("get", url, true, processDeleteBookmark);

}


function processDeleteBookmark(){
if (request.readyState == 4 &&request.status == 200 ) {

doc=request.responseText;

var func = new Function("return "+doc);
var objt=func();

//error
if (objt.status == "1"){
alert("Page " + objt.page + " bookmark has been deleted");
var id=objt.record_id + "-" + objt.page;
var j=document.getElementById(id);


j.parentNode.removeChild(j);

document.location.reload(true);

}
else{
alert("error" + objt.error);
}

}

}

/*
 * add a bookmark
 */

function addBookmark(r,p,c){
var request;
var n;
//n=document.getElementById("note").value;
var n=document.getElementById("note").value.replace(/\n/g, '<br>');

var url="/napsave.php?type=bookmark&action=add&r=" + r + "&p=" + p + "&c=" + c + "&n=" + n;
//alert(r + " " + p + " " + c);
httpRequest("get", url, true, processBookmark);

}



function processBookmark(){
if (request.readyState == 4 &&request.status == 200 ) {

doc=request.responseText;

var func = new Function("return "+doc);
var objt=func();

//error
if (objt.status == "1"){
var msg="Page " + objt.page;

if(objt.title != 0){
  msg += " of " + objt.title;
}
  msg +=" has been bookmarked!";
alert(msg);

//parent.parent.window.location.reload(true);
parent.parent.GB_hide();
disableBookmarkedLink();
var openWindow = new GB_Window("login.php");
       openWindow.show('#'); 

//document.getElementById("addshelf").style.visibility="hidden";
//window.location.assign('/catalog.php?record_id=$record_id');
}
else{
alert("error");
}

}

}
function disableBookmarkedLink() {
  $('li.bookmark', parent.parent.window.document).html('Bookmarked');
  $('li.bookmark', parent.parent.window.document).toggleClass('bookmark bookmark_off');
}

