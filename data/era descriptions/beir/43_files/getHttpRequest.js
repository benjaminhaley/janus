

function httpRequest(reqType, url, async, respHandle)
{

if(window.XMLHttpRequest){
	request=new XMLHttpRequest();
	
}
else if (window.ActiveXObject){
	request=new ActiveXObject("Msxml2.XMLHTTP");
	if(!request){
	request=new ActiveXObject("Microsoft.XMLHTTP");
	}
}
if(request){

	if(reqType.toLowerCase() != "post"){
	initReq(reqType, url, async, respHandle);
	}
	else{
	var args=arguments[4];
	if(args != null && args.length > 0)
	initReq(reqType, url, async, respHandle, args);	
	
	}
} else{

alert("unable to make a request");

}
	


}
function initReq(reqType, url, bool, respHandle)
{
try{
	
	request.onreadystatechange=respHandle;
	
	request.open(reqType, url, bool);
	if(reqType.toLowerCase() == "post"){
	request.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
	request.send(arguments[4]);
	}
	else{
	
request.send(null);
	}
} catch(errv){

alert("unable to contact server " + errv);

}
}



