var GT = {};
var globalOpenWindowMessage = "";
if (!GT.Page) { GT.Page = {}; }
if (!GT.Validation) { GT.Validation = new Object(); }

GT.Validation.email = function(email) {
    var pattern = new RegExp("^[a-zA-Z][\\w\\.-]*[a-zA-Z0-9]@[a-zA-Z0-9][\\w\\.-]*[a-zA-Z0-9]\\.[a-zA-Z][a-zA-Z\\.]*[a-zA-Z]$");
    return email.match(pattern);
};      

function noneBlock() {
	a_arguments = noneBlock.arguments;

	for (var i = 0; i < a_arguments.length ; i++) {
		a_nB = a_arguments[i].split("!");
		s_nBit = a_nB[0];
		i_nBdisplay = a_nB[1];
		o_showHide = document.getElementById(s_nBit);
		if (o_showHide) {
			o_showHide.style.display = (i_nBdisplay==0) ? "none" : (i_nBdisplay==1) ? "block" : "inline";
		}
	}
}

function showHide() {
	for (i = 0; i < showHide.arguments.length; i++) {
		o_showHide = document.getElementById(showHide.arguments[i]);
		s_showHide = (o_showHide.style.display!="") ? o_showHide.style.display : "none";
		o_showHide.style.display = (s_showHide=="none") ? (o_showHide.nodeName=="SPAN") ? "inline" : "block" : "none";
	}
}

function showHideDiv(showHide) {
	for(i = 0; i < showHideDiv.arguments.length; i++) {
		o_showHide = document.getElementById(showHideDiv.arguments[i]);
		s_showHide = (o_showHide.style.display!="") ? o_showHide.style.display : "none";
		o_showHide.style.display = (s_showHide=="none") ? (o_showHide.nodeName=="SPAN") ? "inline" : "block" : "none";
	}
}

function cssSwitch() {
	a_cS = cssSwitch.arguments;
	for (i = 0; i < a_cS.length; i++) {
		a_cSsplit = (a_cS[i].indexOf("&") != -1) ? a_cS[i].split("&") : a_cS[i].split("!");
		if (document.getElementById(a_cSsplit[0])) {
			document.getElementById(a_cSsplit[0]).className = a_cSsplit[1];
		}
	}
}

GT.openNewWindow = function(args) {	
	var url = args.url || '';
	var windowName = args.windowName || '';
	var windowAttributes = args.windowAttributes + ",location=1" || 'location=1';
	var askPermission = args.askPermission || true;
	var replaceInHistory = args.replaceInHistory || false;
	
	if (!url) throw ("Missing url parameter");
	
	if (!askPermission || confirm(globalOpenWindowMessage)) {
		return window.open(url, windowName, windowAttributes, replaceInHistory);
	}
};

function openWindow(url, name, specs, replace, bypassConfirm) {
	specs = specs ? specs + ",location=1" : "location=1";  // Show address bar for security reasons
	if (!url) throw ("Missing url parameter");
	if (bypassConfirm || confirm (globalOpenWindowMessage)) {
		return window.open(url, name, specs, replace);
	}
}

function openPopup(url, name, specs) {
	return openWindow(url, name, specs, false, true);
}

function confirmNewWindow() {
	return confirm(globalOpenWindowMessage);
}

function helpPopup(url) {
	var helpWindow = openPopup(url,"helpWindow","width=625,height=550,toolbar=0,resizable=1,menubar=0,status=0,scrollbars=1");
	if(helpWindow.focus) {
		helpWindow.focus();
	}
}

function showContinueScreen(url) {
	openPopup(url, "_blank", "toolbar=1,status=1,menubar=1,scrollbars=1,resizable=1");
}

function AirportCityLookup(fieldName, url) {
	formField1 = fieldName;
	lookupAirportCityWindow = openPopup(url, "Frame", "width=655,height=400,status,scrollbars,resizable,screenX=20,screenY=40,left=20,top=40");
}

function openTPopup(url) {
	openPopup(url, 'policy', 'width=400,height=400,toolbar=1,resizable=1,menubar=0,status=1,scrollbars=1');
}

function updateStateList(countryCode,searchType) {
  if(countryCode != 'CA' && countryCode != 'US') {
    document.forms[0].elements[searchType + 'State'].selectedIndex = 0;
    document.forms[0].elements[searchType + 'State'].disabled = true;
  }
  else {
    document.forms[0].elements[searchType + 'State'].disabled = false;
  }
}

var globalTargetId;
var globalLinkObj;
var lastClickedInlinePopup;

GT.Page.popups = [];

GT.Page.closeAllInlinePopups = function closeAllInlinePopups() {
  if (GT.Page.popups) {
  	var popupLength = GT.Page.popups.length;
    for(var i = 0; i < popupLength; i++) {
      var popup = GT.Page.popups[i];
      if (popup.close) {
        popup.close.fire();
        lastClickedInlinePopup = "";
      }
    }
    GT.Page.popups = [];
  }
  ModalDialog.hide();
};

function inlinePopup(popup, target, clicked, params)
{
	popup = YAHOO.util.Dom.get(popup);
	popup.style.position = 'absolute';
	popup.style.display = 'block';
	targetReg = YAHOO.util.Dom.getRegion(target);
	var x = targetReg.left;
	var y = targetReg.bottom;

	if(typeof params != "undefined") {
		if(params.offsetX) x += params.offsetX;
		if(params.offsetY) y += params.offsetY; 
	} 

	YAHOO.util.Dom.setXY(popup, [x,y]);
	
	// Ensure popup isn't off screen right
	var popupReg = YAHOO.util.Dom.getRegion(popup);
	var edgeRight = YAHOO.util.Dom.getClientWidth()-20;

	if (popupReg.right > edgeRight)
	{
		var newLeft = targetReg.left-(popupReg.right-edgeRight);
		if (newLeft < 0) {newLeft=0;}  // Don't go too far
		YAHOO.util.Dom.setXY(popup, [newLeft,targetReg.bottom]);
	}
	
	var focus_link = new YAHOO.util.Element(target);
	var popup_container = new YAHOO.util.Element(popup);
	popup_container.close = new YAHOO.util.CustomEvent('close', popup_container);
	var popup_args = {
		'popup' : popup_container.get('id'),
		'focus_return' : focus_link.get('id')
	};

	popup_container.close.subscribe( function(e, args, options) {
		closeInlinePopup(options.popup, options.focus_return);
	}, popup_args );
  
  
	var close_links = YAHOO.util.Dom.getElementsBy(function(child) {
		return YAHOO.util.Dom.hasClass(child, 'close');
	}, 'a', popup);
	var close_link = close_links[0];
	var close_link_args = {
		'popup' : popup_container
	};

	if (close_link) {
		close_link.onclick = "";
		YAHOO.util.Event.addListener(close_link, 'click', function(e, args) {
			if (e.preventDefault) e.preventDefault();
			args.popup.close.fire();
			return false;
		}, close_link_args );
	}
	
	if (clicked) {
		if (lastClickedInlinePopup && (lastClickedInlinePopup.get('id') != popup_container.get('id'))) {
			lastClickedInlinePopup.close.fire();
		}
		lastClickedInlinePopup = popup_container;
	}
	
	GT.Page.popups.push(popup_container);
	
	// Focus on first link (for accessibility)
	var a = popup.getElementsByTagName('A');
	if (a.length > 0) a[0].focus();
}

function closeInlinePopup(popup, link) {
	popup = YAHOO.util.Dom.get(popup);
	if (popup && popup.style) popup.style.display = 'none';
	link = YAHOO.util.Dom.get(link);
	if (link && link.focus) link.focus();
}

function getRadioValue(radioObj) {
	for (var i = 0; i < radioObj.length; i++) {
		if (radioObj[i].checked) {
			radioVal = radioObj[i].value;
			break;
		}
	}
	return radioVal;
}

appendToOnload(function() {
  popupEscListener = new YAHOO.util.KeyListener(document, { 'keys':27 },
    { 'fn':GT.Page.closeAllInlinePopups,
      'scope':document,
      'correctScope':false } );
  popupEscListener.enable();
});

function appendToOnload(func) {
	var oldLoad = window.onload;
	window.onload = function() {
		try { if (oldLoad) oldLoad(); }
		catch(e) {}
		try { func(); }
		catch(e) {}
	};
}


function printMapsDialog(mapId, contentId) {
	maps[mapId].showStaticMap();
	printModalDialog(contentId);
	maps[mapId].hideStaticMap();
}

function setZoomStateImg(mapId) {
	if (maps && maps[mapId]) {
		var zoomObj = document.getElementById('gtZoom' + mapId).getElementsByTagName('A');
		for (var i = 17; i >= 1; i--) {
			if ((17 - maps[mapId].getZoomLevel()) == i) {
				YAHOO.util.Dom.addClass(zoomObj[i], 'selected');
			}
			else {
				YAHOO.util.Dom.removeClass(zoomObj[i], 'selected');
			}
		}
	}
}

// Warn users if they haven't saved their changes
var fieldChanged = false;  // Global variable if a field changed.
function initChangeWarning() {
	var selects = document.getElementsByTagName('SELECT');
	for (var i = 0; i < selects.length; i++) {
		if (!selects[i].onchange) {
			selects[i].onchange = function() {
				fieldChanged = true;
			};
		}
	}
	
	var inputs = document.getElementsByTagName('INPUT');
	for (var j = 0; j < inputs.length; j++) {
		if (!inputs[j].onchange) {
			inputs[j].onchange = function() {
				fieldChanged = true;
			};
		}
	}
	
	var tas = document.getElementsByTagName('TEXTAREA');
	for (var k = 0; k < tas.length; k++) {
		if (!tas[k].onchange) {
			tas[k].onchange = function() {
				fieldChanged = true;
			};
		}
	}
}

// sp: this can be used on pages where a button click results in a call to 'location.href = http://somewhere'.
// in the case that the user keeps pressing buttons the location.href call gets in a pickle and unsavoury things happen
// on the server - on the hotel booking page, for example, the server replies with a message stating that the user
// cannot have more than one session on the booking engine at any one time.  if you use this setLocation function
// instead of a straight location.href call you can prevent that sort of thing from happening.
var locationChanged = false;
function setLocation(loc) {
	if (!locationChanged) {
		// kick off the timeout.  if this fires and the page is still loaded we clear the locationChanged
		// variable.  so if a user presses stop, and then changes their mind, the page load will
		// still occur.
		window.setTimeout(function() {
			locationChanged = false;
		}, 2000);
		
		location.href = loc;
	}
	locationChanged = true;
}

// Opens the Limo Integration popup
// Expects a URL and a vendor to determine size
function openLimoIntegrationPopup(limoUrl, vendor) {
	var params = 'status=no,scrollbars=yes,toolbar=no,menubar=no';
	switch (vendor) {
		case 'groundrez':
		  params += ',height=600,width=260'; 
		  break;
		case 'gt3':
		  params += ',resizable=yes';
		  break;
	}
	
	var newWindowArgs = {
		url : limoUrl,
		windowName : 'LimoIntegration',
		windowAttributes : params
	};
	
	GT.openNewWindow(newWindowArgs);
}

// Check for empty right column on home/login pages.  If empty, change
// the CSS for color and height of 'infoPanels' div to match 'userPanels'
function infoPanelsEmpty() {
	var ip = document.getElementById('infoPanels');
	if (ip) {
		if (ip.getElementsByTagName('DIV').length == 0) {
			ip.className=ip.className+' infoPanelEmpty';
			ip.style.height = document.getElementById('userPanels').getHeight() + 'px';
		}
	}
}

// Function to create paging
function changePage(sortBy, page)	{
	var s = '';
	jQuery('.totalResultCount').html(totalOptionCount);
	for (i = 0; i < pageGroupingCount; i++) {
		var startInt = (i * displayCount) + 1;
		var endInt = (i * displayCount) + displayCount;
		if (endInt > totalOptionCount) {
			endInt = totalOptionCount;
		}
		if (startInt != endInt) {
			if (i == page) {
				s += "<strong>" + startInt + "-" + endInt + "</strong>";
			}
			else {
				s += "<a href=\"javascript:doAvailPageAndSort(";
				if (sortBy && sortBy != "") {
					s += "'" + sortBy + "', ";
				}
				else {
					s += "'', ";
				}
				s += i + ")\">" + startInt + "-" + endInt + "</a>";
			}
			if (i != (pageGroupingCount - 1) && endInt != totalOptionCount) {
				s += "<span class='paging-separator'>|</span>"; 
			}
		}
		else {
			if (i == page) {
				s += "<strong>" + startInt + "</strong>";
			}
			else {
				s += "<a href=\"javascript:doAvailPageAndSort('', " + i + ")\">" + startInt + "</a>";
			}
		}
	}
	jQuery('#paging').doOnce(function(){
		jQuery(this).children().not(':first-child').remove();				
		jQuery(this).append(s);
	});
	jQuery('#pagingBtm').doOnce(function(){
		jQuery(this).children().not(':first-child').remove();				
		jQuery(this).append(s);
	});
}

function doAjaxPageAndSort(url, htmlTarget, errorTarget, options) {
   var targetContentBlock = document.getElementById(htmlTarget);

   var errorContentBlock;
   if (errorTarget && errorTarget != "") {
      errorContentBlock = document.getElementByEd(errorTarget);
   }
   else {
      errorContentBlock = targetContentBlock;
   }

   jQuery.ajax({
      type: "POST",
      url: url,
      beforeSend: function() {
         PageActivityIndicator.show(options);
      },
      success: function(data, status, request) {
    	  data = trimWhitespaceFromHTMLTable(data);
    	  jQuery(targetContentBlock).html(data);
      },
      failure: function(data, status, request) {
         alert('Unable to process your request');
         jQuery(errorContentBlock).html(data);
      },
      complete: function() {
         PageActivityIndicator.hide();
      },
      dataType: "HTML"
   });
   if (document.getElementById('pagingTopTarget')) document.getElementById('pagingTopTarget').focus();
}

function trimWhitespaceFromHTMLTable(data) {
	if (jQuery.browser.msie && jQuery.browser.version === '9.0')
	{
		data = data.replace(/>\s+(?=<\/?t[hard])/gm,'>'); //remove whitespaces between th, table, tr and td elements that causes random gaps in table cells in IE9 - GT-22710
	}
	return data;
}

function showTicketInfo(rowObject, index, numberOfTickets) {
	var elementToShow = 'ticketDetailsTableContainer' + index;
	for (var x = 0; x < numberOfTickets; x++) {
		document.getElementById('ticketDetailsTableContainer' + x).style.display = 'none';
	}
	document.getElementById(elementToShow).style.display = 'block';
	if (rowObject) {
		parentNode = YAHOO.util.Dom.getAncestorByTagName(rowObject, 'tbody');
		for (var y = 0; y < YAHOO.util.Dom.getChildren(parentNode).size(); y++) {
			YAHOO.util.Dom.removeClass((YAHOO.util.Dom.getChildren(parentNode))[y], 'selected');
		}
	}
}

var PageActivityIndicator = {};		//displays and activity indicator that centers to the user window, without covering any controls
var ActivityIndicator = {};
/*==================================================================================================*/
PageActivityIndicator.messageText	= null;
PageActivityIndicator.imageFileUrl	= null;
PageActivityIndicator.modal 		= false;

PageActivityIndicator.init = function(options) {
  if (options) {
    PageActivityIndicator.imageFileUrl	= options.imageFileUrl || GT.ACTIVITY_INDICATOR;
    PageActivityIndicator.messageText	= options.messageText || GT.LOADING_CONTENT;
    PageActivityIndicator.modal			= options.modal;
  }
  else {
    PageActivityIndicator.messageText	= null;
    PageActivityIndicator.imageFileUrl	= null;
    PageActivityIndicator.modal			= false;
  }
};

PageActivityIndicator.show = function(options) {
	if (options) {
		PageActivityIndicator.init(options);
	}
	var dialogContents = "";
	if (PageActivityIndicator.imageFileUrl) {
		dialogContents += "<img src=\"" + PageActivityIndicator.imageFileUrl + "\" border='0' alt=''>";
	}
	if (PageActivityIndicator.messageText) {
		dialogContents += "<span class='activityText'>" + PageActivityIndicator.messageText + "</span>";
	}
	var indicatorOptions = {
		fixedcenter : true,
		zindex : 3000,
		visible : false,
		width : '300px',
		close : false,
		modal : PageActivityIndicator.modal
	};
	
  ActivityIndicator = new YAHOO.widget.Panel("activityIndicator", indicatorOptions );
	ActivityIndicator.setBody(dialogContents);
	ActivityIndicator.render(document.body);
	ActivityIndicator.show();
};

PageActivityIndicator.hide = function(callback) {
	if (ActivityIndicator) {
		ActivityIndicator.hide();
		ActivityIndicator.destroy();
	}
};

var displayTSADialog = function() {
	ModalDialog.show('tsaPrivacyDialog', {height : '160px', width : '500px'});
};

var showOutOfPolicyPopN = function(linkObj, oopV) {
	document.getElementById("oopText").innerHTML = document.getElementById(oopV).innerHTML;
	inlinePopup('outOfPolicyPopN', linkObj);
};

var showOutOfPolicyPopNMO = function(linkObj, oopV) {
	document.getElementById("oopMOText").innerHTML = document.getElementById(oopV).innerHTML;
	timer = setTimeout(function() {
	  inlinePopup('outOfPolicyPopNMO', linkObj);
	}, 500); 
};

function closeInlinePopupMO(popup, linkObj) {
	timer = setTimeout(function() {
	  closeInlinePopup('outOfPolicyPopNMO', linkObj);
	}, 500); 
}

//START mouseover popup on delay timer
var c = 0;
var t;
function timedCount(linkObj, targetId) {
	if (linkObj) {
		globalLinkObj = linkObj;
	}
	else {
		linkObj = globalLinkObj;
	}
	
	if (targetId) {
		globalTargetId = targetId;
	}
	else {
		targetId = globalTargetId;
	}
	
	if (c >= 3) {
		inlinePopup(targetId, linkObj, false);
		return;
	}
	
	c = c + 1;
	t = setTimeout("timedCount()", 100);
}

function stopCount() {
	clearTimeout(t);
	c = 0;
	closeInlinePopup(globalTargetId, globalLinkObj);
}
//END mouseover popup on delay timer

var showInlinePopupMO = function(linkObj, targetId) {
	timer = setTimeout(function() {
	  inlinePopup(targetId, linkObj);
	}, 500);
};

function hideInlinePopupMO(linkObj, targetId)
{
	timer = setTimeout(function() {
	  closeInlinePopup(targetId, linkObj);
	}, 500); 
}

/**
 * @function: stringFormat is used to tokenize a string with params.
 * @param: tokenizedString {String} a String with 0 or more {X}. 
 * @param: params {Object} one param per each {X} in tokenizedString. 
 * @use: stringFormat('Hello {0} !', 'stranger'); // returns 'Hello stranger !'
 */
var stringFormat = function() {
	var txt = arguments[0];
	for (var i = 1; i < arguments.length; i++) {
		var exp = new RegExp('\\{' + (i-1) + '\\}','gm');
		txt = txt.replace(exp,arguments[i]);
	}
	return txt;
};

function displayCustomNoteMD(customNoteId, returnFocus) {
	document.getElementById("customNoteText").innerHTML = document.getElementById(customNoteId).innerHTML;
	ModalDialog.show('customNoteMD', {height : '400px', width : '600px', returnToLink : returnFocus});
}

function checkCharCount(elemObj, maxLength) {
	if (elemObj.value.length > maxLength) {
		elemObj.value = elemObj.value.substring(0, maxLength);
	}
}

function serverCaller(urlBuilder, ajaxCommand)
{
    this.execute = function()
    {
        var url = urlBuilder.build();
        ajaxCommand.executeCall(url);
    }
}

function loadingAjaxCommand(tag)
{
    this.executeCall = function(url)
    {
        if (jQuery(tag).length > 0)
        {
            PageActivityIndicator.show({modal:false, underlay: "none"});

            jQuery(tag).load(url, function(response, status, xhr)
            {
                setTimeout("PageActivityIndicator.hide();", 300);
            });
        }
    }
}

function blockingAjaxCommand(tag)
{
    this.executeCall = function(url){
        jQuery(tag).load(url, {async : false});
    }
}


GT.autocomplete = function() {
	var sidValue = "";
	
    function doHighlight(text, searchTerm, highlightStartTag, highlightEndTag) 
    {
      var newText = "";
      var i = -1;
      var lcSearchTerm = searchTerm.toLowerCase();
      var lcText = text.toLowerCase();
        
      while (text.length > 0) {
        i = lcText.indexOf(lcSearchTerm, i+1);
        if (i < 0) {
          newText += text;
          text = "";
        } else {
          // skip anything inside an HTML tag
          if (text.lastIndexOf(">", i) >= text.lastIndexOf("<", i)) {
              newText += text.substring(0, i) + highlightStartTag + text.substr(i, searchTerm.length) + highlightEndTag;
              text = text.substr(i + searchTerm.length);
              lcText = text.toLowerCase();
              i = -1;
         }
        }
      }
      
      return newText;
    }
    
    function highlightSearchTerms(text, searchText, highlightStartTag, highlightEndTag)
    {
        var searchArray = searchText.replace(/^\s*/, "").replace(/\s*$/, "").split(" ");
          
        for (var i = 0; i < searchArray.length; i++) {
           toSearch = searchArray[i].replace(/,/g,"");
           if (toSearch != "")
              text = doHighlight(text, toSearch, highlightStartTag, highlightEndTag);
        }
        return text;
    }
    
    function createAutoCompleteDataSource()
    {
        var myDataSource = new YAHOO.util.XHRDataSource("/glsServlet?sid=" + sidValue);
        
        myDataSource.responseType = YAHOO.util.XHRDataSource.TYPE_XML;
        myDataSource.useXPath = true;
        myDataSource.responseSchema = 
        {
            resultNode: "LocationInfo",
            fields: ["LocationCode", "CityCode", "LocationName", "CountryCode", "CountryName", "StateProvinceCode", "StateProvinceName", "LocationType"]
        };
        return myDataSource;
    }
    
    return {
    
    	setSidValue:function(sidVal)
    	{
    		sidValue = sidVal;
    	},
        createAutoComplete:function(arguments)
        {
            var field = arguments.field;
            var containerId = arguments.containerId;
            var resolveTo = arguments.resolveTo;
            var itemSelectHandler = arguments.itemSelectHandler;
            var forceAirForThreeChars = arguments.forceAirForThreeChars;
            var allAirportsText = arguments.allAirportsText;
            var railVendors = arguments.railVendors;
            
            var locale;
            var myDataSource = createAutoCompleteDataSource();
            
            if (resolveTo == 'Cty')
                locale = "en_US";
            else
                locale = GT.locale || "en_US";
            
            var myAutoComp = new YAHOO.widget.AutoComplete(field, containerId, myDataSource);
            myAutoComp.generateRequest = function(sQuery) {
            	var resolveToValue;
            	
            	if(resolveTo == 'Air' || (field.value.length==3 && forceAirForThreeChars)){
            		resolveToValue = 'Air';
            	} else {
            		resolveToValue = resolveTo || 'Cty';
            	}
            	var sourceVal = railVendors ? '&source=' + railVendors : "";
            	return "&inputString=" + sQuery + "&resolveTo="+ resolveToValue +"&locale=" + locale + sourceVal + '&client=GET_THERE';
            };
            
            myAutoComp.doBeforeExpandContainer = function(oTextbox, oContainer, sQuery, aResults) { 
                var pos = YAHOO.util.Dom.getXY(oTextbox); 
                pos[1] += YAHOO.util.Dom.get(oTextbox).offsetHeight + 2; 
                YAHOO.util.Dom.setXY(oContainer,pos); 
                return true; 
            }; 
            
            //myAutoComp.prehighlightClassName = "yui-ac-prehighlight";
            myAutoComp.useShadow = true;
            
            myAutoComp.maxResultsDisplayed = 10;
            myAutoComp.minQueryLength = 3;
            myAutoComp.queryDelay = 0.2;
            myAutoComp.resultTypeList = false;
            myAutoComp.animVert = false;
            myAutoComp.formatResult = function(oResultData, sQuery, sResultMatch) {
               var content;
               if (resolveTo == 'Air' || oResultData.LocationType == "ARP")
               {    
                   content = "<div>" + highlightSearchTerms(oResultData.LocationName, sQuery,"<b>", "</b>")+ " (";
                   var code = oResultData.LocationType == "ARP" ? oResultData.LocationCode : allAirportsText;
                   content = content + highlightSearchTerms(code, sQuery,"<b>", "</b>");
                   content = content + ")</div><div class='autocompleteCountry'>";
                   if (oResultData.StateProvinceName)
                      content = content + oResultData.StateProvinceName + ", ";
                   content = content + oResultData.CountryName +"</div>";
                    
                   return content;
                }
                else if (resolveTo == "RLS")
                {
                	content = "<div>";
                	if(oResultData.LocationName)
                	{
                	   content += highlightSearchTerms(oResultData.LocationName, sQuery,"<b>", "</b>");
                	}
                    if (oResultData.StateProvinceName)
                       content = content +", "+ highlightSearchTerms(oResultData.StateProvinceName, sQuery,"<b>", "</b>");
                    content += " " + oResultData.CountryName;
					if( oResultData.LocationCode) content +=  ' (' +  oResultData.LocationCode + ')</div>';
                    return content;
                }
                else
                {
                    content = "<div>" + highlightSearchTerms(oResultData.LocationName, sQuery,"<b>", "</b>");
                    if (oResultData.StateProvinceName)
                       content = content +", "+ highlightSearchTerms(oResultData.StateProvinceName, sQuery,"<b>", "</b>");
                    content = content + "</div><div class='autocompleteCountry'>" + oResultData.CountryName + "</div>";
                    return content;
                 }
                
            };
        
            myAutoComp.itemSelectEvent.subscribe(itemSelectHandler);
            myAutoComp.allowBrowserAutocomplete = false;
            myAutoComp.useIFrame = true;
        }
	};
}();


GT.registerNS = function(namespace) {
	var namespaceParts=namespace.split(".");
	var currentNamespace=GT;
	for(var index = 0; index<namespaceParts.length; index++) {
		if(typeof(currentNamespace[namespaceParts[index]])=='undefined')
		   currentNamespace[namespaceParts[index]]=new Object();

		currentNamespace=currentNamespace[namespaceParts[index]];
	}
}

GT.init = function()
{
   // Automatically configure popIns
   jQuery('a.popIn').each(function() {
      var id = jQuery(this).attr('data-popIn');
      if (!id) return;
      var clicked, timeoutId;
      var target = this;
      jQuery(this).click(function(event) {
    	 event.preventDefault();
         clicked=true;
         inlinePopup(id, target);
      }).mouseover(function() {
         timeoutId = setTimeout(function() {
            inlinePopup(id, target);
         }, 300);
      }).mouseout(function() {
         window.clearTimeout(timeoutId);
         if(!clicked) {
            closeInlinePopup(id, target);
         }
      });
   
      var closeText = GT.CLOSE_TEXT || 'Esc to close';
      var sel = '#'+id;
      jQuery(sel).wrapInner('<div class="inlinePopupContent" />');
      jQuery(sel+' .title').after('<hr>').before('<div class="spriteIcons closeIcon closePopIn"></div><a class="closeDialog closePopIn">'+closeText+'</a>');
      jQuery(sel+' .closePopIn').click(function() {
         clicked=false;
         closeInlinePopup(id, target);
      });
      if(jQuery.fn.bgiframe)
      {
         jQuery(sel).bgiframe();
      }
   });

   //Used to overcome loss of bullets on floating lists/list elements
   jQuery('.twoColList').each(function() {
	   jQuery(this).wrapAll('<div class="clearfix" />');
	   var mid = Math.round(jQuery(this).find('li').length/2) -1; // find midpoint 
	   jQuery(this).find('li:gt(' + mid +')').insertAfter(this).wrapAll('<ul class="twoColList" />');//subtract -1 since indices are zero based for gt selector use below
   });
   
   //Used to control card expanding and collapsing
   jQuery(".controlLink").click(function(event) {
	   event.preventDefault();
	   toggleRow(this);
   });
   jQuery(".expandAll").click(function(event) {
	   event.preventDefault();
	   toggleAllRows(":hidden");
   });
   jQuery(".collapseAll").click(function(event) {
	   event.preventDefault();
	   toggleAllRows(":visible");
   });
}

function allowContinue(allowed)
{
    if (allowed)
    {
        cssSwitch('purchaseTrip!btn_prim');
        jQuery("#purchaseTrip").removeAttr('disabled');
    }
    else
    {
        cssSwitch('purchaseTrip!btn_disabled');
        jQuery("#purchaseTrip").attr('disabled', 'disabled');
    }
}

function toggleRow(controlLink) {
	var travelCard = jQuery(controlLink).closest(".travelCard");
	var subAsset = jQuery(travelCard).find(".subAsset");
	jQuery(subAsset).toggle("blind", "fast", updateControlLink(controlLink, subAsset));
}

function updateControlLink(controlLink, subAsset) {
	if (jQuery(subAsset).is(":visible")) {
		var expandTitle = jQuery(controlLink).attr("data-expandTitle");
		var expandText = jQuery(controlLink).attr("data-expandText");
		jQuery(controlLink).attr("title",expandTitle);
		jQuery(controlLink).html(expandText);
	} else {
		var collapseTitle = jQuery(controlLink).attr("data-collapseTitle");
		var collapseText = jQuery(controlLink).attr("data-collapseText");
		jQuery(controlLink).attr("title",collapseTitle);
		jQuery(controlLink).html(collapseText);
	}
}

function toggleAllRows(selector) {
	var controlLinks = jQuery(".controlLink");
	for(var i = 0; i <= controlLinks.length; i++) {
		var travelCard = jQuery(controlLinks[i]).closest(".travelCard");
		var subAsset = jQuery(travelCard).find(".subAsset"); 
		if (jQuery(subAsset).is(selector)) {
			jQuery(subAsset).toggle("blind", "fast", updateControlLink(controlLinks[i], subAsset));
		}
	}
}

function toggleCollapsibleContent(speed,slideSelector,options) {
	jQuery('#'+slideSelector).slideToggle(speed, function(){
		if(options.arrowOptions.arrowId){
			 var arrowObj = jQuery('#'+options.arrowOptions.arrowId);
		}
		if(jQuery(this).css('display') != 'none') {
			if(options.arrowOptions.arrowId && options.arrowOptions.downImg) {
				jQuery(arrowObj).attr('src', options.arrowOptions.downImg);
			}
			if(options.arrowOptions.arrowId && options.arrowOptions.altOpen){
				jQuery(arrowObj).attr('alt', options.arrowOptions.altOpen);
			}
	        if(options.ariaExpanded){
	        	jQuery(this).attr('aria-expanded', 'true');
	        }
	    }
		else {
			if(options.arrowOptions.arrowId && options.arrowOptions.rightImg) {
				jQuery(arrowObj).attr('src', options.arrowOptions.rightImg);
			}
			if(options.arrowOptions.arrowId && options.arrowOptions.altClose){
				jQuery(arrowObj).attr('alt', options.arrowOptions.altClose);
			}
	        if(options.ariaExpanded){
	        	jQuery(this).attr('aria-expanded', 'false');
	        }
	    }
		return false;
	});
}

jQuery(document).ready(function(){
   GT.init();
   OOPTooltipsHolderModule.init(); 
});

var OOPTooltipsHolderModule = (function (jQuery) {
	  var tips = []; // an array of tooltips
	    return {
		    init: function(){
	        	jQuery('.has-tooltip').each(function(index) {
	        		tips[index] = new Tooltip(jQuery(this));
	                tips[index].bindHandlers();
	        	});
		    }
		};
}(jQuery));

var Tooltip = function(source){
	var KEY_ESC = 27;
	var tooltipOwnedByObj = source;
	var tooltipObj = jQuery('#' + source.attr('aria-describedby'));
	var mouseover = false;
	var focus = false;
	var dismissed = false;
  
    var hideTip = function(){
        tooltipObj.hide();
    };
    var showTip = function(){
        tooltipObj.css('display', 'inline');
    };
    var handleKeyDown = function(e){
          if (e.altKey || e.shiftKey || e.ctrlKey) {
              return true; 
          }
          if (e.which == KEY_ESC) { 
              hideTip(); 
              dismissed = true; 
              e.stopPropagation(); 
              return false; 
          }     
          return true; 
    };
    var handleMouseOver = function(e){
        showTip(); 
        mouseover = true;       
        e.stopPropagation(); 
        return false; 
    };
    var handleMouseOut = function(e){
        if (dismissed === true || focus === false) { 
          hideTip(); 
        }  
        mouseover = false;       
        e.stopPropagation(); 
        return false; 
    };
    var handleFocus = function(e){
        showTip(); 
        focus = true;       
        e.stopPropagation(); 
        return false; 
    };
    var handleBlur = function(e){
        if (mouseover === false) { 
          hideTip(); 
        }       
        focus = false; 
        dismissed = false;       
        e.stopPropagation(); 
        return false; 
    };
    return {
    	bindHandlers: function() {       
            tooltipOwnedByObj.keydown(function(e) { 
                return handleKeyDown(e); 
            });      
            tooltipOwnedByObj.mouseover(function(e) { 
                return setTimeout(function (){ handleMouseOver(e);}, 500); 
            });      
            tooltipOwnedByObj.mouseout(function(e) {
                return setTimeout(function (){ handleMouseOut(e);}, 500); 
            });      
            tooltipOwnedByObj.focus(function(e) { 
                return handleFocus(e); 
            });      
            tooltipOwnedByObj.blur(function(e) { 
                return handleBlur(e); 
            });
        }
    };
};