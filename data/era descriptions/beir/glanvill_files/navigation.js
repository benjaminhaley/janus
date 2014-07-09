
<!--

// variables for rearranging the SoM/CoM
var sceeboid, scimid, sctitle, scauth, scimprint, scdate, scbib, scpages, scillid, scthumb, scauthid, scftid, hastcp, ftvisible, save_istcp2;

//  Javascript functions used to output the navigation bar at the top of
//    each page, and to output the icons for navigating netween citations,
//    page images, illustrations pages etc.

//  Output the "EEBO : Early English Books Online" yellow banner at the
//    very top of the page; and (possibly) also the black title bar
//    underneath it (with the page title and the Home / Search / Browse /
//    Marked List / Search History / Help links)
//
//  The arguments specify how the title bar is displayed.  They indicate:
//
//    - Text for the title bar, e.g. "Search - Results"
//
//    - GIF image containing the title, e.g. "header_results.gif"
//
//    - The width in pixels of this image file, e.g. 180
//
//    - The URL for the online help associated with the page, e.g.
//      "/help/basic_srch.htm".  If omitted, this defaults to the
//      Help Contents page at "/help/contents.htm"
//
//    - An optional argument specifying how the navigation bar should be
//      displayed:
//
//          "CLOSEONLY" - Display just a "Close Window" and "Help?" link,
//                        without the links to other parts of the site.
//                        Used for popups such as the Table of Contents.
//
//          "HOME", "SEARCH", "BROWSE", "MARKEDLIST", "HISTORY", "HELP"
//                      - Display the standard navigation bar with the
//                        indicated item highlighted.
//
//  If all the arguments are omitted, then just the banner is output,
//    without any title bar underneath.
//
function output_banner_and_title( titletext, titleimage, titlewidth, helplink, navoption )
{
	var text, imgsrc, linkstart, linkend;
	if ( ! navoption ) navoption = "";
	if ( ! helplink ) helplink = "/help/contents.htm";

	//  Body tag - includes 'OnLoad' event handler which pre-loads
	//    all the mouse-over images used for the title bar
	//
	text = '<body bgcolor="#FFFFFF" text="#000000" ' +
		   'leftmargin="0" topmargin="0" ' +
		   'marginwidth="0" marginheight="0" ' +
		   'onLoad="MM_preloadImages( \'/images/home_on.gif\', ' +
		                             '\'/images/search_on.gif\', ' +
								 '\'/images/browse_on.gif\', ' +
								 '\'/images/marked_on.gif\', ' +
								 '\'/images/srchhist_on.gif\', ' +
								 '\'/images/basicsrch_on.gif\', ' +
								 '\'/images/advancedsrch_on.gif\', ' +
								 '\'/images/periodicalssrch_on.gif\', ' +
								 '\'/images/authorbrowse_on.gif\', ' +
								 '\'/images/thomasonbrowse_on.gif\', ' +
								 '\'/images/periodicalsbrwse_on.gif\', ' +
								 '\'/images/help_on.gif\', ' +
								 '\'/images/closewin_on.gif\' )">';

	//  'Skip over navigation' link (for speaking browsers etc.)
	text += '<a class="skiplink" href="#start">Skip over navigation</a>';

	//  Target for 'Back to top' links from the page footer
	text += '<a name="top"></a>'

	//  Yellow "EEBO : Early English Books Online" banner
	text += '<table width="760" border="0" cellspacing="0" cellpadding="0">' + '<tr>' +
			'<td bgcolor="#FFCC00" valign="top">' +
			'<img src="/images/eebo_logo_sml.gif" ' +
			'alt="EEBO - Early English Books Online - Logo" ' +
			'width="220" height="76" ' +
			'title="EEBO - Early English Books Online - Logo">' +
			'</td>\n' +
			'<td bgcolor="#FFCC00" valign="top" align="right">' +
			'<a href="http://www.proquest.com" target="_blank" ' +
			'title="www.proquest.com. The ProQuest website will open in a ' +
			'new window. EEBO will remain open in the original window.">' +
			'<img src="/images/ch_logo_sml.gif" width="77" height="76" ' +
			'border="0"></a>' +
			'</td>' + '</tr>' + '</table>';

	//  If no title bar is being output, finish the table now
	if ( ! titleimage || ! titlewidth )
	{
		document.write( text );
		return;
	}

	//  Title line

	//  Ruled line along the top of the title line
	text += '<table width="760" border="0" ' +
			'cellspacing="0" cellpadding="0" height="28">\n' +
			'<tr>\n' +
			'<td colspan="2">' +
			'<img src="/images/navline_top.gif" alt="" ' +
			'width="760" height="2">' +
			'</td></tr>\n';

	//  Title image on left
	text += '<tr>\n' +
			'<td bgcolor="#000000" align="left" valign="top" ' +
			'height="24" width="400">' +
			'<img src="/images/' + titleimage + '" ' +
			'alt="' + titletext + '" ' +
			'title="' + titletext + '" ' +
			'width="' + titlewidth + '" height="24">' +
			'</td>\n';

	//  Navigation links on right
	text += '<td bgcolor="#000000" height="24" align="left">' +
			'<table border="0" cellspacing="0" cellpadding="0" ' +
			'height="24" width="360" ' +
			'summary="6 EEBO navigation links">\n' +
			'<tr>\n';

	//  If the CLOSEONLY option was specified, just place a "CLOSE WINDOW"
	//    icon at the far right of the bar
	//
	if ( navoption.toUpperCase() == "CLOSEONLY" )
	{
		text += '<td align="right">' +
				'<a href="javascript:window.close()" onMouseOut="MM_swapImgRestore()" ' +
				'onMouseOver="MM_swapImage(\'closewin\',\'\',\'/images/closewin_on.gif\',1)">' +
				'<img src="/images/closewin_off.gif" ' +
				'alt="Close Window" title="Close Window" ' +
				'width="116" height="24" name="closewin" border="0">' +
				'</a></td>';
	}

	//  Otherwise (when anything other than CLOSEONLY was specified) display
	//    the navigation bar with links for: "Home", "Marked List",
	//    "Search History" and "Help?" on the title-bar; and a line below
	//    with links to "SEARCH: Basic / Advanced / Periodicals" on the left
	//    and "BROWSE: Authors / Thomason Tracts / Periodicals" on the right.
	//
	else
	{
		//  HOME link
		imgsrc = ( navoption.toUpperCase() == "HOME" )
					? "/images/home_on.gif" : "/images/home_off.gif";
		text += '<td align="left">' +
				'<a href="/home" onMouseOut="MM_swapImgRestore()" ' +
				'onMouseOver="MM_swapImage(\'home\',\'\',\'/images/home_on.gif\',1)" ' +
				'accesskey="1">' +
				'<img src="' + imgsrc + '" ' +
				'alt="Home" title="Home [accesskey = 1]" width="62" height="24" ' +
				'name="home" border="0">' +
				'</a></td>';

		//  MARKED LIST link
		imgsrc = ( navoption.toUpperCase() == "MARKEDLIST" )
					? "/images/marked_on.gif" : "/images/marked_off.gif";
		text += '<td align="left">' +
				'<a href="/works/search?ACTION=SearchOrBrowse&SEARCH=ViewSelectedRecords&SomType=VIEWSELRECS" ' +
				'onMouseOut="MM_swapImgRestore()" ' +
				'onMouseOver="MM_swapImage(\'marked\',\'\',\'/images/marked_on.gif\',1)" ' +
				'accesskey="9">' +
				'<img src="' + imgsrc + '" ' +
				'alt="Marked List" title="Marked List [accesskey = 9]" width="103" height="24" ' +
				'name="marked" border="0"></a></td>';

		//  SEARCH HISTORY link
		imgsrc = ( navoption.toUpperCase() == "HISTORY" )
					? "/images/srchhist_on.gif" : "/images/srchhist_off.gif";
		text += '<td align="left">' +
				'<a href="/search/history?action=CREATE" ' +
				'onMouseOut="MM_swapImgRestore()" ' +
				'onMouseOver="MM_swapImage(\'shist\',\'\',\'/images/srchhist_on.gif\',1)" ' +
				'accesskey="8">' +
				'<img src="' + imgsrc + '" ' +
				'alt="Search History" title="Search History [accesskey = 8]" ' +
				'width="124" height="24" name="shist" border="0"></a></td>';
	}

	//  HELP? link
	imgsrc = ( navoption.toUpperCase() == "HELP" )
				? "/images/help_on.gif" : "/images/help_off.gif";
	text += '<td align="right">' +
			'<a href="' + helplink + '" ' +
			'onMouseOut="MM_swapImgRestore()" ' +
			'onMouseOver="MM_swapImage(\'help\',\'\',\'/images/help_on.gif\',1)" ' +
			'accesskey="l">' +
			'<img src="' + imgsrc + '" ' +
			'alt="Help" title="Help [accesskey = l]" width="71" height="24" ' +
			'name="help" border="0"></a>' +
			'<a href="/about/about.htm" accesskey="a"></a></td>';

	//  End of title bar links
	text += '</tr></table>\n' +
			'</td></tr>\n';

	//  Ruled line along the bottom of the title line
	text += '<tr><td colspan="2">' +
			'<img src="/images/navline_bottom.gif" alt="" ' +
	        'width="760" height="2"></td>' +
			'</tr>\n';

	//  Output row for Search and Browse links
	if ( navoption.toUpperCase() != "CLOSEONLY" )
	{
		text += '<tr>';

		//  Block of "SEARCH" links on left
		text += '<td align="left" bgcolor="#000000">' +
				'<table cellpadding="0" cellspacing="0" bgcolor="red"><tr>';

		//  Indention
		text += '<td align="center" bgcolor="#000000" height="20" ' +
				'width="10"></td>';

		//  "SEARCH:" heading
		text += '<td align="left" bgcolor="#000000" height="20" ' +
				'width="64">' +
				'<img src="/images/search_label.gif" ' +
				'alt="SEARCH:" ' +
				'height="20" width="64" border="0">' +
				'</td>';

		//  Search "Basic" link
		imgsrc = ( navoption.toUpperCase() == "BASIC" )
					? "/images/basicsrch.gif"
					: "/images/basicsrch_off.gif";
		linkstart = ( navoption.toUpperCase() == "BASIC" ) ? "" :
					"<a href=\"/search\" " +
					"onMouseOut=\"MM_swapImgRestore()\" " +
					"onMouseOver=\"MM_swapImage('basicsrch',''," +
					"'/images/basicsrch_on.gif',1)\" " +
					"accesskey=\"2\">";
		linkend = ( linkstart != "" ) ? "</a>" : "";
		text += '<td align="left" bgcolor="#000000" ' +
				'width="28" ' +
				'height="20">' +
				linkstart +
				'<img name="basicsrch" src="' + imgsrc + '" ' +
				'alt="Basic Search" ' +
				'title="Basic Search [accesskey = 2]" ' +
				'height="20" width="28" border="0">' +
				linkend +
				'</td>';

		//  Separator
		text += '<td align="center" bgcolor="#000000" ' +
				'width="35" ' +
				'height="20">' +
				'<img src="/images/basicsrch_spacer.gif" ' +
				'alt="" ' +
				'height="20" width="35" border="0">' +
				'</td>';

		//  Search "Advanced" link
		imgsrc = ( navoption.toUpperCase() == "ADVANCED" )
					? "/images/advancedsrch.gif"
					: "/images/advancedsrch_off.gif";
		linkstart = ( navoption.toUpperCase() == "ADVANCED" ) ? "" :
					"<a href=\"/search?SCREEN=search_advanced.htx\" " +
					"onMouseOut=\"MM_swapImgRestore()\" " +
					"onMouseOver=\"MM_swapImage('advancedsrch',''," +
					"'/images/advancedsrch_on.gif',1)\" " +
					"accesskey=\"3\">";
		linkend = ( linkstart != "" ) ? "</a>" : "";
		text += '<td align="left" bgcolor="#000000" ' +
				'width="55" ' +
				'height="20">' +
				linkstart +
				'<img name="advancedsrch" src="' + imgsrc + '" ' +
				'alt="Advanced Search" ' +
				'title="Advanced Search [accesskey = 3]" ' +
				'height="20" width="55" border="0">' +
				linkend +
				'</td>';

		//  Separator
		text += '<td align="center" bgcolor="#000000" ' +
				'width="33" ' +
				'height="20">' +
				'<img src="/images/advancedsrch_spacer.gif" ' +
				'alt="" ' +
				'height="20" width="33" border="0">' +
				'</td>';

		//  Search "Periodicals" link
		imgsrc = ( navoption.toUpperCase() == "PERIODICALSEARCH" )
					? "/images/periodicalssrch.gif"
					: "/images/periodicalssrch_off.gif";
		linkstart = ( navoption.toUpperCase() == "PERIODICALSEARCH" ) ? "" :
					"<a href=\"/periodicals_search\" " +
					"onMouseOut=\"MM_swapImgRestore()\" " +
					"onMouseOver=\"MM_swapImage('periodicalssrch',''," +
					"'/images/periodicalssrch_on.gif',1)\" " +
					"accesskey=\"4\">";
		linkend = ( linkstart != "" ) ? "</a>" : "";
		text += '<td align="left" bgcolor="#000000" ' +
				'width="65" ' +
				'height="20">' +
				linkstart +
				'<img name="periodicalssrch" src="' + imgsrc + '" ' +
				'alt="Periodicals Search" ' +
				'title="Periodicals Search [accesskey = 4]" ' +
				'height="20" width="65" border="0">' +
				linkend +
				'</td>';

		text += '</tr></table>' + '</td>';

		//  Block of "BROWSE" links on right
		text += '<td align="left" bgcolor="#000000">' +
				'<table cellpadding="0" cellspacing="0" bgcolor="blue"><tr>';

		//  Indention
		text += '<td align="center" bgcolor="#000000" height="20" ' +
				'width="15"></td>';

		//  "BROWSE:" heading
		text += '<td align="left" bgcolor="#000000" ' +
				'width="54" ' +
				'height="20">' +
				'<img src="/images/browse_label.gif" ' +
				'alt="BROWSE:" ' +
				'height="20" width="54" border="0">' +
				'</td>';

		//  Browse "Authors" link
		imgsrc = ( navoption.toUpperCase() == "AUTHORS" )
					? "/images/authorbrowse.gif"
					: "/images/authorbrowse_off.gif";
		linkstart = ( navoption.toUpperCase() == "AUTHORS" ) ? "" :
					"<a href=\"/authors\" " +
					"onMouseOut=\"MM_swapImgRestore()\" " +
					"onMouseOver=\"MM_swapImage('authorbrowse',''," +
					"'/images/authorbrowse_on.gif',1)\" " +
					"accesskey=\"5\">";
		linkend = ( linkstart != "" ) ? "</a>" : "";
		text += '<td align="left" bgcolor="#000000" ' +
				'width="46" ' +
				'height="20">' +
				linkstart +
				'<img name="authorbrowse" src="' + imgsrc + '" ' +
				'alt="Browse Authors" ' +
				'title="Browse Authors [accesskey = 5]" ' +
				'height="20" width="46" border="0">' +
				linkend +
				'</td>';

		//  Separator
		text += '<td align="center" bgcolor="#000000" ' +
				'width="31" ' +
				'height="20">' +
				'<img src="/images/authorbrowse_spacer.gif" ' +
				'alt="" ' +
				'height="20" width="31" border="0">' +
				'</td>';

		//  Browse "Thomason Tracts" link
		imgsrc = ( navoption.toUpperCase() == "THOMASONTRACTS" )
					? "/images/thomasonbrowse.gif"
					: "/images/thomasonbrowse_off.gif";
		linkstart = ( navoption.toUpperCase() == "THOMASONTRACTS" ) ? "" :
					"<a href=\"/thomason_browse\" " +
					"onMouseOut=\"MM_swapImgRestore()\" " +
					"onMouseOver=\"MM_swapImage('thomasonbrowse',''," +
					"'/images/thomasonbrowse_on.gif',1)\" " +
					"accesskey=\"7\">";
		linkend = ( linkstart != "" ) ? "</a>" : "";
		text += '<td align="left" bgcolor="#000000" ' +
				'width="99" ' +
				'height="20">' +
				linkstart +
				'<img name="thomasonbrowse" src="' + imgsrc + '" ' +
				'alt="Browse Thomason Tracts" ' +
				'title="Browse Thomason Tracts [accesskey = 7]" ' +
				'height="20" width="99" border="0">' +
				linkend +
				'</td>';

		//  Separator
		text += '<td align="center" bgcolor="#000000" ' +
				'width="31" ' +
				'height="20">' +
				'<img src="/images/thomasonbrowse_spacer.gif" ' +
				'alt="" ' +
				'height="20" width="31" border="0">' +
				'</td>';

		//  Browse "Periodicals" link
		imgsrc = ( navoption.toUpperCase() == "PERIODICALBROWSE" )
					? "/images/periodicalsbrwse.gif"
					: "/images/periodicalsbrwse_off.gif";
		linkstart = ( navoption.toUpperCase() == "PERIODICALBROWSE" ) ? "" :
					"<a href=\"/periodicals_date_browse\" " +
					"onMouseOut=\"MM_swapImgRestore()\" " +
					"onMouseOver=\"MM_swapImage('periodicalsbrwse',''," +
					"'/images/periodicalsbrwse_on.gif',1)\" " +
					"accesskey=\"6\">";
		linkend = ( linkstart != "" ) ? "</a>" : "";
		text += '<td align="left" bgcolor="#000000" ' +
				'width="70" ' +
				'height="20">' +
				linkstart +
				'<img name="periodicalsbrwse" src="' + imgsrc + '" ' +
				'alt="Browse Periodicals" ' +
				'title="Browse Periodicals [accesskey = 6]" ' +
				'height="20" width="70" border="0">' +
				linkend +
				'</td>';

		text += '</tr></table>' +
				'</td>';

		text += '</tr>';

		//  Ruled line along the bottom of the title line
		text += '<tr><td colspan="2">' +
				'<img src="/images/navline_bottom.gif" alt="" ' +
				'width="760" height="2"></td>' +
				'</tr>\n';
	}

	text += '</table>';

	//  Output the header
	document.write( text );
}

// Clone of output_banner_and_title(), but with less height!
function output_thin_banner_and_title( titletext, titleimage, titlewidth, helplink, navoption )
{
	var text, imgsrc, linkstart, linkend;
	if ( ! navoption ) navoption = "";
	if ( ! helplink ) helplink = "/help/contents.htm";

	//  Body tag - includes 'OnLoad' event handler which pre-loads
	//    all the mouse-over images used for the title bar
	//
	text = '<body bgcolor="#FFFFFF" text="#000000" ' +
		   'leftmargin="0" topmargin="0" ' +
		   'marginwidth="0" marginheight="0" ' +
		   'onLoad="MM_preloadImages( \'/images/home_on.gif\', ' +
		                             '\'/images/search_on.gif\', ' +
								 '\'/images/browse_on.gif\', ' +
								 '\'/images/marked_on.gif\', ' +
								 '\'/images/srchhist_on.gif\', ' +
								 '\'/images/basicsrch_on.gif\', ' +
								 '\'/images/advancedsrch_on.gif\', ' +
								 '\'/images/periodicalssrch_on.gif\', ' +
								 '\'/images/authorbrowse_on.gif\', ' +
								 '\'/images/thomasonbrowse_on.gif\', ' +
								 '\'/images/periodicalsbrwse_on.gif\', ' +
								 '\'/images/help_on.gif\', ' +
								 '\'/images/closewin_on.gif\' )">';

	//  'Skip over navigation' link (for speaking browsers etc.)
	text += '<a class="skiplink" href="#start">Skip over navigation</a>';

	//  Target for 'Back to top' links from the page footer
	text += '<a name="top"></a>'

	//  Yellow "EEBO : Early English Books Online" banner
	var yellowbanner = '<table width="760" border="0" cellspacing="0" cellpadding="0">' +
	        '<tr>' +
			'<td bgcolor="#FFCC00" valign="top">' +
			'<img src="/images/eebo_logo_sml3.gif" ' +
			'alt="EEBO - Early English Books Online - Logo" ' +
			'width="710" height="36" ' +
			'title="EEBO - Early English Books Online - Logo">' +
			'</td>\n' +
			'<td bgcolor="#FFCC00" valign="top" align="right">' +
			'<a href="http://www.proquest.com" target="_blank" ' +
			'title="www.proquest.com. The ProQuest website will open in a ' +
			'new window. EEBO will remain open in the original window.">' +
			'<img src="/images/ch_logo_sml2.gif" width="49" height="36" ' +
			'border="0"></a>' +
			'</td>' +
			'</tr>' +
			'</table>';

	text += yellowbanner;

	//  If no title bar is being output, finish the table now
	if ( ! titleimage || ! titlewidth )
	{
		document.write( text );
		return;
	}

	//  Title line

	//  Ruled line along the top of the title line
	text += '<table width="760" border="0" ' +
			'cellspacing="0" cellpadding="0" height="28">\n' +
			'<tr>\n' +
			'<td colspan="2">' +
			'<img src="/images/navline_top.gif" alt="" ' +
			'width="760" height="0">' +
			'</td></tr>\n';

	//  Title image on left
	text += '<tr>\n' +
			'<td bgcolor="#000000" align="left" valign="top" ' +
			'height="24" width="400">' +
			'<img src="/images/' + titleimage + '" ' +
			'alt="' + titletext + '" ' +
			'title="' + titletext + '" ' +
			'width="' + titlewidth + '" height="24">' +
			'</td>\n';

	//  Navigation links on right
	text += '<td bgcolor="#000000" height="24" align="left">' +
			'<table border="0" cellspacing="0" cellpadding="0" ' +
			'height="24" width="360" ' +
			'summary="6 EEBO navigation links">\n' +
			'<tr>\n';

	//  If the CLOSEONLY option was specified, just place a "CLOSE WINDOW"
	//    icon at the far right of the bar
	if ( navoption.toUpperCase() == "CLOSEONLY" )
	{
		text += '<td align="right">' +
				'<a href="javascript:window.close()" onMouseOut="MM_swapImgRestore()" ' +
				'onMouseOver="MM_swapImage(\'closewin\',\'\',\'/images/closewin_on.gif\',1)">' +
				'<img src="/images/closewin_off.gif" ' +
				'alt="Close Window" title="Close Window" ' +
				'width="116" height="24" name="closewin" border="0">' +
				'</a></td>';
	}

	//  Otherwise (when anything other than CLOSEONLY was specified) display
	//    the navigation bar with links for: "Home", "Marked List",
	//    "Search History" and "Help?" on the title-bar; and a line below
	//    with links to "SEARCH: Basic / Advanced / Periodicals" on the left
	//    and "BROWSE: Authors / Thomason Tracts / Periodicals" on the right.
	else
	{
		//  HOME link
		imgsrc = ( navoption.toUpperCase() == "HOME" )
					? "/images/home_on.gif" : "/images/home_off.gif";
		text += '<td align="left">' +
				'<a href="/home" onMouseOut="MM_swapImgRestore()" ' +
				'onMouseOver="MM_swapImage(\'home\',\'\',\'/images/home_on.gif\',1)" ' +
				'accesskey="1">' +
				'<img src="' + imgsrc + '" ' +
				'alt="Home" title="Home [accesskey = 1]" width="62" height="24" ' +
				'name="home" border="0">' +
				'</a></td>';

		//  MARKED LIST link
		imgsrc = ( navoption.toUpperCase() == "MARKEDLIST" )
					? "/images/marked_on.gif" : "/images/marked_off.gif";
		text += '<td align="left">' +
				'<a href="/works/search?ACTION=SearchOrBrowse&SEARCH=ViewSelectedRecords&SomType=VIEWSELRECS" ' +
				'onMouseOut="MM_swapImgRestore()" ' +
				'onMouseOver="MM_swapImage(\'marked\',\'\',\'/images/marked_on.gif\',1)" ' +
				'accesskey="9">' +
				'<img src="' + imgsrc + '" ' +
				'alt="Marked List" title="Marked List [accesskey = 9]" width="103" height="24" ' +
				'name="marked" border="0"></a></td>';

		//  SEARCH HISTORY link
		imgsrc = ( navoption.toUpperCase() == "HISTORY" )
					? "/images/srchhist_on.gif" : "/images/srchhist_off.gif";
		text += '<td align="left">' +
				'<a href="/search/history?action=CREATE" ' +
				'onMouseOut="MM_swapImgRestore()" ' +
				'onMouseOver="MM_swapImage(\'shist\',\'\',\'/images/srchhist_on.gif\',1)" ' +
				'accesskey="8">' +
				'<img src="' + imgsrc + '" ' +
				'alt="Search History" title="Search History [accesskey = 8]" ' +
				'width="124" height="24" name="shist" border="0"></a></td>';
	}

	//  HELP? link
	imgsrc = ( navoption.toUpperCase() == "HELP" )
				? "/images/help_on.gif" : "/images/help_off.gif";
	text += '<td align="right">' +
			'<a href="' + helplink + '" ' +
			'onMouseOut="MM_swapImgRestore()" ' +
			'onMouseOver="MM_swapImage(\'help\',\'\',\'/images/help_on.gif\',1)" ' +
			'accesskey="l">' +
			'<img src="' + imgsrc + '" ' +
			'alt="Help" title="Help [accesskey = l]" width="71" height="24" ' +
			'name="help" border="0"></a>' +
			'<a href="/about/about.htm" accesskey="a"></a></td>';

	//  End of title bar links
	text += '</tr></table>\n' +
			'</td></tr>\n';

	//  Ruled line along the bottom of the title line
	text += '<tr><td colspan="2">' +
			'<img src="/images/navline_bottom.gif" alt="" ' +
	        'width="760" height="2"></td>' +
			'</tr>\n';

	//  Output row for Search and Browse links
	if ( navoption.toUpperCase() != "CLOSEONLY" )
	{
		text += '<tr>';

		//  Block of "SEARCH" links on left
		text += '<td align="left" bgcolor="#000000">' +
				'<table cellpadding="0" cellspacing="0" bgcolor="red"><tr>';

		//  Indention
		text += '<td align="center" bgcolor="#000000" height="20" ' +
				'width="10"></td>';

		//  "SEARCH:" heading
		text += '<td align="left" bgcolor="#000000" height="20" ' +
				'width="64">' +
				'<img src="/images/search_label.gif" ' +
				'alt="SEARCH:" ' +
				'height="20" width="64" border="0">' +
				'</td>';

		//  Search "Basic" link
		imgsrc = ( navoption.toUpperCase() == "BASIC" )
					? "/images/basicsrch.gif"
					: "/images/basicsrch_off.gif";
		linkstart = ( navoption.toUpperCase() == "BASIC" ) ? "" :
					"<a href=\"/search\" " +
					"onMouseOut=\"MM_swapImgRestore()\" " +
					"onMouseOver=\"MM_swapImage('basicsrch',''," +
					"'/images/basicsrch_on.gif',1)\" " +
					"accesskey=\"2\">";
		linkend = ( linkstart != "" ) ? "</a>" : "";
		text += '<td align="left" bgcolor="#000000" ' +
				'width="28" ' +
				'height="20">' +
				linkstart +
				'<img name="basicsrch" src="' + imgsrc + '" ' +
				'alt="Basic Search" ' +
				'title="Basic Search [accesskey = 2]" ' +
				'height="20" width="28" border="0">' +
				linkend +
				'</td>';

		//  Separator
		text += '<td align="center" bgcolor="#000000" ' +
				'width="35" ' +
				'height="20">' +
				'<img src="/images/basicsrch_spacer.gif" ' +
				'alt="" ' +
				'height="20" width="35" border="0">' +
				'</td>';

		//  Search "Advanced" link
		imgsrc = ( navoption.toUpperCase() == "ADVANCED" )
					? "/images/advancedsrch.gif"
					: "/images/advancedsrch_off.gif";
		linkstart = ( navoption.toUpperCase() == "ADVANCED" ) ? "" :
					"<a href=\"/search?SCREEN=search_advanced.htx\" " +
					"onMouseOut=\"MM_swapImgRestore()\" " +
					"onMouseOver=\"MM_swapImage('advancedsrch',''," +
					"'/images/advancedsrch_on.gif',1)\" " +
					"accesskey=\"3\">";
		linkend = ( linkstart != "" ) ? "</a>" : "";
		text += '<td align="left" bgcolor="#000000" ' +
				'width="55" ' +
				'height="20">' +
				linkstart +
				'<img name="advancedsrch" src="' + imgsrc + '" ' +
				'alt="Advanced Search" ' +
				'title="Advanced Search [accesskey = 3]" ' +
				'height="20" width="55" border="0">' +
				linkend +
				'</td>';

		//  Separator
		text += '<td align="center" bgcolor="#000000" ' +
				'width="33" ' +
				'height="20">' +
				'<img src="/images/advancedsrch_spacer.gif" ' +
				'alt="" ' +
				'height="20" width="33" border="0">' +
				'</td>';

		//  Search "Periodicals" link
		imgsrc = ( navoption.toUpperCase() == "PERIODICALSEARCH" )
					? "/images/periodicalssrch.gif"
					: "/images/periodicalssrch_off.gif";
		linkstart = ( navoption.toUpperCase() == "PERIODICALSEARCH" ) ? "" :
					"<a href=\"/periodicals_search\" " +
					"onMouseOut=\"MM_swapImgRestore()\" " +
					"onMouseOver=\"MM_swapImage('periodicalssrch',''," +
					"'/images/periodicalssrch_on.gif',1)\" " +
					"accesskey=\"4\">";
		linkend = ( linkstart != "" ) ? "</a>" : "";
		text += '<td align="left" bgcolor="#000000" ' +
				'width="65" ' +
				'height="20">' +
				linkstart +
				'<img name="periodicalssrch" src="' + imgsrc + '" ' +
				'alt="Periodicals Search" ' +
				'title="Periodicals Search [accesskey = 4]" ' +
				'height="20" width="65" border="0">' +
				linkend +
				'</td>';

		text += '</tr></table>' +
				'</td>';

		//  Block of "BROWSE" links on right
		text += '<td align="left" bgcolor="#000000">' +
				'<table cellpadding="0" cellspacing="0" bgcolor="blue"><tr>';

		//  Indention
		text += '<td align="center" bgcolor="#000000" height="20" ' +
				'width="15"></td>';

		//  "BROWSE:" heading
		text += '<td align="left" bgcolor="#000000" ' +
				'width="54" ' +
				'height="20">' +
				'<img src="/images/browse_label.gif" ' +
				'alt="BROWSE:" ' +
				'height="20" width="54" border="0">' +
				'</td>';

		//  Browse "Authors" link
		imgsrc = ( navoption.toUpperCase() == "AUTHORS" )
					? "/images/authorbrowse.gif"
					: "/images/authorbrowse_off.gif";
		linkstart = ( navoption.toUpperCase() == "AUTHORS" ) ? "" :
					"<a href=\"/authors\" " +
					"onMouseOut=\"MM_swapImgRestore()\" " +
					"onMouseOver=\"MM_swapImage('authorbrowse',''," +
					"'/images/authorbrowse_on.gif',1)\" " +
					"accesskey=\"5\">";
		linkend = ( linkstart != "" ) ? "</a>" : "";
		text += '<td align="left" bgcolor="#000000" ' +
				'width="46" ' +
				'height="20">' +
				linkstart +
				'<img name="authorbrowse" src="' + imgsrc + '" ' +
				'alt="Browse Authors" ' +
				'title="Browse Authors [accesskey = 5]" ' +
				'height="20" width="46" border="0">' +
				linkend +
				'</td>';

		//  Separator
		text += '<td align="center" bgcolor="#000000" ' +
				'width="31" ' +
				'height="20">' +
				'<img src="/images/authorbrowse_spacer.gif" ' +
				'alt="" ' +
				'height="20" width="31" border="0">' +
				'</td>';

		//  Browse "Thomason Tracts" link
		imgsrc = ( navoption.toUpperCase() == "THOMASONTRACTS" )
					? "/images/thomasonbrowse.gif"
					: "/images/thomasonbrowse_off.gif";
		linkstart = ( navoption.toUpperCase() == "THOMASONTRACTS" ) ? "" :
					"<a href=\"/thomason_browse\" " +
					"onMouseOut=\"MM_swapImgRestore()\" " +
					"onMouseOver=\"MM_swapImage('thomasonbrowse',''," +
					"'/images/thomasonbrowse_on.gif',1)\" " +
					"accesskey=\"7\">";
		linkend = ( linkstart != "" ) ? "</a>" : "";
		text += '<td align="left" bgcolor="#000000" ' +
				'width="99" ' +
				'height="20">' +
				linkstart +
				'<img name="thomasonbrowse" src="' + imgsrc + '" ' +
				'alt="Browse Thomason Tracts" ' +
				'title="Browse Thomason Tracts [accesskey = 7]" ' +
				'height="20" width="99" border="0">' +
				linkend +
				'</td>';

		//  Separator
		text += '<td align="center" bgcolor="#000000" ' +
				'width="31" ' +
				'height="20">' +
				'<img src="/images/thomasonbrowse_spacer.gif" ' +
				'alt="" ' +
				'height="20" width="31" border="0">' +
				'</td>';

		//  Browse "Periodicals" link
		imgsrc = ( navoption.toUpperCase() == "PERIODICALBROWSE" )
					? "/images/periodicalsbrwse.gif"
					: "/images/periodicalsbrwse_off.gif";
		linkstart = ( navoption.toUpperCase() == "PERIODICALBROWSE" ) ? "" :
					"<a href=\"/periodicals_date_browse\" " +
					"onMouseOut=\"MM_swapImgRestore()\" " +
					"onMouseOver=\"MM_swapImage('periodicalsbrwse',''," +
					"'/images/periodicalsbrwse_on.gif',1)\" " +
					"accesskey=\"6\">";
		linkend = ( linkstart != "" ) ? "</a>" : "";
		text += '<td align="left" bgcolor="#000000" ' +
				'width="70" ' +
				'height="20">' +
				linkstart +
				'<img name="periodicalsbrwse" src="' + imgsrc + '" ' +
				'alt="Browse Periodicals" ' +
				'title="Browse Periodicals [accesskey = 6]" ' +
				'height="20" width="70" border="0">' +
				linkend +
				'</td>';

		text += '</tr></table>' + '</td>';

		text += '</tr>';

		//  Ruled line along the bottom of the title line
		text += '<tr><td colspan="2">' +
				'<img src="/images/navline_bottom.gif" alt="" ' +
				'width="760" height="2"></td>' +
				'</tr>\n';
	}

	text += '</table>';

	//  Output the header
	document.write( text );
	//alert( "output_banner_and_title: finished" );
}

//  Output the standard footer
function output_footer()
{
	var today = new Date();
	var y0 = today.getFullYear();

	var beforeText = '<table width="760" border="0" ' +
			   'cellspacing="0" cellpadding="0">\n' +
			   '<tr><td>\n' +
			   '<div class="footer">\n' +
			   'Send your suggestions, comments or queries to our ' +
			   '<a href="/about/webmaster.htm" accesskey="w" title="Webmaster [accesskey = w]">Webmaster</a>.\n' +
			   '<br>\n' +
			   '<a href="/search?action=Logout" title="End Session">End Session</a>' +
			   ' | ' +
			   '<a href="/about/sitemap.htm" accesskey="m" title="Site Map [accesskey = m]">Site map</a>' +
			   ' | ' +
			   '<a href="/about/about_access.htm" accesskey="i" title="Accessibility [accesskey = i]">Accessibility</a>' +
			   '<br><br>\n' +
			   '<a href="/about/terms.htm" accesskey="c" title="Copyright [accesskey = c]">Data arrangement ' +
			   'and software copyright</a> ' +
			   '&copy; 2003-' ;
	var afterText = ' ProQuest LLC.  ' +
			   '<br>All Rights Reserved.' +
			   '<br><a href="/about/privacy.htm" title="Privacy policy">Privacy policy</a>' +
			   '</div>\n' +
			   '</td></tr>\n' +
			   '<tr><td>' +
			   '<img src="/images/bottomborder.gif" alt="" ' +
			   'width="760" height="35">' +
			   '</td></tr>\n' +
			   '</table>';
	document.write( beforeText + y0 + afterText );
}

//  Output the library branding footer
function output_lb_footer(libbrandinghtml)
{
	var today = new Date();
	var y0 = today.getFullYear();

	var beforeText = '<table width="730" border="0" ' +
			   'cellspacing="0" cellpadding="0">\n' +
			   '<tr><td align="left">\n' +
			   '<div class="footer">\n' +
			   'Send your suggestions, comments or queries to our ' +
			   '<a href="/about/webmaster.htm" accesskey="w" title="Webmaster [accesskey = w]">Webmaster</a>.\n' +
			   '<br>\n' +
			   '<a href="/search?action=Logout" title="End Session">End Session</a>' +
			   ' | ' +
			   '<a href="/about/sitemap.htm" accesskey="m" title="Site Map [accesskey = m]">Site map</a>' +
			   ' | ' +
			   '<a href="/about/about_access.htm" accesskey="i" title="Accessibility [accesskey = i]">Accessibility</a>' +
			   '<br><br>\n' +
			   '<a href="/about/terms.htm" accesskey="c" title="Copyright [accesskey = c]">Data arrangement ' +
			   'and software copyright</a> ' +
			   '&copy; 2003-' ;
	var afterText = ' ProQuest LLC. \n' +
			   '<br>All Rights Reserved.' +
			   '<br><a href="/about/privacy.htm" title="Privacy policy">Privacy policy</a>' +
			   '</td>' +
			   '<td align="right">' + libbrandinghtml +
			   '</td></tr>\n' +
			   '</div>\n' +
			   '<tr><td>' +
			   '</table><table><tr><td>' +
			   '<img src="/images/bottomborder.gif" alt="" ' +
			   'width="760" height="35">' +
    			   '</td></tr>' +
			   '</table>';
	document.write( beforeText + y0 + afterText );
}

//  Output the TCP footer
function output_tcpfooter()
{
	var today = new Date();
	var y0 = today.getFullYear();

	var beforeText = '<table width="760" border="0" ' +
			   'cellspacing="0" cellpadding="0">\n' +
			   '<tr><td>\n' +
			   '<div class="footer">\n' +
			   'Send your suggestions, comments or queries to our ' +
			   '<a href="/about/webmaster.htm" accesskey="w" title="Webmaster [accesskey = w]">Webmaster</a>.\n' +
			   '<br>\n' +
			   '<a href="/search?action=Logout" title="End Session">End Session</a>' +
			   ' | ' +
			   '<a href="/about/sitemap.htm" accesskey="6" title="Site Map  [accesskey = 6]">Site map</a>' +
			   ' | ' +
			   '<a href="/about/about_access.htm" accesskey="7" title="Accessibility [accesskey = 7]">Accessibility</a>' +
			   '<br><br>\n' +
			   '<a href="/about/terms.htm" accesskey="c" title="Copyright [accesskey = c]">Data arrangement ' +
			   'and software copyright</a> ' +
			   '&copy; 2003-' ;
	var afterText = ' ProQuest LLC. ' +
			   'All Rights Reserved.' +
			   '<br><br><a href="/about/tcp_terms.htm" title="TCP Copyright">Keyboarded and encoded full text</a> ' +
			   '&copy; 2003-2004 Early English Books Online Text Creation Partnership. ' +
			   'All Rights Reserved.' +
			   '<br><a href="/about/privacy.htm" title="Privacy policy">Privacy policy</a>' +
			   '</div>\n' +
			   '</td></tr>\n' +
			   '<tr><td>' +
			   '<img src="/images/bottomborder.gif" alt="" ' +
			   'width="760" height="35">' +
			   '</td></tr>\n' +
			   '</table>';
	document.write( beforeText + y0 + afterText );
}

//  Output the TCP footer with Library Branding
function output_lb_tcpfooter(libbrandinghtml)
{
	var today = new Date();
	var y0 = today.getFullYear();

	var beforeText = '<table width="760" border="0" ' +
			   'cellspacing="0" cellpadding="0">\n' +
			   '<tr><td>\n' +
			   '<div class="footer">\n' +
			   'Send your suggestions, comments or queries to our ' +
			   '<a href="/about/webmaster.htm" accesskey="w" title="Webmaster [accesskey = w]">Webmaster</a>.\n' +
			   '<br>\n' +
			   '<a href="/search?action=Logout" title="End Session">End Session</a>' +
			   ' | ' +
			   '<a href="/about/sitemap.htm" accesskey="6" title="Site Map  [accesskey = 6]">Site map</a>' +
			   ' | ' +
			   '<a href="/about/about_access.htm" accesskey="7" title="Accessibility [accesskey = 7]">Accessibility</a>' +
			   '<br><br>\n' +
			   '<a href="/about/terms.htm" accesskey="c" title="Copyright [accesskey = c]">Data arrangement ' +
			   'and software copyright</a> ' +
			   '&copy; 2003-' ;
	var afterText = ' ProQuest LLC. ' +
			   'All Rights Reserved.' +
			   '<br><br><a href="/about/tcp_terms.htm" title="TCP Copyright">Keyboarded and encoded full text</a> ' +
			   '&copy; 2003-2004 Early English Books Online Text Creation Partnership. ' +
			   'All Rights Reserved.' +
			   '<br><a href="/about/privacy.htm" title="Privacy policy">Privacy policy</a>' +
			   '</td>' +
			   '<td align="right">' + libbrandinghtml +
			   '</td></tr>\n' +
			   '</div>\n' +
			   '<tr><td>' +
			   '</table><table><tr><td>' +
			   '<tr><td>' +
			   '<img src="/images/bottomborder.gif" alt="" ' +
			   'width="760" height="35">' +
			   '</td></tr>\n' +
			   '</table>';
	document.write( beforeText + y0 + afterText );
}

function setup_alternative_spelling()
{
	if ( document.SEARCHFORM.SPELLING_VARIANTS.checked )
	{
		document.SEARCHFORM.SOURCE.value='var_spell.cfg';
		if ( document.SEARCHFORM.VARIANT_FORMS.checked )
		{
			document.SEARCHFORM.SOURCE.value='var_form.cfg';
		}
	}
	else if ( document.SEARCHFORM.VARIANT_FORMS.checked )
	{
		document.SEARCHFORM.elements['VARIANT_FORMS'].checked = false;
		document.SEARCHFORM.SOURCE.value='config.cfg';
	}
    else
	{
		document.SEARCHFORM.SOURCE.value='config.cfg';
	}

}

function setup_variant_forms()
{
	if ( document.SEARCHFORM.VARIANT_FORMS.checked )
	{
		document.SEARCHFORM.SOURCE.value='var_form.cfg';
		if ( ! document.SEARCHFORM.SPELLING_VARIANTS.checked )
		{
			document.SEARCHFORM.elements['SPELLING_VARIANTS'].checked = true;
		}
	}
	else if ( document.SEARCHFORM.SPELLING_VARIANTS.checked )
	{
		document.SEARCHFORM.SOURCE.value='var_spell.cfg';
	}
    else
	{
		document.SEARCHFORM.SOURCE.value='config.cfg';
	}

}

function setup_alternative_spelling_old()
{
	if ( document.SEARCHFORM.SPELLING_VARIANTS.checked )
	{
		document.SEARCHFORM.SOURCE.value='var_spell.cfg';
	}
        else if (document.SEARCHFORM.AlternativeSpellings.checked)
	{
                document.SEARCHFORM.SOURCE.value='alt_spell.cfg';
	}
        else
	{
                document.SEARCHFORM.SOURCE.value='config.cfg';
	}

}


//  Output the image for the 'Add to Marked List' checkbox for a particular
//    citation record (identified by its numeric EEBO ID) or image set
//    (identified by the citation's EEBO ID and the image set's VID)
//
//  This control is implemented as an image which gets toggled between two
//    states (i.e. empty and ticked) when it is clicked.
//
//  Note that the image is always output onto the page in its unchecked
//    form: the function 'fnCheckForSelectedRecords' needs to be called
//    once the page has loaded to update each of the images associated
//    with a currently-selected record.
//
function write_markedlist_checkbox( eeboid, vidnumber )
{
	//  Determine whether the checkbox is for a citation or an image set
	var href = "";
	var value = "";
	var alttext = "";
	if ( "" + vidnumber == "undefined" )
	{
		//  Checkbox for marking a citation record
		href = "javascript: addRecord( '" + eeboid + "', 'ChEEBOMarked' )";
		value = "" + eeboid;
		alttext = "Add record to Marked List";
	}
	else if ( vidnumber != "" )
	{
		//  Checkbox for marking an image set within a citation record
		href = "javascript: addSubrec( '" + eeboid + "', '" +
			   vidnumber + "', 'ChEEBOMarked' )";
		value = "" + eeboid + "_" + vidnumber;
		alttext = "Add issue to Marked List";
	}
	else
	{
		//  VID number is empty (i.e. image set with no images)
		//    Omit the checkbox.
		return;
	}

	//  Form the HTML for the checkbox image
	var html = '<A NAME="ChEEBOMarked" ' +
			   'HREF="' + href + '">' +
			   '<IMG SRC="/images/add_off.gif" ' +
			   'NAME="sel' + value + '" ' +
			   'ALT="' + alttext + '" ' +
			   'WIDTH="14" HEIGHT="14" BORDER="0" title="' + alttext + '">' +
			   '</A>' +
			   '<INPUT TYPE="hidden" name="EeboId" value="' + value + '">';
	document.write( html );
}

//  Generate the icons representing links to the Full Citation,
//    Document Images, Illustrations and Full Text pages
//
//  These functions all take the following arguments:
//
//  - An identifier for the citation or full-text division.
//    (For full-text, this may be in two parts, consisting of the
//    identifier and its associated size in kilobytes)
//
//  - Flag indicating whether the user is a TCP subscriber (from the
//    'SUBSCRIBER_TCP' CGI parameter).  This is needed on every page
//    to control whether the Full Text icon is displayed or suppressed.
//
//  - Session file name (from the 'FILE' CGI parameter).  This is
//    needed on every page to control whether the 'Back to results' link
//    is displayed.
//
function write_citation_icon( eeboid, tcpflag, sessionfile, searchscreen, display , entryno, entries, source, highlightkw, ecco )
{
	if ( "" + tcpflag == "undefined" ||
		 tcpflag.toLowerCase().indexOf( 'param(' ) == 0 ||
		 tcpflag.indexOf( 'XXX_' ) == 0 ) tcpflag = "";
	if ( "" + sessionfile == "undefined" ||
		 sessionfile.toLowerCase().indexOf( 'param(' ) == 0 ||
		 sessionfile.indexOf( 'XXX_' ) == 0 ||
		 sessionfile.toLowerCase() == "default" ) sessionfile = "";
	if ( "" + searchscreen == "undefined" ||
		 searchscreen.toLowerCase().indexOf( 'param(' ) == 0 ||
		 searchscreen.indexOf( 'XXX_' ) == 0 ) searchscreen = "";
	if ( "" + display == "undefined" ||
		 display.toLowerCase().indexOf( 'param(' ) == 0 ||
		 display.indexOf( 'XXX_' ) == 0 ) display = "";
	var url = "/search/full_rec?"; // +
			  // "?SOURCE=config.cfg"; // +

	if ( "" + source == "undefined" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "param(SOURCE)" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "param(SEARCHCONFIG)" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "default" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "pgillust.cfg" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "pgthumbs.cfg" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "selthumbs.cfg" )
	{
		url += "SOURCE=config.cfg";
	}
	else
	{
		url += "SOURCE=";
		url += source;
	}

	if ( "" + entryno == "undefined" )
	{
		url += "&ACTION=ByID";
	}
	else if (entryno == "default")
	{
		url += "&ACTION=ByID";
	}
	else if (entryno == "")
	{
		url += "&ACTION=ByID";
	}
	else if ( sessionfile == "" )
	{
		url += "&ACTION=ByID";
	}
	else if ( sessionfile == "default" )
	{
		url += "&ACTION=ByID";
	}
	else
	{
		url += "&ACTION=SINGLE";
	}

	url += "&ID=" + eeboid;
	url += "&ECCO=" + ecco;
	if ( sessionfile != "" ) url += "&FILE=" + sessionfile;
	if ( searchscreen != "" ) url += "&SEARCHSCREEN=" + searchscreen;
	if ( display != "" ) url += "&DISPLAY=" + display;
	if ( entryno != "" ) url += "&SUBSET=" + entryno;
	if ( entries != "" ) url += "&ENTRIES=" + entries;
	if ( ( "" + highlightkw != "undefined") && ( highlightkw != "" ) && ( highlightkw != "undefined" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	var icon = '<IMG SRC="/images/citation_icon.gif" ' +
			   'WIDTH="14" HEIGHT="17" BORDER="0" ' +
			   'ALT="Record" TITLE="Record">';
	document.write( '<A HREF="' + url + '">' + icon + '</A>' );
	// document.write(entryno);
	// document.write( '_' );
	// document.write(entries);
}

function write_docimages_icon( eeboid, tcpflag, sessionfile, searchscreen, display, searchconfig, highlightkw, ecco )
{
	if ( eeboid == "" ) return;
	if ( tcpflag.toLowerCase().indexOf( 'param(' ) == 0 ||
		 tcpflag.indexOf( 'XXX_' ) == 0 ) tcpflag = "";
	if ( "" + sessionfile == "undefined" ||
		 sessionfile.toLowerCase().indexOf( 'param(' ) == 0 ||
		 sessionfile.indexOf( 'XXX_' ) == 0 ||
		 sessionfile.toLowerCase() == "default" ) sessionfile = "";
	if ( "" + searchscreen == "undefined" ||
		 searchscreen.toLowerCase().indexOf( 'param(' ) == 0 ||
		 searchscreen.indexOf( 'XXX_' ) == 0 ) searchscreen = "";
	if ( "" + display == "undefined" ||
		 display.toLowerCase().indexOf( 'param(' ) == 0 ||
		 display.indexOf( 'XXX_' ) == 0 ) display = "";
	var url = "/search/full_rec" +
			  "?SOURCE=pgimages.cfg" +
			  "&ACTION=ByID" +
			  "&ID=" + eeboid;
	if ( sessionfile != "" ) url += "&FILE=" + sessionfile;
	if ( searchscreen != "" ) url += "&SEARCHSCREEN=" + searchscreen;
	if ( searchconfig == "param(SEARCHCONFIG)" )
	{
		url += "&SEARCHCONFIG=config.cfg";
	}
	else if ( searchconfig == "default" )
	{
		url += "&SEARCHCONFIG=config.cfg";
	}
	else if ( searchconfig != "" )
	{
		url += "&SEARCHCONFIG=" + searchconfig;
	}
	url += "&ECCO=" + ecco;
	if ( display != "" ) url += "&DISPLAY=" + display;
	// if ( highlightkw != "" ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	// if ( ( highlightkw != "" ) && ( highlightkw != "undefined" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	if ( ( "" + highlightkw != "undefined") && ( highlightkw != "" ) && ( highlightkw != "undefined" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	var icon = '<IMG SRC="/images/docimage_icon.gif" ' +
			   'WIDTH="14" HEIGHT="17" BORDER="0" ' +
			   'ALT="Document Images" TITLE="Document Images">';
	document.write( '<A HREF="' + url + '">' + icon + '</A>' );
}

function write_illustrations_icon( eeboid, tcpflag, sessionfile, searchscreen, display, searchconfig, highlightkw, ecco )
{
	if ( eeboid == "" ) return;
	if ( tcpflag.toLowerCase().indexOf( 'param(' ) == 0 ||
		 tcpflag.indexOf( 'XXX_' ) == 0 ) tcpflag = "";
	if ( "" + sessionfile == "undefined" ||
		 sessionfile.toLowerCase().indexOf( 'param(' ) == 0 ||
		 sessionfile.indexOf( 'XXX_' ) == 0 ||
		 sessionfile.toLowerCase() == "default" ) sessionfile = "";
	if ( "" + searchscreen == "undefined" ||
		 searchscreen.toLowerCase().indexOf( 'param(' ) == 0 ||
		 searchscreen.indexOf( 'XXX_' ) == 0 ) searchscreen = "";
	if ( "" + display == "undefined" ||
		 display.toLowerCase().indexOf( 'param(' ) == 0 ||
		 display.indexOf( 'XXX_' ) == 0 ) display = "";
	var url = "/search/full_rec" +
			  "?SOURCE=pgillust.cfg" +
			  "&ACTION=ByID" +
			  "&ID=" + eeboid;
	if ( sessionfile != "" ) url += "&FILE=" + sessionfile;
	if ( searchscreen != "" ) url += "&SEARCHSCREEN=" + searchscreen;
	// if ( searchconfig != "" ) url += "&SEARCHCONFIG=" + searchconfig;
	if ( searchconfig == "param(SEARCHCONFIG)" )
	{
		url += "&SEARCHCONFIG=config.cfg";
	}
	else if ( searchconfig == "default" )
	{
		url += "&SEARCHCONFIG=config.cfg";
	}
	else if ( searchconfig != "" )
	{
		url += "&SEARCHCONFIG=" + searchconfig;
	}
	if ( ecco != "" )
	{
		url += "&ECCO=" + ecco;
	}
	if ( display != "" ) url += "&DISPLAY=" + display;
	// if ( highlightkw != "" ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	// if ( ( highlightkw != "" ) && ( highlightkw != "undefined" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	if ( ( "" + highlightkw != "undefined") && ( highlightkw != "" ) && ( highlightkw != "undefined" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	var icon = '<IMG SRC="/images/illustration_icon.gif" ' +
			   'WIDTH="14" HEIGHT="17" BORDER="0" ' +
			   'ALT="Illustrations" TITLE="Illustrations">';
	document.write( '<A HREF="' + url + '">' + icon + '</A>' );
}

function write_thumbnails_icon( eeboid, tcpflag, sessionfile, searchscreen, display, searchconfig, highlightkw, ecco )
{
	if ( eeboid == "" ) return;
	if ( tcpflag.toLowerCase().indexOf( 'param(' ) == 0 ||
		 tcpflag.indexOf( 'XXX_' ) == 0 ) tcpflag = "";
	if ( "" + sessionfile == "undefined" ||
		 sessionfile.toLowerCase().indexOf( 'param(' ) == 0 ||
		 sessionfile.indexOf( 'XXX_' ) == 0 ||
		 sessionfile.toLowerCase() == "default" ) sessionfile = "";
	if ( "" + searchscreen == "undefined" ||
		 searchscreen.toLowerCase().indexOf( 'param(' ) == 0 ||
		 searchscreen.indexOf( 'XXX_' ) == 0 ) searchscreen = "";
	if ( "" + display == "undefined" ||
		 display.toLowerCase().indexOf( 'param(' ) == 0 ||
		 display.indexOf( 'XXX_' ) == 0 ) display = "";
	var url = "/search/full_rec" +
			  "?SOURCE=pgthumbs.cfg" +
			  "&ACTION=ByID" +
			  "&ID=" + eeboid;
	if ( sessionfile != "" ) url += "&FILE=" + sessionfile;
	if ( searchscreen != "" ) url += "&SEARCHSCREEN=" + searchscreen;
	// if ( searchconfig != "" ) url += "&SEARCHCONFIG=" + searchconfig;
	if ( searchconfig == "param(SEARCHCONFIG)" )
	{
		url += "&SEARCHCONFIG=config.cfg";
	}
	else if ( searchconfig == "default" )
	{
		url += "&SEARCHCONFIG=config.cfg";
	}
	else if ( searchconfig == "alt_spell.cfg" ) // special case
	{
		url += "&SEARCHCONFIG=" + searchconfig;
	}
	else if ( searchconfig == "var_spell.cfg" ) // special case
	{
		url += "&SEARCHCONFIG=" + searchconfig;
	}
	else if ( searchconfig == "var_form.cfg" ) // special case
	{
		url += "&SEARCHCONFIG=" + searchconfig;
	}
	else if ( searchconfig != "" )
	{
		url += "&SEARCHCONFIG=config.cfg";
	}
	if ( display != "" ) url += "&DISPLAY=" + display;
	if ( ecco != "" ) url += "&ECCO=" + ecco;
	// if ( highlightkw != "" ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	// if ( ( highlightkw != "" ) && ( highlightkw != "undefined" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	if ( ( "" + highlightkw != "undefined") && ( highlightkw != "" ) && ( highlightkw != "undefined" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	var icon = '<IMG SRC="/images/thumbnail_icon.gif" ' +
			   'WIDTH="14" HEIGHT="17" BORDER="0" ' +
			   'ALT="Thumbnails" TITLE="Thumbnails">';
	document.write( '<A HREF="' + url + '">' + icon + '</A>' );
}

function write_som_thumbnail( imagename, eeboid, tcpflag, sessionfile, searchscreen, display, searchconfig, highlightkw )
{
	if ( tcpflag.toLowerCase().indexOf( 'param(' ) == 0 ||
		 tcpflag.indexOf( 'XXX_' ) == 0 ) tcpflag = "";
	if ( "" + sessionfile == "undefined" ||
		 sessionfile.toLowerCase().indexOf( 'param(' ) == 0 ||
		 sessionfile.indexOf( 'XXX_' ) == 0 ||
		 sessionfile.toLowerCase() == "default" ) sessionfile = "";
	if ( "" + searchscreen == "undefined" ||
		 searchscreen.toLowerCase().indexOf( 'param(' ) == 0 ||
		 searchscreen.indexOf( 'XXX_' ) == 0 ) searchscreen = "";
	if ( "" + display == "undefined" ||
		 display.toLowerCase().indexOf( 'param(' ) == 0 ||
		 display.indexOf( 'XXX_' ) == 0 ) display = "";
	var url = "/search/full_rec" +
			  "?SOURCE=pgthumbs.cfg" +
			  "&ACTION=ByID" +
			  "&ID=" + eeboid;
	if ( sessionfile != "" ) url += "&FILE=" + sessionfile;
	if ( searchscreen != "" ) url += "&SEARCHSCREEN=" + searchscreen;
	// if ( searchconfig != "" ) url += "&SEARCHCONFIG=" + searchconfig;
	if ( searchconfig == "param(SEARCHCONFIG)" )
	{
		url += "&SEARCHCONFIG=config.cfg";
	}
	else if ( searchconfig == "default" )
	{
		url += "&SEARCHCONFIG=config.cfg";
	}
	else if ( searchconfig != "" )
	{
		url += "&SEARCHCONFIG=" + searchconfig;
	}
	if ( display != "" ) url += "&DISPLAY=" + display;
	// if ( ( highlightkw != "" ) && ( highlightkw != "undefined" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	if ( ( "" + highlightkw != "undefined") && ( highlightkw != "" ) && ( highlightkw != "undefined" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	if ( "" + imagename == "undefined" ||
		 imagename.toLowerCase().indexOf( 'param(' ) == 0 ||
		 imagename.indexOf( 'XXX_' ) == 0 ||
		 imagename.toLowerCase() == "default" ) sessionfile = "";
	if ( imagename != "" )
	{
		document.write( '<A HREF="' + url + '"><img src="/thumbnails/' + imagename +'"></A>' );
	}
}

function write_somcom_thumbnail( imagename, tcpflag, sessionfile, searchscreen, display, searchconfig, highlightkw )
{
	if ( tcpflag.toLowerCase().indexOf( 'param(' ) == 0 ||
		 tcpflag.indexOf( 'XXX_' ) == 0 ) tcpflag = "";
	if ( "" + sessionfile == "undefined" ||
		 sessionfile.toLowerCase().indexOf( 'param(' ) == 0 ||
		 sessionfile.indexOf( 'XXX_' ) == 0 ||
		 sessionfile.toLowerCase() == "default" ) sessionfile = "";
	if ( "" + searchscreen == "undefined" ||
		 searchscreen.toLowerCase().indexOf( 'param(' ) == 0 ||
		 searchscreen.indexOf( 'XXX_' ) == 0 ) searchscreen = "";
	if ( "" + display == "undefined" ||
		 display.toLowerCase().indexOf( 'param(' ) == 0 ||
		 display.indexOf( 'XXX_' ) == 0 ) display = "";
	var url = "/search/full_rec" +
			  "?SOURCE=pgthumbs.cfg" +
			  "&ACTION=ByID" +
			  "&ID=" + sceeboid;
	if ( sessionfile != "" ) url += "&FILE=" + sessionfile;
	if ( searchscreen != "" ) url += "&SEARCHSCREEN=" + searchscreen;
	if ( searchconfig == "param(SEARCHCONFIG)" )
	{
		url += "&SEARCHCONFIG=config.cfg";
	}
	else if ( searchconfig == "default" )
	{
		url += "&SEARCHCONFIG=config.cfg";
	}
	else if ( searchconfig != "" )
	{
		url += "&SEARCHCONFIG=" + searchconfig;
	}
	if ( display != "" ) url += "&DISPLAY=" + display;
	// if ( ( highlightkw != "" ) && ( highlightkw != "undefined" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	if ( ( "" + highlightkw != "undefined") && ( highlightkw != "" ) && ( highlightkw != "undefined" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	if ( "" + imagename == "undefined" ||
		 imagename.toLowerCase().indexOf( 'param(' ) == 0 ||
		 imagename.indexOf( 'XXX_' ) == 0 ||
		 imagename.toLowerCase() == "default" ) sessionfile = "";
	if ( imagename != "" )
	{
		document.write( '<A HREF="' + url + '"><img src="/thumbnails/' + imagename +'"></A>' );
	}
}

function write_fulltext_icon( fulltextid, tcpflag, sessionfile, searchscreen, display, source, highlightkw, ecco )
{
	if ( fulltextid == "" ) return;
	if ( tcpflag.toLowerCase().indexOf( 'param(' ) == 0 ||
		 tcpflag.indexOf( 'XXX_' ) == 0 ) tcpflag = "";
	if ( "" + sessionfile == "undefined" ||
		 sessionfile.toLowerCase().indexOf( 'param(' ) == 0 ||
		 sessionfile.indexOf( 'XXX_' ) == 0 ||
		 sessionfile.toLowerCase() == "default" ) sessionfile = "";
	if ( "" + searchscreen == "undefined" ||
		 searchscreen.toLowerCase().indexOf( 'param(' ) == 0 ||
		 searchscreen.indexOf( 'XXX_' ) == 0 ) searchscreen = "";
	if ( "" + display == "undefined" ||
		 display.toLowerCase().indexOf( 'param(' ) == 0 ||
		 display.indexOf( 'XXX_' ) == 0 ) display = "";

	//  Check if the identifier also has an appended size in kilobytes
	var fields = fulltextid.split( "|" );
	var identifier = fields[0];
	var size = ( fields.length == 2 ) ? fields[1] : "";

	var url = "/search/fulltext?"; // +

	if ( "" + source == "undefined" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "param(SOURCE)" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "param(SEARCHCONFIG)" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "default" )
	{
		url += "SOURCE=config.cfg";
	}
	else
	{
		url += "SOURCE=";
		url += source;
	}

		url +=	  "&ACTION=ByID" +
			  "&ID=" + identifier +
			  "&WARN=" + ( size >= 500 ? "Y" : "N" ) +
			  "&SIZE=" + size;
	// if ( sessionfile != "" )
	url += "&FILE=" + sessionfile;
	if ( searchscreen != "" ) url += "&SEARCHSCREEN=" + searchscreen;
	if ( display != "" ) url += "&DISPLAY=" + display;
	if ( ecco != "" )
	{
		url += "&ECCO=" + ecco;
	}
	// if ( ( highlightkw != "" ) && ( highlightkw != "undefined" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	if ( ( "" + highlightkw != "undefined") && ( highlightkw != "" ) && ( highlightkw != "undefined" )  && ( highlightkw != "param(HIGHLIGHT_KEYWORD)" ) ) url += "&HIGHLIGHT_KEYWORD=" + highlightkw;
	var icon = '<IMG SRC="/images/fulltext_icon.gif" ' +
			   'WIDTH="14" HEIGHT="17" BORDER="0" ' +
			   'ALT="Full Text" TITLE="Full Text">';
	if ( tcpflag == "Y" )
	{
		document.write( '<A HREF="' + url + '">' + icon + '</A>' );
		ftvisible = "Y";
	}
	else
	{
		ftvisible = "N";
	}
}

function write_fulltext_start_link( fulltextid, source, display, sessionfile, searchscreen, somentry )
{
	if ( fulltextid == "" ) return;
	if ( "" + sessionfile == "undefined" ||
		 sessionfile.toLowerCase().indexOf( 'param(' ) == 0 ||
		 sessionfile.indexOf( 'XXX_' ) == 0 ||
		 sessionfile.toLowerCase() == "default" ) sessionfile = "";
	if ( "" + searchscreen == "undefined" ||
		 searchscreen.toLowerCase().indexOf( 'param(' ) == 0 ||
		 searchscreen.indexOf( 'XXX_' ) == 0 ) searchscreen = "";
	if ( "" + display == "undefined" ||
		 display.toLowerCase().indexOf( 'param(' ) == 0 ||
		 display.indexOf( 'XXX_' ) == 0 ) display = "";

	//  Check if the identifier also has an appended size in kilobytes
	// var fields = fulltextid.split( "|" );
	// var identifier = fields[0];
	// var size = ( fields.length == 2 ) ? fields[1] : "";

	fturl = "/search/fulltext?"; // +

	if ( "" + source == "undefined" )
	{
		fturl += "SOURCE=config.cfg";
	}
	else if ( source == "param(SOURCE)" )
	{
		fturl += "SOURCE=config.cfg";
	}
	else if ( source == "param(SEARCHCONFIG)" )
	{
		fturl += "SOURCE=config.cfg";
	}
	else if ( source == "default" )
	{
		fturl += "SOURCE=config.cfg";
	}
	else
	{
		fturl += "SOURCE=";
		fturl += source;
	}
	fturl += "&FILE=" + sessionfile;
	fturl +=	  "&ACTION=ByID" +
			  "&ID=" + fulltextid;
	if ( searchscreen != "" ) fturl += "&SEARCHSCREEN=" + searchscreen;
	if ( somentry != "" ) fturl += "&SUBSET=" + searchscreen;
	if ( display != "" ) fturl += "&DISPLAY=" + display;
	if ( ecco != "" )
	{
		fturl += "&ECCO=" + ecco;
	}
}

function write_fulltext_end_link( size )
{
	fturl += "&WARN=" + ( size >= 500 ? "Y" : "N" ) +
			  "&SIZE=" + size;
	
	document.write( '<A HREF="' + fturl + '">' + fttitle + '<I> ' + size + 'Kb</I></A>' );
	fturl = ""; fttitle = "";
}

function write_somcom_icons( tcpflag, sessionfile, searchscreen, display, source, highlightkw, ecco )
{
	// write out all of the OPTIONAL icons for the SOMCOM
	document.write( '<TD WIDTH=20 VALIGN="TOP"> ');
	write_docimages_icon( scimid, tcpflag, sessionfile, searchscreen, display, source, highlightkw, ecco )
	document.write( '</TD> ');
	document.write( '<TD WIDTH=20 VALIGN="TOP"> ');
	write_illustrations_icon( scillid, tcpflag, sessionfile, searchscreen, display, source, highlightkw, ecco )
	document.write( '</TD> ');
	document.write( '<TD WIDTH=20 VALIGN="TOP"> ');
	write_fulltext_icon( scftid, tcpflag, sessionfile, searchscreen, display, source, highlightkw, ecco )
	document.write( '</TD> ');
	document.write( '<TD WIDTH=20 VALIGN="TOP"> ');
	write_thumbnails_icon( scthumb, tcpflag, sessionfile, searchscreen, display, source, highlightkw, ecco );
	document.write( '</TD> ');
}

//  Output the full-text icon on the 'Document Images' page -- as a link
//    not to the entire full-text, but to the lowest level division of the
//    document that contains the currently displayed page image.
//
//  This does not use the supplied full-text identifier (for the document
//    as a whole) and instead relies on certain variables which are
//    populated on the 'Document Images' page: 'currentpage' contains the
//    current page image number; and the arrays 'ftdivlevel[]', 'ftdivref[]'
//    and 'ftdivsize[]' contain the division level, identifier and size in Kb
//    for the full-text division associated with each page image.
//
function write_fulltext_icon_for_current_page( fulltextid, tcpflag, sessionfile, searchscreen, display, source, highlightkw )
{
	if ( tcpflag.toLowerCase().indexOf( 'param(' ) == 0 ||
		 tcpflag.indexOf( 'XXX_' ) == 0 ) tcpflag = "";

	//  Return immediately if the user is not a TCP subscriber
	if ( tcpflag != "Y" ) return;

	if ( "" + sessionfile == "undefined" ||
		 sessionfile.toLowerCase().indexOf( 'param(' ) == 0 ||
		 sessionfile.indexOf( 'XXX_' ) == 0 ||
		 sessionfile.toLowerCase() == "default" ) sessionfile = "";

	if ( "" + searchscreen == "undefined" ||
		 searchscreen.toLowerCase().indexOf( 'param(' ) == 0 ||
		 searchscreen.indexOf( 'XXX_' ) == 0 ) searchscreen = "";

	if ( "" + display == "undefined" ||
		 display.toLowerCase().indexOf( 'param(' ) == 0 ||
		 display.indexOf( 'XXX_' ) == 0 ) display = "";

	//  Get the division level and identifier associated with the
	//    current page image
	//
	var divlevel = ftdivlevel[ currentpage - 1 ];
	var divref = ftdivref[ currentpage - 1 ];
	var divsize = ftdivsize[ currentpage - 1 ];

	/*
	alert( "Current page is " + currentpage +
		   ": level = " + divlevel + " divref = " + divref );
	*/

	var url = "/search/fulltext?";

	if ( "" + source == "undefined" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "param(SOURCE)" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "param(SEARCHCONFIG)" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "default" )
	{
		url += "SOURCE=config.cfg";
	}
	else
	{
		url += "SOURCE=";
		url += source;
	}


		url += "&ACTION=ByID" +
			  "&ID=" + divref +
			  "&DIV=" + divlevel +
			  "&WARN=" + ( divsize >= 500 ? "Y" : "N" ) +
			  "&SIZE=" + divsize +
			  "&FILE=" + sessionfile +
			  "&SEARCHSCREEN=" + searchscreen +
			  "&HIGHLIGHT_KEYWORD=" + highlightkw +
			  "&DISPLAY=" + display +
			  "#page-" + currentpage;

	var icon = '<IMG SRC="/images/fulltext_icon.gif" ' +
			   'WIDTH="14" HEIGHT="17" BORDER="0" ' +
			   'ALT="Full Text for page image ' + currentpage + '" ' +
			   'TITLE="Full Text for page image ' + currentpage + '">';

	document.write( '<A HREF="' + url + '">' + icon + '</A>' );
}

// If the user is a Lion subscriber write out a link to the 
function write_lionauthorpage_link( lionauthid, lionus, lionuk )
{
	var url = "http://lion.chadwyck";
	// document.write( '<A HREF="' + url + '">' + icon + '</A>' );

	if ( "" + lionauthid == "undefined" )
	{
		return;
	}
	if ( lionauthid == "" )
	{
		return;
	}

	if ( lionus == "Y" )
	{
		url += ".com/";
	}
	else if ( lionuk == "Y" )
	{
		url += ".co.uk/";
	}
	else
	{
		return;
	}

	url += "searchFullrec.do?id=";
	url += lionauthid;
	url += "&area=authors&forward=author";
	
	document.write( ' [ <A HREF="' + url + '" title="Link to Author page in Literature Online. Literature Online will open in a new window. EEBO will remain open in the original window." target="_blank">Author page in Literature Online</A> ] ' );
}

function readCookie(name)
{
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i=0;i < ca.length;i++) {
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1,c.length);
		if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
	}
	return null;
}

function write_interactions_link(link_text)
{
	var url = "http://eebo-interactions.chadwyck.com/register";
	// var url = "http://eebo-interactions.qa.collabforge.com/";
	// var url = "http://eebo-interactions.test.collabforge.com/user/register";
	// var cookie_date = new Date ( );  // current date & time
	// cookie_date.setTime ( cookie_date.getTime() - 1 );

	// document.write("x");
	var uid = readCookie("UID");
	var chdom = "chadwyck.com";
	var title = "Register. The EEBO Interactions site will open in a new window. EEBO will remain open in the original window";
	var target = "_blank";
	document.cookie = "eebo_user=" + uid + ";domain=" + chdom + ";path=/;";
	document.write( '<a href="' + url + '" title="' + title + '" target =' + target + '">' + link_text + '</a>' );
	var eebo_user = readCookie("eebo_user");
	// document.write(" new cookie value is: " + eebo_user );
}

function write_wiki_link( wikistr, is_auth, write_str)
{
	var url = "http://eebo-interactions.chadwyck.com/";
	// var url = "http://eebo-interactions.qa.collabforge.com/";
	// var url = "http://eebo-interactions.test.collabforge.com/";
	var entrytype = "work";
	// document.write( '<A HREF="' + url + '">' + icon + '</A>' );

	if ( "" + wikistr == "undefined" )
	{
		return;
	}
	if ( wikistr == "" )
	{
		return;
	}
	if ( wikistr == "Anon." )
	{
		return;
	}

	var testchar = wikistr.charAt(0);

	var cleanstr = wikistr.substr(1);

	var eccopattern = new RegExp("G\\d+", "g");

	var result = eccopattern.exec(eeboid);
	if (result != null)
	{
		return;
	}

	if ( is_auth == "Y" )
	{
		entrytype = "author";
	}

	// in case there is any WH tagging - strip it out
	var rewh = new RegExp("<WORDH>", "g");
	var rewhend = new RegExp("</WORDH>", "g");
	var respace = new RegExp(" ", "g");
	var repunc = new RegExp("\\.|,|/|\\`|\\?", "g");
	var reparen = new RegExp("\\(|\\)", "g");

	var fixedwhstart = cleanstr.replace(rewh, "");
	var fixedwhend = fixedwhstart.replace(rewhend, "");

	if ( write_str == "Y" )
	{
		document.write( fixedwhend );
	}

	if (fixedwhend == "Anon.")
	{
		return;
	}

	var fixedspaces = fixedwhend.replace(respace, "-");
	var fixedparen = fixedspaces.replace(reparen, "");
	var fixedpunc = fixedparen.replace(repunc, "");
	url += entrytype;
	url += "/";
	url += fixedpunc.toLowerCase();
	
	if (testchar == "Y")
	{
		// document.write( ' [ <A HREF="' + url + '" title="Link to ' + entrytype + ' page in EEBO Interactions. EEBO Interactions will open in a new window. EEBO will remain open in the original window." target="_blank">Comment on this ' + entrytype + ' on the EEBO Interactions site <img src="/images/wiki_on.gif"></A> ] ' );
		document.write( ' <A HREF="' + url + '" title="View interaction" target="_blank"><img src="/images/wiki_on.gif" alt="View interaction"></A> ' );
	}
	else
	{
		// document.write( ' [ <A HREF="' + url + '" title="Link to ' + entrytype + ' page in EEBO Interactions. EEBO Interactions will open in a new window. EEBO will remain open in the original window." target="_blank">Be the first to comment on this ' + entrytype + ' on the EEBO Interactions site <img src="/images/wiki_off.gif"></A> ] ' );
		document.write( ' <A HREF="' + url + '" title="Create interaction" target="_blank"><img src="/images/wiki_off.gif" alt="Create interaction"></A> ' );
	}
}

function write_intro_link( introid, introauth )
{
	var url = "/intros/htxview?template=basic.htx&content=intro";

	if ( "" + introid == "undefined" )
	{
		return;
	}
	if ( introid == "" )
	{
		return;
	}

	if ( "" + introauth == "undefined" )
	{
		return;
	}
	if ( introauth == "" )
	{
		return;
	}

	url += introid;
	url += ".htm";

	// document.write( '<B>Read the EEBO Introduction to this work by:</B> <a href="' + url + '">' + introauth + '</a>' );
	document.write( '<a href="' + url + '"><B>Read the EEBO Introduction to this work</B></a>' );
}

function write_citlibbrand( libbrand )
{
	// document.write( 'testing!' );
	if ( libbrand &&
	     libbrand != "undefined" )
	{
		document.write( '<TD VALIGN="TOP"><TABLE>\n');
		// only one - BL - library gif for now - expand if needed later by checking currentslbrand
		var blgif = '<IMG SRC="/images/bl_logo.gif" ' +
			   'WIDTH="52" HEIGHT="100" BORDER="0" ' +
			   'TITLE="http://www.bl.uk. The British Library website will open in a new window. EEBO will remain open in the original window.">';
		document.write( '<a href="http://www.bl.uk/" target="new" >' + blgif + '</a>');
		document.write( '</TABLE></TD>\n');
	}
}

//  Output the key to the icons (for citation / document images etc.)
//    which appears below the title bar on pages which have these icons.
//
//  The single parameter specifies whether the user is a TCP subscriber
//    -- in which case the key should include the 'Full Text' icon.
//    This parameter is usually derived from a substitution string
//    'XXX_SUBSCRIBER_TCP_XXX' or 'param(SUBSCRIBER_TCP)' and contains
//    either 'Y' or an empty string.
//
function output_key_gif( fulltextflag )
{
	if ( fulltextflag == "Y" )
	{
		document.write( '<img src="/images/key_tcp.gif" alt="EEBO - Early English Books Online - Key" width="760" height="22" title="EEBO - Early English Books Online - Key">' );
	}
	else
	{
		document.write( '<img src="/images/key_nontcp.gif" alt="EEBO - Early English Books Online - Key" width="760" height="22" title="EEBO - Early English Books Online - Key">' );
	}
}


//  Output the 'Back to Results' link which appears on the Full Citation /
//    Document Images / Context of Matches / Illustrations / Full Text
//    pages etc.
//
//  The arguments are the text for the link (e.g. 'Back to Results'), the
//    the name of the session file for the search (from the CGI parameter
//    'FILE') and the name of the search screen from which the search
//    was submitted (from the 'SEARCHSCREEN' parameter).  Note that this
//    latter parameter is only present for searches submitted from the
//    Basic or Advanced Search screen, and is used to control the display
//    of the 'Refine Search' link on the Search Results page.
//
//  The link is displayed whenever the session file is present.
//
function write_back_to_results_link( text, sessionfile, searchscreen, size, display, ecco, altsource, kw_from_lion )
{
	//  Check whether a session file and searchscreen parameter have been
	//    supplied (via a parameter substitution of the form 'param(...)'
	//    or 'XXX_..._XXX').  If the value indicates that no substitution
	//    has been performed then treat this as equivalent to a null value.
	//
	//alert("write_back_to_results_link");

	if ( "" + sessionfile == "undefined" ||
		 sessionfile.toLowerCase() == "default" ||
		 sessionfile.toLowerCase().indexOf( 'param(' ) == 0 ||
		 sessionfile.indexOf( 'XXX_' ) == 0 )
	{
		sessionfile = "";
	}

	if ( "" + searchscreen == "undefined" ||
		 searchscreen.toLowerCase() == "default" ||
		 searchscreen.toLowerCase().indexOf( 'param(' ) == 0 ||
		 searchscreen.indexOf( 'XXX_' ) == 0 )
	{
		searchscreen = "";
	}

	// Check for size
	if ( "" + size == "undefined" ||
		 size.toLowerCase().indexOf( 'param(' ) == 0 ||
		 size.indexOf( 'XXX_' ) == 0 )
	{
		size = "10";
	}

	// Check for sort order to display results in
	if ( "" + display == "undefined" ||
		 display.toLowerCase().indexOf( 'param(' ) == 0 ||
		 display.indexOf( 'XXX_' ) == 0 )
	{
		display = "";
	}

	// Check for inclusion of ECCO records
	if ( "" + ecco == "undefined" ||
		 ecco.toLowerCase().indexOf( 'param(' ) == 0 ||
		 ecco.indexOf( 'XXX_' ) == 0 )
	{
		ecco = "";
	}

	if ( ("" + kw_from_lion != "undefined") && (kw_from_lion.toLowerCase() != "default")
		&& (kw_from_lion.toLowerCase() != "param(highlight_keyword)") )
	{
		searchscreen = "";
	}

	//  Output the link if a session file and search screen were specified
	if ( sessionfile && searchscreen )
	{
		var path = searchscreen == "search_periodicals.htx" ?
						"/periodicals_search/search" : "/search";
		var source = searchscreen == "search_periodicals.htx" &&
					 display.indexOf( "CITATIONS" ) == 0 ?
					 "citations_config.cfg" :
					 searchscreen == "search_periodicals.htx" ?
					 "issues_config.cfg" :
					 "config.cfg"
		// override with alt-spelling config if present
		if ( "" + altsource == "undefined" )
		{
			// do nothing
		}
		else if ( altsource == "alt_spell.cfg" ) // special case
		{
			source = altsource;
		}
		else if ( altsource == "var_spell.cfg" ) // special case
		{
			source = altsource;
		}
		else if ( altsource == "var_form.cfg" ) // special case
		{
			source = altsource;
		}

		var url = path +
				  "?ACTION=GOTO" +
				  "&SOURCE=" + source +
				  "&FILE=" + sessionfile +
				  "&SEARCHSCREEN=" + searchscreen +
				  "&DISPLAY=" + display +
				  "&ECCO=" + ecco +
				  "&SIZE=" + size;
		document.write( "<A HREF=\"" + url + "\">" + text + "</A>" );
	}
}

//  Output the 'Next / Previous record' links which appear on the Full Citation page
// following a search.
//
function write_next_prev_link(sessionfile, entry, entries, searchscreen, display, source, highlightkw, ecco)
{
	// document.write('test3!');

	entryno = Number(entry);
	var spacer = " | ";
	// document.write( entryno + " " + entries );
	if ( "" + entry == "undefined" )
	{
		return;
	}
	if ( entryno > 1 )
	{
		var prevno = entryno - 1;
		var prevtext = "Previous Record";
		var prevurl = "/search/full_rec" +
				  "?ACTION=SINGLE";
		if ( "" + source == "undefined" )
		{
			prevurl += "&SOURCE=config.cfg";
		}
		else if ( source == "param(SOURCE)" )
		{
			url += "SOURCE=config.cfg";
		}
		else if ( source == "param(SEARCHCONFIG)" )
		{
			url += "SOURCE=config.cfg";
		}
		else if ( source == "default" )
		{
			url += "SOURCE=config.cfg";
		}
		else
		{
			prevurl += "&SOURCE=";
			prevurl += source;
		}
		prevurl += "&ECCO=" + ecco;
		prevurl +=	  "&SUBSET=" + prevno +
				  "&FILE=" + sessionfile +
				  "&ENTRIES=" + entries;
		if ( searchscreen != "" ) prevurl += "&SEARCHSCREEN=" + searchscreen;
		if ( display != "" ) prevurl += "&DISPLAY=" + display;
		document.write( "<A HREF=\"" + prevurl + "\">" + prevtext + "</A>" );
	}
	else
	{
		spacer = "";
	}
	if ( entryno == entries )
	{
		spacer = "";
	}
	if ( entryno < entries )
	{
		var nextno = entryno+1;
		var nexttext = "Next Record";
		var nexturl = "/search/full_rec" +
				  "?ACTION=SINGLE";
		if ( "" + source == "undefined" )
		{
			nexturl += "&SOURCE=config.cfg";
		}
		else if ( source == "param(SOURCE)" )
		{
			url += "SOURCE=config.cfg";
		}
		else if ( source == "param(SEARCHCONFIG)" )
		{
			url += "SOURCE=config.cfg";
		}
		else if ( source == "default" )
		{
			url += "SOURCE=config.cfg";
		}
		else
		{
			nexturl += "&SOURCE=";
			nexturl += source;
		}
		nexturl += "&ECCO=" + ecco;
		nexturl +=	  "&SUBSET=" + nextno +
				  "&FILE=" + sessionfile +
				  "&ENTRIES=" + entries;
		if ( searchscreen != "" ) nexturl += "&SEARCHSCREEN=" + searchscreen;
		if ( display != "" ) nexturl += "&DISPLAY=" + display;
		document.write (spacer);
		document.write( "<A HREF=\"" + nexturl + "\">" + nexttext + "</A>" );
	}
}

//  Display the 'Table of Contents' in a new window
//
function open_toc( url )
{
	window.open( url,
	             "toc",
	             "toolbar=yes,menubar=yes,scrollbars=yes,resizable=yes," +
	             "width=776,height=500" );
}

//  Output a 'Table of Contents' link, which opens the ToC in a new window
//
function write_toc_link( fulltextid, sessionfile, searchscreen, display, source )
{
	/*
	alert( "Writing ToC link for ID = '" + fulltextid + "' and " +
		   "session file = '" + sessionfile + "'" );
	*/

	var url = "/search/toc" +
			  "?ACTION=ExpandID" +
			  "&LEVEL=1" +
			  "&ID=" + fulltextid +
			  "&FILE=" + sessionfile +
			  "&SEARCHSCREEN=" + searchscreen +
			  "&DISPLAY=" + display;

	if ( "" + source == "undefined" )
	{
		// do nothing...
	}
	else if ( source == "" )
	{
		// do nothing...
	}
	else if ( source == "default" )
	{
		// do nothing...
	}
	else if ( source == "param(SOURCE)" )
	{
		url += "SOURCE=config.cfg";
	}
	else if ( source == "param(SEARCHCONFIG)" )
	{
		url += "SOURCE=config.cfg";
	}
	else
	{
		url += "&SOURCE=";
		url += source;
	}

	document.write( '<A HREF="javascript: open_toc( \'' + url + '\' )">' +
					'Table of Contents &#155;&#155;' +
					'</A>' +
					'&nbsp;' +
					'<IMG SRC="/images/popup_icon.gif" ' +
					'WIDTH="14" HEIGHT="14" BORDER="0" ' +
					'ALT="Opens in popup window">' );
}

//  Output a 'Printable Version' link in the form of a button
//
function write_printable_button( fulltextid, sessionfile, searchscreen, display )
{
	/*
	alert( "Writing ToC link for ID = '" + fulltextid + "' and " +
		   "session file = '" + sessionfile + "'" );
	*/

	var url = "/search/fulltext" +
			  "?source=configpr.cfg" +
			  "&ACTION=ByID" +
			  "&ID=" + fulltextid +
			  "&FILE=" + sessionfile +
			  "&DISPLAY=" + display;

	document.write( '<A HREF="' + url + '">' + 
					'<img src="/images/print_ver.gif" alt="Printable Version" title="Printable Version" border="0">' +
					'</A>' );
}

//  Output a 'Durable URL' button, which displays a URL which can be
//    bookmarked for the current page
//
function write_bookmark_button( pagename, openurlparams )
{
	var url = "/openurl" +
			  "?ctx_ver=Z39.88-2003" +
			  "&res_id=xri:eebo" +
			  "&" + openurlparams;
	var button = '<input type="button" ' +
	             'style="font-size: 12px; ' +
	             'font-family: Verdana; ' +
	             'font-weight: bold; color: #fff; ' +
	             'background-color: #0000FF; ' +
	             'border-top: 2px solid #ccc; ' +
	             'border-bottom: 2px solid #333; ' +
	             'border-left: 2px solid #ccc; ' +
	             'border-right: 2px solid #333;" ' +
	             'title="Durable URL.  Click to obtain a URL ' +
	             'for this page that can be saved as a bookmark.  ' +
	             'The URL will open in a separate window." ' +
	             'onclick="show_bookmark_url( \'' + pagename +
	             '\', \'' + url + '\' )" ' +
                 'value="Durable URL">';
    document.write( button );
}

//  Get the domain to use for Link Resolver URLs,
//	sensitive to whether we are on private, US 8086 or live site......
//
function get_link_resolver_domain( )
{
	var	sDomain	= new String("");
	
	if ( "eebo2.private.chadwyck.co.uk" == document.location.host )
	{
		sDomain = "http://devl-openurl.svc.il.pqe";
	}
	else if ( "eebo.chadwyck.com:8086" == document.location.host )
	{
		sDomain = "http://gateway.umi.bhowell.com";
	}
	else if ( "eebo.chadwyck.com" == document.location.host )
	{
		sDomain = "http://gateway.proquest.com";
	}
	else
	{
		sDomain = "http://gateway.proquest.com";
		// sDomain = "Unknown EEBO domain!";
	}
	
	return sDomain;
}

//  Display the durable URL for an image page
//
function show_durable_image_url( vid, pageno )
{
	var url = get_link_resolver_domain() + "/openurl" +
			  "?ctx_ver=Z39.88-2003" +
			  "&res_id=xri:eebo" +
			  "&rft_id=xri:eebo:image:" + vid + 
			  ( pageno && pageno > 1 ? ":" + pageno : "" );
	prompt( "The durable URL for this page image is:", url );
}

//  Display the durable URL for a citation
//
function show_durable_citation_url( eeboid )
{
	var url = get_link_resolver_domain() + "/openurl" +
			  "?ctx_ver=Z39.88-2003" +
			  "&res_id=xri:eebo" +
			  "&rft_id=xri:eebo:citation:" + eeboid;
	prompt( "The durable URL for this citation is:", url );
}

//  Display the URL for a 'Bookmark this page' button
//
function show_bookmark_url( pagename, path )
{
	var url = get_link_resolver_domain() + path;
	prompt( "The durable URL for this '" + pagename + "' page is:", url );
}

//  Display a Cyrillic character (e.g. in a Full Text page)
//
//  The Unicode entity reference should display correctly in MS Windows
//    versions of IE 4+ and Netscape 6+.
//
//  For other operating systems and browser versions, the text description
//    of the character should be displayed instead
//
function write_cyrillic_char( entity, description )
{
	/*
	{
		alert( "UserAgent = '" + navigator.userAgent + "'\n" +
			   "navigator.appName = '" + navigator.appName + "'\n" +
			   "navigator.appVersion = '" + navigator.appVersion + "'\n" );
	}
	*/
	//  Display the entity reference when:
	//
	//    - Using a Window OS (as opposed to Mac, Unix etc.)
	//    - Browser is Internet Explorer at version 4+
	//      or Netscape at version 6+
	//
	if ( navigator.userAgent.indexOf( "Win" ) != -1
	     &&
		 (
	       ( navigator.appName.indexOf( "Microsoft" ) == 0 &&
			 parseFloat( navigator.appVersion ) >= 4 )
		   ||
		   ( navigator.appName.indexOf( "Netscape" ) == 0 &&
		     parseFloat( navigator.appVersion ) >= 5 )
		 )
	   )
	{
		document.write( entity );
	}
	else
	{
		document.write( description );
	}
}

function set_spelling_option(altspell)
{
	// document.write( "set spelling here!" );
	// document.write( XXX_ALTSPELL_XXX );

    	if ( altspell == "Y" )
    	{
 	       	document.SEARCHFORM.ALTSPELLINGS.checked = true;
    	}
}

function write_hyperlink( field, text, ecco )
{
	//  Create a "hyperlink" from a field which will generate a new search
	var starturl = "/search/search?SEARCH=submit+search&ACTION=SearchOrBrowse&" + field + "=EXACT+%22";
	var endurl ="%22&RETRIEVETYPE=subset&HISTLOGGING=N&DATE1=1473&DATE2=1900&DUMMYHIT=&quot;%3CEEBOID%3E&quot;&SOURCE=config.cfg&SCREEN=search_advanced.htx";
	var respace = new RegExp(" ", "g");
	var recomma = new RegExp("\\,", "g");
	var rehitmark = new RegExp("<img.+>&nbsp;", "g");
	var rehitmark2 = new RegExp("<img.+>", "g");
	var rewh = new RegExp("<WORDH>", "g");
	var rewhend = new RegExp("</WORDH>", "g");
	var reft = new RegExp("<FULLTITLE.+?>", "g");
	var reftend = new RegExp("</FULLTITLE.+?>", "g");

	var fixedwhstart = text.replace(rewh, "");
	var fixedwhend = fixedwhstart.replace(rewhend, "");
	var fixedft = fixedwhend.replace(reft, "");
	var fixedftend = fixedft.replace(reftend, "");
	var fixedhitmark = fixedftend.replace(rehitmark, "");
	var fixedhitmark2 = fixedhitmark.replace(rehitmark2, "");
	var fixedspaces = fixedhitmark2.replace(respace, "%20");
	var fixedtext = fixedspaces.replace(recomma, "%2C");

	var finalurl = starturl + fixedtext + endurl;
	document.write("<a href ='");
	document.write(finalurl);
	if ( "" + ecco == "undefined" )
	{
		// do nothing
	}
	else
	{
		document.write("&ECCO=");
		document.write(ecco);
	}
	document.write("'>");
	document.write(text);
	document.write("</a>");
}

function write_ecco_link( link )
{
	var eebostr = new RegExp("docNum", "g");

	var dispurl = link.replace(eebostr, "<WBR>docNum");

	document.write("<a title='View this record in ECCO. ECCO will open in a new window. EEBO will remain open in the original window.' target='_blank' href ='");
	document.write(link);
	document.write("'>");
	document.write("View full record in ECCO <img src='/images/new_window.gif'>");
	document.write("</a>");
}

function write_ecco_branding( flag )
{
	// document.write("<br><a target='_blank' href ='http://www.gale.cengage.com/DigitalCollections/products/ecco/index.htm'>");
	document.write("<br><a target='_blank' href ='http://gdc.gale.com/products/eighteenth-century-collections-online/'>");
	document.write("<img src='/images/ECCO_citationlogo.gif' alt='View more information on ECCO. The ECCO website will open in a new window. EEBO will remain open in the original window.'>");
	document.write("</a>");
}

function write_gale_copyright()
{
	document.write("Copyright &copy; 2008 Gale");
}
function write_pre_som()
{
	// document.write( 'testing!' );
	// document.write( "<TR><TD WIDTH="20" VALIGN="TOP"></TD><TD WIDTH="20" VALIGN="TOP"><SCRIPT>write_markedlist_checkbox( '";

	// document.write(sceeboid);
	// document.write( "')</SCRIPT></TD>";
}

function write_pre_com()
{
	// document.write( 'testing!' );
	// document.write( scdate );
}

function write_post_com()
{
	// document.write( 'testing!' );
}


function check_tcp_subs( istcp2 )
{
    	if ( istcp2 == "X" )
    	{
		if ( tcp2 == "Y" )
		{
			// do nothing
		}
		else
		{
			document.write( 'This full text is not included in your subscription.' );
			document.write( '<div style=\"display:none\"' );
			document.write( '>' );
		}
	}
	else
	{
		if ( tcp == "Y" )
		{
			// do nothing
		}
		else
		{
			document.write( 'This full text is not included in your subscription.' );
			document.write( '<div style=\"display:none\"' );
			document.write( '>' );
		}
	}
	save_istcp2 = istcp2;
}

function end_check_tcp_subs( )
{
    	if ( save_istcp2 == "X" )
    	{
		if ( tcp2 == "Y" )
		{
			// do nothing
		}
		else
		{
			document.write( '<' );
			document.write( '/div' );
			document.write( '>' );
		}
	}
	else
	{
		if ( tcp == "Y" )
		{
			// do nothing
		}
		else
		{
			document.write( '<' );
			document.write( '/div' );
			document.write( '>' );
		}
	}
	
	save_istcp2 = "";
}

// -->
