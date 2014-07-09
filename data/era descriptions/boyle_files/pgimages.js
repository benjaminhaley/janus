<!--

//  JavaScript functions used by the 'Page Images', 'Illustrations' and
//    'Adjust View and Print' pages

// global variable for highlighting kws without a search (eg for results from a Lion search)
var currentkeyword = "";

//  Determine browser type (for event handling etc.)
var isNS4 = 0;
if ( navigator.appName.indexOf( "Netscape" ) == 0 &&
	 parseFloat( navigator.appVersion ) >= 4.0 &&
	 parseFloat( navigator.appVersion ) < 5.0 )
{
	isNS4 = 1;
}

//  Declare variables representing the current state
var currentid = "";
var currentvid = "";
var currentpage = 1;
var currentnumpages = 0;
var currentpdfname = "";
var currentslbrand = "";
var currentslname = "";
var currenttiffname = "";

//  Session file and search screen name.  This is used to generate the
//    '<< Back to results' link (which is only present on pages which
//    have been reached from a set of search results).
//
//  This needs to be passed through in URLs such as "Next Image >>" links
//    and when submitting forms (such as 'Go to image number') so that
//    the "<< Back to results" link does not disappear.
var currentsessionfile = "";
var currentsearchscreen = "";

//  Configuration file (e.g. "pgimages.cfg" or "pgimgadjust.cfg").
//    Needed when generating the "Next Image >>" links
var currentconfigfile = "";

//  Width and height in pixels of current page's TIFF image.
var currentimagewidth = 0;
var currentimageheight = 0;

//  Width and height in pixels of current page's GIF image -- obtained
//    by scaling the TIFF image according to the current zoom percentage.
//
//  Note that it is assumed that the computer monitor has 100dpi resolution,
//    compared with 400dpi for the scanned TIFF images.  Consequently a
//    4000x3000 TIFF image, at a zoom level of 100%, results in a GIF image
//    of size 1000x750 being displayed on the screen.
var currentscaledwidth = 0;
var currentscaledheight = 0;

//  Current zoom percentage (e.g. 100).
//
//  The CGI parameter 'ZOOM' should either be an integer specifying such a 
//    percentage (within the range 10 - 400) or "FIT" (the default)
//    indicating that a default zoom level should be calculated so that
//    the scaled GIF image fits on the page.
var currentzoom = 0;
var currentzoomparam = "FIT";
var currentdefaultzoom;

//  Minimum and maximum values for the zoom percentage
var minzoom = 10;
var maxzoom = 400;

//  Width and height in pixels of the viewport in which the scaled GIF
//    image is to be displayed
//
//  On the 'Adjust View and Print' page, the 'VIEWPORT' parameter explicitly
//    specifies a requested width and height for the viewport, e.g. '640x480'.
//
//  For the 'Document Image Page' the entire scaled image is displayed:
//    any viewport setting is just used when computing the default zoom
//    level that makes the image fit on the page.
var currentviewportparam = "640x480";
var currentviewportwidth = 640;
var currentviewportheight = 480;

//  Width and height in pixels of the image that gets displayed in the viewport.
//
//  If the scaled GIF page image is bigger than the requested viewport
//    then these image dimensions will be the same as the viewport
//    dimensions (and the page image will be cropped).
//
//  Otherwise, if the GIF page image is smaller than the requested
//    viewport, these will be the dimensions of this whole page image;
//    and when displayed, there will be extra padding around the image
//    (because the table cell that emcloses the image is sized to match
//    the requested viewport dimensions).
//
var currentviewportimagewidth;
var currentviewportimageheight;

//  URL of the currently displayed image
//
//  This is used by the 'Print current view' link, which displays a
//    separate page containing just this image, for printing.
var currentimageurl;

//  Current value of the 'CENTREPOS' parameter that is used to specify
//    an explicit position in the image that is to be placed at the
//    centre-point of the viewport.
//
//  This is expressed as a pair 'Xpos,Ypos' with each being an integer
//    between 0 and 10000, representing a proportion of the width and
//    height of the image.  (These proportional values are used instead
//    of pixel coordinates so as to be invariant when the image is
//    being zoomed at the same time as being re-centred)
var currentcentrepos = "";

//  Approximate position of the mouse pointer relative to the top-left 
//    corner of the page image displayed in the viewport.
//
//  These are continually updated by the mouse position tracking 
//    function, so that when the hyperlinked image is clicked on
//    (to do a recentre or zoom of the image) they give the pixel
//    coordinates where the image was clicked.
//
//  (Note that this roundabout mechanism is only used because of the
//    requirement to support Netscape 4.  Later browser versions provide
//    the more obvious mechanism of an 'OnClick' handler for the image
//    element; plus the methods needed to obtain the coordinates of the
//    click and the position of the image).
//
var currentmousex = 0;
var currentmousey = 0;

//  Pixel coordinates for the centre of the image being displayed in the
//    viewport of the 'Adjust View and Print' page.
//
//  These are coordinates in the scaled GIF image, in the range:
//
//      0 <= x <= currentscaledwidth; 0 <= y <= currentscaledheight
//
//  Note that any centre coordinates will be adjusted so that the 
//    viewport does not extend beyond the edge of the image.
//
var currentcentrex;
var currentcentrey;

//  Cropping to be performed on the scaled GIF image, when displaying it
//    in the viewport on the 'Adjust View and Print' page.
//
//  These values all indicate the number of pixels to remove from the
//    four edges of the GIF image (whose dimensions are given by the
//    'currentscaledwidth' and 'currentscaledheight' variables).
//
//  Setting all these variables to zero indicates that no cropping is
//    being done, and the entire page image is to be displayed.
//
var currentcropleft = 0;
var currentcropright = 0;
var currentcroptop = 0;
var currentcropbottom = 0;

//  Location of the left, right, top and bottom edges of the viewport,
//    as proportional values (in the range 0 to 10000) of the width
//    and height of the page image (i.e. the same coordinate system
//    as used for the 'CENTREPOS' parameter) -- used when calculating
//    new values for the viewport centre-point during panning of the
//    image.
//
var currentleftpos = 0;
var currenttoppos = 0;
var currentrightpos = 10000;
var currentbottompos = 10000;

//  Declare the arrays that are populated from the fields in the
//    citation record.
//
var allvids       = new Array();
var allnumpages   = new Array();
var allimages     = new Array();
var allpagerefs   = new Array();
var allprodcodes  = new Array();
var allreelposns  = new Array();
var alltotalsizes = new Array();
var allpdfnames   = new Array();
var allslbrands   = new Array();
var allslnames    = new Array();

var allissuenos   = new Array();
var alldatesfrom  = new Array();
var alldatesto    = new Array();

//  Variable to hold all illustration types (and counts) for all
//    the image sets (e.g. "Map=2|Chart=4|Plan=1")
//
var illustrations = "";

//  Declare variables containing details for each separate image set.
//    These are populated when 'build_image_lists()' processes the
//    'allvids', 'allnumpages', 'allimages', 'allftpagerefs' and
//    'allpdfnames' arrays (and, for periodicals only, the 'alldatesfrom',
//    'alldatesto' and 'allissuenos' arrays)
//
//  For the i'th image set:
//    numpages[i]  -- Number of page images
//    numillus[i]  -- Number of illustrations
//    illtypes[i]  -- Illustration type(s) and counts (e.g. "Map=1|Chart=3")
//    prodname[i]  -- Product name, e.g. "Early English Books, 1475 - 1640"
//    prodabbr[i]  -- Product abbreviation, e.g. "STC", "Wing" or "TT"
//    reelposn[i]  -- Reel position, e.g. "353:08"
//    totalsize[i] -- Total size in bytes (of the image set's TIFF files)
//    pdfname[i]   -- Filename to use for downloaded PDF files
//    slbrand[i]   -- Source library branding for image set
//    slname[i]    -- Source Library name
//    -- Plus, for periodicals, with each image set being a separate issue:
//    datefrom[i]  -- Start date for the issue, as 8 digits YYYYMMDD
//    dateto[i]    -- End date for the issue, as 8 digits YYYYMMDD
//    issueno[i]   -- Issue number for the issue
//
//  For the j'th image in the i'th image set:
//    imagewidths[i][j]  -- Width in pixels
//    imageheights[i][j] -- Height in pixels
//    imagetypes[i][j]   -- Illustration type(s) (e.g. "Map|Chart")
//
//  For the j'th image (in any image set):
//    ftdivlevel[j]   -- Division level (for link to full-text)
//    ftdivref[j]     -- Division reference (for link to full-text)
//    ftdivsize[j]    -- Division size in Kb (for link to full-text)
//
//  For all the image sets associated with a document
//    illustrations -- Illustration type(s) and counts (as for illtypes)
//
var numpages     = new Array();
var numillus     = new Array();
var illtypes     = new Array();
var prodname     = new Array();
var prodabbr     = new Array();
var reelposn     = new Array();
var totalsize    = new Array();
var pdfname      = new Array();
var slbrand      = new Array();
var slname       = new Array();

var datefrom     = new Array();
var dateto       = new Array();
var issueno      = new Array();

var imagewidths  = new Array();
var imageheights = new Array();
var imagetypes   = new Array();

var ftdivlevel   = new Array();
var ftdivref     = new Array();
var ftdivsize    = new Array();

//  Utility function to simulate the 'Array.push()' method -- which is
//    not supported on certain browsers
//
function array_push( array, value )
{
	array[ array.length ] = value;
}

//  Store the CGI parameters specifying the record ID, VID, page number,
//    zoom percentage and viewport dimensions to be used for this page.
//
//  Note that this is called after the list of VIDs and images have been
//    populated into the 'allvids' and 'allimages' arrays; so that the
//    parameters can be validated and defaulted appropriately.
function init_page( id, vid, pageno, zoom, viewport, sessionfile, searchscreen )
{
	//  If a CGI parameter does not exist, it will not have been
	//    replaced in the 'param(...)' substitution string.
	//
	//  Treat these as equivalent to empty parameters
	//
	if ( id.indexOf( "param(" ) != -1 ) id = "";
	if ( vid.indexOf( "param(" ) != -1 ) vid = "";
	if ( pageno.indexOf( "param(" ) != -1 ) pageno = "";
	if ( zoom.indexOf( "param(" ) != -1 ) zoom = "";
	if ( viewport.indexOf( "param(" ) != -1 ) viewport = "";
	if ( sessionfile.indexOf( "param(" ) != -1 ||
		 sessionfile == "default" ) sessionfile = "";

	/*
	alert( "Running init_page: ID = '" + id + "', " +
		   "VID = '" + vid + "', " +
		   "PageNo = '" + pageno + "', " +
		   "Zoom = '" + zoom + "', " +
		   "SessionFile = '" + sessionfile + "', " +
		   "Viewport = '" + viewport + "' " );
	*/

	//  Store the current session file and search screen
	currentsessionfile = sessionfile;
	currentsearchscreen = searchscreen;

	//  Build the lists of image details
	build_image_lists();

	//  Identifier used to select the current record -- always present.
	//    May be a numeric EEBO ID, or an alphanumeric VID number with
	//    a 'V' prefix (this latter case occurs with bookmarked page
	//    images)
	//
	currentid = id;

	//  Determine which VID to display.  This may have been explicitly
	//    selected with the VID or ID parameters; otherwise just default
	//    to the first image set that has some images
	//
	var selectedvidindex = -1;
	if ( vid != "" || id.substring( 0, 1 ) == "V" )
	{
		//  A VID has been requested; look it up in the list of
		//    available VIDs for this record
		selectedvidindex = get_selected_vid_index( id, vid );
	}
	// alert( "init_page: selectedvidindex = '" + selectedvidindex + "'" );

	if ( selectedvidindex >= 0 )
	{
		//  An existing VID has been specified.  Display this image set.
		currentvid = allvids[ selectedvidindex ];
	}
	else
	{
		//  No VID specified.  Use the first one with some images
		currentvid = "NONE";
		for ( i = 0; i < allvids.length; i++ )
		{
			if ( numpages[ i ] > 0 )
			{
				currentvid = allvids[ i ];
				selectedvidindex = i;
				break;
			}
		}
	}
	// alert( "init_page: set currentvid = '" + currentvid + "'" );

	//  Number of page images for the current VID
	currentnumpages = numpages[ selectedvidindex ];
	//alert( "init_page: set currentnumpages = '" + currentnumpages + "'" );

	//  PDF filename to use when downloading images for this VID
	currentpdfname = pdfname[ selectedvidindex ];
	currentslbrand = slbrand[ selectedvidindex ];
	currentslname  = slname[ selectedvidindex ];

	//  Page number -- defaults to 1
	if ( ! is_digits( pageno ) )
	{
		//  No page number supplied.  Default to 1
		currentpage = 1;
	}
	else if ( pageno - 0 < 1 )
	{
		currentpage = 1;
	}
	else if ( pageno - 0 > currentnumpages )
	{
		currentpage = currentnumpages;
	}
	else
	{
		currentpage = pageno - 0;
	}
	//alert( "init_page: set currentpage = '" + currentpage + "'" );

	set_zoom_and_viewport( zoom, viewport );

	/*
	alert( "Settings are: ConfigFile = '" + currentconfigfile + ", " +
		   "ID = '" + currentid + "', " +
		   "VID = '" + currentvid + "', " +
		   "Page = '" + currentpage + "', " +
		   "Viewport = '" + currentviewportwidth +
		   "x" + currentviewportheight + "', " +
		   "Zoom = '" + currentzoom + "', " + 
		   "Width = " + currentimagewidth + ", " +
		   "Height = " + currentimageheight );
	*/
}

//  Calculate the zoom level, image dimensions etc. from the
//    combination of requested zoom level and viewport size.
//
//  If a zoom level is specified without a viewport size, the
//    viewport is calculated so as to enclose the whole image:
//    this is used for the 'Document Images' page, which always
//    displays an entire page image.
//
//  If a viewport size is specified without a zoom level, the 
//    zoom level is calculated so as to fit the whole page image
//    into the viewport: this is the 'Default (xx%)' option in
//    the zoom-level drop-down.
//
//  If neither zoom level nor viewport size is specified, a
//    default viewport size is assumed (e.g. 640x480) and the
//    default zoom level which fits the image to this viewport
//    is used.
//
//  If both a zoom level and viewport are specified, the image
//    is scaled to the requested size and cropped to fit the
//    requested viewport; this is only used for the 'Adjust View
//    and Print' page.
//
function set_zoom_and_viewport( zoom, viewport )
{
	//  Viewport dimensions (e.g. "640x480")
	currentviewportparam = "";
	var dimensions = viewport.toUpperCase().split( "X" );
	if ( dimensions.length == 2 &&
		 is_digits( dimensions[ 0 ] ) &&
		 is_digits( dimensions[ 1 ] ) )
	{
		currentviewportwidth = dimensions[ 0 ];
		currentviewportheight = dimensions[ 1 ];
		currentviewportparam = viewport;
	}

	//  Compute a default zoom level that makes the scaled GIF image
	//    fit into the requested viewport.  This level gets displayed
	//    in the drop-down list of zoom levels
	//
	currentdefaultzoom = compute_default_zoom();

	//  Image zoom level
	if ( ! is_digits( zoom ) )
	{
		//  No explicit zoom percentage supplied.  Use the default
		currentzoom = currentdefaultzoom;
		currentzoomparam = "FIT";
	}
	else if ( zoom < minzoom )
	{
		currentzoom = minzoom;
		currentzoomparam = currentzoom;
	}
	else if ( zoom > maxzoom )
	{
		currentzoom = maxzoom;
		currentzoomparam = currentzoom;
	}
	else
	{
		currentzoom = zoom;
		currentzoomparam = currentzoom;
	}
	/*
	alert( "set_viewport_and_zoom: set currentzoom = '" + currentzoom + "', " +
		   "currentzoomparam = '" + currentzoomparam + "'" );
	*/

	//  Obtain the dimensions for the current page's image
	//
	var vidindex = get_vid_index( currentvid );

	currentimageheight = imageheights[ vidindex ][ currentpage - 1 ];
	currentimagewidth  = imagewidths[ vidindex ][ currentpage - 1 ];

	//  Note that the TIFF images are scanned at 400dpi.  Assuming that
	//    the computer displays images at 100dpi, a TIFF image that is
	//    4000 pixels wide will display as a GIF image 1000 pixels wide,
	//    when viewed at a zoom percentage of 100%.
	//
	var zoomfactor = currentzoom * 0.0025;

	currentscaledwidth  = Math.round( currentimagewidth  * zoomfactor );
	currentscaledheight = Math.round( currentimageheight * zoomfactor );

	//  Determine the dimensions of the image that will be displayed
	//    within the viewport.
	//
	if ( currentscaledwidth < currentviewportwidth )
	{
		currentviewportimagewidth = currentscaledwidth;
	}
	else
	{
		currentviewportimagewidth = currentviewportwidth;
	}

	if ( currentscaledheight < currentviewportheight )
	{
		currentviewportimageheight = currentscaledheight;
	}
	else
	{
		currentviewportimageheight = currentviewportheight;
	}

	/*
	alert( "set_viewport_and_zoom: scaled GIF image = " +
		   currentscaledwidth + "x" + currentscaledheight + "\n" +
		   "viewport = " + currentviewportwidth + "x" +
		   currentviewportheight + "\n" +
		   "viewport image = " + currentviewportimagewidth + "x" +
		   currentviewportimageheight );
	*/
}

//  Determine the cropping that is to be done on the scaled GIF image
//    displayed in the 'Adjust View and Print' page (so as to display
//    the relevant portion of this image in the viewport).
//
//  Note that this function is called after 'init_page', so that all
//    the details about the current page image, zoom percentage and
//    viewport dimensions are already available.
//
//  The parameter specifies the proportional location in the image
//    which should be the centre-point of the viewport.  This location
//    is given as an (x,y) coordinate pair, with x and y both integers
//    in the range 0 to 10000.
//
function set_cropping( centrepos )
{
	//alert( "set_cropping: CentrePos = '" + centrepos + "'" );

	//  Interpret the proportional position in the page image and convert
	//    to (x,y) pixel coordinates in the scaled GIF image.
	//
	var pos = centrepos.split( "," );
	currentcentrepos = "";
	if ( pos.length == 2 &&
		 is_digits( pos[ 0 ] ) && is_digits( pos[ 1 ] ) &&
		 pos[ 0 ] >= 0 && pos[ 0 ] <= 10000 &&
		 pos[ 1 ] >= 0 && pos[ 1 ] <= 10000 )
	{
		//  Store the explicitly supplied centre position
		currentcentrepos = centrepos;
		//  Determine the centre point in the scaled GIF image
		currentcentrex = Math.round( 0.0001 * pos[ 0 ] * currentscaledwidth );
		currentcentrey = Math.round( 0.0001 * pos[ 1 ] * currentscaledheight );
	}
	else
	{
		//  No coordinates specified; use the midpoint of the image
		currentcentrex = Math.round( 0.5 * currentscaledwidth );
		currentcentrey = Math.round( 0.5 * currentscaledheight );
	}

	//  Compute the dimensions of the image to the left and right
	//    and top and bottom of the centre point
	//
	var imageleft    = currentcentrex;
	var imageright   = currentscaledwidth - currentcentrex;
	var imagetop     = currentcentrey;
	var imagebottom  = currentscaledheight - currentcentrey;

	//  Compute the dimensions of the viewport image to the left and right
	//    and top and bottom of the midpoint
	//
	var viewportleft   = Math.floor( currentviewportimagewidth / 2 );
	var viewportright  = currentviewportimagewidth - viewportleft;
	var viewporttop    = Math.floor( currentviewportimageheight / 2 );
	var viewportbottom = currentviewportimageheight - viewporttop;

	//  Adjust the centre coordinates to ensure that the whole of
	//    the viewport is within the boundary of the image
	//
	var shiftamount;
	if ( viewportleft > imageleft )
	{
		//  Viewport is off the left edge of the image;
		//    move the centre point right
		shiftamount = viewportleft - imageleft;
		currentcentrex += shiftamount;
		imageleft      += shiftamount;
		imageright     -= shiftamount;
	}
	if ( viewportright > imageright )
	{
		//  Viewport is off the right edge of the image;
		//    move the centre point left
		shiftamount = viewportright - imageright;
		currentcentrex -= shiftamount;
		imageleft      -= shiftamount;
		imageright     += shiftamount;
	}
	if ( viewporttop > imagetop )
	{
		//  Viewport is off the top edge of the image;
		//    move the centre point down
		shiftamount = viewporttop - imagetop;
		currentcentrey += shiftamount;
		imagetop       += shiftamount;
		imagebottom    -= shiftamount;
	}
	if ( viewportbottom > imagebottom )
	{
		//  Viewport is off the bottom edge of the image;
		//    move the centre point up
		shiftamount = viewportbottom - imagebottom;
		currentcentrey -= shiftamount;
		imagetop       -= shiftamount;
		imagebottom    += shiftamount;
	}

	//  Compute the cropping for the image
	//
	currentcropleft   = imageleft - viewportleft;
	currentcropright  = imageright - viewportright;
	currentcroptop    = imagetop - viewporttop;
	currentcropbottom = imagebottom - viewportbottom;

	//  Determine the proportional positions of the viewport's edges
	//    on the page image as a whole.  These are expressed as integers
	//    in the range 0 to 10000.
	//
	currentleftpos     = Math.round( 10000 * currentcropleft / currentscaledwidth );
	currenttoppos      = Math.round( 10000 * currentcroptop / currentscaledheight );
	currentrightpos    = 10000 - Math.round( 10000 * currentcropright / currentscaledwidth );
	currentbottompos   = 10000 - Math.round( 10000 * currentcropbottom / currentscaledheight );

	/*
	alert( "set_cropping: settings are:\n" +
		   "CentrePos = " + currentcentrepos + "\n" +
		   "TIFF image dimensions = " + currentimagewidth + "x"
		   + currentimageheight + "\n" +
		   "Scaled GIF image dimensions = " + currentscaledwidth + "x"
		   + currentscaledheight + "\n" +
		   "Viewport dimensions = " + currentviewportwidth + "x"
		   + currentviewportheight + "\n" +
		   "Viewport image dimensions = " + currentviewportimagewidth + "x"
		   + currentviewportimageheight + "\n" +
		   "Centre point coords = (" + currentcentrex + ","
		   + currentcentrey + ")\n" +
		   "image cropping = " +
		   "left: " + currentcropleft + ", " +
		   "right: " + currentcropright + ", " +
		   "top: " + currentcroptop + ", " +
		   "bottom: " + currentcropbottom + "\n" +
		   "viewport posistion = " +
		   "left: " + currentleftpos + ", " +
		   "right: " + currentrightpos + ", " +
		   "top: " + currenttoppos + ", " +
		   "bottom: " + currentbottompos + "\n" );
	*/
}
//alert( "Completed definition of function: set_cropping ..." );

//  Given a VID, return the array index according to whether it is
//    the 1st, 2nd, 3rd etc. image set for the document.
//
function get_vid_index( vid )
{
	var i;
	for ( i = 0; i < allvids.length; i++ )
	{
		if ( allvids[ i ] == vid ) return i;
	}
	return -1;
}
//alert( "Completed definition of function: get_vid_index ..." );

//  Build the data structures listing the page and illustration images
//    for each of the image sets associated with the document
//
//  This is called when the page has loaded; at which point the arrays
//    'allvids' and 'allimages' will have been filled in.
//
function build_image_lists()
{
	var i, j;

	//  For each image set, initialise the various data structures
	/*
	alert( "build_image_lists: creating arrays for " + allvids.length + " VIDs: " + allvids.join( ", " ) + " ..." );
	*/
	for ( i = 0; i < allvids.length; i++ )
	{
		numpages[ i ]     = allnumpages[ i ] - 0;
		numillus[ i ]     = 0;
		illtypes[ i ]     = "";
		imagewidths[ i ]  = new Array();
		imageheights[ i ] = new Array();
		imagetypes[ i ]   = new Array();
		prodname[ i ]     = ( allprodcodes[ i ] == 36 )
									? "Early English Books, 1475 - 1640" :
							( allprodcodes[ i ] == 37 )
									? "Early English Books, 1641 - 1700" :
							( allprodcodes[ i ] == 2105 )
									? "Tract Supplement" :
							( allprodcodes[ i ] == 51 )
									? "Thomason Tracts" : "";
		prodabbr[ i ]     = ( allprodcodes[ i ] == 36 ) ? "STC" :
							( allprodcodes[ i ] == 37 ) ? "Wing" :
							( allprodcodes[ i ] == 2105 ) ? "Tract Supplement" :
							( allprodcodes[ i ] == 51 ) ? "Thomason" : "";
		reelposn[ i ]     = allreelposns[ i ];
		totalsize[ i ]    = alltotalsizes[ i ] - 0;
		pdfname[ i ]      = allpdfnames[ i ];
		slbrand[ i ]     = allslbrands[ i ];
		slname[ i ]      = allslnames[ i ];
		
		//  Following fields are only present for periodicals
		if ( i < allissuenos.length &&
		     i < alldatesfrom.length &&
		     i < alldatesto.length )
		{
			datefrom[ i ] = alldatesfrom[ i ];
			dateto[ i ]   = alldatesto[ i ];
			issueno[ i ]  = allissuenos[ i ];
		}
	}

	//  Work through the lists of image files, each specifying a VID,
	//    filename, width and height, and any illustration types, e.g.
	//
	//    '11567|eebo/e0017/11567/00001.000.001.tif|2503|4764|Chart|Map'
	//
	//  Note that the 'allimages' array is only populated for those pages
	//    that need details of individual images (i.e. 'Document Images',
	//    'Illustrations' and 'Adjust View and Print').  The array is not
	//    present for pages that need only the details of the image sets
	//    (i.e. 'Full Citation' and 'Image Sets')
	//
	for ( i = 0; i < allimages.length; i++ )
	{
		var imagefields = allimages[ i ].split( "|" );
		var vid         = imagefields[ 0 ];
		var filename    = imagefields[ 1 ];
		var width       = imagefields[ 2 ];
		var height      = imagefields[ 3 ];
		var types       = "";
		for ( j = 4; j < imagefields.length; j++ )
		{
			var type = imagefields[ j ];

			//  Accumulate the illustration types for this image
			types += ( types ? "|" : "" ) + type;

			//  Accumulate the illustration types for the whole image set
			var vidindex = get_vid_index( vid );
			illtypes[ vidindex ] = add_illustration_type( type, illtypes[ vidindex ] );

			//  Accumulate the illustration types for all the image sets
			//    associated with the document
			illustrations = add_illustration_type( type, illustrations );
		}

		/*
		alert( "Read image: '" + allimages[ i ] + "' : \n" +
			   "VID = '" + vid + "' \n" +
			   "Filename = '" + filename + "' \n" +
			   "Width = '" + width + "' \n" +
			   "Height = '" + height + "' \n" +
			   "Illustration(s) = '" + types + "' \n" );
		*/
		if  (vid != "")
		{
			//  Get the array index corresponding to this image's VID
			var vidindex = get_vid_index( vid );

			//  Count the illustration types for this VID
			if ( types != "" )
			{
				numillus[ vidindex ] ++;
			}

			//  Fill in the details for this page of the image set
			var pageindex = imagewidths[ vidindex ].length;
			imagewidths[ vidindex ][ pageindex ]  = width
			imageheights[ vidindex ][ pageindex ] = height
			imagetypes[ vidindex ][ pageindex ]   = types;
		}
	}

	//  For the 'Document Images' page: work through the list of division
	//    levels, identifiers and sizes used to link into the full-text, and
	//    populate the 'ftdivlevel', 'ftdivref' and 'ftdivsize[]' arrays.
	//
	for ( i = 0; i < allpagerefs.length; i++ )
	{
		//  Split the value into separate fields
		var pagereffields = allpagerefs[ i ].split( "|" );
		var pageno        = pagereffields[ 0 ];
		var divlevel      = pagereffields[ 1 ];
		var divref        = pagereffields[ 2 ];
		var divsize       = pagereffields[ 3 ];
		//  Store the division level and identifier for this page
		ftdivlevel[ pageno - 1 ] = divlevel;
		ftdivref[ pageno - 1 ] = divref;
		ftdivsize[ pageno - 1 ] = divsize;
	}

	/*
	alert( "After building image lists:\n" +
		   "  VIDs = " + allvids.join( ", " ) + "\n" +
		   "  No. pages = " + numpages.join( ", " ) + "\n" + 
		   "  Illustrations = " + illtypes.join( ", " ) + "\n" );
	*/
}
//alert( "Completed definition of function: build_image_lists ..." );

//  Add another illustration type (e.g. "Plan") to the string of
//    sorted types and counts(e.g. "Chart=1|Map=3|Portrait=1")
//    associated with an image set.  If the type is already present
//    then its count is incremented; if not present the type is added
//
//  The updated string of types and counts is returned.
//
function add_illustration_type( type, typecountstring )
{
	//  Split the string of illustration types and counts
	var typecountlist = new Array();
	if ( typecountstring != "" )
	{
		typecountlist = typecountstring.split( "|" );
	}

	//  Check whether the ilustration type is already present
	var i;
	var found = 0;
	for ( i = 0; i < typecountlist.length; i++ )
	{
		//  Split into a type and count
		var typecount = typecountlist[ i ].split( "=" );
		var typename  = typecount[ 0 ];
		var count     = typecount[ 1 ];
		if ( typename == type )
		{
			//  Found the type
			found = 1;
			//  Increment the count
			var newcount = count - 0 + 1;
			typecountlist[ i ] = type + "=" + newcount;
			break;
		}
	}

	//  If the type was not found, add it with a count of 1
	if ( ! found )
	{
		array_push( typecountlist, type + "=1" );
	}

	//  Return the sorted string of types and counts
	typecountlist.sort();
	typecountstring = typecountlist.join( "|" );
	return typecountstring;
}

//  Check whether a value is valid as an integer: i.e. a non-empty
//    string composed entirely of digits.
//
function is_digits( value )
{
	var i;
	//  Convert the value to a string
	value = value + "";
	//  Check whether the string is empty
	if ( value.length == 0 ) return false;
	//  Check whether any characters are non-digits
	for ( i = 0; i < value.length; i++ )
	{
		if ( value.charAt( i ) < "0" ||
			 value.charAt( i ) > "9" ) return false;
	}
	//  Value is composed entirely of digits
	return true;
}
//alert( "Completed definition of function: is_digits ..." );

//  Redisplay the 'Document Page Image' or 'Adjust View and Print' page;
//   after selecting a different page number or zoom level from the
//   controls on the page.
//
//  The page number is taken from:
//    - The optional argument (if supplied); or
//    - The value from the 'Go to image number' text box (if valid); or
//    - The current page number
//
//  The zoom level is taken from:
//    - The optional argument (if supplied); or
//    - The value from the 'Zoom to' text box (if valid); or
//    - The current zoom level
//
//  This function returns false, so as not to trigger a form
//    submission when used as a form's OnSubmit handler
//
function redisplay( newpage, newzoom )
{
	//alert( "called redisplay: newpage = '" + newpage + "', newzoom = '" + newzoom + "'" );
	//  Get any new page number.  Defaults to the current page.
	var pageno = currentpage;

	//  Use the supplied page number, if present
	if ( is_digits( newpage ) )
	{
		pageno = newpage;
	}

	//  Otherwise use any value entered in the 'Go to image number'
	//    text box (if valid)
	else
	{
		var text = document.CONTROLSFORM.GOTOPAGENO.value;
		//  Remove leading spaces
		while ( text.length > 0 && text.charAt( 0 ) == " " )
					text = text.substring( 1 );
		//  Remove trailing spaces
		while ( text.length > 0 && text.charAt( text.length - 1 ) == " " )
					text = text.substring( 0, text.length - 1 );
		//  If the resulting string is numeric, use this page number
		if ( is_digits( text ) ) pageno = text;
	}

    //  Get any new zoom percentage.  Defaults to the current zoom
	var zoom = currentzoomparam;

	//  Retrieve any values from the zoom level text box or drop-down
	var zoomtext = document.CONTROLSFORM.ZOOMTEXTBOX.value;
	var zoomselection = "";
	var zoomoptions = document.CONTROLSFORM.ZOOMLIST.options;
	var i;
	for ( i = 0; i < zoomoptions.length; i++ )
	{
		if ( zoomoptions[ i ].selected && zoomoptions[ i ].value )
					zoomselection = zoomoptions[ i ].value;
	}

	if ( is_digits( newzoom ) )
	{
		zoom = newzoom;
	}
	else if ( is_digits( zoomtext ) )
	{
		zoom = zoomtext;
	}
	else if ( zoomselection )
	{
		zoom = zoomselection;
	}

	//  Populate the form fields for the CGI parameters
	document.CONTROLSFORM.ID.value = currentid;
	document.CONTROLSFORM.FILE.value = currentsessionfile;
	document.CONTROLSFORM.SEARCHSCREEN.value = currentsearchscreen;
	document.CONTROLSFORM.VID.value = currentvid;
	document.CONTROLSFORM.PAGENO.value = pageno;
	document.CONTROLSFORM.ZOOM.value = zoom;
	document.CONTROLSFORM.VIEWPORT.value = currentviewportparam;
	document.CONTROLSFORM.CENTREPOS.value = currentcentrepos;

	return true;
}
//alert( "Completed definition of function: redisplay ..." );

//  Calculate a default zoom percentage for the current image
//
//  The default is calculated so that the image will not exceed the
//    current viewport dimensions
//
function compute_default_zoom()
{
	var maxdisplaywidth  = currentviewportwidth;
	var maxdisplayheight = currentviewportheight;

	//  Obtain the dimensions of the current page image
	//
	var vidindex    = get_vid_index( currentvid );
	var pageindex   = currentpage - 1;
	var imagewidth  = imagewidths[ vidindex ][ pageindex ];
	var imageheight = imageheights[ vidindex ][ pageindex ];

	//  Compute the displayed width at 100% zoom
	//
	var defaultzoom   = 100;
	var displaywidth  = imagewidth  * defaultzoom * 0.0025;
	var displayheight = imageheight * defaultzoom * 0.0025;

	//  Scale the image down if too wide
	//
	if ( displaywidth > maxdisplaywidth )
	{
		defaultzoom   *= maxdisplaywidth / displaywidth;
		displaywidth  = imagewidth  * defaultzoom * 0.0025;
		displayheight = imageheight * defaultzoom * 0.0025;
	}

	//  Scale the image down if too tall
	//
	if ( displayheight > maxdisplayheight )
	{
		defaultzoom   *= maxdisplayheight / displayheight;
		displaywidth  = imagewidth  * defaultzoom * 0.0025;
		displayheight = imageheight * defaultzoom * 0.0025;
	}

	//  Return the computed zoom percentage
	defaultzoom = Math.floor( defaultzoom );

	/*
	alert( "compute_default_zoom: " +
		   "currentpage = " + currentpage + "\n" +
		   "viewport = " + currentviewportwidth +
		   "x" + currentviewportheight + "\n" +
		   "image = " + imagewidth +
		   "x" + imageheight + "\n" +
		   "defaultzoom = " + defaultzoom );
	*/

	return defaultzoom;
}
//alert( "Completed definition of function: compute_default_zoom ..." );

//  Output the list options for the 'Adjust image size' drop-down.
//
//  A 'default zoom' option will be calculated such that the image
//    is no larger than 640 pixels wide by 480 pixels tall.
//
//  If the current zoom matches one of the listed options, then
//    that option will be initially selected.
//
function write_size_options()
{
	var values = new Array( 25, 50, 75, 100, 150, 200, 300, 400 );
	var i;
	var text = "<SELECT NAME=\"ZOOMLIST\" SIZE=\"1\">\n" +
			   "<OPTION VALUE=\"\">Adjust image size</OPTION>\n";
	for ( i = 0; i < values.length; i++ )
	{
		text += "<OPTION VALUE=\"" + values[ i ] + "\" " +
				( currentzoomparam == values[ i ] ? "SELECTED" : "" ) + ">" +
				values[ i ] + "%</OPTION>\n";
	}
	text += "<OPTION VALUE=\"FIT\" " +
			( currentzoomparam == "FIT" ? "SELECTED" : "" ) + ">" +
			"Default (" + currentdefaultzoom + "%)</OPTION>\n";
	text += "</SELECT>";
	//alert( "Current zoom parameter = '" + currentzoomparam + "'" );
	//alert( "Image size options are:\n" + text );
	document.write( text );
}
//alert( "Completed definition of function: write_size_options ..." );

//  Get the URL for a specified page and zoom percentage
//
function get_url( configfile, vid, pageno, zoom, viewport, searchconfig, sortorder )
{
	var url = "/search/full_rec?SOURCE=" + configfile +
			  "&ACTION=ByID" +
			  "&ID=" + currentid +
			  "&FILE=" + currentsessionfile +
			  "&SEARCHSCREEN=" + currentsearchscreen +
			  "&VID=" + vid +
			  "&PAGENO=" + pageno +
			  "&ZOOM=" + zoom +
			  "&VIEWPORT=" + viewport +
			  "&SEARCHCONFIG=" + searchconfig +
			  "&DISPLAY=" + sortorder;
	// if ( currentkeyword != "" )
	// {
		url += "&HIGHLIGHT_KEYWORD=" + currentkeyword;
	// }
	return url;
}
//alert( "Completed definition of function: get_url ..." );

//  Output the HTML fragment for the "PDF of the current image" icon
//
function write_pdfcurrentpage_icon( )
{
	//  Form the URL for the PDF download of the current page,
	//    using the current VID and page number
	//
	//var url = "http://eebo.chadwyck.com:8080/downloadpdf" +
	var url = "/downloadpdf" +
			  "?id=" + currentid +
			  "&vid=" + currentvid +
			  "&filename=" + currentpdfname +
			  "&pages=" + currentpage;
	var icon = '<IMG SRC="/images/pdf_icon.gif" ' +
			   'WIDTH="16" HEIGHT="17" BORDER="0" ' +
			   'ALT="PDF of the current image" ' +
			   'TITLE="PDF of the current image">';
	document.write( '<A HREF="' + url + '">' + icon + '</A>' );
}
//alert( "Completed definition of function: write_pdfcurrentpage_icon ..." );

//  Output the HTML fragment for the source library name
//
function write_currentslname( )
{
	if ( currentslname &&
	     currentslname != "undefined" )
	{
		document.write( '<B>Copy from:</B> ' + currentslname );
	}
}

//  Output the HTML fragment for the source library branding
//  - needs to be in a table....
//
function write_currentslbrand( )
{
	if ( currentslbrand &&
	     currentslbrand != "undefined" )
	{
		document.write( '<TD><TABLE>\n');
		// only one - BL - library gif for now - expand if needed later by checking currentslbrand
		// was bl_logo_with_text.gif - now changed to match other logo following comments from Jo-Anne 20 Nov 06
		var blgif = '<IMG SRC="/images/bl_logo.gif" ' +
			   'WIDTH="52" HEIGHT="100" BORDER="0" ' +
			   'TITLE="http://www.bl.uk. The British Library website will open in a new window. EEBO will remain open in the original window.">';
		document.write( '<a href="http://www.bl.uk/" target="_blank" >' + blgif + '</a>');
		document.write( '</TABLE></TD>\n');
	}
}

//  Output the HTML fragment for the download "TIFF of the current image" link
//
function write_tiffcurrentpage_link()
{
	//  Form the URL for the PDF download of the current page,
	//    using the current VID and page number
	//
	//var url = "http://eebo.chadwyck.com:8080/downloadpdf" +
	var url = "/downloadtiff" +
			  "?vid=" + currentvid +
			  "&eeboid=" + currentid +
			  "&filename=" + currentpdfname +
			  "&page=" + currentpage;
	var icon = '<IMG SRC="/images/tiff_icon.gif" ' +
			   'WIDTH="18" HEIGHT="14" BORDER="0" >';
	document.write( icon + '&nbsp;<A HREF="' + url + '">Download TIFF file</A>' );
}
//alert( "Completed definition of function: write_tiffcurrentpage_icon ..." );

function write_shorttiffcurrentpage_link( )
{
	//  Form the URL for the PDF download of the current page,
	//    using the current VID and page number
	//
	//var url = "http://eebo.chadwyck.com:8080/downloadpdf" +
	var url = "/downloadtiff" +
			  "?vid=" + currentvid +
			  "&eeboid=" + currentid +
			  "&filename=" + currentpdfname +
			  "&page=" + currentpage;
	var icon = '<IMG SRC="/images/tiff_icon.gif" ' +
			   'WIDTH="18" HEIGHT="14" BORDER="0" ' +
			   'ALT="TIFF of the current image" TITLE="TIFF of the current image">';
	document.write( '<A HREF="' + url + '">' + icon + '</A>' );
}

function write_texttiffcurrentpage_link( linktext )
{
	//  Form the URL for the PDF download of the current page,
	//    using the current VID and page number
	//
	//var url = "http://eebo.chadwyck.com:8080/downloadpdf" +
	if (linktext == "")
	{
		linktext = "Download TIFF file";
	}
	var url = "/downloadtiff" +
			  "?vid=" + currentvid +
			  "&eeboid=" + currentid +
			  "&filename=" + currentpdfname +
			  "&page=" + currentpage;
	document.write( '<A HREF="' + url + '">' + linktext + '</A>' );
}

//  Output the Marked List checkbox and the accompanying message on the
//    Document Images page.  This has several variants according to
//    whether the record is a monograph or periodical.
//
function write_markedlist_checkbox_and_message( eeboid, rectype )
{
	//  See whether there are multiple image sets for this record
	var numsets = 0;
	var i;
	for ( i = 0; i < allvids.length; i++ )
	{
		if ( numpages[ i ] > 0 ) numsets ++;
	}

	if ( rectype == "MONOGRAPH" && numsets > 1 )
	{
		//  Monograph which has multiple image sets
		write_markedlist_checkbox( eeboid, currentvid );
		document.write( " Add this Document Images set to your Marked List" );
	}
	else if ( rectype == "MONOGRAPH" )
	{
		//  Monograph for which there is only a single image set
		write_markedlist_checkbox( eeboid );
		document.write( " Add this record to your Marked List" );
	}
	else if ( rectype == "PERIODICAL" )
	{
		//  Periodical (with each image set representing a different issue)
		write_markedlist_checkbox( eeboid, currentvid );
		document.write( " Add this periodical issue to your Marked List" );
	}
}
//alert( "Completed definition of function: write_markedlist_checkbox_and_message ..." );

//  Output the table of reel positions that appears on the Full Citation page
//    against the "UMI Collection / reel number" heading.
//
function write_reel_position_table( starthtml, eeboid, sessionfile, searchscreen, source )
{
	var html = "";
	
	var i;
	
	if ( allvids.length > 0 )
	{
		document.write( starthtml );
	}
	//  For a periodical, produce tabular output which includes the
	//    collection and reel position, issue number, and date range
	//
	if ( allissuenos.length > 0 )
	{
		//  Start the table for the reel positions
		html += "<TABLE WIDTH=\"100%\">\n";
		html += "<TR>" +
		        "<TD><SPAN CLASS=\"boldtext\">Reel pos.</SPAN></TD>" +
		        "<TD><SPAN CLASS=\"boldtext\">Issue no.</SPAN></TD>" +
		        "<TD><SPAN CLASS=\"boldtext\">Date</SPAN></TD>" +
		        "</TR>\n";
		//  Add the row for each reel position
		for ( i = 0; i < allvids.length; i++ )
		{
			var issuenum = ( "" + issueno[ i ] == "undefined" )
									? "None" : issueno[ i ];
			var dates = get_printable_date_range( datefrom[ i ],
			                                      dateto[ i ] );
			var reelpos = prodabbr[ i ] + " / " +
						  get_reel_position_link( reelposn[ i ],
												  eeboid, sessionfile,
												  searchscreen, source );
			html += "<TR>" +
			        "<TD>" + reelpos + "</TD>" +
			        "<TD>" + issuenum + "&nbsp;</TD>" +
			        "<TD>" + dates + "&nbsp;</TD>" +
			        "</TR>\n";
		}
		//  Finish the table
		html += "</TABLE>\n";
	}
	
	//  For a monograph, produce rows of output with the collection and
	//    reel position
	else
	{
		for ( i = 0; i < allvids.length; i++ )
		{
			//  Add the collection abbreviation and reel position
			if ( html != "" ) html += "<BR>";
			html += prodabbr[ i ] + " / " +
			        get_reel_position_link( reelposn[ i ],
											eeboid, sessionfile,
											searchscreen, source );
		}
	}
	
	document.write( html );
}
//alert( "Completed definition of function: write_reel_position_links ..." );

//  Obtain the HTML for a particular reel position within the current record.
//
//  If the reel position represents an image set that has some associated
//    page images then the returned value is a hyperlink to the appropriate
//    'Document Images' page.
//
function get_reel_position_link( reelpos, eeboid, sessionfile, searchscreen, source )
{
	var i;
	
	//  Default is to return the reel position as plain text (not a link)
	var linktext = reelpos;

	//  Locate the supplied reel position within the current record,
	//    and form the appropriate link (if any)
	for ( i = 0; i < reelposn.length; i++ )
	{
		if ( reelpos == reelposn[ i ] && numpages[ i ] > 0 )
		{
			//  The requested reel position has been found, and it
			//    does have some images.  Form the corresponding link.
			var vid = allvids[ i ];
			var url = "/search/full_rec?SOURCE=pgimages.cfg" +
					  "&ACTION=ByID" +
					  "&ID=" + eeboid +
					  "&VID=" + vid +
					  "&PAGENO=1";
			//  If a sessionfile has been supplied, add it to the URL
			if ( sessionfile &&
			     sessionfile != "XXX_FILE_XXX" &&
			     sessionfile != "param(FILE)" )
			{
				url += "&FILE=" + sessionfile;
			}
			//  If a search screen has been supplied, add it to the URL
			if ( searchscreen &&
			     searchscreen != "XXX_SEARCHSCREEN_XXX" &&
			     searchscreen != "param(SEARCHSCREEN)" )
			{
				url += "&SEARCHSCREEN=" + searchscreen;
			}
			if ( source &&
			     source != "undefined" )
			{
				url += "&SEARCHCONFIG=" + source;
			}
			if ( currentkeyword &&
				currentkeyword != "" )
			{
				url += "&HIGHLIGHT_KEYWORD" + currentkeyword;
			}
			//  If a search screen has been supplied, add it to the URL
			//  Form the link text
			linktext = '<A HREF="' + url + '">' + reelpos + '</A>';
			//  Stop now the required reel position has been found
			break;
		}
	}
	
	return linktext;
}

//  Obtain the HTML for a particular reel position within the current record.
//
//  If the reel position represents an image set that has some associated
//    page images then the returned value is a hyperlink to the appropriate
//    'Document Images' page.
//
function get_reel_pos_thumb_link( reelpos, eeboid, sessionfile, searchscreen, source )
{
	var i;
	
	//  Default is to return the reel position as plain text (not a link)
	var linktext = reelpos;

	//  Locate the supplied reel position within the current record,
	//    and form the appropriate link (if any)
	for ( i = 0; i < reelposn.length; i++ )
	{
		if ( reelpos == reelposn[ i ] && numpages[ i ] > 0 )
		{
			//  The requested reel position has been found, and it
			//    does have some images.  Form the corresponding link.
			var vid = allvids[ i ];
			var url = "/search/full_rec?SOURCE=pgthumbs.cfg" +
					  "&ACTION=ByID" +
					  "&ID=" + eeboid +
					  "&VID=" + vid +
					  "&PAGENO=1";
			//  If a sessionfile has been supplied, add it to the URL
			if ( sessionfile &&
			     sessionfile != "XXX_FILE_XXX" &&
			     sessionfile != "param(FILE)" )
			{
				url += "&FILE=" + sessionfile;
			}
			//  If a search screen has been supplied, add it to the URL
			if ( searchscreen &&
			     searchscreen != "XXX_SEARCHSCREEN_XXX" &&
			     searchscreen != "param(SEARCHSCREEN)" )
			{
				url += "&SEARCHSCREEN=" + searchscreen;
			}
			if ( source &&
			     source != "undefined" )
			{
				url += "&SEARCHCONFIG=" + source;
			}
			if ( currentkeyword &&
				currentkeyword != "" )
			{
				url += "&HIGHLIGHT_KEYWORD" + currentkeyword;
			}
			//  If a search screen has been supplied, add it to the URL
			//  Form the link text
			linktext = '<A HREF="' + url + '">' + reelpos + '</A>';
			//  Stop now the required reel position has been found
			break;
		}
	}
	
	return linktext;
}

//  Functions to extract printable year / month / day components from
//    a numeric 8-digit date of the form YYYYMMDD, e.g.
//
//      get_printable_year(  "16570403" ) --> "1657"
//      get_printable_month( "16570403" ) --> "April"
//      get_printable_day(   "16570403" ) --> "3"
//
//      get_printable_year(  "16570400" ) --> "1657"
//      get_printable_month( "16570400" ) --> "April"
//      get_printable_day(   "16570400" ) --> ""
//
//      get_printable_year(  "16570000" ) --> "1657"
//      get_printable_month( "16570000" ) --> ""
//      get_printable_day(   "16570000" ) --> ""
//
function get_printable_year( dateval )
{
	//  Return the year, from the first 4 digits
	var yyyy = dateval.substring( 0, 4 );
	return yyyy;
}
function get_printable_month( dateval )
{
	//  Get the month number, from the middle 2 digits
	var mm = dateval.substring( 4, 6 );
	
	//  Convert month number to a name
	var monthname = ( mm == "01" ) ? "January" :
	                ( mm == "02" ) ? "February" :
	                ( mm == "03" ) ? "March" :
	                ( mm == "04" ) ? "April" :
	                ( mm == "05" ) ? "May" :
	                ( mm == "06" ) ? "June" :
	                ( mm == "07" ) ? "July" :
	                ( mm == "08" ) ? "August" :
	                ( mm == "09" ) ? "September" :
	                ( mm == "10" ) ? "October" :
	                ( mm == "11" ) ? "November" :
	                ( mm == "12" ) ? "December" :
	                "";
	return monthname;
}
function get_printable_day( dateval )
{
	//  Get the day number, from the last 2 digits
	var dd = dateval.substring( 6, 8 );
	
	//  Convert to a number (to discard any leading zero)
	var dayno = dd - 0;
	
	//  Return the day number, if any
	var day = ( dayno == 1 ) ? "1st" :
	          ( dayno == 2 ) ? "2nd" :
	          ( dayno == 3 ) ? "3rd" :
	          ( dayno >= 4 && dayno <= 20 ) ? "" + dayno + "th" :
	          ( dayno == 21 ) ? "21st" :
	          ( dayno == 22 ) ? "22nd" :
	          ( dayno == 23 ) ? "23rd" :
	          ( dayno >= 24 && dayno <= 30 ) ? "" + dayno + "th" :
	          ( dayno == 31 ) ? "31st" :
	          "";

	return day;
}

//  Convert a pair of 8-digit numeric dates, of the form YYYYMMDD, into
//    a date range for printing, e.g.
//
//      "16570403", "16570403" --> "3 April 1657"
//      "16570403", "16570410" --> "3-10 April 1657"
//      "16570403", "16570501" --> "3 April-1 May 1657"
//      "16571227", "16580103" --> "27 December 1657-3 January 1658"
//
function get_printable_date_range( fromdate, todate )
{
	if ( ! fromdate && ! todate ) return "None";
	
	var year1   = get_printable_year( fromdate );
	var month1  = get_printable_month( fromdate );
	var day1    = get_printable_day( fromdate );

	var year2   = get_printable_year( todate );
	var month2  = get_printable_month( todate );
	var day2    = get_printable_day( todate );
	
	var text = "";
	
	//  If years, months and days are identical, output a single date
	if ( year1 == year2 && month1 == month2 && day1 == day2 )
	{
		text = year1;
		if ( month1 ) text = month1 + " " + text;
		if ( day1 ) text = day1 + " " + text;
	}
	
	//  If years and months are identical, output a day range, month and year
	else if ( year1 == year2 && month1 == month2 )
	{
		text = day1 + "-" + day2 + " " + month1 + " " + year1;
	}
	
	//  If years are identical, output a day and/or month range, and year
	else if ( year1 == year2 )
	{
		text = ( day1 ? day1 + " " + month1 : month1 ) + "-" +
	           ( day2 ? day2 + " " + month2 : month2 ) + " " + year1;
	}

	//  If years are different, output two complete dates
	else
	{
		text = ( day1 ? day1 + " " + month1 : month1 ) + " " + year1 + "-" +
			   ( day2 ? day2 + " " + month2 : month2 ) + " " + year2;
	}
	
	/*
	alert( "Converted date range: '" + fromdate + "' to '" + todate +
	       "' to: '" + text + "'" );
	*/
	return text;
}

//  Output the reel position for the current VID, to appear in the heading
//    of the 'Document Images' page.
//
//  For a record with only one set of images, this appears as e.g.:
//
//    Reel position: STC / 806:05
//
//  For a record with multiple sets of images, this appears as e.g.:
//
//    This reel position: TT / 217:E.1743[1]
//
function write_current_reelpos_and_caption( )
{
	//  Count the number of image sets that include images for this record
	var i;
	var numsets = 0;
	for ( i = 0; i < allvids.length; i++ )
	{
		if ( numpages[ i ] > 0 ) numsets ++;
	}

	//  Form the text of the caption, according to whether this record
	//    has a single image set available, or several.
	var caption = '<SPAN CLASS="boldtext">' + 
	              ( numsets > 1 ? 'This reel position:'
	                            : 'Reel position:' ) +
	              '</SPAN>';

	//  Get the product name and reel position for this VID
	var vidindex = get_vid_index( currentvid );
	var reelposntext = prodabbr[ vidindex ] + ' / ' + reelposn[ vidindex ];

	//  Output the caption and reel position
	document.write( caption + " " + reelposntext + "&nbsp; " );
}
//alert( "Completed definition of function: write_current_reelpos_and_caption ..." );

//  Output any issue-level details for the current VID, to appear in the
//    heading for the 'Document Images' page, e.g.
//
//      Issue number: 360    Date for this issue: 30th April-7th May 1657
//
//  This only applies when displaying records which are periodicals
//
function write_issue_details( )
{
	//  No action if this record is not a periodical
	if ( allissuenos.length == 0 ) return;
	
	//  Get the issue number and date range for this issue
	var vidindex = get_vid_index( currentvid );
	var issuenum = ( "" + issueno[ vidindex ] == "undefined" )
							? "None" : issueno[ vidindex ];
	var daterange = get_printable_date_range( datefrom[ vidindex ],
	                                          dateto[ vidindex ] );
	
	var text = '<SPAN CLASS="boldtext">Issue number:</SPAN> ' +
	           issuenum + '&nbsp; ' +
	           '<SPAN CLASS="boldtext">Date for this issue:</SPAN> ' +
	           daterange + '&nbsp; ';
	           
	document.write( text );
}
//alert( "Completed definition of function: write_issue_details ..." );

//  Output the HTML fragment for the "Adjust View and Print" link
//
function write_adjustview_link( )
{
	//  Form the URL for the 'Adjust View and Print' page
	//    using the current VID and page number
	//
	var url = "/search/full_rec?SOURCE=pgimgadjust.cfg" +
			  "&ACTION=ByID" +
			  "&ID=" + currentid +
			  "&FILE=" + currentsessionfile +
			  "&SEARCHSCREEN=" + currentsearchscreen +
			  "&VID=" + currentvid +
			  "&PAGENO=" + currentpage +
			  "&VIEWPORT=640x480" +
			  "&ZOOM=FIT";
	var text = "Adjust view & print &#155;&#155;";
	//alert( "write_adjustview_link: URL = " + url );
	document.write( "<A HREF=\"" + url + "\">" + text + "</A>" );
}
//alert( "Completed definition of function: write_adjustview_link ..." );

//  Output the HTML fragment for the "Previous Image" link
//
function write_prev_page_link( text, currentsearchconfig, currentsortorder , highlightkw )
{
	if ( currentpage <= 1 )
	{
		//  Already at the first page in the image set.
		//    Output just the text, with no hyperlink
		document.write( '&#139;&#139; ' + text );
	}
	else
	{
		currentkeyword = highlightkw;
		var url = get_url( currentconfigfile, currentvid, currentpage - 1,
						   currentzoomparam, currentviewportparam, 
						   currentsearchconfig, currentsortorder );
		document.write( '<A HREF="' + url + '" ' +
						'TITLE="' + text + '">' +
						'&#139;&#139; ' + text + '</A>' );
	}
}
//alert( "Completed definition of function: write_prev_page_link ..." );

//  Output the HTML fragment for the "Next Image" link
//
function write_next_page_link( text, currentsearchconfig, currentsortorder, highlightkw )
{
	if ( currentpage >= currentnumpages )
	{
		//  Already at the last page in the image set.
		//    Output just the text, with no hyperlink
		document.write( text + ' &#155;&#155;' );
	}
	else
	{
		currentkeyword = highlightkw;
		var url = get_url( currentconfigfile, currentvid, currentpage - 0 + 1,
						   currentzoomparam, currentviewportparam,
						   currentsearchconfig, currentsortorder );
		document.write( '<A HREF="' + url + '" ' +
						'TITLE="' + text + '">' +
						text + '&nbsp;&#155;&#155;' + '</A>' );
	}
}
//alert( "Completed definition of function: write_next_page_link ..." );

//  Output the HTML fragment for the "Previous Illustration" link
//
function write_prev_illustration_link( text, currentsearchconfig, currentsortorder )
{
	//  Find the previous illustration before the current page
	var i;
	var targetpage = 0;
	var vidindex = get_vid_index( currentvid );
	for ( i = currentpage - 1; i >= 1; i-- )
	{
		if ( imagetypes[ vidindex ][ i - 1 ] )
		{
			//  Found an illustration
			targetpage = i;
			break;
		}
	}
	/*
	alert( "Before page " + currentpage + " of " + currentnumpages +
		   ": previous illustration is " + targetpage );
	*/

	if ( targetpage == 0 )
	{
		//  No illustrations found preceding the current page
		//    Output just the text, with no hyperlink
		document.write( '&#139;&#139; ' + text );
	}
	else
	{
		var url = get_url( currentconfigfile, currentvid, targetpage,
						   currentzoomparam, currentviewportparam,
						   currentsearchconfig, currentsortorder );
		document.write( '<A HREF="' + url + '" ' +
						'TITLE="' + text + '">' +
						'&#139;&#139; ' + text + '</A>' );
	}
}
//alert( "Completed definition of function: write_prev_illustration_link ..." );

//  Output the HTML fragment for the "Next Illustration" link
//
function write_next_illustration_link( text, currentsearchconfig, currentsortorder )
{
	//  Find the next illustration after the current page
	var i;
	var targetpage = 0;
	var vidindex = get_vid_index( currentvid );
	for ( i = currentpage - 0 + 1; i <= currentnumpages; i++ )
	{
		if ( imagetypes[ vidindex ][ i - 1 ] )
		{
			//  Found an illustration
			targetpage = i;
			break;
		}
	}
	/*
	alert( "After page " + currentpage + " of " + currentnumpages +
		   ": next illustration is " + targetpage );
	*/

	if ( targetpage == 0 )
	{
		//  No illustrations found following the current page
		//    Output just the text, with no hyperlink
		document.write( text + ' &#155;&#155;' );
	}
	else
	{
		var url = get_url( currentconfigfile, currentvid, targetpage,
						   currentzoomparam, currentviewportparam, 
						   currentsearchconfig, currentsortorder );
		document.write( '<A HREF="' + url + '" ' +
						'TITLE="' + text + '">' +
						text + ' &#155;&#155;' + '</A>' );
	}
}

//  Output the HTML for the page image, on the 'Document Images' page
//
function write_page_image( )
{
	//var imageurl = "http://eebo.chadwyck.com/fetchimage" +
	var imageurl = "/fetchimage" +
				   "?vid=" + currentvid + 
				   "&page=" + currentpage +
				   "&width=" + currentscaledwidth;

	var imagetag = "<IMG BORDER=2 " +
				   "WIDTH=" + currentscaledwidth + " " +
				   "HEIGHT=" + currentscaledheight + " " +
				   "SRC=\"" + imageurl + "\" " +
				   "ALT=\"Page Image " + currentpage +
				   " of " + currentnumpages + "\">";

	document.write( imagetag );
}
//alert( "Completed definition of function: write_page_image ..." );


//  Redisplay the illustrations page, with different options for
//    the illustrations type(s)
//
//  The zoom level is taken from the 'Adjust image size' drop-down
//
//  This function returns false, so as not to trigger a form
//    submission when used as a form's OnSubmit handler
//
function redisplay_illustrations( )
{
	//alert( "redisplaying illustrations page ..." );

	//  Get the list of selected illustration types, as a
	//    comma-separated list, e.g. "Chart,Map"
	//
	var showtypes = "";
	var selectlist = document.forms[ 0 ].elements[ 0 ];
	var i;
	for ( i = 0; i < selectlist.options.length; i++ )
	{
		var selected = selectlist.options[ i ].selected;
		var value    = selectlist.options[ i ].value;
		if ( selected && value != "" )
		{
			showtypes += ( showtypes != "" ? "," : "" ) + value;
		}
	}

	//  Get the option for viewing as text or images
	//
	var viewas = "IMAGES";
	if ( document.forms[ 0 ].elements[ 3 ].checked )
	{
		viewas = "TEXT";
	}

	//  Form the new URL for the illustrations page
	//
	var url = "/search/full_rec" +
			  "?ACTION=ByID" +
			  "&SOURCE=pgillust.cfg" +
			  "&ID=" + currentid +
			  "&FILE=" + currentsessionfile +
			  "&SEARCHSCREEN=" + currentsearchscreen +
			  "&SHOWTYPES=" + showtypes +
			  "&VIEWAS=" + viewas;

	if ( currentkeyword != "" )
	{
		url += "&HIGHLIGHT_KEYWORD" + currentkeyword;
	}

	//  Jump to this URL
	window.location = url;

	//  Return false, so as not to trigger a form submission when
	//    used as an 'OnSubmit' event handler
	return false;
}

//  Write the 'Illustration Type(s)' select list containing the
//    different illustration types that are present in the image
//    set(s) for the current document
//
//  These are taken from the 'illustrations' variable, which contains
//    a sorted list of types and counts, e.g. "Chart=1|Map=3|Plan=2"
//
//  The CGI parameter 'SHOWTYPES', containing the list of currently
//    selected types (e.g. "Map,Chart") is passed in so that these
//    options can be selected in the list.
//
function write_illustration_options( showtypes )
{
	var i, j;

	//  If the CGI parameter does not exist, it will not have been
	//    replaced in the 'param(...)' substitution string.
	//
	//  Treat this as equivalent to an empty parameter
	//
	if ( showtypes.indexOf( "param(" ) != -1 ) showtypes = "";

	/*
	alert( "Generating illustration options:\n" +
		   "showtypes = '" + showtypes + "'\n" +
		   "illustrations = '" + illustrations + "'\n" );
	*/

	//  Get the list of illustrations for this document
	var typecountlist = illustrations.split( "|" );

	//  Split the list of currently selected types
	var selectedlist = showtypes.split( "," );

	//  Work through the illustrations and form the select-list
	var options = 'Only show the following illustrations:<BR>' +
				  '<SELECT NAME="SHOWTYPES" SIZE="3" MULTIPLE>\n' +
				  '<OPTION VALUE="">All illustration types</OPTION>\n';
	for ( i = 0; i < typecountlist.length; i++ )
	{
		//  Split into a type and count at the embedded '='
		var typecount = typecountlist[ i ].split( "=" );
		var label = typecount[ 0 ];
		var count = typecount[ 1 ];

		//  Convert the type label into the displayed string
		var name = get_type_for_display( label );

		//  See whether this type should be selected
		var selectflag = "";
		for ( j = 0; j < selectedlist.length; j++ )
		{
			if ( selectedlist[ j ] == label )
			{
				selectflag = " SELECTED";
			}
		}

		//alert( "label = '" + label + "', count = " + count + ", selectflag = " + selectflag );

		//  Form the option
		options += "<OPTION VALUE=\"" + label + "\"" + selectflag + ">" +
				   name + "    (" + count + " " +
				   ( count > 1 ? "images" : "image" ) + ")" +
				   "</OPTION>\n";
	}
	options += "</SELECT>\n";

	//  Output the string of options
	//alert( "Illustration options are:\n" + options );
	document.write( options );
}
//alert( "Completed definition of function: write_illustration_options ..." );

//  Convert the label representing an illustration type into
//    the text used for display
//
function get_type_for_display( label )
{
	var name = ( label == "Coat-of-Arms" )
							? "Coat of Arms" :
			   ( label == "Genealogical-Table" )
							? "Genealogical Table" :
			   ( label == "Printers-Marks-Colophon" )
							? "Printers Marks: Colophon" :
			   ( label == "Printers-Marks-Title-Page" )
							? "Printers Marks: Title Page" :
			   ( label == "Title-Page-Borders" )
							? "Title Page Borders" :
			   label;
	return name;
}

//  Write the 'View as ...' radio buttons which control whether the
//    illustrations table is displayed as text or thumbnail images.
//
//  The CGI parameter 'VIEWAS', containing either TEXT (the default)
//    or IMAGES determines which radio button is selected
//
function write_illustration_buttons( viewas )
{
	var i, j;

	var buttons = "";

	var textflag   = "";
	var imagesflag = "CHECKED";
	if ( viewas.toUpperCase() == "TEXT" )
	{
		textflag   = "CHECKED";
		imagesflag = "";
	}
		
	buttons = "<INPUT TYPE=\"RADIO\" NAME=\"VIEWAS\" " +
			  "VALUE=\"IMAGES\" " + imagesflag + ">" +
			  "View as thumbnails" +
			  "<BR>" +
			  "<INPUT TYPE=\"RADIO\" NAME=\"VIEWAS\" " +
			  "VALUE=\"TEXT\" " + textflag + ">" +
			  "View as text links";
	document.write( buttons );
}

//  Output the tabular list of illustrations that forms the body
//    of the 'Illustrations' page.
//
//  The argument is the CGI parameter 'VIEWAS' which specifies
//    whether the table's contents are displayed as text only,
//    or with thumbnail images
//
function write_illustrations_table( viewas, showtypes, searchconfig, sortorder )
{
	//alert( "Writing illustrations table for VIDs: " + allvids.join( ", " ) );
	//  If a CGI parameter does not exist, it will not have been
	//    replaced in the 'param(...)' substitution string.
	//
	//  Treat this as equivalent to an empty parameter
	//
	if ( showtypes.indexOf( "param(" ) != -1 ) showtypes = "";
	if ( viewas.indexOf( "param(" ) != -1 ) viewas = "";

	document.write( "<!-- Writing illustrations table ... -->\n" );

	//  Widths to use for the illustrations table and its cells
	var tablewidth = 720;
	var cellwidth  = 180;

	//  Determine whether to display thumbnail images of the pages (Feb 07 - now default action)
	var showimages = 1;
	if ( viewas.toUpperCase() == "TEXT" )
	{
		showimages = 0;
	}

	var i, j;
	var text = "";
	for ( i = 0; i < allvids.length; i++ )
	{
		var vid = allvids[ i ];

		var setno = i + 1;

		//  Form the title for this set of images, with the reel position
		var settitle = "Image set " + setno + " - " +
					   "<SPAN CLASS=\"boldtext\">Reel position:</SPAN> " +
					   prodabbr[ i ] + " / " + reelposn[ i ];
		//  For periodical citations, add the issue-level details
		if ( allissuenos.length > 0 )
		{
			settitle += ' &nbsp; ' +
						'<SPAN CLASS="boldtext">Issue number:</SPAN> ' +
						issueno[ i ] +
						' &nbsp; ' +
						'<SPAN CLASS="boldtext">Date for this issue:</SPAN> ' +
			            get_printable_date_range( datefrom[ i ], dateto[ i ] );
		}

		//  Start the table
		text += '<TABLE WIDTH=' + tablewidth + ' BORDER="1">\n' +
				'<TR><TD COLSPAN="4">' + settitle + '</TD></TR>\n';

		//  Skip this image set if it has no images or no illustrations
		if ( numpages[ i ] == 0 )
		{
			text += '<TR><TD COLSPAN="4" ALIGN="CENTER">' +
					'Images for this reel position ' +
					'have not yet been digitized' +
					'</TD></TR></TABLE><BR>\n';
			continue;
		}
		if ( numillus[ i ] == 0 )
		{
			text += '<TR><TD COLSPAN="4" ALIGN="CENTER">' +
					'No illustrations for this image set' +
					'</TD></TR></TABLE><BR>\n';
			continue;
		}

		//  Otherwise output a row for each 4 illustrations
		//
		var numillustrations = 0;
		text += "<TR>\n";
		for ( j = 0; j < numpages[ i ]; j++ )
		{
			//  Skip pages that are not illustrations
			//
			var caption = get_illustration_caption( imagetypes[ i ][ j ],
													showtypes );
			if ( caption == "" )
			{
				continue;
			}

			//  Start a new row every 4 illustrations
			if ( numillustrations % 4 == 0 )
			{
				text += "</TR>\n<TR>\n";
			}

			//  Output the cell for this illustration
			var pageno = j - 0 + 1;
			var url = get_url( "pgimages.cfg", vid, pageno, "", "", searchconfig, sortorder );
			var thumbnail = get_thumbnail_image( vid, pageno, cellwidth );
			var types = imagetypes[ i ][ j ];

			if ( showimages )
			{
				text += '<TD ALIGN="CENTER" WIDTH="' + cellwidth + '">' +
						'<A HREF="' + url + '">' + thumbnail + '</A><BR>' +
						'<B>' + pageno + '</B> ' +
						'<A HREF="' + url + '">' + caption + '</A>' +
						'</TD>\n';
			}
			else
			{
				text += '<TD ALIGN="CENTER" WIDTH="' + cellwidth + '">' +
						'<B>' + pageno + '</B> ' +
						'<A HREF="' + url + '">' + caption + '</A>' +
						'</TD>\n';
			}

			//  Increment the count of illustrations
			numillustrations ++;
		}

		//  Pad the last row of the table with empty cells
		//alert( "Output table with " + numillustrations + " illustrations" );
		while ( numillustrations % 4 != 0 )
		{
			text += '<TD ALIGN="CENTER" WIDTH="' + cellwidth + '">' +
					'&nbsp;</TD>';
			numillustrations ++;
		}

		//  Finish the table
		text += "</TR>\n</TABLE>\n<BR>\n";
	}

	//  Output the table into the document
	//alert( "Illustrations table is as follows:\n\n" + text );
	document.write( text );

	document.write( "<!-- ... finished writing illustrations table -->\n" );

	return;
}
//alert( "Completed definition of function: write_illustrations_table ..." );

//  Form the caption that is displayed for an image in the
//    illustrations table, containing the list of illustration
//    types for the image (e.g. "Chart, Map").
//
//  The 'typelist' argument is the list of illustration types for
//    the image, e.g. "Map|Chart"
//
//  The 'showtypes' argument is the list of types that are being
//    displayed in the table (from the 'SHOWTYPES' CGI parameter).
//
//  Returns an empty string if the image is not to be displayed
//    in the table (i.e. an image that is not an illustration, or
//    not an illustration of the selected types)
//
function get_illustration_caption( typelist, showtypes )
{
	//  Return an empty string if the image is not an illustration
	if ( ! typelist )
	{
		return "";
	}

	//  Split the list of illustration types for this image
	var typenames = typelist.split( "|" );

	//  Split the list of illustration types currently being shown
	var displaytypes = new Array;
	if ( showtypes != "" )
	{
		displaytypes = showtypes.split( "," );
	}

	//  Return an empty string if the illustration does not match
	//    any of the types that are currently being shown -- so as
	//    to exclude this illustration from the displayed table
	//
	if ( displaytypes.length > 0 )
	{
		var found = 0;
		var i, j;
		for ( i = 0; i < displaytypes.length; i++ )
		{
			for ( j = 0; j < typenames.length; j++ )
			{
				if ( displaytypes[ i ] == typenames[ j ] )
				{
					found = 1;
				}
			}
		}
		if ( ! found )
		{
			return "";
		}
	}

	//  Form the caption (e.g. "Chart, Map")
	var caption = "";
	typenames.sort();
	for ( i = 0; i < typenames.length; i++ )
	{
		caption += ( caption != "" ? ", " : "" ) +
				   get_type_for_display( typenames[ i ] );
	}
	return caption;
}

//  Output the tabular list of illustrations that forms the body
//    of the 'Illustrations' page.
//
//  The argument is the CGI parameter 'VIEWAS' which specifies
//    whether the table's contents are displayed as text only,
//    or with thumbnail images
//
function write_thumbnails_table( viewas, showtypes, searchconfig, sortorder, start, thcount )
{
	//alert( "Writing illustrations table for VIDs: " + allvids.join( ", " ) );
	//  If a CGI parameter does not exist, it will not have been
	//    replaced in the 'param(...)' substitution string.
	//
	//  Treat this as equivalent to an empty parameter
	//
	if ( showtypes.indexOf( "param(" ) != -1 ) showtypes = "";
	if ( viewas.indexOf( "param(" ) != -1 ) viewas = "";
	if ( start.indexOf( "param(" ) != -1 ) start = 0;
	if ( thcount.indexOf( "param(" ) != -1 ) thcount = 100;

	document.write( "<!-- Writing illustrations table ... -->\n" );

	//  Widths to use for the illustrations table and its cells
	var tablewidth = 720;
	var cellwidth  = 180;

	//  always display thumbnails 
	var showimages = 1;

	var i, j;
	var text = "";
	var pagelinks = "";
	// only display current vid, not all vids...
	for ( i = 0; i < allvids.length; i++ )
	{
		var vid = allvids[ i ];
		if (vid != currentvid)
		{
			continue;
		}

		var setno = 1;

		//  Form the title for this set of images, with the reel position
		var settitle = "<SPAN CLASS=\"boldtext\">Reel position:</SPAN> " +
					   prodabbr[ i ] + " / " + reelposn[ i ];
		//  For periodical citations, add the issue-level details
		if ( allissuenos.length > 0 )
		{
			settitle += ' &nbsp; ' +
						'<SPAN CLASS="boldtext">Issue number:</SPAN> ' +
						issueno[ i ] +
						' &nbsp; ' +
						'<SPAN CLASS="boldtext">Date for this issue:</SPAN> ' +
			            get_printable_date_range( datefrom[ i ], dateto[ i ] );
		}

		//  Start the table
		text += '<TABLE WIDTH=' + tablewidth + ' BORDER="1">\n';
		// text += '<TR><TD COLSPAN="4">' + settitle + '</TD></TR>\n';

		//  Skip this image set if it has no images or no illustrations
		if ( numpages[ i ] == 0 )
		{
			text += '<TR><TD COLSPAN="4" ALIGN="CENTER">' +
					'Images for this reel position ' +
					'have not yet been digitized' +
					'</TD></TR></TABLE><BR>\n';
			continue;
		}
		// if ( numillus[ i ] == 0 )
		// {
		// 	text += '<TR><TD COLSPAN="4" ALIGN="CENTER">' +
		// 			'No illustrations for this image set' +
		// 			'</TD></TR></TABLE><BR>\n';
		// 	continue;
		// }

		//  Otherwise output a row for each 4 illustrations
		//
		var numthumbs = 0;
		// var thumbstodisp = thcount;
		var whereiam = Math.ceil(start / (thcount-2));
		if (whereiam == 0)
		{
			whereiam = 1;
		}
		if (start > 0)
		{
			// this means the URL can have nice round numbers like 1, 100, 200...
			start = start - 1;
		}
		var end = start + thcount;
		text += "<TR>\n";
		var totalpages = numpages[ i ];
		if (end > totalpages)
		{
			end = totalpages;
		}

		var numlinks = Math.ceil(totalpages / thcount);
		// build the links to multiple thumbnail pages
		var baseurl = get_url( "pgthumbs.cfg", vid, "", "", "", searchconfig, sortorder );
		pagelinks += '<TABLE WIDTH=' + tablewidth + '>\n<TR><TD>';
		pagelinks += '<b>View thumbnails:</b>';
		if (numlinks > 1)
		{
			pagelinks += ' ';
			if (whereiam != 1)
			{
				pagelinks += '<a href="' + baseurl + '&START=1">';
			}
			pagelinks += '1-100';
			if (whereiam != 1)
			{
				pagelinks += '</a>';
			}
			for ( j = 1; j < numlinks; j++ )
			{
				pagelinks += ' | ';
				if (j != (whereiam - 1))
				{
					pagelinks += '<a href="' + baseurl + '&START=' + j + '01">';
				}
				var lastnum = (j+1) + '00';
				if (j == (numlinks -1))
				{
					lastnum=totalpages;
				}
				pagelinks += j + '01-' + lastnum;
				if (j != (whereiam - 1))
				{
					pagelinks += '</a>';
				}
			}
		}
		else
		{
			pagelinks += ' 1-' + totalpages;
		}
		pagelinks += '</TD></TR></TABLE>';
		
		for ( j = start; j < end; j++ )
		{
			var caption = get_illustration_caption( imagetypes[ i ][ j ], showtypes );
			// if ( caption == "" )
			// {
			// 	continue;
			// }

			//  Start a new row every 4 illustrations
			if ( numthumbs % 4 == 0 )
			{
				text += "</TR>\n<TR>\n";
			}

			//  Output the cell for this illustration
			var pageno = j - 0 + 1;
			var url = get_url( "pgimages.cfg", vid, pageno, "", "", searchconfig, sortorder );
			var thumbnail = get_thumbnail_image( vid, pageno, cellwidth );
			var types = imagetypes[ i ][ j ];

						// illustration info removed from below
						// '<A HREF="' + url + '">' + caption + '</A>' +
			if ( showimages )
			{
				text += '<TD ALIGN="CENTER" WIDTH="' + cellwidth + '">' +
						'<A HREF="' + url + '">' + thumbnail + '</A><BR>' +
						'<A HREF="' + url + '">Image ' + pageno + ' of ' + totalpages + '</A> ' +
						'</TD>\n';
			}
			else
			{
				text += '<TD ALIGN="CENTER" WIDTH="' + cellwidth + '">' +
						'<B>' + pageno + '</B> ' +
						'<A HREF="' + url + '">' + caption + '</A>' +
						'</TD>\n';
			}

			//  Increment the count of illustrations
			numthumbs ++;
		}

		//  Pad the last row of the table with empty cells
		//alert( "Output table with " + numthumbs + " illustrations" );
		while ( numthumbs % 4 != 0 )
		{
			text += '<TD ALIGN="CENTER" WIDTH="' + cellwidth + '">' +
					'&nbsp;</TD>';
			numthumbs ++;
		}

		//  Finish the table
		text += "</TR>\n</TABLE>\n<BR>\n";
	}

	//  Output the table into the document
	//alert( "Thumbnails table is as follows:\n\n" + text );
	document.write( pagelinks );
	document.write( "<BR><BR>" );
	document.write( text );
	document.write( "<BR><BR>" );
	document.write( pagelinks );

	document.write( "<!-- ... finished writing thumbnails table -->\n" );

	return;
}
//alert( "Completed definition of function: write_thumbnails_table ..." );

//  Get the <IMG> tag for a thumbnail image, given the VID and page number
//
function get_thumbnail_image( vid, pageno, maxwidth )
{
	//  Get the array index corresponding to the specified VID
	var vidindex = get_vid_index( vid );

	//  Get the pixel dimensions of the image
	var imageheight = imageheights[ vidindex ][ pageno - 1 ];
	var imagewidth  = imagewidths[ vidindex ][ pageno - 1 ];

	//  Thumbnail is displayed at 1/40th of the actual size
	var imagedisplaywidth  = Math.round( imagewidth  / 40.0 );
	var imagedisplayheight = Math.round( imageheight / 40.0 );

	//  If the optional maximum width is supplied, ensure that the image
	//    is no wider than this width, and scale it down if needed.
	if ( maxwidth && imagedisplaywidth > maxwidth )
	{
		//  Reduce the image size further, to the requested maximum width
		imagedisplaywidth = maxwidth;
		imagedisplayheight = Math.round( maxwidth * imageheight / imagewidth );
	}

	//  Pad the page number to 5 digits, e.g. 12 --> "00012"
	var pagenumber = pageno + "";
	while ( pagenumber.length < 5 ) pagenumber = "0" + pagenumber;

	//var imageurl = "http://eebo.chadwyck.com/thumbnails/" +
	var imageurl = "/thumbnails/" +
				   vid.substring( 0, 3 ) + "/" + vid + "/" +
				   vid + "." + pagenumber + ".s40.gif";

	var imagetag = "<IMG BORDER=1 " +
				   "WIDTH=" + imagedisplaywidth + " " +
				   "HEIGHT=" + imagedisplayheight + " " +
				   "SRC=\"" + imageurl + "\">";

	return imagetag;
}
//alert( "Completed definition of function: get_thumbnail_image ..." );

//  Output the table which appears on the "Download Images" page, which
//    lists the image sets for a citation, with radio buttons to select
//    one of these image sets for downloading (either as the whole set,
//    or selected pages) followed by a 'Download' button to start the
//    download process.
//
//  Each set of download options consists of a radio button named 
//    SELECTION, whose value is a number 'n' which identifies the set
//    of options.  Associated with this are controls 'VIDn' and 'PAGESn'
//    indicating the image set and page range which are to be downloaded.
//
//  This first form of the page is for monographs.  See below for the
//    equavalent function for periodicals.
//
function write_pdf_download_table( eeboid )
{
	var text = "";
	var i, j;

	//  Check if the user has bookmarked particular image sets (instead
	//    of just marking the citation itself).  If so, then only display
	//    the selected image sets.
	//
	var selectedvidlist = "";
	var markedlist = "|" + cookieRead( "ChEEBOMarked2" ) + "|";
	var pos = markedlist.indexOf( "|" + eeboid + ":" );
	if ( pos != -1 )
	{
		//  Extract the colon-separated list of VIDs
		var startpos = markedlist.indexOf( ":", pos );
		var endpos = markedlist.indexOf( "|", pos + 1 );
		selectedvidlist = markedlist.substring( startpos, endpos ) + ":";
	}
	/*
	alert( "Selected VIDs for monographic record " + eeboid + " are:\n" +
		   selectedvidlist );
	*/

	//  Counter used in labelling each radio button
	var buttoncount = 0;

	//  Start the table
	text += '<TABLE BORDER="0">\n';

	for ( i = 0; i < allvids.length; i++ )
	{
		var setno = i + 1;
		var vid = allvids[ i ];
		//alert( "Filename for image set " + setno + " is:\n" + pdfname[ i ] );

		//  Skip this image set if it has no images
		if ( numpages[ i ] == 0 ) continue;

		//  If the user has selected particular image sets, skip this 
		//    image set if its VID is not included in the list
		if ( selectedvidlist != "" &&
			 selectedvidlist.indexOf( ":" + vid + ":" ) == -1 ) continue;

		//  Otherwise output the details of this image set ...
		var sizemsg = "";
		if ( totalsize[ i ] > 0 )
		{
			var megabytes = Math.ceil( totalsize[ i ] / 1000000 );
			sizemsg = " of total size " + megabytes + " Mb";
		}

		//  Form the label with details of the image set
		var label = '<SPAN CLASS="boldtext">' +
					'Reel position:' +
					'</SPAN> ' +
					prodabbr[ i ] + ' / ' + reelposn[ i ];

		//  Output the label, plus hidden fields
		text += '<TR>' + '<TD COLSPAN="3">' + label + '</TD>' + '</TR>\n';
		
		//  Radio button for downloading this entire image set
		buttoncount ++;
		text += '<TR><TD VALIGN="TOP">' +
				'<INPUT TYPE="HIDDEN" NAME="VID' + buttoncount + '" ' +
				'VALUE="' + vid + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="FILENAME' + buttoncount + '" ' +
				'VALUE="' + pdfname[ i ] + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="REELPOS' + buttoncount + '" ' +
				'VALUE="' + prodabbr[ i ] + ' / ' + reelposn[ i ] + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="NUMPAGES' + buttoncount + '" ' +
				'VALUE="' + numpages[ i ] + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="PAGES' + buttoncount + '" ' +
				'VALUE="1-' + numpages[ i ] + '">\n' +
				'<INPUT TYPE="RADIO" NAME="SELECTION" ' +
				'VALUE="' + buttoncount + '"> ' +
				'</TD><TD COLSPAN="2">' +
				'Download entire document' +
				'<BR>' +
				'(' +  numpages[ i ] + ' images' + sizemsg + ')' +
				'</TD></TR>\n';

		//  Radio button for downloading a selected range of pages
		buttoncount ++;
		text += '<TR>' +
				'<TD VALIGN="TOP">' +
				'<INPUT TYPE="HIDDEN" NAME="VID' + buttoncount + '" ' +
				'VALUE="' + vid + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="FILENAME' + buttoncount + '" ' +
				'VALUE="' + pdfname[ i ] + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="REELPOS' + buttoncount + '" ' +
				'VALUE="' + prodabbr[ i ] + ' / ' + reelposn[ i ] + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="NUMPAGES' + buttoncount + '" ' +
				'VALUE="' + numpages[ i ] + '">\n' +
				'<INPUT TYPE="RADIO" NAME="SELECTION" ' +
				'VALUE="' + buttoncount + '"> ' +
				'</TD><TD COLSPAN="2">' +
				'Download range of document images ' +
				'</TD>' +
				'</TR>\n' +
				'<TR>' +
				'<TD></TD>' +
				'<TD>Range:</TD>' +
				'<TD>' +
				'<INPUT TYPE="TEXT" NAME="PAGES' + buttoncount + '" ' +
				'SIZE="20" VALUE="1-' + numpages[ i ] + '">\n' +
				'</TD>' +
				'</TR>\n' +
				'<TR>' +
				'<TD></TD>' +
				'<TD></TD>' +
				'<TD>' +
				'<SPAN CLASS="smalltext">' +
				'(e.g. 1-5,23,36,40-49)' +
				'</SPAN>' +
				'</TD>' +
				'</TR>\n';
		
		//  Add the 'Download' button
		text += '<TR>' +
				'<TD COLSPAN="3" ALIGN="RIGHT">' +
				'<INPUT TYPE="BUTTON" NAME="DOWNLOAD" ' +
				'VALUE="Download" ' +
				'TITLE="Download Document Images" STYLE="' +
				'font-size: 12px; font-family: Verdana; ' +
				'font-weight: bold; color: #fff; ' +
				'background-color: #0000FF; ' +
				'border-top: 2px solid #ccc; ' +
				'border-bottom: 2px solid #333; ' +
				'border-left: 2px solid #ccc; ' +
				'border-right: 2px solid #333;" ' +
				'ONCLICK="download_pdf_images()">' +
				'</TD>' +
				'</TR>\n';

		//  Add a separator between the controls for successive image sets
		text += '<TR>' +
				'<TD COLSPAN="3" HEIGHT="20" VALIGN="MIDDLE">' +
				'<IMG SRC="/images/blackline.gif" HEIGHT="1" WIDTH="330">' +
				'</TD>' +
				'</TR>\n';
	}

	//  Finish the table
	text += '</TABLE>\n';

	//  Output the table into the document
	//
	document.write( text );

	return;
}
//alert( "Completed definition of function: write_pdf_download_table ..." );

//  Alternative version of the PDF download table, for periodicals
//
function write_pdf_download_table2( eeboid )
{
	var text = "";
	var i, j;

	//  Check if the user has bookmarked particular image sets (instead
	//    of just marking the citation itself).  If so, then only display
	//    the selected image sets.
	//
	var selectedvidlist = "";
	var markedlist = "|" + cookieRead( "ChEEBOMarked2" ) + "|";
	var pos = markedlist.indexOf( "|" + eeboid + ":" );
	if ( pos != -1 )
	{
		//  Extract the colon-separated list of VIDs
		var startpos = markedlist.indexOf( ":", pos );
		var endpos = markedlist.indexOf( "|", pos + 1 );
		selectedvidlist = markedlist.substring( startpos, endpos ) + ":";
	}
	/*
	alert( "Selected VIDs for periodical record " + eeboid + " are:\n" +
		   selectedvidlist );
	*/

	//  Counter used in labelling each radio button
	var buttoncount = 0;

	//  Start the table
	text += '<TABLE BORDER="0">\n';

	for ( i = 0; i < allvids.length; i++ )
	{
		var setno = i + 1;
		var vid = allvids[ i ];
		//alert( "Filename for image set " + setno + " is:\n" + pdfname[ i ] );

		//  Skip this image set if it has no images
		if ( numpages[ i ] == 0 ) continue;

		//  If the user has selected particular image sets, skip this 
		//    image set if its VID is not included in the list
		if ( selectedvidlist != "" &&
			 selectedvidlist.indexOf( ":" + vid + ":" ) == -1 ) continue;

		//  Otherwise output the details of this image set ...
		var sizemsg = "" + numpages[ i ] + " images";
		if ( totalsize[ i ] > 0 )
		{
			var megabytes = Math.ceil( totalsize[ i ] / 1000000 );
			sizemsg += " of total size approx. " + megabytes + " Mb";
		}

		//  Form the label with details of the image set
		var label = prodabbr[ i ] + ' / ' + reelposn[ i ] +
					', ' + issueno[ i ] + ', ' +
					get_printable_date_range( datefrom[ i ], dateto[ i ] );

		//  Output the label, plus hidden fields
		text += '<TR>' +
				'<TD WIDTH="20" VALIGN="MIDDLE">' +
				'<IMG SRC="/images/blackline.gif" WIDTH="5" HEIGHT="5" ' +
				'BORDER="0" ALT="">' +
				'</TD>' +
				'<TD COLSPAN="2">' +
				label +
				'</TD>' +
				'</TR>\n' +
				'<TR><TD></TD>' +
				'<TD>' +
				'<SPAN CLASS="boldtext">(' + sizemsg + ')</SPAN>' +
				'</TD>' +
				'</TR>\n';
		
		//  Radio button for downloading this entire image set
		buttoncount ++;
		text += '<TR><TD></TD><TD COLSPAN="2" VALIGN="TOP">' +
				'<INPUT TYPE="HIDDEN" NAME="VID' + buttoncount + '" ' +
				'VALUE="' + vid + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="FILENAME' + buttoncount + '" ' +
				'VALUE="' + pdfname[ i ] + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="REELPOS' + buttoncount + '" ' +
				'VALUE="' + prodabbr[ i ] + ' / ' + reelposn[ i ] + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="NUMPAGES' + buttoncount + '" ' +
				'VALUE="' + numpages[ i ] + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="PAGES' + buttoncount + '" ' +
				'VALUE="1-' + numpages[ i ] + '">\n' +
				'<INPUT TYPE="RADIO" NAME="SELECTION" ' +
				'VALUE="' + buttoncount + '"> ' +
				'Download entire document&nbsp;&nbsp;&nbsp;\n';

		//  Radio button for downloading a selected range of pages
		buttoncount ++;
		text += '<INPUT TYPE="HIDDEN" NAME="VID' + buttoncount + '" ' +
				'VALUE="' + vid + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="FILENAME' + buttoncount + '" ' +
				'VALUE="' + pdfname[ i ] + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="REELPOS' + buttoncount + '" ' +
				'VALUE="' + prodabbr[ i ] + ' / ' + reelposn[ i ] + '">\n' +
				'<INPUT TYPE="HIDDEN" NAME="NUMPAGES' + buttoncount + '" ' +
				'VALUE="' + numpages[ i ] + '">\n' +
				'<INPUT TYPE="RADIO" NAME="SELECTION" ' +
				'VALUE="' + buttoncount + '"> ' +
				'Download range of document images ' +
				'</TD>' +
				'</TR>\n' +
				'<TR>' +
				'<TD></TD>' +
				'<TD>Range: ' +
				'<INPUT TYPE="TEXT" NAME="PAGES' + buttoncount + '" ' +
				'SIZE="20" VALUE="1-' + numpages[ i ] + '">\n' +
				'<SPAN CLASS="smalltext">' +
				'(e.g. 1-5,23,36,40-49)' +
				'</SPAN>' +
				'</TD>\n';
		
		//  Add the 'Download' button
		text += '<TD ALIGN="RIGHT">' +
				'<INPUT TYPE="BUTTON" NAME="DOWNLOAD" ' +
				'VALUE="Download" ' +
				'TITLE="Download Document Images" STYLE="' +
				'font-size: 12px; font-family: Verdana; ' +
				'font-weight: bold; color: #fff; ' +
				'background-color: #0000FF; ' +
				'border-top: 2px solid #ccc; ' +
				'border-bottom: 2px solid #333; ' +
				'border-left: 2px solid #ccc; ' +
				'border-right: 2px solid #333;" ' +
				'ONCLICK="download_pdf_images()">' +
				'</TD>' +
				'</TR>\n';

		//  Add a separator between the controls for successive image sets
		text += '<TR>' +
				'<TD COLSPAN="3" HEIGHT="20" VALIGN="MIDDLE">' +
				'<IMG SRC="/images/blackline.gif" HEIGHT="1" WIDTH="700">' +
				'</TD>' +
				'</TR>\n';
	}

	//  Finish the table
	text += '</TABLE>\n';

	//  Output the table into the document
	//
	document.write( text );
}
//alert( "Completed definition of function: write_pdf_download_table2 ..." );

//  This function is invoked by the 'Download' button on the 'Download Images'
//    page, which contains radio buttons for downloading page images as PDF
//    files under different options.
//
//  Each radio button specifies arguments for the corresponding download.
//    This function just jumps to the selected URL.
//
function download_pdf_images( )
{
	//alert( "PDF Download ..." );
	//  Determine which of the checkboxes has been selected
	//
	var elements = document.DOWNLOADFORM.elements;
	var i;
	var selection = 0;
	var selectedvid = "";
	var selectedfile = "";
	var selectedpages = "";
	var selectedreelpos = "";
	var selectednumpages = "";
	for ( i = 0; i < elements.length; i++ )
	{
		if ( elements[ i ].name == "SELECTION" &&
			 elements[ i ].checked )
		{
			//  Get the selection number for this checkbox
			selection = elements[ i ].value;
			//  Get the VID and page range from the associated fields
			selectedvid = elements[ "VID" + selection ].value;
			selectedfile = elements[ "FILENAME" + selection ].value;
			selectedpages = elements[ "PAGES" + selection ].value;
			selectedreelpos = elements[ "REELPOS" + selection ].value;
			//  Get the number of pages in the image set, for validation
			selectednumpages = elements[ "NUMPAGES" + selection ].value;
			//  Trim leading and trailing spaces from the page range
			selectedpages = string_trim( selectedpages );
			break;
		}
	}
	//alert( "Selected URL is: " + selectedurl );

	//  Check that an option has been selected
	if ( selection == 0 )
	{
		alert( "--- Download Images as a PDF File ---\n" +
			   "\n" +
			   "First select a group of pages using the radio buttons,\n" +
			   "then press 'Download' to retrieve these as a PDF file" );
		return;
	}

	//  Validate the entered image range
	var errmsg = validate_image_range( selectedpages, selectednumpages );
	if ( errmsg != "" )
	{
		alert( "--- Download Images as a PDF File ---\n" +
			   "\n" +
			   "The requested image range '" + selectedpages + "' does not " +
			   "represent a valid selection from the " +
			   ( selectednumpages == 1 ? "single image " :
				 "" + selectednumpages + " images " ) +
			   "in this image set.\n(" + errmsg + ")" );
		return;
	}
	
	//  Populate the field values into the form's hidden elements
	document.SUBMITFORM.elements[ "vid" ].value = selectedvid;
	document.SUBMITFORM.elements[ "filename" ].value = selectedfile;
	document.SUBMITFORM.elements[ "pages" ].value = selectedpages;
	document.SUBMITFORM.elements[ "reelpos" ].value = selectedreelpos;
	//  Submit the form
	/*
	alert( "Submitting download form:\n" +
		   "vid=" + document.SUBMITFORM.elements[ "vid" ].value + "\n" +
		   "reelpos=" + document.SUBMITFORM.elements[ "reelpos" ].value + "\n" +
		   "filename=" + document.SUBMITFORM.elements[ "filename" ].value + "\n" +
		   "pages=" + document.SUBMITFORM.elements[ "pages" ].value );
	*/
	document.SUBMITFORM.submit();
}
//alert( "Completed definition of function: download_pdf_images ..." );

//  Validate the text entered as the selected image range for a download
//
//  Returns an error message if invalid, 
//
function validate_image_range( text, numpages )
{
	//  Discard any spaces surrounding commas and dashes
	text = string_global_replace( text, " -", "-" );
	text = string_global_replace( text, "- ", "-" );
	text = string_global_replace( text, " ,", "," );
	text = string_global_replace( text, ", ", "," );
	
	//  Discard duplicated commas
	text = string_global_replace( text, ",,", "," );
		
	//  Check for any characters other than digits, commas and dashes
	if ( text.indexOf( " " ) != -1 )
	{
		return "Image range contains superfluous spaces";
	}
	var allowedchars = "0123456789,-";
	if ( string_begins_with( text, "0123456789,-" ) != text.length )
	{
		return "Image range contains characters other than digits, " +
		       "commas and dashes";
	}

	//  Split the text at any embedded commas
	var rangelist = string_split( text, "," );
	
	//  Validate each page number or range (and count if any are found)
	var numvalid = 0;
	for ( i = 0; i < rangelist.length; i++ )
	{
		var range = rangelist[ i ];
		if ( string_begins_with( range, "0123456789" ) == range.length )
		{
			//  This item is a single page number.  Check that it is
			//    in the range from 1 to the number of pages
			var pageno = parseInt( range );
			if ( pageno < 1 || pageno > numpages )
			{
				return "Image number " + pageno + " is not between 1 and " +
				       numpages;
			}
			//  Found a valid page number
			numvalid ++;
		}
		else
		{
			//  Item is not a single page number.  Check that it is
			//    a valid dash-separated range
			var dashpos = range.indexOf( "-" );
			var part1 = range.substr( 0, dashpos );
			var part2 = range.substr( dashpos + 1, range.length );
			if ( string_begins_with( part1, "0123456789" ) != part1.length ||
			     string_begins_with( part2, "0123456789" ) != part2.length )
			{
				return "Image range '" + range + "' is improperly formed";
			}
			var pageno1 = parseInt( part1 );
			var pageno2 = parseInt( part2 );
			if ( pageno1 < 1 || pageno1 > numpages )
			{
				return "Image number " + pageno1 + " is not between 1 and " +
				       numpages;
			}
			else if ( pageno2 < 1 || pageno2 > numpages )
			{
				return "Image number " + pageno2 + " is not between 1 and " +
				       numpages;
			}
			else if ( pageno1 > pageno2 )
			{
				return "Image number range " + range + " has the numbers " +
				       "the wrong way round";
			}
			//  Found a valid page range
			numvalid ++;
		}
	}
	
	//  Check that the list of page numbers and ranges was not empty
	if ( numvalid == 0 )
	{
		return "Image range is empty";
	}
	
	//  No errors found
	return "";
}

//  Utility function to trim leading and trailing spaces from a string
//
function string_trim( text )
{
	var origtext = text;
	while ( text.length > 0 && text.charAt( 0 ) == " " )
		text = text.substr( 1, text.length );
	while ( text.length > 0 && text.charAt( text.length - 1 ) == " " )
		text = text.substr( 0, text.length - 1 );
	return text;
}

//  Utility function to do a global substitution on a string
//
function string_global_replace( text, target, replacement )
{
	var matchpos;
	while ( ( matchpos = text.indexOf( target ) ) != -1 )
	{
		text = text.substr( 0, matchpos ) + replacement +
		       text.substr( matchpos + target.length, text.length );
	}
	return text;
}

//  Utility function to split a string at a delimiter, returning an array
//
function string_split( text, delimiter )
{
	var matchpos;
	var results = new Array();
	//  Repeatedly snip off the text up to the first/next delimiter
	while ( ( matchpos = text.indexOf( delimiter ) ) != -1 )
	{
		results[ results.length ] = text.substr( 0, matchpos );
		text = text.substr( matchpos + delimiter.length, text.length );
	}
	//  Add any text after the final delimiter
	if ( text != "" )
	{
		results[ results.length ] = text;
	}
	return results;
}

//  Utility function to check whether a string begins with characters
//    from a specified list.  Returns the number of leading characters
//    that are from the list.
function string_begins_with( text, allowedchars )
{
	var i;
	var numchars = 0;
	for ( i = 0; i < text.length; i++ )
	{
		if ( allowedchars.indexOf( text.charAt( i ) ) == -1 ) break;
		numchars ++;
	}
	return numchars;
}

//  Comparison function used when comparing image sets (for periodical
//    issues) according to the start dates of the issue
function compare_by_date( index1, index2 )
{
	//  Get the issue date for the first image set (as YYYYMMDD)
	var date1 = datefrom[ index1 ];
	//  Get the issue date for the second image set (as YYYYMMDD)
	var date2 = datefrom[ index2 ];
	//  Return the results of comparison
	return ( date1 < date2 ) ? -1 :
	       ( date1 > date2 ) ? 1 :
	       0;
}

//  Convert a (Thomason) reel position into a sortable form, by padding
//    the embedded fields to a fixed width of 5 characters, e.g.
//
//      "123:E.456[78]" --> "00123.E    .00456.00078"
//
function get_sortable_reel_position( reelpos )
{
	var i;
	var ch;
	
	//  Convert the reel position to uppercase, and add a trailing period
	var text = reelpos ? reelpos.toUpperCase() + "." : ".";
	//alert( "Converting reel position: '" + text + "'" );
	
	//  Step through the reel position, breaking it into separate parts
	//    at the punctuation between alphanumeric items
	var result = "";
	var partisnumeric = true;
	var padding;
	var currentpart = "";
	for ( i = 0; i < text.length; i++ )
	{
		ch = text.toUpperCase().charAt( i );
		if ( ch >= "0" && ch <= "9" )
		{
			//  Add this digit to the current part
			currentpart += "" + ch;
		}
		else if ( ch >= "A" && ch <= "Z" )
		{
			//  Add this letter to the current part
			currentpart += "" + ch;
			//  Mark the part as not being a number
			partisnumeric = false;
		}
		else
		{
			//  Otherwise have punctuation, which terminates the part.
			//  Add the currentpart (if any) to the result
			if ( currentpart.length > 0 )
			{
				if ( partisnumeric )
				{
					//  Pad this numeric part by prepending zeroes
					padding = "00000".substring( 0, 5 - currentpart.length );
					result += "" + padding + "" + currentpart + ".";
				}
				else
				{
					//  Pad this non-numeric part by appending blanks
					padding = "     ".substring( 0, 5 - currentpart.length );
					result += "" + currentpart + padding + ".";
				}
			}
			//  Reset the current part
			currentpart = "";
			partisnumeric = true;
		}
	}
	
	//alert( "Converted reel pos. '" + reelpos + "' to '" + result );
	return result;
}

//  Comparison function used when comparing image sets (for periodical
//    issues) according to their reel positions.
//
//  Because this only applies to Thomason Tracts items, each should have
//    a reel position of the form "123:E.456[78]", which contains a reel
//    number, shelf letter, volume number, and item number within volume
//
function compare_by_reel_pos( index1, index2 )
{
	//  Get the reel position for the first image set (as YYYYMMDD)
	var reelpos1 = get_sortable_reel_position( reelposn[ index1 ] );
	//  Get the reel position for the second image set (as YYYYMMDD)
	var reelpos2 = get_sortable_reel_position( reelposn[ index2 ] );
	
	//  Return the results of comparison
	return ( reelpos1 < reelpos2 ) ? -1 :
	       ( reelpos1 > reelpos2 ) ? 1 :
	       0;
}

//  Output the table which appears on the "Select Images" page, which
//    lists the image sets for a citation, with each being a link to
//    the "Document Images" page displaying that image set.
//
//  For a periodical item (where each image set represents a different
//    issue) the entry also lists the issue number and date range.
//
function write_image_set_list( eeboid, sessionfile, searchscreen, sortorder, source )
{
	if ( sessionfile.toLowerCase().indexOf( 'param(' ) == 0 ||
		 sessionfile.indexOf( 'XXX_' ) == 0 ||
		 sessionfile.toLowerCase() == "default" ) sessionfile = "";

	//  Sort order defaults to date
	if ( sortorder != "DATE" && sortorder != "REELPOS" ) sortorder = "DATE";
	
	//  If a sort order has been specified (to re-sort the image sets by
	//    date or reel position) produce the sorted list.
	//
	//  This should only apply to periodicals, since only these have the
	//    re-sort control on the Image Sets page.
	//
	var i;
	var imagesetlist = new Array();
	//  First create an unsorted list of all the image sets' array indices
	//    (i.e. in occurrence order within the citation record)
	for ( i = 0; i < allvids.length; i++ )
	{
		imagesetlist[ i ] = i;
	}
	//alert( "Creating image set list: sort order = '" + sortorder + "'" );
	if ( sortorder == "DATE" )
	{
		//  Resort the array indices according to date
		imagesetlist = imagesetlist.sort( compare_by_date );
	}
	else if ( sortorder == "REELPOS" )
	{
		//  Resort the array indices according to reel position
		imagesetlist = imagesetlist.sort( compare_by_reel_pos )
	}
	//alert( "Sorted array = " + imagesetlist.join( ", " ) );

	document.write( "<!-- Writing image set list ... -->\n" );

	var text = "";
	
	//  Styles used to outline the table with a black border
	var tlstyle = 'STYLE="border-top: 1px solid black; ' +
	              'border-left: 1px solid black;"';
	var brstyle = 'STYLE="border-bottom: 1px solid black; ' +
	              'border-right: 1px solid black;"';

	if ( allissuenos.length > 0 )
	{
		//  For a periodical, add the control used to resort the list by
		//    MARC record occurrence / date / reel position order.
		//
		var url = "/search/full_rec" +
		          "?SOURCE=selimages.cfg" +
		          "&ACTION=ByID" +
		          "&ID=" + eeboid +
		          "&FILE=" + sessionfile +
		          "&SEARCHSCREEN=" + searchscreen +
		          "&SORTBY=";
		text += '<P>Sort image sets by: ' +
		        '<SELECT NAME="SORTOPTS" SIZE="1"> ' +
		        '<OPTION VALUE="' + url + 'DATE" ' +
		        ( sortorder == "DATE" ? "SELECTED" : "" ) +
		        '>Date</OPTION>' +
		        '<OPTION VALUE="' + url + 'REELPOS" ' +
		        ( sortorder == "REELPOS" ? "SELECTED" : "" ) +
		        '>Reel number</OPTION>' +
		        '</SELECT>&nbsp;' +
		        '<INPUT TYPE="BUTTON" VALUE=" Go " ' +
		        'ONCLICK="javascript: location.replace( ' +
		        'document.forms[0].SORTOPTS.options' +
		        '[document.forms[0].SORTOPTS.selectedIndex].value )" ' +
		        'STYLE="font-size: 12px; ' +
		        'font-family: Verdana; ' +
		        'font-weight: bold; ' +
		        'color: #fff; ' +
		        'background-color: #0000FF; ' +
		        'border-top: 2px solid #ccc; ' +
		        'border-bottom: 2px solid #333; ' +
		        'border-left: 2px solid #ccc; ' +
		        'border-right: 2px solid #333;">' +
		        '</P>\n';

		//  For periodicals, use a bordered table with column headings
		text += '<TABLE CELLSPACING="0" ' +
		        'CELLPADDING="3" ' + tlstyle + '>\n' +
		        '<TR>' +
		        '<TD ' + brstyle + '><SPAN CLASS="boldtext">UMI Collection</TD>' +
		        '<TD ' + brstyle + '><SPAN CLASS="boldtext">UMI reel number</TD>' +
				'<TD ' + brstyle + '><SPAN CLASS="boldtext">Issue no.</TD>' +
	        	'<TD ' + brstyle + '><SPAN CLASS="boldtext">Date</TD>' +
	        	'</TR>\n';
	}
	else
	{
		//  For monographs, use a bordered table 
		text += '<TABLE CELLSPACING="0" CELLPADDING="3" ' + tlstyle + '>\n' +
		        '<TR>' +
		        '<TD ' + brstyle + '><SPAN CLASS="boldtext">UMI Collection</TD>' +
		        '<TD ' + brstyle + '><SPAN CLASS="boldtext">UMI reel number</TD>' +
	        	'</TR>\n';
	}

	//  Build up the entries in the table
	var j;
	for ( i = 0; i < imagesetlist.length; i++ )
	{
		var setno = i + 1;
		var vidindex = imagesetlist[ i ];
		var vid = allvids[ vidindex ];
		//alert( "Creating entry for VID " + vidindex + " = " + vid );

		//  Reel position links to the 'Document Images' page, if any
		var reelposnlink;
		var reelposnmsg = "";
		if ( numpages[ vidindex ] > 0 )
		{
			//  Create the link for this reel position
			reelposnlink = get_reel_position_link( reelposn[ vidindex ],
		                                           eeboid, sessionfile,
		                                           searchscreen, source );
		}
		else
		{
			//  Display a "not yet digitised" message after the reel position
			reelposnlink = reelposn[ vidindex ];
			reelposnmsg  = ' <SPAN CLASS="smalltext">' +
			               '(Images not yet digitized)' +
			               '</SPAN>';
		}

		//  Form the entry for this image set.
		if ( allissuenos.length > 0 )
		{
			//  Use the multi-column layout for a periodical
			var issuenum = ( "" + issueno[ vidindex ] == "undefined" )
									? "None" : issueno[ vidindex ];
			var dates = get_printable_date_range( datefrom[ vidindex ],
			                                      dateto[ vidindex ] );
			text += '<TR>' +
			        '<TD ' + brstyle + '>' + prodabbr[ vidindex ] + '</TD>' +
			        '<TD ' + brstyle + '>' + reelposnlink + reelposnmsg + '</TD>' +
			        '<TD ' + brstyle + '>' + issuenum + '&nbsp;</TD>' +
			        '<TD ' + brstyle + '>' + dates + '&nbsp;</TD>' +
			        '</TR>\n';
		}
		else
		{
			//  Use a two-column layout for a monograph
			text += '<TR>' +
			        '<TD ' + brstyle + '>' + prodabbr[ vidindex ] + '</TD>' +
			        '<TD ' + brstyle + '>' + reelposnlink + reelposnmsg + '</TD>' +
			        '</TR>\n';
		}
	}

	//  Finish the table
	text += '</TABLE>\n';

	//  Output the table into the document
	//
	document.write( text );

	document.write( "<!-- End of image set list -->\n" );

	return;
}

//  Output the table which appears on the "Select Images" page, which
//    lists the image sets for a citation, with each being a link to
//    the "Document Images" page displaying that image set.
//
//  For a periodical item (where each image set represents a different
//    issue) the entry also lists the issue number and date range.
//
function write_thumb_set_list( eeboid, sessionfile, searchscreen, sortorder, source )
{
	if ( sessionfile.toLowerCase().indexOf( 'param(' ) == 0 ||
		 sessionfile.indexOf( 'XXX_' ) == 0 ||
		 sessionfile.toLowerCase() == "default" ) sessionfile = "";

	//  Sort order defaults to date
	if ( sortorder != "DATE" && sortorder != "REELPOS" ) sortorder = "DATE";
	
	//  If a sort order has been specified (to re-sort the image sets by
	//    date or reel position) produce the sorted list.
	//
	//  This should only apply to periodicals, since only these have the
	//    re-sort control on the Image Sets page.
	//
	var i;
	var imagesetlist = new Array();
	//  First create an unsorted list of all the image sets' array indices
	//    (i.e. in occurrence order within the citation record)
	for ( i = 0; i < allvids.length; i++ )
	{
		imagesetlist[ i ] = i;
	}
	//alert( "Creating image set list: sort order = '" + sortorder + "'" );
	if ( sortorder == "DATE" )
	{
		//  Resort the array indices according to date
		imagesetlist = imagesetlist.sort( compare_by_date );
	}
	else if ( sortorder == "REELPOS" )
	{
		//  Resort the array indices according to reel position
		imagesetlist = imagesetlist.sort( compare_by_reel_pos )
	}
	//alert( "Sorted array = " + imagesetlist.join( ", " ) );

	document.write( "<!-- Writing thumb set list ... -->\n" );

	var text = "";
	
	//  Styles used to outline the table with a black border
	var tlstyle = 'STYLE="border-top: 1px solid black; ' +
	              'border-left: 1px solid black;"';
	var brstyle = 'STYLE="border-bottom: 1px solid black; ' +
	              'border-right: 1px solid black;"';

	if ( allissuenos.length > 0 )
	{
		//  For a periodical, add the control used to resort the list by
		//    MARC record occurrence / date / reel position order.
		//
		var url = "/search/full_rec" +
		          "?SOURCE=selthumbs.cfg" +
		          "&ACTION=ByID" +
		          "&ID=" + eeboid +
		          "&FILE=" + sessionfile +
		          "&SEARCHSCREEN=" + searchscreen +
		          "&SORTBY=";
		text += '<P>Sort image sets by: ' +
		        '<SELECT NAME="SORTOPTS" SIZE="1"> ' +
		        '<OPTION VALUE="' + url + 'DATE" ' +
		        ( sortorder == "DATE" ? "SELECTED" : "" ) +
		        '>Date</OPTION>' +
		        '<OPTION VALUE="' + url + 'REELPOS" ' +
		        ( sortorder == "REELPOS" ? "SELECTED" : "" ) +
		        '>Reel number</OPTION>' +
		        '</SELECT>&nbsp;' +
		        '<INPUT TYPE="BUTTON" VALUE=" Go " ' +
		        'ONCLICK="javascript: location.replace( ' +
		        'document.forms[0].SORTOPTS.options' +
		        '[document.forms[0].SORTOPTS.selectedIndex].value )" ' +
		        'STYLE="font-size: 12px; ' +
		        'font-family: Verdana; ' +
		        'font-weight: bold; ' +
		        'color: #fff; ' +
		        'background-color: #0000FF; ' +
		        'border-top: 2px solid #ccc; ' +
		        'border-bottom: 2px solid #333; ' +
		        'border-left: 2px solid #ccc; ' +
		        'border-right: 2px solid #333;">' +
		        '</P>\n';

		//  For periodicals, use a bordered table with column headings
		text += '<TABLE CELLSPACING="0" ' +
		        'CELLPADDING="3" ' + tlstyle + '>\n' +
		        '<TR>' +
		        '<TD ' + brstyle + '><SPAN CLASS="boldtext">UMI Collection</TD>' +
		        '<TD ' + brstyle + '><SPAN CLASS="boldtext">UMI reel number</TD>' +
				'<TD ' + brstyle + '><SPAN CLASS="boldtext">Issue no.</TD>' +
	        	'<TD ' + brstyle + '><SPAN CLASS="boldtext">Date</TD>' +
	        	'</TR>\n';
	}
	else
	{
		//  For monographs, use a bordered table 
		text += '<TABLE CELLSPACING="0" CELLPADDING="3" ' + tlstyle + '>\n' +
		        '<TR>' +
		        '<TD ' + brstyle + '><SPAN CLASS="boldtext">UMI Collection</TD>' +
		        '<TD ' + brstyle + '><SPAN CLASS="boldtext">UMI reel number</TD>' +
	        	'</TR>\n';
	}

	//  Build up the entries in the table
	var j;
	for ( i = 0; i < imagesetlist.length; i++ )
	{
		var setno = i + 1;
		var vidindex = imagesetlist[ i ];
		var vid = allvids[ vidindex ];
		//alert( "Creating entry for VID " + vidindex + " = " + vid );

		//  Reel position links to the 'Document Images' page, if any
		var reelposnlink;
		var reelposnmsg = "";
		if ( numpages[ vidindex ] > 0 )
		{
			//  Create the link for this reel position
			reelposnlink = get_reel_pos_thumb_link( reelposn[ vidindex ],
		                                           eeboid, sessionfile,
		                                           searchscreen, source );
		}
		else
		{
			//  Display a "not yet digitised" message after the reel position
			reelposnlink = reelposn[ vidindex ];
			reelposnmsg  = ' <SPAN CLASS="smalltext">' +
			               '(Images not yet digitized)' +
			               '</SPAN>';
		}

		//  Form the entry for this image set.
		if ( allissuenos.length > 0 )
		{
			//  Use the multi-column layout for a periodical
			var issuenum = ( "" + issueno[ vidindex ] == "undefined" )
									? "None" : issueno[ vidindex ];
			var dates = get_printable_date_range( datefrom[ vidindex ],
			                                      dateto[ vidindex ] );
			text += '<TR>' +
			        '<TD ' + brstyle + '>' + prodabbr[ vidindex ] + '</TD>' +
			        '<TD ' + brstyle + '>' + reelposnlink + reelposnmsg + '</TD>' +
			        '<TD ' + brstyle + '>' + issuenum + '&nbsp;</TD>' +
			        '<TD ' + brstyle + '>' + dates + '&nbsp;</TD>' +
			        '</TR>\n';
		}
		else
		{
			//  Use a two-column layout for a monograph
			text += '<TR>' +
			        '<TD ' + brstyle + '>' + prodabbr[ vidindex ] + '</TD>' +
			        '<TD ' + brstyle + '>' + reelposnlink + reelposnmsg + '</TD>' +
			        '</TR>\n';
		}
	}

	//  Finish the table
	text += '</TABLE>\n';

	//  Output the table into the document
	//
	document.write( text );

	document.write( "<!-- End of thumb set list -->\n" );

	return;
}

//  Perform a redirection to the 'Select Images' page, if needed.
//
//  This is done when the 'Page Images' page is displayed for a document
//    which has multiple image sets.  If the parameters do not specify
//    a particular VID number, then the user needs to be redirected to
//    the intermediate page for selecting a particular image set.
//
function redirect_to_select_images_page( idparam, vidparam, sessionfile, searchscreen , searchconfig )
{
	//  Check whether the document has more than one image set with
	//    some associated image files.
	//
	var numsets = 0;
	for ( i = 0; i < allvids.length; i++ )
	{
		if ( numpages[ i ] > 0 )
		{
			numsets ++;
		}
	}

	//  No redirection is needed if there is only a single image set
	if ( numsets <= 1 )
	{
		//alert( "Document has " + numsets + " image set(s) - " +
		//	   "no redirection needed" );
		return;
	}

	//  Check if the CGI parameters indicate that a particular image set
	//    has been selected.  This is either an explicit VID parameter
	//    giving a VID number (e.g. '12345' or '23456-01') or an ID
	//    parameter which specifies an alphanumeric VIDREF (e.g. 'V12345'
	//    or 'V23456D01') instead of the numeric EEBO ID.
	//
	var selectedvidindex = get_selected_vid_index( idparam, vidparam )
	// alert( "redirect_to_select_images_page: selectedvidindex = '" + selectedvidindex + "'" );
	if ( selectedvidindex >= 0 )
	{
		//  An explicit VID has been specified.  No redirection needed.
		//alert( "VID " + allvids[ selectedvidindex ] + " has been selected. " +
		//	   "No redirection needed" );
		return;
	}

	//  Otherwise redirect to the 'Select Images' page
	//
	var url = "/search/full_rec" +
	          "?SOURCE=selimages.cfg" +
	          "&ACTION=ByID" +
	          "&ID=" + idparam +
	          "&FILE=" + sessionfile +
	          "&SEARCHSCREEN=" + searchscreen;

		if ( "" + searchconfig == "undefined" )
		{
			url += "&SEARCHCONFIG=config.cfg";
		}
		else
		{
			url += "&SEARCHCONFIG=";
			url += searchconfig;
		}
		if ( currentkeyword &&
			currentkeyword != "" )
		{
			url += "&HIGHLIGHT_KEYWORD" + currentkeyword;
		}
	//alert( "Document has multiple image sets - " +
	//	   "redirecting to the 'Select Images' page" );
	window.location.replace( url );
}

//  Perform a redirection to the 'Select Thumbnails' page, if needed.
//
//  This is done when the 'Page Images' page is displayed for a document
//    which has multiple image sets.  If the parameters do not specify
//    a particular VID number, then the user needs to be redirected to
//    the intermediate page for selecting a particular image set.
//
function redirect_to_select_thumbs_page( idparam, vidparam, sessionfile, searchscreen , searchconfig )
{
	//  Check whether the document has more than one image set with
	//    some associated image files.
	//
	var numsets = 0;
	for ( i = 0; i < allvids.length; i++ )
	{
		if ( numpages[ i ] > 0 )
		{
			numsets ++;
		}
	}

	//  No redirection is needed if there is only a single image set
	if ( numsets <= 1 )
	{
		//alert( "Document has " + numsets + " image set(s) - " +
		//	   "no redirection needed" );
		return;
	}

	//  Check if the CGI parameters indicate that a particular image set
	//    has been selected.  This is either an explicit VID parameter
	//    giving a VID number (e.g. '12345' or '23456-01') or an ID
	//    parameter which specifies an alphanumeric VIDREF (e.g. 'V12345'
	//    or 'V23456D01') instead of the numeric EEBO ID.
	//
	var selectedvidindex = get_selected_vid_index( idparam, vidparam )
	// alert( "redirect_to_select_images_page: selectedvidindex = '" + selectedvidindex + "'" );
	if ( selectedvidindex >= 0 )
	{
		//  An explicit VID has been specified.  No redirection needed.
		//alert( "VID " + allvids[ selectedvidindex ] + " has been selected. " +
		//	   "No redirection needed" );
		return;
	}

	//  Otherwise redirect to the 'Select Images' page
	//
	var url = "/search/full_rec" +
	          "?SOURCE=selthumbs.cfg" +
	          "&ACTION=ByID" +
	          "&ID=" + idparam +
	          "&FILE=" + sessionfile +
	          "&SEARCHSCREEN=" + searchscreen;

		if ( "" + searchconfig == "undefined" )
		{
			url += "&SEARCHCONFIG=config.cfg";
		}
		else
		{
			url += "&SEARCHCONFIG=";
			url += searchconfig;
		}
		if ( currentkeyword &&
			currentkeyword != "" )
		{
			url += "&HIGHLIGHT_KEYWORD" + currentkeyword;
		}
	//alert( "Document has multiple image sets - " +
	//	   "redirecting to the 'Select Images' page" );
	window.location.replace( url );
}

//  Compare the contents of the CGI parameters 'ID' and 'VID' with the
//    contents of the 'allvids' array, to see whether these parameters
//    indicate a particular image set.
//
//  The VID number which identifies an image set can either be numeric
//    (e.g. '12345') or may contain an embedded dash (e.g. '23456-01').
//
//  The optional 'VID' CGI parameter specifies a VID number in its
//    original form (e.g. '12345' or '23456-01').
//
//  The mandatory 'ID' parameter may specify a value from the <VIDREF>
//    tag, instead of the numeric EEBO ID.  This is an alphanumeric
//    identifier formed by prefixing the VID number with a 'V' and
//    converting any dash to a 'D' (e.g. 'V12345' or 'V23456D01').
//    This gives an identifier which are recognisably distinct from the
//    numeric EEBO ID, and still indexed as a single word by OpenText.
//
//  This function returns the array index of the entry which has been
//    selected from the 'allvids' array (if found); or -1 if no VID
//    has been explicitly selected.
//
function get_selected_vid_index( idparam, vidparam )
{
	var selectedvid = "";

	//  Search through the 'allvids' array
	for ( i = 0; i < allvids.length; i++ )
	{
		var vid = allvids[ i ];
		if ( vidparam != "" && vidparam == vid )
		{
			//  The VID parameter explicitly selects this VID
			return i;
		}

		//  Convert the VID number into the VIDREF identifier that may
		//    be specified in the ID parameter (in place of the EEBO ID)
		//
		//  This VIDREF is formed by prepending a 'V' prefix to the VID
		//    and converting any embedded dash to a 'D'.  For example:
		//      '12345' --> 'V12345', and '23456-01' --> 'V23456D01'
		//  (The VIDREF takes this form so that it is recognisably
		//    distinct from the all-digit EEBO ID, and is alphanumeric
		//    so that OpenText indexes it as a single word)
		//
		var vidref = "V" + vid;
		var dashpos = vidref.indexOf( "-" );
		if ( dashpos > 0 )
		{
			vidref = vidref.substring( 0, dashpos ) + "D" +
					 vidref.substring( dashpos + 1, vidref.length );
		}

		if ( idparam == vidref )
		{
			//  The ID parameter explicitly selects this VID
			return i;
		}
	}

	//  No VID has been selected.  Return -1
	return -1;
}

//  Write the 'Click image to ...' radio buttons on the Adjust View page,
//    which control whether a click on the image causes it to redisplay
//    at a higher zoom level; or just be panned so that the clicked
//    location is the new centre point
//
//  The CGI parameter 'CLICKACTION', containing either ZOOM (the default)
//    or RECENTRE determines which radio button is selected
//
function write_adjustview_buttons( clickaction )
{
	var i, j;

	var buttons = "";

	var zoomflag     = "CHECKED";
	var recentreflag = "";
	if ( clickaction.toUpperCase() == "RECENTRE" )
	{
		zoomflag     = "";
		recentreflag = "CHECKED";
	}
		
	buttons = "<INPUT TYPE=\"RADIO\" NAME=\"CLICKACTION\" " +
			  "VALUE=\"ZOOM\" " + zoomflag + ">" + "Zoom" +
			  "<INPUT TYPE=\"RADIO\" NAME=\"CLICKACTION\" " +
			  "VALUE=\"RECENTRE\" " + recentreflag + ">" + "Re-Center";
	document.write( buttons );
}

//  Write the 'Select window size: ...' links on the 'Adjust View & Print'
//    page.  Clicking one of these links causes the page to be redisplayed
//    with a different viewport size.
//
//  The single parameter is the CGI parameter 'VIEWPORT' which contains
//    the current viewport dimensions.  The corresponding 
//
function write_viewport_sizes( viewport )
{
	var i;

	var currentsize = currentviewportwidth + "x" + currentviewportheight;
	//alert( "write_viewport_sizes: current size = '" + currentsize + "'" );

	var viewportlist = new Array( "320x200", "vpSized1", 22, 10,
								  "530x400", "vpSized2", 18, 11,
								  "640x480", "vpSized3", 22, 13,
								  "768x576", "vpSized4", 24, 15,
								  "925x700", "vpSized5", 20, 17 );
	
	var text = "";
	for ( i = 0; i < viewportlist.length; i += 4 )
	{
		var size   = viewportlist[ i ];
		var imgsrc = viewportlist[ i + 1 ];
		var width  = viewportlist[ i + 2 ];
		var height = viewportlist[ i + 3 ];
		var imgtag = "";
		if ( size == currentsize )
		{
			imgtag = '<IMG SRC="/images/' + imgsrc + '_on.gif" ' +
					 'WIDTH="' + width + '" HEIGHT="' + height + '" ' +
					 'BORDER="0" ' +
					 'ALT="Window - ' + size + ' pixels" ' +
					 'TITLE="Window - ' + size + ' pixels">';
			text += imgtag + "&nbsp;";
		}
		else
		{
			imgtag = '<IMG SRC="/images/' + imgsrc + '_off.gif" ' +
					 'WIDTH="' + width + '" HEIGHT="' + height + '" ' +
					 'BORDER="0" ' +
					 'ALT="Window - ' + size + ' pixels" ' +
					 'TITLE="Window - ' + size + ' pixels">';
			text += "<A HREF=\"javascript: redisplay_with_viewport( '" +
					size + "' )\">" + imgtag + "</A>&nbsp;";
		}
	}
	document.write( text );
}
//alert( "Completed definition of function: write_viewport_sizes ..." );

function redisplay_with_viewport( viewport )
{
	//  Setup all the form parameters for redisplaying the page
	redisplay();
	//  Update the viewport setting
	document.CONTROLSFORM.VIEWPORT.value = viewport;
	//  Submit the form
	document.CONTROLSFORM.submit();
}

//  Output the HTML for the viewport, on the 'Adjust View and Print' page.
//  This is surrounded by buttons for panning the image
function write_viewport_table( )
{
	var text = "";

	//  Form the HTML for the image in the middle of the viewport
	//
	//  This is enclosed within its own table so as to ensure that
	//    Netscape 4 does not collapse the cell containing the
	//    image when the user resizes the page.
	//
	//  For Netscape 4 the image is enclosed within a hyperlink to
	//    the function which processes the image click.
	//
	//  For Explorer this interferes with the 'OnClick' handler
	//    for the image, so no hyperlinking is done.
	//
	var pageimage =   '<TABLE WIDTH="' + currentviewportwidth + '" ' +
				      'HEIGHT="' + currentviewportheight + '" ' +
				      'CELLSPACING="0" CELLPADDING="0" BORDER="0">' +
				      '<TR><TD ALIGN="CENTER" VALIGN="MIDDLE">';
	if ( isNS4 )
	{
		pageimage +=  '<A HREF="javascript: ns4_process_image_click()">';
	}
	pageimage +=      '<IMG NAME="PAGEIMAGE" ' +
	                  'SRC="' + get_viewport_image() + '" ' +
				      'WIDTH="' + currentviewportimagewidth + '" ' +
				      'HEIGHT="' + currentviewportimageheight + '" ' +
				      'BORDER="0">';
	if ( isNS4 )
	{
		pageimage +=  '</A>';
	}
	pageimage +=      '</TD></TR></TABLE>';

	//  Start the table for the viewport
	var tablewidth = currentviewportwidth - 0 + 100;
	text += '<TABLE BGCOLOR="#CCCCCC" WIDTH="' + tablewidth + '" ' +
			'CELLPADDING="0" CELLSPACING="0" BORDER="0">\n';

	//  Top row, with the "up" arrow
	var cellwidth = Math.round( 0.5 * ( currentviewportwidth - 50 ) );
	text += '<TR>\n' +
			'<TD WIDTH="50" HEIGHT="50">&nbsp;</TD>\n' +
			'<TD WIDTH="' + cellwidth + '">' +
			'Click on arrows to move in desired direction</TD>\n' +
			'<TD WIDTH="50" ALIGN="CENTER">' +
			'<A HREF="javascript: recentre_image( 50, 0 )">' +
			'<IMG SRC="/images/arrow_up.gif" WIDTH="18" HEIGHT="18" ' +
			'BORDER="0" ALT="Scroll image up"></A>' +
			'</TD>\n' +
			'<TD WIDTH="' + cellwidth + '">&nbsp;</TD>\n' +
			'<TD WIDTH="50">&nbsp;</TD>\n' +
			'</TR>\n';

	//  Middle row, with "left" and "right" arrows, and the clickable image
	text += '<TR>\n' +
			'<TD WIDTH="50" ALIGN="CENTER">' +
			'<A HREF="javascript: recentre_image( 0, 50 )">' +
			'<IMG SRC="/images/arrow_left.gif" WIDTH="18" HEIGHT="18" ' +
			'BORDER="0" ALT="Scroll image left"></A>' +
			'</TD>\n' +
			'<TD COLSPAN="3" ALIGN="CENTER" VALIGN="MIDDLE" ' +
			'WIDTH="' + currentviewportwidth + '" ' +
			'HEIGHT="' + currentviewportheight + '">' + pageimage + '</TD>\n' +
			'<TD WIDTH="50" ALIGN="CENTER">' +
			'<A HREF="javascript: recentre_image( 100, 50 )">' +
			'<IMG SRC="/images/arrow_right.gif" WIDTH="18" HEIGHT="18" ' +
			'BORDER="0" ALT="Scroll image right"></A>' +
			'</TD>\n' +
			'</TR>\n';

	//  Bottom row, with the "down" arrow
	text += '<TR>\n' +
			'<TD WIDTH="50" HEIGHT="50">&nbsp;</TD>\n' +
			'<TD WIDTH="' + cellwidth + '">&nbsp;</TD>\n' +
			'<TD WIDTH="50" ALIGN="CENTER">' +
			'<A HREF="javascript: recentre_image( 50, 100 )">' +
			'<IMG SRC="/images/arrow_down.gif" WIDTH="18" HEIGHT="18" ' +
			'BORDER="0" ALT="Scroll image down"></A>' +
			'</TD>\n' +
			'<TD WIDTH="' + cellwidth + '">' +
			'Click on arrows to move in desired direction</TD>\n' +
			'<TD WIDTH="50">&nbsp;</TD>\n' +
			'</TR>\n';

	//  Finish the table
	text += '</TABLE>\n';

	//  Middle row, with the 
	document.write( text );
}

//  Get the URL for the image displayed in the viewport
//
//  This is stored in the 'currentimageurl' variable, so that the
//    'Print current view' link can retrieve it.
//
function get_viewport_image()
{
	//  Form the string which specifies the cropping to be applied 
	//    to the scaled GIF image
	var x1 = currentcropleft;
	var y1 = currentcroptop;
	var x2 = currentscaledwidth - 1 - currentcropright;
	var y2 = currentscaledheight - 1 - currentcropbottom;
	var cropping = x1 + "," + y1 + "-" + x2 + "," + y2;

	var url = "/fetchimage" +
			  "?vid=" + currentvid +
			  "&page=" + currentpage +
			  "&width=" + currentscaledwidth +
			  "&crop=" + cropping;

	//  Store this URL, for the 'Print current view' link
	currentimageurl = url;

	return url;
}

//  Nestscape 4 event handler for tracking the mouse position on the page
//
//  The variables 'currentmousex' and 'currentmousey' are populated
//    with the position of the mouse pointer relative to the top left
//    corner of the image
//
function ns4_track_mouse_position( event )
{
	//  Determine the location on the page of the page image
	//
	//  This is enclosed in the central cell of a table, with the
	//    surrounding cells being 50 pixels in width and height
	//    (so as to enclose the arrows etc. for panning the image)
	//
	//  This table occurs at the very end of the document, at the
	//    left edge of the page.  Note that Netscape adds a margin
	//    of approximately 8 pixels to the left and bottom of the
	//    document
	//
	//  The image (with dimensions currentviewportimagewidth/height)
	//    is centred within the table cell for the viewport (with
	//    dimensions currentviewportwidth/height)
	//
	var imagex1 = 8 + 50 + Math.round( ( currentviewportwidth - currentviewportimagewidth ) / 2 );
	var imagex2 = imagex1 + currentviewportimagewidth;
	var imagey2 = window.document.height - 8 - 50 -
						Math.round( ( currentviewportheight -
										currentviewportimageheight ) / 2 );
	var imagey1 = imagey2 - currentviewportimageheight;

	//  Obtain the position of the mouse pointer within the image
	//
	currentmousex = event.pageX - imagex1;
	currentmousey = event.pageY - imagey1;

	/*
	window.status = "ns4_track_mouse_position: " +
					"mouse at (" + currentmousex + "," +
					currentmousey + ") within " +
					currentviewportimagewidth +
					"x" + currentviewportimageheight + " image";
	*/
}

//  Netscape 4 - handle a mouse click on the page image
//  This function is invoked by the hyperlink around the image
function ns4_process_image_click( )
{
	//  The click position is given by the 'currentmousex' and
	//    'currentmousey' variables which are updated as the
	//    mouse is moved
	//
	//alert( "ns4_process_image_click: click at (" + currentmousex + "," + currentmousey + ")" );
	process_image_click( currentmousex, currentmousey );
}

//  Internet Explorer - handle a mouse click on the page image
//  This function is invoked as the 'OnClick' handler of the image
function ie4_process_image_click( )
{
	//  The event specifies the click location relative to the 
	//    image on which the event occurred
	//alert( "ie4_process_image_click: click at (" + window.event.offsetX + "," + window.event.offsetY + ")" );
	process_image_click( window.event.offsetX, window.event.offsetY );
}

//  Process the mouse click at a pixel position in the page image
function process_image_click( clickx, clicky )
{
	//  Determine the zoom level for the image (according to which
	//    of the Zoom / Re-centre radio buttons are selected).
	var newzoom = currentzoom;
	if ( document.CONTROLSFORM.CLICKACTION[0].checked )
	{
		//  Zoom button selected; increase zoom level by half
		newzoom = Math.round( 1.5 * currentzoom );
		//  Reduce this if larger than the maximum possible
		if ( newzoom > maxzoom ) newzoom = maxzoom;
		//alert( "Zooming in from " + currentzoom + " to " + newzoom );
	}

	//  If the existing image does not fill all of the viewport, and
	//    the zoom level is being increased, then the new image will
	//    be bigger.  We need to handle this by redisplaying the
	//    whole page
	//
	if ( newzoom > currentzoom && 
		 ( currentviewportimagewidth < currentviewportwidth ||
		   currentviewportimageheight < currentviewportheight ) )
	{
		//  Call the 'redisplay' function to setup the form fields
		//    corresponding to the current page and new zoom level
		//alert( "Redisplaying page, at new zoom level = " + newzoom );
		redisplay( currentpage, newzoom );
		//  Submit the form
		document.CONTROLSFORM.submit();
	}

	//  Otherwise, when the existing image does fill the whole viewport,
	//    the panning and zooming can be handled by just updating this
	//    image (without refreshing the whole page)
	//
	else
	{
		pan_and_zoom_image( clickx, clicky, newzoom );
	}
}

//  Pan and (possibly) zoom the image in the viewport.
//
//  This is called as a result of a mouse click on the image (to zoom
//    or recentre) or on one of the up / down / left / right arrows
//    (which pan the image by 1/4 of the viewport width / height)
//
//  The arguments are the pixel location within the viewport image
//    that gives the new centre point (e.g. from a mouse click) and
//    the new zoom level.
//
function pan_and_zoom_image( imagex, imagey, zoom )
{
	//  Ensure that the supplied coordinates are within the image
	/*
	alert( "pan_and_zoom_image:\n" +
		   "imagex = " + imagex + "\n" +
		   "imagey = " + imagey + "\n" +
		   "currentviewportimagewidth = " + currentviewportimagewidth + "\n" +
		   "currentviewportimageheight = " + currentviewportimageheight + "\n" );
	*/
	imagex = ( imagex < 0 ) ? 0 :
			 ( imagex > currentviewportimagewidth )
							? currentviewportimagewidth :
			 imagex;
	imagey = ( imagey < 0 ) ? 0 :
			 ( imagey > currentviewportimageheight )
							? currentviewportimageheight :
			 imagey;

	//  Compute the new centre position that corresponds to the clicked
	//    location, as a proportion of the entire page image
	//
	var xfraction = imagex / currentviewportimagewidth;
	var yfraction = imagey / currentviewportimageheight;
	var xpos = Math.round( currentleftpos * ( 1.0 - xfraction ) +
						   currentrightpos * xfraction );
	var ypos = Math.round( currenttoppos * ( 1.0 - yfraction ) +
						   currentbottompos * yfraction );
	var centrepos = xpos + "," + ypos;
	//alert( "New centre position = " + centrepos );

	//  Recalculate the image dimensions etc. for any new zoom level
	//    and the current viewport size
	var viewport = currentviewportwidth + "x" + currentviewportheight;
	set_zoom_and_viewport( zoom, viewport );

	//  Recalculate the cropping for this new centre position
	set_cropping( centrepos );

	//  Update the image with the new URL
	var url = get_viewport_image();
	document.PAGEIMAGE.src = url;
}

//  Re-centre the image in the viewport, onto a point given by a
//    certain percentage of the width and height of the image
//
//  This is used by the up / down / left / right arrows which are
//    used to pan round the image.
//
function recentre_image( percentx, percenty )
{
	//  Get the pixel coordinates in the image corresponding to the
	//    supplied percentage positions
	//    
	var imagex = Math.round( 0.01 * percentx * currentviewportimagewidth );
	var imagey = Math.round( 0.01 * percenty * currentviewportimageheight );

	//  Pan the image to re-centre at the requested point
	//
	pan_and_zoom_image( imagex, imagey, currentzoom );
}

//  Setup the event handler used for the mouse click on the page image
function setup_event_handlers( )
{
	//  For Netscape 4, track the mouse position via an event handler
	//    on the document.  When the user clicks on the image, the
	//    stored mouse position is used to determine the click location.
	//
	if ( isNS4 )
	{
		window.captureEvents( Event.MOUSEMOVE );
		window.onmousemove = ns4_track_mouse_position;
		//alert( "Setup mouse position tracker for Netscape 4" );
	}

	//  For Internet Explorer 4 etc. the image object supports an
	//    OnClick event directly
	else
	{
		document.PAGEIMAGE.onclick = ie4_process_image_click;
	}
}

//  Display the current page image on its own, in a new window, for printing
function print_current_view()
{
	window.open( currentimageurl );
}

//  From the 'Adjust View & Print' page, go back to the 'Document Images' page
function back_to_images( )
{
	//  Form the URL for the 'Document Images' page
	//    using the current VID and page number
	//
	var url = "/search/full_rec?SOURCE=pgimages.cfg" +
			  "&ACTION=ByID" +
			  "&ID=" + currentid +
			  "&FILE=" + currentsessionfile +
			  "&SEARCHSCREEN=" + currentsearchscreen +
			  "&VID=" + currentvid +
			  "&PAGENO=" + currentpage
	window.location = url;
}

function write_highlighted_keyword_field ( hlkw )
{
	var text = "";

	document.write( text );
}

function set_highlighted_keyword ( hlkw )
{
	currentkeyword = hlkw;
}

// -->
