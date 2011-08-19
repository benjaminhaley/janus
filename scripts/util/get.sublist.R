
test.get.sublist = FALSE

#
# So you can apply get to sublists like get.sublist("list$sublist")
#		
get.sublist <- function( expression )
{
	expression <- as.character( expression  )
	elements <- strsplit( expression, "\\$" )
	elements <- elements[[1]]
	#
	# Raise Error if we can't handle the input
	#
		levels = length( elements )
		tollerable = levels %in% 2:3
		if( !tollerable )	
		{
			stop( "get.sublist is designed to handle one or two levels of sublist like list$sub or list$sublist$sub, but not list$sub$sub$sub" )
		}
	#
	# Return a sub from the list
	#
	list.name <- elements[1]
	sub.name <- elements[2]

	if( levels == 2 )
	{
		object <- get( list.name )[[ sub.name ]]
	}
	if( levels == 3 )
	{
		sub.sub.name <- elements[3]
		object <- get( list.name )[[ sub.name ]][[ sub.sub.name ]]
	}	
	return( object )
}


if( test.get.sublist )
{
	a <- list()
	a$b <- list()	
	a$b$c <- list()
	expression <- c( "a$b", "a$b$c" )
	dbe <- mapply( get.sublist, expression )
}
