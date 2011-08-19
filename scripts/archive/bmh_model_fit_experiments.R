#
# Here we are going to play with discriminating between two unfair coins
#

shoot <- function( p1, p2, flips )	
{
	#
	# Roll the Dice
	#
		o1 <- ( runif( flips, 0, 1 ) > p1 )
		o2 <- ( runif( flips, 0, 1 ) > p2 )
		print( paste( "coin 1:", ( 1 - sum( o1 ) / flips ), "coin 2:", ( 1 - sum( o2 ) / flips ) ) )
	
	#
	# Create a false indepenedent varaible
	#
		moon.phase <-  sample(c( "_waning", "_waxing" ), (flips*2) , replace=T)

	#
	# Create the Data Frame
	#
		coin <- c()
		coin[ 1 : flips ] <- "_c1"
		coin[ ( flips + 1 ) : ( 2 * flips ) ] <- "_c2"
		
		outcome <- c( o1, o2 )
	 	dataset <- data.frame( coin, outcome, moon.phase )
	
	#
	# Run some regression
	#
		good.formula <- as.formula( "outcome ~ coin" )
		bad.formula <- as.formula( "outcome ~ moon.phase" )
		good.result <- glm( good.formula, family=binomial, data=dataset )
		bad.result <- glm( bad.formula, family=binomial, data=dataset )
		return( list( good.result, bad.result ) )
}

result <- shoot( p1=0.5, p2=0.1, flips=4000 )
good.glm <- result[[1]]
bad.glm <- result[[2]]
lrtest( good.glm, bad.glm )