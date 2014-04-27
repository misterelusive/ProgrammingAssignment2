## 
## Function name : makeCacheMatrix
## Description : 
## It creates a list comprising of getters and setters and also other functions
## set(x) - to set the value of the matrix
## get() - to retrive the value of the matrix
## setInverse(inverse) - to set the value of inverse
## getInverse() - to get the value of the cached matrix inverse
##

makeCacheMatrix <- function(theMatrix = matrix()) {
	# Set inv to be NULL initially
    	inv <- NULL
	
	# This functions assigns the value of x to theMatrix
    	set <- function(x) {
    		theMatrix <<- x
    		inv <<- NULL
    	}
	
	# This is for retriveing the value of theMatrix
    	get <- function() theMatrix
	
	# To assign the value of inverse
    	setInverse <- function(inverse) inv <<- inverse
	
	# To retrive the value of inverse
    	getInverse <- function() inv
	
    	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


##
## Function name : cacheSolve 
## Description : 
## It computes the inverse of the special "matrix"
## created using makeCacheMatrix.
##

cacheSolve <- function(theMatrix, ...) {
    ## Return a matrix that is the inverse of 'theMatrix'
    inv <- theMatrix$getInverse()
    
	## Check whether the inverse has been computed previously. If yes,
	## then it retrives the value from the cache and further computation
	## is aborted.
	if (!is.null(inv)) {
        message("Retrieving from cache...")
        return(inv)
    }
	
	# If the inverse has not been computed previously then compute it.
    	data <- theMatrix$get()
    	inv <- solve(data, ...)
	
	# Set the inverse of the matrix.
    	theMatrix$setInverse(inv)
    	inv
}
