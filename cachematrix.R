## makeCacheMatrix provides a way of caching the inverse of a user supplied matrix
## it does so by utilising the lexical scoping provided by R
## cacheSolve takes in a cacheMatrix (generated from makeCacheMatrix) and returns the inverse
## it checks first if its been previously cached, and returns if it is. Otherwise, it performs the
## calculation to determine the inverse and caches it for future reference.


## For a given matrix, returns a list of functions that allow you to:
## 1. get the matrix
## 2. set a new matrix
## 3. get the inverse of the matrix
## 4. set (override) the inverse of the matrix
makeCacheMatrix <- function(matrix = matrix()) {
	inverseMatrix <- NULL

	## getter for the matrix
	get <- function() matrix
	
	## setter for the matrix 
	## using the <<- assignment operator to search the parent environment for the variable to bind to
	## 'resets' the inverse aswell as assigning the new matrix
	set <- function(new_matrix) {
		matrix <<- new_matrix
		inverseMatrix <- NULL
	}

	## getter for the inverse matrix
	getInverse <- function() inverseMatrix
	
	## setter for the inverse matrix
	setInverse <- function(inverse) {
		inverseMatrix <<- inverse
	}
	
	## return a list of functions that 'expose' the setters and getters for the matrix & inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse for the supplied cacheMatrix
## if the inverse is already cached, this is returned
## otherwise it is calculated, cached and then returned
cacheSolve <- function(cacheMatrix, ...) {
	## Return a matrix that is the inverse of 'x'
	inverseMatrix <- cacheMatrix$getInverse()	        
	if (!is.null(inverseMatrix)) {
		message("getting cached matrix")
		return(inverseMatrix)
	}
	
	message("Nothing cached - creating inverse and caching")
	inverseMatrix <- solve(cacheMatrix$get())
	cacheMatrix$setInverse(inverseMatrix)
	inverseMatrix
}
