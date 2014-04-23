## Brian J. Engelhardt
## R Programming Assignment #2
## 04/22/2014
##
## This file contains two functions that allow the user to construct a "special" matrix that can
## be solved using the cacheSolve() function, which will cache the time consuming inverse
## calculations, so repeated calls to cacheSolve (for the same matrix data) will not result in
## re-calculating the inverse of the matrix.


## makeCacheMatrix
## Creates a special "matrix", which is really a list of 4 functions allowing access to the data
##
## Args:
##	x : a matrix
##
## Returns:
##	A list of 4 functions that allow you to get/set the matrix, and get/set the invere of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	## set the matrix data, and set the inverse to NULL, since the matrix data has changed
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function() x

	setInverse <- function(inverse) inv <<- inverse

	getInverse <- function() inv

	list(set = set, get = get, setInverse = setInverse, 
		getInverse = getInverse )
}


## cacheSolve
## Calculate the inverse of a matrix, given a "special" matrix. If the inverse has already been
## calculated, then the inverse is just retrieved from memory
##
## Args:
##	x : a special matrix created from "makeCacheMatrix"
##
## Returns:
##	The inverse of the matrix contained in the special matrix x.

cacheSolve <- function(x, ...) {

	inv <- x$getInverse()

	if(!is.null(inv)) {
		## The inverse has already been calculated, just return it
		message("Retrieving cached inverse")
		return(inv)
	}

	## in this case, the inverse has not been calculated, so retrieve the original
	## matrix data from x, and then solve the inverse
	data <- x$get()
	inv <- solve(data, ...)

	## store the inverse back in the "special" matrix x, so that if we need it again
	## it doesn't have to be solved again
	x$setInverse(inv)

	inv
}
