## The purpose of these two functions is to store a matrix, cache its inverse, 
## and use the cached inverse to save computing time.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	# Initialize the inverse matrix to null

	i <- NULL	

	# Store the input matrix

	set <- function(y) {  
		x <<- y		
		i <<- NULL		
	}

	# Retrieve the input matrix when called by cacheSolve function

	get <- function() x	

	# set the inverse matrix to inverse passed from cacheSolve function

	setinverse <- function(solve) i <<- solve	

	# Retrieve the inverse when called by cacheSolve function

	getinverse <- function() i

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}

## This function computes the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## cacheSolve returns the inverse from cache.

cacheSolve <- function(x, ...) {

	## Returns a matrix that is the inverse of 'x'

	## Call the inverse matrix from makeCacheMatrix
	
	i <- x$getinverse()

	## If the inverse is not null, return it from cache

	if(!is.null(i)) {

		message("getting cached data")
		return(i)
	}

	# If the inverse is null, get the matrix from makeCacheMatrix, 
	# compute the inverse, set it in cache, and return it.

	data <- x$get()

 	i <- solve(data, ...)

	x$setinverse(i)

	i

}
