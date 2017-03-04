## These functions will calculate the inverse of an invertible matrix or retrieve the inverse of such a matrix from the cache

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y 
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve 
	getinverse <- function() m 
	list( set = set, get = get,
		  setinverse = setinverse,
		  getinverse = getinverse)
}

## This function returns the inverse of the special matrix by calculating it or by retrieving it from the cached info from the previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
    	message("getting cached data")
    	return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
