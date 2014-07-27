## The function "makeCacheMatrix" sets the matrix within a special vector list so that the said matrix and its inverse can be stored there: 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL 		
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## "cacheSolve" determines whether the inverse has been previously solved. If the inverse has been solved, it is returned from the caching address rather than recalculated; if not, the inverse matrix is calculated and cached for later use:

cacheSolve <- function(x, ...) {
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
