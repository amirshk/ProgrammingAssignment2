## makeCacheMatrix create a matrix that can cache it's inverse
## cacheSolve will solve the inverse of the matrix and cache it

## Usage Example:
#source('cachematrix.R')
## create a matrix
#m <- matrix( c(2,4,3,1,5,7,0,1,2), nrow=3,ncol=3)
## create a cacheMatrix
#mat<-makeCacheMatrix(m)
## solve the inverse
#cacheSolve(mat)
##

## Create a matrix that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		# reset the cache solve in case the original matrix was changed
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
		 getinverse = getinverse,
		 setinverse = setinverse)
}


## Return a matrix that is an inverse of 'x' and cache it
cacheSolve <- function(x, ...) {
		m <- x$getinverse()
		# there is a cached solution, return it
		if (!is.null(m)) {
			return(m)
		}
		# there is no solution, solve and cache it
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m
}
