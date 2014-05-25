## Put comments here that give an overall description of what your
## functions do

## function accepts a matrix, caches that matrix and computes the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	set <- function(y) {
         x <<- y
         m <<- NULL
     	}

	get <- function() x
	
	savematrix <- function(solve) m <<- solve
	getinverse <- function() solve(x)

	
	list(get = get, 
	savematrix = savematrix,
	getinverse = getinverse)		


}

## function accepts a matrix, checks to see if the inverse has been previously calculated
## if so it is returned, if not the matrix inverse is calculated

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'

	z <- x$getinverse()

	if (!is.null(z))  {
		message("getting cached data")
		return(z)
	}	
	
	data <- x$get()
	
	m <- solve(data)

	x$savematrix(m)
	
	m

}