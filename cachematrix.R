## Caching the inverse of a matrix
## Assume that the matrix supplied is always invertible

## makeCacheMatrix creates a special "matrix"
## which is really a list containing a function to





makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        	set <- function(y) {
                x <<- y
                m <<- NULL
        	} ## set the value of the matrix
        	
		get <- function() x ## get the value of the matrix
        	
		setinverse <- function(solve) m <<- solve 
			## set the value of the inverse
        	
		getinverse <- function() m 
			## get the value of the inverse
        	
		list(set = set, get = get,
             	setinverse = setinverse,
             	getinverse = getinverse)

}


## cacheSolve calculates the inverse of the special "matrix"
## created with the above function


cacheSolve <- function(x, ...) {
		
		## Return a matrix that is the inverse of 'x'
		
		m <- x$getinverse() 
			## check if the inverse has already been calculated
        	
		if(!is.null(m)) {
                message("getting cached data")
                return(m)
        	}
			## if calculated, gets it from the cache, skips the computation

        	data <- x$get()
        	m <- solve(data, ...) ## calculates the inverse
        	x$setinverse(m) ## sets the value of the inverse in the cache
       	
		m

        
}
