## Put comments here that give an overall description of what your
## functions do

## Solves a matrix and caches the result using a "special" matrix.
## makeCacheMatrix -> Used to create the "special" matrix
## cacheSolve -> Returns the inverse of a "special" matrix using the cached
##               value if exists.

## Write a short comment describing this function

## Creates a "special" matrix that can be cached.
## Also defines functions for getting/setting the data and the inverse value.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

	  ## Function for setting/replace the data matrix (if needed)
	  ## Note this resets the "cached" inverse value.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Function for retrieving data for the matrix
        get <- function() x
	  
        ## Function for storing the inverse value
        setinverse <- function(inverse) m <<- inverse
        
	  ## Function for retrieving the inverse value
	  getinverse <- function() m
        
	  ##Return the list of functions
	  list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## Returns the inverse of a matrix returned by makeCacheMatrix. 
## Checks the cache if there is a result already cached otherwise
## computs the results with the solve() function
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
	  ##Check if result is already cached
        m <- x$getinverse()
        if(!is.null(m)) {
		    ## Cache value found -> Return cached value
                message("getting cached data")
                return(m)
        }
	  ##Couldn't find cache -> Solve matrix then add to cache and return result.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
