############################################################################
## Matrix inversion is usually a costly computation and there may be      ##
## some benefit to caching the inverse of a matrix rather than compute    ## 
## it repeatedly. This file contains a pair of functions that cache the   ##
## inverse of a matrix.                                                   ##
############################################################################


## This function creates a special "matrix" object that can cache its 
## inverse.  It is really a list containing functions to:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  ## this is were to store (cache) the inverse of the matrix
  inverse <- NULL
  
  ## this is the setter and getter for the matrix
  set <- function(y) { x <<- y; inverse <<- NULL }
  get <- function() { x }
  
  ## this is the setter and getter for the cached inverse
  setInverse <- function(i) { inverse <<- i }
  getInverse <- function() { inverse }
  
  ## return the functions for making a cached matrix
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## retrieve the inverse from cache
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) {
    message("Found matrix inverse in cache")
  }
  else {
    
    ## get the matrix represented by x
    m <- x$get()
    
    ## calculate the matrix inverse
    inverse <- solve(m)
    
    ## cache the inverse to be used later
    x$setInverse(inverse)
  }
     
  ## Return a matrix that is the inverse of 'x'
  return(inverse)
}
