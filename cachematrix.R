## Matrix inversion is a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly
## This module contains a pair of functions that cache the inverse of a matrix.


## This function creates a matrix object, thats is really a list containing a
## number of functions which allow you to set or get the value of the vector
## and set or get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    

}


## This function calculates the inverse of the cachedMatrix created with the makeCacheMatrix function.
## Before doing the calc, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data using the solve library function and sets the
## value of the inverse of the matrix in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
  
}
