## This file contains an implementation of a cache that is used to call the
## Solve() function with a matrix. The purpose is to cache matrix inversions
## so that a cache is checked instead of recalculating the inversion. To use
## these functions, call makeCacheMatrix() to create an instance of the
## cache. Then, call cacheSolve() to calculate the inversion or retrieve the
## cached inversion.
## e.g.
##    cache_matrix <- makeCacheMatrix(your_matrix)
##    inverted_matrix <- cacheSolve(cache_matrix)

## makeCacheMatrix creates a cached matrix object. It maintains a cache on
## the inverse of the matrix passed in. It is designed for use with the 
## function cacheSolve() below.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve is a cached implementation of the Solve() function. It returns 
## the inverse of a special type of matrix passed in and will cache the result 
## in case it is requested again. The parameter cache_matrix must be created 
## by the above function makeCacheMatrix. 
cacheSolve <- function(cache_matrix, ...) {
    ## Return a matrix that is the inverse of that in the cache_matrix object
    inv <- cache_matrix$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- cache_matrix$get()
    inv <- solve(data, ...)
    cache_matrix$setinv(inv)
    inv
}
