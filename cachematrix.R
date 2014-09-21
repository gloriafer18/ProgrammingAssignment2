## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    # initialize caches with NULL
    caches <- NULL
  
    # store a matrix
    setMatrix <- function(newValue) {
      x <<- newValue
      # flush the cache
      caches <<- NULL
    }
  
    # returns the matrix
    getMatrix <- function() {
      x
    }
  
    # cache the argument that was given
    cacheInverse <- function(solve) {
      caches <<- solve
    }
  
    # get the cached value
    getInverse <- function() {
      caches
    }
  
    # return a list. Each named element of the list is a function
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # get the cached value
    inverse <- x$getInverse()
    # if a cached value exists return it
    if(!is.null(inverse)) {
      message("getting cached info")
      return(inverse)
    }
    # otherwise get the matrix, caclulate the inverse and store it in
    # the cache
    data <- x$getMatrix()
    inverse <- solve(data)
    x$cacheInverse(inverse)
    
    # return the inverse
    inverse
}