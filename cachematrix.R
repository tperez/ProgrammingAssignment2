# This set of functions will will allow you to cache the calculation of the inverse
# for a matrix.

# makeCacheMatrix creates a set of functions that contain a matrix in its state
# makeCacheMatrix returns a list containing functions to
# 1. get the value of the matrix
# 2. set the value of the matrix
# 3. get the value of the matrix inverse
# 4. set the value of the matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  get <- function() x
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  getInverse <- function() inverse
  
  setInverse <- function(i) inverse <<- i
  
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


# cacheSolve will operate on a special matrix returned by makeCacheMatrix and
#   parameters to pass to the caculation of the inverse
# the return value is the inverse matrix


cacheSolve <- function(m, ...) {
        inverse <- m$getInverse()
        
        if (!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        
        data <- m$get()
        inverse <- solve(data, ...)
        m$setInverse(inverse)
        
        inverse
}
