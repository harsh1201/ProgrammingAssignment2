# Read through the README.md file

# This function creates a special "matrix" object that can cache its inverse.
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   cahce value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  
  setMatrix <- function(newValue){
    x <<- newValue
    cache <<- NULL
  }
  
  getMatrix <- function(){
    x
  }
  
  cacheInverse <- function(x){
    # x is a sqaure matrix whose inverse is to be found
    cache <<- solve(x) 
  }
  
  getInverse < function(){
    cache
  }
  
  # Return a list of methods
  list(setMatrix, getMatrix, cacheInverse, getInverse)
}


# Compute the inverse of the matrix 'x'
# But first check if the result is already stored in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  mtx <- x$getMatrix()
  inverse <- x$getInverse()
  
  if(!is.null(inverse) && mtx == x){
    message('Fetching cached data...')
    return(inverse)
  }
  
  inverse <- cacheInverse(mtx)
  
  return(inverse)
}
