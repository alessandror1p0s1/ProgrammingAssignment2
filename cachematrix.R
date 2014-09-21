## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## set a matrix 'x'
  setMatrix <- function(matrix) {
    x <<- matrix
    ## a new matrix is assigned as 'x' so the cache flushed by setting it to 'NULL'
    cache <<- NULL
  }
  
  ## returns the new matrix 'x'
  getMatrix <- function() {
    x
  }
  
  ## cache the given 'solve' argument used later 
  ## to calculate the inverse matrix
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  ## get the cached value
  getInverse <- function() {
    cache
  }
  
  
  ## return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse cached value
  inverse <- x$getInverse()
  
  ## return the inverse cached value for the current matrix if it has been already calculated
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## if it does not exist, get the matrix, caclulate the inverse and set it in
  ## the cache
  data <- x$getMatrix()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  
  ## return the inverse matrix 'x'
  inverse
}