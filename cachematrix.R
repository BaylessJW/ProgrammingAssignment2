## Date: 11/2/2021

## This function is designed to make a cache matrix. that will become
## the inverse of itself.

makeCacheMatrix <- function(x = matrix()) 
{
  
  invert <- NULL
  set <- function(y) 
  {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invert <<- inverse
  getinverse <- function() invert
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  invert <- x$getinverse()
  if(!is.null(invert)) {
    message("solving cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinverse(invert)
}