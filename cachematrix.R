## Functions that cache the inverse of a matrix


## Creating a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initializing the inverse variable
  i <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse matrix
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  ## Just return the inverse if its already set
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  i <- solve(data,...)
  
  ## Set the inverse to the object
  x$setInverse(i)
  
  ## Return the matrix
  i
}
