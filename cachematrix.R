## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- x
  inv <- NULL

  set <- function(y) {
    m <<- y
    inv <<- NULL 
  }
  get <- function() m
  getInverse <- function() inv
  setInverse <- function(inverse) inv <<- inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
