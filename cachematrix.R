## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(matrix) {
    mat <<- matrix 
    inv <<- NULL
  }
  get <- function() mat
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(cacheMatrix) {
  inverse <- cacheMatrix$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached data.")
    return(inverse)
  }

  data <- cacheMatrix$get()
  inverse <- solve(data)
  cacheMatrix$setInverse(inverse)
  inverse
}
