## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL  # Variable to store the inverse of the matrix
  set <- function(matrix) {
    mat <<- matrix  # Assign the matrix to the local variable
    inv <<- NULL    # Reset the cached inverse when the matrix changes
  }
  get <- function() mat  # Get the matrix
  setInverse <- function(inverse) inv <<- inverse  # Set the cached inverse
  getInverse <- function() inv  # Get the cached inverse
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" and cache it
cacheSolve <- function(cacheMatrix) {
  # Check if the inverse is already cached
  inverse <- cacheMatrix$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached data.")
    return(inverse)
  }
  
  # If not, calculate the inverse and cache it
  data <- cacheMatrix$get()
  inverse <- solve(data)
  cacheMatrix$setInverse(inverse)
  inverse
}
