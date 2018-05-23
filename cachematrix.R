## Pair of functions that cache the inverse of a matrix.

# function that creates matrix and cache the inverse of a matrix
makeCacheMatrix <- function(m = matrix()) {
  #Starting the inverse property
  i <- NULL
  
  # Setting the matrix
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  # Getting the matrix
  get <- function() m # Returns matrix
  
  # sets inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  #retruns the inverse matrix
  getinverse <- function() i
  
  # retruns list of the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function that returns inverse of matrix already set if not - calculates a new one

cacheSolve <- function(x, ...) {
  # return a matrix that is inverse of 'x'
  i <- x$getinverse()
  
  # Returns inverse already set
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  #calculate inverse using solve function
  i <- solve(data, ...)
  
  # set inverse to the object
  x$setinverse(i)
  
  # returns matrix
  i
}

