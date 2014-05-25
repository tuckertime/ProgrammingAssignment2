## Two functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse
  i <- NULL
  
  ## Method for setting matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Method to retrieve matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Method to set inverse matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to ret inverse of the matrix
  getInverse <- function() {
    ## Returns the inverse property
    i
  }
  
  ## Return list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix" function.
## If the inverse has already been calculated  (the matrix has not changed), then the inverse should be retrieved.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return inverse if prev set
  if( !is.null(m) ) {
    message("Getting cached data...")
    return(m)
  }
  
  ## Get the matrix from obj
  data <- x$get()
  
  ## Calculate the inverse with matrix multiplication
  m <- solve(data) %*% data
  
  ## Set inverse to object
  x$setInverse(m)
  
  ## Return the matrix
  m
}