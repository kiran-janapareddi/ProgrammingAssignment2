## makeCacheMatrix creates a matrix and defines functions to set, get the matrix and also the inverse of the matrix

## getMat displays the matrix, setMat initialized the matrix, setInv sets the Inverse of the matrix and 
## getInv displays the inverse of the matrix  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMat <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMat <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(setMat = setMat, getMat = getMat,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve receives a matrix as an input, checks if the inverse of the matrix has been calculated already
## if yes it returns the value from the cache else it calculates the inverse 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMat()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
