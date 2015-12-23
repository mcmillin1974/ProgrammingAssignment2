## This script has two primary functions, makeCacheMatrix and cacheSovle.  These two functions work together to allow you to 
## store a matrix value that will later be processed (and cached by cacheSolve).  Caching this results of the solve process is important as it keeps us
## from having to spend resources processing this variable again if the values haven't changed.


## makeCacheMatrix allows a user to assign and cache an invertible matrix to a variable for future retrieval.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## Set the value to NULL.  The value will stay that way until cached.
  set <- function(y) {
    x <<- y
    m <<- NULL

  }
  get <- function() x
  setinvmat <- function(mmat) m <<- mmat
  getinvmat <- function() m
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## cacheSolve first checks to see if the matrix has been processed previously and cached.  If it has, then
## the cached results are retrieved and presented
## If the matrix hasn't been processed previously, then cacheSolve will process it and present the results.
 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmat()
  ## if the value has already been cached, return that value.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)  #Determines the inverse value of the matrix (if it hasn't already happened).
  x$setinvmat(m)
  m
}
