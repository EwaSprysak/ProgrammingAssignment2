#makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  odwr <- NULL
  set <- function(y) {
    x <<- y
    odwr <<- NULL
  }
  get <- function() x
  setodwr <- function(solve) odwr <<- solve
  getodwr <- function() odwr
  list(set = set, get = get,
       setodwr = setodwr,
       getodwr = getodwr)
}
#This function assumes that the matrix is always invertible.
# inverse a matrix, if the matrix is inversed already, return the cached-inversed one,
# otherwise, inverse it and save the inversed matrix in cache and return it
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  odwr <- x$getodwr()
  if(!is.null(odwr)) {
    message("getting cached data")
    return(odwr)
  }
  dane <- x$get()
  odwr <- solve(dane, ...)
  x$setodwr(odwr)
  odwr
}
