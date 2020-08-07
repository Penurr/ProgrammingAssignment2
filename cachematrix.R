## the function calculates the inverse of a matrix

## first make a matrix

makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  setsol <- function(solve) sol <<- solve
  getsol <- function() sol
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol)
}


## examine if x value already exists, if not, than calculate using 
## solve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  sol <- x$getsol()
  if(!is.null(sol)) {
    message("getting cached data")
    return(sol)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setsol(sol)
  sol
}
