## The following R functions are able to cache potentially time-
## consuming computations, such as matrix inversions done in a 
## loop.

## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  invmat <- NULL
  set <- function(y){
    x <<- y*
    invmat <<- NULL
  }
  get <- function() {x}
  setInverse <- function(solve) {invmat<<-solve}
  getInverse <- function() {invmat}
  list(set = set, get = get, setInverser = setInverse, getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above.  If the inverse
## has already been calculated (and the matrix has not changed),
## then the chacheSolve should retrieve the inverse from the
## chache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  invmat <- x$getInverse()
  if(!is.null(invmat)){
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setInverse(invmat)
  invmat
}
