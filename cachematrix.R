

## input matrix and its inverse to makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Calculate inverse
## If there is an inverse, simply return inverse
## Otherwise, calculate inverrse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  matrix <- x$get()
  inver <- solve(matrix, ...)
  x$setInverse(inver)
  inver
}
