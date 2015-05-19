## This pair of functions reduces computations by storing a matrix's inverse
## once it is calculated so that stored inverse can be returned in the future
## as long as the matrix isn't changed

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) 
    m <<- solve
  getinverse <- function() m 
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## function cacheSolve determines whether the inverse of matrix x has been calcuated
##   if it has been calculated, cacheSolve retrieves the inverse from cache
##   if it has not been calculated, cacheSolve computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
