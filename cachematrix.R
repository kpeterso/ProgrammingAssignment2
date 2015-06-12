## makeCacheMatrix creates an object that contains a matrix and the inverse
## of the matrix, once the inverse has been computed.
## cacheSolve replaces solve, returning the inverse of the cacheMatrix by
##retrieving it from the cache or calculating it.

## makeCacheMatrix creates and returns an object that contains a matrix
## that is set using x$set().  The original matrix is returned with x$get().
## x$getinverse returns what is stored in the inv variable.
## x$setinverse(data) stores data into the inv variable.
## If the cacheSolve function is used, the inv variable will contain the
## inverse of matrix x, or NULL otherwise.

makeCacheMatrix <- function(x = matrix()) {
## Return a cacheMatrix object
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a cachematrix x and returns the inverse of x, either by
## retrieving the cached inverse or, if that does not exist,  solving
## the inverse. It then caches the inverse matrix into x.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}