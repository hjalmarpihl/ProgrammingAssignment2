## These functions performs matrix inversion on a given matrix,
## provided that the matrix inversion has not previously been computed and cached,
## in which case the cached inverse is returned. If the inversion is performed, the 
## inverse is cached for future reference.

## The function "makeCacheMatrix" creates a list containing four functions to be used by "cacheSolve".
## In effect, this function is used to create the "matrix" that is to be passed to "cacheSolve".

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function "cacheSolve" returns the inverse of the matrix. 
## If it has not yet been cached, the function performes the inversion and caches the inverse.
## If it has been cached, the cached inverse is returned.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv  
}
