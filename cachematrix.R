##Matrix inversion, a costly computation can be handled by caching the inverse of a matrix
##rather than computing it repeatedly

#makeCacheMatrix function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    x <<- y
    xinverse <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) xinverse <<- inverse
  getinverse <- function () xinverse
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated and the matrix has not changed, then the 
## cachesolve should retrieve the inverse from the cache.  

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
  }
   mat <- x$get()
   inverse <- solve(mat,...)
   x$setinverse(inverse)
   inverse
}
