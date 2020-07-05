## The functions makeCacheMatrix and cacheSolve together are used for caching
## the inverse of the matrix. The first function creates an object which stores
## the matrix and it's inverse whereas the second function is used to compute or
## retrieve the inverse from the cache value stored in the first.

## A small set of function's are created and returned to the parent environment
## in the form of a list.

makeCacheMatrix <- function(x = matrix()) {
  inv <-  NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The below function computes the inverse of a matrix returned by the makecache
##Matrix function but if the inverse has already been calculated then it 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Getting the cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
