## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly.

## The following two functions caches theinverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The below function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  invOutput <- x$getinverse()
  if(!is.null(invOutput)) {
    message("getting cached data.")
    return(invOutput)
  }
  data <- x$get()
  invOutput <- solve(data)
  x$setinverse(invOutput)
  invOutput
}
