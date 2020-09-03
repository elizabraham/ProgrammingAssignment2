## Cache the inverse of a matrix. There are 2 
## functions in this code : 
##
## 1. makeCacheMatrix : is a function which takes 
## a matrix as an argument and creates a special
## matrix object that caches the inverse of a 
## matrix.
##
## 2. cacheSolve : This function computes the 
## inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has 
## not changed), then the cachesolve should 
## retrieve the inverse from the cache.


## makeCacheMatrix creating cache of the inverse 
## of the matrix argument

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve checks for any calculated inverse 
## cache for the argument and returns the required
## inverse for the matrix argument in makeCacheMatrix()

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
