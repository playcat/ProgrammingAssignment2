## The functions makeCacheMatrix and cacheSolve in combination allow
## calculation of matrix inverse with caching, thus allowing
## significant speed-ups when no changes have been applied
## to matrix

### Please find a comments in the bottom showing how to test

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <- NULL
  }
  get <- function() x
  setInverse <- function(inv) mi <<- inv
  getInverse <- function() mi
  list(set = set, get = get,
      setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## mi - matrix inverse
  mi <- x$getInverse()
  
  ## if mi is cached AND x hasn't changed, only then use the cache
  if (!is.null(mi)) { 
    message("getting cached data...")
    return(mi)
  }
  
  ## otherwise, do the calculation and then cache mi
  m <- x$get()
  mi <- solve(m, ...)
  x$setInverse(mi)
  mi
}

#### To test do the following:
## x <- matrix(c(1, 1, 0, 1))
## xx <- makeCacheMatrix(x)
## cacheSolve(xx)
#### and one more time to get cached
## cacheSolve(xx)
#### ... and now invert this one
## xxx <- makeCacheMatrix(xx$get())
## cacheSolve(xxx)
#### result is same as x