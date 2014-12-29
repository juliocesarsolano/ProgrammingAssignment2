## Assignment2: R-Programming

## The two functions below are used concurrently to find the inverse of a matrix x. 
## If the inverse has already been found, it is returned without the need to 
## recalculate the inverse, thus, saves computing time.

## caching the inverse of a matrix

## --------------------------------------------------------------------------------
## This function creates a special R object that 
## 1. Initializes a variable 'm' 
##    (which will be used to save inverse matrix latter, i.e. a cached data);
## 2. Provides function get() to obtain "raw" matrix (of which one needs to find 
##    its inverse);
## 3. Provides function setImatrix() to assign computed inverse matrix (of x) to m;
## 4. Provides function getImatrix() to obtain the cached inverse matrix.
## --------------------------------------------------------------------------------
makeCacheMatrix <- function(x=matrix()) {
  ## Creates a list of functions that
  ## can cache the inverse of a matrix.
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

## --------------------------------------------------------------------------------
## This function does the actual inversing of matrix x.  It first checks if the in-
## verse matrix has been found; if yes, returns the result and quits. If not, the 
## inverse of x is calculated, saved to cached, and returned.
## NOTE: argument x for this function must be cached, i.e. a list returned from
##       calling makeCacheMatrix(x).
## --------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  ## Computes the inverse of the matrix returned
  ## by makeCacheMatrix(), unless the inverse has
  ## already been calculated, in which case
  ## it retrieves it from the cache.
  
  m <- x$getInverse()
  if ( ! is.null(m)) {
    message("getting cached data")
    return(m)
  }
  m <- solve(x$get(), ...)
  x$setInverse(m)
  m
}
