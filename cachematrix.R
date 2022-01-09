# This R file contains two functions, makeCacheMatrix() and cacheSolve().
# we create an inverse matrix from an input using two functions. 

# makeCacheMatrix() creates an object that can cache input's inverse matrix.
# The output contains a list of objects, set(), get(), setsolve(),getsolve().

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}

# cacheSolve() computes the inverse of the matrix returned by makeCacheMatrix(). 
# If the inverse has already been calculated or the matrix has not changed, 
# then the function should retrieve the inverse from the cache, avoiding recalculating.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}  
###