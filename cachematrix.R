#This R file contains two functions, makeCacheMatrix() and cacheSolve().
#Using two functions, we create an inverse matrix from an input. 

# makeCacheMatrix() creates an object that can cache its inverse.
# The object created contains a list of set(), get(), setsolve(),getsolve()

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

# cacheSolve() computes the inverse of the matrix returned by this makeCacheMatrix(). 
# If the inverse has already been calculated or the matrix has not changed, 
# then the function should retrieve the inverse from the cache, avoiding recalculating

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