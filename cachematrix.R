## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special 'matrix' object that can be passed to 
## 'cacheSolve' to solve the inverse of the matrix object.
##this special object is really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of its inverse
##4.  get the value of its inverse



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)  

}


## This function computes the inverse of the matrix
## returned by makeCacheMatrix. If the inverse
## has already been calculated, then
## this function can retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inverse)
  inv
}
