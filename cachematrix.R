#RProgramming Assignment 2 - Lexical Scoping

## The functions create a matrix, and then set its inverse 
## and get the inverse of be used in the next function. The
## second function then gets the matrix generated in the function
## from the cache and inverts it and displays it.

## This function creates a matrix for future caching

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function()x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function()inv
  list(set = set, get = get,
  setinv = setinv,
  getinv = getinv)
}

## This function calculates the inverse of the matrix 
## produced by the frst function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}