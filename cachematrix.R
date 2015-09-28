## Programming Assignment 2: Lexical Scoping 
## R Programming
## Caching Inverse Matrix


## Create an special object that stores a numerical matrix 
## and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Calculates the inverse matrix of the "special matrix" checking before 
## if the calculation have already been done. In that case, just get the
## value previously stored in the cache

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  
  ## Return a matrix that is the inverse of 'x' 
  s  
}
