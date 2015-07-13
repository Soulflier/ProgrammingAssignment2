## Functions for working on a special "matrix" object that can cache its inverse

## makeCacheMatrix function creates a special "matrix". It returns a list of functions to
## get matrix, set matrix, get inversed matrix and set inverted matrix

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y){
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setcached <- function(inverted) m <<- inverted
   getcached <- function() m
   list(set = set, get = get, setcached = setcached, getcached = getcached)
}


## cacheSolve calculates the inverse of our "matrix" created with previous function.
## First it checks if the inverse has been calculated. If yes, it returns that inverted matrix.
## if not, it calculates inverted matrix (function solve()) and sets this matrix into the
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getcached()
   if (!is.null(m)) {
      message("getting cached data")
      return (m)
   }
   data <- x$get
   m <- solve(data)
   x$setcached(m)
   m
}
