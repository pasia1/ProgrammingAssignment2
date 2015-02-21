## Assignment 2 - pair of functions: 
## makeCacheMatrix function that creates a specal "matrix"object that can cache its inverse
## cacheSolve function that computes the inverse of the special "matrix"returned by the first function

## makeCacheMatrix is a function that creates a matrix, sets a value to the matrix, 
## gets the value of the matrix, sets the inverse matrix and gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
          x <<- y
           m <<- NULL
       }
       get <- function () x
       setinverse <- function(solve) m <<- solve
       getinverse <- function() m
       list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting inverse matrix")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
