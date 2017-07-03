## Put comments here that give an overall description of what your
## functions do

## This function (makeCacheMatrix) creates a matrix and sets value
## gets value of matrix
## sets value of inverse of matrix
## gets value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The function calculates the inverse of the matrix created w/ above function. 
## If first checks to see if inverse is already calculated. If so, it gets inverse from cache and 
## skips computation. Otherwise, it calculates the inverse via the setinverse function.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- inverse(data, ...)
      x$setinverse(m)
      m
        ## Return a matrix that is the inverse of 'x'
}
