## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function create a "special matrix" that its really a list with a serial
##of functions: set the value of the matrix; get the value of the matrix;
##set the value of the inverse; get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          set <- function(y) {
                    x <<- y
                    i <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) i <<- inverse
          getinverse <- function() i
          list(set = set,
                    get = get,
                    setinverse = setinverse,
                    getinverse = getinverse)
}

## Write a short comment describing this function
##This function returns the inverse of the previous "special matrix". If the
##inverse has been cached before, this function takes from there and print it,
##if not, calculates it and print it to.

cacheSolve <- function(x, ...) {
          i <- x$getinverse()
          if (!is.null(i)) {
                    message("getting cached data")
                    return(i)
          }
          data <- x$get()
          i <- solve(data, ...)
          x$setinverse(i)
          i
}
