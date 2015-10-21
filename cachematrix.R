##These functions help to make compitation of inverse matrix more efficient. 
##If an inverse for a matrix has been calculated already, 
##the functions will take it from the cache instead of recalculating it again.

## This function constructs a vector that is a list of 4 different functions: 
##set and get the value of the matrix, set and get the inverse matrix.

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


##This function checks if the inverse of the matrix above has already been calculated. 
##If yes, it returns the inverse, otherwise, it calculates the inverse.

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
