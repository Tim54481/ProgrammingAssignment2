## This pair of functions allow a computation, the inverse of a matrix,
## to be cached in memory and recalled which can be a time-savings if the need to calculate 
## an inverse is in a loop and needs to done repeatedly.

## This function sets the value of a natrix obtained from a matrix call m. 
## It then gets the matrix and sets the value of the inverse of the matrix and caches it.
## Finally it gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m<<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function calculates the inverse of a matrix created with the previous function.
## Before it does this it checks to see if the inverse already exists.
## If it exists it gets the inverse from the cache.
## Otherwise it calculates the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()    ## Return a matrix that is the inverse of 'x'
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
