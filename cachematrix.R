## These pair of functions work together to generate
## a matrix' inverse and cache it, or just return the 
## already cached inverse, if it exists

## This is the main funcion generating a matrix object
## And establishing the methods to cache it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) m <<- solve
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)

}


## This is the actual matrix inverter (and caching) function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m
  
}
