## create a matrix with getter ad setter to store matrix value and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get    <- function() { return(x) }
  setinv <- function(inverse) { inv <<- inverse }
  getinv <- function() { return(inv) }
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## For x being a CacheMatric 'object' as defined above, efficiently compute 
## the inverse of x, by caching its value once it is computed once.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  return(inv)
}

test.matrix <- matrix(c(1,2,3, 0,1,4, 5,6,0),
                      nrow=3, ncol=3, byrow=TRUE)