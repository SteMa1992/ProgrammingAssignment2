## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This functions takes a matrix as input and creates a special matrix object that can cache its inverse.

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


## Write a short comment describing this function
##It computes the inverse of the special matrix returned by the above function.
##In case the inverse has already been calculated for this matrix, the result is retrieved from the cache.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


#### Test the functions ####

#X <- matrix(c(1,3,5,7),2,2)
#X_cache <- makeCacheMatrix(X)
#cacheSolve(X_cache)
#cacheSolve(X_cache)