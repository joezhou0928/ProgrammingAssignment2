## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function, makeVector creates a special "matrix"
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# The following function calculates the inverse matrix of the special "matrix" created. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix in the cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
