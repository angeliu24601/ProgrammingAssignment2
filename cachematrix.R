## Put comments here that give an overall description of what your
## functions do

## The first function, 'makeCacheMatrix' creates a special "matrix",
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinvrs <- function(inverse) invrs <<- inverse
  getinvrs <- function() invrs
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)

}

## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the setinvrs function

cacheSolve <- function(x, ...) {
  invrs <- x$getinvrs()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinvrs(invrs)
  invrs
}

## Trial run to see if the functions work
set.seed(17)
a <- matrix(rnorm(25), 5, 5)
a <- makeCacheMatrix(matrix(rnorm(25), 5, 5))
cacheSolve(a) ## First run with no cached data
cacheSolve(a) ## Second run
