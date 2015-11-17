makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) cached_inverse <<- inverse
  get_inverse <- function() cached_inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## This function creates a special "matrix" object that can cache its inverse.
## cached_inverse <- NULL begins by setting the cached_inverse to NULL.
## set <- function(y) {x <<- y; cached_inverse <<- NULL} defines a function to set the matrix, x, to a new matrix, y, and resets cached_inverse to NULL
## get <- function() x returns the matrix, x
## set_inverse <- function(inverse) cached_inverse <<- inverse sets cached_inverse to inverse
## get_inverse <- function() cached_inverse returns cached_inverse
## list(set = set, get = get,set_inverse = set_inverse,get_inverse = get_inverse) returns the 'special matrix' containing all of the functions just defined

cacheSolve <- function(x, ...) {
  invFunc <- x$get_inverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$set_inverse(invFunc)
  invFunc
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## However, it first checks to see if the inverse has already been calculated (and the matrix has not changed).
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the set_inverse function.
