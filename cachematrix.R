## The following functions are intended to cache the inverse of a matrix.
## I used the solve() function in computing the inverse.

## This function creates a special "matrix" object that can cache its inverse. I largely followed the example given re: mean

makeCacheMatrix <- function(x = matrix()) {
  inverse_data <- NULL
  set <- function(y) {
    x <<- y
    inverse_data <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse_data <<- inverse
  getInverse <- function() inverse_data
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. Here, I applied the solve() function as suggested in the assignment.

cacheSolve <- function(x, ...) {
  inverse_data <- x$getInverse()
  if (!is.null(inverse_data)) {
    message("getting cached data")
    return(inverse_data)
  }
  value <- x$get()
  inverse_data <- solve(value, ...)
  x$setInverse(inverse_data)
  inverse_data
}

## Here, I used the test matrices given on one of the discussions to make sure that I'm getting the correct result.

mat_data <- makeCacheMatrix(matrix(c(1/2,-1/4,-1,3/4), nrow = 2, ncol = 2))
mat_data$get()
cacheSolve(mat_data)

mat_data <- makeCacheMatrix(matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2))
mat_data$get()
cacheSolve(mat_data)

