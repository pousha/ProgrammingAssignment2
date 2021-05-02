## Our aim in this experiment is to write a pair of functions, namely, 
##"makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a special "matrix" object that can
##cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##cacheSolve is a function which computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the
##inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
##Solution to the abouve code-
##> m <- matrix(rnorm(16),4,4)
##> m1 <- makeCacheMatrix(m)
##> cacheSolve(m1)
##[,1]       [,2]       [,3]       [,4]
##[1,]  0.4919452 0.12148001 -0.5646491  0.1629567
##[2,] -0.7904299 0.04022381  0.2627858  0.4201680
##[3,]  0.5972144 0.59717172 -0.3489597 -0.2812774
##[4,] -0.8550518 0.18722214 -0.2584538 -0.5378928