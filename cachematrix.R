## Assignment: Caching the Inverse of a Matrix

## 'makeCacheMatrix': This function creates a special "matrix" object that can cache its inverse.
## Following is the full flow of the code
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  smv <- function(y) {
          x <<- y
          m <<- NULL
  }
  gmv <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(smv = smv,
       gmv = gmv,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## 'cacheSolve': This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if (!is.null(m)) {
          message("getting cached data")
          return(m)
  }
  data <- x$gmv()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
