## Caching the Inverse of a Matrix:
## For a very big Matix ,inversion is a costly computation
## So we want to caching the inverse of matrix rather than compute repeatedly


## This function creates a special "matrix" object that can cache its inverse.
## Cause variables x and inv stored in enclosing environment ofset, 
## get, setInverse, getInverse functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function calculates the mean of the special "matix" created with the makeCacheMatrix function
## If the inverse of matrix has already be calculated,then get the caced data
## else do the inverse calculate.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

