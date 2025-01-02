## The following function *makeCacheMatrix* creates a special matrix object that is capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function *cacheSolve* copmputes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of "x"
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cache data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# Testing the code
mat <- matrix(c(2, -1, 0, 1), 2, 2)
cachedMatrix <- makeCacheMatrix(mat)

# First call, computing the inverse
inverse1 <- cacheSolve(cachedMatrix)
print(inverse1)

# Second call, retrieving from cache
inverse2 <- cacheSolve(cachedMatrix)
print(inverse2)
