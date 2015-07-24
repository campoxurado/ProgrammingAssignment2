# The goal of this function, which stores a list of two functions, is to give
# the possibility to inverse a matrix and store it in the cache.
# So that when the inverse of a matrix is required it would be retrieved by the cache if
# it was previously calculated


# The first function makeCacheMatrix() stores 4 functions:
# 1. set() : sets de value of the matrix, changing the previous value if there was any
# 2. get() : gets the value of the matrix and display it
# 3. setInverse() : sets the value of the inverse matrix (This function just stores it) 
# 4. getInverse() : gets the value of the inverse matrix

# So that, makeCacheMatrix()creates a special "matrix object" that has the possibility
# to cache its inverse

# Note: to get the inverse of a matrix we need to use the second function beloww cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# The function cacheSolve() computes the inverse of the special "matrix object" returned
# by the function above y runs the following:
# 1. In case that there would be a matrix previously stored in the cache with setInverse(), 
# cacheSolve() would return that value without other calculations
# 2. In case that any  matrices would have been stored in the cache, cacheSolve() would calculate
# the inverse of the matrix and would retrieve it

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting data stored in cache")
    return(inv)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(inv)
    # Returns a matrix that is the inverse of x
  inv
}
