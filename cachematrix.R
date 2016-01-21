## Matrix Inverse and Caching functions
# The purpose of these funtions is to take the inverse of a matrix (if it's not already been 
# calculated) and to "cache" it in a list.  

## makeCacheMatrix
# Call this function to create a object that stores a matrix and it's inverse.  
# The object is initalised with matrix x (which should be a square matrix) and a cache of NULL.
# The object provides a setter function: setInverse(valueToCache) and a getter function: getInverse().

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solved) inv <<- solved
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## CacheSolve
# This function returns the inverse of the matrix stored in a cacheMatrix object.  
# The function checks if a cacheMatrix object has cached it's inverse.  If the inverse is
# cached, the inverse is returned with no further computation.  If the inverse hasn't been 
# cached, this function calculates the matrix's inverse using the solve() R function and then 
# caches that inverse matrix in the cacheMatrix object before returning the inverse value.  

cacheSolve <- function(x, ...) {
  #Check to see if the inverse already exists
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if here, no inverse exists, calculate it and store the value.
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
