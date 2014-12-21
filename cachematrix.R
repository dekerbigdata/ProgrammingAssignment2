## Pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" 
##                  object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
  
  # default value inverse
  inv <- NULL
  
  # setter matrix 
  set <- function(x) {
    mat <<- x
    inv <<- NULL
  }
  
  # getter matrix
  get <- function() mat
  
  # setter inverse
  setinv <- function(y) inv <<- y
  
  # getter inverse
  getinv <- function() inv
  
  # returns list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: This function computes the inverse of the 
##             special "matrix" returned by makeCacheMatrix. 
##             If the inverse has already been calculated 
##             then it retrieves the inverse from the cache.

cacheSolve <- function(cache_mat, ...) {
  inv <- cache_mat$getinv()
  
  # if is already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not then solve
  mat <- cache_mat$get()
  inv <- solve(mat)
  cache_mat$setinv(inv)
  return(inv)
}





