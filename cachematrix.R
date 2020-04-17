## This function calculates its own inverse for a matrix using cache
## x is a square invert matrix
## what is returned is 
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse
##      The above is used for the function cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  invrs = NULL
  set = function(y) {
    x <<- y
    invrs <<- NULL
  }
  get = function() x
  setinvrs = function(inverse) invrs <<- inverse 
  getinvrs = function() invrs
  list(set=set, get=get, setinvrs=setinvrs, getinvrs=getinvrs)
}


## This will calculate the inverse from makeCacheMatrix(). But if it has been already calculated,
##     this function will retrieve the inverse from cache

cacheSolve <- function(x, ...) {
  ## x is the output from makeCacheMatrix()
  ## return is inverse input from makeCacheMatrix()
  
  invrs = x$getinvrs()
  
  # if the inverse is complete
  if (!is.null(invrs)){
    # get it from the cache and don't calculate it 
    message("getting cached data")
    return(invrs)
  }
  
  # if not calculates inverse 
  mat.data = x$get()
  invrs = solve(mat.data, ...)
  
  # places inverse in cache
  x$setinvrs(invrs)
  
  return(invrs)
}
