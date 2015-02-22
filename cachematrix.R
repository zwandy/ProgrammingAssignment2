## This function creates a list of functions related to a matrix
## that is passed to it.
## Each function in the list will allow the user to "Set" the
## matrix to the cache, "Get" the currently Set matrix, set the 
## inverse of the matrix to the cache, and get the inverse of the 
## matrix to the cach.
## The matrix and its inverse are being set to the cache, because
## calculating the inverse of a matrix is a costly computation,
## and using the cache will prevent us from repeatly calculating it.

## THIS SCRIPT ASSUMES THAT THE MATRIX SUPPLIED IS ALWAYS INVERTIBLE

## This function creates a special "matrix" object (which is
## actually a list of functions) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y  ##setting to cache
    i <<- NULL  ##setting to cache
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse  #setting to cache
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()  ## looking into the cached value for i
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get() ## pulling the matrix from the cache
  i <- solve(data, ...)
  x$setinv(i) ## setting to cache via makeCacheMatrix
  i
}

#testcommit
