## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # Cached matrix
    cachedinv <- NULL
    # set matrix
    setmatrix <- function(m) {
      x <<- m
      setmatrix <<- NULL
    }
    # get the matrix
    getmatrix <- function() x
    
    # set inverse matrix
    setinvm <- function(inverse) cachedinv <<- inverse
    # get the inverse matrix
    getinvm <- function() cachedinv
    
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinvm = setinvm, getinvm = getinvm)
}
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cachedinv <- x$getinvm()
  
  # If the inverse is calculated, return it
  if (!is.null(cachedinv)) {
    message("get cache")
    return(cachedinv)
  }
  # if not in cache, calculate inverse
  data <- x$getmatrix()
  cachedinv <- solve(data, ...)
  
  # then cache inv
  x$setinvm(cachedinv)
  return(cachedinv)
}
