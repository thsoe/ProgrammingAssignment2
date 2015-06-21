## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  
  get <- function() x
  
  setInversion <- function(solve) invMat <<- solve
  
  getInversion <- function() invMat
  
  list(set = set, get = get,
       setInversion = setInversion,
       getInversion = getInversion)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInversion()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setInversion(invMat)
  invMat
}
