## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix function creates a special "matrix" object and create its inverse.

## Write a short comment describing this function
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inversion
## 4. get the value of the inversion


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
## cacheSolve function check and create the inverse of the matrix

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
