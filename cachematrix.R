## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching cachethe inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
## functions in this module:
  
## makeCacheMatrix:  Creates a special "matrix" object that can cache its inverse.

## cacheSolve: Computes the inverse of the special "matrix" returned by 
##    makeCacheMatrix above. If the inverse has already been calculated 
##   (and the matrix has not changed), then the cachesolve should retrieve 
##    the inverse from the cache.

## Usage:
## EX: create a matrix e,g. mymatrix <- matrix(c(4,4,-2,2,6,2,2,8,4),3,3)'
## create special matrix e.g. myspecialMartix <- makeCacheMatrix(mymatrix)
## then you can run cacheSolve(myspecialMatrix)


## makeCacheMatrix:  Creates a special "matrix" object that can cache its inverse.
## this function does not check to make sure the matrix is invertable. If not,
## standard R errors will be thrown.

makeCacheMatrix <- function(x = matrix()) {
  ##initialize internal function objects
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## define functions getter, setter
  get <- function() x
  ## using the R function "solve() to create inverse matrix
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve: Computes the inverse of the special "matrix" returned by 
##    makeCacheMatrix above. If the inverse has already been calculated 
##   (and the matrix has not changed), then the cachesolve should retrieve 
##    the inverse from the cache.
## this function does not check to make sure the matrix is invertable. If not,
## standard R errors will be thrown.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## if inverse matrix already cahaed, return cched value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## else calculate inverse matrix with R solve() function
  data <- x$get()
  m <- solve(data, ...)
  ## store in cached matrix
  x$setinverse(m)
  m
  
}
