##------------------------------------------------------------------------------
##
## cacheMatrix.R    8/16/14
## Mike Parkin  mparkin@gmail,com
##
## R Functions to cache an inverted matrix. 
## With large matrices, inverting can take a lot of processing time. 
## This functions allows the user to store the inverted matrix in a 
## persistent object. 
## 
##-------------------------------------------------------------------------------
##
## functions in this module:
  
## makeCacheMatrix:  Creates a special "matrix" object that can cache its inverse.

## cacheSolve: Computes the inverse of the special "matrix" returned by 
##    makeCacheMatrix above. If the inverse has already been calculated 
##   (and the matrix has not changed), then the cachesolve should retrieve 
##    the inverse from the cache.

## Usage:
## EX: create a matrix e,g. myMatrix <- matrix(c(4,4,-2,2,6,2,2,8,4),3,3)'
## create special matrix e.g. myspecialMatrix <- makeCacheMatrix(myMatrix)
## then you can run invertedMatrix <- cacheSolve(myspecialMatrix)


## makeCacheMatrix:  Creates a special "matrix" object that can cache its inverse.
## this function does not check to make sure the matrix is invertable. If not,
## standard R errors will be thrown.
## Usage: myspecialMatrix <- makeCacheMatrix(mymatrix)

makeCacheMatrix <- function(x = matrix()) {
  ##initialize internal function objects
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## define functions getter
  get <- function() x
  
  ## Object Functions to create and retrieve the inverted matrix

  ## using the R function "solve() to create inverse matrix
  setinverse <- function(solve) m <<- solve
  ## retreive cached invert matrix
  getinverse <- function() m
  
  ## put object functions in a list to make use/retrevial easier
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve: Computes the inverse of the special "matrix" returned by 
##    makeCacheMatrix above. If the inverse has already been calculated 
##   (and the matrix has not changed), then the cachesolve should retrieve 
##    the inverse from the cache.
##   this function does not check to make sure the matrix is invertable. If not,
##   standard R errors will be thrown.
##USage: invertedMatrix <- cacheSolve(myspecialMatrix)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## pull invert matrix from cache
  m <- x$getinverse()
  ## if inverse matrix already cached, return cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## else calculate inverse matrix with R solve() function
  data <- x$get()
  m <- solve(data, ...)
  ## store inverse matrix in cache
  x$setinverse(m)
  m  
}
