## The following two functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
## by returning a list of functions that assign and retrieve both the matrix 
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
## Creates a default value for m
  m <- NULL
  
## Creates function that caches the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
## Creates function that retrieves the matrix  
  get <- function() x
  
## Creates function that caches the inverse of the matrix   
  setinverse <- function(solve) m <<- solve
  
## Creates function that retreives the inverse of the matrix 
  getinverse <- function() m
  
## Creates a list containing the functions needed to cache the inverse 
## of the matrix and returns them as a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  
## Checks to see if the inverse has been cached. If the inverse exists,
## the inverse of the matrix is retrieved
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
## In the event that the inverse of the matrix has not been created, 
## the get function in makeCacheMatrix is called upon to get the matrix
  matrix <- x$get()
  
## The inverse of the matrix is calculated using solve 
  m <- solve(matrix, ...)
  
## The inverse of the matrix is cached
  x$setinverse(m)
  
## Returns a matrix that is the inverse of 'x'   
  m
}
