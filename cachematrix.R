## makeCacheMatrix : This function creates a special "matrix" object 
##                  that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve : This function computes the inverse of the special "matrix" 
##              returned by makeCacheMatrix above. If the inverse has already 
##              been calculated (and the matrix has not changed), then cacheSolve 
##              should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  m<-x$getmatrix()
  
  # check if inverse has been calculated and stored in cache
  if(!is.null(m)){
    message("Getting inverse from cache")
    return(m)
  }
  
  # m is null since we are here, compute inverse of matrix 
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
