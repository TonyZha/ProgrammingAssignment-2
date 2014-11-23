## the two functions makeCacheMatrix and cacheSolve together can save time while practising matrix inversion

## This is to create an object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  
  ## intialize the inverse. 
  i <- NULL
  
  ## set the matrix
  set <- function(matrix) {
          m <<- matrix
          i <<- NULL
  }

  ## get the matrix.
  get <- function() {
      ## return the matrix
      m
  }
  
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## return a list of the methods
  list(set = set, get= get,
        setInerse = setInverse,
       getInverse = getInverse)
}


## Calculate the inverse of the matrix returned by the function makeCacheMatrix
## If the inverse has already been calculated and the matrix has no changes, 
## then the function cachesolve should retrive the inverse from the cache


cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
        ## return the inverse if its already set
    if( !is.null(m)) {
          message("getting cached data")
          return(m)
              }
    ## get the matrix from the object
    data <- x$get()
    
    ## calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    
    ## set the inverse to the object
    x$setInverse(m)
    
    ## return the matrix
    m
}
