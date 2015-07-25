
## This function creates a special matrix
## that can cache it's inverse. Functions 
## included are
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize the cache as NULL
  m <- NULL
  
  ##Set the matrix with a new value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Retrieve the stored matrix
  get <- function() x
  
  ## Set the cache inverse
  setinverse <- function(inv) m <<- inv
  
  ## Get the cache inverse
  getinverse <- function() m
  
  ##return the list of functions
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
}


## This function computes the inverse of the
## matrix returned by the makeCacheMatrix().

cacheSolve <- function(x, ...) {
  
  inverseMatrix <- x$getinverse()
  
  ##return the cache if exist
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
   
  ##if cache does not exist, calculate it
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinverse(inverseMatrix)
  
  ## Return a matrix that is the inverse of 'inverseMatrix'
  inverseMatrix
}
