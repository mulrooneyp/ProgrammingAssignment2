## This is assignment 2 of the "R Programming" Coursera Course
## It consists of a pair of functions that cache the inverse of an invertible matrix.
##    1) makeCacheMatrix
##    2) cacheSolve
##  pass your invertible matrix into "cacheSolve", which returns the inverse matrix
##  if the inverse has already been calculated, it uses a "cached" calcuation


"special matrix" 
##  pass your "special matrix" into "cacheSolve", , which returns the inverse matric


##  makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
##    1) set the value of the Matix
##    2) get the value of the Matix
##    3) set the value of the Inverse Matix
##    4) get the value of the Inverse Matix
##  the function returns the "special" matrix ...


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
    
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  getevn<- function() environment()
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)       
}


##  This function calculates the Inverse of the special "matrix" created with #makeCacheMatrix function
##    However, it first checks to see if the Inverse has already been calculated.
##    If so, it gets the Inverse from the cache and skips the computation. 
##    Otherwise, it calculates the Inverse of the data and sets the value of the Inverse in the cache 
##    via the setInverse function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("solving data")
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
  
}