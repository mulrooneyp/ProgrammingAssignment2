## This is assignment 2 of the "R Programming" Coursera Course
## It consists of a pair of functions that cache the inverse of an invertible matrix.
##    1) makeCacheMatrix
##    2) cacheSolve
##  pass your invertible matrix into makeCacheMatrix, which returns a specially formatted list
##  pass this special list into cacheSolve & it will return the inverse of the original matrix
##  if the inverse has already been calculated, it uses a cached calcuation


##  makeCacheMatrix creates a special "matrix", which is really a list containing a function to
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


##  cacheSolve,  calculates the Inverse of the original matrix 
##    contained in the list returned from the makeCacheMatrix function
##    However, it first checks to see if the Inverse has already been calculated.
##    If so, it gets the Inverse from the cache and skips the computation. 
##    Otherwise, it calculates the Inverse of the data and sets the value of the Inverse in the cache 
##    via the setInverse function.
##    It return a matrix that is the inverse of original matrix

cacheSolve <- function(x, ...) {
  
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