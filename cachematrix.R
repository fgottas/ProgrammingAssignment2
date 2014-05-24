## The two functions 'makeCacheMatrix' and 'cacheSolve' are used to create a
## special 'matrix' object that stores a numeric matrix and caches its inverse.
## This function creates a special "matrix" object that can cache its inverse
## which has function:
##
##   set: pass the initial matrix to the object
##   get: returns the initial matrix         
##   getinv: returns the chached inverse matrix
##   setinv: set the inverse matrix  

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<-NULL
  } 
  get <- function() x
  
  setinv <- function(inverse){
      inv <<- inverse
  }  
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Calculates the inverse matrix. First checks if the inverse matrix
## has already been calculated. If so, it gets the inverse matrix from the
## cache via the 'getinv' function and skips the calculation. 
## Otherwise, it calculates the inverse matrix and sets the 
## result in the cache via the `setinv` function.  

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv) 
  }
  message("calculating inverse matrix")
  data <-x$get()
  inv <-solve(data, ...)
  x$setinv(inv)
  inv
}

