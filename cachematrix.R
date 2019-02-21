## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  #this fx returns the list given below
  list(set = set, get = get, setinv = setinv , getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)){
    message("Getting cached invertible matrix")
    return(inv)
    
  }
  # if inv is NULL then we get the data and then find its inverse 
  # and then return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
