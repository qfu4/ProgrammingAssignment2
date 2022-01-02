## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#function to enable cache & getters/setters
makeCacheMatrix <- function(x = matrix()) {
  #initiate a field variable
  inv <- NULL
  
  #when update content, reset the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  #get the current matrix
  get <- function() x
  
  #set the inverse matrix
  setinverse <- function(solve) inv <<- solve
  
  #get the inverse matrix
  getinverse <- function() inv
  
  #return a list of functions
  list(set = set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  #try to get the inverse value
  inv <- x$getinv()
  
  #if already calculated - already cached
  if(!is.null(inv)){
    #return the inverse matrix
    message("getting cached data")
    return(inv)
  }
  
  #else, get the matrix, calculate the inverse matrix, and store (cache)
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv 
}
