## The functions will create a vector of functions for storing and retrieving matrix inversion calculations.

## This function creates a vector containing a list of functions that will be used by the cacheSolve function.
## To use the function a vector is created with an inversible matrix as the function input.
## set() sets the value of x.
## get() returns x, and will be used to get the actual matrix to be inverted.
## getinverse() returns the value of the inverse, stored in i.
## setinverse() sets the value of the variable i.

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will use the vector created by makeCacheMatrix and return an inverse matrix.
## The function first calls getinverse() to set the i variable to the cached value of the inverse.
## The function then checks if the current i value is NULL, if it is not, it returns the cached value of the inverse.
## If the cached value is NULL the function assumes the inverse has not been calculated, and uses the get() function to 
## get the matrix data from the input vector, and saves it as the variable data.
## The function then uses solve(data) to calculate the inverse of the matrix, and sets the cached value of the inverse using 
## setinverse().

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
