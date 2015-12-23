## In this task the assignment is to write a pair of functions that cache the inverse of a matrix. 
## We use the operator <<-  which can be used to assign a value to an object in an environment that 
## is different from the current environment

## This function creates a special "matrix" object that can cache its inverse.
## Function 'get' returns the vector x stored in the main function, while
## function 'set' changes the vector stored in the main function.
## setinverse and getinverse are functions very similar to set and get, but
## they don’t calculate the inverse, they simply store the value of the input in a variable inv.
## into the main function makeCacheMatrix (setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated and the matrix has not changed, 
##then the cacheSolve should retrieve the inverse from the cache. If the inverse has not been calculated, 
##data gets the matrix stored with makeCacheMatrix, inv calculates the inverse, and x$setinverse(inv) stores it 
##in the object inv in makeCacheMatrix.

cacheSolve <- function(x, ...) {
         inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
