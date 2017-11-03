## This pair of functions will cache the inverse of a matrix.

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## First initialize objects x and matrixinverse for later use
  matrixinverse <- NULL
  ## set() takes an argument y and assigns the input argument to x in the makeCachematrix() environment then assigns the value NULL 
  ## to the matrixinverse object in order to clear any values previously cached by prior executions of cacheSolve()
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL 
  }
  get <- function() x ## defines the getter for x. x is not defined in get() so R retrieves it's value from makeCacheMatrix()
  setinverse <- function(solve) matrixinverse <<- solve  ##defines the setter for solve and assigns the input argument solve to matrixinverse in the makeCachematrix environment
  getinverse <- function() matrixinverse ##defines the getter for matrixinverse
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

 


## This function takes a special matrix and checks to see if it's inverse is stored in cache.  If so it returns the cached inverse
## if not it calculates the inverse, stores it to cache and returns the inverse

cacheSolve <- function(x, ...) {
  matrixinverse <- x$getinverse()  ## calls to see if there is a previously cached value for matrixinverse of matrix x
  if (!is.null(matrixinverse)){ ## if there is a cached value return the cached data rather than recalculating inverse
    message("getting cached data")
    return(matrixinverse)
  }
  data <- x$get() ## if there is no cached matrixinverse than retrieve the data for matrix x & store it as object data
  matrixinverse <- solve(data, ...) ## create the inverse of matrix x
  x$setinverse(matrixinverse) ## store the matrixinverse in cache
  matrixinverse  ## return the matrixinverse of matrix x
}
  
      

