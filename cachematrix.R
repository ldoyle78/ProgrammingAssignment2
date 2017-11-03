## This pair of functions will cache the inverse of a matrix.

## makeCacheMatrix creates a special matrix object that can cache its inverse
## set() takes an argument y and assigns the input argument to x in the makeCachematrix() environment then assigns the value NULL 
## to the matrixinverse object in order to clear any values previously cached by prior executions of cacheSolve()
## defines the getter for x. x is not defined in get() so R retrieves it's value from makeCacheMatrix()
##defines the setter for solve and assigns the input argument solve to matrixinverse in the makeCachematrix environment
##defines the getter for matrixinverse


makeCacheMatrix <- function(x = matrix()) { 
  matrixinverse <- NULL
  
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL 
  }
  
  get <- function() x 
  setinverse <- function(solve) matrixinverse <<- solve  
  getinverse <- function() matrixinverse 
  
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




## This function takes a special matrix and checks to see if it's inverse is stored in cache.  If so it returns the cached inverse
## if not it calculates the inverse, stores it to cache and returns the inverse
## 1) calls to see if there is a previously cached value for matrixinverse of matrix x
## 2) if there is a cached value return the cached data rather than recalculating inverse
## 3) if there is no cached matrixinverse than retrieve the data for matrix x & store it as object data
## 4) create the inverse of matrix x
## 5) store the matrixinverse in cache
## 6) return the matrixinverse of matrix x


cacheSolve <- function(x, ...) {
  matrixinverse <- x$getinverse()  
  
  if (!is.null(matrixinverse)){ 
    message("getting cached data")
    return(matrixinverse)
  }
  
  data <- x$get() 
  matrixinverse <- solve(data, ...) 
  x$setinverse(matrixinverse) 
  matrixinverse  
}



