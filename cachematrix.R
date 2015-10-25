## Coursera: Programming in R, Programming Assignment #2
## Author: Carson McFadden
## Edited: October 25, 2015
## This script has 2 functions used to cache the inverse of an invertible matrix.
##      The first function creates a special type of matrix that takes a regular
##      matrix and extends its functionality by being able to calculate and return the inverse.

##      The second function returns the inverse solution: first it
##      checks whether this has been calculated and if so returns the already calculated solution.
##      If not, it calculates the inverse, stores it, and returns it.

## Function: makeCacheMatrix
## Inputs: x: invertible matrix
## This creates a special "matrix" type that has the following abilities
##  Set the value of the matrix to the input specified by x
##  Return the value (get) of the matrix
##  Set the value of the inverse of the matrix x
##  Return the value (get) of the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set the matrix to value specified by x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Return the matrix (x) by using the get function
  get <- function() x
  
  #Set the inverse inv using the setInv function
  setInv <- function(solve) inv <<- solve
  
  ## Return the inverse inv using the getInv function
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Function: cacheSolve
## Inputs: x: invertible matrix 
## This function takes a matrix x and checks to see if the inverse
## has already been calculated for it. If so, returns the known inverse.
## If not, the function calculates the inverse by getting the data, using the
## solve function to calculate the inverse, and sets the value of the inverse
## in the cache using the setInv function.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
