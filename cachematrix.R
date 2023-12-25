## Put comments here that give an overall description of what your
## functions do

##In this assignment, the fuction creates a special type of matrix object whose inverse matrix can be cached.


## Write a short comment describing this function

##Describtion: "makeCacheMatrix <- function(x = matrix())" creates a special type of matrix object that can cache its inverse.
##In summary, it is a function designed to create a matrix object
##with enhanced capabilities for storing the matrix data, and its derived properties (such as its inverse) for efficient retrieval.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  #initializing iverse as NULL
  
  #set function for the matrix
  setMatrix <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  
  #get function for the matrix
  getMatrix <- function() x
  
  #function for the inverse
  setInverse <- function(inverseMatrix) inv <<- inverseMatrix
  
  #get function for the inverse
  getInverse <- function() inv
  
  #list of the functions
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setInverse = setInverse,
    getInverse = getInverse
  )
}





## Write a short comment describing this function

##Describtion: This function is used to get cached data.
##This fuction computes the inverse of the special “matrix” created with the function above (makeCacheMatrix).
##Firstly, thsi function will check whether the inverse number has been calculated
##if yes, it will take the inverse value from the cache and skips the calculation
##if no, it will computes the inverse of the matrix and stores it in the cache using the setinverse function.


cacheSolve <- function(cacheMatrix, ...) {
  #calculate and cache the inverse of a matrix
  
  #check if already cached
  cachedInverse <- cacheMatrix$getInverse()
  if (!is.null(cachedInverse)) {
    message("Retrieving cached inverse.")
    return(cachedInverse)
  }

  #if not cached, calculate the inverse
  matrixToInvert <- cacheMatrix$getMatrix()
  calculatedInverse <- solve(matrixToInvert, ...)
  #cache the newly calculated inverse
  cacheMatrix$setInverse(calculatedInverse)
  #return the inverse
  calculatedInverse
}




