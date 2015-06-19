##The functions below create an object that stores a squared matrix and 
##caches its inverse.

####################################################################
## Example for executng these functions:                          ##
##                                                                ##
##  ma<-makeCacheMatrix(matrix(rnorm(16),4))                      ##
##  cacheSolve(ma)                                                ##
##                                                                ##
####################################################################

##MakeCacheMatrix function:
##Create an object containing functions for accessing a matrix and its inverse
##
##Parameters
## x-squared matrix
##
##Return
## special object (a list ) containing the functions for setting and getting 
## the matrix x and its inverse
##   -set the value of the matrix
##   -get the value of the matrix
##   -set the matrix's inverse
##   -get the matriz's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  mx <- NULL
  
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) mx <<- solve
  
  getinverse <- function() mx
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##cacheSolve function:
##calculate the inverse of the matrix created with the 'makeCacheMatrix' function.
##It gets the matrix's inverse if it is cached (has already been created). 
##Otherwise, it creates the matrix's inverse and sets it in the cache using 
##the setinverse function.
##
##Parameters
## x - the matrix to get its inverse

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  
  ma <- x$getinverse()
  
  if(!is.null(ma)) {
    message("getting cached data")
    return(ma)
  }
  
  data <- x$get()
  ma <- solve(data)
  x$setinverse(ma)
  
  ma
  
}
