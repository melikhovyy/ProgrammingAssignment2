## The functions <makeCacheMatrix> and <cacheSolve> are designed to create a special
## object that stores a numeric matrix and caches its inverse.
##
####################################################################################
## Example of usage:
##  source("cachematrix.R")                                         # load R-file
##  myMatrix <- makeCacheMatrix( matrix(rnorm(100*100), 100, 100) ) # create 100-by-100 matrix
##  myMatrix$get()                                                  # show the created matrix
##  myMatrix$getinverse()                                           # no inverse matrix yet
##  myMatrix$set( matrix(rnorm(100*100), 100, 100) )                # set new 100-by-100 matrix (only for <makeCacheMatrix> object)
##  myMatrix$setinverse( matrix(rnorm(100*100), 100, 100) )         # will fail as there is a check built in
##  cacheSolve(myMatrix)                                            # create an inverse matrix (invisible mode)
##  myMatrix$getinverse()                                           # show the inverse matrix
##  cacheSolve(myMatrix)                                            # show the stored inverse matrix (invisible mode)
##  isSymmetric.matrix( myMatrix$get() %*% myMatrix$getinverse() )  # confirm that inverse matrix is found
####################################################################################

## The function <makeCacheMatrix> creates an object, i.e. a list of 4 functions to 
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inversed matrix
## 4) get the value of the inversed matrix
## 
makeCacheMatrix <- function(x = matrix()) {
  ## create a NULL inverse matrix: NA values and (0,0) dims  
  inversedMatrix <- matrix(NA,nrow=0,ncol=0)
  ## set a new matrix and also creates/sets a NULL inverse matrix
  set <- function(y) {
    x <<- y
    inversedMatrix <<- matrix(NA,nrow=0,ncol=0)
  }
  ## get a stored matrix
  get <- function() x
  ## set a new inverse matrix but after a check that this new matrix is indeed
  ## an inverse matrix of the stored matrix
  setinverse <- function(newValueForInverse, isCheckNeeded=TRUE) {
    if (isCheckNeeded) {
      if ( isSymmetric.matrix( x %*% newValueForInverse ) ) {
        inversedMatrix <<- newValueForInverse
      } else {
        message("setinverse() failed as the provided matrix is not an inverse matrix of the existing")
      }  
    } else {
      inversedMatrix <<- newValueForInverse
    }
  }
  ## get a stored inverse matrix
  getinverse <- function() inversedMatrix
  ## this list is returned 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function <cacheSolve> returns an inversed matrix received within an object
## created by <makeCacheMatrix>
## First, it checks whether the inversed matrix has been computed already.
## If yes, then it returns the existing inversed matrix.
## If not, then it calulates a new inversed matrix and stores it in the object
## created by <makeCacheMatrix>
## 
cacheSolve <- function(x, ...) {
  ## get theinverse matrix of 'x'
  inversedMatrix <- x$getinverse()
  ## check if it is not a a NULL inverse matrix, i.e. if it exists
  if (sum(dim(inversedMatrix))!=0) {
    ## if it exists, return the stored inverse matrix (in invisible mode) 
    ## after a corresponding message and exit the function 
    message("getting cached data")
    return( invisible(inversedMatrix) )
  }
  ## inverse matrix does not exist, therefore, it needs to be calculated
  ## get the stored data matrix
  dataMatrix <- x$get()
  ## inverse the stored data matrix assuming that it is a square invertible matrix
  inversedMatrix <- solve(dataMatrix, ...)
  ## set the inverse matrix but do not perform check (check has been done by solve() )
  x$setinverse(inversedMatrix, isCheckNeeded=FALSE)
  # return the inverse matrix (in invisible mode)
  invisible(inversedMatrix)
}
