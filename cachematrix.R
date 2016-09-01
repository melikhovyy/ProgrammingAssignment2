## The functions <makeCacheMatrix> and <cacheSolve> are designed to create a special
## object that stores a numeric matrix and caches its inverse.
##

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
  ## an inverse matrix of the stored matrix:
  ## 
  setinverse <- function(newValueForInverse) {
    if ( isSymmetric.matrix( x %*% newValueForInverse ) ) {
      inversedMatrix <<- newValueForInverse
    } else { message("setinverse() failed as the provided matrix is not an inverse matrix of the existing") }
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
  ## Return a matrix that is the inverse of 'x'
  ##
  inversedMatrix <- x$getinverse()
  if (sum(dim(inversedMatrix))!=0) {
    message("getting cached data")
    return(inversedMatrix)
  }
  dataMatrix <- x$get()
  inversedMatrix <- solve(dataMatrix, ...)
  x$setinverse(inversedMatrix)
  inversedMatrix
}
