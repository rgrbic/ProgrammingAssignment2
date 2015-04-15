## This function creates a special "matrix" object that can cache its inverse
## methods: set - set matrix values
##          get - get matrix values
##          setInv - set matrix inverse
##          getInv - get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  matrixInv <- NULL
  set <- function(y) {
    x <<- y
    matrixInv <<- NULL
  }
  get <- function() x
  setInv <- function(mInv) matrixInv <<- mInv
  getInV <- function() matrixInv
  list(set = set, get = get,
       setInv = setInv,
       getInV = getInV)
}


## This function calculates matrix inverse if it is equal to NULL,
## otherwise returns already calculated inverse

cacheSolve <- function(x, ...) {        
  mInv <- x$getInV()
  if(!is.null(mInv)) {
    message("getting cached inverse")
    return(mInv)
  }
  data <- x$get()
  mInv <- solve(data)
  x$setInv(mInv)
  message("calculating matrix inverse if possible")
  mInv
}


## Function for testing purposes; input is special matrix created with makeCacheMatrix function
## testMatrix <- makeCacheMatrix(matrix(runif(16),4,4))
## TestMatrixInverse(testMatrix)  # first time is inverse computed
## TestMatrixInverse(testMatrix)  # here will get cached inverse

TestMatrixInverse <- function(x = matrix()){
  
  message("Matrix by get function:")
  print(x$get())
  message("-------------")
  

  message("Matrix inverse:")
  print(cacheSolve(x))
  message("-------------")  
  
  
}
