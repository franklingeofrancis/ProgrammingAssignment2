## PROGRAM OBJECTIVE
## The objective of this program is to calculate the inverse of matrix 
## It also tries to utilize lexical scoping feature of R to ensure that the once 
## an inverse gets calculated then it is stored in the cache memory, thereby
## avoiding recalculation and leading to saving of computing time


##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setminv <- function(matrixinverse) minv <<- matrixinverse
  getminv <- function() minv
  list(set = set, 
       get = get,
       setminv = setminv,
       getminv = getminv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getminv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setminv(minv)
  minv
  
}
