## The following functions help to read and store a matrix and cache its inverse. More details below


## This first function is really a list of 4 functions that help to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the matrix' inverse
## 4) get the value of the matrix' inverse

makeCacheMatrix <- function(x = matrix()) {   ## note! the value inputted must be preceded by the 'matrix' argument. 
  inv <- NULL                                 ##     e.g. makeCacheMatrix(matrix(c(2,4,3,5,6,3,4,6,7),3,3))
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This second function calculates the inverse of the matrix inputted into the first function,
## but checks beforehand to see if this inverse has been calculated before and stored in the cache.
## If the latter is true, it gets the value from the cache and skips the computation.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv           ## Return a matrix that is the inverse of 'x'
        
}
