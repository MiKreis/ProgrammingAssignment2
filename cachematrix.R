## The following functions are used to cache the inverse of a matrix in order to
## circumvent timeconsuming recomputations

## makeCacheMatrix provides a list of functions to store and call a matrix and
## the respective inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
        x <<-y
        mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mat <<- inverse
  getinverse <- function() mat
  list(set=set, get =get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve checks for an already cached inverse and otherwise computes and 
## stores it

cacheSolve <- function(x, ...) {
    mat <- x$getinverse()
    ## Checking whether an inverse has already been stored...
    if(!is.null(mat)) {
      message("getting cached data")
      return(mat)
    } ## Otherwise computing the inverse and storing it
    data <- x$get()
    mat <- solve(data, ...)
    x$setinverse(mat)
    mat
  
  
        ## Return a matrix that is the inverse of 'x'
}
