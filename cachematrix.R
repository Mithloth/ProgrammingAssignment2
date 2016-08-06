## These functions take an invertible matrix as an input, store  
## the matrix, compute its inverse, and store this inverse

## This function build a vector of functions to manipulate
## and store the data noted above
makeCacheMatrix <- function(x = matrix()) {
     M <- NULL
     set <- function(Y) {
          X <<- Y
          M <<- NULL
     }
     get <- function() X
     setinverse <- function(inverse) M <<- inverse
     getinverse <- function() M
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function retrieves the inverse of the matrix if it has
## been computed or otherwise computes it directly
cacheSolve <- function(X, ...) {
     M <- X$getinverse()
     if(!is.null(M)) {
          message("getting cached data")
          return(M)
     }
     data <- X$get()
     M <- solve(data, ...)
     X$setinverse(M)
     return(M)
}
