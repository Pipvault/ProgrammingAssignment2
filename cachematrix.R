## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#      It sets & gets the Matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

    M <- NULL
    #Sets the Matrix to a global variable outside of this function
    set <- function(y) {
      x <<- y
      M <<- NULL
    }
    #Gets the Matrix
    get <- function() x
    
    #Sets the Inverse of the Matrix
    setinv <- function(inverse) M <<- inverse
    #Gets the Inverse of the Matrix
    getinv <- function() M
    
    #Make Above into a vectored List
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
  


## Write a short comment describing this function
#   It checks the Cache for the inverse and uses the data therein (if it exists), else it solves for the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  #Check if M (the inverse Matrix) is in the cache...
    M <- x$getinv()
    
  #....and if it is, use it...
    if(!is.null(M)) {
      message("getting cached data")
      return(M)
    }
  #...if not, solve for the inverse and sets it as a global variable accessible to this function.
    data <- x$get()
    M <- solve(data, ...)
    x$setinv(M)
    M
  }

