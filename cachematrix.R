## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
  invM <- NULL
  
  set <- function(y) { # setter
    m <<- y
    invM <<- NULL
  }
  
  get <- function() m # getter
  
  setinv <- function(inverse) invM <<- inverse # setter of inverse
  
  getinv <- function() invM # getter of inverse
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(m, ...) {
  inv <- m$getinv()
  if (!is.null(inv)) { # inverse matrix exists
    message("cached inverse matrix")
    print(inv)
    return(inv)
  }
  
  mtrx <- m$get() # retrieve original
  if (!ncol(mtrx)==nrow(mtrx)) { # not a square matrix, not invertible 
    
    message("not square matrix, set inverse to the original")
    m$setinv(mtrx)
    
  } else if (det(mtrx)==0) { # singular matrix
    
    message("singular matrix, set inverse to the original")
    m$setinv(mtrx)
    
  } else { # inverse and cache matrix
    
    invmtrx <- solve(mtrx)
    m$setinv(invmtrx)
  }
  m  
}
