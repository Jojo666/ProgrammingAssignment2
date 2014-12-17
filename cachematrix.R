## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

xinverse <- NULL # this is where the result of inversion is stored
set <- function(y) {
	  x <<- y
	  xinverse <<- NULL # it also initialises xinv to null
      }
 get <- function() x # return the input matrix
      setInv <- function(inv) xinverse <<- inv # set the inversed matrix
      getInv <- function() xinverse # return the inversed matrix
list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
  }
cacheSolve <- function(x, ...) {
      m <- x$getInv() # get the inversed matrix from object x
      # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
      if(!is.null(m)) { # if the inversion result is there
	  message("getting cached data")
	  return(m) # return the calculated inversion
      }
      data <- x$get() # if not, we do x$get to get the matrix object
      m <- solve(data) # we solve it
      x$setInv(m) # we then set it to the object
      m # return the solved result
  }

}


