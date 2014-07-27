
# This function creates a special "matrix" object.
# Return a list of functions:
#  1. set the value of the matrix
#  2. get the value of the matrix
#  3. set the inverse value of the matrix
#  4. get the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # define object for our inverse matrix
  invMatrix <- NULL
  
  #Setter function for matrix
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  #Getter function for matrix
  get <- function() x
  
  #Setter function for inverse matrix
  setinverse <- function(inverse) invMatrix <<- inverse
  
  #Getter function for inverse matrix
  getinverse <- function() invMatrix
  
  #Return value
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function inverses a matrix. If the resualt was already calculated, it returns
# the cached inversed matrix
cacheSolve <- function(x, ...) {
  
  # Trying to get data from the cache
  invMatrix <- x$getinverse()
  
  # Check if the matrix already exist and return
  if(!is.null(invMatrix)) {
    message("getting cached data.")
    return(invMatrix)
  }
  
  message("No cached data was found. Calulating the inverse matrix for first time")
  
  # If the matrix did not exist we have to calculate it
  data <- x$get()
  invMatrix <- solve(data)
  
  # Caching the inverse matrix for next time
  x$setinverse(invMatrix)
  
  # Return result of inversed matrix
  invMatrix
}