## This function takes a matrix and returns the inverse of it

## This section setups up the functions for gathering the matrix data and sets up the functions for the second part of the code

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##if new Matric clear the old Cache value
  set <- function(y) { ##reseting 
    x <<- y
    m <<- NULL
  }
  get <- function() x ##grabbing the matric and returning it
  setsolve <- function(solve) m <<- solve ##setting the solve and creating the value of m
  getsolve <- function () m ##grabs inverse of the matrix
  list(set = set, get = get, ##left hand side  name in the list and right hand side if the function in a list
       setsolve = setsolve,
       getsolve = getsolve)
}


## this function actually runs the solve fucntion and gives mea the inverse
## the function checks to see if there is currently a cache value it is holding and returns it if there one


cachesolve <- function(x, ...) {##if m is not null
  m <- x$getsolve()
  if(!is.null(m)) {
    message("checking to see if you have a cache value")
    return(m) ##if there is a cache value turn the cache value
  } ##if nothing in cache do the following
  data <- x$get() #take what I index above and call it data
  m <- solve(data, ...) ##pass through function (solve) and call it m
  x$setsolve(m) #store the new value of m here
  m ##returning m
}
