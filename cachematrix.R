
##Kevin Cooper-Fenske's Programming Assignment 2
## [trying to figure this out, Still!]


###The first function, makeVector creates a special "vector", which 
##is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                  # Here we make four functions, and output
  set <- function(y) {                       # then as a list.
    x <<- y
    m <<- NULL
  }
  get <- function() {                        # Grabs the data file
    x
  }
  setinverse <- function(solve) {            # Sets the solve variable, from the
    s <<- solve                              # external cacheSolve function
  }
  getinverse<- function() {                  # Retrieves the matric inverse, if already
    s                                        # computed
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)             #Returns vector type list of 4 functions
}


###[Write a short comment describing this function]
## The following function calculates the mean of the special "vector"
## created with the above function. However, it first checks to see if the
## mean has already been calculated. If so, it get's the mean from the
## cache and skips the computation. Otherwise, it calculated the mean of
## the data and sets the value on the mean in the cache via the 'setmean'
## function.


cacheSolve <- function(x, ...) {     ##Goal: Return a matrix that is the inverse of 'x'
  
  s <- x$getinverse()
  if(!is.null(s)) {                  # if the mean was cached-
    message("getting cached data")
    return(s)                        #exit program without 
  }                                  #subsequent code
  data <- x$get()             
  s <- solve(data, ...)              # otherwise, puts the data in the
  x$setinverse(s)                    # 'data'. Computes the mean of the 
  s                                  # function to cache the mean, and submits answer                                    # return the mean
}
