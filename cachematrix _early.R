
##Kevin Cooper-Fenske's Programming Assignment 2
## [trying to figure this out, Still!]


###The first function, makeVector creates a special "vector", which 
##is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

kkdata <- matrix(c(1,3,4,5,6,23,13,9), 3,3)
kkdata
kk <- makeCacheMatrix(kkdata)
kk


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(solve) {
    s <<- solve
  }
  getinverse<- function() {
    s
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #Return vector type list of functions
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
  s <- solve(data, ...)              # otherwise, put the data in the
  x$setinverse(s)                    # 'data'. Compute the mean of the 
  s                                  # function to cache the mean
                                     # return the mean
}

kk <- makeCacheMatrix(kkdata)
kk

ans <- cacheSolve(kk)

