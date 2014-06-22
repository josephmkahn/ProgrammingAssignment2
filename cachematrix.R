## Coursera R programming,  assignment 2: develop two functions to
# 1) create a "cached matrix object" via makeCacheMatrix 
# 2) retreive the object's cached inverse, or else compute and cache if cache not yet present.

## makeCacheMatrix takes input matric x (or attempts to force into matrix)
# and returns a list with slots for  functions to retreive the original matrix,
# and to cache and retreive its inverse
makeCacheMatrix <- function(x = matrix()) {

  M <- NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) M <<- inverse
  getinverse <- function() M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} # end function makeCacheMatrix



## cachesolve returns the inverse matrix of x,
# where x is a chachedMatrix created via makeCacheMatrix function
# If the cached inverse exists for the matrix, it will be returned,
# otherwise the inverse will be computed and cached into x as a side effect
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  M <- x$getinverse()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data, ...) # solve of a square matrix with no extra arguments yields inverse
  x$setinverse(M)
  M
} # end function cacheSolve

# Example, no run:
#Xo <- matrix(rnorm(9,0,1),nrow=3,byrow=TRUE) # initial matrix Xo
#solve(Xo) # check what regular solve (inverse) gives
#X <- makeCacheMatrix(Xo)
#cacheSolve(X)
#cacheSolve(X) # second time around, uses cached results




