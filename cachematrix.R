# The following two functions cache the inverse of a matrix rather than recompute it 
# repeatedly, to minimise computing time

# makeCacheMatrix creates a list containing a function to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the inverse of the matrix
# 4. Get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve computes the inverse of the matrix returned by makeCacheMatrix above.
# If the inverse has already been computed, the cacheSolve retrieves the inverse from
# the cache. Otherwise, it calculates the inverse of the matrix and sets the inverse in 
# the cache via the setinverse function.
# It is assumed that the matrix is always invertible.
cacheSolve<-function(x,...){
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
