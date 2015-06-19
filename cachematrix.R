## This function 1) creates and sets the value of a matrix, then 2) gets the value of the matrix
## The function then processes to set the inverse (using the solve function), and then get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse= setinverse,
       getinverse = getinverse)
}



## This function looks for an existing solution for the inverse.
## If it does not exist, the inverse is calculated and stored to the cache, 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
