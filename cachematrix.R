## makeCacheMatrix:creates an object that can cache the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }

  get <- function() {
    x
  }

  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() {
    i
  }

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()

  if(!is.null(i)) {
    message("getting cached data")
  }
  else {
    m <- x$get()
    ## Calculate the inverse using matrix multiplication
    i <- solve(m)
    ## Cache the inverse on the object
    x$setInverse(i)
  }
  i
}