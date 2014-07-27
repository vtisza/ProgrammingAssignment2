## makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inversex <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inversex <<-inverse
  getinverse <- function() inversex
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inversex <- x$getinverse()
  #Checks inverse was already cached
  if (!is.null(inversex)) {
    message("getting cached inverse matrix")
    return(inversex)
  } else {
    inversex <- solve(x$get())
    x$setinverse(inversex)
    return(inversex)
  }
}