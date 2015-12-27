
# The function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # this function sets the value of the matrix 
    x <<- y
    inv <<- NULL
  }
  get <- function() x  # this function gets the value of the matrix
  setinverse <- function(inverse) inv <<- inverse # this function sets the inverse of the matrix
  getinverse <- function() inv  # this function gets the value of the inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # a list of functions to be returned by the function makeCacheMatrix
}

# The function (cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {   # If inverse is already computed then the inverse is retrieved from the cache and computation of the inverse is skipped
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()   # if the inverse was not previously computed the following statements will compute the inverse and save it in the cache
  inv <- solve(data)
  x$setinverse(inv)
  inv
}