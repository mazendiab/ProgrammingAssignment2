
# The function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.
# The function returns a list containing a function to set the value of the matrix, get the value of the matrix, 
# set the value of the inverse and get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # this function sets the value of the matrix 
    x <<- y
    inv <<- NULL
  }
  get <- function() x  # this function gets the value of the matrix
  setinverse <- function(inverse) inv <<- inverse # this function sets the inverse of the matrix
  getinverse <- function() inv  # this function gets the value of the inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}

# The function (cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache, 
# But if the inverse has not been computed before then the cacheSolev function will calculate the inverse and store it in the Cache.

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