## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL ## Creates a null matrix
  set <- function(y) {  ## set the value of the matrix
    x <<- y
    inverse <<- NULL
  }
  get <- function() ## get the value of the matrix
    x  
  setinverse <- function(inv) ## set the value of the inverse matrix
    inverse <<- inv   
  getinverse <- function() ## get the value of the inverse matrix
    inverse
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse) ## list containing a function to: (1)set the value of the matrix (2)get the value of the matrix(3)set the value of the inverse matrix. (4)get the value of the inverse matrix.
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
