## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(c(), nrow, ncol)) {
  # stores cached value or NULL if nothing is cached
  # initially nothing is cached so = NULL
  inverse <- NULL 
  
  # caches matrix
  set <- function(y) {
    x <<- y
    # since the matrix is assigned a new value, cache is cleared
    inverse <<- NULL
  }
  
  # Returns existing matrix
  get <- function() x 
  
  # Modify existing matrix
  setinverse <- function(solve) inverse <<- solve 
  
  # Returns matrix inverse
  getinverse <- function() inverse 
  
  # return a list. Each named element of the list is a function
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # getting cached data
  inverse <- x$getinverse() 
  # if a cached value exists return it
    if(!is.null(inverse)) { 
    message("getting cached data")
    return(inverse)
  }
  # if not: get matrix, caclulate inverse and store it in cache
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)  # Computes, caches, and returns new matrix inverse
  inverse
}


