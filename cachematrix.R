##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) {
    c <<- solve
  }
  
  getinverse <- function() c
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##            If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(y, ...) {
  i <- y$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- y$get()
  i <- solve(data)
  y$setinverse(i)
  
  i
}
