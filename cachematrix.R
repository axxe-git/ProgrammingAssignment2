##  The functions 'makeCacheMatrix' and 'cacheSolve' provide interfaces to compute and cache inverse of matrices 
##  to avoid computation time for consecutive accesses of the inverse.

## Func Name  : makeCacheMatrix
## Arguments  : a matrix (whose inverse needs to be computed and cached)
## Returns    : list of setter/getter methods to set/get original matrix and it's inverse 
##              (in cached form in a seperate environment)
## Description: This method creates the setter/getter methods to store(cache)/access the matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Func Name  : cacheSolve
## Arguments  : a list object created by the 'makeCacheMatrix' method 
## Returns    : The inverse of the original matrix provided to the 'makeCacheMatrix' method
## Description: This method checks whether the inverse of the original matrix is already computed and cached.
##              If it is cached then the cached inverse is returned. If it's not cached the inverse is computed, 
##              cached (for future access) and returned.

cacheSolve <- function(x) {
  cachedInverse <- x$getInverse()
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}


######### USAGE #########
## create a non-singular matrix
m <- matrix(c(1,1,1,4,5,3,1,8,9), nrow=3, ncol=3)

## create the inverse caching interface object
x <- makeCacheMatrix(m)                  

## run inverse solve function to cache the inverse
cacheSolve(x)

## run inverse solve function again to observe the cached inverse computed above
cacheSolve(x)