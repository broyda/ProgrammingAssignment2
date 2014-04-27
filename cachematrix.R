## Essentially creates a data structure
## to cache calculated matrix inverses.
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL;
  
  # Set the matrix
  setMatrix <- function(z) {
    x <<- z;
    cachedInverse <<- NULL;
  }
  
  # get the matrix
  getMatrix <- function() {
    x;
  }
  
  # Sets the inverse in the cache post computation
  putInverseInCache <- function(inverse) {
    cachedInverse <<- inverse;    
  }
  
  # Returns the cached inverse of the matrix
  getInverseFromCache <- function() {
    cachedInverse;
  }
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       putInverseInCache = putInverseInCache,
       getInverseFromCache = getInverseFromCache);  
}

## Returns a matrix that is the inverse of the input matrix 'x'
cacheSolve <- function(x, ...) {  
  ## Attempt to retrieve the inverse from the cache (if the data exist in the cache)
  inverse <- x$getInverseFromCache();
  if (!is.null(inverse)) {
    message("retrieving inverse from the cache");
    return(inverse);
  }
  mtx <- x$getMatrix();
  
  ## Using the solve function that R provides to compute the inverse
  inverse <- solve(mtx, ...);  
  x$putInverseInCache(inverse); ## Add result to the cache for reuse
  inverse;  
}
