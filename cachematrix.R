## Returns a list containing functions that are able to cache inverse matrix value
makeCacheMatrix <- function(x = matrix()) {
  cachedValue <- NULL
  
  # Creating functions that point to the same environment
  getCache <- function() { cachedValue }
  getOriginal <- function() { x }
  setCache <- function(newVal) { cachedValue <<- newVal }
  
  # Returning the functions in a list
  list(getCache = getCache, getOriginal = getOriginal, setCache = setCache)
}

## If cache is empty, fills it with inverse. Returns the cache
cacheSolve <- function(x, ...) {
  if (is.null(x$getCache())) 
  {
    temp <- solve(x$getOriginal())
    x$setCache(temp)
  }
  ## Return a matrix that is the inverse of 'x'
  x$getCache()
}



