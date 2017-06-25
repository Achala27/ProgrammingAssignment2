# makes a cache matrix from a given matrix
# 1. initialize the cache Matrix 'cacheMatrix'
# assign the value NULL for the first initialization

makeCacheMatrix <- function(x = matrix()) {
 cacheMatrix <- NULL
 
 setMatrix <- function(y){
      x <<- y
      cacheMatrix <<- NULL
  }
 
 getMatrix <- function() x
 setCache <- function(inverse) cacheMatrix <<- inverse

getCache <- function() cacheMatrix

list(setMatrix = setMatrix, getMatrix = getMatrix, 
     setCache = setCache, getCache = getCache)
}

# return the inverse of a given matrix utilizing the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  cacheMatrix <- x$getCache()
  
  # 2. if the content is not null then: return the result  
  if(!is.null(cacheMatrix)) {
    
    message("loading cache matrix")
    return(cacheMatrix)
  }
  
  # 3. if the content is empty then: 
  # get the matrix, create, set, update and return the cache matrix 
  
  else {
    
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix,...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
    
  }

}