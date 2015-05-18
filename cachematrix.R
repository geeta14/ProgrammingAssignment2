## -----------------  makeCacheMatrix -----------------
##This function creates a object that holds and serves to execute operations on the input matrix.
##The function defines the following operations: set, get, setinverse and getinverse, 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ##  set: save in cache the matrix (x) on which the inverse will be calculated.
  set <- function(y) {
    x <<- y
    m <<- NULL ##Clear any previously cached matrix inverse results (m)
  }
  
  ##  get: obtain the currently cached matrix (x)
  get <- function() x
  
  ##  set inverse: save in cache the calculated inverse (m) of 
  ##the matrix currently stored in cache (x)
  setinverse <- function(inverse) m <<- inverse
  
  ##  get inverse: Obtain the calculated inverse of the matrix (m) stored in cache.
  getinverse <- function() m
  
  ##Return a list containing the defined operations
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## -----------------  cacheSolve -----------------
##"This function calculates the inverse of the object returned by makeCacheMatrix function.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache."
cacheSolve <- function(x, ...) {  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m ## Return a matrix that is the inverse of input matrix 'x'
}