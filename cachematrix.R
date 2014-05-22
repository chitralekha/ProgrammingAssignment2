
## The below functions are used cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
   ##set the value of the vector
   set <- function(y) {
   	 x <<- y
    	m <<- NULL
   }
   
   ##get the value of the vector
   get <- function() x
   
   ##set the value of inverse
   setinv <- function(inv) m <<- inv
   
   ##get the value of inverse
   getinv <- function() m
   list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
   ##get the inverse value from cache
   m <- x$getinv()
   if(!is.null(m)) {
 	   message("getting cached data")
	   return(m)
   }
   data <- x$get()
   
   #calculate the inverse of the matrix using solve function
   m <- solve(data, ...)
   x$setinv(m)
   ## Return a matrix that is the inverse of 'x'
   m
        
}
