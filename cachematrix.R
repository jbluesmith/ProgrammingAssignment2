
#The function makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL       
      set <- function(y) {
            x <<- y   #sets the value of the matrix from the parent environment.
            m <<- NULL
      }
      get <- function() x  #gets the value of the matrix from above
      setinverse <- function(solve) m <<- solve  #sets the inverse of the matrix from the parent directory
      getinverse <- function() m    #gets the inverse of the matrix 
      list(set = set, get = get,    #creates a list of functions
           setinverse = setinverse,
           getinverse = getinverse)
}


#The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix. However, if the inverse of the same 
#input matrix has already been calculated then cacheSolve simply retrieves the inverse from the cache. Finally, it places
#the newly calculated inverse into cache.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()    #if getinverse has a cached value, it assigns that value to object 'm'
      if(!is.null(m)) {      #checks to see if the object 'm' has a value
            message("retrieving cached data")  #if object 'm' has a value, it prints a message
            return(m)                       #then it returns the value of 'm'
      }
      data <- x$get()        #if object 'm' did not have a cached value, function now defines an object to hold the input matrix
      m <- solve(data, ...)  #computes the inverse of the matrix
      x$setinverse(m)        #places the matrix inverse into cache.  
      m                      #returns the newly computed inverse of the matrix
}