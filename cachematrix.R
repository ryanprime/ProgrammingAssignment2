# makeCacheMatrix is a function that stores a list of 4 other functions and returns it to an object.
# set() - define the matrix.
# get() - retrieve the matrix.
# setinverse() - set the inverse of the matrix.
# getinverse() - retrieve the inverse of the matrix.
makeCacheMatrix <- function(m = matrix()) {
     mInverse <- NULL
     set <- function(y) {
          m <<- y
          mInverse <<- NULL    #reset inverse matrix when new matrix is defined.
     }
     get <- function() m
     setinverse <- function(inverse) mInverse <<- inverse
     getinverse <- function() mInverse
    
     #return list of 4 functions to be assigned to object.
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

#cacheSolve is a function that receives an object where
#makeCacheMatrix is stored.
cacheSolve <- function(m, ...) {
     mInverse <- m$getinverse()     #Look-up current inverse matrix for m.
     if(!is.null(mInverse)) {       #Return inverse matrix if it is already defined.
          message("getting cached data")
          return(mInverse)
     }
     data <- m$get()                #Assign current matrix to the 'data' object.
     mInverse <- solve(data, ...)   #Determine the inverse matrix.
     m$setinverse(mInverse)         #Set the inverse matrix (cache it).
     mInverse                       #Return the inverse matrix.
}
