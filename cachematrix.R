## The first function, makeCacheMatrix creates a special "Matrix", 
## which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
          x <<- y
          inv <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set=set, 
        get=get, 
        setinverse=setinverse, 
        getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above and store it in the variable INV.  If INV exists 
## then the function retrieves the inverse matix from the cache rather then computing it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
         inv <- x$getinverse()
         if(!is.null(inv)) {
            message("getting cached data.")
            return(inv)
         }
         data <- x$get()
         inv <- solve(data, ...)
         x$setinverse(inv)
         inv  
}
