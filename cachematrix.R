## MakeCacheMatrix takes in a matrix and stores it in its cache, 
## as well as a null matrix value.  The object returned is a list
## containing the helper functions that allow for the matrix and its
## to be stored and retrieved.  cacheSolve checks the cache for an 
## existing inverse matrix and returns it, or calculates and stores
## the inverse matrix using the setinv() helper function in 
## MakeCacheMatrix.

## makeCacheMatrix at initialization stores a matrix in cache and sets
## its inverse function to null and returns a list
## of functions 4 helper functions described below:  

## set() sets or resets the matrix value and changes the stored 
## matrix to NULL erasing the old matrix data.

## get() retrieves the matrix

## setinv() stores the new inverse matrix

## getinv() retrieves the value stored by the setinv() function

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
                  x <<- y
                  inv <<- NULL
          }
          
          get <- function() x
          setinv <- function(inverse){
                    inv <<- inverse
                    }
          getinv <- function() inv
          
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)


}


## cacheSolve returns the inverse matrix for x.
## If the stored inverse is *not* NULL then the cached inverse is retrieved.
## If the inverse is NULL the inverse matrix is calculated, stored in
## cache, and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        
        data <- x$get()
        ##print(data)
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
