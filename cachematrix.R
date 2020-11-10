## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns data for a matrix and returns a list
## of functions that allows for the matrix and its inverse to be 
## set and retrieved.  Any time a matrix is initialized the inv
## is set to NULL which will work as a flag indicating the matrix's
## inverse is not yet set.  

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


## cacheSolve retrieves the inverse matrix for x.
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
