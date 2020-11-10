## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns data for a matrix and returns a list
## of functions that allows for the matrix and its inverse to be 
## set and retrieved.  Any time a matrix is initialized the inv
## is set to NULL which will work as a flag indicating the matrix's
## inverse is not yet set.  

## set() allows for the matrix to be changed as well as setting
## the inverse matrix to NULL which so the old matrix inverse 
## can be erased.

## get() allows for the retrieval of the matrix

## setinv() stores the new inverse matrix

## getinv() retrieves the value stored by the setinv() function

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
                  x <<- y
                  inv <<- NULL
          }
          
          get <- function() x
          setinv <- function(solve){
                    ## print(solve)
                    inv <<- solve
                    ## print(inv)
                    }
          getinv <- function() inv
          
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)


}


## cacheSolve retrieves the inverse matrix for x.
## IF the stored inverse is not NULL then the cached inverse is retrieved.
## If the inverse is NULL the inverse matrix is calculated, stored in
## cache, returned

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
