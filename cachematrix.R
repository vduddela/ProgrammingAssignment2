## Put comments here that give an overall description of what 
## functions do
## Below are the functions in makeCacheMatrix
## set - #1 function used for debugging
## Get - #2 this function returns the value of the original    ## matrix
## setInverse - #3 this is called by cacheSolve() during the   ## first cacheSolve() access and it will store the value using  ## superassignment
## getInverse - #4 # this will return the cached value to 
## cacheSolve() on subsequent accesses
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) s <<- solve
        getInverse <- function() s
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

## accesses the object 'x' and gets the value of the Inverse if ## mean was already cached (not NULL) ... else calculate the   ## Inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setInverse(s)
        s   
}
