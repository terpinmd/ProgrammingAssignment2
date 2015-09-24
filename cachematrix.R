## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## This is a utility function that takes a matrix and stores both the matrix and the inverse of the matrix.
## The function doesnt actualy compute the inverse it is just used to store the matrix and inverse of the matrix values.
##
## @PARAM X -> Of type matrix.  The original value and its inverse are stored internally.
## @RETURNS -> list of functions:  
##	1)  set/get to set and get the input param X
##      2)  setinv / setinv which get and set the inverse of the matrix X
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        funcs <- list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
	funcs
}


## Write a short comment describing this function
## 
## This function takes a makeCacheMatrix and looks to see if the inverse of the matrix has been computed.
## If so then the inverse value is returned.  Otherwise the matrix is returned from the get function, its inverse computed via solve
## and then the inverse is set in cacheMatrix and the value is returned.
##
## @PARAM X -> Of type mackeCacheMatrix of which the value is queried from.  If the value is in the getinv function then that value is 
## returned.  Otherwise the inverse of the matrix returned from the get function is computed, set then returned.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        ## First check to see if the inverse has been computed.  If it has then returne the cached data.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If we have gotten here then the inverse has not been computed so get the matrix and store it in data
        data <- x$get()
        ## Now compute the inverse of the matrix
        m <- solve(data, ...)
        ## Finally set the inverse in the makeCacheMAtrix object and return it
        x$setinv(m)
        m 
}
