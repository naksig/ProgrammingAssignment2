## Put comments here that give an overall description of what your
## functions do
##
## This pair of functions cache the inverse of a matrix.
## If the inverse has already been calculated (and the matrix has 
## not changed), 
## the functions retrieve the inverse from the cache.

## Write a short comment describing this function
## 'makeCacheMatrix`
##      This function creates a special "matrix" object
##      that can cache its inverse.
##
##     s: saves the computed value of inversed matrix.
##     set: sets the value of the matrix.
##     get: gets the value of the matrix.
##     setinv: sets the value of the inversed martix.
##     getinv: gets the value of the inversed martix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## `cacheSolve`: 
##     This function computes the inverse of the special
##     "matrix" returned by `makeCacheMatrix` above. 
##     If the inverse has already been calculated, 
##     then `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        if(!is.null(s)) {s
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}
