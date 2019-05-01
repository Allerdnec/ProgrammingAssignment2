## Put comments here that give an overall description of what your
## functions do :

# The following pair of functions is meant to be used together. The first function 
# (makeCacheMatrix) should create a special matrix that can cache its inverse. 
# The second function (cacheSolve) computes the inverse of the special matrix 
# (created by the makeCacheMatrix function) except if the inverse has already 
# been computed, in this case, the function cacheSolve will get it from the cache.

# To use the functions you need to create a matrix x, feed it into makeCacheMatrix
# and saving it into an object (e.g a). Then you need to feed a into the cacheSolve
# function. 
# e.g: x <- matrix(rnorm(4),2,2) ; a <- makeCacheMatrix(x) ; cacheSolve(a)
# NB!: Please make sure your matrix IS invertible !!


## Write a short comment describing this function:
# This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## Write a short comment describing this function:
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve should retrieve the inverse from 
# the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

