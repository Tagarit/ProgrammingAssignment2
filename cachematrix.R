## This function creates a square invertible matrix object that can
## cache its inverse


makeCacheMatrix <- function(x = matrix()) {
                 cache <- NULL
                 set <- function (y){
                            x <<- y
                            cache <<- NULL
                 }
                 get <- function () x
                 setmatrix <- function(solve) cache <<- solve
                 getmatrix <- function()cache
                 list(set=set, get=get,
                    setmatrix=setmatrix,
                    getmatrix=getmatrix)
} 


## This function computes the inverse of the matrix returned 
## by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
                 cache <- x$getmatrix()
                 if (!is.null(cache)){
                            message("getting cached data")
                            return(cache)
                 }
                 matrix <- x$get()
                 cache <- solve(matrix, ...)
                 x$setmatrix(cache)
                 cache
                 }

