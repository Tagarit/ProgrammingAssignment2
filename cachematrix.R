## This function creates a square invertible matrix object that can
## cache its inverse


makeCacheMatrix <- function(x = matrix()) {
                 # stores the cached value
                 # initialize to NULL                                 
                 m <- NULL
                 
                 # create a matrix 
                 set <- function (y){
                            x <<- y
                            m <<- NULL
                 }
                 
                 # get the value of the matrix
                 get <- function () x
                 # invert the matrix and store in m
                 
                 setmatrix <- function(solve) m <<- solve
                 # get the inverted matrix from m
                 
                 getmatrix <- function()m
                 
                 # return the created functions to the working environment
                 list(set=set, get=get,
                    setmatrix=setmatrix,
                    getmatrix=getmatrix)
} 


## This function computes the inverse of the matrix returned 
## by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        # Return a matrix that is the inverse of 'x'
                 m <- x$getmatrix()
                 
                 # return the inverted matrix if it exists
                 if (!is.null(cache)){
                            message("getting cached data")
                                                 
                            # dislplays the matrix on the console                     
                            return(cache)
                 }
                 
                 # creates a matrix since it does not exist
                 matrix <- x$get()
                 
                 # set and return the inverse of a matrix 
                 m <- solve(matrix, ...)
                 
                 # set the inverted matrix in 'm' 
                 x$setmatrix(m)
                 
                 # displays the matrix on the console
                 m
                 }

