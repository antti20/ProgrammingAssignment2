## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a list of functions related to the matrix
# that it takes as an input. This list contains:
# 1) a function get that returns the matrix itself, 
# 2) a function set that can be used to alter said matrix 
# 3) a function setinverse, that is used in the cacheSolve function 
# to create the inverse of the input function
# 4) a function getinverse that returns the inverse of the matrix
# (if it has been solved)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
}


## Write a short comment describing this function
# This function returns the inverse of a matrix that has been
# used as an input in the makeCacheMatrix function. 
# It also stores the inverted function in the list created
# with the makeCacheMatrix
# This function also checks if the inverted matrix has already been
# created. If that is the case it returns from the list and does
# not redo the calculation.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
