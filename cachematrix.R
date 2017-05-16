## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Creating a vector and writing a function to get 
## and set the matrix in a different environment using <<- operator
makeCacheMatrix <- function(x = matrix()) {
    mtx <- NULL
    set <- function(y) {
        x <<- y
        mtx <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mtx <<- inverse
    getinverse <- function() mtx
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## The function below checks if the mean is already calculated or not, 
## if it doesn't then it calculates the inverse using the builtin function
## SOLVE and returns the value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mtx <- x$getinverse()
    if(!is.null(mtx)){
        message("getting the cached data")
        return(mtx)
    }
    matinv <- x$get()
    mtx <- solve(matinv,...)
    x$setinverse(mtx)
    mtx
}
