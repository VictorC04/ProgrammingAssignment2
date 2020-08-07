## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#As the assignment requires, what this function does is give the 
#inverse of a created matrix. For example, if you create a 2x2 array 
#containing the values 2 through 5 with makeCacheMatrix, get () will 
#print that array to the console, getInverse () will print the inverse 
#of that array, but since its inverse is not yet available, will result in NULL.

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invs <<- inverse
        getInverse <- function() invs 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

#Here create the function that generates the inverse of the array 
#previously created with makeCacheMatrix. It will save the inverse 
#of the matrix, so that when requesting getInverse () on it, it will return its inverse.

cacheSolve <- function(x, ...) {
        invs <- x$getInverse() ## Return a matrix that is the inverse of 'x'
        if(!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
        mat <- x$get()
        invs <- solve(mat,...)
        x$setInverse(invs)
        invs
} 
