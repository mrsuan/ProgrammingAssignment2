## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## This pair of functions will cache the inverse of a matrix.

## Write a short comment describing this function

## The function 'makeCacheMatrix' creates a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created with above function. 
## First checks to see if the inverse has already been calculated. If so, it 
## gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the data and sets the value of the inverse in the 
## cache via the setinverse function.

## Assumes matrix is inversible

cacheSolve <- function(x, ...) {
        
        ## Returns a matrix that is the inverse of 'x' from cache
        i <- x$getinverse()
        if(!is.null(i)) { 
                message("getting cached data")
                return(i)
        }
        ## Computes inverse if there is no data from cache
        data <- x$get()
        i <- solve(data, ...)
        ## setinverse to put value of inverse into cache
        x$setinverse(i)
        return(i)
}
