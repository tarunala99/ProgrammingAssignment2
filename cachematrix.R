## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function returns a list of functions. The functions present 
##in the list modify the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) m <<- mean
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## This function looks if the matrix entry has changed. If it has 
## then it recomputes or else delivers the answer from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
