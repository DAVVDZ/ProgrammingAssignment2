## This is the assignment for 3rd week of R programming on Coursera
## In this we have to create two functions over a matrix to get the inverse
## of that and, if it is done previously over that matrix, get the cached 
## inverse instead of compute it again.


## makeCacheMatrix does the following actions: - set & get the value of a
## matrix, and set & get the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        # Set
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # Get
        get <- function() x
        
        # Set inverse
        setinverse <- function(inverse) i <<- inverse
        
        # Get inverse
        getinverse <- function() i
        
        list(set=set, 
             get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


## Write a short comment describing this function

## CacheSolve retrieves the inverse of a matrix assuming that it is
## allways invertible.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        
        if(!is.null(i)) {
                return(i)
        }
        
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        
        print(i)
        
}