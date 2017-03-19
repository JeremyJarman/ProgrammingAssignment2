## JJarman C
## Coursera R Programming week 3 assignment
## 19/3/2017

## makeCacheMatrix creates an object from a function which recieves a matrix argument
## the matrix object contains set,get,setinverse and getinverse functions
makeCacheMatrix <- function(x = matrix()) {
  
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## cacheSolve is a function which recieves a matrix object as and argument
## and calls the object's functions to return either the cached inverse 
## or the newly calculated inverse
cacheSolve <- function(x, ...) {
        
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}   
        

