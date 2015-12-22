## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #empty matrix that, once called, will store the inverse
        set <- function(y){
                x <<- y #my matrix
                m <<- NULL #if it does not find m, it will look for it here and find it NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve #it will be used to store the result in m
        getinverse <- function() m #it will look for m, if it does not find it, it will go up until it calls setinverse
        list(set=set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
    
}


## Write a short comment describing this function
#This functionn computes the inverse of the special "matrix" returned by CacheMatrix above
#If the inverse has been calculated, then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("Getting cache data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
