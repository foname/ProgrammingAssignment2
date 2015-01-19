## To avoid cost of computation repeately on matrix inversion, this function retrieves the result if it is already existed.
## If it is not, it should be able to calculate the inverse.

## This function creates a special "matrix" object that can cache its inverse.

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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        #get and check if inverse is existed
        i <- x$getinverse()
        if(!is.null(i) && identical(x$get(),solve(x)) {
                message("getting cached data")
                return(i)
        }
        
        #calculate new matrix inverse
        data <- x$get()
        message("calculate new data")
        i <- solve(data, ...)
        
        #set into cache
        x$setinverse(i)
        i
}
