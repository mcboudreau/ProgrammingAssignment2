## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.  
## The following functions accomplish this task.

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
            x <<- y
            i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

## Sample Output
# > x = rbind(c(2,1), c(3,1))
# > m$get()
# [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    2    1
# [2,]    3    1
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -1    1
# [2,]    3   -2
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -1    1
# [2,]    3   -2
