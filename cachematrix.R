## the first function makeCacheMatrix takes a matrix and caches its inverse
## the second function cacheSolve takes an object, checks if its matrix inverse already exists. if it doesn't it calculates and stores it in the cache.
## 


makeCacheMatrix <- function(x = matrix()) {
    matixInverse <- NULL
    set <- function(y) {
        x <<- y
        matixInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matixInverse <<- inverse
    getInverse <- function() matixInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
    
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix <- x$getInverse()
    if(!is.null(matrix)) {
        message("getting cached data")
        return(matrix)
    }
    data <- x$get()
    matrix <- solve(data, ...)
    x$setInverse(matrix)
    matrix

}
