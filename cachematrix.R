## The following 2 functions compute the inverse of a matrix and 
## store it in the cache, so this operation doesn't need to be 
## repeated repeatedly.


## makeCacheMatrix() is called on a matrix and returns a 
## "matrix object" implemented as a list of functions.
## It permits access to the intial matrix, and its inverse 
## via get/set calls. 


makeCacheMatrix <- function(x = matrix()) {

    invX <- NULL
    set <- function(y) {
        x <<- y
        invX <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invX <<- inverse
    getInverse <- function() invX
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## cacheSolve() returns the inverse of a matrix. It takes a list 
## created by makeCacheMatrix() as input, i.e. a matrix object. 
## If the inverse has been computed previously, it is put out 
## to the screen. If no inverse is stored in memory, then the 
## computation is performed, the inverse is stored and put out 
## to the screen.

cacheSolve <- function(x, ...) {
    
    invX <- x$getInverse()
    if(!is.null(invX)) {
        message("getting cached data")
        return(invX)
    }
    data <- x$get()
    invX <- solve(data, ...)
    x$setInverse(invX)
    invX
}
