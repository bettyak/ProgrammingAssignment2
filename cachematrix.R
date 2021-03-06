## The functions below cache an inverse of a matrix and then recall 
## the cached matrix for use.  The cacheSolve function requires the sue of the makeCacheMatrix
## function.  In example if your matrix is cube<-matrix(1:4, nrow=2, ncol=2), THEN your cacheSolve
## function would take the follwoing argument cacheSolve(makeCacheMatrix(cube))

## This function caches the inverse of a matrix

makeCacheMatrix <- function(inputMatrix = matrix()) {
        inverseMatrix <- NULL
        set <- function(storeMatrix) {
            inputMatrix <<- storeMatrix
            inverseMatrix <<- NULL
        }
        get <- function() inputMatrix
        setinverse <- function(solve) inverseMatrix <<- solve
        getinverse <- function() inverseMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function sets the inverse of a matrix if it is not already
## set or retrieves the inverse of a matrix if it has already been
## set and stored in a cache location and must utilize the makeCacheMatrix
##function

cacheSolve <- function(inputMatrix, ...) {
    inverseMatrix <- inputMatrix$getinverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- inputMatrix$get()
    inverseMatrix <- solve(data, ...)
    inputMatrix$setinverse(inverseMatrix)
    inverseMatrix
}
