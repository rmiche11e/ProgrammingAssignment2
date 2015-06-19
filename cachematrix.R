## These functions make use of lexical scoping to pull
## a previously calculated inverse matrix from the cache
## rather than recalculating an inverse matrix that has
## already been calculated. If the inverse of the matrix
## has not already been calculated, these functions will
## calculate it and store it in the cache for potential
## future retrieval.

## This first function creates a list of functions where
## the value of the matrix can be set (set) or retrieved 
## (get) and the value of the inverse matrix can also be 
## set (setinverse) or retrieved (getinverse). These values
## are set using the <<- operator, which means the values
## assigned to these objects are within an environment that 
## is different than the environment of the function.


makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        set <- function(y) {
                x <<- y 
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function takes the cached matrix from the
## previous function (makeCacheMatrix) and checks to
## see if the inverse matrix for the provided cache
## matrix has been calculated. If the inverse has been
## calculated, it is pulled from the cache and returned.
## If the inverse of the matrix has not been calculated,
## the inverse will be calculated and then returned.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i 
}
        ## Return a matrix that is the inverse of 'x'