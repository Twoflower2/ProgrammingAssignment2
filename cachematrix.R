############################################################################################
## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
## cacheSolve() This function computes the inverse of the special "matrix" returned  
##              by makeCacheMatrix() above. If the inverse has already been calculated
##              (and the matrix has not changed), then cacheSolve() should retrieve the
##              inverse from the cache.
############################################################################################

## This function creates a special "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    ##settter of the matrix and solved(inverse) matrix initialization as global assignment
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ##getter of the matrix
    get <- function() x
    
    ##setter of the inverse matrix using global assignment
    setsolve <- function(solve) s <<- solve

    ##getter of the inverse matrix
    getsolve <- function() s
    
    
    ##list of all the functions returned in makeCacheMatrix()
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned  
## by makeCacheMatrix() above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve() should retrieve the
## inverse from the cache.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    
    ##Check if inverse matrix was cashed and if so return this matix as s 
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ##Get cashed matrix
    data <- x$get()
    
    ##Get inverse from data
    s <- solve(data, ...)
    
    ##Setter to cashe the inverse matrix
    x$setsolve(s)
    
    ##return s
    s
}

