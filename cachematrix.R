## Put comments here that give an overall description of what your
## functions do



## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    #settter of the matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ##getter of the matrix
    get <- function() x
    
    ##setter of the inverse matrix using solve()
    setsolve <- function(solve) s <<- solve

    ##getter of the inverse matrix
    getsolve <- function() s
    
    
    ##list of all the functions returned in makeCacheMatrix()
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## Write a short comment describing this function

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

