## Put comments here that give an overall description of what your
## functions do
## functions for caching the inverse of a matrix
## Write a short comment describing this function
## Creates a special matrix object with the ability to cache its own inverse

makeCacheMatrix <- function( matx = matrix() ) {
        
        ## Initialize inverse
        inv <- NULL
        
        ## create a method for setting the matrix 
        set <- function( matrix ) {
                matx <<- matrix
                inv <<- NULL
        }
        
        ## create a method for getting the matrix
        get <- function() {
                ## Returning matrix
                matx
        }
        
        ## create a method to generate the inverse of matrix
        setInv <- function(inverse) {
                inv <<- inverse
        }
        
        ## create a method to get the inverse of matrix
        getInv <- function() {
                ## Return the inverse property
                inv
        }
        
        ## list of the methods
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## generate inverse of the matrix given by "makeCacheMatrix"
## If the inverse already exists then the "cachesolve" should load the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        
        ## Return inverse of 'x'
        matx <- x$getInv()
        
        ## return the inverse if already exists
        if( !is.null(matx) ) {
                print("use stored matrix")
                return(matx)
        }
        
        ## Get the matrix 
        newmatx <- x$get()
        
        ## Calculate inverse 
        matx <- solve(newmatx) %*% newmatx
        
        ## Set inverse 
        x$setInv(matx)
        
        ## Return  matrix
        matx
}
