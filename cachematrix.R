## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        inv_Mat <- NULL
        set <- function(y) 
        {
                x <<- y
                inv_Mat <<- NULL
        }
 
        get <- function() x
        setInverse <- function(inverse) inv_Mat <<- solve(inverse)
        getInverse <- function() inv_Mat
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inv_Mat <- x$getInverse()
        
        if(!is.null(inv_Mat))
        {
                message("getting cached data")
                return(inv_Mat)
        }
        mat <- x$get()
        ## if X is a square invertible matrix, then solve(X) returns its inverse.
        inv_Mat <- solve(mat, ...)
        x$setInverse(inv_Mat)
        inv_Mat

}

## Sample commands to execute: 
## a <- matrix(c(1,2,3,4) ,2,2)
## b <- makeCacheMatrix(a)
## b
## b$get()
## b$set(a) 
## b$setInverse(a)
## cacheSolve(b)  ## "getting cached data"
