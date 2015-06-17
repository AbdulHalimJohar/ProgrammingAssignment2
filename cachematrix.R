## These functions are being used to perform inverse calculation
## of matrix that is being supplied as a parameter. If the same
## matrix has been calculated before, the inverse of that matrix
## will be retrieved from the cache, thus resulting a saving the
## time from performing the same calculation again.
## 

## makeCacheMatrix function will receive a matrix as a variable
## and return a list of functions nested within it.

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertable matrix
        ## return: list containing function to
        ##      1. set the matrix
        ##      2. get the matrix
        ##      3. set the inverse
        ##      4. get the inverse
        ##      this list will be used as a parameter to cacheSolve() function

        inv <- NULL
        set <- function(y) {
                ## use <<- to assign a value to an object in an 
                ## environment different from current environment
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## cacheSolve() function computes the inverse of the matrix return
## by makeCacheMatrix() function. If the inverse has already been 
## calculated and the matrix has not been changed, the inverse from
## the cache will be returned instead

cacheSolve <- function(x, ...) {
        ## @x: return from makeCacheMatrix()
        ## return: the inverse of the original matrix input to makeCacheMatrix() function
        ## if the inverse has already been calculated, return this inverse matrix from 
        ## cache and skips the computation
        ## else perform the calculation to identify the inverse matrix using solve() function

        inv <- x$get_inv()
        if (!is.null(inv)) {
                ## get the inverse from cache and skip the calculation
                message("Getting cache data")
                return(inv)
        }

        ## this is where the calculation of inverse is being done if
        ## no data or data has changed from the one in cache
        data <- x$get()
        inv <- solve(data, ...)

        ## store the inverse matrix in the cache for future call of
        ## cacheSolve() function
        x$set_inv(inv)

        return(inv)
}


##
## Sample Output:
## > m <- makeCacheMatrix()
## > m$set(matrix(c(10,9,10,5,2,1,2,3,4),3,3))
## > m$get()
##     [,1] [,2] [,3]
## [1,]   10    5    2
## [2,]    9    2    3
## [3,]   10    1    4
## > cacheSolve(m)
##     [,1] [,2] [,3]
## [1,] -2.5    9 -5.5
## [2,]  3.0  -10  6.0
## [3,]  5.5  -20 12.5
## > cacheSolve(m)
## Getting cache data
##     [,1] [,2] [,3]
## [1,] -2.5    9 -5.5
## [2,]  3.0  -10  6.0
## [3,]  5.5  -20 12.5
## >
## END OUTPUT 
