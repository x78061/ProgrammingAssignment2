## "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse.

## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix. Main benefit of this action can be exhibited through 
## streamlining of the computation process and further reductions in run time of a given program.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## "cacheSolve" function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## The purpose of theinverse matrix is that it will allow us to run  a series of cache storing tests on the
## makeCacheMatrix function. Objective of this exercise is to gain access to cached data for it will 
## drastically speed up calculations on more complex datasets.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("YAY!! Success. Got cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
## Sample run:
## > x = rbind(c(2, -1/6), c(-1/6, 2))
## > m = makeCacheMatrix(x)
## > m$get()
##            [,1]       [,2]
## [1,]  2.0000000 -0.1666667
## [2,] -0.1666667  2.0000000

## No cache in the first run
## > cacheSolve(m)
##            [,1]       [,2]
## [1,] 0.50349650 0.04195804
## [2,] 0.04195804 0.50349650

## Retrieving from the cache in the second run
## > cacheSolve(m)
## YAY!! Success. Got cached data.
##            [,1]       [,2]
## [1,] 0.50349650 0.04195804
## [2,] 0.04195804 0.50349650
## > 