## These two functions allow you to perform matrix inversion and IF the inversion has already been performed, the cached result will
## be pulled versus re-running the required computation. In addition to the functions, a test case is included



## This function creates a list of functions that enable you to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
              x <<- y
              m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function 'solves' (i.e. does the matrix inversion) but will first check to see if the matrix inversion
## has already been performed, in which case it pulls the cached result and skips the computation
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

##create a matrix
A<- matrix(c(2,6,5,9,1,4,6,2,3),nrow = 3,ncol = 3)
##use first function to create list of functions for matrix A
f <- makeCacheMatrix(A)
##first time solve - no cached data, returns newly computed inverse matrix
cacheSolve(f)
##second time solve - accesses cached data, returns cached inverse matrix
cacheSolve(f)

