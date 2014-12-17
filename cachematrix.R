## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix contains 4 functions . set , get ,setinvert,getinvert
## matrix can be initialized/constructed by passing matrix as parameter 
## example a <- makeCacheMatrix(matrix(c(1, 2, 5, 6), 2, 2))
## get function outputs current values of matrix
## > a$get()
##       [,1] [,2]
## [1,]    1    5
## [2,]    2    6
## set  function takes matrix as parameter and  assigns new values . Matrix is not validated to check if is invertible
## a$set(matrix(c(7, 8, 9, 0), 2, 2))

## set for assigning a new matrix 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y  #Global assignment
                i <<- NULL
           }
        get <- function() {x}
        setinvert <- function(inv) {i <<- inv} # No calculation is done here . it is simple assignment
        getinvert <- function() { i}   # returnig global value
        list(set = set, get = get, setinvert=setinvert,getinvert=getinvert)
}


## Write a short comment describing this function
## Invert matrix can be solved by using solve function
## usage :  b <- cacheSolve(a)
##  > b
##      [,1]  [,2]
## [1,] -1.5  1.25
## [2,]  0.5 -0.25
## Validate result by performing matrix multiplication and verifying result is identity matrix
## > b%*%a$get()
##       [,1] [,2]
## [1,]    1    0
## [2,]    0    1




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## check current value of invert matrix . if it null , solve for invert matrix . if it is not null return current value
        i <- x$getinvert() 
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
      
        i <- solve(x$get())  # solve for invert matrix for current global matrix
        x$setinvert(i)       # assign solved matrix to global value for reuse
        i
}



