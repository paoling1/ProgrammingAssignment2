## Put comments here that give an overall description of what your
## functions do

## 'makeCacheMatrix' creates a matrix, can cache the inverse of that matrix

## 'cacheSolve' calculates the inverse of an object created by 'makeCacheMatrix'
## if the inverse has not been calculated and retrieves the inverse from the 
## object's cache and returns the value if it has been calculated



## Write a short comment describing this function

## Formal argument 'x' is a matrix
## When matrix is created, 'I' has value NULL

## 'get' retrieves value of 'x'

## 'setInv' sets inverse of 'x', assigns  
## value to 'I'
## 'getInv' retrieves value of 'I'

## Return list with elements 'get',
## 'setInv' and 'getInv'

makeCacheMatrix <- function(x = matrix()) {
        
        I <- NULL 
        get <- function() x
        setInv <- function(Inv) {
                I <<- Inv
        }
        getInv <- function() I
        list(get = get, setInv = setInv,
             getInv = getInv)
}



## Write a short comment describing this function

## Retrieves inverse of x, assigns 
## value to 'I'

## If inverse already calculated,
## retrieve inverse from cache of x
## and return value

## If not, compute inverse of 'x',
## store its value in cache of 'x'
## and return its value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        I <- x$getInv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInv(I)
        I
}
