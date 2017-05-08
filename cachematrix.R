#################################################
## Programming Assignment 2: Lexical Scoping 
#################################################


## Assignment: Caching the Inverse of a Matrix
## Create two R functions that cache the inverse of a matrix.

## The first function makeCacheMatrix. 
## creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {     ## define the argument with default mode of "matrix"
  inv <- NULL                             ## initialize inv as NULL; hold value of matrix inverse 
  set <- function(y) {                    ## define the set function - assigns new values to the matrix x stored in the main function (makeCacheMatrix) in the parent environment 
    x <<- y                         ## hence x <<- y
    inv <<- NULL                    ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                     ## define the get fucntion - returns the matrix x 
  setinverse <- function(inverse) inv <<- inverse         ## define the setinverse function -  assigns value of inv 
  getinverse <- function() inv                            ## define the getinverse - gets the value of inv 
  list(set = set, get = get,                              ## need this list to refer to the functions with the $ operator
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Create a new R function cacheSolve.
## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, the cacheSolve retrieve the inverse from the cache.                        


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}



### Test of functions

## 2x2 matrix of the values 1:4, assign makeCacheMatrix to this matrix.
## Store as my_test 
my_test <- makeCacheMatrix(matrix(1:4, 2,2))

## use the get subset of the makeCacheMatrix function to look at the original matrix.
my_test$get()

## Get the inverse of the matrix
cacheSolve(my_test)
## The resulting inverse matrix can now be cached later.

## Run the cacheSolve function again.
cacheSolve(my_test)

## Now it uses the cached information.
## I have also instructed to print the message "getting cached data" when using cached data.
## This will print the second time we run cacheSolve(my_test). 

