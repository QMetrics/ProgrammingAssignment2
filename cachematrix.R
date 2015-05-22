## This file contains the functions makeCachematrix and cacheSolve

## The makeCacheMatrix is a function with a matrix as input
## The function will be used to store 4 functions:
## - get; get the matrix x
## - set; to change the matrix and put it in cache
## - getinverse; to get the inverse of the matrix
## - setinverse; to set the inverse of the matrix in put it in cache

makeCacheMatrix <- function(x = matrix()) {
## Reset the inverse
i<-NULL
## get the matrix x
get<-function(){x
}

## change the matrix and put it in cache
set<-function(y){
  ## First check whether the new matrix(y) is different from the original(x). 
  ## If so, reset the inverse
  if (identical(x,y)==F){
    ## reset the inverse
    i<<-NULL
  }
  ## Set the new matrix and put it in cache
  x<<-y 
}

## get the inverse of the matrix
getinverse<-function(){
  i
}

## set the inverse of the matrix in put it in cache
setinverse<-function(inverse)
  i<<-inverse
list(get=get,set=set,getinverse=getinverse,setinverse=setinverse)
}


## The cacheSolve function is used to calculate the inverse of a matrix and store it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## First check whether the inverse matrix is already calculated.
  if (is.null(x$getinverse())==F){
    ## If the inverse is already calculated, just return the existing inverse matrix
    message("getting cached data")
    return(x$getinverse())
  }  
    ## if the inverse matrix isnt calculated yet, calculate the inverse by using the solve function
    x$setinverse(solve(x$get()))
    ## Store the inverse in cache
    x$getinverse()
}
