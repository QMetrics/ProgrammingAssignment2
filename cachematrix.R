## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
get<-function(){x
}
set<-function(y){

  if (identical(x,y)==F){
    i<<-NULL
  }
  x<<-y 
}
getinverse<-function(){
  i
}
setinverse<-function(inverse)
  i<<-inverse
list(get=get,set=set,getinverse=getinverse,setinverse=setinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  if (is.null(x$getinverse())==F){
    message("getting cached data")
    return(x$getinverse())
  }  
    x$setinverse(solve(x$get()))
    x$getinverse()
}
