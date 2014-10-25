## Put comments here that give an overall description of what your
## functions do

## create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inverse<-NULL
  
  setMatrix<-function(y){
    x<<-y
    Inverse<<-NULL
  }
  
  getMatrix<-function(){
    x
  }
  
  setInverse<-function(inverse){
    Inverse<<-inverse
  }
  
  getInverse<-function(){
    Inverse
  }
  
  list(set=setMatrix,get=getMatrix,
       setInverse=setInverse,
       getInverse=getInverse)

}


## compute the inverse of 
## the special matrix returned by the function above
## if the inverse matrix already existed, retrieve it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  Inverse<-x$getInverse()
  
  if(!is.null(Inverse)){
    message("getting cached data")
    return (Inverse)
  }
  
  data<-x$get()
  Inverse<-solve(data,...)
  x$setInverse(Inverse)
  
  Inverse
}
