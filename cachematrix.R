## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  x.inverse = NULL
  set<- function(y){
    if(!identical(y,x)){
      x<<-y
      x.inverse <<- NULL
    }
  }
  get<- function() x
  setInverse<- function(i)x.inverse<<-i
  getInverse<- function()x.inverse
  
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  this_inverse<- x$getInverse()
  if(is.null(this_inverse)){
    this_inverse<- solve(x$get())
    x$setInverse(this_inverse)
  }
  this_inverse
}
