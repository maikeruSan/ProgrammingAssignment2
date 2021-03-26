#' To Keep A Matrix And Its Inverse. 
#' 
#' This function can get and set a matrix and its inverse in-memory by 
#' functions contained in a returned list object.
#' @param x The default matrix whose dimension (1,1) with value "NA".  
#'
#' @return A list typed object which contains multiple set and get exposure functions.
#' @export
#'
#' @examples
cacheMatrix <- function(x = matrix()){
  # The inverse of x whose type is Matrix
  I <- NULL 
  
  # An initial function, aim to make sure I set NULL once the x reassigned.
  initialize<- function(y= matrix()){
    x<<-y
    I <<- NULL
  }
  
  set<- function(y){
    # Assign passed matrix y to global variable x and NULL to the inverse 
    # when y is different from the current matrix x.
    if(!identical(y,x)){
      initialize(y)
    }
  }
  get<- function() x
  setInverse<- function(i) I <<-i
  getInverse<- function() I
  
  # Initialize global variable x and its inverse
  initialize(x)
  
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


#' Calculate the inverse and set it to a passed cacheMatrix object.
#'
#' @param cachedX a cacheMatrix object expected here.
#' @param ... any objects you want to pass as parameters of this function.
#' 
#' @return the inverse of cachedX 
#' @export
#'
#' @examples
cacheSolve<-function(cachedX,...){
  # Get previously solved result from the cache matrix, solve and set again 
  # if the result equals NULL otherwise return the result immediately.
  I <- cachedX$getInverse()
  if(is.null(I)){
    I<- solve(cachedX$get())
    cachedX$setInverse(I)
  }
  I
}
