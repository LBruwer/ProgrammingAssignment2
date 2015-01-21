## Coursera R Programming Assignment2. This function will cache the inverse of
## a supplied matrix.  To run, launch this program in R, then type mekecachematrix
## at the prompt.  Create a matrix, for example x, run solve(x) at the prompt
## example matrix: y <- rbind(c(1,-1),c(2,-1))
## [,1] [,2]
##   1   -1
##   2   -1
## inverse of y 
## [,1] [,2]
##  -1    1
##  -2    1
## to run type at prompt: cachesolve(makecachematrix(y))

makecachematrix <- function(x=matrix()) {  # function makecachematrix
  i <- NULL                                # set i equal to 0
  set <- function(matrix) {
    m<<- matrix                            # <<- different environment from current
    i <<-NULL
  }
  get <-function(){
    m
  }
  setinverse<- function() {
    i<<-inverse
  }
  getinverse<-function() {
    i
  }
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}



## This function calculates the inverse of
## supplied matrix

cachesolve <- function(x,...) {
  m <-x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")  ## display message to screen
    return(m)
  }
  data <- x$get()
  
  m<- solve(data) %*% data
  x$setinverse(m)
  m
}




