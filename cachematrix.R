## Put comments here that give an overall description of what your
## functions do

## The function defined below creates a special matrix (object) 
## that gets stored in another environment and has functions to set 
## and get inverse of the main matrix

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){
  x<<-y
  inv<<-NULL
}
get<-function() x
setinv<-function(solveMatrix) inv<<-solveMatrix
getinv<-function() inv
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## The following function takes the special matrix created by the above function 
## as the input and calculates its inverse if it was not calculated before. 
## If it is already calculated, then it is retrieved from the environment 
## where it was saved earlier.

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("Getting cached Matrix")
    return(inv)
  }
  dat<-x$get()
  inv<- solve(dat)
  x$setinv(inv)
  inv        ## Return a matrix that is the inverse of 'x'
}