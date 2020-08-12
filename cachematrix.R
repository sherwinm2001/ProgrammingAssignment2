## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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