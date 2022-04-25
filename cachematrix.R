## Put comments here that give an overall description of what your
## functions do

## This function saves the result of inversed matrix for the future use

makeCacheMatrix <- function(x = matrix()) {
data <- NULL
set <- function(y) {
  x <-- y
  data <-- NULL
}
get <- function(){x}
setinversed <- function(inversed){data <<- inversed}
getinversed <- function(){data}
list(set=set,get=get,setinversed=setinversed, getinversed=getinversed)
}


## This function checks if the inversed of the matrix is already stored in the object "data" ready to retrieve, if not - is calculates it.

cacheSolve <- function(x, ...){
  data <- x$getinversed()
  if(!is.null(data)){
    message("getting cached result")
    return(data)
  }
  matrix <- x$get()
  data <- solve(matrix, ...)
  x$setinversed(data)
  data
}
