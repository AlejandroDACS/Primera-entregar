#------------ Primera funcion --------------------------
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inversa_matriz) inv <<- inversa_matriz 
  getInverse <- function() inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
#------------ Segunda funcion --------------------------
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  dato <- x$get()
  inv <- solve(dato)
  x$setInverse(inv)
  inv
}
