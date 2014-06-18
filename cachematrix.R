## Create two functions: one to cache the value of the inverse of a square matrix and the other to check 
## if this value is already computed and if so to retrieve it from the cache so we don't need to recompute it,
##  if not to caluculate it.

## This first function computes the inverse of a square matrix x and caches it
    
makeCacheMatrix <- function(x = matrix()) {
iv<-NULL
set<-function(y){
  x<<-y
  iv<<-NULL
}

get<-function() x

setinv<-function(solve) iv<<-solve
getinv<-function() iv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  iv <- x$getinv()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setinv(iv)
  iv
}
