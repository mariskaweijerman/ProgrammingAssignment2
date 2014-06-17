## Create two functions: one to cache the value of the inverse of a square matrix and the other to check 
## if this value is already computed and if so to retrieve it from the cache so we don't need to recompute it,
##  if not to caluculate it.

## This first function computes the inverse of a square matrix x and caches it
    
makeCacheMatrix <- function(x = matrix()) {
  m<-matrix()
  set<-function(y){
    x<<-y
    m<<-matrix()
  }
  
  get<-function() x
  setinv<-function(solve) m<<-solve
  getinv<-function()m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
     
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
