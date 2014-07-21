## Caching and keeping a previously computer Inverse of A Matrix

## makeCacheMatrix creates a matrix,also there are fucntions  get ,set,setinv(to set inverse)
## getinv(to get inverse)

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  
  list(set = set,get = get,setinv = setinv, getinv = getinv)
  
}


## cacheSolve takes the matrix and produces a Inverse of the Matrix
## I am using TryCatch to Handle The Error is a controlled fashion

cacheSolve <- function(x, ...) {
  
  out <- tryCatch(
    
{
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<-solve(data);
  
  x$setinv(m)
  print(m)
},

error=function(cond) {
  print("Error :YOur Matrix has no Inverse MAtrix ,Please Enter a DIfferent Matrix");
  
}
  )
}
