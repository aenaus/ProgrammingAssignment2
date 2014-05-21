## "makeCacheMatrix" creates a function that will take x which is assumed to be an invertible matrix and
##will return a list of functions that are able to set and get the matrix and its inverse
##The <<- operator is used to assign a value to a variable in the parent environment
##set nullifies the m which is the inverse matrix
##get returns the matrix
##setinv sets the inverse of the matrix
##getinv returns the inverse of the matrix
##The output is a list containing those functions

makeCacheMatrix <- function ( x  =as.matrix()){
  m <- NULL

  set <- function (y) {  
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function (solved) {
    m <<- solved
  } 
  getinv <- function () m
  list (set = set, 
        get = get, 
        setinv = setinv,
        getinv = getinv)
  
}

##"cacheSolve" creates a function that will take x, a list of functions that is the output of a matrix
##processed with the "makeCacheMatrix" function and will calculate it's inverse with the solve function,
## unless this has already done.Then, the cached result will be returned along with a message.
##First the funtion assigns m the value of the inverse matrix, this is either NULL or something computed
##in a previous invoke of the "cacheSolve" function. If m is not NULL then the cached value is returned.
##If m is NULL it calculates the inverse matrix and assigns this to m with the  "setinv". 
##In the first invoke of "cacheSolve" m is always NULL

cacheSolve <- function( x, ...) {
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m<- solve(data, ...)
  x$setinv(m)
  m
}
