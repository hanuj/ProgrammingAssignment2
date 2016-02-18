## These functions allow the user to cache the inverse of a matrix for future use rather than recalculating it every time 
## The function first converts the input into a matrix m
## The user can then simply first cacheinverse(m) of the matrix m and then later on m$getinverse() to retrieve it.  

### FUNCTION TO MAKE THE INPUT INTO A MATRIX m ##
rm(list=ls(all=TRUE)) # CLEAR YOUR CURRENT R SESSION
makeMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

###CACHE FUNCTION####
cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#### T*E*S*T ####
input<-matrix(c(1:4),nrow=2,ncol=2,byrow=T)
m <- makeMatrix(input)
m$get()
m$getinverse()
cacheinverse(m)
m$getinverse()
cacheinverse(m)
m$getinverse()
x <- cacheinverse(m)
x
m$set(matrix(c(4:7),nrow=2,ncol=2))
m$get()  
cacheinverse(m) 

## THANK YOU & HAVE A GOOD DAY! ##
