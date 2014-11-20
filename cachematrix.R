## Write a pair of functions that cache the inverse of a matrix.
# The first function, makeCacheMatrix creates a special "Matrix", which is really 
# a list containing a function to

#1. set the Matrix
#2. get the Matrix
#3. set the Inverse of this Matrix
#4. get the Inverse of this Matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Inverse of a Matrix property
  
  Inv <-NULL  ## Inv will be our 'Inverse' and it's reset to NULL every
  ## time makeCacheMatrix is called.
  
  ## Set the Matrix
  set <- function(n) {
    x<<-n
    Inv<<-NULL
  }
  ## get the Matrix
  
  get<-function(){ x 
                   ## return the matrix.
  }
  ## set the inverse of the matrix
  setInverse <-function(inverse) {
    
    Inv<<-inverse
  }
  ## get the inverse of the matrix
  getInverse <- function() {
    ## return the inverse property
    Inv
  }
  ## return the list of the process
  
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}
# The following function calculates the inverse of the matrix created 
# with the above function. However, it first checks to see if the inverse has already 
# been calculated. If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse 
# in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## the input is an object created by makeVector
  ## return a matrix that inverse of 'x'
  
  Inv <-x$getInverse()
  ## accesses the object 'x' and gets the value of the Inverse.
  
  if(!is.null(Inv)){ 
    ## if the Inv was already cached (not NULL) ...
    
    message("getting cached data")
    return(Inv)
  }
  matrix <- x$get() 
  Inv<-solve(matrix) 
  x$setInverse(Inv)
  Inv
}
## makeCacheMatrix run on R

## > x = rbind(c(1, -1/3), c(-1/3, 1))
## > m = makeCacheMatrix(x)
## > m$get()

##[,1]       [,2]
##[1,]  1.0000000 -0.3333333
##[2,] -0.3333333  1.0000000



## CacheSolve run on R
## x=rbind(c(1, -1/3), c(-1/3, 1))
##Inv = makeCacheMatrix(x)
##Inv $get()
##cacheSolve(Inv)

##[,1]  [,2]
##[1,] 1.125 0.375
##[2,] 0.375 1.125