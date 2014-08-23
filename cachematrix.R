## A pair of functions that can cache the inverse of a matrix so that
## repeated computing to find the inverse of the same matrix can be
## avoided


## create an object that can cache a matrix and its inverse
makeCacheMatrix <- function() {

  mat <- matrix()
  inv <- matrix()
  
  setMat <- function(x){
    mat <<- x
    inv <<- matrix()
  }
  
  getMat <- function() mat
  
  setInv <- function(x){
    inv <<- x
  }
  
  getInv <- function() inv
  
  list(setMat=setMat, getMat=getMat, setInv=setInv, getInv=getInv)
}


# run the function to create the object
m<-makeCacheMatrix()


## give the inverse of a matrix from the cache or from calculation
cacheSolve <- function(x = matrix()) {
  
  y <- m$getMat()
  
  ## check if matrix in cache is same as the argument
  if(dim(x)==dim(y) && all(x==y)){
    
    message("getting cached data")
    m$getInv()
    
  } else {
    
    # cache the matrix for checking next time
    m$setMat(x)
    
    # find inverse and cache it also
    z <- solve(x)
    m$setInv(z)
    
    # return the inverse
    return(z)
  }
  
}



# check if everything working fine and we get the inverse
x <- matrix(1:4, 2)
cacheSolve(x)

