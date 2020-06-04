## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#create function as example
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL #cached data
  set <- function(y) { #setter
    x <<- y 
    s <<- NULL 
  }
  get <- function() x  #getter
  #acessors to solution
  setsolve <- function(solved) s <<- solved 
  getsolve <- function() s
  #return psevdo-object
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}



#transpond matrix
solve <-function(x){
     res <-matrix(nrow=ncol(x),ncol=nrow(x))
     for(i in 1:nrow(x)){
       res[,i]<-x[i,] #x's row is res's column
     }
     res
}

## Write a short comment describing this function
#do as in example for vectors
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve() #try to get cache
  if(!is.null(s)) { #if have cache
    message("getting cached data")
    return(s)  #return it
  }
  #else find solution and cache it
  data <- x$get() 
  s <- solve(data, ...)
  x$setsolve(s)
  s #return solution
}
