#makeCacheMatrix returns a list of functions that operate on the input 'matrix',
#cacheSolve tries to get the cached value of inverse if it exits, computes/set cache if not

makeCacheMatrix <- function(x = matrix()) { 
  #x is the variable containing matrix
  
  inverse = NULL #initilize inverse to null
 
  set <- function(x2) { #set thy matrix
    x<<-x2 #use double headed arrow to refer to parent scope's 'x' 
  }
  
  get <- function() x #get thy matrix
  
  setinv <- function(inv) {
    inverse <<- inv 
  }
  
  getinv <- function() inverse
 
  
  
  #return list of functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## returns inverse of matrix x, this inverse is fetched from cache
## if it exists, else it's computed and stored in cache for future calls to use
cacheSolve <- function(x, ...) {
  inv = x$getinv()
  ##step 1 . check if it is in cache
  if(!is.null(inv)) {
    message("got inv from cache")  
    return(inv)
  }
  message("didn't find inv in cache, computing now")
  #y is a intermediate var
  y.data = x$get() 
  new_inverse = solve(y.data, ...)
  x$setinv(new_inverse)
  
  return(new_inverse)
  ## Return a matrix that is the inverse of 'x'
}
