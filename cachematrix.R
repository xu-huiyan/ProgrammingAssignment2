## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Huiyan Xu, 2017 Nov.

makeCacheMatrix <- function(x = matrix()) {
  #set inverse matrix to NULL
  inverse <- NULL
  
  #set up
  set <- function(y) {
    x <<-y
    inverse <<- NULL
  }
  
  #get matrix itself
  get <- function() x
  
  #set matrix inverse
  setInverse <- function(i) inverse<<-i
  
  #return matrix inverse
  getInverse <- function() inverse
  
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #try getting matrix inverse
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) {
    #found inverse
    message ("getting cached data")
    return(inverse)
  }
  
  #failed to find inverse
  data <- x$get()
  inverse <- solve(data)
  #assign inverse matrix
  x$setInverse(inverse)
  inverse
}
