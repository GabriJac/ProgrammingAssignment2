## makeCacheMatrix and cacheSolve are 2 functions useful for the optimization of the operation of matrix inversion. Infact, using this code,
##it's not necessary to repeatedly compute the inversion. Once the calculation has been computed, the result is cached and it can be uncached
#afterwards, when needed.

## makeCacheMatrix is a function that creates a matrix and caches it in another environment


makeCacheMatrix<-function(x=matrix()) {
  inverse<-NULL
##the set function caches the matrix passed as parameter
  set<-function(y) {
    inverse<<-NULL
    x<<-y
  }
##the get function returns the matrix passed as parameter
  get<-function() x
##the setInverse function allows to set the inverse matrix
  setInverse<- function(solve) inverse<<-solve
##the getInverse function is the getter method for the inverse
  getInverse<-function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



#cacheSolve is a method for calculate the Inverse. If the inverse had already been calculated
#the function uncaches it, otherwise it calculates the inverse matrix and caches it.

cacheSolve<-function(m,...) {
  inverse<-m$getInverse()
  if(!is.null(inverse)){
  message("getting cached data")
  return(inverse) 
  }
  #calculation of the inverse matrix
  matrix<-m$get()
  inverse<-solve(matrix)
  m$setInverse(inverse)
  inverse
}
