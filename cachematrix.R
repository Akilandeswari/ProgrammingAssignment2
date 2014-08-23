## Two functions are defined below
## makeCacheMatrix will create a matrix and stores the inverse of the matrix
## computed in cacheSovle function
## cacheSolve function computes the inverse of the matrix using sovle function 

## This function gets the input as matrix. It should be called as
## m1<-makeCacheMatrix(matrix(c(1,2,3,4)2,2)). The values given in the call is just an example


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL  ## initialized with a NULL value
  ## The set function is to set a value to the inv
  set<-function(y){
    x<<-y
    inv<<-NULL
  } 
  get<-function()x ## returns with the original matrix : m1$get()
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The cacheSovle function will return the inverse of the matrix : cacheSovle(m1)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # accesses the object inv
  if(!is.null(inv)) {
    message("getting cached data") # if already computed get the computed inverse
    return(inv)
  }
  data <- x$get() # access the original matrix
  inv <- solve(data, ...) #compute the inverse of the matrix
  x$setinverse(inv) #stores the computed mean in the object x
  inv # returns the mean 
}
