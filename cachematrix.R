## To cache an object and make script more efficient it is neccessary to first store it in a
## special vector (actually a list of functions) to access the building blocks of the object.in question
## Then we use a second function to take elements (functions) from the "special vector" and
## either reproduce the cached mean or calculate a new one if the initial object has changed

## makeCacheMatrix here takes a matrix as an argument and creates an object inv which will
## eventually be assigned to the inverse of the matrix, initially this is NULL (until we 
## calculate an inverse for it). Four functions are then concatenated into a list. 

## Firstly the set function is created where y is an argument subsequently defined within this
## function's environment as x (allowing the input matrix to be changed or set again later)
## upon using this function the value of inv within this function becomes NULL (if already
## defined)

## Secondly the get function is created where a new value for x can be assigned

## Thirdly the setinv function is created where the solve function is an argument and also
## assigned now to the inv object. This means when called upon, this function will produce
## the required matrix inversion and assign it to our inv object

## The final function produces the newly calculated inv object

## These four functions are now concatenated into a list, the "special vector"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function () inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Here we access the functions created in the makeCacheMatrix function to either retrieve
## the derived matrix inverse or calculate it from scratch. This is
## achieved by passing the "special vector" as an argument to cacheSolve and checking whether
## the inv object is still a NULL value, if not the script will break here and return the
## value of inv (alongside a handy message). Otherwise the matrix will be retrieved from the
## "special vector" function list, inverted using the solve function and set as the new value
## of inv

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
