## The point of these functions is to be able to have in cache memory the result of a complicated and time-consuming computation
## such as the Inverse of a matrix, so that you can access it whenever you need it simply by looking it up in cache.
## These functions are makeCacheMatrix and cacheSolve. When the first function(makeCacheMatrix) is assigned to a variable
## the variable becomes an instantiation of a closure that includes the functions set,get,getInverse,setInverse.In other
## words it creates an environment where m (used to hold the result of the computation) and x(the original matrix) are stored
## You need this instantiation in order to call the cacheSolve function.
## Here is where the actual storing to memory takes place. This function accesses the environment
## where the variable m exists and this variable is either NULL (as initiated in makeCacheMatrix)
## or it has a non-null value (the inverse of the matrix we want to compute) if it has already been called once.
## 

## ABOUT makeCacheMatrix:
## This function is given a matrix x as an argument and starts by initializing a variable m to be NULL.
## Then (lines 18-24) it defines 4 functions associated with any variable that is assigned with makeCacheMatrix.
## The set sub-function is used in case you want to set the matrix x to a new matrix y and resets the m variable to be NULL
## so that there aren't any previous values left to it.
## The get sub-function simply returns the matrix x.
## The setInverse sub-function is used to set the result of the solve function to m (the calculation actually happens 
## later on when the cacheSolve function calls the solve() function)
## The getInverse sub-function simply returns the variable m
## all the above functions are returned in form of a list
## The <<- means that we are making an assignment in a non-local environment so that you can actually access it outside 
## the function


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInverse<-function(solve)m<<-solve
  getInverse<-function() m
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## ABOUT cacheSolve:
## The point of this function is when it is called with an argument x (x is actually an object type makeCacheMatrix)
## to access the m variable that holds the inverse of the matrix if it has been calculated before.
## The statement x$getInverse accesses the value of m from the non-local environment where it has been created.
## if this is  the first time the function cacheSolve is called then the value of is null and therefore
## the piece of code lines 49-52 is not executed and lines 53-57 are executed. In these lines the get function returns
## the value of the matrix whose inverse we want to compute and assigns it to the local variable data.
## Then the solve function computes the inverse of the matrix and assigns it to m. Line 57 is where the actual
## storing of the inverse happens.
## The messages are put in the code to indicated which part of the code is executed every time the cacheSolve is called. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setInverse(m)
  message("calculating inverse matrix")
  m
}

