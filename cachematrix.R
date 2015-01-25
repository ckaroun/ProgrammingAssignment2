## The first function is used for creating an environment to update and create a given matrix 
## as well as calculate its inverse with the makeCacheMatrix. Rather than repeating the time 
## costly operation of calculating an inverse, the cacheSolve function takes the input 
## of the list of functions created by makeCacheMatrix and calculates the inverse a matrix if 
## it hasn't already been cached in makeCacheMatrix. 

# This is a function that creates a list of functions for storing and updating a 
#  matrix passed as the argument and its inverse. 

makeCacheMatrix <- function (x = matrix()) {
  
  solution <- NULL # When first creating the list for storing, updating the matrix the solution is assumed to be uncalculated
  
  setMatrix<- function(y){
    #function setMatrix assigns its argument to be the matrix in the surrounding environment 
    #which is the function makeCacheMatrix
    
    x<<-y   #super assign the argument for setMatrix to be the matrix of makeCacheMatrix
    
    solution<<- NULL  # if solution is being set again then the surrounding environment's ( makeCacheMatrix) variable, "solution" is reset to null
  }
  
  getMatrix<-function(){
    # function getMatrix simply returns the matrix in the surrounding environment,
    # "makeCacheMatrix" since x is not defined in local environment
    
    x
  }
  
  setSolution<- function(y){ 
    # function serSolution super assigns its argument to be the new solution for 
    # the surrounding environment, "makeCacheMatrix"
    
    solution<<-y
  }  
  
  getSolution<-function(){
    # function getSolution simply returns the inverse matrix, "solution" in the
    # surrounding environment, "makeCacheMatrix" since x is not defined in local environment
    
    solution
  }
  
  #for output of the makeCacheMatrix a list is generated with each of the 4 functions defined above
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setSolution = setSolution,
       getSolution = getSolution)
  
}


# a function for testing if a cached version of the inverse of the a matrix is available. 
# The input, x, is list of functions for creating and updating a matrix and its inverse 
# which also contains at least the data of the matrix and possibly(if cached) the value of its matrix. 
# If the cached inverse matrix is available it prints a message and returns he cached version ending the function.
# If it is not it will skip over the return statement and calculate the inverse of the matrix which is accessed through 
# the input list of functions and  then set with setSoultion function within the input list.  

cacheSolve <- function (x, ...) {
  
  # solution assigned by list of functions, x
  solution<-x$getSolution()
  
  if(!is.null(solution)){  #tests if the solution is not a null or in other words has a value assigned so it can return it if it already has a value 
    
    message("getting cache for solution")  # prints to console a message to alert the user that it is using the cahced inverse.
    
    return(solution)  # returns the already cal
  }
  # execution  will only reach here if the solution is NULL and therefore should be calculate for the first time 
  
  data <- x$getMatrix() # brings the matrix associated with x to the local environment with x's getmatrix function
  
  solution <- solve(data) # finds the inverse of the matrix X 
  
  x$setSolution(solution) # uses X's set solution function to super assign the solution found above to the environment (which is in the input list X)
  
  solution # returns the solution for cacheSolve since it is the last expression
  
}