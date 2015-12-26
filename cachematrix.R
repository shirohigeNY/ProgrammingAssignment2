# cachematrix.R
# Programmed by EG for Coursera Programming Assignmet 2, in JHU R Programming course.
# December 2015

## Function makeCacheMatrix and cacheSolve cache the inverse of a matrix  
## using the solve() function in base R. Matrix 'x' is assumed to be
## invertible, there is no programmed check. cacheSolve will check if inverse
## is already cached and will compute locally if Inverse is NULL.

## makeCacheMatrix is of class list. It is a list of functions to set or
## get the inverse of an input matrix 'x'.
## The inverse can also be set or reset to NULL manually.
## The inverse is cached so that it does not have to be recomputed.

makeCacheMatrix <- function(x = matrix()) {      

    Inv <- NULL                                   # Parent environment object.
     
      
    set_matrix <- function(y) {                            # Set matrix 'x' and reset inv 
      message("Setting Matrix, Resetting Inverse to NULL") 
      x <<- y
      Inv <<- NULL
      }
    
    
    get_matrix <- function() {x}
    
    setInv <- function(Inv = NULL) {           # Compute Inverse
      Inv <<- solve(x)
      getInv()
      }
    
    setInvMan<-function(invM=matrix()){        # Manually set Inverse
      Inv<<-invM
      }
    
    
    getInv <- function() {
      message("Inverse of Matrix is")
      Inv
      }
    
    
    resetInv<-function(){                      # Reset Inverse to Null
      message("Resetting Inverse to NULL")
      Inv<<-NULL
      }
    
    
    list(set_matrix = set_matrix,   get_matrix = get_matrix,
        setInv = setInv,   getInv = getInv, resetInv=resetInv, 
        setInvMan = setInvMan)
    
} # end makeCacheMatrix
  
  


## cacheSolve checks if inverse of matrix has been computed. Retrieves if
## inverse is !NULL. Otherwise will compute inverse locally and invoke setInverse
## to cache a value. 
## Input 'a' is in the form of object (list) created by makeCacheMatrix.

cacheSolve <- function(a, ...) {
  
    Inv <- a$getInv()                    # Retrieve Inv
    
    if(!is.null(Inv)) {                  # Use cached Inv if available
      message("getting cached data")
      return(Inv)
      }
    
    
   else{                                 # Compute if Inv not available
     message("computing inverse ...")
     local_matrix <- a$get_matrix()
     Inv <- solve(local_matrix, ...)
     a$setInv(Inv)                       # Cache inverse
     Inv                                 # Return Inverse
     }
    
    
  } # end cacheSolve

      

