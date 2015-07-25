## Name: Gelonia Dent
# Program: Caching the Inverse of a Matrix
#  The two functions makeCacheMatrix() and cacheSolve() take a square matrix and computes its inverse, and stores
# the inverse in cache so that it can be retrieved. This reserves compute time resourse by eliminate the repeated
# calculate of the inverse. 



## #Purpose: This function makeCacheMatrix takes a square matrix as input, 
# computes its inverse and stores the inverse in cache. 

makeCacheMatrix <- function(A = matrix()) { 
    I <- NULL       #initialize the inverse matrix 
    setmatrix <-function(B) {     # set the matrix 
          A <<- B        # store a copy of the matrix       
          I <<- NULL
    }

    getmatrix <-function() A                  # get the value of the matrix
    setinverse <-function(solve) I <<-solve    # Use solve() to compute the matrix inverse
    getinverse <-function() I                 # get the value of the inverse

    list(setmatrix = setmatrix,                
          getmatrix = getmatrix,
          setinverse = setinverse,          # return options either empty, same or inverse
          getinverse = getinverse
    )

}  # end function


## Write a short comment describing this function
# Purpose: The function cacheSolve() takes a square matrix as input, then checks to see if its inverse has already
#been computed and if the matrix is unique (unchanged), then and retrieves the inverse. 

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
    
     I <-X$getinverse()     # get the inverse from cache if it exists
  
    if( !is.null(I)) {
        if(X$setmatrix() == X$getmatrix() )  # Conditional ...check if the matrix has changed 
          return(I)
     }
  
     #if the inverse is not in cache
  
     B <- X$getmatrix()      # load the matrix if it is new
  
    X$setmatrix(B)           # put the input matrix into cache for comparison
    I <-solve(B)             # compute its inverse using solve()
    X$setinverse(I)          # set inverse in cache
    return(I)                # return the inverse
  
 # hopefully this returns the inverse matrix
}
