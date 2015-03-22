#====================================================================================
# cachematrix.R
#------------------------------------------------------------------------------------
# This file contains 2 main functions, makeCacheMatrix and cacheSolve. The former
# creates a special variable that contains a list of functions and a variable for
# inverseMatrix and the data associated with matrix that was sent as a parameter
# while creating this object
# The latter is a function that gets optimises the call for inversing the matrix
# If the matrix is already inversed earlier, it gets it from the cache and does away
# from calling the solve function repeatedly
#====================================================================================

#====================================================================================
# function name  :       makeCacheMatrix(matrix as input variable)
#------------------------------------------------------------------------------------
# This function creates a special object that takes a matrix as input and creates
# a list of 3 functions and has an additional variable that contains the inversed
# matrix
#====================================================================================

makeCacheMatrix <- function(normalMatrix = matrix()){
        
        # initialise the variable that should contain the inversed matrix to NULL
        inversedMatrix <- NULL
        
        # create a new function called getData that will return the matrix that was
        # sent as an input while the object got created
        getData <- function() normalMatrix

        # create a new function that will store the inversed matrix sent as input to 
        # the inverse matrix variable created earlier
        setInverseMatrix <- function (sentInversedMatrix)
                inversedMatrix <<- sentInversedMatrix
        
        # create a new function that will return the inverse matrix value
        getInverseMatrix <- function () inversedMatrix
        
        # create a list of 3 function that will be returned as part of this function
        # for making these function avaialbe as part of the object for accessing the
        # data and inverse matrix
        list (getData = getData,
              setInverseMatrix = setInverseMatrix,
              getInverseMatrix = getInverseMatrix)
}


#====================================================================================
# function name  :       cacheSolve(special Matirx created using makeCacheMatrix,...)
#                        the ... variable means multiple special matrices can be sent
#------------------------------------------------------------------------------------
# This function can be called instead of normal solve function as this function 
# ensures that if the inverse matrix is required for the same set of data, it gets it
# from the cache as it has already called the solve function earlier...
#====================================================================================

cacheSolve <- function(specialMatrix, ...) {
        
        # get the value of inversed matrix
        inversedMatrix <- specialMatrix$getInverseMatrix()
        
        # check if the value is NULL. if it is not, then we have already found the
        # inverse of the data matrix earlier, hence return the inverse from the cache
        # which was stored earlier
        if(!is.null(inversedMatrix)) {
                message("got from cache")
                return(inversedMatrix)
        }
        
        # if we have reached here, then this is the first time this function is called
        # and we have not found the inverse of the data matrix yet
        
        # get the data matrix sent as part of the input to this object
        data <- specialMatrix$getData()
        
        # get the inverse of the data matrices
        inversedMatrix <- solve(data)
        
        #set the inversed matrix value into the cached variable
        specialMatrix$setInverseMatrix(inversedMatrix)
        
        #return the value of inversed matrix to the caller
        inversedMatrix
}