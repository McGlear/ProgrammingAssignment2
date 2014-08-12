##############
## Overview ##
##############

## The given set of functions can be used to 
    ## a) create a "matrix"-object

    ## b) calculate and return the inverse of the created matrix and cache that
    ## inverted matrix, so that on later function calls the time-consuming
    ## calculations do not need to be repeated


#######################################
## Creating and accessing the matrix ##
#######################################

## Use makeCacheMatrix to define your matrix, giving a 
## matrix object as the functions argument.

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL         # Can later store the inverted matrix
    set <- function(mat){   # "set" allows changing the matrix later on.
        x<<-mat
        inverse <<- NULL
    }
    
    get <- function() x     # "get" retrieves the matrix object
    
    setinverse <- function(i) inverse <<- i     # Do not access this directly.
                                                # Use cacheSolve()!
    
    getinverse <- function() inverse            # Retrieves the inverted matrix. 
    
    list(set = set,                             # Return the accessible list
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}

## The function returns a list that allows you 
 ## 1. to retrieve the matrix (_yourvariablename$get), 
 ## 2. change the matrix (_yourvariablename$set(yournewmatrix))
 ## 3. get the inverse (_yourvariablename$getinverse). 
## It includes a setinverse function, that you should not access directly. 
## Use the cachSolve() function instead (see below)!


###########################
## Computing the inverse ##
###########################

## The function cacheSolve computes the inverse of a given "matrix"-object created
## with makeCacheMatrix, but only if that inverse has not yet been computed or the
## matrix has changed since the last function call. Otherwise, it returns the 
## cached value.

cacheSolve <- function(x) {         # Pass your "matrix"-object (list)
    inverse <- x$getinverse()       # Store the cache in the local environment
    if(!is.null(inverse)){          # If a cached value exist, return it!
        message("using cached value")
        return(inverse)
    }
    
    data <-x$get()                  # If not: compute a new one...
    inverse <- solve(data)
    x$setinverse(inverse)           # ... and pass it via setinverse()
    
    return(inverse)                 # Return the new inverted matrix
}
