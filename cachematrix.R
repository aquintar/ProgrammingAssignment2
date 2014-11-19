makeCacheMatrix <- function(m = matrix()) { # define 'constructor' with input m
    invm <- NULL                            # Initialize output (inverse of m)
    set <- function(y) {                    # Subfunction to store input in m. Why?
        m <<- y                             # A 'makeCacheMatrix' object can be created in two ways:
        invm <<- NULL                       # i)   > cool_m <- makeCacheMatrix( some_matrix )
    }                                       # ii)  > cool_m <- makeCacheMatrix()
                                            #      > cool_m$set( some_matrix)
    get <- function() m                     # Subfunction to retrieve m 
    
    setinverse <- function(inversem) invm <<- inversem
    getinverse <- function() invm           
                                            # Subfunctions to store/retrieve the inverse of m
                                            
    list(set = set, get = get,              # Create output list of methods
            setinverse = setinverse,
            getinverse = getinverse)

}


                                                                                                                                                                                    

cacheSolve <- function(m, ...) {            # Creates function that returns inverse of
                                            # matrix in m (which is a 'makeCacheMatrix' object)
    invm <- m$getinverse()                  # Try to get inverse of the matrix in m, if it's stored in m *
        if(!is.null(invm)) {                # If succesfull, just return it (with a message)
            message("getting cached data")
                return(invm)
        }
    data <- m$get()                         # Else, get the matrix in m and put it in data
        invm <- solve(data, ...)            # Call solve to find the inverse of m and put it in invm
        m$setinverse(invm)                  # Store it in m, for later, if necessary
        invm                                # Return invm
}

# *: There should be a check of m. If it's not a 'makeCacheMatrix' object, 
#    check if m is a square matrix and if so, create a 'makeCacheMatrix' object and follow on.
#    Else, return a failure message and exit.
