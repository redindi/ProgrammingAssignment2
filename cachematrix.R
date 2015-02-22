## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # m will store the cached inverse matrix
        m <- NULL
        # Setter for the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # Getter for the matrix
        get <- function() x
       
        # Setting the inverse
        setinverse <- function(solve) m <<- solve
        
        # Getting the inverse
        getinverse <- function() m
        
        # Return the matrix with our newly defined functions
        list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}
        
        
# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        # If the inverse is already calculated, return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # We calculate the inverse
        data <- x$get()
        m <- solve (data, ...)
        
        # Cache the inverse
        x$setinverse(m)
       
        # Return it
        m
}
        ## Return a matrix that is the inverse of 'x'
}
