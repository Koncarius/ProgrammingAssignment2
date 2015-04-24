# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

## makeCacheMatrix creates a list of functions and returns them
## to the function cacheSolve that gets the inverted matrix from 
## the cache

makeCacheMatrix <- function(x = numeric()) {
    
    # i stores cached values and is initially set to NULL
    # set creates the matrix
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # The function get, gets the values of the matrix
    get <- function() x
    # getinv and setinv sets the inverted matrix in the cache 
    # and gets the inverted matrix from the cache memory
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    
    # Returns the functions to the working environment
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve gives the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix doesn't exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache

cacheSolve <- function(x, ...) {
    
    ## Get the inverse of the matrix stored in the cache
    i <- x$getinv()
    
    # Return inverted matrix from cache if it's there
    # otherwise create the matrix in working environment
    if(!is.null(i)) {
        message("getting cached data")
        
        # Shows the matrix in the console
        return(i)
    }
    
    # Gets values from the matrix and solves them,
    # and prints the result
    data <- x$get()
    i <- solve(data, ...)
    x$set(i)
    i
}

# Try out program:

# > x <- matrix(rnorm(25), nrow = 5, ncol = 5)  // Creates a matrix
# > fx <- makeCacheMatrix(x)                    // Values are put into the function
# > fx$get()                                    // Gets the matrix                                  
# > cacheSolve(fx)                              // Solves the function
