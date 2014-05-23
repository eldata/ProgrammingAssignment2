## Put comments here that give an overall description of what your    ##########
## functions do:  [See Below:]                                 #################


## Write a short comment describing this function  #############################

###  1) makeCacheMatrix: return a list of functions to:
        # 1. Set the value of the matrix
        # 2. Get the value of the matrix
        # 3. Set the value of the inverse
        # 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        # Use inv to store the cached inverse matrix
        inv <- NULL
        # Setter for the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Getter for the matrix
        get <- function() x
        # Setter for the inverse
        setinv <- function(inverse) inv <<- inverse
        # Getter for the inverse
        getinv <- function() inv
        # Return the matrix with the new defined functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function  #############################

###  2) cacheSolve: Compute the inverse of the matrix. If the inverse is already
#       calculated prior, it returns the cached inverse.
###  Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        # If the inverse is already calculated, then return it:
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # The inverse is not yet calculated, so we calculate it
        data <- x$get()
        inv <- solve(data, ...)
        # Cache the inverse
        x$setinv(inv)
        # Return it
        inv          
}

