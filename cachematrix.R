## Functions create matrix and set functions to compute its' inverse and cache it
## Functions check if inverse is already available and use it to speed up operations

## makeCacheMatrix function creates matrix, sets variables and functions
## to commit values to cache and retrieve it,
## returnes list of functions so they are available in other environments

makeCacheMatrix <- function(x = matrix()) {

    inv_m <- NULL

    set <- function(y) {
    
        x <<- y
        
        inv_m <<- NULL

    }

    get <- function() x

    set_inv <- function(inverse) inv_m <<- inverse 

    get_inv <- function() inv_m

    list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
  
}

## cacheSolve function checks if the inverse has already been computed
## and in this case retrieves value stored in cache skipping calculation,
## if not computes new inverse and stores it in memory, using functions
## created under makeCacheMatrix function

cacheSolve <- function(x, ...) {

    inv <- x$get_inv()
  
    if (!is.null(inv_m)){

    return(inv_m)
    
    }
  
    mat_dat <- x$get()

    inv_m <- solve(mat_dat, ...)
    
    x$set_inv(inv_m)
  
    return(inv_m)
  
}