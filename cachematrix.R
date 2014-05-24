## The following functions cache the inverse of a matrix and return it whenever needed.

## Creates a list of functions for setting/getting a given matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) #matrix name as parameter
{
        i <- NULL #Default for the inverse of the matrix
        set <- function(y) 
        {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inv) i <<- inv
        getInv <- function() i
        list(set = set, get = get, #final list of functions
             setInv = setInv,
             getInv = getInv)
}


## Returns the inverse of a given matrix. First looks up the inverse in the cache. 
## If none exists, the inverse is calculated. 
cacheSolve <- function(x, ...) {
        i <- x$getInv()    #Gets the inverse. NULL if not in cache.
        if(!is.null(i))    #Outputs the inverse if found in cache
        {
                message("getting cached data")
                return(i)
        }
        #calculate and return inverse ...
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
        i
}
