## I use a 2 function: The first one cache the matrix datas (notably the inverse of matrix)
## The second function calculate the inverse of the matrix only if not previously calculated 

## Make cache matrix is a vector of 4 functions that set the value of the matrix,
## get this value, set the value on inverse of the matrix, and finally get this value

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve first look if makeCacheMatrix have already a data for 'inverse of x'
##If this data exist it is returned with comment 'getting cacheed data'
## Otherwise, it compute 'inverse of x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
