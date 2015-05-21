## makeCacheMatrix creates lexical function closure with the environment
## It is used to calculate and cache inverse of matrix

## makeCacheMatrix creates a list of 4 function binded to the same environment
## The functions are set(matrix), get(), get.solve() and set.solve(matrix)
## get.solve() calculates matrix inverse only if cache is empty otherwise
## it returns the unverse
## set.solve() caches the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set.solve <- function(solve) m <<- solve
        get.solve <- function() m
        list(set = set, get = get,
             set.solve = set.solve,
             get.solve = get.solve)
}


## cacheSolve() takes "special" matrix from makeCacheMatrix and calculates 
## it's inverse. Then it caches the result back into "special" matrix environment
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get.solve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set.solve(m)
        m
}