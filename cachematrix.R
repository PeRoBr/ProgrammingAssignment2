## This is a pair of functions that cache the inverse of a matrix

## makeCacheMatrix allows the user to enter a matrix using the set "sub function", 
## verify the content of the matrix with the get "sub function", set the value of an 
## inverse matrix with the set "sub function" and retrieve the value of the inverse
## matrix with the get_solve "sub function"

makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL          
        set <- function(y) {  
                x <<- y
                sol <<- NULL
        }
        get <- function() x
        set_solve <- function(solve) sol <<- solve
        get_solve <- function() sol
        list(set = set, 
             get = get, 
             set_solve = set_solve, 
             get_solve = get_solve)
}


## Takes as input the matrices stored by the functions above. Looks to see if a non NULL
## matrix is stored as the inverse, and returns it if it exists, first giving the   
## message "getting cached data", otherwise it calculates the inverse from the matrix
## set in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        sol <- x$get_solve()
        if(!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        sol <- solve(data, ...)
        x$set_solve(sol)
        sol
}

## this completes the assignment.
