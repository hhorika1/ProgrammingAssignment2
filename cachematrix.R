## Put comments here that give an overall description of what your
## functions do
## The makeCacheMatrix function below will get the inverse of a matrix 
## and cache the inverse matrix.
## The cacheSolve function will compute the inverse of the special "matrix"
## returned by the makeCacheMatrix 

## Write a short comment describing this function
## This function was created based on the makeVector function provided in
## the assignment page
## Instead of getting the mean, the function below is getting the inverse as
## defined in the setinverse, getinverse, and m <<- inverse part

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )

}


## Write a short comment describing this function
## the function is modelled from the cachemean funtion defined in the
## assignment page
## the differense is that the variable m in the function below is using the
## solve function instead of the mean function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
