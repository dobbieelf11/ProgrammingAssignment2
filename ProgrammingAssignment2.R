#Creating a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                       #initializing m as NULL to hold value of inverse of matrix
        set <- function(y) {
                x <<- y
                m <<- NULL                              #Incase of new matrix, assign m to NULL
        }
        get <- function() x                             #Returns value of the matrix
        setinverse <- function(inverse) m <<- inverse   #assigns value
        getinverse <- function() m                      #gets the value of m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
#This function computes the inverse of the matrix created above,
#if inverse is already calculated the it retrieves inverse from cache
cacheSolve <- function(x, ...) {
        #Returns an inverse matrix of x
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
