## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
  
#Creating a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                       #initializing m as NULL to hold value of inverse of matrix
        set <- function(y) {
                x <<- y
                m <<- NULL                              #Incase of new matrix, assign m to NULL
        }
        get <- function() x                             #Returns value of the matrix
        setinverse <- function(inverse) m <<- inverse   #assigns value of m in parent enviornment
        getinverse <- function() m                      #gets the value of m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

#This function computes the inverse of the matrix created above,
#if inverse is already calculated the it retrieves inverse from cache
cacheSolve <- function(x, ...) {
        #Returns an inverse matrix of x
        m <- x$getinverse()
        if(!is.null(m)) {                               #If value is already calculated then it returns the cached value
                message("getting cached data")
                return(m)
        }
        #Otherwise it continues to solve the inverse of the matrix
        data <- x$get()                                 #matrix is stored in data
        m <- solve(data, ...)                           #Inverse is calculated here and assigned to m
        x$setinverse(m)
        m
}
