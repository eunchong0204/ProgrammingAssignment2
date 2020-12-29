# The makeCacheMatrix function takes a matrix as an argument.
# The cacheSolve function takes a makeCacheMatrix object as an argument,
# takes its matrix, and calculate the inverse of it. 
# Finally, it stores that value in the m symbol in the makeCacheMatrix function.
# When some makeCacheMatrix object is called within the cacheSolve function again, 
# the cacheSolve function just returns the value of m (cached data) in the environment of the makeCacheMatrix object.


# makeCacheMatrix function has 6 objects in its environment,
# which are x (argument), m, set, get, setinverse, and getinverse.
# x is the symbol which stores data(matrix)
# and m stores the inverse of a matrix or the solution of a matrix equation.
# set, get, setinver, and getinverse are functions.
# Especially, the setinverse function changes the value of m in the parent environment.

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
             setinverse = setinverse,
             getinverse = getinverse)
}


# solve(a, b, ...) function solves a %*% x = b for x.
# b : numeric or complex vector or matrix giving the right-hand side(s) of the linear system. 
# If missing, b is taken to be an identity matrix and solve will return the inverse of a
# Therefore, ... can be used for inputting b and the function returns the solution x
# For example,
# x <- makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(x)
# cacheSolve(x, matrix(1:4, 2, 2))


cacheSolve <- function(x, ...) {
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
