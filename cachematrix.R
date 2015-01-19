

## The function receive a matrix as argument, and returns a list of functions
## [set,get,setinverse,getinverse], and save in memory the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ##We initialize the variable m where the inverse is going to be stored
        m <- NULL
        
        ##We define the set function, so everytime its being called
        ## it changes the matrix stored in "x" and set the inverse (m) NULL
        ## because the matrix has changed
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        ##We create the function get, that gives us the matrix
        get <- function() x
        
        ##We create the function setinverse that stores the inverse of the matrix
        ## in the variable "m"
        setinverse <- function(solve) m <<- solve
        
        ##We create the function getinverse that returns the value of "m" that is
        ## the inverse of the matrix
        getinverse <- function() m
        
        ##We create the list of functions that are going to be returned
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The functions checks if inverse if the matrix "x" have been calculated before
## If it had, returns it, and if it not it gets the matrix and calculate the inverse
## and stores it in memory

cacheSolve <- function(x, ...) {
        
        ##We calls getinverse to obtain the inverse if it has been calculated
        m <- x$getinverse()
        
        ##We check if the inverse have been calculated
        if(!is.null(m)){
                ##If the inverse have been calculated we returns it
                message("getting cached data")
                return(m)
        }
        
        ##if the matrix haven't been calculated we get the matrix "x"
        data <- x$get()
        ##We calculate the inverse of the matrix "x"
        m <- solve(data)
        ##We stores the inverse matrix of "x" in the list of functions
        x$setinverse(m)
        ##We return the inverse
        m
}
