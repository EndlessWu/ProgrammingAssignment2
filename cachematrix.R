## makeCacheMatrix function stores four functions and based on that
## cacheSolve function either calculates or simplifies the calculation process of matrix inverse

## makeCacheMatrix function creates a special matrix and stores four functions set, get, setinverse, getinverse
## to simplify the process of calculating matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- null
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get=get,
             setinverse = setinverse
             getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the special "matrix" created with the above function

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
