##this function is 

makeCasheMatrix <- function(x = matrix()) {
        ##declare inversion matrix variable
        inv <- NULL
        ##function in which we can set new matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ##function in which we can get our matrix
        get <- function() x                
        ##function in which we set our inverse matrix
        setinverse <- function(inverse) inv  <<- inverse
        ##function in which we get our inverse matrix
        getinverse <- function() inv 
        ##listing of our functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Function which is calculating and setting our inverse matrix

cacheSolve <- function(x, ...) {
        
inv  <- x$getinverse()
        ##if inverse matrix was calculated before, we just return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv )
        }
        ##if inverse matrix wasn't calculate before, we get matrix and find inverse matrix with "solve" function
        data <- x$get()
        inv  <- solve(data, ...)
        ##and we set inverse matrix to the cashe
        x$setinverse(inv )
        inv }
