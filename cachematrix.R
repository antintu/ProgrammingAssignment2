## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## get/set for matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        ## setinverse/getinverse
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        ## display the set, get, setinverse, getinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## return computed cache matrix
        inv <- x$getinverse()
        
        ## show message if cache is retreaved
        if(!is.null(inv)) {
                message("Please wait, getting cached data!")
                return(inv)
        }
        data <- x$get()
        
        ## compute inversion
        inv <- solve(data, ...)
       
        ## cach inverse
        x$setinverse(inv)
       
        ## Return a matrix that is the inverse of 'x'
        inv
}


## Tests and tryouts
## inversing a matrix --> use solve() function
## x <- matrix(c(2,3,4,5,6,5,4,9,9), 3,3)
## x
##[,1] [,2] [,3]
##[1,]    2    5    4
##[2,]    3    6    9
##[3,]    4    5    9
## solve(x)
##[,1]        [,2]       [,3]
##[1,]  0.3333333 -0.92592593  0.7777778
##[2,]  0.3333333  0.07407407 -0.2222222
##[3,] -0.3333333  0.37037037 -0.1111111

## tested it in a function too
## makeCacheMatrix <- function(x = matrix()) {
##        inv_matrix <- solve(x)
##        return(inv_matrix)
## }
