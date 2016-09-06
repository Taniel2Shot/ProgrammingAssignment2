# Coursera R Programming Assignment 2 from Week 3
# Dale Richardson
# September 6, 2016

## The following two functions will allow for the solving/caching/retrieving of a matrix's inverse, assuming that the matrix is square
## and invertible. 

## This function will create the special "matrix" object, which will return a list of functions and variables that can be used by
## cacheSolve

makeCacheMatrix <- function(x = matrix() ) { # the input to this function is a pre-created matrix, x
        inv <- NULL # declare the inverse to be NULL
        set <- function(y) { # should we want to change the input matrix from x to a new matrix, y, we can do it with this function
                x <<- matrix(y) # set x in the parent environment above to be y
                inv <<- NULL # do the same for the inverse variable
        }
        get <- function() x # get the matrix
        setinv <- function(inverse) inv <<- inverse # store the inverse value in the cache
        getinv <- function() inv # retrieve the inverse from the cache
        list(set = set, get = get, # return a list of the functions defined here that can be called using the $ operator in cacheSolve below
             setinv = setinv,
             getinv = getinv)

}


## This function will take the object created by makeCacheMatrix and check if there is already
## an inverse solved for the matrix. If not, it will solve the inverse of the matrix.

cacheSolve <- function(x, ...) { # x is the object created by makeCacheMatrix

        inv <- x$getinv() # get the inverse
        if(!is.null(inv)) { # if inv isn't NULL, return the cached inverse data
                message("getting cached data")
                return(inv)
        }
        data <- x$get() # get the matrix and store it in the data variable
        message("inverse not cached! calculating the inverse")
        inv <- solve(data, ...) # solve the inverse
        x$setinv(inv) #set the inverse
        
        ## Return a matrix that is the inverse of 'x'
        inv
}

## Test the functions
## Found some nice test code from Alan E. Berger in this week's forum: http://tinyurl.com/hekellw

# a simple 2 by 2 invertible matrix with an even simpler looking inverse
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

myMatrix_object <- makeCacheMatrix(m1)

cacheSolve(myMatrix_object)

# returns (and caches) the matrix inverse of m1

# which is equal to

matrix(c(6,2, 8,4), nrow = 2, ncol = 2)

#     [,1] [,2]
#[1,]    6    8
#[2,]    2    4

# check 
m1 %*% matrix(c(6,2, 8,4), nrow = 2, ncol = 2)

# gives the 2 by 2 identity matrix matrix(c(1,0, 0,1), nrow = 2, ncol = 2)
#      [,1] [,2]
#[1,]    1    0
#[2,]    0    1

# and running cacheSolve again should simply bring up the cached inverse
 
cacheSolve(myMatrix_object)
