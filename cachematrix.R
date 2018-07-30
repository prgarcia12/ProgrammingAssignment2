## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

#Week 3 Programming Assignment
#Goal of this assignment is to create a special "matrix" object that can cache it's inverse.
#An example was shown describing the process to cache a mean. 

#first - let's define the group of elements that will structure our cache
#We create a list to be able to pull from the function using '$'.

makeCacheMatrix <- function(x = matrix()) {
        ih <- NULL
        #cleanout the Inverse Holder, now define the set function
        set <- function(y) {
                x <<- y
                ih <<- NULL
        }
        #define the get function to return the OG matrix
        get <- function() x
        #assign inverse value in overarching environment
        setinv <- function(solve) ih <<- solve
        #get the value of inverse from call location
        getinv <- function() ih
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
#Now we will create a function to compute the inverse of the special matrix / function from 
#step 1. A sub goal of this function is to check the cache (created above) to see if the 
#inverse has already been calculated - if it has, the function will skip this computation.

#We want the inverse of 'x' - x being a matrix, no matter if it is stored in the cache or must
#be computed indepedently 

cacheSolve <- function(x, ...) {
        ih <- x$getinv()
        #Set the value of our holder to to the updated value of our cached item
        #now, check if the cache is empty. if it's not (!), i.e. the cache is populated, 
        #deliver the cached value to the function
        if(!is.null(ih)) {
                message("getting cached data") #fancy
                return(ih)
        }
        ##If the cache is empty, we need to compute a new inverse of the matrix requested
        #assign 'data' to the OG matrix X, passed in above
        data <- x$get()
        #Use R's built in function 'solve()' to calculate the inverse of the matrix
        ih <- solve(data, ...)
        #Deliver this result into the cache
        x$setinv(ih)
        #show us the inverse computed
        ih
}
