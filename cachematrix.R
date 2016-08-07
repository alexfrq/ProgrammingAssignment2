## Put comments here that give an overall description of what your
## functions do

# function 1: makeCacheMatrix
# argument list: a matrix
# return value: a list of functions including set/get/setInvert/getInvert
# other impact: assign global variables x as matrix and invertMatrix as x's inverse
# sub-functions:
    # set: assign global variables x as matrix
    # get: return global variable x
    # setInvert: assign global variables invertMatrix
    # getInvert: return global variables invertMatrix

# function 2: cacheSolve
# argument list: a list of function
# return value: invert of the matrix
# other impact: it will check if global variables invertMatrix has been set
#     if yes, call the getInvert function from list to get global variable invertMatrix
#     if no, call the get function from list to get the matrix from global variable x, use solve to get the inverse of x, assign that to 
#         global variable invertMatrix
        

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invertMatrix <- NULL
    set <- function(y) {
        x <<- y
        invertMatrix <<- NULL
    }
    get <- function() x
    setInvert <- function(invert) invertMatrix <<- invert
    getInvert <- function() invertMatrix
    list(set = set, get = get,
         setInvert = setInvert,
         getInvert = getInvert)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invert <- x$getInvert()
    if(!is.null(invert)) {
        message("getting cached data")
        return(invert)
    }
    data <- x$get()
    invert <- solve(data, ...)
    x$setInvert(invert)
    invert
}

# TESTING
# x <- makeCacheMatrix(matrix(c(2,1,4,3),2,2))
# x$get()
# x$getInvert()
# cacheSolve(x)
# x$getInvert()
# 
# x$set(matrix(c(4,3,2,1),2,2))
# x$getInvert()
# cacheSolve(x)
# x$getInvert()

