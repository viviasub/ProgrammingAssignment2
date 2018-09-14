## take a square matrix (not singular) and create an object of type makeCacheMarix
## that contain two funcion to set(x) and get() the matrix
## and two function that set_inv_mat(x) and get_inv_mat()  inverse matrix, if inv_mat is not present it return NULL
## the object is retun as a list with 4 elements
##
## es.  x <- matrix(c(1,5,2,7,1,9,2,2,2), nrow=3)       , create and assign the matrix
##      temp <- makeCacheMatrix(x)                      , create and assign the object
##      temp$get()                                      , syntax of the get method
##      temp$set(x)                                     , syntax of the set method
##      temp$get_inv_mat()                              , syntax of the get_inv_mat method
##      temp$set_inv_mat(x)                             , syntax of the  method


makeCacheMatrix <- function(x = matrix()) {
        inv_mat<<-NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        set_inv_mat <- function(inv_mat) inv_mat <<- inv_mat
        get_inv_mat<- function() inv_mat
        list(set = set, get = get,
             set_inv_mat = set_inv_mat,
             get_inv_mat = get_inv_mat)
}

## the function run the get_inv_mat methods present inside the object of type makeCacheMatrix
## if inv_mat is presente return it
## otherwise recall the matrix by get() method and calculate the inverse matrix by solve(data,...)
## than set the value by set_inv_mat methods and return the inv_mat
##
## es:  cacheSolve(temp)        , how to call the function
##
## IMPORTANT! the function has to be call on makeCacheMatrix object

cacheSolve <- function(x, ...) {
        inv_mat <- x$get_inv_mat()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$set_inv_mat(inv_mat)
        inv_mat
}

