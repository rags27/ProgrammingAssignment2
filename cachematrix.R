## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and also pulls it's inverse matrix if available. This list of functions is then passed as argument
## to the next function

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <-function(y){
                x<<-y
                m<<-NULL
        }
        get <-function()x
        setinv <-function(inv)m<<-inv
        getinv <-function() m
        list(set =set, get = get, setinv = setinv, getinv = getinv)
}

##This function takes as argument the list created in the previous function and if the inverse is already cached, returns it.
## Otherwise it calculates the inverse and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinv()
        if(!is.null(m)){
                message("getting cached inverse matrix")
                return(m)
        }
        data<-x$get()
        m<-solve(data)
        x$setinv(m)
        m
}