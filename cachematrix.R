## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as input and creates a list of functions. This list is then passed as argument
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

##This function takes as argument the list created in the previous function and if the inverse of the matrix is already cached
##, returns it otherwise it calculates the inverse using solve() and returns it

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