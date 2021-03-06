## calculates the inverse of a matrix
## caches the inverse to avoid recomputation

## function to set and get both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_mat<- NULL
        set<- function(y) {
                x<<-y
                inv_mat<<-matrix()
        }
        get<- function() x
        setinv<- function(inverse = matrix()) inv_mat<<- inverse
        getinv <- function() inv_mat
        list(set = set,get = get,setinv = setinv,getinv=getinv)
}



## function to calculate inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat<-x$getinv()
        
        if (!(is.null(inv_mat))) {
                message("getting cached data")
                return(inv_mat)
                
        }
        data<-x$get()
        inv_mat<- solve(data)
        x$setinv(inv_mat)
        inv_mat
}
