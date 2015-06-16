# Since inversing a matrix is heavy on the memory, I am writing a function to cache the 
# inverse of a matrix.

# The first function 'makeCacheMatrix' caches the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(new_value) {
                x <<- new_value
                inverse <<- NULL
        }
        get <- function() x
        set_new <- function(new_inverse) inverse <<- new_inverse
        get_new <- function() inverse
        list(set = set, get = get, set_new = set_new, get_new = get_new)
}

# If the value was calculated before, it will be stored in cache. Instead of doing the
# calculation again, this second function will return stored value. 
# If not, it will calculate a new value.

cacheSolve <- function(x, ...) {
        inverse <- x$get_new()
        if(!is.null(inverse)) { 
                message("Returning value from cache")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$set_new(inverse)
        inverse
}