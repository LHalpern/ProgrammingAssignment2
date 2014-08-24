## makeCacheMatrix - Creates a matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function () m
        list(set = set, get = get,setmean = setmean, getmean = getmean)
        }


## cache - computes the inverse of a special matrix

cache <- function(x, ...) {
        m <- x$makeCacheMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmean(m)
        m
}
