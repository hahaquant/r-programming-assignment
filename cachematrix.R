
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {

    inverse_cached <- NULL
    # cache the matrix and initialize the variable to cache the inverse matrix
    cache_matrix <- function(matrix) {
            matrix_cached <<- matrix
            inverse_cached <<- NULL
    }
    # get the cahced matrix
    get_matrix <- function() {
      return (matrix_cached)
    }

    # cache the inverse of the matrix
    cache_inverse <- function(inverse_matrix) {
      inverse_cached <<- inverse_matrix
    }
    # get the inverse of the matrix
    get_inverse <- function() {
      return(inverse_cached)
    }

    return (list(cache_matrix = cache_matrix, get_matrix = get_matrix,
         cache_inverse = cache_inverse, get_inverse = get_inverse))
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(special_matrix, ...) {

    # get the inverse of special_matrix
    inverse_matrix<- special_matrix$get_inverse()
    # if the inverse already exists, return the cached inverse
    if( !is.null(inverse_matrix) ) { # inverse already exists
      return(inverse_matrix)
    }

    # if the inverse doesn't exist, get the matrix and compute the inverse
    mat <- special_matrix$get_matrix()
    # compute the inverse of the matrix
    inverse_matrix <- solve(mat) %*% mat

    # cache the inverse
    special_matrix$cache_inverse(inverse_matrix)
    # return the inverse
    inverse_matrix
}