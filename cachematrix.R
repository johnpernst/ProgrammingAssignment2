## JHopkins Coursera R Programming: Assignment 2
##
## Two functions to calculate the inverse of a matrix using cached values 
## when available.
#
#  Function name: makeCacheMatrix
#  
#  Description: creates a list of functions for calling and caching a matrix 
#   and its inverse.
#
#  Arguments:
#    new.m: a matrix specified by user or passed from the function 'cacheSolve'
#
#  Returns:
#    A list containing 4 functions for getting and storing a matrix and its
#    inverse from a cache of matrices and inverses.
#
makeCacheMatrix <- function(new.m = matrix()) {
        force(new.m)
        cache.inv <- list()  # initialize solution for inverse of matrix
        cachem <- list()  #initialize list of matrices 'cachem'
        cache.count <- 0   #initialize counter for no. of matrices in 'cache'
        mat.list <- logical() # initialize subset factor for selecting matrix
        set <- function(new.m) {
                cache.count <- cache.count + 1  # increment counter
                cachem[[cache.count]] <- new.m  # store new matrix in list
                cache.count <<- cache.count  #save counter value to parent envt
        }
        get <- function(cache.count) {
                mat.list <- sapply(cachem, function(t) identical(t, new.m))
                #     create logical vector of any identical matrix in cache
                mat.list <<- mat.list  # set mat.list vector in parent envt
        }
        set.inv <- function(new.m) cache.inv[[cache.count]] <- solve(new.m)
        get.inv <- function(cache.count) {
                mat.list <- sapply(cachem, function(t) identical(t, new.m))
                #     create logical vector of any identical matrix in cache
                mat.list <<- mat.list  # set mat.list vector in parent envt
                subset(cache.inv, mat.list, c(1,2))  # return inverse for match
        }
        mat.list <<- mat.list  # save selection vector to parent environment
        cache.count <<- cache.count  #save counter to parent environment
        list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}
#  end of first function
#
## Second Function name: cacheSolve
#
#  Description: For any invertible matrix, returns the inverse of that matrix by
#    first looking in a cache of solutions, and then either returning the cached
#    inverse or a newly solved inverse.
#
#  Calls Function: makeCacheMatrix
#
#  Arguments:
#    new.m: a matrix specified by user 
#
#  Returns:
#    Inverse solution for matrix entered
#
cacheSolve <- function(x=list()) {
        if(all(mat.list) == FALSE) {
                makeCacheMatrix$set()  # if no match, add matrix to cache
                message("generating new inverse solution")
                makeCacheMatrix$set.inv()  #and add inverse to inverse cache
                makeCacheMatrix$get.inv()  # show inverse
                return  # end function
        } 
        message("getting cached data")
        makeCacheMatrix$get.inv()  # show inverse from cached data
}
