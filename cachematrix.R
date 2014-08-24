## JHopkins Coursera R Programming: Assignment 2
##
## Two functions to calculate the inverse of a matrix.
## The first 'makeCacheMatrix' creates a list of functions for calling and 
## caching a matrix and its inverse.
## The second 'cacheSolve' generates the inverse of any invertible matrix 
## entered by calling the list of functions from 'makeCacheMatrix'.
#
#
#  Function name: makeCacheMatrix
#  
#  Description
#
#  Arguments:
#    new.m: a matrix specified by user or passed from the function 'cacheSolve'
#
#  Returns:
#    A list containing 4 functions for getting and storing and matrix and its
#    inverse.
#
makeCacheMatrix <- function(new.m = matrix()) {
        force(new.m)
        mat.new <- matrix()  # initialize temporary matrix
        cache.inv <- NULL  # initialize solution for inverse of matrix
        cache <- matrix()  #initialize list of matrices
        cache.count <- 0   #initialize counter for no. of matrices in 'cache'
        set <- function(new.m) {
                cache.count <- cache.count + 1  # increment counter
                cache[[cache.count]] <<- new.m  # store new matrix in list
                cache.count <<- cache.count  #save counter value to parent envt
        }
        get <- function(cache.count) {
                mat.list <- sapply(cache, function(t) identical(t, new.m))
                #     create logical vector of any identical matrix in cache
                mat.list <<- mat.list  # set mat.list vector in parent envt
        }
        set.inv <- function(mat.new) cache.inv[[cache.count]] <- solve(mat.new)
        get.inv <- function(cache.count) {
                mat.list <- sapply(cache, function(t) identical(t, new.m))
                #     create logical vector of any identical matrix in cache
                mat.list <<- mat.list  # set mat.list vector in parent envt
                subset(cache.inv, mat.list, c(1,2))  # return inverse for match
        }
        list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
        mat.new <<- mat.new  # save matrix to parent envionment
        cache.count <<- cache.count  #save counter to parent environment
}
#  end of first function
#
#
#  Function name: makeCacheMatrix
#  
#  Description
#
#  Arguments:
#    new.m: a matrix specified by user or passed from the function 'cacheSolve'
#
#  Returns:
#    A list containing 4 functions for getting and storing and matrix and its
#    inverse.
#
makeCacheMatrix <- function(new.m = matrix()) {
        force(new.m)
        mat.new <- matrix()  # initialize temporary matrix
        cache.inv <- NULL  # initialize solution for inverse of matrix
        cache <- matrix()  #initialize list of matrices
        cache.count <- 0   #initialize counter for no. of matrices in 'cache'
        set <- function(new.m) {
                cache.count <- cache.count + 1  # increment counter
                cache[[cache.count]] <<- new.m  # store new matrix in list
                cache.count <<- cache.count  #save counter value to parent envt
        }
        get <- function(cache.count) {
                mat.list <- sapply(cache, function(t) identical(t, new.m))
                #     create logical vector of any identical matrix in cache
                mat.list <<- mat.list  # set mat.list vector in parent envt
        }
        set.inv <- function(mat.new) cache.inv[[cache.count]] <- solve(mat.new)
        get.inv <- function(cache.count) {
                mat.list <- sapply(cache, function(t) identical(t, new.m))
                #     create logical vector of any identical matrix in cache
                mat.list <<- mat.list  # set mat.list vector in parent envt
                subset(cache.inv, mat.list, c(1,2))  # return inverse for match
        }
        list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
        mat.new <<- mat.new  # save matrix to parent envionment
        cache.count <<- cache.count  #save counter to parent environment
}
#  end of first function
