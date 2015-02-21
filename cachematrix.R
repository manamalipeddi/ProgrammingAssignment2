#Final R Code for Computing Matrix Inverse using Lexical Scoping
#Code written by Manasa Malipeddi for the Coursera R Programming Course
#Programming Assignment 2 for peer review

#The following functions compute the inverse of a matrix using lexical scoping
#makeCacheMatrix defines functions that access and modify cached variables returning a list of these functions
#cacheSolve uses the functions defined in makeCacheMatrix and computes to inverse of any nxn matrix

#this function computes the inverse of a square matrix
cacheSolve <- function(my_matrix, ...) {
        
        #if same matrix as last time is passed, inverse already exists, print that
        cached_matrix <- my_matrix$getmatrixincache()
        new_matrix <- my_matrix$getmatrix()
        if(identical(new_matrix, cached_matrix)) {
                message("Same as previous matrix. Retrieving inverse from cache...")
                matrix_inverse <- my_matrix$getinverse()
        }
        else {
                #compute new inverse for the new matrix
                matrix_inverse <- solve(new_matrix, ...)
                #save new matrix and new inverse computed into cache for future
                my_matrix$setmatrixincache(new_matrix)
                my_matrix$setinverse(matrix_inverse)         
        }
        
        #finally return inverse matrix to the console
        matrix_inverse
}

#this function creates functions to manipulate cache variables
makeCacheMatrix <- function(my_matrix=matrix()) {
        #initialize variables
        my_matrix_cache <- NULL
        matrix_inverse_cache <- NULL
        
        #the set function stores new matrix 
        setmatrix <- function(new_matrix) my_matrix <<- new_matrix        
        # get new matrix 
        setmatrixincache <-  function(new_matrix) my_matrix_cache <<- new_matrix
        #get new matrix
        getmatrix <- function() my_matrix
        # get cached old matrix
        getmatrixincache <- function() my_matrix_cache
        #this function sets the inverse computed into cache for future use
        setinverse <- function(matrix_inverse) matrix_inverse_cache <<- matrix_inverse
        #this function returns the inverse already computed
        getinverse <- function() matrix_inverse_cache

        #return the list of functions we just defined to the calling function
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, 
             getinverse = getinverse, getmatrixincache = getmatrixincache, setmatrixincache = setmatrixincache)
}