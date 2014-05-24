## This function take 3 parameters: "x" of class matrix, "nrow" to set number of row, and "ncol" to set number of column
## makeCacheMatrix function contains 5 subfunctions(methods) as follow:
## set(y) - to setup a different matrix where the parameter "y" will overwrite matrix "x"
## get_matrix() - non-argument method that returns the matrix
## compute_inverse(x) - the argument is an object of a matrix and this method will return the inverse of that matrix object.
## get_inverse()- non-argument method that returns the inverse of matrix 
## set_inverse(inv) - this method sets the inverse to inv.	


makeCacheMatrix <- function(x = matrix(),nrow,ncol) {
	inverse <- NULL
	mat <- matrix(x,nrow,ncol)
	set <- function(y,nrow1,ncol1){
		mat <<- matrix(y,nrow1,ncol1)
		inverse <<- NULL
	}
	get_matrix <- function() mat
	get_inverse <- function() inverse
	set_inverse <- function(inv) inverse <<- inv
	compute_inverse <- function(mat) inverse <<- solve(mat)
	list(set = set, 
		get_inverse = get_inverse,
		get_matrix = get_matrix,
		compute_inverse = compute_inverse,
		set_inverse = set_inverse)


}

## cacheSolve function takes 1 argument of matrix object and returns the inverse
## first it will check if the inverse of the matrix has been calculated in the function above
## if not calculated, the inverse is NULL and it will skip the IF statement and bracket
## if the inverse has been calculated, message will appear and it will return the inverse
## if not, the inverse is calculated in this function body using "solve" function 
## ... then the inverse is cached using set_inverse method of the makeCacheMatrix(...) function.



cacheSolve <- function(mat, ...) {
       inverse <- mat$get_inverse()
	 if(!is.null(inverse)){
		message("getting inversed and cached matrix")
		return(inverse)
	 }
	 inverse <-solve(mat$get_matrix(), ...)
	 mat$set_inverse(inverse)
	 inverse
}
