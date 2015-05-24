## There are two functions in the script. First-makeCacheMatrix,second cacheSolve.
## makeCacheMatrix accepts a matrix as input a matrix and creates a list of 4 functions
## 1.setMat sets the value of the  matrix in mymat in main function-make CacheMatrix
## and restores invmat to NULL. cacheSolve will then find inverse of new matrix.
## 2. getMat- function doesn't require any argument, it reurns the matrix stored in the main function.
## 3. setMinv- will store the inverse matrix that is its input into invmat in main function
## 4. getinv- will return the inverse matrix to the main function.

## makeCacheMtrix is function that returns a list of functions storing input and output in main function
## and it doesn't find the inverse

makeCacheMatrix <- function(mymat=matrix()) {
  invmat<-NULL
  setMat<-function(mat2) {
    mymat<<-mat2
    invmat<<-NULL
  }
  getMat<-function() mymat
  setMinv<-function(inverse)invmat<<-inverse
  getinv<-function() invmat
  
  list(setMat=setMat , getMat=getMat,
       setMinv=setMinv , getinv=getinv)
}


## cacheSolve accepts the list of four functions. 
## invmat receives the matrix returned by getinv function from main.
## if the inverse is already found then the received inverse from cache is returned 
## and the function terminates.
## else the special matrixed is returned by the getMat function from main
## and stored in data. Solve function calculates inverse of matrix in data
## setMinv takes invmat as input and stores in invmat of main function.


cacheSolve <- function(x, ...) {
  invmat<-x$getinv()
  if(!is.null(invmat)) {
    message("getting Cached matrix")
    return(invmat)
  }
  data<-x$getMat()
  invmat<-solve(data)
  x$setMinv(invmat)
  invmat
        ## Return a matrix that is the inverse of 'mymat'
}
