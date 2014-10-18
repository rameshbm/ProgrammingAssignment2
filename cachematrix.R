## Put comments here that give an overall description of what your
## functions do

## It is like c class used to encapsulation of functionality. 
## create class and set the matrix to find inverse(solve)
## one can use set to assign new matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL;
	  isidentical <- FALSE;
        set <- function(y) {
		if (!identical(x,y)) {
			x <<- y;
			m <<- NULL;
	  	      isidentical <<- FALSE;
		} else {
	  	      isidentical <<- TRUE;
		}

        }
        get <- function() x;
        setinverse <- function(inverse) {m <<- inverse; isidentical <<- TRUE;}
        getinverse <- function() m ;
	  isIdentical <- function() isidentical;
        list(	  set = set
			, get = get
			, isIdentical = isIdentical
			, setinverse = setinverse
			, getinverse = getinverse 
		);
}


## cacheSolve Is used to retrive the inverse of matrix, which will check if matrix is same then 
## retrives the cache and retruns other wise computes the inverse and saves it in the object.

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- z$getinverse();

        if( !is.null(mi) && z$isIdentical()) {
           message("getting cached data");
           return(mi);
        }

        data <- z$get();
	  data;
        mi <- solve(data, ...);
        z$setinverse (mi);
        mi;
}
## Here is the sample program demonstrate how to use/test above two functions.

checkmtx<-function(){
	
	hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }; ## some random matrix
	h8 <- hilbert(8); ## get 8X8 matrix

	kk<-makeCacheMatrix(); 			## create object
	kk$set(h8);					## set the matrix we want to find inverse
	for(i in 1:4){				## we run in loop and check it retrives from cache second time.
	   tt<- cacheSolve(kk);
	   print(tt);
	   if (i > 1){ 				## after two tries set new matrix and see it calculates inverse
            h4<-hilbert(4);			## again set the same matrix thiw time it should 
		kk$set(h4);				## not calculate instead retrun from cache
	   }
	}
}
##debug(cacheSolve)
##debug(makeCacheMatrix)
##debug(checkmtx)
checkmtx()

