library(matlib)
#Given a polynomial as a vector of coefficients, converts it to a printable string
convertPoly <- function(coeff, carats = TRUE) {
  v <- character(0)
  n <- length(coeff)
  while(n > 0) {
    if (n > 2 && coeff[n] > 0){
      if (coeff[n] > 1) v <- c(v, coeff[n])
      v <- c(v,"x",ifelse(carats,"^","<sup>"),n-1,ifelse(carats,"","</sup>"),"+")
    }

    if (n== 2 && coeff[2] > 0) {
      if (coeff[2] > 1) v <- c(v, coeff[2])
      v <- c(v,"x","+")
    }
    if ((n == 1) && (length(v) == 0 || coeff[1]>0)) v <- c(v, coeff[1])
    n <- n-1
  }
  if (tail(v,1)=="+") v <- head(v,-1)
  return(paste(v,sep="",collapse=""))
}

#Computes all the powers of a generator, which must be a constant or x
#If a power of x that is too high arises, it is replaced by a polynomial of lower degree
#The first item in the list is always 0
#The second item in the list is always 1

powerTable <- function(p,n, gen, replace) {
tbl <- list(rep(0,n))
tbl[[1]] <- rep(0,n)
tbl[[2]] <- c(1, rep(0,n-1))
product <- gen
#Case 1: The field is Z_p
if (n == 1) {
  for (i in 3:p) {
    tbl[[i]] <- product
    product <- (product*gen)%%p
    
  }      
}  
#Case 2: The generator is x
if (n > 1){
  for (i in 3:p^n) {
    tbl[[i]] <- product
    # product <- (c(0,head(product,-1))+tail(product,1)*replace)%%p
    product <- (c(0,head(product,-1))+tail(product,1)*replace)%%p
  }
}
return(tbl)
}
# gen = 3, replace = x^2
p2 <- powerTable(5,2,c(3,0,0,0),c(0,0,1,0)); p2
p2poly <- lapply(p2, convertPoly); p2poly

# gen = x^2 replace = 3
p2 <- powerTable(5,2,c(0,0,1,0),c(3,0,0,0)); p2
p2poly <- lapply(p2, convertPoly); p2poly

length(p2poly)

result <- character(0)
for (i in 25) {
  result <- paste(result, lapply(powerTable(5,2,c(0,0,1,0),c(3,0,0,0)), convertPoly), sep="<br/>")
}

result

p <- 3
n <- 2
product <- c(0,1)
replace <- c(1,1)
tbl <- list(rep(0,n))
tbl[[1]] <- rep(0,n)
tbl[[2]] <- c(1, rep(0,n-1))
tbl

tbl[[1]] <- product
product <- (c(0,head(product,-1))+tail(product,1)*replace)%%p;product
tbl

head(3,-1)

tail(3,1)
(3*(c(0,0,1,0)))%%5

# powerTable(7,1,3,0)
# p <-powerTable(2,4,c(0,1,0,0),c(1,1,0,0));p
# convertPoly(p)
powerTable(3,2,c(0,1),c(1,1))
p <- powerTable(2,3,c(0,1,0),c(1,1,0));p
lapply(p, convertPoly)

test <- c(3,4)
convertPoly(test)

8%%5

# p2 <- powerTable(5,2,c(3,0,0,0),c(0,0,1,0)); p2
p2 <- powerTable(5,2,c(0,0,1,0),c(3,0,0,0)); p2
poly <- lapply(p2, convertPoly); poly

A <<- matrix(c(1,2,1,1), nrow=2,ncol=2,byrow = TRUE);A

inv <- solve(A)%%3; inv
