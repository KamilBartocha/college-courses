library(MASS)
library(matrixcalc)

## Create the matrix
set.seed(12345)
n <- 4
m <- replicate(n, runif(n)) 
m[lower.tri(m)] <- t(m)[lower.tri(m)]
m

is.positive.definite(m)


## Inverse the matrix
inv1 <- solve(m)
inv2 <- solve(m, tol = .Machine$double.eps)
inv3 <- qr.solve(m)
inv4 <- ginv(m)
inv5 <- chol2inv(m)

all.equal(inv1, inv2)
all.equal(inv1, inv3)
all.equal(inv1, inv4)
all.equal(inv1, inv5)

## Eigenvalues of the inverse
em1 <- eigen(inv1)
em2 <- eigen(inv2)
em3 <- eigen(inv3)
em4 <- eigen(inv4)
em5 <- eigen(inv5)

## Plot the abs of the eigenvalues (may be complex)
myPch=c( 20, 15, 17, 25, 3 )
plot(abs(em1$values),pch=myPch[1],cex=1.5)
points(abs(em2$values),pch=myPch[2], cex=1.5)
points(abs(em3$values),pch=myPch[3], cex=1.5)
points(abs(em4$values),pch=myPch[4], cex=1.5)
points(abs(em5$values),pch=myPch[5], cex=1.5)
legend( "topright", c("solve","solve-double","solve-fast","Moore-Penrose","Cholesky"), pch=myPch )


## Create the matrix
set.seed(12345)
n <- 4
m <- runif(n) 
d <- diag(m)
d

is.positive.definite(d)

chol(d)
d2 <- chol(d) %*% t(chol(d))


## Inverse the matrix
inv1 <- solve(d)
inv2 <- solve(d, tol = .Machine$double.eps)
inv3 <- qr.solve(d)
inv4 <- ginv(d)
inv5 <- chol2inv(chol(d))
all.equal(inv1, inv5)
