## 1. 강수량 데이터(X)
### (1) Data2fd
basis_function <- function(my_basis, num){
  X.F <- Data2fd(times, X, my_basis)
  mu.F <- mean.fd(X.F)
  sd.F <- sd.fd(X.F)
  plot(X.F,col='grey')
  plot(mu.F,col=1,lwd=2,add=TRUE)
  plot(mu.F+2*sd.F,col=2,lwd=1,add=TRUE)
  plot(mu.F-2*sd.F,col=2,lwd=1,add=TRUE)
  title(paste('nbasis = ',num))
  legend("topright", c("mean", "sd"), col = c(1,2), lwd = c(2,1), cex = 0.5)
}

### bspline ###
# basis 개수 정하기 
times <- seq(1, 353)
X <- weather
X <- as.matrix(X)
my_basis1 <- create.bspline.basis(c(0,353),nbasis=5) 
my_basis2 <- create.bspline.basis(c(0,353),nbasis=10) 
my_basis3 <- create.bspline.basis(c(0,353),nbasis=15) 
my_basis4 <- create.bspline.basis(c(0,353),nbasis=20) 
basis_function(my_basis1,5)
basis_function(my_basis2,10)
basis_function(my_basis3,15)
basis_function(my_basis4,20)

### fourier ###
my_fourier1 <- create.fourier.basis(c(0,353), nbasis=5)
my_fourier2 <- create.fourier.basis(c(0,353), nbasis=10)
my_fourier3 <- create.fourier.basis(c(0,353), nbasis=15)
my_fourier4 <- create.fourier.basis(c(0,353), nbasis=20)
basis_function(my_fourier1,5)
basis_function(my_fourier2,10)
basis_function(my_fourier3,15)
basis_function(my_fourier4,20)

### (2) Penalized Smoothing
basis_function3 <- function(my_basis){
  par(mfrow=c(3,3))
  for (i in c(0.01,0.1,0.5,1,10,5000,10000,100000,500000)){
    my_par<-fdPar(my_basis,Lfdobj=2,lambda=i)
    X.S<-smooth.basis(times,X,my_par)
    
    X.S.F<-X.S$fd 
    XD.S.F<-deriv.fd(X.S.F,Lfdobj=2)
    muD.F<-mean.fd(XD.S.F)
    sdD.F<-sd.fd(XD.S.F)
    main1=paste("lambda=",i," GCV=",round(mean(X.S$gcv),3))
    plot(XD.S.F,col='grey',main=main1)
    plot(muD.F,lwd=2,add=TRUE)
    plot(muD.F,col=1,lwd=2,add=TRUE)
    plot(muD.F+2*sdD.F,col=2,lwd=1,add=TRUE)
    plot(muD.F-2*sdD.F,col=2,lwd=1,add=TRUE)
  }
}

### bspline ###
my_basis1 <- create.bspline.basis(c(0,353),nbasis=5) 
my_basis2 <- create.bspline.basis(c(0,353),nbasis=10) 
my_basis3 <- create.bspline.basis(c(0,353),nbasis=15) 
my_basis4 <- create.bspline.basis(c(0,353),nbasis=20) 
basis_function3(my_basis1)
basis_function3(my_basis2)
basis_function3(my_basis3)
basis_function3(my_basis4)

### fourier ###
my_fourier1 <- create.fourier.basis(c(0,353), nbasis=5)
my_fourier2 <- create.fourier.basis(c(0,353), nbasis=10)
my_fourier3 <- create.fourier.basis(c(0,353), nbasis=15)
my_fourier4 <- create.fourier.basis(c(0,353), nbasis=20)
basis_function3(my_fourier1)
basis_function3(my_fourier2)
basis_function3(my_fourier3)
basis_function3(my_fourier4)

# nbasis=10, lambda=10 선택
n <- 10; i <- 10

my_basis <- create.bspline.basis(c(0,353),nbasis=n) 
my_par<-fdPar(my_basis,Lfdobj=2,lambda=i)
X.S<-smooth.basis(times,X,my_par)
X.S.F<-X.S$fd 
XD.S.F<-deriv.fd(X.S.F,Lfdobj=2)
muD.F<-mean.fd(XD.S.F)
sdD.F<-sd.fd(XD.S.F)
par(mfrow=c(1,1))
main1=paste("lambda=",i," GCV=",round(mean(X.S$gcv),3))
plot(XD.S.F,col='grey',main=main1)
plot(muD.F,lwd=2,add=TRUE)
plot(muD.F,col=1,lwd=2,add=TRUE)
plot(muD.F+2*sdD.F,col=2,lwd=1,add=TRUE)
plot(muD.F-2*sdD.F,col=2,lwd=1,add=TRUE)

### (3) Functional Data
# fourier nbasis=15 선택
X.F <- Data2fd(times,X,my_fourier3)

### covariance ###
var.F <- var.fd(X.F)
pts <- seq(from = 0,to = 353)
pinch_mat <- eval.bifd(pts, pts, var.F)
persp3D(pts, pts, pinch_mat, main = "covarinace")

### PCA ###
pca.F <- pca.fd(X.F, nharm = 4)
plot(pca.F$harmonics)
cumsum(pca.F$varprop)

### (4) Functional Clustering
my_basis <- create.fourier.basis(c(0,353), nbasis=15) 
fdobj <- smooth.basis(times, X, my_basis)$fd
clustering_plot <- function(k){
  res <- funFEM(fdobj, K=k)
  par(mfrow=c(1,2))
  plot(fdobj,col=res$cls,lwd=2,lty=1)
  fdmeans = fdobj; fdmeans$coefs = t(res$prms$my)
  plot(fdmeans,col=1:max(res$cls),lwd=2)
}
clustering_plot(2)
clustering_plot(3)

res <- funFEM(fdobj, K=3)
fdmeans = fdobj; fdmeans$coefs = t(res$prms$my)
plot(fdmeans,col=1:max(res$cls),lwd=2)

### pca score ###
X.F <- Data2fd(times, X, my_fourier3)
pca.F <- pca.fd(X.F, nharm = 3)
Scores <- pca.F$scores
cluster <- res$cls
plot(Scores, col=cluster, xlab="PC1", ylab="PC2", main="Plot of scores")
legend(x="topleft", col=1:3, legend=c("cluster1","cluster2","cluster3"), pch=1)

### (5) Function-on-Scalar Regression
cluster <- data.frame(t(cluster))
names(cluster) <- colnames(X)
my_basis <- create.bspline.basis(c(0,353),nbasis=20) 
delivery.fd <- smooth.basis(times, X, my_basis)$fd
modmat=cbind(1,model.matrix(~factor(cluster)-1))
constraints = matrix(c(0,1,1,1),1) 
olsmod = fosr(fdobj = delivery.fd, X = modmat, con = constraints, method="OLS", lambda=100 *10:30)
par(mfrow=c(1,4), mar=c(5,2,4,1))
plot(olsmod, split=1, set.mfrow=FALSE,titles=c("OLS: Intercept", levels(factor(cluster))),ylab="",xlab="Day")

## 2. 배달 데이터(Y)
### (1) Data2fd
### bspline ###
# basis 개수 정하기 
times <- seq(1, 353)
X <- delivery; X <- as.matrix(X)
my_basis1 <- create.bspline.basis(c(0,353),nbasis=5) 
my_basis2 <- create.bspline.basis(c(0,353),nbasis=10) 
my_basis3 <- create.bspline.basis(c(0,353),nbasis=15) 
my_basis4 <- create.bspline.basis(c(0,353),nbasis=20) 
basis_function(my_basis1,5)
basis_function(my_basis2,10)
basis_function(my_basis3,15)
basis_function(my_basis4,20)

### fourier ###
my_fourier1 <- create.fourier.basis(c(0,353), nbasis=5)
my_fourier2 <- create.fourier.basis(c(0,353), nbasis=10)
my_fourier3 <- create.fourier.basis(c(0,353), nbasis=15)
my_fourier4 <- create.fourier.basis(c(0,353), nbasis=20)
basis_function(my_fourier1,5)
basis_function(my_fourier2,10)
basis_function(my_fourier3,15)
basis_function(my_fourier4,20)

### (2) Penalized Smoothing
### bspline ###
basis_function3(my_basis1)
basis_function3(my_basis2)
basis_function3(my_basis3)
basis_function3(my_basis4)

### fourier ###
basis_function3(my_fourier1)
basis_function3(my_fourier2)
basis_function3(my_fourier3)
basis_function3(my_fourier4)

# nbasis=20, lambda=10
n <- 20; i <- 10

my_basis <- create.bspline.basis(c(0,353),nbasis=n) 
my_par<-fdPar(my_basis,Lfdobj=2,lambda=i)
X.S<-smooth.basis(times,X,my_par)
X.S.F<-X.S$fd 
XD.S.F<-deriv.fd(X.S.F,Lfdobj=2)
muD.F<-mean.fd(XD.S.F)
sdD.F<-sd.fd(XD.S.F)
par(mfrow=c(1,1))
main1=paste("lambda=",i," GCV=",round(mean(X.S$gcv),3))
plot(XD.S.F,col='grey',main=main1)
plot(muD.F,lwd=2,add=TRUE)
plot(muD.F,col=1,lwd=2,add=TRUE)
plot(muD.F+2*sdD.F,col=2,lwd=1,add=TRUE)
plot(muD.F-2*sdD.F,col=2,lwd=1,add=TRUE)

### (3) Functional Data
# bspline nbasis=20 선택
X.F <- Data2fd(times,X,my_basis4)

### covariance ###
var.F <- var.fd(X.F)
pts <- seq(from = 0,to = 353)
pinch_mat <- eval.bifd(pts, pts, var.F)
persp3D(pts, pts, pinch_mat, main = "covarinace")

### PCA ###
pca.F <- pca.fd(X.F, nharm = 6)
plot(pca.F$harmonics)
cumsum(pca.F$varprop)

### (4) Functional Clustering
fdobj <- smooth.basis(times, X, my_basis4)$fd
clustering_plot <- function(k){
  res <- funFEM(fdobj, K=k)
  par(mfrow=c(1,2))
  plot(fdobj,col=res$cls,lwd=2,lty=1)
  fdmeans = fdobj; fdmeans$coefs = t(res$prms$my)
  plot(fdmeans,col=1:max(res$cls),lwd=2)
}
clustering_plot(2)
clustering_plot(3)

res <- funFEM(fdobj, K=3)
fdmeans = fdobj; fdmeans$coefs = t(res$prms$my)
plot(fdmeans,col=1:max(res$cls),lwd=2)

### pca score ###
X.F <- Data2fd(times, X, my_basis4)
pca.F <- pca.fd(X.F, nharm = 5)
Scores <- pca.F$scores
cluster <- res$cls
plot(Scores, col=cluster, xlab="PC1", ylab="PC2", main="Plot of scores")
legend(x="bottomleft", col=1:3, legend=c("cluster1","cluster2","cluster3"), pch=1)

### (5) Function-on-Scalar Regression
cluster <- data.frame(t(cluster))
names(cluster) <- colnames(X)
my_basis <- create.bspline.basis(c(0,353),nbasis=20) 
delivery.fd <- smooth.basis(times, X, my_basis)$fd
modmat=cbind(1,model.matrix(~factor(cluster)-1))
constraints = matrix(c(0,1,1,1),1) 
olsmod = fosr(fdobj = delivery.fd, X = modmat, con = constraints, method="OLS", lambda=100 *10:30)
par(mfrow=c(1,4), mar=c(5,2,4,1))
plot(olsmod, split=1, set.mfrow=FALSE,titles=c("OLS: Intercept", levels(factor(cluster))),ylab="",xlab="Day")
