## 3. Function-on-Function Regression
### (0) Load Data
```{r}
## 강수량 데이터
weather <- read.csv("강수량_함수.csv", fileEncoding = "euc-kr")
rownames(weather) <- seq(1,354)
weather <- weather[-307,]
weather <- weather[,-1]

## 배달데이터
delivery <- read.csv("배달주문건수_함수.csv", fileEncoding = "euc-kr") 
rownames(delivery) <- seq(1,353)
delivery <- delivery[,-1]

times = seq(1,353)
X <- weather
X <- as.matrix(X)
my_basis <- create.fourier.basis(c(0,353), nbasis=15)
X.F<-Data2fd(times,X,my_basis)

Y <- delivery
Y <- as.matrix(Y)
my_basis_Y <- create.bspline.basis(c(0,353), nbasis=20)
Y.F<-Data2fd(times,Y,my_basis_Y)
```

### (1) Scaling 하지 않은 경우
```{r}
weatherfd  <- X.F
deliveryfd <- Y.F
plot(X.F, main="날씨 함수형데이터")
plot(Y.F, main="배달 함수형데이터")

### regression ###
deli.weather.f <- fRegress(Y.F ~ X.F)

plot(deli.weather.f$yfdPar, main="실제 배달량")
plot(deli.weather.f$yhatfdobj, main="non-scaling을 통한 예측 배달량")

par(mfrow=c(1,2))
plot(deli.weather.f$betaestlist$const, main=expression(beta[0]))
plot(deli.weather.f$betaestlist$X.F, main=expression(beta[1]))
```

### (2) Scaling 한 경우
```{r} 
## 배달데이터 scale 하기
df_copy <- delivery
for(i in 1:17){
  df_copy[,i] <- scale(df_copy[,i])
}

Y.scale <- as.matrix(df_copy)
my_basis_Y <- create.bspline.basis(c(0,353), nbasis=20)
Y.F.scale <- Data2fd(times,Y.scale,my_basis_Y)
plot(X.F, main="날씨 함수형데이터")
plot(Y.F.scale, main="scaling 배달 함수형데이터")

### regression ###
deli.weather.f.scale <- fRegress(Y.F.scale ~ X.F)

plot(deli.weather.f.scale$yfdPar, main="실제 배달량의 scale 값")
plot(deli.weather.f.scale$yhatfdobj, main="scaling을 통한 예측 배달량")

par(mfrow=c(1,2))
plot(deli.weather.f.scale$betaestlist$const, main=expression(beta[0]))
plot(deli.weather.f.scale$betaestlist$X.F, main=expression(beta[1]))
```

```{r}
### unscaling ###
unscale_copy <- deli.weather.f.scale$yhatfdobj 
unscale_sd <- apply(delivery,2,sd)
unscale_mean <- apply(delivery,2,mean)

for (i in 1:17){
  unscale_copy$y[,i] <- unscale_copy$y[,i]*unscale_sd[i]+unscale_mean[i]
}

## unscale해서 데이터 그리기
tmp <- hue_pal()(17)

plot(unscale_copy$y[,1], type='l', col="#F8766D", ylim=c(0,21000),xlab="time",ylab="지역별 배달건수 예측값", main="scaling 예측값을 unscaling한 배달량")
for (i in 2:17){
  par(new=TRUE)
  plot(unscale_copy$y[,i], type='l', col=tmp[i], ylim=c(0,21000), xaxt="n",yaxt="n",xlab="",ylab="")
}
```

```{r}
### 경기/서울 제외 ###
Y.nogk <- Y[,-c(2,5)]
my_basis_Y <- create.bspline.basis(c(0,353), nbasis=20)
Y.F.nogk<-Data2fd(times,Y.nogk,my_basis_Y)
plot(Y.F.nogk, main="실제 배달량(서울/경기 제외)")

plot(unscale_copy$y[,1], type='l', col="#F8766D", ylim=c(0,2500),xlab="time",ylab="지역별 배달건수 예측값", main="scaling 예측값을 unscaling한 배달량(서울/경기 제외)")
for (i in c(3,4,6:17)){
  par(new=TRUE)
  plot(unscale_copy$y[,i], type='l', col=tmp[i], ylim=c(0,2500), xaxt="n",yaxt="n",xlab="",ylab="")
}
```

### (3) 동일 Basis 사용한 경우
```{r}
## cc: scale한 함수형데이터
cc <- array(0,dim=c(353,17,2))
cc[,,1] <- as.matrix(weather)
cc[,,2] <- as.matrix(df_copy)


times = seq(1,353)
datarange <- c(0,353)

# 그래프 그리는 함수
basis_function5 <- function(X.F, num){
  mu.F <- mean.fd(X.F)
  sd.F <- sd.fd(X.F)
  plot(X.F,col='grey')
  plot(mu.F,col=1,lwd=2,add=TRUE)
  plot(mu.F+2*sd.F,col=2,lwd=1,add=TRUE)
  plot(mu.F-2*sd.F,col=2,lwd=1,add=TRUE)
  title(paste('nbasis = ',num))
}

basis_function4 <- function(my_basis,num){
  harmaccelLfd <- vec2Lfd(c(0, (2*pi/20)^2, 0), rangeval=datarange)
  datafd <- smooth.basisPar(times, cc, my_basis, Lfdobj=harmaccelLfd, lambda=1e-2)$fd
  weatherfd  <- datafd[,1]
  deliveryfd <- datafd[,2] 
  basis_function5(weatherfd,num)
  basis_function5(deliveryfd,num)
}

my_basis1 <- create.bspline.basis(c(0,353),nbasis=5, norder = 5) 
my_basis2 <- create.bspline.basis(c(0,353),nbasis=10, norder = 5) 
my_basis3 <- create.bspline.basis(c(0,353),nbasis=15, norder = 5) 
my_basis4 <- create.bspline.basis(c(0,353),nbasis=20, norder = 5) 

my_fourier1 <- create.fourier.basis(c(0,353), nbasis=5)
my_fourier2 <- create.fourier.basis(c(0,353), nbasis=10)
my_fourier3 <- create.fourier.basis(c(0,353), nbasis=15)
my_fourier4 <- create.fourier.basis(c(0,353), nbasis=20)

#강수량데이터
par(mfrow=c(2,2))
basis_function4(my_basis1,5)
basis_function4(my_basis2,10)
basis_function4(my_basis3,15)
basis_function4(my_basis4,20)

par(mfrow=c(2,2))
basis_function4(my_fourier1,5)
basis_function4(my_fourier2,10)
basis_function4(my_fourier3,15)
basis_function4(my_fourier4,20)
```
```{r}
### fourier nbasis=15 선택 ###
my_fourier3 <- create.fourier.basis(c(0,353), nbasis=15)

harmaccelLfd <- vec2Lfd(c(0, (2*pi/20)^2, 0), rangeval=datarange)
datafd <- smooth.basisPar(times, cc, my_fourier3, Lfdobj=harmaccelLfd, lambda=1e-2)$fd
weatherfd  <- datafd[,1]
deliveryfd <- datafd[,2] 

### regression ###
deli.weather.four15 <- fRegress(deliveryfd ~ weatherfd)

plot(deli.weather.four15$yfdPar, main="실제 배달량의 scale 값")
plot(deli.weather.four15$yhatfdobj, main="푸리에 basis=15를 통한 예측 배달량")

par(mfrow=c(1,2))
plot(deli.weather.four15$betaestlist$const, main=expression(beta[0]))
plot(deli.weather.four15$betaestlist$weatherfd, main=expression(beta[1]))
```
```{r}
### unscaling ###
unscale_copy2 <- deli.weather.four15$yhatfdobj 
unscale_sd <- apply(delivery,2,sd)
unscale_mean <- apply(delivery,2,mean)

for (i in 1:17){
  unscale_copy2$y[,i] <- unscale_copy2$y[,i]*unscale_sd[i]+unscale_mean[i]
}

## unscale해서 데이터 그리기
tmp <- hue_pal()(17)

plot(unscale_copy2$y[,1], type='l', col="#F8766D", ylim=c(0,21000),xlab="time",ylab="지역별 배달건수 예측값", main="(동일basis) scaling 예측값을 unscaling한 배달량")
for (i in 2:17){
  par(new=TRUE)
  plot(unscale_copy2$y[,i], type='l', col=tmp[i], ylim=c(0,21000), xaxt="n",yaxt="n",xlab="",ylab="")
}
```
```{r}
### 경기/서울 제외 ###
plot(unscale_copy2$y[,1], type='l', col="#F8766D", ylim=c(0,2500),xlab="time",ylab="지역별 배달건수 예측값", main="scaling 예측값을 unscaling한 배달량(서울/경기 제외)")
for (i in c(3,4,6:17)){
  par(new=TRUE)
  plot(unscale_copy2$y[,i], type='l', col=tmp[i], ylim=c(0,2500), xaxt="n",yaxt="n",xlab="",ylab="")
}
```
