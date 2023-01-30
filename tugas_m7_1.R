# import data csv
data_ipm <- read.csv("/Users/tori/Documents/data_ipm_jatim.csv")

# ubah data csv menjadi matriks
m_data_ipm <- as.matrix(data_ipm)

# menentukan vektor y
vektor_y <- as.vector(m_data_ipm[,1])

# menentukan matriks X
j <- rep(1,length(vektor_y))
matx <- matrix(cbind(j,m_data_ipm[,c(2,3)]),ncol=3)

# menentukan vektor y dengan prosedur jackniffe
vektor_jack <- function(x)
{
  x <- as.vector(x)
  n <- length(x)
  xbaru <- list()
  for (i in 1:n)
  {
    xbaru[[i]] <- x[-i]
  }
  return(xbaru)
}
yjack <- vektor_jack(vektor_y)

# menentukan matriks X dengan prosedur jackniffe
matrix_jack <- function(x)
{
  x <- as.matrix(x)
  n <- nrow(x)
  xbaru <- list()
  for (i in 1:n)
  {
    xbaru[[i]] <- x[-i,]
  }
  return(xbaru)
}
xjack <- matrix_jack(matx)

# menghitung estimator-estimator dari sampel jackknif
reggan<-function(x,y)
{
  x<-as.matrix(x)
  y<- as.vector(y)
  n<-length(y)
  b <- list()
  for (i in 1:n)
  {
    b[[i]] <- solve(t(x[[i]])%*%x[[i]])%*%(t(x[[i]])%*%y[[i]]) 
  }
  return(b)
}
b <- reggan(xjack,yjack)

# menentukan estimator koefisien regresi
bjack <- function(x)
{
  beta <- do.call(cbind,x)
  bja <- apply(beta,1,mean)
  return(bja)
}
beta_jack <- bjack(b)

# uji hipotesis 
uji_hipotesis <- function(x,alpha)
{
  b <- bjack(x)
  beta <- do.call(cbind,x)
  beta_jack <- bjack(x)
  t_beta <- t(beta)
  bb <- vector()
  ba <- vector()
  hasil <- list()
  for (i in 1:(ncol(t_beta)))
  {
    bb[i] <- quantile(t_beta[,i],prob=alpha/2)
    ba[i] <- quantile(t_beta[,i],prob=1-alpha/2)
    if (bb[i]<=0 && 0<=ba[i]){
      hasil[[i]] <- print("Terima H0")
    }
    else {
      hasil[[i]] <- print("Tolak H0")
    }
  }
  return(hasil)
}
hipo <- uji_hipotesis(b,0.05)
names(hipo) <- c("B0", "B1","B2")