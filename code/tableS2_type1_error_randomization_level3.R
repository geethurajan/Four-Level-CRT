# ============================================================================
# Produces: Table S2: sample size N4 and empirical Type I error, randomization at level 3
# This script runs one row of the table; README.txt, Section III, says which
# row and which lines to edit for the other rows.
# Run via main.R, or on its own from the package folder. See README.txt.
# ============================================================================
alpha <- 0.05
beta <- 0.2
N1 <- 4
N2 <-4
N3T<-6
N3C<-4
N3 <-N3T+N3C
r<-N3C/N3T 
rho1 <- 0.4
rho2 <- 0.1
rho3 <- 0.05
sigma <- 1
beta0 <- 0
Delta <- 0.6
zi <-0
Z_sum <-(qnorm(0.975, 0, 1) + qnorm(0.8, 0, 1))^2
complex_part1 <- 1 + (N1 - 1) * rho1 + N1 * (N2 - 1) * rho2 - N1 * N2 * rho3 
denominator <- (r / (r + 1)) * Delta^2 * N3T * N2 * N1
N4 <- ceiling((Z_sum * complex_part1) / denominator)
print(paste("N4 is =",N4))



sigma_4_sq <- sigma^2 * rho3
sigma_3_sq <- sigma^2 * (rho2 - rho3)
sigma_2_sq <- sigma^2 * (rho1 - rho2)
sigma_e_sq <- sigma^2 - (sigma_2_sq + sigma_3_sq + sigma_4_sq)


count=0


itr=1000
for (it in 1:itr)
{  
  a<- c(rep(0, N3C), rep(1, N3T))
  X<-sample(a,size=N3,replace = FALSE)
  Xj<-rep(X,times=N4)
  ui <- rnorm(N4, mean = 0, sd = sqrt(sigma_4_sq))
  uj_i <- rnorm(N4 * N3, mean = 0, sd = sqrt(sigma_3_sq))
  uk_ij <- rnorm(N4 * N3 * N2, mean = 0, sd = sqrt(sigma_2_sq))
  e_ijkl <- rnorm(N4 * N3 * N2 * N1, mean = 0, sd = sqrt(sigma_e_sq))
  
 
  u_i=rep(ui,each=N3*N2*N1)
  
  u_ji=rep(uj_i,each=N2*N1)
  
  u_kij=rep(uk_ij,each=N1)
  treat=rep(Xj,each=N2*N1)
  
  y<- beta0 +zi * treat +u_i+u_ji+u_kij+e_ijkl
  
  
  hospital<-rep(c(1:(N4)),each=N3*N2*N1)
  clinics<-rep(rep(c(1:N3),each=N2*N1),times=N4)
  physician<-rep(rep(c(1:N2),each=N1),times=N4*N3)
  patient<-rep(rep(c(1:N1),each=1),times=N4*N3*N2)
  
  treat=rep(Xj,each=N2*N1)
  
  data<-cbind(y,hospital,clinics,physician,patient,treat)
  data<-data.frame(data)
  colnames(data)<-c("y","hospital","clinics","physician","patient","treat")
  rownames(data)<-NULL
  #View(data)
  
  library(nlme)
  ctrl <- lmeControl(opt='optim');
  model1<-lme(y ~treat,random= list( hospital=~1, clinics=~1, physician=~1,patient=~1), data=data, method= "REML",control=ctrl)
  summary(model1)
  bb=coef(summary(model1))
  
  
  zts<- bb[2, 4]
  
  
  if(qnorm(0.975, 0, 1)< abs(zts))
  {
    count= count+1
  }  
}

Type_1_error= formatC(count/itr, digits = 5, format = "f") 
Type_1_error

