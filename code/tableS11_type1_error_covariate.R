# ============================================================================
# Produces: Table S11: sample size N4 and empirical Type I error, model with covariate (HTE)
# This script runs one row of the table; README.txt, Section III, says which
# row and which lines to edit for the other rows.
# Run via main.R, or on its own from the package folder. See README.txt.
# ============================================================================
alpha <- 0.05
beta <- 0.2
N1 <-7
N2<-16
N3<-9


rho1 <- 0.4
rho2 <- 0.1
rho3 <- 0.05
alpha1 <- 0.5
alpha2 <- 0.3
alpha3 <- 0.1
sigma <- 1
sigma_w <-1
beta0 <- 1
zi<-0.2
delta<-0.3
Xbar<-0.5
muw <-1

Delta <- 0.1
tau <-0

Z_sum <-(qnorm(0.975, 0, 1) + qnorm(0.8, 0, 1))^2

lamda1 <-1-rho1

lamda2 <-1+(N1-1)*rho1-N1*rho2

lamda3 <-1+(N1-1)*rho1+N1*(N2-1)*rho2-N1*N2*rho3

lamda4 <-1+(N1-1)*rho1+N1*(N2-1)*rho2+N1*N2*(N3-1)*rho3

l1 <-1-alpha1

l2 <-1+(N1-1)*alpha1-N1*alpha2

l3 <-1+(N1-1)*alpha1+N1*(N2-1)*alpha2-N1*N2*alpha3

l4 <-1+(N1-1)*alpha1+N1*(N2-1)*alpha2+N1*N2*(N3-1)*alpha3

psi_1<- (lamda1*lamda2*lamda3*lamda4)/(N3*N2*N1*lamda2*lamda3*lamda4-N3*N2*lamda3*lamda4*(lamda2-lamda1)*(l2+N1*alpha2)-N3*lamda1*lamda4*(lamda3-lamda2)*(l3+N1*N2*alpha3)-lamda1*lamda2*(lamda3-lamda4)*l4)


N4 <- ceiling((Z_sum * psi_1) / (Delta^2*Xbar*(1-Xbar)*sigma_w^2))

print(paste("N4 is =",N4))





sigma_4_sq <- sigma^2 * rho3
sigma_3_sq <- sigma^2 * (rho2 - rho3)
sigma_2_sq <- sigma^2 * (rho1 - rho2)
sigma_e_sq <- sigma^2 - (sigma_2_sq + sigma_3_sq + sigma_4_sq)



sigma_4W_sq <- sigma_w^2 * alpha3
sigma_3W_sq <- sigma_w^2 * (alpha2 - alpha3)
sigma_2W_sq <- sigma_w^2 * (alpha1 - alpha2)
sigma_eW_sq <- sigma_w^2 - (sigma_2W_sq + sigma_3W_sq + sigma_4W_sq)




count=0

itr=1000
for (it in 1:itr)
{  
  a<- c(rep(0, N4/2), rep(1, N4/2))
  Xi<-sample(a,size=N4,replace = FALSE)
  
  ui <- rnorm(N4, mean = 0, sd = sqrt(sigma_4_sq))
  uj_i <- rnorm(N4 * N3, mean = 0, sd = sqrt(sigma_3_sq))
  uk_ij <- rnorm(N4 * N3 * N2, mean = 0, sd = sqrt(sigma_2_sq))
  e_ijkl <- rnorm(N4 * N3 * N2 * N1, mean = 0, sd = sqrt(sigma_e_sq))
  
  # Initialize Yijkl vector
  u_i=rep(ui,each=N3*N2*N1)
  
  u_ji=rep(uj_i,each=N2*N1)
  
  u_kij=rep(uk_ij,each=N1)
  treat=rep(Xi,each=N3*N2*N1)
  
  
  
  ai <- rnorm(N4, mean = 0, sd = sqrt(sigma_4W_sq))
  bj_i <- rnorm(N4 * N3, mean = 0, sd = sqrt(sigma_3W_sq))
  ck_ij <- rnorm(N4 * N3 * N2, mean = 0, sd = sqrt(sigma_2W_sq))
  d_ijkl <- rnorm(N4 * N3 * N2 * N1, mean = 0, sd = sqrt(sigma_eW_sq))
  
  
  a_i=rep(ai,each=N3*N2*N1)
  
  b_ji=rep(bj_i,each=N2*N1)
  
  c_kij=rep(ck_ij,each=N1)
  
  w_ijkl <- muw+a_i+b_ji+c_kij+ d_ijkl
  
  
  
  y<- beta0 +zi * treat +delta*w_ijkl+tau*treat*w_ijkl+ u_i+u_ji+u_kij+e_ijkl
  
  
  
  
  hospital<-rep(c(1:(N4)),each=N3*N2*N1)
  clinics<-rep(rep(c(1:N3),each=N2*N1),times=N4)
  physician<-rep(rep(c(1:N2),each=N1),times=N4*N3)
  patient<-rep(rep(c(1:N1),each=1),times=N4*N3*N2)
  treat=rep(Xi,each=N3*N2*N1)
  w= w_ijkl
  
  
  data<-cbind(y,hospital,clinics,physician,patient,treat,w)
  data<-data.frame(data)
  colnames(data)<-c("y","hospital","clinics","physician","patient","treat","w")
  rownames(data)<-NULL
  #View(data)
  
  library(nlme)
  ctrl <- lmeControl(opt='optim');
  model1<-lme(y ~   treat + w + treat:w , random= list( hospital=~1, clinics=~1, physician=~1,patient=~1), data=data, method= "REML",control=ctrl)
  summary(model1)
  bb=coef(summary(model1))
  
  zts<- bb[4, 4]
  
  
  if(qnorm(0.975, 0, 1)< abs(zts))
  {
    count= count+1
  }  
  
  
  
}

Type_1_error= formatC(count/itr, digits = 5, format = "f") 
Type_1_error
