# ============================================================================
# Produces: Table 1: sample size N2, theoretical and empirical power, randomization at level 4
# This script runs one row of the table; README.txt, Section III, says which
# row and which lines to edit for the other rows.
# Run via main.R, or on its own from the package folder. See README.txt.
# ============================================================================
alpha <- 0.05 # The level of significance
beta <- 0.2 # probability   Type II error
N1 <-6 # Number of level one unit (eg, patient) 
N3<-10 # Number of level three unit (eg, clinic )
N4T<-8 # Number of level  four units assigned to the experimental
N4C<-7 # Number of level  four units assigned to the control arm
N4 <-N4T+N4C # Number of level four unit (eg, hospital)
r<-N4C/N4T 
rho1 <- 0.4 # The intra-cluster correlation coefficients (ICC) at level one data
rho2 <- 0.1 # The intra-cluster correlation coefficients (ICC) at level two data
rho3 <- 0.05 # The intra-cluster correlation coefficients (ICC) at level three data
sigma <- 1  # Variance
beta0 <- 0 # Model parameters
Delta <- 0.4 # The standardized effect size
zi <- Delta * sigma # Intervention effect

Z_sum <-(qnorm(0.975, 0, 1) + qnorm(0.8, 0, 1))^2

complex_part1 <- (1 - rho1 + N1 * (rho1 - rho2))

denominator <- ((r / (r + 1)) * (Delta^2) * N4T * N3 * N1) - (Z_sum * ((N1 * rho2 )+ ((N3 - 1) * N1 * rho3)))


N2 <- ceiling((Z_sum * complex_part1) / (denominator))

print(paste("N2 is =",N2)) # Number of level two unit (eg, physician)




complex_part2 <-1 + (N1 - 1) * rho1 + N1 * (N2 - 1) * rho2 +(N3-1)* N1 * N2 * rho3 
psi_4 <- complex_part2 / (N3 * N4T * N2 * N1)


# Theoretical power formula

th_power <-pnorm((Delta / sqrt(psi_4)) * sqrt(r) / sqrt(r + 1) -qnorm(0.975, 0, 1) ) # Theoretical power formula

print(paste("theoretical power is =",th_power))



sigma_4_sq <- sigma^2 * rho3
sigma_3_sq <- sigma^2 * (rho2 - rho3)
sigma_2_sq <- sigma^2 * (rho1 - rho2)
sigma_e_sq <- sigma^2 - (sigma_2_sq + sigma_3_sq + sigma_4_sq)



count_Z=0

itr=1000
for (it in 1:itr)
{  
  a<- c(rep(0, N4C), rep(1, N4T))
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
  
  y<- beta0 +zi * treat +u_i+u_ji+u_kij+e_ijkl
  
  hospital<-rep(c(1:(N4)),each=N3*N2*N1)
  clinics<-rep(rep(c(1:N3),each=N2*N1),times=N4)
  physician<-rep(rep(c(1:N2),each=N1),times=N4*N3)
  patient<-rep(rep(c(1:N1),each=1),times=N4*N3*N2)
  treat=rep(Xi,each=N3*N2*N1)
  
  data<-cbind(y,hospital,clinics,physician,patient,treat)
  data<-data.frame(data)
  colnames(data)<-c("y","hospital","clinics","physician","patient","treat")
  rownames(data)<-NULL
  #View(data)
  
  library(nlme)
  ctrl <- lmeControl(opt='optim');
  model1<-lme(y ~treat , random= list( hospital=~1, clinics=~1, physician=~1,patient=~1), data=data, method= "REML",control=ctrl)
  summary(model1)
  bb=coef(summary(model1))
  zts<- bb[2, 4]
  pval= 2*(1-pnorm(abs(zts), mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE))
  
  if(pval< alpha)
  {
    count_Z= count_Z+1
  }  
  
}

pow_Z= formatC(count_Z/itr, digits = 5, format = "f") # Empirical power for Z test
pow_Z 



