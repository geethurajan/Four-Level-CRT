# ============================================================================
# Produces: Section 8: sample size N4 for ATE vs HTE (N4 = 3 vs 14 at level 3; 2 vs 9 at level 2)
# Run via main.R, or on its own from the package folder. See README.txt.
# ============================================================================

# AT RANDOMIZATION AT LEVEL 3



alpha <- 0.05
beta <- 0.2
N1 <-4
N2<-5
N3<-10
N3T<-N3/2
Xbar<-0.5
r<-1
rho1 <- 0.4
rho2 <- 0.1
rho3 <- 0.05
sigma <- 1

sigma_w <-1
alpha1 <- 0.2
alpha2 <- 0.05
alpha3 <- 0.01


DeltaATE <- 0.4
DeltaHTE <- 0.1
Z_sum <-(qnorm(0.975, 0, 1) + qnorm(0.8, 0, 1))^2

complex_part1 <- 1 + (N1 - 1) * rho1 + N1 * (N2 - 1) * rho2 - N1 * N2 * rho3 

denominator <- (r / (r + 1)) * Delta^2 * N3T * N2 * N1


N4 <- ceiling((Z_sum * complex_part1) / denominator)

print(paste("N4 is =",N4))


lamda1 <-1-rho1

lamda2 <-1+(N1-1)*rho1-N1*rho2

lamda3 <-1+(N1-1)*rho1+N1*(N2-1)*rho2-N1*N2*rho3

lamda4 <-1+(N1-1)*rho1+N1*(N2-1)*rho2+N1*N2*(N3-1)*rho3

l1 <-1-alpha1

l2 <-1+(N1-1)*alpha1-N1*alpha2

l3 <-1+(N1-1)*alpha1+N1*(N2-1)*alpha2-N1*N2*alpha3

l4 <-1+(N1-1)*alpha1+N1*(N2-1)*alpha2+N1*N2*(N3-1)*alpha3

psi_1<- (lamda1*lamda2*lamda3*lamda4)/(N2*N1*lamda2*lamda3*lamda4-N2*lamda3*lamda4*(lamda2-lamda1)*(l2+N1*alpha2)-lamda1*lamda4*(lamda3-lamda2)*(l3+N1*N2*alpha3))


N4H <- ceiling((Z_sum * psi_1) / (DeltaHTE^2*N3*Xbar*(1-Xbar)*sigma_w^2))

print(paste("N4H is =",N4H))




# Randomization at level 2
alpha <- 0.05
beta <- 0.2
N1 <-5
N2<-10
N3<-6
N2T<-N2/2
Xbar<-0.5
r<-1
rho1 <- 0.4
rho2 <- 0.1
rho3 <- 0.05
sigma <- 1

sigma_w <-1
alpha1 <- 0.2
alpha2 <- 0.05
alpha3 <- 0.01


DeltaATE <- 0.4
DeltaHTE <- 0.1
Z_sum <-(qnorm(0.975, 0, 1) + qnorm(0.8, 0, 1))^2

complex_part1 <-(1-rho1)+(N1*(rho1-rho2))

denominator <- (r / (r + 1)) * Delta^2 * N3* N2T * N1



N4 <- ceiling((Z_sum * complex_part1) / denominator)

print(paste("N4 is =",N4))


lamda1 <-1-rho1

lamda2 <-1+(N1-1)*rho1-N1*rho2

lamda3 <-1+(N1-1)*rho1+N1*(N2-1)*rho2-N1*N2*rho3

lamda4 <-1+(N1-1)*rho1+N1*(N2-1)*rho2+N1*N2*(N3-1)*rho3

l1 <-1-alpha1

l2 <-1+(N1-1)*alpha1-N1*alpha2

l3 <-1+(N1-1)*alpha1+N1*(N2-1)*alpha2-N1*N2*alpha3

l4 <-1+(N1-1)*alpha1+N1*(N2-1)*alpha2+N1*N2*(N3-1)*alpha3

psi_1<- (lamda1*lamda2*lamda3*lamda4)/(N1*lamda2*lamda3*lamda4-lamda3*lamda4*(lamda2-lamda1)*(l2+N1*alpha2))



N4H <- ceiling((Z_sum * psi_1) / (DeltaHTE^2*N3*N2*Xbar*(1-Xbar)*sigma_w^2))

print(paste("N4H is =",N4H))



