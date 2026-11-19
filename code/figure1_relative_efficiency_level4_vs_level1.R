# ============================================================================
# Produces: Figure 1 (panels a-c): relative efficiency of randomization at level 4 vs level 1
# Run via main.R, or on its own from the package folder. See README.txt.
# ============================================================================
### --- First Plot: Varying N1 Values ---

N1_values <- c(2, 5, 10, 15)
N2 <- 2
N3 <- 2
rho2 <- 0.15
rho3 <- 0.05
rho1_seq <- seq(0.3, 1, by = 0.01)

line_types1 <- c(1, 2, 3, 5)  # solid, dashed, dotted, longdash

plot(rho1_seq, rep(NA, length(rho1_seq)), type = "n",
     xlim = c(0.3, 1), ylim = c(0, 1),
     xlab = "rho1",
     ylab = "Relative Efficiency_(3)",
     main = "Relative Efficiency_(3) vs rho1")

for (i in 1:length(N1_values)) {
  N1 <- N1_values[i]
  RE_values <- (1 - rho1_seq) / (1 + (N1 - 1) * rho1_seq + N1 * (N2 - 1) * rho2 + (N3 - 1) * N2 * N1 * rho3)
  lines(rho1_seq, RE_values, col = "black", lwd = 2, lty = line_types1[i])
}

legend("topright", legend = paste("N1 =", N1_values), col = "black", lwd = 2, lty = line_types1)



### --- Second Plot: Varying rho2 Values ---

N1 <- 2
rho2_values <- c(0.05, 0.15, 0.25)
N2 <- 2
N3 <- 2
rho3 <- 0.01
rho1_seq <- seq(0.3, 1, by = 0.01)

line_types2 <- c(1, 2, 3)  # solid, dashed, dotted

plot(rho1_seq, rep(NA, length(rho1_seq)), type = "n",
     xlim = c(0.3, 1), ylim = c(0, 1),
     xlab = "rho1",
     ylab = "Relative Efficiency_(3)",
     main = "Relative Efficiency_(3) vs rho1")

for (i in 1:length(rho2_values)) {
  rho2 <- rho2_values[i]
  RE_values <- (1 - rho1_seq) / (1 + (N1 - 1) * rho1_seq + N1 * (N2 - 1) * rho2 + (N3 - 1) * N2 * N1 * rho3)
  lines(rho1_seq, RE_values, col = "black", lwd = 2, lty = line_types2[i])
}

legend("topright", legend = paste("rho2 =", rho2_values), col = "black", lwd = 2, lty = line_types2)



### --- Third Plot: Varying rho3 Values ---

N1 <- 2
rho3_values <- c(0.05, 0.1, 0.15)
N2 <- 2
N3 <- 2
rho2 <- 0.2
rho1_seq <- seq(0.3, 1, by = 0.01)

line_types3 <- c(1, 2, 3)  # solid, dashed, dotted

plot(rho1_seq, rep(NA, length(rho1_seq)), type = "n",
     xlim = c(0.3, 1), ylim = c(0, 1),
     xlab = "rho1",
     ylab = "Relative Efficiency_(3)",
     main = "Relative Efficiency_(3) vs rho1")

for (i in 1:length(rho3_values)) {
  rho3 <- rho3_values[i]
  RE_values <- (1 - rho1_seq) / (1 + (N1 - 1) * rho1_seq + N1 * (N2 - 1) * rho2 + (N3 - 1) * N2 * N1 * rho3)
  lines(rho1_seq, RE_values, col = "black", lwd = 2, lty = line_types3[i])
}

legend("topright", legend = paste("rho3 =", rho3_values), col = "black", lwd = 2, lty = line_types3)
