# ============================================================================
# Produces: Figure 3 (2 panels): relative efficiency of randomization at level 3 vs level 1
# Run via main.R, or on its own from the package folder. See README.txt.
# ============================================================================
### --- First Plot: Varying N1 Values ---

# Define parameters
N1_values <- c(2, 5, 10, 15)
N2 <- 2
N3 <- 2
rho2 <- 0.15
rho3 <- 0.05
rho1_seq <- seq(0.3, 1, by = 0.01)

# Line types for each N1
line_types <- c(1, 2, 3, 5)  # solid, dashed, dotted, longdash

# Create an empty plot
plot(rho1_seq, rep(NA, length(rho1_seq)), type = "n",
     xlim = c(0.3, 1), ylim = c(0, 1),
     xlab = "rho1",
     ylab = "Relative Efficiency_(2)",
     main = "Relative Efficiency_(2) vs rho1")

# Compute and add lines for each N1
for (i in 1:length(N1_values)) {
  N1 <- N1_values[i]
  RE_values <- (1 - rho1_seq) / (1 + (N1 - 1) * rho1_seq + N1 * (N2 - 1) * rho2 - N2 * N1 * rho3)
  lines(rho1_seq, RE_values, col = "black", lwd = 2, lty = line_types[i])
}

# Add legend
legend("topright", legend = paste("N1 =", N1_values), col = "black", lwd = 2, lty = line_types)



### --- Second Plot: Varying rho2 Values ---

# Define parameters
N1 <- 2
rho2_values <- c(0.05, 0.15, 0.25)
N2 <- 2
N3 <- 2
rho3 <- 0.01
rho1_seq <- seq(0.3, 1, by = 0.01)

# Line types for each rho2
line_types <- c(1, 2, 3)  # solid, dashed, dotted

# Create an empty plot
plot(rho1_seq, rep(NA, length(rho1_seq)), type = "n",
     xlim = c(0.3, 1), ylim = c(0, 1),
     xlab = "rho1",
     ylab = "Relative Efficiency_(2)",
     main = "Relative Efficiency_(2) vs rho1")

# Compute and add lines for each rho2
for (i in 1:length(rho2_values)) {
  rho2 <- rho2_values[i]
  RE_values <- (1 - rho1_seq) / (1 + (N1 - 1) * rho1_seq + N1 * (N2 - 1) * rho2 - N2 * N1 * rho3)
  lines(rho1_seq, RE_values, col = "black", lwd = 2, lty = line_types[i])
}

# Add legend
legend("topright", legend = paste("rho2 =", rho2_values), col = "black", lwd = 2, lty = line_types)
