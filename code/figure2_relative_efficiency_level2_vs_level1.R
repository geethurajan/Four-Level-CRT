# ============================================================================
# Produces: Figure 2: relative efficiency of randomization at level 2 vs level 1
# Run via main.R, or on its own from the package folder. See README.txt.
# ============================================================================
# Define parameters
N1_values <- c(2, 5, 10, 15)
N2 <- 2
N3 <- 2

# Ensure correct order: rho1 > rho2 > rho3
rho2 <- 0.15
rho3 <- 0.05
rho1_seq <- seq(0.3, 1, by = 0.01)

# Line types for distinguishable black lines
line_types <- c(1, 2, 3, 4)  # solid, dashed, dotted, longdash

# Create an empty plot
plot(rho1_seq, rep(NA, length(rho1_seq)), type = "n", 
     xlim = c(0.3, 1), ylim = c(0, 1), 
     xlab = "rho1", 
     ylab = "Relative Efficiency_(1)", 
     main = "Relative Efficiency_(1) vs rho1")

# Compute and add lines for each N1
for (i in 1:length(N1_values)) {
  N1 <- N1_values[i]
  
  # Compute Relative Efficiency
  RE_values <- (1 - rho1_seq) / (1 + (N1 - 1) * rho1_seq - N1 * rho2)
  
  # Add line to the plot
  lines(rho1_seq, RE_values, col = "black", lwd = 2, lty = line_types[i])
}

# Add legend
legend("topright", legend = paste("N1 =", N1_values), 
       col = "black", lwd = 2, lty = line_types)
