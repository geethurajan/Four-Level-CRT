# ============================================================================
# Produces: Figure 4 (a)-(c): relative efficiencies R(4,2), R(3,2), R(4,3)
# Run via main.R, or on its own from the package folder. See README.txt.
# ============================================================================
# Define line types for distinction
line_types <- c(1, 2, 3, 5)  # solid, dashed, dotted, longdash


### --- R42 ---
N1_values <- c(2, 5, 15, 30)
N2 <- 2
N3 <- 3
rho2 <- 0.15
rho3 <- 0.05
rho1_seq <- seq(0.3, 1, by = 0.01)

plot(rho1_seq, rep(NA, length(rho1_seq)), type = "n",
     xlim = c(0.3, 1), ylim = c(0, 1),
     xlab = "rho1",
     ylab = "Relative Efficiency_(42)",
     main = "Relative Efficiency_(42) vs rho1")

for (i in 1:length(N1_values)) {
  N1 <- N1_values[i]
  RE_values <- (1 + (N1 - 1) * rho1_seq - N1 * rho2) / 
    (1 + (N1 - 1) * rho1_seq + N1 * (N2 - 1) * rho2 + (N3 - 1) * N2 * N1 * rho3)
  lines(rho1_seq, RE_values, col = "black", lwd = 2, lty = line_types[i])
}

legend("topright", legend = paste("N1 =", N1_values), col = "black", lwd = 2, lty = line_types)


### --- R32 ---
N1_values <- c(2, 5, 15, 30)
N2 <- 4
N3 <- 3
rho2 <- 0.15
rho3 <- 0.05
rho1_seq <- seq(0.3, 1, by = 0.01)

plot(rho1_seq, rep(NA, length(rho1_seq)), type = "n",
     xlim = c(0.3, 1), ylim = c(0, 1),
     xlab = "rho1",
     ylab = "Relative Efficiency_(32)",
     main = "Relative Efficiency_(32) vs rho1")

for (i in 1:length(N1_values)) {
  N1 <- N1_values[i]
  RE_values <- (1 + (N1 - 1) * rho1_seq - N1 * rho2) / 
    (1 + (N1 - 1) * rho1_seq + N1 * (N2 - 1) * rho2 - N2 * N1 * rho3)
  lines(rho1_seq, RE_values, col = "black", lwd = 2, lty = line_types[i])
}

legend("topright", legend = paste("N1 =", N1_values), col = "black", lwd = 2, lty = line_types)


### --- R43 ---
N1_values <- c(2, 5, 10, 20)
N2 <- 2
N3 <- 3
rho2 <- 0.1
rho3 <- 0.05
rho1_seq <- seq(0.3, 1, by = 0.01)

plot(rho1_seq, rep(NA, length(rho1_seq)), type = "n",
     xlim = c(0.3, 1), ylim = c(0, 1),
     xlab = "rho1",
     ylab = "Relative Efficiency_(43)",
     main = "Relative Efficiency_(43) vs rho1")

for (i in 1:length(N1_values)) {
  N1 <- N1_values[i]
  RE_values <- (1 + (N1 - 1) * rho1_seq + N1 * (N2 - 1) * rho2 - N2 * N1 * rho3) / 
    (1 + (N1 - 1) * rho1_seq + N1 * (N2 - 1) * rho2 + (N3 - 1) * N2 * N1 * rho3)
  lines(rho1_seq, RE_values, col = "black", lwd = 2, lty = line_types[i])
}

legend("bottomright", legend = paste("N1 =", N1_values), col = "black", lwd = 2, lty = line_types, bty = "n")
