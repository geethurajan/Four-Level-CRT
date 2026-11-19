################################################################################
# main.R
# Reproduces the figures and simulation results of
# "Design and Power Considerations for Four-Level Cluster Randomized Trials
#  with Continuous Outcomes and Covariates" (Biometrical Journal, bimj.202600017)
#
# Run from the folder that contains this file:
#     Rscript main.R
#
# This file only calls the scripts in code/ as they are. It does not change them.
# For each script it
#   - sets a random seed (so every run of main.R gives the same numbers),
#   - saves the console output to results/<script name>.txt,
#   - saves plots to results/<script name>.pdf (figure scripts),
#   - collects the sample size, theoretical power and empirical power / Type I
#     error into results/summary_all_scripts.csv.
#
# Each simulation script runs ONE design (one row of its table). Which row,
# and how to run the other rows, is explained in README.txt.
################################################################################

library(nlme)

dir.create("results", showWarnings = FALSE)

# script, seed, is it a figure, values the script expects to exist but does
# not define itself (N2 for Table S5, Delta for Section 8)
jobs <- list(
  list("figure1_relative_efficiency_level4_vs_level1.R", NA, TRUE, list()),
  list("figure2_relative_efficiency_level2_vs_level1.R", NA, TRUE, list()),
  list("figure3_relative_efficiency_level3_vs_level1.R", NA, TRUE, list()),
  list("figure4_relative_efficiency_R42_R32_R43.R", NA, TRUE, list()),
  list("table1_power_randomization_level4.R", 101, FALSE, list()),
  list("tableS1_type1_error_randomization_level4.R", 100, FALSE, list()),
  list("tableS2_type1_error_randomization_level3.R", 102, FALSE, list()),
  list("tableS3_type1_error_randomization_level2.R", 103, FALSE, list()),
  list("tableS4_type1_error_randomization_level1.R", 104, FALSE, list()),
  list("tableS5_power_randomization_level3.R", 105, FALSE, list(N2 = 9)),
  list("tableS6_power_randomization_level2.R", 106, FALSE, list()),
  list("tableS7_power_randomization_level1.R", 107, FALSE, list()),
  list("tableS8_type1_error_covariate.R", 108, FALSE, list()),
  list("tableS9_type1_error_covariate.R", 109, FALSE, list()),
  list("tableS10_type1_error_covariate.R", 110, FALSE, list()),
  list("tableS11_type1_error_covariate.R", 111, FALSE, list()),
  list("tableS12_HTE_power_randomization_level4.R", 112, FALSE, list()),
  list("tableS13_HTE_power_randomization_level3.R", 113, FALSE, list()),
  list("tableS14_HTE_power_randomization_level2.R", 114, FALSE, list()),
  list("tableS15_HTE_power_randomization_level1.R", 115, FALSE, list()),
  list("section8_ATE_vs_HTE_sample_size.R", NA, FALSE, list(Delta = 0.4))
)

# values to pick up from each script after it has run
keep <- c("N1", "N2", "N3", "N4", "N4T", "N4C", "N3T", "N3C", "N2T", "N2C",
          "N1T", "N1C", "Delta", "rho1", "rho2", "rho3", "alpha1", "alpha2",
          "alpha3", "th_power", "pow_Z", "Type_1_error", "N4H")

run_script <- function(script, seed, is_figure, inputs) {
  name <- sub("\\.R$", "", script)
  cat("Running", script, "\n")
  env <- new.env(parent = globalenv())
  for (v in names(inputs)) assign(v, inputs[[v]], envir = env)
  if (!is.na(seed)) set.seed(seed)
  if (is_figure) pdf(file.path("results", paste0(name, ".pdf")), width = 7, height = 6)
  log_con <- file(file.path("results", paste0(name, ".txt")), open = "wt")
  sink(log_con)
  t0 <- Sys.time()
  status <- tryCatch({
    source(file.path("code", script), local = env, print.eval = TRUE)
    "ok"
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
  minutes <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2)
  sink()
  close(log_con)
  if (is_figure) dev.off()
  cat("   ", status, "(", minutes, "min )\n")

  row <- data.frame(script = script, status = status, minutes = minutes, seed = seed)
  for (v in keep) {
    if (exists(v, envir = env, inherits = FALSE)) {
      x <- get(v, envir = env)
      if (length(x) == 1) row[[v]] <- as.character(x)
    }
  }
  row
}

summary_rows <- lapply(jobs, function(j) run_script(j[[1]], j[[2]], j[[3]], j[[4]]))

all_cols <- unique(unlist(lapply(summary_rows, names)))
summary_rows <- lapply(summary_rows, function(r) {
  r[setdiff(all_cols, names(r))] <- NA
  r[all_cols]
})
write.csv(do.call(rbind, summary_rows), file.path("results", "summary_all_scripts.csv"),
          row.names = FALSE)

cat("\nDone. Results are in the results/ folder.\n")
writeLines(capture.output(sessionInfo()), file.path("results", "sessionInfo.txt"))
