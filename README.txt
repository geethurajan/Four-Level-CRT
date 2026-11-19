This folder contains the R code to reproduce all tables and figures in the
article "Design and Power Considerations for Four-Level Cluster Randomized
Trials with Continuous Outcomes and Covariates" by Geethu Rajan, Priyanka
Majumder and Samiran Ghosh (Biometrical Journal, manuscript bimj.202600017)
and its Supporting Information.
All results come from closed-form formulas and Monte Carlo simulation; no real
data are used.



I. HOW TO RUN

Set the working directory to this folder and run

    Rscript main.R

main.R runs every script in code/, sets a seed before each one, and saves
its output to results/: the console output as <script>.txt, the figures as
<script>.pdf, a summary of all scripts as summary_all_scripts.csv, and
sessionInfo.txt.


II. MAIN FILES (code/)

figure1_..._level4_vs_level1.R = Figure 1 (relative efficiency, level 4 vs 1)
figure2_..._level2_vs_level1.R = Figure 2 (level 2 vs 1)
figure3_..._level3_vs_level1.R = Figure 3 (level 3 vs 1)
figure4_..._R42_R32_R43.R      = Figure 4 (a)-(c)
table1_power_randomization_level4.R          = Table 1
tableS1_type1_error_randomization_level4.R   = Table S1
tableS2/S3/S4_type1_error_randomization_level3/2/1.R = Tables S2, S3, S4
tableS5/S6/S7_power_randomization_level3/2/1.R       = Tables S5, S6, S7
tableS8/S9/S10/S11_type1_error_covariate.R           = Tables S8-S11
tableS12/S13/S14/S15_HTE_power_randomization_level4/3/2/1.R = Tables S12-S15
section8_ATE_vs_HTE_sample_size.R = sample sizes in Section 8

The level in a file name is the level at which the script randomizes.


III. TABLE ROWS

Each table script runs one row of its table. "Lines to edit" gives the line
numbers in the script where the design values of that row are set, e.g. for
table1, lines 9-12 hold N1, N3, N4T, N4C and line 20 holds Delta. To
reproduce another row, enter that row's values in these lines and rerun. The
lines in between (ICCs, calculated quantities) stay the same for the whole
table, and N2 or N4 is computed by the script. For Tables S12-S15, the
second range holds rho1-rho3 and alpha1-alpha3 (the ICC row of the table).

Script    Row as supplied                          Lines to edit
table1    N1=6,N3=10,N4T=8,N4C=7,Delta=0.4         9-12, 20
tableS1   N1=3,N3=3,N4T=9,N4C=9,Delta=0.4          9-12, 28
tableS2   N1=4,N2=4,N3T=6,N3C=4,Delta=0.6          9-12, 20
tableS3   N1=4,N3=10,N2T=4,N2C=3,Delta=0.6         9-12, 22
tableS4   N2=2,N3=4,N1T=5,N1C=1,Delta=0.4          9-12, 20
tableS5   N1=4,N2=9,N3T=7,N3C=3,Delta=0.6          9-11, 23 (N2 set in main.R)
tableS6   N1=8,N3=10,N2T=5,N2C=4,Delta=0.6         9-12, 20
tableS7   N2=8,N3=6,N1T=4,N1C=1,Delta=0.6          9-12, 20
tableS8   N1=10,N2=6,N3=5                          9-11
tableS9   N1=5,N2=8,N3=8                           9-11
tableS10  N1=8,N2=4,N3=6                           9-11
tableS11  N1=7,N2=16,N3=9                          9-11
tableS12  N1=4,N2=5,N3=6 (6th ICC row)             9-11, 15-20
tableS13  N1=7,N2=8,N3=4 (5th ICC row)             9-11, 13-18
tableS14  N1=10,N2=4,N3=9 (1st ICC row)            9-11, 12-17
tableS15  N1=4,N2=3,N3=8 (6th ICC row)             9-11, 12-17

Sample sizes and theoretical power match the paper exactly. Empirical power
and Type I error in Table 1 and Tables S1-S15 (1000 replicates) can differ
from the published values by Monte Carlo error (one standard error: about
0.013 at power 0.8, 0.007 at Type I error 0.05).


IV. SOFTWARE AND HARDWARE

R 4.4.1, nlme 3.1-164, Windows laptop (one core). Only package needed: nlme
(included with R). No code runs in parallel.
Runtime: each table script takes from a few minutes to under an hour on a
standard laptop; the figure scripts take seconds. main.R runs all scripts
and takes several hours.
