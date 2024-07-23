#grab median and sd for each group for design
m_sd_trial_load

#define the 2x3 within design with 300 n and the data taken from pilot
design_result <- ANOVA_design(design = "3w*2w",
                              n = 450, 
                              mu = c(10, 10.5, 11, 12.5, 13.5, 14), 
                              sd = c(6, 6.5, 5, 7, 8, 8.5),
                              r = 0.8, 
                              label_list = list("load"  = c("1", "2", "3"),
                                                "block" = c( "base", "pm")),
                              plot = TRUE)

#calculate the power based on the design with alpha=.05 and X simulations
power_result_vig_1 <- ANOVA_power(design_result, 
                                  alpha = 0.02, 
                                  nsims = 5000, 
                                  seed = 1234)
#run a ANOVA with the exact numbers given design_result
result_exact <- ANOVA_exact(design_result)

#plot potential power depending on participants with previous design
plot_power(design_result, min_n = 100, max_n = 400)




design_result <- ANOVA_design(design = "3w*2w",
                              n = 300, 
                              mu = c(10, 9.57, 11, 12.3, 16.3, 16.8), 
                              sd = c(17.5, 19.4, 21, 24.4, 34, 36.5),
                              r = 0.9, 
                              label_list = list("load"  = c("1", "2", "3"),
                                                "block" = c( "base", "pm")),
                              plot = TRUE)
