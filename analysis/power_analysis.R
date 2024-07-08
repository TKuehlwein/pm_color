#grab median and sd for each group for design
m_sd_trial_load

#define the 2x3 within design with 300 n and the data taken from pilot
design_result <- ANOVA_design(design = "3w*2w",
                              n = 300, 
                              mu = c(10, 9.57, 11, 12.3, 16.3, 16.8), 
                              sd = c(17.5, 19.4, 21, 24.4, 34, 36.5),
                              r = 0.8, 
                              label_list = list("load"  = c("1", "2", "3"),
                                                "block" = c( "base", "pm")),
                              plot = TRUE)

#calculate the power based on the design with alpha=.05 and 200 simulations
power_result_vig_1 <- ANOVA_power(design_result, 
                                  alpha = 0.05, 
                                  nsims = 200, 
                                  seed = 1234)

#plot potential power depending on participants with previous design
> plot_power(design_result, min_n = 100, max_n = 400)
