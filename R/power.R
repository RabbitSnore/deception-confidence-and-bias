################################################################################

# Confidence and Bias -- Power Analysis

################################################################################

# This script assumes you have already run analyze.R

# Set up environment -----------------------------------------------------------

library(simr)

# Simulation -------------------------------------------------------------------

fixed <- fixef(judge_fixed_conf_int_2)

es <- seq(.20, 1.20, .20)

pwr <- NA

for(i in 1:length(es)){

  fixed["contentquarrel:confidence_condition"] <- es[i]
  
  sim_model <- makeGlmer(judgment ~ (veracity + content + confidence_condition)^2 + (1|id) + (1|sender), 
                         family = binomial(link = "logit"),
                         fixef = fixed,
                         VarCorr = VarCorr(judge_fixed_conf_int_2),
                         data = judge_long)
  
  pwr[i] <- round(
    as.numeric(
      summary(
        powerSim(sim_model, 
                 test = fixed("contentquarrel:confidence_condition"), 
                 nsim = 250)
        ))[3],
      3)
  
  }

power_data <- data.frame("Log Odds" = es, "Power" = pwr)

if (!file.exists("./RDS/power-data.rds")) {
  
  save(power_data, file = "./RDS/power-data.rds")
  
}
