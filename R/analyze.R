#######################################################################

# Confidence and Bias -- Loading and wrangling

#######################################################################

# Set up enviroment ---------------------------------------------------

set.seed(1138)

# Does sender confidence influence bias? ------------------------------

## Raw judgments

### A base model with only random effects and veracity condition

judge_model_base <- glmer(judgment ~ veracity + (1|id) + (1|content/sender), family = binomial(link = "logit"),  data = judge_long)

### Adding sender confidence

judge_model_conf <- glmer(judgment ~ veracity + confidence_condition + (1|id) + (1|content/sender), family = binomial(link = "logit"),  data = judge_long)

### Random slopes for the interaction between confidence condition and content

judge_model_conf_int <- glmer(judgment ~ veracity + confidence_condition + (1|id) + (1 + confidence_condition|content) + (1|content:sender), family = binomial(link = "logit"),  data = judge_long)

### Compare models

lrt_judge <- anova(judge_model_base, judge_model_conf, judge_model_conf_int, test = "LRT")
# Interestingly the model with random slopes fits the data the best, even though none of the fixed effects are significant. In the model with only random intercepts, we see that there is a tendency for sender confidence to increase truth judgments on average, but adding the interaction substantially increases the standard error for this coefficient. Thus, we see some evidence for an interaction between content type and confidence condition, such that sender confidence has effects of different sizes and/or directions depending on the content of the message.

### Mean predicted judgment rates

judge_long$pred <- predict(judge_model_conf_int, type = "response")

#### Bootstrapping functions

pred_cond <- function(x) {
  
  judge_long$pred_temp <- predict(x, type = "response")
  
  out <- c(low = mean(judge_long[judge_long$confidence_condition == 0, ]$pred_temp, na.rm = TRUE),
           high = mean(judge_long[judge_long$confidence_condition == 1, ]$pred_temp, na.rm = TRUE))
  
  return(out)
  
}

pred_sender <- function(x) {
  
  judge_long$pred_temp <- predict(x, type = "response")
  
  out <- c(n1 = mean(judge_long[judge_long$sender == "n1", ]$pred_temp, na.rm = TRUE),
           n2 = mean(judge_long[judge_long$sender == "n2", ]$pred_temp, na.rm = TRUE),
           n3 = mean(judge_long[judge_long$sender == "n3", ]$pred_temp, na.rm = TRUE),
           n4 = mean(judge_long[judge_long$sender == "n4", ]$pred_temp, na.rm = TRUE),
           n5 = mean(judge_long[judge_long$sender == "n5", ]$pred_temp, na.rm = TRUE),
           n6 = mean(judge_long[judge_long$sender == "n6", ]$pred_temp, na.rm = TRUE),
           n7 = mean(judge_long[judge_long$sender == "n7", ]$pred_temp, na.rm = TRUE),
           n8 = mean(judge_long[judge_long$sender == "n8", ]$pred_temp, na.rm = TRUE)
           )
  
  return(out)
  
}

pred_cont <- function(x) {
  
  judge_long$pred_temp <- predict(x, type = "response")
  
  out <- c(accident = mean(judge_long[judge_long$content == "accident", ]$pred_temp, na.rm = TRUE),
           bereavement = mean(judge_long[judge_long$content == "bereavement", ]$pred_temp, na.rm = TRUE),
           holiday = mean(judge_long[judge_long$content == "holiday", ]$pred_temp, na.rm = TRUE),
           quarrel = mean(judge_long[judge_long$content == "quarrel", ]$pred_temp, na.rm = TRUE)
           )
  
  return(out)
  
}

pred_inter <- function(x) {
  
  judge_long$pred_temp <- predict(x, type = "response")
  
  out <- c(accident_low    = mean(judge_long[judge_long$content == "accident" & judge_long$confidence_condition == 0, ]$pred_temp, na.rm = TRUE),
           bereavement_low = mean(judge_long[judge_long$content == "bereavement" & judge_long$confidence_condition == 0, ]$pred_temp, na.rm = TRUE),
           holiday_low     = mean(judge_long[judge_long$content == "holiday" & judge_long$confidence_condition == 0, ]$pred_temp, na.rm = TRUE),
           quarrel_low     = mean(judge_long[judge_long$content == "quarrel" & judge_long$confidence_condition == 0, ]$pred_temp, na.rm = TRUE),
           accident_high    = mean(judge_long[judge_long$content == "accident" & judge_long$confidence_condition == 1, ]$pred_temp, na.rm = TRUE),
           bereavement_high = mean(judge_long[judge_long$content == "bereavement" & judge_long$confidence_condition == 1, ]$pred_temp, na.rm = TRUE),
           holiday_high     = mean(judge_long[judge_long$content == "holiday" & judge_long$confidence_condition == 1, ]$pred_temp, na.rm = TRUE),
           quarrel_high     = mean(judge_long[judge_long$content == "quarrel" & judge_long$confidence_condition == 1, ]$pred_temp, na.rm = TRUE)
  )
  
  return(out)
  
}

##### Bootstrap confidence intervals (95% percentile CIs)

bootstrap_ci <- function(boot_out) {
  
  require(boot)
  
  n_rows <- length(boot_out$t0)
  
  out <- data.frame(
    estimate = boot_out$t0,
    ci_lb = rep(NA, n_rows),
    ci_ub = rep(NA, n_rows)
  )
  
  for (i in 1:n_rows) {
    
    ci_temp <- boot.ci(boot_out, index = c(i, i), type = "perc")$percent
    
    out[i, 2:3] <- ci_temp[(length(ci_temp) - 1):length(ci_temp)]
    
  }
  
  return(out)
  
}

#### By condition

judge_rates_cond <- 
judge_long %>% 
  group_by(confidence_condition) %>% 
  summarise(
    mean  = mean(pred, na.rm = TRUE)
  )

if (file.exists("./RDS/boot_cond.rds")) {
  
  boot_cond <- readRDS("./RDS/boot_cond.rds")

  } else {
  
  boot_cond <- bootMer(judge_model_conf_int, pred_cond, nsim = 500, parallel = "multicore")
    # Note that the bootstrapping will take a quite long time with this many simulations, unless you've got a supercomputer lying around.
  
  saveRDS(boot_cond, "./RDS/boot_cond.rds")

}

boot_ci_cond <- bootstrap_ci(boot_cond)

#### By sender

judge_rates_sender <- 
  judge_long %>% 
  group_by(sender) %>% 
  summarise(
    mean = mean(pred, na.rm = TRUE)
  )

if (file.exists("./RDS/boot_sender.rds")) {
  
  boot_sender <- readRDS("./RDS/boot_sender.rds")
  
} else {
  
  boot_sender <- bootMer(judge_model_conf_int, pred_sender, nsim = 500, parallel = "multicore")
  
  saveRDS(boot_sender, "./RDS/boot_sender.rds")
  
}

boot_ci_sender <- bootstrap_ci(boot_sender)

#### By content

judge_rates_content <- 
  judge_long %>% 
  group_by(content) %>% 
  summarise(
    mean = mean(pred, na.rm = TRUE)
  )

if (file.exists("./RDS/boot_cont.rds")) {
  
  boot_cont <- readRDS("./RDS/boot_cont.rds")
  
} else {
  
  boot_cont <- bootMer(judge_model_conf_int, pred_cont, nsim = 500, parallel = "multicore")
  
  saveRDS(boot_cont, "./RDS/boot_cont.rds")
  
}

boot_ci_cont <- bootstrap_ci(boot_cont)

#### By condition and content

judge_rates_cond_cont <- 
  judge_long %>% 
  group_by(confidence_condition, content) %>% 
  summarise(
    mean  = mean(pred, na.rm = TRUE)
  )

if (file.exists("./RDS/boot_inter.rds")) {
  
  boot_inter <- readRDS("./RDS/boot_inter.rds")
  
} else {
  
  boot_inter <- bootMer(judge_model_conf_int, pred_inter, nsim = 500, parallel = "multicore")
  
  saveRDS(boot_inter, "./RDS/boot_inter.rds")
  
}

boot_ci_inter <- bootstrap_ci(boot_inter)

## Signal detection (c)

sdt_model_base <- lm(c ~ confidence_condition,  data = sdt_data)
# The signal detection approach supports the analysis of raw judgments, at least insofar as there seems to be an increase (on average) in truth bias across the narrative types.

## Modeling narrative content as a fixed effect

# One way to approach this analysis is to model the message content as a fixed effect instead of a random effect. This approach is arguably more conventional, and it seems likely that most readers will find this approach more intuitive.

judge_long$content <- factor(judge_long$content, levels = c("holiday", "bereavement", "accident", "quarrel")) # Holiday messages are set to be the reference group. That way, the most emotionally neutral content is the "baseline" against which the others are compared.

### A base model with only random effects, veracity condition, and content

judge_fixed_base <- glmer(judgment ~ veracity + content + (1|id) + (1|sender), family = binomial(link = "logit"),  data = judge_long)

### Adding sender confidence

judge_fixed_conf <- glmer(judgment ~ veracity + content + confidence_condition + (1|id) + (1|sender), family = binomial(link = "logit"),  data = judge_long)

### Adding the interaction between confidence condition and content

judge_fixed_conf_int <- glmer(judgment ~ veracity + content * confidence_condition + (1|id) + (1|sender), family = binomial(link = "logit"),  data = judge_long)

### Compare models

lrt_judge_fixed <- anova(judge_fixed_base, judge_fixed_conf, judge_fixed_conf_int, test = "LRT")

## Mean predicted truth judgment rates

### Add predictions

judge_long$pred_fixed <- predict(judge_fixed_conf_int, type = "response")

### By condition

judge_rates_fixed_cond <- 
  judge_long %>% 
  group_by(confidence_condition) %>% 
  summarise(
    mean  = mean(pred_fixed, na.rm = TRUE)
  )

if (file.exists("./RDS/boot_fixed_cond.rds")) {
  
  boot_fixed_cond <- readRDS("./RDS/boot_fixed_cond.rds")
  
} else {
  
  boot_fixed_cond <- bootMer(judge_fixed_conf_int, pred_cond, nsim = 500, parallel = "multicore")
  
  saveRDS(boot_fixed_cond, "./RDS/boot_fixed_cond.rds")
  
}

boot_ci_fixed_cond <- bootstrap_ci(boot_fixed_cond)

### By sender

judge_rates_fixed_sender <- 
  judge_long %>% 
  group_by(sender) %>% 
  summarise(
    mean = mean(pred_fixed, na.rm = TRUE)
  )

if (file.exists("./RDS/boot_fixed_sender.rds")) {
  
  boot_fixed_sender <- readRDS("./RDS/boot_fixed_sender.rds")
  
} else {
  
  boot_fixed_sender <- bootMer(judge_fixed_conf_int, pred_sender, nsim = 500, parallel = "multicore")
  
  saveRDS(boot_fixed_sender, "./RDS/boot_fixed_sender.rds")
  
}

boot_ci_fixed_sender <- bootstrap_ci(boot_fixed_sender)

### By content

judge_rates_fixed_content <- 
  judge_long %>% 
  group_by(content) %>% 
  summarise(
    mean = mean(pred_fixed, na.rm = TRUE)
  )

if (file.exists("./RDS/boot_fixed_cont.rds")) {
  
  boot_fixed_cont <- readRDS("./RDS/boot_fixed_cont.rds")
  
} else {
  
  boot_fixed_cont <- bootMer(judge_fixed_conf_int, pred_cont, nsim = 500, parallel = "multicore")
  
  saveRDS(boot_fixed_cont, "./RDS/boot_fixed_cont.rds")
  
}

boot_ci_fixed_cont <- bootstrap_ci(boot_fixed_cont)

### By condition and content

judge_rates_fixed_cond_cont <- 
  judge_long %>% 
  group_by(confidence_condition, content) %>% 
  summarise(
    mean  = mean(pred_fixed, na.rm = TRUE)
  )

if (file.exists("./RDS/boot_fixed_inter.rds")) {
  
  boot_fixed_inter <- readRDS("./RDS/boot_fixed_inter.rds")
  
} else {
  
  boot_fixed_inter <- bootMer(judge_fixed_conf_int, pred_inter, nsim = 500, parallel = "multicore")
  
  saveRDS(boot_fixed_inter, "./RDS/boot_fixed_inter.rds")
  
}

boot_ci_fixed_inter <- bootstrap_ci(boot_fixed_inter)
