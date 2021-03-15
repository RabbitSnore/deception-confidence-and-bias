#######################################################################

# Confidence and Bias -- Loading and wrangling

#######################################################################

# Does sender confidence influence bias? ------------------------------

## Raw judgments

### A base model with only random effects and veracity condition

judge_model_base <- glmer(judgment ~ veracity + (1|id) + (1|content/sender), family = binomial(link = "logit"),  data = judge_long)

### Adding sender confidence

judge_model_conf <- glmer(judgment ~ veracity + confidence_condition + (1|id) + (1|content/sender), family = binomial(link = "logit"),  data = judge_long)

### Compare models

lrt_judge <- anova(judge_model_base, judge_model_conf, test = "LRT")
# It seems that the LRT supports retaining the model with confidence condition included, and it seems confidence condition increases the tendency to judge messages as true. There is quite a bit of variance in the tendency for different senders to be judged as telling the truth and different content to be judged as truthful. There is less -- but still some -- variance in receivers' tendency to judge messages as truthful.

### Mean predicted judgment rates

judge_long$pred <- predict(judge_model_conf, type = "response")

#### By condition

judge_rates_cond <- 
judge_long %>% 
  group_by(confidence_condition) %>% 
  summarise(
    mean = mean(pred, na.rm = TRUE)
  )

#### By sender

judge_rates_sender <- 
  judge_long %>% 
  group_by(sender) %>% 
  summarise(
    mean = mean(pred, na.rm = TRUE)
  )

#### By content

judge_rates_content <- 
  judge_long %>% 
  group_by(content) %>% 
  summarise(
    mean = mean(pred, na.rm = TRUE)
  )

## Signal detection (c)

sdt_model_base <- lm(c ~ confidence_condition,  data = sdt_data)
# The signal detection approach supports the analysis of raw judgments.

