#######################################################################

# Confidence and Bias -- Loading and wrangling

#######################################################################

# Set up environment --------------------------------------------------

packages <- c("tidyverse", "readxl", "lme4", "psycho")

lapply(packages, library, character.only = TRUE)

## Import data

raw <- read_xlsx("confidence_emotion_data.xlsx")

# Basic clean up ------------------------------------------------------

## All column names to lowercase

colnames(raw) <- tolower(colnames(raw))

## Clean up content names

colnames(raw) <- gsub("car_accident", "accident", colnames(raw))

# Wrangling -----------------------------------------------------------

## Long form judgments

judge_long <- raw %>% 
  pivot_longer(
    cols = starts_with("raw"),
    names_to = "name",
    values_to = "judgment"
  ) %>% 
  extract("name", c("sender", "content", "veracity"), "raw_(.*)_(.*)_(.)")

## Long form accuracy

accuracy_long <- raw %>% 
  pivot_longer(
    cols = starts_with("acc"),
    names_to = "name",
    values_to = "accuracy"
  ) %>% 
  extract("name", c("sender", "content", "veracity"), "acc_(.*)_(.*)_(.)")

## Long form receiver confidence, to add to accuracy data

confidence_long <- raw %>% 
  pivot_longer(
    cols = starts_with("conf_"),
    names_to = "name",
    values_to = "confidence"
  ) %>% 
  extract("name", c("sender", "content", "veracity"), "conf_(.*)_(.*)_(.)") %>% 
  select(id, sender, content, veracity, confidence)

accuracy_long <- accuracy_long %>% 
  left_join(confidence_long, by = c("id", "sender", "content", "veracity"))

## Signal detection

### Hits, false alarms, misses, correct rejections

signal_long <- accuracy_long %>% 
  mutate(
    hit = case_when(
      veracity == "f" & accuracy == 1   ~ 1,
      veracity == "f" & accuracy == 0   ~ 0,
      veracity == "t"                   ~ 0
    ),
    fa = case_when(
      veracity == "t" & accuracy == 0  ~ 1,
      veracity == "t" & accuracy == 1  ~ 0,
      veracity == "f"                  ~ 0
    ),
    miss = case_when(
      veracity == "f" & accuracy == 0   ~ 1,
      veracity == "f" & accuracy == 1   ~ 0,
      veracity == "t"                   ~ 0
    ),
    cr = case_when(
      veracity == "t" & accuracy == 1  ~ 1,
      veracity == "t" & accuracy == 0  ~ 0,
      veracity == "f"                  ~ 0
    )
  )

### Hits

hit_wide <- signal_long %>% 
  pivot_wider(
    id_cols = "id",
    names_from = "sender",
    values_from = "hit"
  ) %>% 
  select(-id) %>% 
  mutate(
    hits = rowSums(., na.rm = TRUE)
  )

### False alarms

fa_wide <- signal_long %>% 
  pivot_wider(
    id_cols = "id",
    names_from = "sender",
    values_from = "fa"
  ) %>% 
  select(-id) %>% 
  mutate(
    fa = rowSums(., na.rm = TRUE)
  )

### Misses

miss_wide <- signal_long %>% 
  pivot_wider(
    id_cols = "id",
    names_from = "sender",
    values_from = "miss"
  ) %>% 
  select(-id) %>% 
  mutate(
    misses = rowSums(., na.rm = TRUE)
  )

### Correct rejections

cr_wide <- signal_long %>% 
  pivot_wider(
    id_cols = "id",
    names_from = "sender",
    values_from = "cr"
  ) %>% 
  select(-id) %>% 
  mutate(
    cr = rowSums(., na.rm = TRUE)
  )

### Signal detection indices

sdt_indices <- 
  dprime(n_hit = hit_wide$hits, 
         n_fa = fa_wide$fa, 
         n_miss = miss_wide$misses, 
         n_cr = cr_wide$cr) %>% 
  as.data.frame()

### Binding data

sdt_data <- bind_cols(raw, sdt_indices)
