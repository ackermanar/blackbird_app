library(lme4)
library(tidyverse)
library(purrr)
library(magrittr)
library(readxl)
library(data.table)

df <- read_csv("INSERT FILE HERE IE GREENHOUSE2023.CSV")

df <- df %>%
  mutate_at(vars(2:9), factor)

test <- summary(df)

results <- tibble()

column_names <- colnames(df)[10:18]

for (i in column_names) {

  df2 <- df %>% select(colnames(df)[2:9], i) %>% rename(pheno = i)

  proportion_zeros <- mean(df2$pheno, na.rm = TRUE)
  if (proportion_zeros <= 0.5) {
    mean <- df2 %>% group_by(sample, test, treatment) %>%
      summarise(mean = mean(pheno, na.rm = TRUE)) %>%
      ungroup()
      results <- tibble(pheno = mean$mean, test = mean$test, treatment = mean$treatment, sample = mean$sample, status = "Averaged", dpi = i) %>%
      rbind(results)

  } else {

    model <- lmer(pheno ~ test + treatment + (1 | sample) + (1 | sample:test:treatment), # nolint: line_length_linter.
                  data = df2,
                  REML = FALSE,
                  na.action = na.omit)
    results2 <- model@frame %>%
      mutate(status = "Predicted") %>%
      mutate(dpi = i) %>%
      rbind(results)
  }
}

# Replace predicted values less than 0 with 0
results %<>% mutate(predicted.value = ifelse(predicted.value < 0, 0, predicted.value))

write.csv(results, file = "MYRESULTS.csv")
