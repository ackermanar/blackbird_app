library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(agricolae)
library(DT)
library(openxlsx)
library(lme4)
library(Matrix)
library(purrr)
library(tidyverse)


jobs <- list("/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_09_2023_T2_21007 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_09_2023_T1_21007 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_09_2023_T3_22031 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_10_2023_T1_21007 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_10_2023_T1_21007 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_10_2023_T2_22031 Results.xlsx")

df <- map_dfr(jobs, ~{
  meta <- str_remove(basename(.x), " Results.xlsx") %>%
    strsplit("_T")
  date <- meta[[1]][1]
  testIso <- strsplit(meta[[1]][2], "_")
  test <- testIso[[1]][1]
  iso <- testIso[[1]][2]

  df <- read.xlsx(xlsxFile = .x,
                  colNames = TRUE,
                  rowNames = FALSE,
                  detectDates = TRUE,
                  skipEmptyRows = TRUE,
                  na.strings = "N/A") %>%
    mutate(Date = date, Test = test, Iso = iso)

  df$Sample <- str_remove_all(df$Sample, "[\\-_0-9]")

  colnames(df) <- str_remove(colnames(df), ".*_")
  return(df)
})

# Move Date, Test, and Iso columns after Sample
df <- df %>%
  select(Sample, Date, Iso, everything()) %>%
  pivot_longer(cols = -c(Sample, Date, Iso), names_to = "DPI", values_to = "Pheno")

dfList <- map(colnames(df)[str_detect(colnames(df), "dpi$")], ~{
  df <- df %>% select(all_of(c("Sample", "Date", "Iso", .x)))
  return(df)
})

results <- map_dfr(dfList, ~{
  dpi <- colnames(.x)[str_detect(colnames(.x), "dpi$")]
  df <- .x %>%
    rename(Pheno = dpi)
  results <- tibble()
  model <- NULL
  zero_counts <- df %>%
    group_by(Sample, Date, Iso) %>%
    summarise(zero_count = sum(Pheno == 0, na.rm = TRUE) / n()) %>%
    mutate(varianceThreshold = zero_count <= 0.25) %>%
    select(-zero_count)

  df <- df %>% left_join(zero_counts, by = c("Sample","Date", "Iso"), relationship = "many-to-many")

  df_low_var <- df %>% filter(varianceThreshold == FALSE) %>%
    mutate(Value = 0, DPI = dpi, Status = "Low Variance, Zeroed") %>%
    select(-varianceThreshold)

  df_high_var <- df %>% filter(varianceThreshold == TRUE) %>%
    select(-varianceThreshold) %>%
    na.omit(Pheno)

   sumSample <- df_high_var %>%
    group_by(Sample) %>%
    summarise(n())

  if (nrow(df_high_var) == 0) {

    results <- df_low_var %>%
      mutate(Residual = NA) %>%
      select(Sample, Date, Iso, DPI, Pheno, Value, Status, Residual) %>%
      arrange(desc(Sample), desc(Iso), Date)

  } else if (nrow(sumSample) < 5) {

    results <- df_high_var %>%
      mutate(Value = 0, Status = "Low Sample Count, Zeroed", DPI = dpi) %>%
      select(Sample, Date, Iso, Pheno, Value, DPI, Status) %>%
      rbind(df_low_var) %>%
      arrange(desc(Sample), desc(Iso), Date)

  } else {

    sumDate <- df_high_var %>%
      group_by(Date) %>%
      summarise(n())

    sumIso <- df_high_var %>%
      group_by(Iso) %>%
      summarise(n())

    if (nrow(sumDate) == 1 && nrow(sumIso) > 1) {
      model <- lmer(Pheno ~ Iso + (1 | Sample) + (1 | Sample:Iso), # nolint: line_length_linter.
                    data = df_high_var,
                    REML = TRUE,
                    na.action = na.omit)
    } else if (nrow(sumDate) < 5 && nrow(sumIso) < 5) {
      model <- lmer(Pheno ~ Date + Iso + (1 | Sample) + (1 | Sample:Date:Iso), # nolint: line_length_linter.
                    data = df_high_var,
                    REML = TRUE,
                    na.action = na.omit)
    } else if (nrow(sumDate) >= 5 && nrow(sumIso) >= 5) {
      model <- lmer(Pheno ~ (1:Date) + (1:Iso) + (1 | Sample) + (1 | Sample:Date:Iso), # nolint: line_length_linter.
                  data = df_high_var,
                  REML = TRUE,
                  na.action = na.omit)
    } else if (nrow(sumDate) >= 5 && nrow(sumIso) < 5) {
      model <- lmer(Pheno ~  Iso + (1 | Date) + (1 | Sample) + (1 | Sample:Date:Iso), # nolint: line_length_linter.
                    data = df_high_var,
                    REML = TRUE,
                    na.action = na.omit)
    } else if (nrow(sumDate) < 5 && nrow(sumIso) >= 5) {
      model <- lmer(Pheno ~  Date + (1 | Iso) + (1 | Sample) + (1 | Sample:Date:Iso), # nolint: line_length_linter.
                    data = df_high_var,
                    REML = TRUE,
                    na.action = na.omit)
    } else {
      stop(paste("Mixed model failed to run on samples taken at", dpi, "please ensure there are enough samples to run mixed model analysis."))
    }
  }

  if (is.null(model)) {
    return(results)
  } else {
    sum_mod <- summary(model)
    res <- sum_mod$residuals
    results <- model@frame %>%
      bind_cols(as_tibble(predict(model))) %>%
      mutate(Status = "Predicted", Residual = res, DPI = dpi) %>%
      rename(Value = value)

    if (!any(colnames(results) == "Date")) {
      results <- results %>%
        mutate(Date = unique(sumDate$Date))
    }

    results <- results %>% rbind(df_low_var) %>%
      arrange(desc(Sample), desc(Iso), Date) %>%
      select(all_of(c("Sample", "Date", "Iso", "DPI", "Pheno", "Value", "Status", "Residual")))

    return(results)

  }
})

resultsDate<- results %>%
  group_by(Sample, Iso, DPI) %>%
  mutate(DPI = str_remove(DPI, "dpi")) %>%
  mutate(DPI = as.numeric(DPI)) %>%
  summarise(PhenoSD = sd(Pheno, na.rm = TRUE),
            Value = mean(Value, na.rm = TRUE))

resultsSelect <- results %>%
  mutate(DPI = str_remove(DPI, "dpi")) %>%
  mutate(DPI = as.numeric(DPI)) %>%
  group_by(Sample, Iso) %>%
  summarise(minDPI = min(DPI, na.rm =TRUE),
    maxDPI = max(DPI, na.rm = TRUE),
    totalDPI = n_distinct(DPI, na.rm = TRUE),
    Dates = n_distinct(Date, na.rm = TRUE))

Sample <- "AB"
Iso <- "21007"

resultsAUDPC <- resultsDate %>%
  filter(Sample == "AB", Iso == "21007")
resultsPheno <- results %>%
  mutate(DPI = str_remove(DPI, "dpi")) %>%
  mutate(DPI = as.numeric(DPI)) %>%
  filter(Sample == "AB", Iso == "21007")

dpis <- resultsAUDPC$DPI
values <- resultsAUDPC$Value


audpc_new <- function(evaluation, dates, type) {
  if (!(is.null(dim(evaluation)))) {
    cat('Error:\nThis function can only be applied to a vector, not matrix or df')
  }
  na_ind <- which(!is.na(evaluation) & !is.na(dates))
  dates <- dates[na_ind]
  evaluation <- evaluation[na_ind]
  
  n <- length(dates)
  k <- length(evaluation)
  if (n != k) {
    cat("Error:\nThe number of dates of evaluation \nmust agree with the number of evaluations\n")
    return()
  }
  if (n == 0 | k == 0){
    return(NA)
  }
  audpc <- 0
  area.total <- 100 * (dates[n] - dates[1])
  for (i in 1:(n - 1)) {
    audpc <- audpc + (evaluation[i] + evaluation[i+1]) * (dates[i + 1] - dates[i])/2
  }
  if (type == "relative") {
    audpc <- audpc/area.total
  } else if (type == "absolute") {
    return(audpc)
  } else {
    cat("Error: type is 'absolute' or 'relative'\n\n")
  }
}

AUDPCRel <- audpc_new(values, dpis, type = "relative")
AUDPCAbs <- audpc_new(values, dpis, type = "absolute")
plot(plotAUDPC)

# Create the box plot
boxPlot <- ggplot(resultsPheno, aes(x = DPI, y = Pheno)) +
  geom_boxplot(aes(group = DPI))
  plot(boxPlot)
# Create the line plot
linePlot <- ggplot(resultsAUDPC, aes(x = DPI, y = Value)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Value - PhenoSD, ymax = Value + PhenoSD), width = 0.4)
plot(linePlot)

# Overlay the plots
combinedPlot <- boxPlot +
  geom_line(data = resultsAUDPC, aes(x = DPI, y = Value, group = 1))

print(combinedPlot)
