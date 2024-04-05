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


hasConverged <- function (mm) {
  if (!inherits(mm, "merMod") && !inherits(mm, "lm")) stop("Error: must pass a lmerMod or lm object")
  retval <- NULL
  if (inherits(mm, "merMod")) {
    if(is.null(unlist(mm@optinfo$conv$lme4))) {
      retval = 1
    }
    else {
      if (isSingular(mm)) {
        retval = 0
      } else {
        retval = -1
      }
    }
  }
  return(retval)
}

jobs <- list("/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_09_2023_T2_21007 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_09_2023_T1_21007 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_09_2023_T3_22031 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_10_2023_T1_21007 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_10_2023_T1_21007 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_10_2023_T2_22031 Results.xlsx")

results <- map_dfr(jobs, ~{
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
}) %>%
  select(Sample, Date, Test, Iso, everything()) %>%
  pivot_longer(cols = -c("Sample", "Date", "Test", "Iso"), names_to = "DPI", values_to = "Pheno") %>%
  mutate(DPI = str_remove_all(DPI, "dpi")) %>%
  mutate(DPI = as.numeric(DPI)) %>%
  na.omit(Pheno) %>%
  group_split(Iso, DPI) %>%
  map_dfr(~{
    iso <- unique(.x$Iso)
    dpi <- unique(.x$DPI)
    results <- tibble()
    model <- NULL

    zero_counts <- .x %>%
      group_by(Sample, Date, Test) %>%
      summarise(zero_count = sum(Pheno == 0, na.rm = TRUE) / n()) %>%
      mutate(varianceThreshold = zero_count <= 0.25) %>%
      select(-zero_count)

    df <- .x %>% left_join(zero_counts, by = c("Sample","Date", "Test"), relationship = "many-to-many") %>%
      group_by(Sample, Date) %>%
      mutate(SE = sd(Pheno, na.rm = TRUE) / length(na.omit(Pheno))) %>%
      ungroup()

    df_low_var <- df %>% filter(varianceThreshold == FALSE) %>%
      mutate(Value = 0, Iso = iso, DPI = dpi, Status = "Low Variance, Zeroed", Residual = NA) %>%
      select(-varianceThreshold)

    df_high_var <- df %>% filter(varianceThreshold == TRUE) %>%
      select(-varianceThreshold) %>%
      na.omit(Pheno)

    sumSample <- df_high_var %>%
      group_by(Sample) %>%
      summarise(n())

    if (nrow(df_high_var) == 0) {

      results <- df_low_var %>%
        select(Sample, Date, Iso, Test, DPI, Pheno, Value, Status, Residual, SE) %>%
        arrange(desc(Sample), desc(Iso), Date)

    } else if (nrow(sumSample) < 5) {

      results <- df_high_var %>%
        mutate(Value = 0, Status = "Low Sample Count, Zeroed", Iso = iso, DPI = dpi, Residual = NA) %>%
        select(Sample, Date, Iso, Test, DPI, Pheno, Value, Status, Residual, SE) %>%
        rbind(df_low_var) %>%
        arrange(desc(Sample), desc(Iso), Date)

    } else {

      sumDate <- df_high_var %>%
        group_by(Date) %>%
        summarise(n())

      sumTest <- df_high_var %>%
        group_by(Test) %>%
        summarise(n())

      if (nrow(sumDate) == 1 && nrow(sumTest) == 1) {
        model <- lm(Pheno ~ Sample,
                    data = df_high_var,
                    na.action = na.omit)

        sum_mod <- summary(model)
        res <- sum_mod$residuals
        pred_mod <- predict(model, se.fit = TRUE)
        pred <- pred_mod$fit
        se <- pred_mod$se.fit

        results <- model.frame(model) %>%
          mutate(Value = pred,
                 SE = se,
                 Date = unique(sumDate$Date),
                 Test = unique(sumTest$Test),
                 Status = "Predicted",
                 Residual = res,
                 Iso = iso,
                 DPI = dpi) %>%
          rbind(df_low_var) %>%
          arrange(desc(Sample), desc(Iso), Date) %>%
          select(Sample, Date, Iso, Test, DPI, Pheno, Value, Status, Residual, SE)
        return(results)
        print("Model 1")
      } else if (nrow(sumDate) == 1 && nrow(sumTest) > 1) {
        model <- lm(Pheno ~ Test + Sample, # nolint: line_length_linter.
                    data = df_high_var,
                    na.action = na.omit)

        sum_mod <- summary(model)
        res <- sum_mod$residuals
        pred_mod <- predict(model, se.fit = TRUE)
        pred <- pred_mod$fit
        se <- pred_mod$se.fit

        results <- model.frame(model) %>%
          mutate(Value = pred,
                 SE = se,
                 Date = unique(sumDate$Date),
                 Status = "Predicted",
                 Residual = res,
                 Iso = iso,
                 DPI = dpi)
        return(results)
        print("Model 2")
      } else if (nrow(sumDate) > 1 && nrow(sumTest) > 1) {
        model <- lmer(Pheno ~ Sample + (1 | Date/Test), # nolint: line_length_linter.
                      data = df_high_var,
                      REML = TRUE,
                      na.action = na.omit)
        if (hasConverged(model) != 1) {
          print(paste(dpi, iso, "has not converged, retrying model."))
          model <- lmer(Pheno ~ Sample + (1 | Date:Test), # nolint: line_length_linter.
                        data = df_high_var,
                        REML = TRUE,
                        na.action = na.omit)
        }
        if (hasConverged(model) == 1) {
          print(paste(dpi, iso, "has converged succesfully!"))
        } else {
          model <- lm(Pheno ~ Sample + Date + Test, # nolint: line_length_linter.
                      data = df_high_var,
                      na.action = na.omit)
          print(paste(dpi, iso, "converged as lm."))
        }
        sum_mod <- summary(model)
        res <- sum_mod$residuals
        pred_mod <- predict(model, se.fit = TRUE)
        pred <- pred_mod$fit
        se <- pred_mod$se.fit
        if (class(model) == "lm") {
          frame <- model.frame(model)
        } else if (class(model) == "lmerMod") {
          frame <- model@frame
        }
        results <- frame %>%
          mutate(Value = pred,
                 SE = se,
                 Status = "Predicted",
                 Residual = res,
                 Iso = iso,
                 DPI = dpi) %>%
          rbind(df_low_var) %>%
          arrange(desc(Sample), desc(Iso), Date) %>%
          select(Sample, Date, Iso, Test, DPI, Pheno, Value, Status, Residual, SE)
        return(results)
        print("Model 3")
      } else if (is.null(model)) {
        return(results)
      } else {
        stop(paste("Mixed model failed to run on samples taken at", dpi, "please ensure there are enough samples to run mixed model analysis."))
      }
    }
})

if (length(unique(results$Iso)) == 1) {
  model <- lmer(Pheno ~ (1 | Sample/DPI), # nolint: line_length_linter.
              data = results,
              REML = TRUE,
              na.action = na.omit)
} else {
  model <- lmer(Pheno ~ Iso + (1 | Sample/DPI), # nolint: line_length_linter.
                data = results,
                REML = TRUE,
                na.action = na.omit)
}

pred_mod <- predict(model, se.fit = TRUE)
pred <- pred_mod$fit
se <- pred_mod$se.fit

audpc_mod <- model@frame %>%
  mutate(Value = pred, SE = se) %>%
  mutate(Value = if_else(Value < 0, 0, Value)) %>%
  select(!Pheno)
  distinct()

if (!any(colnames(audpc_mod) == "Iso")) {
  audpc_mod <- audpc_mod %>%
    mutate(Iso = unique(results$Iso), .after = "Sample") 
}

audpc_mod <- audpc_mod %>%
  select(Sample, Iso, DPI, Value, SE)
  
resultsSelect <- audpc_mod %>%
  group_by(Sample, Iso) %>%
  summarise(minDPI = min(DPI, na.rm = TRUE),
    maxDPI = max(DPI, na.rm = TRUE),
    totalDPI = n_distinct(DPI, na.rm = TRUE))

Sample <- "AB"
Iso <- "21007"

resultsAUDPC <- audpc_mod %>%
  filter(Sample %in% c("AB", "AR", "ASCS", "Abacus"), Iso %in% c("21007", "22031"))

# Create the line plot
linePlot <- ggplot(resultsAUDPC, aes(x = DPI, y = Value, color = Sample)) +
  geom_line() +
  geom_point() +
  geom_area(aes(fill = Sample), alpha = 0.2) +
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), width = 0.4) +
  facet_grid(Iso ~ Sample) +
  scale_x_continuous(breaks = seq(floor(min(df$DPI)), ceiling(max(df$DPI)), by = 1))
plot(linePlot)

audpc_table <- audpc_mod %>%
  group_split(Sample, Iso) %>%
  map_dfr(~{
    dpis <- .x$DPI
    values <- .x$Value

    AUDPCRel <- audpc(values, dpis, type = "relative")
    AUDPCAbs <- audpc(values, dpis, type = "absolute")
    return(tibble(Sample = unique(.x$Sample),
            Iso = unique(.x$Iso),
            AUDPCRel = AUDPCRel,
            AUDPCAbs = AUDPCAbs))
  })