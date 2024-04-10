library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(agricolae)
library(DT)
library(openxlsx)
library(lme4)
library(car)
library(Matrix)
library(purrr)
library(tidyverse)

hasConverged <- function (mm) {
  if ( !inherits(mm, "merMod")) stop("Error: must pass a lmerMod object")
  retval <- NULL
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
  return(retval)
}

jobs <- list("/Users/aja294-admin/Hemp/Blackbird/data/USDA 2023 Results Validated Severity Files/Validated Results Data Sheets/21007/Rep 3/06_15_2023_T3_21007_Results_Validated.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/data/USDA 2023 Results Validated Severity Files/Validated Results Data Sheets/21007/Rep 3/06_15_2023_T3_21007_Results_Validated.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/data/USDA 2023 Results Validated Severity Files/Validated Results Data Sheets/21007/Rep 2/06_12_2023_T1_21007_Results_Validated.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/data/USDA 2023 Results Validated Severity Files/Validated Results Data Sheets/21007/Rep 2/06_14_2023_T1_21007_Results_Validated.xlsx")

results <- map_dfr(jobs, ~{
  meta <- str_remove(basename(.x), "_Results*") %>%
    strsplit("_T")
  date <- meta[[1]][1]
  TrayIso <- strsplit(meta[[1]][2], "_")
  tray <- TrayIso[[1]][1]
  iso <- TrayIso[[1]][2]

  df <- read.xlsx(xlsxFile = .x,
                  colNames = TRUE,
                  rowNames = FALSE,
                  detectDates = TRUE,
                  skipEmptyRows = TRUE,
                  na.strings = c("N/A", "NA")) %>%
    filter(str_detect(Sample, "_")) %>%
    mutate(Date = date, Tray = tray, Iso = iso) %>%
    separate(Sample, c("ID", "Sample"),
             sep = "-",
             extra = "merge",
             fill = "right") %>%
    mutate(Sample = str_replace(Sample, "(.*)_", "\\1-")) %>%
    separate(Sample, c("Sample", "Iso"),
             sep = "-") %>%
    mutate(Sample = str_replace(Sample, "(.*)_", "\\1-")) %>%
    separate(Sample, c("Sample", "Rep"),
             sep = "-")

  if (any(str_detect(colnames(df), "_"))) {
    colnames(df) <- str_remove(colnames(df), ".*_")
  }

  if (any(str_detect(colnames(df), "dpi"))) {
    colnames(df) <- str_remove(colnames(df), "dpi")
  }

  return(df)
}) %>%
  select(-ID, -Rep) %>%
  distinct() %>%
  select(Sample, Date, Iso, everything()) %>%
  pivot_longer(cols = -c(Sample, Date, Tray, Iso), names_to = "DPI", values_to = "Pheno") %>%
  mutate(DPI = str_remove(DPI, "dpi"), DPI = as.numeric(DPI)) %>%
  na.omit(Pheno) %>%
  group_by(Sample, Date, Iso, Tray) %>%
  summarise(absoluteAUDPC = audpc(Pheno, DPI, "absolute"), relativeAUDPC = audpc(Pheno, DPI, "relative")) %>%
  ungroup() %>%
  group_split(Iso) %>%
  map_dfr(~{
    model <- NULL
    iso <- unique(.x$Iso)

    sumDate <- .x  %>%
      group_by(Date) %>%
      summarise(n())

    sumTray <- .x %>%
      group_by(Tray) %>%
      summarise(n())

    if (nrow(sumDate) == 1 && nrow(sumTray) == 1) {
      model <- lmer(absoluteAUDPC ~ (1 | Sample), # nolint: line_length_linter.
                    data = .x,
                    REML = TRUE,
                    na.action = na.omit)
    } else if (nrow(sumDate) == 1 && nrow(sumTray) > 1) {
      model <- lmer(absoluteAUDPC ~ Tray + (1 | Sample), # nolint: line_length_linter.
                    data = .x,
                    REML = TRUE,
                    na.action = na.omit)
    } else if (nrow(sumDate) > 1) {
      model <- lmer(absoluteAUDPC ~ (1 | Sample) + (1 | Date/Tray), # nolint: line_length_linter.
              data = .x,
              REML = TRUE,
              na.action = na.omit)
      if (hasConverged(model) != 1) {
        model <- lmer(absoluteAUDPC ~ (1 | Sample) + (1 | Date:Tray), # nolint: line_length_linter.
                      data = .x,
                      REML = TRUE,
                      na.action = na.omit)
      }
    }
    if (is.null(model)) {
      print("Model failed")
      results <- tibble()
      return(results)
    } else {
      sum_mod <- summary(model)
      res <- sum_mod$residuals
      pred_mod <- predict(model, se.fit = TRUE)
      pred <- pred_mod$fit
      se <- pred_mod$se.fit

      resultsAbsolute <- model@frame %>%
      as_tibble() %>%
      mutate(predAbsoluteAUDPC = pred, absoluteAUDPC_SE = se, absoluteAUDPC_Status = "Predicted", absoluteAUDPC_Residual = res, Iso = iso)

      if (!any(colnames(resultsAbsolute) == "Date")) {
        resultsAbsolute <- resultsAbsolute %>%
          mutate(Date = unique(sumDate$Date))
      }

      if (!any(colnames(resultsAbsolute) == "Tray")) {
        resultsAbsolute <- resultsAbsolute %>%
          mutate(Tray = unique(sumTray$Tray))
      }
    }

    model <- NULL

    if (nrow(sumDate) == 1 && nrow(sumTray) == 1) {
      model <- lmer(relativeAUDPC ~ (1 | Sample), # nolint: line_length_linter.
                    data = .x,
                    REML = TRUE,
                    na.action = na.omit)
    } else if (nrow(sumDate) == 1 && nrow(sumTray) > 1) {
      model <- lmer(relativeAUDPC~ Tray + (1 | Sample), # nolint: line_length_linter.
                    data = .x,
                    REML = TRUE,
                    na.action = na.omit)
    } else if (nrow(sumDate) > 1) {
      model <- lmer(relativeAUDPC ~ (1 | Sample) + (1 | Date/Tray), # nolint: line_length_linter.
              data = .x,
              REML = TRUE,
              na.action = na.omit)
      if (hasConverged(model) != 1) {
        model <- lmer(relativeAUDPC ~ (1 | Sample) + (1 | Date:Tray), # nolint: line_length_linter.
                      data = .x,
                      REML = TRUE,
                      na.action = na.omit)
        if (hasConverged(model) == 1) {
          print("Success! Model has converged on second attempt")
        }
      }
    }
    if (is.null(model)) {
      print("Model failed")
      results <- tibble()
      return(results)
    } else {
      sum_mod <- summary(model)
      res <- sum_mod$residuals
      pred_mod <- predict(model, se.fit = TRUE)
      pred <- pred_mod$fit
      se <- pred_mod$se.fit

      resultsRelative <- model@frame %>%
        as_tibble() %>%
        mutate(predRelativeAUDPC = pred, relativeAUDPC_SE = se, relativeAUDPC_Status = "Predicted", relativeAUDPC_Residual = res, Iso = iso)

      if (!any(colnames(resultsRelative) == "Date")) {
        resultsRelative <- resultsRelative %>%
          mutate(Date = unique(sumDate$Date))
      }

      if (!any(colnames(resultsRelative) == "Tray")) {
        resultsRelative <- resultsRelative %>%
          mutate(Tray = unique(sumTray$Tray))
      }

    results <- .x %>%
        left_join(resultsAbsolute, by = c("Sample", "Date", "Tray", "Iso", "absoluteAUDPC"), relationship = "many-to-many") %>%
        left_join(resultsRelative, by = c("Sample", "Date", "Tray", "Iso", "relativeAUDPC"), relationship = "many-to-many") %>%
        select(all_of(c("Sample", "Date", "Tray", "Iso",
                        "absoluteAUDPC", "predAbsoluteAUDPC", "absoluteAUDPC_Status", "absoluteAUDPC_Residual", "absoluteAUDPC_SE",
                        "relativeAUDPC", "predRelativeAUDPC", "relativeAUDPC_Status", "relativeAUDPC_SE", "relativeAUDPC_Residual")))  %>%
        arrange(Sample, Date, Tray, Iso)
      return(results)
    }
  })