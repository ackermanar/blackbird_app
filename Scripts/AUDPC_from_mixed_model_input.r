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

jobs <- list("/Users/aja294-admin/Hemp/Blackbird/data/USDA 2023 Results Validated Severity Files/Validated Results Data Sheets/21007/Rep 2/06_12_2023_T1_21007_Results_Validated.xlsx",
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
  select(Sample, Date, Tray, Iso, everything()) %>%
  pivot_longer(cols = -c("Sample", "Date", "Tray", "Iso"), names_to = "DPI", values_to = "Pheno") %>%
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
      group_by(Sample, Date, Tray) %>%
      summarise(zero_count = sum(Pheno == 0, na.rm = TRUE) / n()) %>%
      mutate(varianceThreshold = zero_count <= 0.25) %>%
      select(-zero_count)

    df <- .x %>% left_join(zero_counts, by = c("Sample","Date", "Tray"), relationship = "many-to-many") %>%
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
        select(Sample, Date, Iso, Tray, DPI, Pheno, Value, Status, Residual, SE) %>%
        arrange(desc(Sample), desc(Iso), Date)

    } else if (nrow(sumSample) < 5) {

      results <- df_high_var %>%
        mutate(Value = 0, Status = "Low Sample Count, Zeroed", Iso = iso, DPI = dpi, Residual = NA) %>%
        select(Sample, Date, Iso, Tray, DPI, Pheno, Value, Status, Residual, SE) %>%
        rbind(df_low_var) %>%
        arrange(desc(Sample), desc(Iso), Date)

    } else {

      sumDate <- df_high_var %>%
        group_by(Date) %>%
        summarise(n())

      sumTray <- df_high_var %>%
        group_by(Tray) %>%
        summarise(n())

      if (nrow(sumDate) == 1 && nrow(sumTray) == 1) {
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
                 Tray = unique(sumTray$Tray),
                 Status = "Predicted",
                 Residual = res,
                 Iso = iso,
                 DPI = dpi) %>%
          rbind(df_low_var) %>%
          arrange(desc(Sample), desc(Iso), Date) %>%
          select(Sample, Date, Iso, Tray, DPI, Pheno, Value, Status, Residual, SE)
        return(results)
        print("Model 1")
      } else if (nrow(sumDate) > 1 && nrow(sumTray) == 1) {
        model <- lm(Pheno ~ Sample + Date, # nolint: line_length_linter.
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
                  Tray = unique(sumTray$Tray),
                  Status = "Predicted",
                  Residual = res,
                  Iso = iso,
                  DPI = dpi)
          return(results)
          print("Model 2.1")
      } else if (nrow(sumDate) == 1 && nrow(sumTray) > 1) {
        model <- lm(Pheno ~ Tray + Sample, # nolint: line_length_linter.
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
        print("Model 2.2")
      } else if (nrow(sumDate) > 1 && nrow(sumTray) > 1) {
        model <- lmer(Pheno ~ Sample + (1 | Date/Tray), # nolint: line_length_linter.
                      data = df_high_var,
                      REML = TRUE,
                      na.action = na.omit)
        if (hasConverged(model) != 1) {
          print(paste(dpi, iso, "has not converged, retrying model."))
          model <- lmer(Pheno ~ Sample + (1 | Date:Tray), # nolint: line_length_linter.
                        data = df_high_var,
                        REML = TRUE,
                        na.action = na.omit)
        }
        if (hasConverged(model) == 1) {
          print(paste(dpi, iso, "has converged succesfully!"))
        } else {
          model <- lm(Pheno ~ Sample + Date + Tray, # nolint: line_length_linter.
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
          select(Sample, Date, Iso, Tray, DPI, Pheno, Value, Status, Residual, SE)
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

audpcMod <- model@frame %>%
  mutate(Value = pred, SE = se) %>%
  mutate(Value = if_else(Value < 0, 0, Value)) %>%
  select(!Pheno)

if (!any(colnames(audpcMod) == "Iso")) {
  audpcMod <- audpcMod %>%
    mutate(Iso = unique(results$Iso), .after = "Sample") 
}

audpcMod <- audpcMod %>% select(Sample, Iso, DPI, Value, SE) %>%
  distinct()

resultsSelect <- audpcMod %>%
  group_by(Sample, Iso) %>%
  summarise(minDPI = min(DPI, na.rm = TRUE),
    maxDPI = max(DPI, na.rm = TRUE),
    totalDPI = n_distinct(DPI, na.rm = TRUE))

Sample <- "AB"
Iso <- "21007"

resultsAUDPC <- audpcMod %>%
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

audpc_table <- audpcMod %>%
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