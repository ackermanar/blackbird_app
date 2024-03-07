library(lme)

jobs <- list("/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_09_2023_T2_21007 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_09_2023_T1_21007 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_09_2023_T3_22031 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_10_2023_T1_21007 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_10_2023_T1_21007 Results.xlsx",
             "/Users/aja294-admin/Hemp/Blackbird/blackbird_app/Data/HempDMNet3/06_10_2023_T2_22031 Results.xlsx")

df <- map_dfr(jobs, ~{
  meta <- str_remove(basename(.x), "Results.xlsx") %>%
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
  select(Sample, Date, Iso, everything())

dfList <- map(colnames(df)[str_detect(colnames(df), "dpi$")], ~{
  df %>% select(all_of(c("Sample", "Date", "Iso", .x)))
})

results <- map_dfr(dfList, ~{
  dpi <- colnames(.x)[str_detect(colnames(.x), "dpi$")]
  df <- .x %>%
    rename(Pheno = dpi)
  results <- tibble()
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
      select(Sample, Date, Iso, DPI, Pheno, Value, Status) %>%
      arrange(desc(Sample), desc(Iso), Date)
    return(results)

  } else if (nrow(df_high_var) < 5 || nrow(sumSample) < 5) {

    results <- df_high_var %>%
      mutate(Value = 0, Status = "Low Sample Count, Zeroed", DPI = dpi) %>%
      rbind(df_low_var) %>%
      select(Sample, Date, Iso, DPI, Pheno, Value, Status) %>%
      arrange(desc(Sample), desc(Iso), Date) 
    return(results)

  } else {

    sumDate <- df_high_var %>%
      group_by(Date) %>%
      summarise(n())

    sumIso <- df_high_var %>%
      group_by(Iso) %>%
      summarise(n())

    if (nrow(sumDate) <= 1 || nrow(sumIso) <= 1) {

      results <- df_high_var %>%
        mutate(Value = 0, Status = "Low Sample Count, Zeroed", DPI = dpi) %>%
        rbind(df_low_var) %>%
        select(Sample, Date, Iso, DPI, Pheno, Value, Status) %>%
        arrange(desc(Sample), desc(Iso), Date) 
      return(results)

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

  if (!is.null(model)) {

    results <- model@frame %>%
      bind_cols(as.tibble(predict(model))) %>%
      mutate(Status = "Predicted", DPI = dpi) %>%
      rename(Value = value) %>%
      rbind(df_low_var) %>%
      arrange(desc(Sample), desc(Iso), Date) %>%
      select(Sample, Date, Iso, DPI, Pheno, Value, Status)
    return(results)
  }
})

test <- results %>% group_by(Status) %>% summarise(n())
