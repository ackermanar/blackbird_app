install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
}

options(repos = c(CRAN = "https://cloud.r-project.org/"))
required_packages <- c("shiny", "shinydashboard", "shinyFiles", "shinyjs", "htmltools", "agricolae", "DT", "openxlsx", "lme4", "Matrix", "purrr", "tidyverse", "stringi")
install_if_missing(required_packages)

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(htmltools)
library(agricolae)
library(DT)
library(openxlsx)
library(lme4)
library(Matrix)
library(purrr)
library(tidyverse)

outdated_packages <- old.packages()
if (!is.null(outdated_packages)) {
  update.packages(oldPkgs = row.names(outdated_packages), ask = FALSE)
}

get_root_dir <- function() {
  if (.Platform$OS.type == "unix") {
      return(normalizePath("~"))
  } else {
      return(normalizePath("C:/"))
  }
}

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

safe_audpc <- purrr::safely(audpc)

server <- function(input, output, clientData, session) { # nolint
  get_root_dir <- function() {
    if (.Platform$OS.type == "unix") {
      return(normalizePath("~"))
    } else {
      return(normalizePath("C:/"))
    }
  }
  # Select and view the image
  shinyDirChoose(input, "folder", roots = c("home" = get_root_dir()), session = session, filetypes = NULL)
  observe({
    file <- input$file
    folder <- input$folder
    req(file)
    req(folder)

    if (input$search == FALSE) {
      fileSep <- .Platform$file.sep
      df <- read.xlsx(xlsxFile = file$datapath,
                      colNames = TRUE,
                      rowNames = FALSE,
                      detectDates = TRUE,
                      skipEmptyRows = TRUE,
                      na.strings = "N/A") # nolint: line_length_linter.
      dpi_columns <- dpi_columns <- which(str_detect(names(df), "dpi"))
      # Render tbl
      output$tbl <- renderDT(df, server = TRUE,
                             selection = list(mode = "single", target = "cell", columns = dpi_columns))

      dirPath <- str_c(parseDirPath(c("home" = get_root_dir()), folder), sep = fileSep)

      output$pathReadout <- renderText({
        str_c("Searching for images in folder along path: ", dirPath)
      })
        
 
      output$text <- renderPrint({
        folderPath <- str_c(dirPath, fileSep, "Batch", df[input$tbl_cell_clicked$row, 1], "_Rep", df[input$tbl_cell_clicked$row, 2])
        allDirs <- list.dirs(folderPath, full.names = TRUE, recursive = FALSE)
        # Find the string in allDirs that ends with colName
        req(input$tbl_cell_clicked)
        matchDir <- str_subset(allDirs, colnames(df)[input$tbl_cell_clicked$col])
        if (length(matchDir) == 0) {
          print(str_c(colnames(df)[input$tbl_cell_clicked$col], " not found in directory ", folderPath))
        } else {
        image <- str_c(df[input$tbl_cell_clicked$row, 4], "-", df[input$tbl_cell_clicked$row, 6], "_", df[input$tbl_cell_clicked$row, 5], "_", df[input$tbl_cell_clicked$row, 3], ".png")
        imagePath <- str_c(matchDir, "1", image, sep = fileSep)
        imagePath
        }
      })

      output$imageName <- renderText({
        str_c("Image: ", str_c(df[input$tbl_cell_clicked$row, 4], "-", df[input$tbl_cell_clicked$row, 6], "_", df[input$tbl_cell_clicked$row, 5], "_", df[input$tbl_cell_clicked$row, 3]))
      })

      # Render image
      output$image <- renderImage({
        width  <- clientData$output_image_width
        height <- clientData$output_image_height
        folderPath <- str_c(dirPath, fileSep, "Batch", df[input$tbl_cell_clicked$row, 1], "_Rep", df[input$tbl_cell_clicked$row, 2])
        allDirs <- list.dirs(folderPath, full.names = TRUE, recursive = FALSE)
        # Find the string in allDirs that ends with colName
        matchDir <- str_subset(allDirs, colnames(df)[input$tbl_cell_clicked$col])
        image <- str_c(df[input$tbl_cell_clicked$row, 4], "-", df[input$tbl_cell_clicked$row, 6], "_", df[input$tbl_cell_clicked$row, 5], "_", df[input$tbl_cell_clicked$row, 3], ".png")
        imagePath <- str_c(matchDir, "1", image, sep = fileSep)
        list(src = imagePath,
            contentType = "image/png",
            width = width,
            height = height)},
            deleteFile = FALSE)

    } else if (input$search == TRUE) {
       df <- read.xlsx(xlsxFile = file$datapath, colNames = TRUE, rowNames = TRUE,
       detectDates = TRUE, skipEmptyRows = TRUE, na.strings = "N/A") # nolint: line_length_linter.

      # Find the directory path
      dir_path <- parseDirPath(c("home" = get_root_dir()), folder)

      # Render tbl
      output$tbl <- renderDT(df, server = TRUE,
                             selection = list(mode = "single", target = "cell"))

      output$text <- renderPrint({
        validate(
          need(sub("_Results.xlsx", "", file$name) == basename(dir_path), "Image file and result file do not match") # nolint: line_length_linter.
        )
        paste(
          dir_path, # nolint: line_length_linter.
          colnames(df)[input$tbl_cell_clicked$col],
          "1",
          paste0(rownames(df)[input$tbl_cell_clicked$row], ".png"), # nolint: line_length_linter.
          sep = .Platform$file.sep)
      })
              # Render image
      output$image <- renderImage({
        width  <- clientData$output_image_width
        height <- clientData$output_image_height
        list(src = paste(dir_path,
                        colnames(df)[input$tbl_cell_clicked$col],
                        "1",
                        paste0(rownames(df)[input$tbl_cell_clicked$row], ".png"), # nolint: line_length_linter.
                        sep = .Platform$file.sep), contentType = "image/png", width = width, height = height) # nolint: line_length_linter.
      }, deleteFile = FALSE)
    }
  }) %>%
    bindEvent(c(input$file, input$folder, input$search))

  # Cacluate Output Files
  observe({
    files <- input$files
    req(files)
    if (is.null(files)) {
      output$text2 <- renderText({
        "No files selected, please upload files to analyze."
      })
    } else {
      output$text2 <- renderText({
        files_names <- files$name %>%
          str_remove(" Results.xlsx")
        paste(files_names, collapse = ", ")
      })

      names <- files$name
      jobs <- files$datapath

      # audpuc from mixed model
      results1 <- map2_dfr(names, jobs, ~{
        meta <- str_remove(basename(.x), " Results.xlsx") %>%
        strsplit("_T")
        date <- meta[[1]][1]
        testIso <- strsplit(meta[[1]][2], "_")
        test <- testIso[[1]][1]
        iso <- testIso[[1]][2]

        df <- read.xlsx(xlsxFile = .y,
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
      # Mixed model from audpc
      results2 <- map2_dfr(names, jobs, ~{
        meta <- str_remove(basename(.x), " Results.xlsx") %>%
          strsplit("_T")
        date <- meta[[1]][1]
        testIso <- strsplit(meta[[1]][2], "_")
        test <- testIso[[1]][1]
        iso <- testIso[[1]][2]

        df <- read.xlsx(xlsxFile = .y,
                        colNames = TRUE,
                        rowNames = FALSE,
                        detectDates = TRUE,
                        skipEmptyRows = TRUE,
                        na.strings = "N/A") %>%
          filter(str_detect(Sample, "_")) %>%
          mutate(Date = date, Test = test, Iso = iso) %>%
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

        df$Sample <- str_remove_all(df$Sample, paste0("_", iso))

        colnames(df) <- str_remove(colnames(df), ".*_")
        return(df)
        }) %>%
        distinct() %>%
        select(Sample, Date, Iso, everything()) %>%
        pivot_longer(cols = -c(ID, Sample, Rep, Date, Test, Iso), names_to = "DPI", values_to = "Pheno") %>%
        mutate(DPI = str_remove(DPI, "dpi"), DPI = as.numeric(DPI)) %>%
        na.omit(Pheno) %>%
        group_by(Sample, Date, Iso, Test, ID, Rep) %>%
        summarise(absoluteAUDPC = audpc(Pheno, DPI, "absolute"), relativeAUDPC = audpc(Pheno, DPI, "relative")) %>%
        ungroup() %>%
        group_split(Iso) %>%
        map_dfr(~{
          model <- NULL
          iso <- unique(.x$Iso)

          sumDate <- .x  %>%
            group_by(Date) %>%
            summarise(n())

          sumTest <- .x %>%
            group_by(Test) %>%
            summarise(n())

          if (nrow(sumDate) == 1 && nrow(sumTest) == 1) {
            model <- lmer(absoluteAUDPC ~ (1 | Sample), # nolint: line_length_linter.
                          data = .x,
                          REML = TRUE,
                          na.action = na.omit)
          } else if (nrow(sumDate) == 1 && nrow(sumTest) > 1) {
            model <- lmer(absoluteAUDPC ~ Test + (1 | Sample), # nolint: line_length_linter.
                          data = .x,
                          REML = TRUE,
                          na.action = na.omit)
          } else if (nrow(sumDate) > 1) {
            model <- lmer(absoluteAUDPC ~ (1 | Sample) + (1 | Date/Test), # nolint: line_length_linter.
                    data = .x,
                    REML = TRUE,
                    na.action = na.omit)
            if (hasConverged(model) != 1) {
              model <- lmer(absoluteAUDPC ~ (1 | Sample) + (1 | Date:Test), # nolint: line_length_linter.
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

            if (!any(colnames(resultsAbsolute) == "Test")) {
              resultsAbsolute <- resultsAbsolute %>%
                mutate(Test = unique(sumTest$Test))
            }
          }

          model <- NULL

          if (nrow(sumDate) == 1 && nrow(sumTest) == 1) {
            model <- lmer(relativeAUDPC ~ (1 | Sample), # nolint: line_length_linter.
                          data = .x,
                          REML = TRUE,
                          na.action = na.omit)
          } else if (nrow(sumDate) == 1 && nrow(sumTest) > 1) {
            model <- lmer(relativeAUDPC~ Test + (1 | Sample), # nolint: line_length_linter.
                          data = .x,
                          REML = TRUE,
                          na.action = na.omit)
          } else if (nrow(sumDate) > 1) {
            model <- lmer(relativeAUDPC ~ (1 | Sample) + (1 | Date/Test), # nolint: line_length_linter.
                    data = .x,
                    REML = TRUE,
                    na.action = na.omit)
            if (hasConverged(model) != 1) {
              model <- lmer(relativeAUDPC ~ (1 | Sample) + (1 | Date:Test), # nolint: line_length_linter.
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

            resultsRelative <- model@frame %>%
              as_tibble() %>%
              mutate(predRelativeAUDPC = pred, relativeAUDPC_SE = se, relativeAUDPC_Status = "Predicted", relativeAUDPC_Residual = res, Iso = iso)

            if (!any(colnames(resultsRelative) == "Date")) {
              resultsRelative <- resultsRelative %>%
                mutate(Date = unique(sumDate$Date))
            }

            if (!any(colnames(resultsRelative) == "Test")) {
              resultsRelative <- resultsRelative %>%
                mutate(Test = unique(sumTest$Test))
            }

          results <- .x %>%
              left_join(resultsAbsolute, by = c("Sample", "Date", "Test", "Iso", "absoluteAUDPC"), relationship = "many-to-many") %>%
              left_join(resultsRelative, by = c("Sample", "Date", "Test", "Iso", "relativeAUDPC"), relationship = "many-to-many") %>%
              select(all_of(c("Sample", "Date", "Test", "Iso",
                              "absoluteAUDPC", "predAbsoluteAUDPC", "absoluteAUDPC_Status", "absoluteAUDPC_Residual", "absoluteAUDPC_SE",
                              "relativeAUDPC", "predRelativeAUDPC", "relativeAUDPC_Status", "relativeAUDPC_SE", "relativeAUDPC_Residual")))  %>%
              arrange(Sample, Date, Test, Iso)
            return(results)
          }
        })

      if (length(unique(results1$Iso)) == 1) {
      model <- lmer(Pheno ~ (1 | Sample/DPI), # nolint: line_length_linter.
                  data = results1,
                  REML = TRUE,
                  na.action = na.omit)
      } else {
        model <- lmer(Pheno ~ Iso + (1 | Sample/DPI), # nolint: line_length_linter.
                      data = results1,
                      REML = TRUE,
                      na.action = na.omit)
      }

      pred_mod <- predict(model, se.fit = TRUE)
      pred <- pred_mod$fit
      se <- pred_mod$se.fit

      audpcMod <- model@frame %>%
        mutate(Value = pred, SE = se) %>%
        mutate(Value = if_else(Value < 0, 0, Value)) %>%
        select(!Pheno) %>%
        distinct()

      if(!any(colnames(audpcMod) == "Iso")) {
        audpcMod <- audpcMod %>%
        mutate(Iso = unique(results1$Iso), .after = "Sample") 
      }
      
      audpcMod <- audpcMod %>%
        select(Sample, Iso, DPI, Value, SE)

      resultsSelect <- audpcMod %>%
        group_by(Sample, Iso) %>%
        summarise(minDPI = min(DPI, na.rm = TRUE),
        maxDPI = max(DPI, na.rm = TRUE),
        totalDPI = n_distinct(DPI, na.rm = TRUE))

      output$results <- renderDT({
        datatable(resultsSelect, selection = "multiple", options = list(pageLength = 50, lengthChange = TRUE))
      })

      output$text3 <- renderPrint({
        paste0("Showing AUDPC for Sample ", resultsSelect$Sample[input$results_rows_selected], " and Iso ", resultsSelect$Iso[input$results_rows_selected])
      })

      output$plot_audpc <- renderPlot({
        audpcPlot <- audpcMod %>%
          filter(Sample %in% c(resultsSelect$Sample[input$results_rows_selected]) & Iso %in% c(resultsSelect$Iso[input$results_rows_selected])) %>%
              ggplot(aes(x = DPI, y = Value, color = Sample)) +
              geom_line() +
              geom_point() +
              geom_area(aes(fill = Sample), alpha = 0.2) +
              geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), width = 0.4)

        if(input$rotate == FALSE) {
          audpcPlot + facet_grid(Iso ~ Sample) +
          scale_x_continuous(breaks = seq(floor(min(audpcMod$DPI)), ceiling(max(audpcMod$DPI)), by = 1))
        } else if (input$rotate == TRUE) {
          audpcPlot + facet_grid(Sample ~ Iso) +
          scale_x_continuous(breaks = seq(floor(min(audpcMod$DPI)), ceiling(max(audpcMod$DPI)), by = 1))
        }
      })
      
      output$download <- downloadHandler(
        filename = 'results.zip',
        content = function(fname) {
          tmpdir <- tempdir()
          setwd(tempdir())

          fs <- c()
          if (any(input$filesForDownload %in% "1phase")) {
            write.csv(results1, file = paste0("mixed_model_1phase", Sys.Date(), ".csv"), sep =",")
            fs <- c(fs, paste0("mixed_model_1phase", Sys.Date(), ".csv"))
          }
          if (any(input$filesForDownload %in% "2phase")) {
            write.csv(audpcMod, file = paste0("mixed_model_2phase", Sys.Date(), ".csv"), sep =",")
            fs <- c(fs, paste0("mixed_model_2phase", Sys.Date(), ".csv"))
          }
          if (any(input$filesForDownload %in% "audpcMod")) {
            write.csv(results2, file = paste0("mixed_model_audpc", Sys.Date(), ".csv"), sep =",")
            fs <- c(fs, paste0("mixed_model_audpc", Sys.Date(), ".csv"))
          }
          zip(zipfile=fname, files=fs)
        },
        contentType = "application/zip"
      )
    }
  }) %>%
    bindEvent(input$calculate)
}
