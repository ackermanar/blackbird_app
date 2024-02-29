### Quick lookup of blackbird output images for qucik validation ###
# Wrote by A.J. Ackerman, November 17th, 2023.
# contact: aja294@cornell.edu

required_packages <- c("shiny", "shinydashboard", "shinyFiles", "DT", "openxlsx", "tidyverse")

install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
}

install_if_missing(required_packages)

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(DT)
library(openxlsx)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Blackbird Image Lookup"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("File Upload", tabName = "upload", icon = icon("cloud-upload")),
      menuItem("Selector", tabName = "selector", icon = icon("search")),
      menuItem("Viewer", tabName = "viewer", icon = icon("binoculars")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
        fluidPage(
          fluidRow(column = 6,
          h3("Select the directory that contains the corresponding set of images."), # nolint: line_length_linter.
          p('e.g. "2020-11-17_15-00-00"'),
          shinyDirButton("folder", "Select the image folder",
                         "Please select image folder", multiple = FALSE),
          br(),
          br(),
          fileInput("file", "Choose result file, e.g. 'Result .xlsx.'",
                    accept = ".xlsx")
          ),
          fluidRow(column = 6,
            actionButton("calculate", "Run Mixed Model")
          )
        )
      ),
      tabItem(tabName = "selector",
        verbatimTextOutput("text"),
        DTOutput("tbl")
      ),
      tabItem(tabName = "viewer",
        fluidRow(
          column(width = 12,
            imageOutput("image", width = "100%", height = "800px")
          )
        )
      ),
      tabItem(tabName = "help",
        h3("Help"),
        p("Blackbird Image Lookup is designed to help you quickly look up the corresponding image for a result file, allowing quick image validation."), # nolint: line_length_linter.
        p("First, using the file upload tab, please select the directory that contains the images and the result file."),
        p("Second, using selector tab, select a value in the displayed result file."),
        p("Finally, using the viewer tab, validate the image. The image will be displayed at a size according to the user viewport.")
      )
    )
  )
)

server <- function(input, output, clientData, session) { # nolint
  get_root_dir <- function() {
    if (.Platform$OS.type == "unix") {
      return(normalizePath("~"))
    } else {
      return(normalizePath("C:/"))
    }
  }

  shinyDirChoose(input, "folder", roots = c("home" = get_root_dir()), session = session, filetypes = NULL)

  observe({
    file <- input$file
    folder <- input$folder
    req(file)
    req(folder)
    df <- read.xlsx(xlsxFile = file$datapath, colNames = TRUE, rowNames = TRUE,
                    detectDates = TRUE, skipEmptyRows = TRUE, na.strings = "N/A") # nolint: line_length_linter.

    # Find the directory path
    dir_path <- parseDirPath(c("home" = get_root_dir()), folder)

    # Find string that starts with "T" followed by a number
    tray <- sub(".*T(\\d+).*", "\\1", parseDirPath(c("home" = "~"), folder)) # nolint: line_length_linter.

    # Render tbl
    output$tbl <- renderDT(df, server = TRUE,
                           selection = list(mode = "single", target = "cell"))

    output$text <- renderPrint({
      validate(
        need(sub(" Results.xlsx", "", file$name) == basename(dir_path), "Image file and result file do not match") # nolint: line_length_linter.
      )
      paste(
        dir_path, # nolint: line_length_linter.
        colnames(df)[input$tbl_cell_clicked$col],
        tray,
        paste0(rownames(df)[input$tbl_cell_clicked$row], ".png"), # nolint: line_length_linter.
        sep = .Platform$file.sep)
    })

    # Render image
    output$image <- renderImage({
      width  <- clientData$output_image_width
      height <- (clientData$output_image_height)
      list(src = paste(dir_path,
                       colnames(df)[input$tbl_cell_clicked$col],
                       tray,
                       paste0(rownames(df)[input$tbl_cell_clicked$row], ".png"), # nolint: line_length_linter.
                       sep = .Platform$file.sep), contentType = "image/png", width = width, height = height) # nolint: line_length_linter.
    }, deleteFile = FALSE)
  }) %>%
    bindEvent(c(input$file, input$folder))

  observe({
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
          results <- tibble(pheno = mean$mean,
                            test = mean$test,
                            treatment = mean$treatment,
                            sample = mean$sample,
                            status = "Averaged",
                            dpi = i) %>%
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
    results %<>% 
      mutate(predicted.value = ifelse(predicted.value < 0, 0, predicted.value))

    write.csv(results, file = "MYRESULTS.csv")

  }) %>%
    bindEvent(input$calculate)
}
shinyApp(ui, server)

outputOptions(output, "image", suspendWhenHidden = FALSE, throttleMs = 500)
