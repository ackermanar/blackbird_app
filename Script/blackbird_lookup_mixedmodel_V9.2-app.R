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
library(lme4)
library(Matrix)
library(purrr)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Blackbird Image Lookup"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("File Upload", tabName = "upload", icon = icon("cloud-upload")),
      menuItem("Selector", tabName = "selector", icon = icon("search")),
      menuItem("Viewer", tabName = "viewer", icon = icon("binoculars")),
      menuItem(" Analyze", tabName = "analyze", icon = icon("calculator")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
        h3("Select the directory that contains the corresponding set of images."), # nolint: line_length_linter.
        p('e.g. "2020-11-17_15-00-00"'),
        shinyDirButton("folder", "Select the image folder",
                       "Please select image folder", multiple = FALSE),
        br(),
        br(),
        fileInput("file", "Choose result file, e.g. 'Result .xlsx.'",
                  accept = ".xlsx")
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
      tabItem(tabName = "analyze",
        fluidPage(
          fluidRow(
            column(width = 3,
              fileInput("files", "Choose result files to run mixed model analysis, e.g. '06_09_2023_T1_21007 Results.xlsx'",
                        multiple = TRUE, accept = ".xlsx"),
              actionButton("calculate", "Run Mixed Model")
            ),
            column(width = 9,
              conditionalPanel(
                condition = "input.calculate > 0",
                h3("Running mixed model analysis on:")
              ),
              verbatimTextOutput("text2"),
              renderTable("results")
            )
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
    files <- input$files
    req(files)
    if (is.null(files)) {
      output$text2 <- renderPrint({
        "No files selected, please upload files to analyze."
      })
    } else {
      output$text2 <- renderPrint({
        files_names <- files$name %>%
          str_remove(" Results.xlsx")
        paste(files_names, collapse = ", ")
      })


    }
  }) %>%
    bindEvent(input$calculate)
}
shinyApp(ui, server)

outputOptions(output, "image", suspendWhenHidden = FALSE, throttleMs = 500)
