install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
}

options(repos = c(CRAN = "https://cloud.r-project.org/"))
required_packages <- c("shiny", "shinydashboard", "shinyFiles", "shinyjs", "htmltools", "agricolae", "DT", "openxlsx", "lme4", "Matrix", "purrr", "tidyverse")
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


ui <- dashboardPage(
  dashboardHeader(title = "Blackbird Image Lookup"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("File Upload", tabName = "upload", icon = icon("cloud-upload")),
      menuItem("Selector", tabName = "selector", icon = icon("search")),
      menuItem("Viewer", tabName = "viewer", icon = icon("binoculars")),
      menuItem("Analyze", tabName = "analyze", icon = icon("calculator")),
      menuItem("AUDPC", tabName = "AUDPC", icon = icon("chart-line")),
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
              actionButton("calculate", "Run Mixed Model Analysis"),
              br(),
              br(),
              checkboxGroupInput("filesForDownload", "Output Files to download:",
                                 choices = c("Mixed model 1phase" = "1phase",
                                             "Mixed model 2phase" = "2phase",
                                             "mixed model on AUDPC" = "audpcMod1",
                                             "AUDPC from mixed model" = "audpcMod2"),
                                 selected = c("1phase", "2phase", "audpcMod1", "audpcMod2")),
              downloadButton("download", "Download Selected Results")
            ),
            column(width = 9,
              conditionalPanel(
                condition = "input.calculate > 0",
                h3("Running mixed model analysis on:")
              ),
              textOutput("text2"),
              conditionalPanel(
                condition = "input.calculate > 0",
                h4("Select up to ten rows to visualize in the AUDPC visualizer tab."),
              ),
              br(),
              DTOutput("results")
            )
          )
        )
      ),
      tabItem(tabName = "AUDPC",
        fluidPage(
          h3("Area Under the Disease Progress Curve (AUDPC)"),
          verbatimTextOutput("text3"),
          checkboxInput("rotate", "Rotate axes of AUDPC visualizer", value = FALSE, width = NULL),
          plotOutput("plot_audpc")
        )
      ),
      tabItem(tabName = "help",
        h3("Help"),
        p("Blackbird Image Lookup is designed to help you quickly look up the corresponding image for a result file, allowing quick image validation."), # nolint: line_length_linter.
        h4("Instructions for viewing images:"),
        p("First, using the file upload tab, please select the directory that contains the images and the result file."),
        p("Second, using selector tab, select a value in the displayed result file."),
        p("Finally, using the viewer tab, validate the image. The image will be displayed at a size according to the user viewport."),
        h4("Instructions for running mixed model analysis and using AUDPC visualizer:"),
        p("First, using the analyze tab, select the result files to run mixed model analysis on. As little or as many files can be selected using cmd/cntrl + click or shift + click."),
        p("Second, click the 'Run Mixed Model Analysis' button to run the mixed model analysis. The results will be displayed in an interactive table."),
        p("Third, select up to ten different rows in the interactive table  to visualize in the AUDPC visualizer."),
        p("Fourth, click the 'AUDPC' button to view AUDPC per sample/iso combination."),
        h4("Instructions for downloading results:"),
        p("First, in the Analyze tab, select the output files you would like to download using the checkboxes."),
        p("Second, click the 'Download Selected Results' button to download the selected files."),
        h4("Results files explained:"),
        p("Mixed model 1phase:  includes a prediction for every individual dpi per sample/iso/test/date combination."),
        p("Mixed model 2phase: includes a prediction for every individual dpi per sample/iso combination, this will be the most accurate prediction model, but with less data than 1phase."),
        p("Mixed model on AUPDC: First performs AUDPC for each individual sample across all dpi dates avaiable for the correspaning sample, then performas a mixed model on all available AUDPC for a giving variety/genotype. Due to higher standard error across sample values than AUDPC per sample, this may be a desirable solution."),
        p("AUDPC from mixed model: This is the AUDPC value calculated from the mixed model results titled Mixed Model 2phase.")
      )
    )
  )
)
