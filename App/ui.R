required_packages <- c("shiny", "shinydashboard", "shinyFiles", "shinyjs", "DT", "openxlsx", "tidyverse")
install_if_missing(required_packages)

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

source("functions.R")

dashboardPage(
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
                                 choices = c("Mixed model results" = "1phase",
                                   "AUDPC from Mixed Model" = "2phase",
                                   "Mixed model from AUPDC" = "audpcMod"),
                                  selected = c("Mixed model results" = "1phase",
                                   "AUDPC from Mixed Model" = "2phase",
                                   "Mixed model from AUPDC" = "audpcMod")),
              downloadButton("download", "Download Selected Results"),
            ),
            column(width = 9,
              conditionalPanel(
                condition = "input.calculate > 0",
                h3("Running mixed model analysis on:")
              ),
              textOutput("text2"),
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
        ),
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