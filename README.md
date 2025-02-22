# Blackbird App
## Description: 
BlackBird imaging platforms are robotic cameras developed through the USDA-ARS funded VitisGen2 grape breeding project to allow high-throughput phenotyping of leaf-borne diseases. BlackBird imaging platforms are capable of capturing high-resolution images from inoculated leaf discs and through use of a convolutional neural network (CNN), generating disease severity scores for each. Innately, the BlackBird generates data in the scope of terabytes, and user validation of severity scores from the CNN has proven difficult. Blackbird User Interface allows users to filter data, and select severity scores to quickly view the image captured by BlackBird. Additionally, Blackbird User Interface has a suite of data analysis and visualization features, including mixed model-prediction of disease severity scores for a given entry, AUDPC visualizer using various user selected-options, and a comprehensive export function of data. BlackBird is consistently maintained, but also updated on an as-needed basis to meet the requests of users.

## Launch instructions
Launch from R or Rstudio by entering the following commands:

    shiny::runGitHub("blackbird_app", "ackermanar")

Select the "help" tab from within the app for instructions to get started.
