

source("ui/CEtimeCorrection.ui.R")
source("ui/Matching.ui.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel(
    title = div(style = "font-weight:bold;color: #760001;font-size: 0.6em"
                , "MAtching of Peptide SEQuences (MAPSEQ)"),
    
    windowTitle = "Matching of peptide sequences"
  ),
  
  #theme = shinytheme("united"),
  theme = shinytheme("lumen"),
  
  #shinythemes::themeSelector(),
  
  shinyjs::useShinyjs(),
  useSweetAlert(),
  includeCSS("www/style.css"),
  

  
  tags$head(
    tags$script(
      'Shiny.addCustomMessageHandler("changeProgressHeader",',
      'function(msg) {',
      '    $("#progressBarHeader_" + msg.tid).html(msg.value);',
      '});',
      'Shiny.addCustomMessageHandler("changeProgressFooter",',
      'function(msg) {',
      '    $("#progressBarFooter_" + msg.tid).html(msg.value);',
      '});',
      'Shiny.addCustomMessageHandler("emptyNode",',
      'function(msg) {',
      '    $("#" + msg.id).empty();',
      '});'
    )
  ),
  
  #### Suppression the warning an error message in console shiny
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  
  navbarPage(
    id = "IDNavbar",
    title = "MAPSEQ",
    inverse = TRUE,
    collapsible = TRUE,
    
    #-------------------------- Correction time --------------------------#
    #----------------------------------------------------------------------#
    
    tabPanel(
      "Upload data and CE-time correction",
      icon = icon("flask"),
      fluidRow(br()),
      TabPanelFilterSampleCEtimeCorrection(),
      
    ),
    
    tabPanel(
      "Matching sequences",
      icon = icon("microscope"),
      fluidRow(br()),
      TabPanelMatchingSequence()
      
    )
  )
  
  
)
