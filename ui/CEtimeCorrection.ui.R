
TabPanelFilterSampleCEtimeCorrection<-function(){
  tabsetPanel(
    id = "IDTabsetPanelFilterSampleCEtimeCorrection",
    
    #----------------------------------------------------------------#
    #------------------------ Format PSMs Files ------------------------#
    #----------------------------------------------------------------#
    tabPanel(fluidRow(br()),
             title = "Format PSMs files",
             fluidRow(column(
               5,
               
               #---------------------- upload data -------------------#
               hr(),
               div(
                 class = "well well-sm",
                 h4("Upload PSMs files", icon("question-circle",
                                         class = "myIcoInfo")),
                 
                 fileInput(
                   inputId = "PSMsFilesID",
                   label = "Upload PSMs files (accept .csv, .txt, .xlsx)",
                   accept = c(".csv", ".txt", ".xlsx"),
                   multiple = FALSE,
                   width = "100%"
                 ),
                 hr(),
                 fluidRow(column(10,
                                 uiOutput("columnSelectedOutput"), 
                                 ))
       
                 
               ),
               hr(),
               fluidRow(
                 column(6,
                        ),
                 column(6,
                        div(
                          class="pull-right",
                          style="display:inline-block",
                          
                          shinySaveButton(
                            id =  "IDSaveFormatPSMsFile",
                            label = "Save files formatted",
                            title = "Save file as...",
                            filename = "formatted",
                            filetype = list(text = "csv"),
                            viewtype = "icon",
                            icon = icon("save", lib = "glyphicon"),
                            class =
                              "btn-primary"
                          )                     
                          
                        )
                 )
               )
               
               
               
               ),
               column(7,
                      hr(),
                      div(
                        class = "well well-sm",
                        shinycssloaders::withSpinner(
                          DTOutput("DataTableSelected"),
                          type = 1,
                          size = 0.8,
                          id = "IDDataTableSelected"
                        )
                      )
                      )
               )
             ),
               
    #----------------------------------------------------------------#
    #------------------------ Filter samples ------------------------#
    #----------------------------------------------------------------#
    
    tabPanel(fluidRow(br()),
             title = "Upload files and filter CE-timeter",
             fluidRow(column(
               5,
               
               #---------------------- upload data -------------------#
               div(
                 class = "well well-sm",
                 h4("Import files", icon("question-circle",
                                         class = "myIcoInfo")),
                 
                 fileInput(
                   inputId = "refData",
                   label = "Upload reference file (accept .csv)",
                   accept = c(".csv"),
                   multiple = FALSE,
                   width = "100%"
                 ),
                 
                 fileInput(
                   inputId = "Samples",
                   label = "Upload file to align (accept .csv)",
                   accept = c(".csv"),
                   multiple = TRUE,
                   width = "100%"
                 )
               ),
               
               #---------------------- Filter samples -------------------#
               div(
                 class = "well well-sm",
                 div(
                   id = "RetentionTimeFiltering_id_newSample",
                   
                   h4("Retention time filtering"),
                   fluidRow(column(10,
                                   uiOutput("SelectSampleCut"), )),
                   uiOutput("titleTimefilter_newSample"),
                   fluidRow(
                     column(
                       4,
                       align = "center",
                       numericInput(
                         inputId = "rt_min_cut_newSample",
                         label = "Min",
                         value = NULL,
                         min = 0
                       )
                     ),
                     column(
                       4,
                       align = "center",
                       numericInput(
                         inputId = "rt_max_cut_newSample",
                         label = "Max",
                         value = NULL,
                         min = 0
                       )
                     ),
                     column(4, div(
                       style = "position:relative;top:20px",
                       actionButton(
                         inputId = "cutOff_newSample",
                         label =
                           "Cut-off",
                         class =
                           "btn-primary",
                         icon = icon("scissors")
                       )
                     ))
                   )
                   
                 )
                
               ),
               hr(),
               fluidRow(
                 div(id = "id_validCuttingRun_newSample",
                     column(
                       6,
                     )),
                 
                 column(
                   6,
                   div(
                     class="pull-right",
                     style="display:inline-block",
                     actionButton(inputId="FilterDensityNexPage_newSample",
                                  label="Next",
                                  class="btn-primary",
                                  icon = icon("arrow-right"))
                     
                   )
                 )
                 
               )
               
             ),
             column(7, 
                    
                    fluidRow(column(12,
                                    # hidden(
                                    div(
                                      id = "id_PlotCutt_newSample",
                                      style = "position:relative",
                                      shinycssloaders::withSpinner(
                                        plotOutput(
                                          "sample_selectedCutting_Plot_newSample",
                                          width = "100%",
                                          height = "450px",
                                          dblclick = "sample_selectedCutting_dblclick_newSample",
                                          click = "sample_selectedCutting_click_newSample",
                                          brush = brushOpts(id = "sample_selectedCutting_brush_newSample",
                                                            resetOnNew = TRUE),
                                          hover = hoverOpts(
                                            "sample_selectedCutting_hover_newSample",
                                            delay = 100,
                                            delayType = "debounce"
                                          )
                                          
                                        ),
                                        type = 1,
                                        size = 0.8
                                      ),
                                      uiOutput("sample_selectedCutting_hover_info_newSample"),
                                      
                                      hidden(
                                        div(
                                          id = "UnitTime_sampleCutting_id_newSample",
                                          class = "pull-left",
                                          style = "display:inline-block;",
                                          awesomeRadio(
                                            inputId = "UnitTime_sampleCutting_newSample",
                                            label = "Unit of CE-time",
                                            choices = c("Second" = "Second",
                                                        "Minute" = "Minute"),
                                            inline = TRUE,
                                            checkbox = TRUE
                                          )
                                        )
                                      )
                                    )
                                    #)
                    )),
                    fluidRow(column(12,
                                    hidden(
                                      div(
                                        id = "mousepositionSampleCutting_newSample",
                                        style = "font-weight:bold;width: 100%; color: #000;",
                                        verbatimTextOutput("sample_selectedCutting_Plot_info_newSample")
                                        
                                        
                                      )
                                    )))
                    
             ))),
    
    tabPanel(fluidRow(br()),
             title = "CE-time correction",
             
             #----------------------------------------------------------------#
             #----------------------- Correction CE-time ---------------------#
             #----------------------------------------------------------------#
             fluidRow(column(12,
                             HTML('<h4>Step 3: CE-time correction with kernel density and grouping </h4>'),
                             hr(),
                             fluidRow(column(5,
                                             div(
                                               id="IDPanelCorrectimeKernelDensity_newSample",
                                               class = "well well-sm",
                                               
                                               fluidRow(column(10,
                                                               pickerInput(
                                                                 inputId = "SelectSample_KernelDensity_newSample",
                                                                 label = "Select a sample:",
                                                                 choices = NULL,
                                                                 options = pickerOptions(
                                                                   liveSearch = TRUE,
                                                                   size = 10,
                                                                   showTick = TRUE,
                                                                   style = "btn-primary"
                                                                 ),
                                                                 width = "100%")
                                               )
                                               )),
                                             ##~~~~~~~~~~~~~~~~~~ Parameters for filter density kernel ~~~~~~~~~~~~#
                                             ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                                             ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                                             div(
                                               class = "well well-sm",
                                               h4("Filter with kernel density:"),
                                               sliderInput("bandwidth_Filter_newSample",
                                                           "Bandwidth",
                                                           min = 1,
                                                           max = 500,
                                                           value = 20),
                                               fluidRow(column(6,
                                                               numericInput(
                                                                 inputId = "minDensity_newSample",
                                                                 label = "Min density",
                                                                 min = 0,
                                                                 max = 1,
                                                                 step = 0.01,
                                                                 value = 0.15)
                                               ),
                                               column(6,
                                                      hidden(
                                                        numericInput(
                                                          inputId = "gridSize_newSample",
                                                          label = "Grid size (n)",
                                                          value = 500)
                                                        
                                                      )
                                                      
                                               )
                                               )
                                               
                                             ),
                                             
                                             ##~~~~~~~~~~~~~~~~~~ Parameters for correction model with kernel density ~~~~~~~~~~~~#
                                             ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                                             ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                                             div(
                                               class = "well well-sm",
                                               h4("Correction model with Kernel density:"),
                                               selectInput(
                                                 inputId="KernelType_newSample",
                                                 label = "Kernel type: ",
                                                 choices=list("Gaussian" = "gaussian",
                                                              "Truncated gaussian" = "truncated gaussian",
                                                              "Epanechnikov" = "epanechnikov",
                                                              "Uniform" = "uniform")
                                               ),
                                               
                                               sliderInput("bandwidth_Model_newSample",
                                                           "Bandwidth:",
                                                           min = 1,
                                                           max = 1000,
                                                           value = 100),
                                               fluidRow(
                                                 column(6,
                                                        div(
                                                          class="pull-left",
                                                          style="display:inline-block",
                                                          
                                                          actionButton(inputId="fitModel_newSample",
                                                                       label="Adjust CE-time",
                                                                       icon=icon("rocket"),
                                                                       class="btn-primary")
                                                          
                                                        )),
                                                 column(6,
                                                        div(
                                                          class="pull-right",
                                                          style="display:inline-block",
                                                          
                                                          shinySaveButton(
                                                            id =  "IDSaveAdjustSamples",
                                                            label = "Save files ajusted",
                                                            title = "Save file as...",
                                                            filename = "adjusted",
                                                            filetype = list(text = "csv"),
                                                            viewtype = "icon",
                                                            icon = icon("save", lib = "glyphicon"),
                                                            class =
                                                              "btn-primary"
                                                          )                     
                                                          
                                                        )
                                                        )
                                               )
                                               
                                             ),
                                             
                                             fluidRow(
                                               column(6,
                                                      div(
                                                        class="pull-left",
                                                        style="display:inline-block",
                                                        actionButton(inputId="ReturToFilterSample_newSample",
                                                                     label="Return",
                                                                     class="btn-primary",
                                                                     icon = icon("arrow-left"))
                                                        
                                                      )),
                                               column(6,
                                                      div(
                                                        class="pull-right",
                                                        style="display:inline-block",
                                                        
                                                        actionButton(inputId="CorrectionKernelDensityNexPage_newSample",
                                                                     label="Next",
                                                                     class="btn-primary",
                                                                     icon = icon("arrow-right"))
                                                          
                                                      ))
                                             )
                                             
                             ),
                             column(7,
                                    
                                    tabsetPanel(
                                      id = "TabPanelKernelDensityFilter_ID_newSample",
                                      
                                      tabPanel(
                                        fluidRow(br()),
                                        title="CE-time Correction",
                                        
                                        
                                        ##~~~~~~~~~~~~~~~ Density filter ~~~~~~~~~~~~~###
                                        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                                        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                                        h4("Density filter:"),
                                        div(
                                          style = "position:relative",
                                          shinycssloaders::withSpinner(
                                            
                                            plotOutput("DensityFilterPlot_newSample",
                                                       width = "100%",
                                                       height = "450px"), 
                                            type = 1, size = 0.8)
                                        ),
                                        
                                        ##~~~~~~~~~~~~~~~ Correction time ~~~~~~~~~~~~~###
                                        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                                        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                                        
                                        ###~~~~~Before correction~~~~~~~~##
                                        fluidRow(br()),
                                        h4("Correction model:"),
                                        div(
                                          style = "position:relative",
                                          shinycssloaders::withSpinner(
                                            
                                            plotOutput("PlotCorrectionKernelDensity_Before_newSample",
                                                       width = "100%",
                                                       height = "225px",
                                                       hover = hoverOpts("PlotCorrectionKernelDensity_Before_hover_newSample", delay = 100, delayType = "debounce")
                                                       
                                            ), type = 1, size = 0.8),
                                          uiOutput("PlotCorrectionKernelDensity_Before_hover_info_newSample"),
                                          
                                        ),
                                        
                                        ###~~~~~After correction~~~~~~~~##
                                        div(
                                          style = "position:relative",
                                          shinycssloaders::withSpinner(
                                            
                                            plotOutput("PlotCorrectionKernelDensity_After_newSample",
                                                       width = "100%",
                                                       height = "225px",
                                                       hover = hoverOpts("PlotCorrectionKernelDensity_After_hover_newSample", delay = 100, delayType = "debounce")
                                                       
                                            ), type = 1, size = 0.8),
                                          uiOutput("PlotCorrectionKernelDensity_After_hover_info_newSample"),
                                          
                                        )
                                      ),
                                      
                                      tabPanel(
                                        fluidRow(br()),
                                        title="Distribution: M+H vs CE-Time",
                                        
                                        div(
                                          style = "position:relative",
                                          shinycssloaders::withSpinner(
                                            
                                            plotOutput("CorrectimeKernelDensityViewer_2_newSample",
                                                       width = "100%",
                                                       height = "650px"), 
                                            type = 1, size = 0.8)
                                        )
                                        
                                      )
                                    )
                                    
                                    
                             )
                             )
             )
             )
    )
    
  )
  
}

