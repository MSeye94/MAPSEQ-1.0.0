TabPanelMatchingSequence <- function() {
  fluidRow(column(
    12,
    HTML(
      '<h4>Matching sequences to reference file</h4>'
    ),
    hr(),
    fluidRow(column(
      4,
      div(class = "well well-sm",
          h4("Parameters of matching"),
          hr(),
          fluidRow(column(
            12,
            fluidRow(column(
              6,
              numericInput(
                inputId = "CEToleranceID",
                label =
                  "CE-time tolerence (Second)",
                value =
                  3 * 60,
                min = 0,
                step = 1
              )
            ),
            column(
              6,
              
              numericInput(
                inputId = "massToleranceID",
                label = "mass tolerance (ppm)",
                value = 50,
                min = 0,
                step = 1
              )
            ))
          )),
          hr(),
          
          fluidRow(
            column(6,
                   div(
                     class="pull-left",
                     style="display:inline-block",
                     
                     actionButton(inputId="IDMatchingButton",
                                  label="Matching to the reference file",
                                  icon=icon("rocket"),
                                  class="btn-primary")
                     
                   )),
            column(6,
            )
          ),
          hr(),
          fluidRow(
            column(6,
                   div(
                     class="pull-left",
                     style="display:inline-block",
                     
                     shinySaveButton(
                       id =  "IDSaveMatchingSamples",
                       label = "Save match results",
                       title = "Save file as...",
                       filename = "Matched-Table",
                       filetype = list(text = "csv"),
                       viewtype = "icon",
                       icon = icon("save", lib = "glyphicon"),
                       class =
                         "btn-primary"
                     )                     
                     
                   )
                   
                   ),
            column(6,
            )
          )
          
          
          )
    ),
    column(8,
           fluidRow(column(10,
                           uiOutput("selectSampleMatchedOutput"), )),
           hr(),
           div(
             style = "position:relative",
             shinycssloaders::withSpinner(plotOutput("matchPlot",
                                                     width = "100%",
                                                     height = "550px",
                                                     dblclick = "matchPlot_dblclick",
                                                     brush = brushOpts(
                                                       id = "matchPlot_brush",
                                                       resetOnNew = TRUE
                                                     ),
                                                     hover = hoverOpts("matchPlot_hover", 
                                                                       delay = 100, delayType = "debounce")
             ), 
             type = 1, size = 0.8,
             id = "Id_matchPlot"),
             uiOutput("matchPlot_hover_info")
           )
           )
    )
  ))
  
}