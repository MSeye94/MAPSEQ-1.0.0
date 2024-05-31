#---------------------------------------------------------------#
#------------------- Upload PSMs Files -------------------------#
#---------------------------------------------------------------#
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$PSMsFilesID
  }
  ,{
    
    files <- input$PSMsFilesID
    ext <- tools::file_ext(files$datapath)
    
    
    if (!is.element(ext, c("csv","xlsx", "txt"))) {
      message("File extension must be csv, xlsx, txt....!")
      
      sendSweetAlert(
        session = session,
        title = "Warning !",
        text = "File extension must be: csv, xlsx or txt !",
        type = "warning"
      )
    } else {
      if(ext=="csv"){
        
        observe({
          rVars$psms_file<-read.csv(input$PSMsFilesID$datapath, sep = req(input$delim_psms_file))
        })
        
        output$psmsDataView<-renderDT({
          
          if(!is.null(rVars$psms_file)){
            datatable(
              req(rVars$psms_file, 
                  options = list(scrollX = TRUE))
            )
          }
        })
        
        showModal(modalDialog(
          title = "View Data",
          
          h4("Options"),
          awesomeRadio(
            inputId="delim_psms_file",
            label = "Delimiter :",
            inline=FALSE,
            checkbox = TRUE,
            choices=list("Comma" = ",",
                         "Semicolon" = ";",
                         "Tab" = "\t")
          ),
          div(
            DTOutput("psmsDataView",
                     width = "100%"),
            style = "overflow: auto;"
          ),
          
          size = "l", easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
        ))
        
        
      } else if(ext=="xlsx"){
        
        observe({
          rVars$psms_file<-read_excel(input$PSMsFilesID$datapath)
        })
        
        output$psmsDataView<-renderDT({
          
          if(!is.null(rVars$psms_file)){
            datatable(
              req(rVars$psms_file, 
                  options = list(scrollX = TRUE))
            )
          }
        })
        
        
        showModal(modalDialog(
          title = "View Data",

          div(
            DTOutput("psmsDataView",
                     width = "100%"),
            style = "overflow: auto;"
          ),
          
          size = "l", easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
        ))
        
        
      } else{
        
        observe({
          rVars$psms_file<-fread2(input$PSMsFilesID$datapath,
                                  data.table = TRUE
                                  )
        })
        
        output$psmsDataView<-renderDT({
          
          if(!is.null(rVars$psms_file)){
            datatable(
              req(rVars$psms_file, 
                  options = list(scrollX = TRUE))
            )
          }
        })
        
        
        showModal(modalDialog(
          title = "View Data",
          
          div(
            DTOutput("psmsDataView",
                     width = "100%"),
            style = "overflow: auto;"
          ),
          
          size = "l", easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
        ))
      }
    }
  
    })
      



#----------------------- Update input columnSelected --------------------------#
observe({
  if (!is.null(rVars$psms_file)) {
    
    
    output$columnSelectedOutput <- renderUI({
      pickerInput(
        inputId = "columnSelected",
        label = "Select columns to use:",
        choices = colnames(rVars$psms_file),
        options = pickerOptions(
          actionsBox = TRUE,
          size = 10,
          #selectedTextFormat = "count",
          #showTick = TRUE,
          style = "btn-primary"
        ),
        multiple = TRUE
      )
    })
    
    
  } else{
    output$columnSelectedOutput <- renderUI({
      pickerInput(
        inputId = "columnSelected",
        label = "Select columns to use:",
        choices = NULL,
        options = pickerOptions(
          actionsBox = TRUE,
          size = 10,
          #selectedTextFormat = "count",
          #showTick = TRUE,
          style = "btn-primary"
        ),
        multiple = TRUE
      )    
      })
  }
  
  
})


#----------------------- Column to select -----------------------#
observe({
    
    if(!is.null(input$columnSelected) &
       !is.null(rVars$psms_file)){
      
      
      rVars$psms_file_formatted<-rVars$psms_file %>%
        dplyr::select(any_of(input$columnSelected))

    } else {
      
      rVars$psms_file_formatted<-NULL
      
    }
    
    })

output$DataTableSelected <- renderDT({
  
  
  if(!is.null(rVars$psms_file_formatted)){
    datatable(rVars$psms_file_formatted,
              rownames = FALSE,
              options = list(scrollX = TRUE))
  }
  
})


#----------------------- Saving selected Data -----------------------#

observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$IDSaveFormatPSMsFile
             }
             ,
             handlerExpr = {
               shinyFileSave(input,
                             id = "IDSaveFormatPSMsFile",
                             roots = volumes,
                             session = session)
               
               
               
               path_Save_Files_origine <-
                 parseSavePath(volumes, input$IDSaveFormatPSMsFile)$datapath
               
               if (length(path_Save_Files_origine) > 0) {
                 path_Save_Files <- strsplit(path_Save_Files_origine, split = "/")[[1]]
                 directory_Save_Files <-
                   paste0(path_Save_Files[-length(path_Save_Files)], collapse = "/")
                 
                 if (!is.null(rVars$psms_file_formatted) & 
                     !is.null(input$PSMsFilesID$name)) {
                   psms_file_formatted <- rVars$psms_file_formatted
                   

                   # Format csv
                   write.table(psms_file_formatted,
                               file = paste0(c(directory_Save_Files,
                                               paste(
                                                 str_remove_all(req(input$PSMsFilesID$name), 
                                                                pattern = paste0(".",tools::file_ext(input$PSMsFilesID$datapath))),
                                                 #req(input$PSMsFilesID$name),
                                                 path_Save_Files[length(path_Save_Files)],
                                                 collapse = "", sep = "-")), collapse = "/"),
                               sep = ",", row.names = FALSE)
                   
                   print("Save ok")
                   
                 }
                 
               }
               
               
             })

#---------------------------------------------------------------#
#------------------- Upload files ------------------------------#
#---------------------------------------------------------------#

#------------- Upload ref data ----------#
#----------------------------------------#
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$refData
  }
  ,{
    
    files <- input$refData
    ext <- tools::file_ext(files$datapath)
    
    
    if (ext != "csv") {
      message("Please upload a csv file...!")
      
      sendSweetAlert(
        session = session,
        title = "Warning !",
        text = "Please upload a csv file !",
        type = "warning"
      )
    } else {
      
      observe({
        
        
        rVars$ref_data<-read.csv(input$refData$datapath, sep = req(input$delim_ref))
        
        #----------------------- Verify necessaries columns --------------------#
        if(!is.element("M.H", colnames(rVars$ref_data))||
           !is.element("CE.time", colnames(rVars$ref_data))){
          
          sendSweetAlert(
            session = session,
            title = "Warning !",
            text = "Required columns : 'M+H', 'CE-time', not found in file.",
            type = "warning"
          )
          
          rVars$ref_data_valid<-NULL
          Valid_ref = FALSE
        }
      })
      
      output$refDataView<-renderDT({
        if(!is.null(rVars$ref_data)){
          datatable(
            req(rVars$ref_data, 
                options = list(scrollX = TRUE))
          )
        }
        
      })
      
      showModal(modalDialog(
        title = "View Data",
        
        div(
          class = "well well-sm",
          h4("Options"),
          awesomeRadio(
            inputId="delim_ref",
            label = "Delimiter :",
            inline=FALSE,
            checkbox = TRUE,
            choices=list("Comma" = ",",
                         "Semicolon" = ";",
                         "Tab" = "\t")
          ),
          div(
            DTOutput("refDataView",
                     width = "100%"),
            style = "overflow: auto;"
          )
        ),
        
        size = "l", easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
      ))
      
      observe({
        if(!is.null(rVars$ref_data) &
          rVars$Valid_ref){
          rVars$ref_data_valid <- rVars$ref_data
        } else {
          rVars$ref_data_valid<-NULL
        }
        
      })
    }
    
  })

#------------- Upload samples ----------#
#----------------------------------------#
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$Samples
  }
  ,{
    
    files <- input$Samples
    ext <- tools::file_ext(files$datapath)
    
    if(length(ext) > 1){
      if (all.equal(ext, rep("csv", length(ext))) != TRUE) {
        sendSweetAlert(
          session = session,
          title = "Warning !",
          text = "Please upload a csv file !",
          type = "warning"
        )
      } else{
        observe({
          
          data_list<-lapply(X = input$Samples$datapath, read.csv, sep = req(input$delim_samples))
          
          tryCatch({
            rVars$samples_data <- do.call("rbind", data_list)
          },
          error = function(e) {
            sendSweetAlert(
              session = session,
              title = "Warning !",
              text = "Required columns : 'M+H', 'CE-time', 'sample',not found in file.",
              type = "warning"
            )
            
            rVars$samples_data<-NULL
            rVars$samples_data_list<-NULL
            rVars$features_samples_list<-NULL
            rVars$features_samples_KernelDensityCorrection<-NULL
          })
          
          
          
          #rVars$samples_data<-read.csv(input$Samples$datapath, sep = req(input$delim_samples)) 
          
          #----------------------- Verify necessaries columns --------------------#
          if(!is.element("M.H", colnames(rVars$samples_data))||
             !is.element("CE.time", colnames(rVars$samples_data))||
             !is.element("sample", colnames(rVars$samples_data))){
            
            sendSweetAlert(
              session = session,
              title = "Warning !",
              text = "Required columns : 'M+H', 'CE-time', 'sample', not found in file.",
              type = "warning"
            )
            
            rVars$samples_data_list<-NULL
            rVars$features_samples_list<-NULL
            rVars$features_samples_KernelDensityCorrection<-NULL
          }
        })
        
        output$SamplesDataView<-renderDT({
          datatable(
            req(rVars$samples_data, 
                options = list(scrollX = TRUE))
          )
          
        })
        
        showModal(modalDialog(
          title = "View Data",
          
          div(
            class = "well well-sm",
            h4("Options"),
            awesomeRadio(
              inputId="delim_samples",
              label = "Delimiter :",
              inline=FALSE,
              checkbox = TRUE,
              choices=list("Comma" = ",",
                           "Semicolon" = ";",
                           "Tab" = "\t")
            ),
            div(
              DTOutput("SamplesDataView",
                       width = "100%"),
              style = "overflow: auto;"
            )
          ),
          
          size = "l", easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
        ))
        
        observe({
          if(!is.null(rVars$samples_data)){
            rVars$samples_data_list <- req(split(req(rVars$samples_data), f = req(rVars$samples_data$sample)))
          } else {
            rVars$samples_data_list<-NULL
            rVars$features_samples_list<-NULL
            rVars$features_samples_KernelDensityCorrection<-NULL
          }
          
        })
      }
      
    } else{
      if (ext != "csv") {
        message("Please upload a csv file...!")
        
        sendSweetAlert(
          session = session,
          title = "Warning !",
          text = "Please upload a csv file !",
          type = "warning"
        )
      } else {
        
        observe({
          
          data_list<-lapply(X = input$Samples$datapath, read.csv, sep = req(input$delim_samples))
          
          tryCatch({
            rVars$samples_data <- do.call("rbind", data_list)
          },
          error = function(e) {
            sendSweetAlert(
              session = session,
              title = "Warning !",
              text = "Required columns : 'M+H', 'CE-time', 'sample',not found in file.",
              type = "warning"
            )
            rVars$samples_data<-NULL
            rVars$samples_data_list<-NULL
            rVars$features_samples_list<-NULL
            rVars$features_samples_KernelDensityCorrection<-NULL
          })
          
          
          
          #rVars$samples_data<-read.csv(input$Samples$datapath, sep = req(input$delim_samples)) 
          
          #----------------------- Verify necessaries columns --------------------#
          if(!is.element("M.H", colnames(rVars$samples_data))||
             !is.element("CE.time", colnames(rVars$samples_data))||
             !is.element("sample", colnames(rVars$samples_data))){
            
            sendSweetAlert(
              session = session,
              title = "Warning !",
              text = "Required columns : 'M+H', 'CE-time', 'sample', not found in file.",
              type = "warning"
            )
            
            
            rVars$samples_data_list<-NULL
            rVars$features_samples_list<-NULL
            rVars$features_samples_KernelDensityCorrection<-NULL
          }
        })
        
        output$SamplesDataView<-renderDT({
          datatable(
            req(rVars$samples_data, 
                options = list(scrollX = TRUE))
          )
          
        })
        
        showModal(modalDialog(
          title = "View Data",
          
          div(
            class = "well well-sm",
            h4("Options"),
            awesomeRadio(
              inputId="delim_samples",
              label = "Delimiter :",
              inline=FALSE,
              checkbox = TRUE,
              choices=list("Comma" = ",",
                           "Semicolon" = ";",
                           "Tab" = "\t")
            ),
            div(
              DTOutput("SamplesDataView",
                       width = "100%"),
              style = "overflow: auto;"
            )
          ),
          
          size = "l", easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
        ))
        
        observe({
          if(!is.null(rVars$samples_data)){
            rVars$samples_data_list <- req(split(req(rVars$samples_data), f = req(rVars$samples_data$sample)))
          } else {
            rVars$samples_data_list<-NULL
            rVars$features_samples_list<-NULL
            rVars$features_samples_KernelDensityCorrection<-NULL
          }
          
        })
      }
    }
    
    
  })

#----------------------------------------------------------------------#
#-----------Manager button FilterDensityNexPage_newSample-------------#
#----------------------------------------------------------------------#
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$FilterDensityNexPage_newSample
             },
             handlerExpr = {
               
               updateTabsetPanel(
                 session = session,
                 inputId = "IDTabsetPanelFilterSampleCEtimeCorrection",
                 selected = "CE-time correction"
               )
               
               
             })

#-------------------------------------------------------------------------------#
#-----------Manager button CorrectionKernelDensityNexPage_newSample-------------#
#--------------------------------------------------------------------------------#
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$CorrectionKernelDensityNexPage_newSample
             },
             handlerExpr = {
               
               updateTabsetPanel(
                 session = session,
                 inputId = "IDNavbar",
                 selected = "Matching sequences"
               )
               
               
             })

#----------------------------------------------------------------------#
#-----------Manager button ReturToFilterSample_newSample-------------#
#----------------------------------------------------------------------#
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$ReturToFilterSample_newSample
             },
             handlerExpr = {
               
               updateTabsetPanel(
                 session = session,
                 inputId = "IDTabsetPanelFilterSampleCEtimeCorrection",
                 selected = "Upload files and filter CE-timeter"
               )
               
               
             })