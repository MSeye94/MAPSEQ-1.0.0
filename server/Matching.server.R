#---------------------------------------------------------------#
#----- Matching to the reference file (compute auto) -----------#
#---------------------------------------------------------------#


#---------------------------------------------------------------#
#--- Matching to the reference file (when click the button) -----#
#---------------------------------------------------------------#
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {c(
    input$IDMatchingButton
  )
    
  },
  handlerExpr = {
    if(!is.null(rVars$features_samples_KernelDensityCorrection) &
       !is.null(rVars$ref_data_valid)){
      source("lib/lib.R", local=TRUE)
      
      
      
      withProgress(message = 'Matching samples :', value = 0, {
        res_match_list<-list()
        for (i in 1:length(rVars$features_samples_KernelDensityCorrection)) {
          
          incProgress(1/length(rVars$features_samples_KernelDensityCorrection), 
                      detail = paste(names(rVars$features_samples_KernelDensityCorrection)[i],"..."))
         
           res_match_list[[i]]<-matchMzRt(x = req(rVars$ref_data_valid),
                                         table = req(rVars$features_samples_KernelDensityCorrection[[i]]),
                                         ppm_tolereance = req(input$massToleranceID),
                                         rt_tolerance = req(input$CEToleranceID),
                                         mzcol = "M.H",
                                         rtcol = "CE.time",
                                         session = session)
        }
        
      })
      
      names(res_match_list)<-names(rVars$features_samples_KernelDensityCorrection)
      for (i in 1:length(res_match_list)) {
        
        res_match_list[[i]]$MatchTable$Match<-"No match"
        
        if(is.element(TRUE,!is.na(res_match_list[[i]]$MatchTable$IDSample))){
          
          res_match_list[[i]]$MatchTable[!is.na(res_match_list[[i]]$MatchTable$IDSample),]$Match<-"Matched"
        }  
        
        res_match_list[[i]]$MatchTable$Match<-factor(res_match_list[[i]]$MatchTable$Match, levels = c("No match", "Matched"))
      }
      
      
      rVars$res_match_data_list<-res_match_list
      
      res_match_toSave_list<-list()
      for (i in 1:length(rVars$res_match_data_list)) {
        
        res_match_toSave_list[[i]]<-res_match_list[[i]]$MatchTable
      }
      
      names(res_match_toSave_list)<-names(res_match_list)
      
      
      rVars$res_match_toSave<-do.call("rbind", res_match_toSave_list)
      
      rownames(rVars$res_match_toSave)<-1:nrow(rVars$res_match_toSave)
      
      rVars$res_match_toSave<-rVars$res_match_toSave[order(rVars$res_match_toSave$M.H1),]
      
      
      
    }
  })




#---------------------------------------------------------------#
#---------------------- Select sample to view -------------------#
#---------------------------------------------------------------#
observe({
  if (!is.null(names(rVars$res_match_data_list))) {
  
    
    
    output$selectSampleMatchedOutput <- renderUI({
      pickerInput(
        inputId = "selectSampleMatched",
        label = "Select/deselect sample(s) (to viewer):",
        choices = names(rVars$res_match_data_list),
        options = pickerOptions(
          size = 10,
          liveSearch = TRUE,
          showTick = TRUE,
          style = "btn-primary"
        ),
        width = "fit"
      )
    })
    
    
  } 
  # else{
  #   output$SelectSampleCut <- renderUI({
  #     pickerInput(
  #       inputId = "selectSampleMatched",
  #       label = "Select/deselect sample(s) (to viewer):",
  #       choices = NULL,
  #       options = pickerOptions(
  #         size = 10,
  #         liveSearch = TRUE,
  #         showTick = TRUE,
  #         style = "btn-primary"
  #       ),
  #       width = "fit"
  #     )
  #   })
  # 
  #   rVars$res_match_data_list <- NULL
  # }
  
  
})


#---------------------------------------------------------------#
#------------------ Plotting matching results -------------------#
#---------------------------------------------------------------#
rangesZoommatchViewer_matchPlot <- reactiveValues(x = NULL, y = NULL)

# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
observeEvent({input$matchPlot_dblclick},{
  brush <- input$matchPlot_brush
  if (!is.null(brush)) {
    rangesZoommatchViewer_matchPlot$x <- c(brush$xmin, brush$xmax)
    rangesZoommatchViewer_matchPlot$y <- c(brush$ymin, brush$ymax)
    
  } else {
    rangesZoommatchViewer_matchPlot$x <- NULL
    rangesZoommatchViewer_matchPlot$y <- NULL
  }
})


output$matchPlot<-renderPlot({
  
  if(!is.null(input$selectSampleMatched) &
     !is.null(rVars$res_match_data_list)){
    
    data_match<-rVars$res_match_data_list[input$selectSampleMatched][[1]]
    
    par(fig = c(0,1,0,1),mar=c(4,4,5,0))
    plot(x = data_match$MatchTable$CE.time1,
         y = data_match$MatchTable$M.H1,
         axes = T, pch=20, col= "gray", cex= 1,
         xlab = paste("Ce-time(Second)"),
         ylab = "M+H(Da)",
         xlim = rangesZoommatchViewer_matchPlot$x,
         ylim = rangesZoommatchViewer_matchPlot$y,
         main = paste("Match: Reference file vs ",req(input$selectSampleMatched),"\n",
                      "Number of features matched :",data_match$numberMatch,"\n",
                      "% of matched rapported to sample :", data_match$percentageMatch,"%"),
         col.main = "#760001",
         
         font.main = 2,
         cex.lab = 1.2,
         font.lab = 2,
         cex.axis = 1.1,
         font.axis = 2,
         cex.main = 1.2)
    
    
    
    
    points(x = data_match$MatchTable[data_match$MatchTable$Match == "Matched",]$CE.time1,
           y = data_match$MatchTable[data_match$MatchTable$Match == "Matched",]$M.H1,
           pch=20, col= "blue"#, cex=input$sizePointsMatch,
    )
    
    # axis(1,col="black",col.axis="black",
    #      font = 2, cex.lab = 1.3, cex.axis = 1.2, lwd = 2, line = 0)
    # # mtext("Ce-time(second)",side=1,line=3,col="black", font = 2, cex=1.2)
    # #
    # axis(2,col="black",col.axis="black",
    #      font = 2, cex.lab = 1.3, cex.axis = 1.2, lwd = 2, line = 0)
    # # mtext("M+H(Da)",side=2,line=3,col="black", font = 2, cex=1.2)
    
    legend("topleft", pch = 20, pt.cex = 2, inset = 0, text.font = 2.3,
           legend = levels(data_match$MatchTable$Match),  bty = "n", xpd = NA, cex = 1.2,
           col = c("gray", "blue"), horiz=FALSE)
    grid(nx = 14)
  }
  
 
})

#---------------Info bull-----------------#
output$matchPlot_hover_info <- renderUI({
  hover <- req(input$matchPlot_hover)
  if(!is.null(input$selectSampleMatched) &
     !is.null(rVars$res_match_data_list)){
    
    data_match_table<-rVars$res_match_data_list[input$selectSampleMatched][[1]]$MatchTable
    point <-
      suppressWarnings(
        nearPoints(
            data_match_table,
          hover,
          xvar = "CE.time1",
          yvar = "M.H1",
          threshold = 5,
          maxpoints = 1,
          addDist = TRUE
        )
      )
    if (nrow(point) == 0)
      return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <-
      (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <-
      (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <-
      hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <-
      hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    # style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    #                 "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    style <-
      paste0(
        "position:absolute; z-index:100; background-color: #760001; color: white;",
        "left:",
        left_px + 2,
        "px; top:",
        top_px + 2,
        "px;"
      )
    
    # actual tooltip created as wellPanel
    div(class = "well well-sm",
        style = style,
        p(HTML(
          paste0(
            "<span class='bullText'> M+H: </span>",
            round(point$M.H1, 4),
            "<br/>",
            "<span class='bullText'> CE-time: </span>",
            round(point$CE.time1, 2),
            "<br/>"
          )
        )))
    
  }
  
  
})



#---------------------------------------------------------------#
#--------------------- Saving match results ---------------------#
#---------------------------------------------------------------#

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$IDSaveMatchingSamples
  }
  ,
  handlerExpr = {
    
    
    shinyFileSave(input,
                  id = "IDSaveMatchingSamples",
                  roots = volumes,
                  session = session)
    
    path_Save_Files_origine<-parseSavePath(volumes, input$IDSaveMatchingSamples)$datapath
    
    
    if(length(path_Save_Files_origine)>0){
      
      
      path_Save_Files<-strsplit(path_Save_Files_origine, split = "/")[[1]]
      directory_Save_Files<-paste0(path_Save_Files[-length(path_Save_Files)], collapse = "/")
      
      if(!is.null(rVars$res_match_toSave)){
        
        data_match_toSave<-rVars$res_match_toSave
        

        
        write.table(data_match_toSave, 
                    file = paste0(c(directory_Save_Files, 
                                    paste(
                                      path_Save_Files[length(path_Save_Files)],
                                      collapse = "-", sep = "-")),
                                  collapse = "/"),
                    sep = ",", row.names = FALSE)
      }
      
    }
    
  })