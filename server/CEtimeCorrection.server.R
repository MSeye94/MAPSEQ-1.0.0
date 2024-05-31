#---------------------------------------------------------------#
#------------ CE-time correction (kernel Density) --------------#
#---------------------------------------------------------------#

#----- update input 'SelectSample_KernelDensity_newSample' -------#

observe({
  if (!is.null(data_list_selectedCutting()) &
      !is.null(rVars$ref_data_valid)) {
    sample_name <-
      names(data_list_selectedCutting())
    
    rVars$features_samples_list <-
      data_list_selectedCutting()
    
    peaks <-
      rVars$features_samples_list
    rVars$features_samples_KernelDensityCorrection <-
      peaks
    
    updatePickerInput(session = session,
                      inputId = "SelectSample_KernelDensity_newSample",
                      choices = sample_name)
    
    
    
  } else {
    
    rVars$features_samples_list <-
      data_list_selectedCutting()
    
    peaks <-
      rVars$features_samples_list
    rVars$features_samples_KernelDensityCorrection <-
      peaks
    
    updatePickerInput(
      session = session,
      inputId = "SelectSample_KernelDensity_newSample",
      choices = character(0),
      selected = character(0)
    )
    
    
  }
})



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~ Forced reactive Kernel density filter when choosing samples when cutting sample~~#
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {c(
    input$cutOff_newSample
  )
  },
  handlerExpr = {
    if(!is.null(data_list_selectedCutting()) &
       !is.null(rVars$ref_data_valid)){

      sample_name<-names(data_list_selectedCutting())

      rVars$features_samples_list<-data_list_selectedCutting()

      updatePickerInput(session = session,
                        inputId = "SelectSample_KernelDensity_newSample",
                        choices = character(0),
                        selected = character(0))

      updatePickerInput(session = session,
                        inputId = "SelectSample_KernelDensity_newSample",
                        choices = sample_name)
    } 
  })

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~~~~~~ Kernel density filter when choosing samples ~~~~~~~~~#
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$SelectSample_KernelDensity_newSample
  },
  handlerExpr = {
    
    if(!is.null(input$SelectSample_KernelDensity_newSample) &
       !is.null(rVars$features_samples_list) & 
       !is.null(rVars$ref_data_valid)){
      
      ##Matched samples
      
      source("lib/lib.R", local=TRUE)
      
      ref<-rVars$ref_data_valid[, c("M.H","CE.time")]
      
      table.newSample<-rVars$features_samples_list[[input$SelectSample_KernelDensity_newSample]]
      table.newSample<-table.newSample[,c("M.H","CE.time","sample")]
      
      # resMatch.newSample<-matchMz(x = ref,
      #                             table = table.newSample,
      #                             ppm_tolereance = 500,
      #                             mzcol = "M.H",
      #                             rtcol = "CE.time" ,
      #                             session = session)
      
      resMatch.newSample<-matchMzRt.V2(x = ref,
                                  table = table.newSample,
                                  ppm_tolereance = 150,
                                  rt_tolerance = 750,
                                  mzcol = "M.H",
                                  rtcol = "CE.time" ,
                                  session = session)
      
      
      
      rVars$Data_Plot.newSample<-resMatch.newSample$MatchTable[!is.na(resMatch.newSample$MatchTable$CE.time.2),
                                                                                     c("M.H.1","CE.time.1","M.H.2","CE.time.2","sample.2")]
      
     
      
      
      median_line.newSample<-seq(min(range(rVars$Data_Plot.newSample$CE.time.2)[1], 
                                     range(rVars$Data_Plot.newSample$CE.time.1)[1]),
                                 max(range(rVars$Data_Plot.newSample$CE.time.2)[2], 
                                     range(rVars$Data_Plot.newSample$CE.time.1)[2]), by = 50)
      
      median_line_data.newSample<-data.frame(x =  median_line.newSample, 
                                             y  =  median_line.newSample)
      
      ####~~~~~~~~~~~~~~~~ Correction with Kernel Density ~~~~~~~~~~~~~~~~~~~~###
      
      #~~~~~~~~~~~~~~~~~~~~ Density filter ~~~~~~~~~~~~~~~~~~#
      data_dens_filter.newSample<-reactive(
        if(!is.null(rVars$Data_Plot.newSample) &
           !is.null(data_list_selectedCutting()) &
           !is.null(rVars$features_samples_list)){
          dens<-kde2d(rVars$Data_Plot.newSample$CE.time.2, rVars$Data_Plot.newSample$CE.time.1, h = input$bandwidth_Filter_newSample, 
                      n = input$gridSize_newSample)
          nx <- nrow(rVars$Data_Plot.newSample)
          df <- expand.grid(x = dens$x, y = dens$y)
          df$density <- (as.vector(dens$z))
          df$density<-df$density/max(df$density)
          #df$group <- data$group[1]
          df$ndensity <- df$density / max(df$density, na.rm = TRUE)
          df$count <- nx * df$density
          df$n <- nx
          df$level <- 1
          df$piece <- 1
          df_filter<-df %>%
            dplyr::filter(density>=req(input$minDensity_newSample))
          
          colnames(df_filter)[1:2]<-c("CE.time.2","CE.time.1")
          
          df_filter
        }
      )
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~ Plot filter density ~~~~~~~~~~~~~~~~~~~~~~~#
      output$DensityFilterPlot_newSample <- renderPlot({
        if(!is.null(data_dens_filter.newSample())){
          
          p1<-rVars$Data_Plot.newSample %>% 
            ggplot() +
            aes(x=CE.time.2, y=CE.time.1) +#, colour = maxo.2) + 
            geom_point(size = 0.5) +
            geom_density_2d(h = input$bandwidth_Filter_newSample, n = input$gridSize_newSample)+
            #scale_color_viridis_c(option = "inferno", direction = -1) +
            scale_x_continuous(n.breaks = 14)+
            scale_y_continuous(n.breaks = 14)+
            
            coord_cartesian(xlim =c(min(range(rVars$Data_Plot.newSample$CE.time.2)[1], 
                                        range(rVars$Data_Plot.newSample$CE.time.1)[1]),
                                    max(range(rVars$Data_Plot.newSample$CE.time.2)[2], 
                                        range(rVars$Data_Plot.newSample$CE.time.1)[2])),
                            
                            ylim = c(min(range(rVars$Data_Plot.newSample$CE.time.2)[1], 
                                         range(rVars$Data_Plot.newSample$CE.time.1)[1]),
                                     max(range(rVars$Data_Plot.newSample$CE.time.2)[2], 
                                         range(rVars$Data_Plot.newSample$CE.time.1)[2]))) +
            
            
            xlab(paste("CE-time (", rVars$Data_Plot.newSample$sample.2,")"))+
            ylab("CE-time (reference sample)")+
            #labs(colour ="log2 Intensity")+
            theme_ben()
          
          p2<-ggplot(data_dens_filter.newSample(), aes(x=CE.time.2, y=CE.time.1)) +
            geom_point(aes(color = density), size = 0.5) +
            
            scale_x_continuous(n.breaks = 14)+
            scale_y_continuous(n.breaks = 14)+
            
            coord_cartesian(xlim =c(min(range(data_dens_filter.newSample()$CE.time.2)[1], 
                                        range(data_dens_filter.newSample()$CE.time.1)[1]),
                                    max(range(data_dens_filter.newSample()$CE.time.2)[2], 
                                        range(data_dens_filter.newSample()$CE.time.1)[2])),
                            
                            ylim = c(min(range(data_dens_filter.newSample()$CE.time.2)[1], 
                                         range(data_dens_filter.newSample()$CE.time.1)[1]),
                                     max(range(data_dens_filter.newSample()$CE.time.2)[2], 
                                         range(data_dens_filter.newSample()$CE.time.1)[2])))+
            
            xlab(paste("CE-time (", rVars$Data_Plot.newSample$sample.2,")"))+
            ylab("CE-time (Reference map)")+
            
            theme_ben()+
            theme(legend.title = element_text(size = rel(0.95), face = "bold.italic", hjust = 0.5),
                  legend.text = element_text(size = rel(0.85), face = "bold.italic"))
          
          grid.arrange(p1, p2, nrow = 2)
          
        }
        
      })
      
    }
    
  })


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~~~~~~~~~~~~~~ Fit model kernel density estimation ~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {c(
    input$fitModel_newSample,
    input$SelectSample_KernelDensity_newSample
  )
    
  },
  handlerExpr = {
    
    #~~~~~~~~~~~~~~~~~~~~ Density filter ~~~~~~~~~~~~~~~~~~#
    data_dens_filter.newSample<-reactive(
      if(!is.null(rVars$Data_Plot.newSample) &
         !is.null(data_list_selectedCutting()) &
         !is.null(rVars$features_samples_list)){
        dens<-kde2d(rVars$Data_Plot.newSample$CE.time.2, rVars$Data_Plot.newSample$CE.time.1, h = input$bandwidth_Filter_newSample, 
                    n = input$gridSize_newSample)
        nx <- nrow(rVars$Data_Plot.newSample)
        df <- expand.grid(x = dens$x, y = dens$y)
        df$density <- (as.vector(dens$z))
        df$density<-df$density/max(df$density)
        #df$group <- data$group[1]
        df$ndensity <- df$density / max(df$density, na.rm = TRUE)
        df$count <- nx * df$density
        df$n <- nx
        df$level <- 1
        df$piece <- 1
        df_filter<-df %>%
          dplyr::filter(density>=req(input$minDensity_newSample))
        
        colnames(df_filter)[1:2]<-c("CE.time.2","CE.time.1")
        
        df_filter
      }
    )
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~ Fit model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    if(!is.null(data_dens_filter.newSample()) &
       !is.null(data_list_selectedCutting()) &
       !is.null(rVars$features_samples_list)){
      
      
      
      dataDensity<-isolate(data_dens_filter.newSample()) 
      
      if(!is.null(dataDensity)){
        
        withProgress(message = 'Fit model...', value = 0, {
          incProgress(1/3, detail = "")
          
          #~~~~~~~~~~~~~~~~~ Kernel density estimation ~~~~~~~~~~~~~~~~~~~~~~~#
          rVars$modelKernelDensity<-npreg(CE.time.1 ~ CE.time.2,
                                                                bws = req(input$"bandwidth_Model_newSample"),
                                                                bwtype = c("fixed","generalized_nn","adaptive_nn")[1],
                                                                regtype = "ll",
                                                                ckertype = input$KernelType_newSample,
                                                                #bwmethod = "cv.aic",
                                                                gradients = TRUE,
                                                                data = dataDensity)
          
          #~~~~~~~~~~~~~~~~~ Calculate prediction rt for sample selected ~~~~~~~~~~~~~~~~~~~~~~~#
          if(!is.null(rVars$modelKernelDensity)){
            incProgress(1/3, detail ="")
            
            ### correction rt new sample Kernel Density
            rVars$features_samples_KernelDensityCorrection[[input$SelectSample_KernelDensity_newSample]]$CE.time<-
              predict(rVars$modelKernelDensity, newdata = data.frame(CE.time.2 = rVars$features_samples_list[[input$SelectSample_KernelDensity_newSample]]$CE.time))
            
            ### Delete negative correction rt

            ## ~~~~~~~~~~~~~~~~~~ Plot correction rt with kernel Density ~~~~~~~~~~~~~#
            
            ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Before~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            output$PlotCorrectionKernelDensity_Before_newSample <- renderPlot({
              
              predict_data_model.np<-data.frame(CE.time.1 = predict(rVars$modelKernelDensity,
                                                               newdata = data.frame(CE.time.2 = seq(min(range(rVars$Data_Plot.newSample$CE.time.2)[1],
                                                                                                   range(rVars$Data_Plot.newSample$CE.time.1)[1]),
                                                                                               max(range(rVars$Data_Plot.newSample$CE.time.2)[2],
                                                                                                   range(rVars$Data_Plot.newSample$CE.time.1)[2])), by = 50)),
                                                CE.time.2 = seq(min(range(rVars$Data_Plot.newSample$CE.time.2)[1],
                                                               range(rVars$Data_Plot.newSample$CE.time.1)[1]),
                                                           max(range(rVars$Data_Plot.newSample$CE.time.2)[2],
                                                               range(rVars$Data_Plot.newSample$CE.time.1)[2])), by = 50)
              
              median_line<-seq(min(range(rVars$Data_Plot.newSample$CE.time.2)[1], 
                                   range(rVars$Data_Plot.newSample$CE.time.1)[1]),
                               max(range(rVars$Data_Plot.newSample$CE.time.2)[2], 
                                   range(rVars$Data_Plot.newSample$CE.time.1)[2]), by = 50)
              
              median_line_data<-data.frame(x =  median_line, y  =  median_line)
              
              ggplot(rVars$Data_Plot.newSample, aes(x=CE.time.2, y=CE.time.1)) +
                geom_point(size = 0.5) +
                
                coord_cartesian(xlim =c(min(range(rVars$Data_Plot.newSample$CE.time.2)[1], 
                                            range(rVars$Data_Plot.newSample$CE.time.1)[1]),
                                        max(range(rVars$Data_Plot.newSample$CE.time.2)[2], 
                                            range(rVars$Data_Plot.newSample$CE.time.1)[2])),
                                ylim = c(min(range(rVars$Data_Plot.newSample$CE.time.2)[1],
                                             range(rVars$Data_Plot.newSample$CE.time.1)[1]),
                                         max(range(rVars$Data_Plot.newSample$CE.time.2)[2], 
                                             range(rVars$Data_Plot.newSample$CE.time.1)[2])))+
                
                scale_x_continuous(n.breaks = 14)+
                scale_y_continuous(n.breaks = 14)+
                
                # xlab("CE-time (sample to align)")+
                # ylab("CE-time (reference sample)")+
                
                geom_line(data = predict_data_model.np, aes(x = CE.time.2, y = CE.time.1, color = "Model"), lwd = 1, size = 1.5) +
                #geom_smooth(method ="loess", color="blue", fill="#69b3a2", se=TRUE, span = input$span) +
                # geom_abline(yintercept = 0, lwd = 1, color = "green")+
                geom_line(data = median_line_data, aes(x=x,y=x, color = "Median"), lwd = 1, size = 1.5)+
                ggtitle("Before CE-time correction")+
                
                labs(x = paste("CE-time (", rVars$Data_Plot.newSample$sample.2,")"), 
                     y = "CE-time (Reference map)",
                     color = "Legend") +
                
                scale_color_manual(values = c("Model" = "red",
                                              "Median" = "green"))+
                
                
                theme_ben()+
                theme(plot.title = element_text(size = rel(1),
                                                face = "bold",
                                                color = "#760001",
                                                margin = margin(0,0,5,0), hjust = 0.5),
                      plot.subtitle = element_text(hjust = 0.5),
                      plot.background = element_rect(fill = "aliceblue"),
                      legend.title = element_text(size = rel(0.95), face = "bold.italic", hjust = 0.5),
                      legend.text = element_text(size = rel(0.85), face = "bold.italic"))
              
            })
            
            #### info-bulle
            output$PlotCorrectionKernelDensity_Before_hover_info_newSample<- renderUI({
              hover <- req(input$PlotCorrectionKernelDensity_Before_hover_newSample)
              
              if(!is.null(rVars$Data_Plot.newSample) &
                 !is.null(rVars$features_samples_list) & 
                 !is.null(rVars$ref_data_valid)){
                point <- nearPoints(req(rVars$Data_Plot.newSample), 
                                    hover, threshold = 5, maxpoints = 1, addDist = TRUE)
                if (nrow(point) == 0) return(NULL)
                
                # calculate point position INSIDE the image as percent of total dimensions
                # from left (horizontal) and from top (vertical)
                left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
                top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
                
                # calculate distance from left and bottom side of the picture in pixels
                left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
                top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top) 
                
                # create style property fot tooltip
                # background color is set so tooltip is a bit transparent
                # z-index is set so we are sure are tooltip will be on top
                style <- paste0("position:absolute; z-index:100; background-color: #760001; color: white;",
                                "left:", left_px + 2, "px; top:", top_px + 2, "px;")
                
                # actual tooltip created as wellPanel 
                div(
                  class = "well well-sm",
                  style = style,
                  p(HTML(paste0(
                    "<span class='bullText'> M+H (ref): </span>", round(point$M.H.1,4), "<br/>",
                    "<span class='bullText'> CE-time (ref): </span>", round(point$CE.time.1,2), "<br/>",
                    "<br>",
                    "<span class='bullText'> M+H (sample): </span>", round(point$M.H.2,4), "<br/>",
                    "<span class='bullText'> CE-time (sample): </span>", round(point$CE.time.2,2), "<br/>"
                  )))
                )
              }
              
            })
            
            incProgress(1/3, detail ="")
            ####~~~~~~~~~~~~~~~~~~~~ Plot After ~~~~~~~~~~~~~~~~~~~~~~~~#
            ####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            output$PlotCorrectionKernelDensity_After_newSample<-renderPlot({
              if(!is.null(input$SelectSample_KernelDensity_newSample) &
                 !is.null(rVars$features_samples_list) & 
                 !is.null(rVars$ref_data_valid) & 
                 !is.null(rVars$features_samples_KernelDensityCorrection)){
                
                source("lib/lib.R", local=TRUE)
                
                ref<-rVars$ref_data_valid[, c("M.H","CE.time")]
                
                table.after<-rVars$features_samples_KernelDensityCorrection[[input$SelectSample_KernelDensity_newSample]]
                
                table.after<-table.after[,c("M.H","CE.time","sample")]
                
                
                resMatch.after<-matchMz(x = ref,
                                        table = table.after,
                                        ppm_tolereance = 1000,
                                        mzcol = "M.H",
                                        rtcol = "CE.time" ,
                                        session = session)
                
                Data_Plot.after<-resMatch.after$MatchTable[!is.na(resMatch.after$MatchTable$CE.time.2),
                                                           c("M.H.1","CE.time.1","M.H.2","CE.time.2","sample.2")]
                
                rVars$Data_Plot.after_KernelDensity<-Data_Plot.after
                
                median_line.after<-seq(min(range(Data_Plot.after$CE.time.2)[1], 
                                           range(Data_Plot.after$CE.time.1)[1]),
                                       max(range(Data_Plot.after$CE.time.2)[2], 
                                           range(Data_Plot.after$CE.time.1)[2]), by = 50)
                
                median_line_data.after<-data.frame(x =  median_line.after, 
                                                   y  =  median_line.after)
                
                
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~ Plot after Correction ~~~~~~~~~~~~~~~~~~~~~~~~~~#
                Plot.after<-ggplot(Data_Plot.after, aes(x=CE.time.2, y=CE.time.1)) +
                  geom_point(size = 0.5) +
                  
                  coord_cartesian(xlim =c(min(range(Data_Plot.after$CE.time.2)[1], range(Data_Plot.after$CE.time.1)[1]),
                                          max(range(Data_Plot.after$CE.time.2)[2], range(Data_Plot.after$CE.time.1)[2])),
                                  ylim = c(min(range(Data_Plot.after$CE.time.2)[1], range(Data_Plot.after$CE.time.1)[1]),
                                           max(range(Data_Plot.after$CE.time.2)[2], range(Data_Plot.after$CE.time.1)[2])))+
                  
                  
                  
                  scale_x_continuous(n.breaks = 14)+
                  scale_y_continuous(n.breaks = 14)+
                  
                  # xlab("CE-time (sample to align)")+
                  # ylab("CE-time (reference sample)")+
                  
                  # geom_line(data = predict_data_model.np, aes(x = rt2, y = rt1, color = "Model"), lwd = 1, size = 1.5) +
                  # #geom_smooth(method ="loess", color="blue", fill="#69b3a2", se=TRUE, span = input$span) +
                  # # geom_abline(yintercept = 0, lwd = 1, color = "green")+
                  
                  geom_line(data = median_line_data.after, aes(x=x,y=x, color = "Median"), lwd = 1, size = 1.5)+
                  
                  labs(x = paste("CE-time (", Data_Plot.after$sample.2,")"),
                       y = "CE-time (Reference map)",
                       color = "Legend") +
                  ggtitle("Correction with Kernel Density")+
                  
                  scale_color_manual(values = c("Median" = "green"))+
                  
                  
                  theme_ben()+
                  theme(plot.title = element_text(size = rel(1),
                                                  face = "bold",
                                                  color = "#760001",
                                                  margin = margin(0,0,5,0), hjust = 0.5),
                        plot.subtitle = element_text(hjust = 0.5),
                        plot.background = element_rect(fill = "aliceblue"),
                        legend.title = element_text(size = rel(0.95), face = "bold.italic", hjust = 0.5),
                        legend.text = element_text(size = rel(0.85), face = "bold.italic"))
                
                Plot.after
              }
              
            })
            
            #### info-bulle
            output$PlotCorrectionKernelDensity_After_hover_info_newSample <- renderUI({
              hover <- req(input$PlotCorrectionKernelDensity_After_hover_newSample)
              
              if(!is.null(rVars$Data_Plot.after_KernelDensity) &
                 !is.null(rVars$features_samples_list) & 
                 !is.null(rVars$ref_data_valid)){
                point <- nearPoints(req(rVars$Data_Plot.after_KernelDensity), 
                                    hover, threshold = 5, maxpoints = 1, addDist = TRUE)
                if (nrow(point) == 0) return(NULL)
                
                # calculate point position INSIDE the image as percent of total dimensions
                # from left (horizontal) and from top (vertical)
                left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
                top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
                
                # calculate distance from left and bottom side of the picture in pixels
                left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
                top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top) 
                
                # create style property fot tooltip
                # background color is set so tooltip is a bit transparent
                # z-index is set so we are sure are tooltip will be on top
                style <- paste0("position:absolute; z-index:100; background-color: #760001; color: white;",
                                "left:", left_px + 2, "px; top:", top_px + 2, "px;")
                
                # actual tooltip created as wellPanel 
                div(
                  class = "well well-sm",
                  style = style,
                  p(HTML(paste0(
                    "<span class='bullText'> M+H (ref): </span>", round(point$M.H.1,4), "<br/>",
                    "<span class='bullText'> CE-time (ref): </span>", round(point$CE.time.1,2), "<br/>",
                    "<br>",
                    "<span class='bullText'> M+H (sample): </span>", round(point$M.H.2,4), "<br/>",
                    "<span class='bullText'> CE-time (sample): </span>", round(point$CE.time.2,2), "<br/>"
                  )))
                )
              }
              
            }) 
            
            #~~~~~~~~~ Plot mz (or M+H)~rt reference and mz(M+H)~rt sample (CE-time correction with xcms)~~~~~~~~~~~~~#
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            
            output$CorrectimeKernelDensityViewer_2_newSample<-renderPlot({
              
              if(!is.null(input$SelectSample_KernelDensity_newSample) &
                 !is.null(rVars$features_samples_list) & 
                 !is.null(rVars$ref_data_valid) & 
                 !is.null(rVars$features_samples_KernelDensityCorrection)){
               
                
                ref<-rVars$ref_data_valid[, c("M.H","CE.time")]
                
                ref$sample<-"Reference map"
                
                ##~~~~~~~~~~~~~~~~~~~~ After correction xcms ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                table.after<-rVars$features_samples_list[[input$SelectSample_KernelDensity_newSample]]
                table.after<-table.after[,c("M.H","CE.time","sample")]
                
                Data_after<-rbind(table.after,
                                  ref)
                rownames(Data_after)<-1:nrow(Data_after)
                
                
                Data_after$sample<-factor(Data_after$sample,
                                          levels = c(unique(Data_after$sample)[unique(Data_after$sample)!="Reference map"],
                                                     "Reference map"
                                          ))
                
                Plot.After<-Data_after %>%
                  ggplot() +
                  aes(x = CE.time, y = M.H, colour = sample) +
                  #geom_point(shape = "circle", size = input$sizePoints) +
                  geom_point(size = 0.5) +
                  theme_gray() +
                  #facet_grid(vars(sample), vars())  +
                  facet_wrap(~sample, dir = "v")+
                  ylab("Mass (M+H) (Da)") +
                  #xlab(paste("CE-time (",input$UnitTime_NewRefMap,")")) +
                  xlab(paste("CE-time")) +
                  
                  ggtitle("Before CE-time correction")+
                  
                  scale_x_continuous(n.breaks = 14)+
                  scale_y_continuous(n.breaks = 5)+
                  
                  theme_ben() +
                  theme(plot.title = element_text(size = rel(1),
                                                  face = "bold",
                                                  color = "#760001",
                                                  margin = margin(0,0,5,0), hjust = 0.5),
                        plot.subtitle = element_text(hjust = 0.5),
                        panel.border = element_rect(fill = "transparent", # Needed to add the border
                                                    color = "blue",
                                                    linewidth = 0.5,
                                                    linetype = "dashed"),
                        legend.position = "none",
                        
                        # Les étiquettes dans le cas d'un facetting
                        strip.background = element_rect(fill = "grey", color = "grey"),
                        strip.text = element_text(size = rel(1), face = "bold.italic", color = "black", margin = margin(5,0,5,0)),
                        plot.background = element_rect(fill = "aliceblue"))
                
                ##~~~~~~~~~~~~~~~~~~~~ After correction With Kernel Density ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                table.After_KernelDensity<-rVars$features_samples_KernelDensityCorrection[[input$SelectSample_KernelDensity_newSample]]
                table.After_KernelDensity<-table.After_KernelDensity[,c("M.H","CE.time","sample")]
                
                Data_After_KernelDensity<-rbind(table.After_KernelDensity,
                                                ref)
                
                rownames(Data_After_KernelDensity)<-1:nrow(Data_After_KernelDensity)
                
                
                Data_After_KernelDensity$sample<-factor(Data_After_KernelDensity$sample, 
                                                        levels = c(unique(Data_After_KernelDensity$sample)[unique(Data_After_KernelDensity$sample)!="Reference map"],
                                                                   "Reference map"))
                
                
                
                Plot.After_KernelDensity<-Data_After_KernelDensity %>%
                  ggplot() +
                  aes(x = CE.time, y = M.H, colour = sample) +
                  #geom_point(shape = "circle", size = input$sizePoints) +
                  geom_point(size = 0.5) +
                  theme_gray() +
                  #facet_grid(vars(sample), vars())  +
                  facet_wrap(~sample, dir = "v")+
                  #coord_cartesian(xlim = rangesZoomOffsetSamplePlot$x, ylim = rangesZoomOffsetSamplePlot$y, expand = TRUE)+
                  ylab("Mass (M+H) (Da)") +
                  #xlab(paste("CE-time (",input$UnitTime_NewRefMap,")")) +
                  xlab(paste("CE-time")) +
                  
                  ggtitle("Correction with Kernel density")+
                  
                  scale_x_continuous(n.breaks = 14)+
                  scale_y_continuous(n.breaks = 5)+
                  
                  theme_ben() +
                  theme(plot.title = element_text(size = rel(1),
                                                  face = "bold",
                                                  color = "#760001",
                                                  margin = margin(0,0,5,0), hjust = 0.5),
                        plot.subtitle = element_text(hjust = 0.5),
                        panel.border = element_rect(fill = "transparent", # Needed to add the border
                                                    color = "blue",
                                                    linewidth = 0.5,
                                                    linetype = "dashed"),
                        legend.position = "none",
                        
                        # Les étiquettes dans le cas d'un facetting
                        strip.background = element_rect(fill = "grey", color = "grey"),
                        strip.text = element_text(size = rel(1), face = "bold.italic", color = "black", margin = margin(5,0,5,0)),
                        plot.background = element_rect(fill = "aliceblue"))
                
                
                
                
                
                grid.arrange(Plot.After, Plot.After_KernelDensity, nrow = 2)
                
              }
              
            })
            
            
            
          }
          
          
        })
        
      }
    } 
    
  })


observe({
  if(is.null(rVars$modelKernelDensity) ||
     is.null(rVars$features_samples_KernelDensityCorrection)){
    output$PlotCorrectionKernelDensity_Before_newSample <- renderPlot({})
    output$PlotCorrectionKernelDensity_After_newSample<-renderPlot({})
    output$CorrectimeKernelDensityViewer_2_newSample<-renderPlot({})
  }
  
  if(is.null(input$SelectSample_KernelDensity_newSample) ||
     is.null(rVars$features_samples_list) ||
     is.null(data_list_selectedCutting()) ||
     is.null(rVars$ref_data_valid)){
    output$DensityFilterPlot_newSample <- renderPlot({})
  }
})




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Saving adjusted files ~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$IDSaveAdjustSamples
             }
             ,
             handlerExpr = {
               shinyFileSave(input,
                             id = "IDSaveAdjustSamples",
                             roots = volumes,
                             session = session)
               
               
               path_Save_Files_origine <-
                 parseSavePath(volumes, input$IDSaveAdjustSamples)$datapath
               
               if (length(path_Save_Files_origine) > 0) {
                 path_Save_Files <- strsplit(path_Save_Files_origine, split = "/")[[1]]
                 directory_Save_Files <-
                   paste0(path_Save_Files[-length(path_Save_Files)], collapse = "/")
                 
                 if (!is.null(rVars$features_samples_KernelDensityCorrection)) {
                   Features_list <-
                     rVars$features_samples_KernelDensityCorrection
                   

                   
                   if (length(Features_list) == 1) {
                     write.table(
                       Features_list[[1]],
                       #file = path_Save_Files_origine,
                       file = paste0(c(
                         directory_Save_Files,
                         paste(
                           names(Features_list)[1],
                           path_Save_Files[length(path_Save_Files)],
                           collapse = "",
                           sep = "-"
                         )
                       ), collapse = "/"),
                       sep = ",",
                       row.names = FALSE
                     )
                     print("Save ok")
                   } else{
                     withProgress(message = 'Saving files...', value = 0, {
                       for (i in 1:length(Features_list)) {
                         incProgress(1 / length(Features_list),
                                     detail = paste(names(Features_list)[i]))
                         print(basename(paste0(
                           c(
                             directory_Save_Files,
                             paste(
                               #as.character(i),
                               names(Features_list)[i],
                               path_Save_Files[length(path_Save_Files)],
                               collapse = "",
                               sep = "-"
                             )
                           ), collapse = "/"
                         )))
                         write.table(
                           Features_list[[i]],
                           file = paste0(c(
                             directory_Save_Files,
                             paste(
                               #as.character(i),
                               names(Features_list)[i],
                               path_Save_Files[length(path_Save_Files)],
                               collapse = "",
                               sep = "-"
                             )
                           ), collapse = "/"),
                           sep = ",",
                           row.names = FALSE
                         )
                         print("Save ok")
                       }
                       
                     })
                     
                   }
                   
                   
                 }
               }
               
               
             })