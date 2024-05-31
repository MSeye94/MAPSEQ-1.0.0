#---------------------------------------------------------------#
#------------------- CE-time filter --------------------#
#---------------------------------------------------------------#

#----------------------- Update input --------------------------#
observe({
  if (!is.null(names(rVars$samples_data_list))) {
    rVars$samplesCuttingTable_newSample <-
      data.frame(
        row.names = names(rVars$samples_data_list),
        rt_min_cut_newSample = rep(NA,
                                   length(rVars$samples_data_list)),
        rt_max_cut_newSample = rep(NA,
                                   length(rVars$samples_data_list)),
        Unit = rep("Second",
                   length(rVars$samples_data_list))
      )
    
    
    output$SelectSampleCut <- renderUI({
      pickerInput(
        inputId = "sample_selectedCutting_newSample",
        label = "Select a sample:",
        choices = names(rVars$samples_data_list),
        options = pickerOptions(
          size = 10,
          liveSearch = TRUE,
          showTick = TRUE,
          style = "btn-primary"
        ),
        width = "fit"
      )
    })
    
    
  } else{
    output$SelectSampleCut <- renderUI({
      pickerInput(
        inputId = "sample_selectedCutting_newSample",
        label = "Select a sample:",
        choices = NULL,
        options = pickerOptions(
          size = 10,
          liveSearch = TRUE,
          showTick = TRUE,
          style = "btn-primary"
        ),
        width = "fit"
      )
    })
    
    rVars$samplesCuttingTable_newSample <-
      NULL
  }
  
  
})



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~ sample cutting viewer ~~~~~~~~~~~~~~~~~~~~~~~~#
observe({
  if (length(input$sample_selectedCutting_newSample) != 0) {
    sample_cutting_rv <-
      req(rVars$samples_data_list[req(input$sample_selectedCutting_newSample)][[1]])
    
    rVars$sample_cutting_newSample <-
      sample_cutting_rv
  }
  
})



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Sample cutting to used ~~~~~~~~~~~~~~~~~~~~~~~~#
## Convert time sample select
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$sample_selectedCutting_newSample
             },
             handlerExpr = {
               rVars$sample_cutting_viewer_ConvertTime_newSample <-
                 req(rVars$samples_data_list[req(input$sample_selectedCutting_newSample)][[1]])
               
               #Because les data viennent initialement en second
               rVars$Second_Cutting_newSample <-
                 TRUE
               if (input$UnitTime_sampleCutting_newSample == "Minute") {
                 print(
                   paste(
                     "Convert to min",
                     input$UnitTime_sampleCutting_newSample == "Minute" &
                       rVars$Second_Cutting_newSample == TRUE
                   )
                 )
                 if (input$UnitTime_sampleCutting_newSample == "Minute" &
                     rVars$Second_Cutting_newSample == TRUE) {
                   if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                     rVars$sample_cutting_viewer_ConvertTime_newSample$CE.time <-
                       req(rVars$sample_cutting_viewer_ConvertTime_newSample$CE.time) / 60
                     rVars$Second_Cutting_newSample <-
                       FALSE
                   }
                 }
               }
               
               
               if (!is.null(rVars$samplesCuttingTable_newSample)) {
                 rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), 3] <-
                   input$UnitTime_sampleCutting_newSample
                 
               }
             })


observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$UnitTime_sampleCutting_newSample
             },
             handlerExpr = {
               print(
                 paste(
                   "Convert to min",
                   input$UnitTime_sampleCutting_newSample == "Minute" &
                     rVars$Second_Cutting_newSample == TRUE
                 )
               )
               if (isolate(input$UnitTime_sampleCutting_newSample) == "Minute" &
                   rVars$Second_Cutting_newSample == TRUE) {
                 if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                   rVars$sample_cutting_viewer_ConvertTime_newSample$CE.time <-
                     req(rVars$sample_cutting_viewer_ConvertTime_newSample$CE.time) / 60
                   rVars$Second_Cutting_newSample <-
                     FALSE
                 }
               }
               
               print(
                 paste(
                   "Covert to Second",
                   input$UnitTime_sampleCutting_newSample == "Second" &
                     rVars$Second_Cutting_newSample == FALSE
                 )
               )
               if (isolate(input$UnitTime_sampleCutting_newSample) == "Second" &
                   rVars$Second_Cutting_newSample == FALSE) {
                 if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                   rVars$sample_cutting_viewer_ConvertTime_newSample$CE.time <-
                     req(rVars$sample_cutting_viewer_ConvertTime_newSample$CE.time) * 60
                   rVars$Second_Cutting_newSample <-
                     TRUE
                 }
               }
               
               if (input$UnitTime_sampleCutting_newSample == "Second" &
                   rVars$Second_Cutting_newSample == TRUE) {
                 rVars$sample_cutting_viewer_ConvertTime_newSample <-
                   req(rVars$sample_cutting_newSample)
               }
               
               if (!is.null(rVars$samplesCuttingTable_newSample)) {
                 rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), 3] <-
                   input$UnitTime_sampleCutting_newSample
                 
               }
               
               
             })


rangesZoomSample_selectedCutting_newSample <-
  reactiveValues(x = NULL, y = NULL)

# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
observeEvent({
  input$sample_selectedCutting_dblclick_newSample
}, {
  brush <- input$sample_selectedCutting_brush_newSample
  if (!is.null(brush)) {
    rangesZoomSample_selectedCutting_newSample$x <-
      c(brush$xmin, brush$xmax)
    rangesZoomSample_selectedCutting_newSample$y <-
      c(brush$ymin, brush$ymax)
    
  } else {
    ## reset coord_cartesian
    rangesZoomSample_selectedCutting_newSample$x <- NULL
    rangesZoomSample_selectedCutting_newSample$y <- NULL
  }
})

observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$sample_selectedCutting_newSample
             },
             handlerExpr = {
               updateNumericInput(
                 session = session,
                 inputId = "rt_min_cut_newSample",
                 value = rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_min_cut_newSample
               )
               
               updateNumericInput(
                 session = session,
                 inputId = "rt_max_cut_newSample",
                 value = rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_max_cut_newSample
               )
               
               
             })

observe({
  if (!is.null(input$sample_selectedCutting_newSample)) {
    if (!is.null(rVars$samples_data_list[req(input$sample_selectedCutting_newSample)][[1]])) {
      if (is.na(rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_min_cut_newSample) &
          is.na(rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_max_cut_newSample)) {
        print("Sample selected not cutted")
        if (!is.null(rVars$samplesCuttingTable_newSample)) {
          rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), 1:2] <-
            c(NA, NA)
          
        }
        
        
        
        output$sample_selectedCutting_Plot_newSample <- renderPlot({
          if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
            shinyjs::show("UnitTime_sampleCutting_id_newSample")
            shinyjs::show("mousepositionSampleCutting_newSample")
            
            rVars$sample_cutting_viewer_ConvertTime_newSample %>%
              ggplot() +
              aes(x = CE.time,
                  y = M.H) +
              geom_point(size = 1) +
              ylab("Mass (M+H) (Da)") +
              xlab(paste(
                "CE-time (",
                req(input$UnitTime_sampleCutting_newSample),
                ")"
              )) +
              coord_cartesian(xlim = rangesZoomSample_selectedCutting_newSample$x,
                              ylim = rangesZoomSample_selectedCutting_newSample$y,
                              expand = TRUE) +
              ggtitle(paste(
                "Sample selected :",
                req(input$sample_selectedCutting_newSample),
                "\n"
              )) +
              
              scale_x_continuous(n.breaks = 14) +
              scale_y_continuous(n.breaks = 14) +
              
              
              #facet_grid(vars(sample), vars())+
              #option_graphe.3
              theme_ben() +
              theme(
                plot.title = element_text(
                  size = rel(0.9),
                  face = "bold",
                  color = "#760001",
                  margin = margin(0, 0, 5, 0),
                  hjust = 0.5
                ),
                plot.subtitle = element_text(hjust = 0.5)
              )
            
          }
          
          
        })
        
        output$sample_selectedCutting_Plot_info_newSample <-
          renderText({
            paste0(
              "Mouse position : ",
              xy_str(
                input$sample_selectedCutting_hover_newSample,
                "CE-time",
                "M+H"
              ),
              "Click: ",
              xy_str(
                input$sample_selectedCutting_click_newSample,
                "CE-time",
                "M+H"
              )
            )
          })
        
        ## Bulle info
        
        #### info-bulle
        output$sample_selectedCutting_hover_info_newSample <-
          renderUI({
            hover <- req(input$sample_selectedCutting_hover_newSample)
            
            if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
              point <-
                nearPoints(
                  req(
                    rVars$sample_cutting_viewer_ConvertTime_newSample
                  ),
                  hover,
                  threshold = 5,
                  maxpoints = 1,
                  addDist = TRUE
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
                      "<span class='bullText'> CE-time: </span>",
                      round(point$CE.time, 2),
                      "<br/>",
                      "<span class='bullText'> M+H: </span>",
                      round(point$M.H, 4),
                      "<br/>"
                    )
                  )))
            }
            
          })
        
        
        
      }
    }
    else {
      output$sample_selectedCutting_Plot_newSample <- renderPlot({
        
      })
      output$sample_selectedCutting_Plot_info_newSample <-
        renderText({
          
        })
      output$sample_selectedCutting_hover_info_newSample <-
        renderUI({
          
        })
      hide("UnitTime_sampleCutting_id_newSample")
    }
  } else {
    output$sample_selectedCutting_Plot_newSample <- renderPlot({
      
    })
  }
  
  
})


observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$cutOff_newSample
             },
             handlerExpr = {
               if (!is.null(rVars$samples_data_list[req(input$sample_selectedCutting_newSample)][[1]])) {
                 if (!is.na(input$rt_min_cut_newSample) &
                     !is.na(input$rt_max_cut_newSample)) {
                   print("cutoff_newSample")
                   output$sample_selectedCutting_Plot_newSample <-
                     renderPlot({
                       if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                         shinyjs::show("UnitTime_sampleCutting_id_newSample")
                         shinyjs::show("mousepositionSampleCutting_newSample")
                         
                         rVars$sample_cutting_viewer_ConvertTime_newSample %>%
                           ggplot() +
                           aes(x = CE.time,
                               y = M.H) +
                           
                           geom_point(size = 1) +
                           geom_vline(
                             xintercept = isolate(input$rt_min_cut_newSample),
                             color = "gray",
                             linetype = "dashed"
                           ) +
                           geom_vline(
                             xintercept = isolate(input$rt_max_cut_newSample),
                             color = "gray",
                             linetype = "dashed"
                           ) +
                           ggplot2::annotate(
                             "rect",
                             xmin = c(0, isolate(input$rt_max_cut_newSample)),
                             xmax = c(isolate(input$rt_min_cut_newSample), Inf),
                             ymin = -Inf ,
                             ymax = Inf,
                             alpha = 0.5,
                             fill = "gray"
                           ) +
                           ylab("Mass (M+H) (Da)") +
                           xlab(paste(
                             "CE-time (",
                             req(input$UnitTime_sampleCutting_newSample),
                             ")"
                           )) +
                           coord_cartesian(xlim = rangesZoomSample_selectedCutting_newSample$x,
                                           ylim = rangesZoomSample_selectedCutting_newSample$y,
                                           expand = TRUE) +
                           ggtitle(paste(
                             "Sample selected :",
                             req(input$sample_selectedCutting_newSample),
                             "\n"
                           )) +
                           
                           scale_x_continuous(n.breaks = 14) +
                           scale_y_continuous(n.breaks = 14) +
                           
                           
                           #facet_grid(vars(sample), vars())+
                           #option_graphe.3
                           theme_ben() +
                           theme(
                             plot.title = element_text(
                               size = rel(0.9),
                               face = "bold",
                               color = "#760001",
                               margin = margin(0, 0, 5, 0),
                               hjust = 0.5
                             ),
                             plot.subtitle = element_text(hjust = 0.5)
                           )
                         
                       }
                       
                       
                       
                     })
                   
                   
                   output$sample_selectedCutting_Plot_info_newSample <-
                     renderText({
                       paste0(
                         "Mouse position: ",
                         xy_str(
                           input$sample_selectedCutting_hover_newSample,
                           "CE-time",
                           "M+H"
                         ),
                         "Click: ",
                         xy_str(
                           input$sample_selectedCutting_click_newSample,
                           "CE-time",
                           "M+H"
                         )
                       )
                     })
                   
                   ## Bulle info
                   
                   #### info-bulle
                   output$sample_selectedCutting_hover_info_newSample <-
                     renderUI({
                       hover <- req(input$sample_selectedCutting_hover_newSample)
                       
                       if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                         point <-
                           nearPoints(
                             req(
                               rVars$sample_cutting_viewer_ConvertTime_newSample
                             ),
                             hover,
                             threshold = 5,
                             maxpoints = 1,
                             addDist = TRUE
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
                                 "<span class='bullText'> CE-time: </span>",
                                 round(point$CE.time, 2),
                                 "<br/>",
                                 "<span class='bullText'> M+H: </span>",
                                 round(point$M.H, 4),
                                 "<br/>"
                               )
                             )))
                       }
                       
                       
                       
                       
                       
                     })
                   
                   if (!is.null(rVars$samplesCuttingTable_newSample)) {
                     rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), 1:2] <-
                       c(
                         isolate(input$rt_min_cut_newSample),
                         isolate(input$rt_max_cut_newSample)
                       )
                   }
                   
                   
                   
                 } else if (!is.na(input$rt_min_cut_newSample) &
                            is.na(input$rt_max_cut_newSample)) {
                   print("cutoff_newSample")
                   output$sample_selectedCutting_Plot_newSample <-
                     renderPlot({
                       if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                         shinyjs::show("UnitTime_sampleCutting_id_newSample")
                         shinyjs::show("mousepositionSampleCutting_newSample")
                         
                         rVars$sample_cutting_viewer_ConvertTime_newSample %>%
                           ggplot() +
                           aes(x = CE.time,
                               y = M.H) +
                           geom_point(size = 1) +
                           geom_vline(
                             xintercept = isolate(input$rt_min_cut_newSample),
                             color = "gray",
                             linetype = "dashed"
                           ) +
                           ggplot2::annotate(
                             "rect",
                             xmin = 0,
                             xmax = isolate(input$rt_min_cut_newSample),
                             ymin = -Inf ,
                             ymax = Inf,
                             alpha = 0.5,
                             fill = "gray"
                           ) +
                           ylab("Mass (M+H) (Da)") +
                           xlab(paste(
                             "CE-time (",
                             req(input$UnitTime_sampleCutting_newSample),
                             ")"
                           )) +
                           coord_cartesian(xlim = rangesZoomSample_selectedCutting_newSample$x,
                                           ylim = rangesZoomSample_selectedCutting_newSample$y,
                                           expand = TRUE) +
                           ggtitle(paste(
                             "Sample selected :",
                             req(input$sample_selectedCutting_newSample),
                             "\n"
                           )) +
                           
                           scale_x_continuous(n.breaks = 14) +
                           scale_y_continuous(n.breaks = 14) +
                           
                           
                           #facet_grid(vars(sample), vars())+
                           #option_graphe.3
                           theme_ben() +
                           theme(
                             plot.title = element_text(
                               size = rel(0.9),
                               face = "bold",
                               color = "#760001",
                               margin = margin(0, 0, 5, 0),
                               hjust = 0.5
                             ),
                             plot.subtitle = element_text(hjust = 0.5)
                           )
                         
                       }
                       
                       
                       
                     })
                   
                   
                   output$sample_selectedCutting_Plot_info_newSample <-
                     renderText({
                       paste0(
                         "Mouse position: ",
                         xy_str(
                           input$sample_selectedCutting_hover_newSample,
                           "CE-time",
                           "M+H"
                         ),
                         "Click: ",
                         xy_str(
                           input$sample_selectedCutting_click_newSample,
                           "CE-time",
                           "M+H"
                         )
                       )
                     })
                   
                   ## Bulle info
                   
                   #### info-bulle
                   output$sample_selectedCutting_hover_info_newSample <-
                     renderUI({
                       hover <- req(input$sample_selectedCutting_hover_newSample)
                       
                       if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                         point <-
                           nearPoints(
                             req(
                               rVars$sample_cutting_viewer_ConvertTime_newSample
                             ),
                             hover,
                             threshold = 5,
                             maxpoints = 1,
                             addDist = TRUE
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
                                 "<span class='bullText'> CE-time: </span>",
                                 round(point$CE.time, 2),
                                 "<br/>",
                                 "<span class='bullText'> M+H: </span>",
                                 round(point$M.H, 4),
                                 "<br/>"
                               )
                             )))
                       }
                       
                       
                       
                       
                       
                     })
                   
                   if (!is.null(rVars$samplesCuttingTable_newSample)) {
                     rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), 1:2] <-
                       c(isolate(input$rt_min_cut_newSample), NA)
                   }
                   
                   
                 } else if (is.na(input$rt_min_cut_newSample) &
                            !is.na(input$rt_max_cut_newSample)) {
                   print("cutoff")
                   output$sample_selectedCutting_Plot_newSample <-
                     renderPlot({
                       if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                         shinyjs::show("UnitTime_sampleCutting_id_newSample")
                         shinyjs::show("mousepositionSampleCutting_newSample")
                         
                         rVars$sample_cutting_viewer_ConvertTime_newSample %>%
                           ggplot() +
                           aes(x = CE.time,
                               y = M.H) +
                           geom_point(size = 1) +
                           geom_vline(
                             xintercept = isolate(input$rt_max_cut_newSample),
                             color = "gray",
                             linetype = "dashed"
                           ) +
                           ggplot2::annotate(
                             "rect",
                             xmin = isolate(input$rt_max_cut_newSample),
                             xmax = Inf,
                             ymin = -Inf ,
                             ymax = Inf,
                             alpha = 0.5,
                             fill = "gray"
                           ) +
                           ylab("Mass (M+H) (Da)") +
                           xlab(paste(
                             "CE-time (",
                             req(input$UnitTime_sampleCutting_newSample),
                             ")"
                           )) +
                           coord_cartesian(xlim = rangesZoomSample_selectedCutting_newSample$x,
                                           ylim = rangesZoomSample_selectedCutting_newSample$y,
                                           expand = TRUE) +
                           ggtitle(paste(
                             "Sample selected :",
                             req(input$sample_selectedCutting_newSample),
                             "\n"
                           )) +
                           
                           scale_x_continuous(n.breaks = 14) +
                           scale_y_continuous(n.breaks = 14) +
                           
                           #facet_grid(vars(sample), vars())+
                           #option_graphe.3
                           theme_ben() +
                           labs(colour = "log2 Intensity") +
                           theme(
                             plot.title = element_text(
                               size = rel(0.9),
                               face = "bold",
                               color = "#760001",
                               margin = margin(0, 0, 5, 0),
                               hjust = 0.5
                             ),
                             plot.subtitle = element_text(hjust = 0.5)
                           )
                         
                       }
                       
                       
                       
                     })
                   
                   
                   output$sample_selectedCutting_Plot_info_newSample <-
                     renderText({
                       paste0(
                         "Mouse position: ",
                         xy_str(
                           input$sample_selectedCutting_hover_newSample,
                           "CE-time",
                           "M+H"
                         ),
                         "Click: ",
                         xy_str(
                           input$sample_selectedCutting_click_newSample,
                           "CE-time",
                           "M+H"
                         )
                       )
                     })
                   
                   ## Bulle info
                   
                   #### info-bulle
                   output$sample_selectedCutting_hover_info_newSample <-
                     renderUI({
                       hover <- req(input$sample_selectedCutting_hover_newSample)
                       
                       if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                         point <-
                           nearPoints(
                             req(
                               rVars$sample_cutting_viewer_ConvertTime_newSample
                             ),
                             hover,
                             threshold = 5,
                             maxpoints = 1,
                             addDist = TRUE
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
                                 "<span class='bullText'> CE-time: </span>",
                                 round(point$CE.time, 2),
                                 "<br/>",
                                 "<span class='bullText'> M+H: </span>",
                                 round(point$M.H, 4),
                                 "<br/>"
                               )
                             )))
                       }
                       
                       
                       
                       
                       
                     })
                   
                   if (!is.null(rVars$samplesCuttingTable_newSample)) {
                     rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), 1:2] <-
                       c(NA, isolate(input$rt_max_cut_newSample))
                   }
                   
                   
                 } else  if (is.na(input$rt_min_cut_newSample) &
                             is.na(input$rt_max_cut_newSample)) {
                   if (!is.null(rVars$samplesCuttingTable_newSample)) {
                     rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), 1:2] <-
                       c(NA, NA)
                     
                   }
                 }
                 
               } else {
                 output$sample_selectedCutting_Plot_newSample <- renderPlot({
                   
                 })
                 output$sample_selectedCutting_Plot_info_newSample <-
                   renderText({
                     
                   })
                 output$sample_selectedCutting_hover_info_newSample <-
                   renderUI({
                     
                   })
                 hide("UnitTime_sampleCutting_id_newSample")
               }
               
               
               
               
             })



observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$sample_selectedCutting_newSample
             },
             handlerExpr = {
               updateNumericInput(
                 session = session,
                 inputId = "rt_min_cut_newSample",
                 value = as.numeric(
                   rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_min_cut_newSample
                 )
               )
               
               updateNumericInput(
                 session = session,
                 inputId = "rt_max_cut_newSample",
                 value = as.numeric(
                   rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_max_cut_newSample
                 )
               )
               
               
             })


observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$sample_selectedCutting_newSample
             },
             handlerExpr = {
               if (!is.null(rVars$samples_data_list[req(input$sample_selectedCutting_newSample)][[1]])) {
                 if (!is.null(rVars$samplesCuttingTable_newSample)) {
                   if (!is.na(rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_min_cut_newSample) &
                       !is.na(rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_max_cut_newSample)) {
                     print("Selectsample")
                     output$sample_selectedCutting_Plot_newSample <-
                       renderPlot({
                         if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                           shinyjs::show("UnitTime_sampleCutting_id_newSample")
                           shinyjs::show("mousepositionSampleCutting_newSample")
                           
                           rVars$sample_cutting_viewer_ConvertTime_newSample %>%
                             ggplot() +
                             aes(x = CE.time,
                                 y = M.H) +
                             geom_point(size = 1) +
                             geom_vline(
                               xintercept = as.numeric(
                                 rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_min_cut_newSample
                               ),
                               color = "gray",
                               linetype = "dashed"
                             ) +
                             geom_vline(
                               xintercept = as.numeric(
                                 rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_max_cut_newSample
                               ),
                               color = "gray",
                               linetype = "dashed"
                             ) +
                             ggplot2::annotate(
                               "rect",
                               xmin = c(
                                 0,
                                 as.numeric(
                                   rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_max_cut_newSample
                                 )
                               ),
                               xmax = c(
                                 as.numeric(
                                   rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_min_cut_newSample
                                 ),
                                 Inf
                               ),
                               ymin = -Inf ,
                               ymax = Inf,
                               alpha = 0.5,
                               fill = "gray"
                             ) +
                             ylab("Mass (M+H) (Da)") +
                             xlab(paste(
                               "CE-time (",
                               req(input$UnitTime_sampleCutting_newSample),
                               ")"
                             )) +
                             coord_cartesian(xlim = rangesZoomSample_selectedCutting_newSample$x,
                                             ylim = rangesZoomSample_selectedCutting_newSample$y,
                                             expand = TRUE) +
                             ggtitle(paste(
                               "Sample selected :",
                               req(input$sample_selectedCutting_newSample),
                               "\n"
                             )) +
                             
                             scale_x_continuous(n.breaks = 14) +
                             scale_y_continuous(n.breaks = 14) +
                             
                             
                             #facet_grid(vars(sample), vars())+
                             #option_graphe.3
                             theme_ben() +
                             theme(
                               plot.title = element_text(
                                 size = rel(0.9),
                                 face = "bold",
                                 color = "#760001",
                                 margin = margin(0, 0, 5, 0),
                                 hjust = 0.5
                               ),
                               plot.subtitle = element_text(hjust = 0.5)
                             )
                           
                         }
                         
                         
                         
                       })
                     
                     
                     output$sample_selectedCutting_Plot_info_newSample <-
                       renderText({
                         paste0(
                           "Mouse position: ",
                           xy_str(
                             input$sample_selectedCutting_hover_newSample,
                             "CE-time",
                             "M+H"
                           ),
                           "Click: ",
                           xy_str(
                             input$sample_selectedCutting_click_newSample,
                             "CE-time",
                             "M+H"
                           )
                         )
                       })
                     ## Bulle info
                     
                     #### info-bulle
                     output$sample_selectedCutting_hover_info_newSample <-
                       renderUI({
                         hover <- req(input$sample_selectedCutting_hover_newSample)
                         
                         if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                           point <-
                             nearPoints(
                               req(
                                 rVars$sample_cutting_viewer_ConvertTime_newSample
                               ),
                               hover,
                               threshold = 5,
                               maxpoints = 1,
                               addDist = TRUE
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
                                   "<span class='bullText'> CE-time: </span>",
                                   round(point$CE.time, 2),
                                   "<br/>",
                                   "<span class='bullText'> M+H: </span>",
                                   round(point$M.H, 4),
                                   "<br/>"
                                 )
                               )))
                         }
                         
                         
                         
                         
                         
                       })
                     
                     
                     
                     
                   } else if (!is.na(rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_min_cut_newSample) &
                              is.na(rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_max_cut_newSample)) {
                     print("Selectsample")
                     output$sample_selectedCutting_Plot_newSample <-
                       renderPlot({
                         if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                           shinyjs::show("UnitTime_sampleCutting_id_newSample")
                           shinyjs::show("mousepositionSampleCutting_newSample")
                           
                           rVars$sample_cutting_viewer_ConvertTime_newSample %>%
                             ggplot() +
                             aes(x = CE.time,
                                 y = M.H) +
                             geom_point(size = 1) +
                             geom_vline(
                               xintercept = as.numeric(
                                 rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_min_cut_newSample
                               ),
                               color = "gray",
                               linetype = "dashed"
                             ) +
                             ggplot2::annotate(
                               "rect",
                               xmin = 0,
                               xmax = as.numeric(
                                 rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_min_cut_newSample
                               ),
                               ymin = -Inf ,
                               ymax = Inf,
                               alpha = 0.5,
                               fill = "gray"
                             ) +
                             ylab("Mass (M+H) (Da)") +
                             xlab(paste(
                               "CE-time (",
                               req(input$UnitTime_sampleCutting_newSample),
                               ")"
                             )) +
                             coord_cartesian(xlim = rangesZoomSample_selectedCutting_newSample$x,
                                             ylim = rangesZoomSample_selectedCutting_newSample$y,
                                             expand = TRUE) +
                             ggtitle(paste(
                               "Sample selected :",
                               req(input$sample_selectedCutting_newSample),
                               "\n"
                             )) +
                             
                             scale_x_continuous(n.breaks = 14) +
                             scale_y_continuous(n.breaks = 14) +
                             
                             
                             #facet_grid(vars(sample), vars())+
                             #option_graphe.3
                             theme_ben() +
                             labs(colour = "log2 Intensity") +
                             theme(
                               plot.title = element_text(
                                 size = rel(0.9),
                                 face = "bold",
                                 color = "#760001",
                                 margin = margin(0, 0, 5, 0),
                                 hjust = 0.5
                               ),
                               plot.subtitle = element_text(hjust = 0.5)
                             )
                           
                         }
                         
                         
                         
                       })
                     
                     output$sample_selectedCutting_Plot_info_newSample <-
                       renderText({
                         paste0(
                           "Mouse position: ",
                           xy_str(
                             input$sample_selectedCutting_hover_newSample,
                             "CE-time",
                             "M+H"
                           ),
                           "Click: ",
                           xy_str(
                             input$sample_selectedCutting_click_newSample,
                             "CE-time",
                             "M+H"
                           )
                         )
                       })
                     ## Bulle info
                     
                     #### info-bulle
                     output$sample_selectedCutting_hover_info_newSample <-
                       renderUI({
                         hover <- req(input$sample_selectedCutting_hover_newSample)
                         
                         if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                           point <-
                             nearPoints(
                               req(
                                 rVars$sample_cutting_viewer_ConvertTime_newSample
                               ),
                               hover,
                               threshold = 5,
                               maxpoints = 1,
                               addDist = TRUE
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
                                   "<span class='bullText'> CE-time: </span>",
                                   round(point$CE.time, 2),
                                   "<br/>",
                                   "<span class='bullText'> M+H: </span>",
                                   round(point$M.H, 4),
                                   "<br/>"
                                 )
                               )))
                         }
                         
                         
                         
                         
                         
                       })
                     
                     
                     
                   } else if (is.na(rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_min_cut_newSample) &
                              !is.na(rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_max_cut_newSample)) {
                     print("Selectsample")
                     output$sample_selectedCutting_Plot_newSample <-
                       renderPlot({
                         if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                           shinyjs::show("UnitTime_sampleCutting_id_newSample")
                           shinyjs::show("mousepositionSampleCutting_newSample")
                           
                           rVars$sample_cutting_viewer_ConvertTime_newSample %>%
                             ggplot() +
                             aes(x = CE.time,
                                 y = M.H) +
                             geom_point(size = 1) +
                             geom_vline(
                               xintercept = as.numeric(
                                 rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_max_cut_newSample
                               ),
                               color = "gray",
                               linetype = "dashed"
                             ) +
                             ggplot2::annotate(
                               "rect",
                               xmin = as.numeric(
                                 rVars$samplesCuttingTable_newSample[req(input$sample_selectedCutting_newSample), ]$rt_max_cut_newSample
                               ),
                               xmax = Inf,
                               ymin = -Inf ,
                               ymax = Inf,
                               alpha = 0.5,
                               fill = "gray"
                             ) +
                             ylab("Mass (M+H) (Da)") +
                             xlab(paste(
                               "CE-time (",
                               req(input$UnitTime_sampleCutting_newSample),
                               ")"
                             )) +
                             coord_cartesian(xlim = rangesZoomSample_selectedCutting_newSample$x,
                                             ylim = rangesZoomSample_selectedCutting_newSample$y,
                                             expand = TRUE) +
                             ggtitle(paste(
                               "Sample selected :",
                               req(input$sample_selectedCutting_newSample),
                               "\n"
                             )) +
                             
                             scale_x_continuous(n.breaks = 14) +
                             scale_y_continuous(n.breaks = 14) +
                             
                             
                             #facet_grid(vars(sample), vars())+
                             #option_graphe.3
                             theme_ben() +
                             labs(colour = "log2 Intensity") +
                             theme(
                               plot.title = element_text(
                                 size = rel(0.9),
                                 face = "bold",
                                 color = "#760001",
                                 margin = margin(0, 0, 5, 0),
                                 hjust = 0.5
                               ),
                               plot.subtitle = element_text(hjust = 0.5)
                             )
                           
                         }
                         
                       })
                     
                     output$sample_selectedCutting_Plot_info_newSample <-
                       renderText({
                         paste0(
                           "Mouse position: ",
                           xy_str(
                             input$sample_selectedCutting_hover_newSample,
                             "CE-time",
                             "M+H"
                           ),
                           "Click: ",
                           xy_str(
                             input$sample_selectedCutting_click_newSample,
                             "CE-time",
                             "M+H"
                           )
                         )
                       })
                     ## Bulle info
                     
                     #### info-bulle
                     output$sample_selectedCutting_hover_info_newSample <-
                       renderUI({
                         hover <- req(input$sample_selectedCutting_hover_newSample)
                         
                         if (!is.null(rVars$sample_cutting_viewer_ConvertTime_newSample)) {
                           point <-
                             suppressWarnings(
                               nearPoints(
                                 req(
                                   rVars$sample_cutting_viewer_ConvertTime_newSample
                                 )
                               ),
                               hover,
                               threshold = 5,
                               maxpoints = 1,
                               addDist = TRUE
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
                                   "<span class='bullText'> CE-time: </span>",
                                   round(point$CE.time, 2),
                                   "<br/>",
                                   "<span class='bullText'> M+H: </span>",
                                   round(point$M.H, 4),
                                   "<br/>"
                                 )
                               )))
                         }
                         
                       })
                     
                   }
                   
                 }
               } else {
                 output$sample_selectedCutting_Plot_newSample <- renderPlot({
                   
                 })
                 output$sample_selectedCutting_Plot_info_newSample <-
                   renderText({
                     
                   })
                 output$sample_selectedCutting_hover_info_newSample <-
                   renderUI({
                     
                   })
                 hide("UnitTime_sampleCutting_id_newSample")
               }
               
               
             })



output$titleTimefilter_newSample <- renderUI(h6(paste0(
  "CE-time (", input$UnitTime_sampleCutting_newSample, ") :"
)))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Save samples cutting ~~~~~~~~~~~~~~~~~~~~~~~~#*
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

data_list_selectedCutting <- reactive({

  if (!is.null(rVars$samples_data_list)) {
    table_to_save <- rVars$samples_data_list
   
    for (i in rownames(rVars$samplesCuttingTable_newSample)) {
      if (rVars$samplesCuttingTable_newSample[i, "Unit"] == "Second") {
        if (!is.na(rVars$samplesCuttingTable_newSample[i,]$rt_min_cut_newSample) &
            !is.na(rVars$samplesCuttingTable_newSample[i,]$rt_max_cut_newSample)) {
          table_to_save[[i]] <- rVars$samples_data_list[[i]] %>%
            dplyr::filter(
              rVars$samplesCuttingTable_newSample[i,]$rt_min_cut_newSample < CE.time,
              CE.time < rVars$samplesCuttingTable_newSample[i,]$rt_max_cut_newSample
            )
           
        } else if (!is.na(rVars$samplesCuttingTable_newSample[i,]$rt_min_cut_newSample) &
                   is.na(rVars$samplesCuttingTable_newSample[i,]$rt_max_cut_newSample)) {
          table_to_save[[i]] <- rVars$samples_data_list[[i]] %>%
            dplyr::filter(rVars$samplesCuttingTable_newSample[i,]$rt_min_cut_newSample <
                            CE.time)
        } else if (is.na(rVars$samplesCuttingTable_newSample[i,]$rt_min_cut_newSample) &
                   !is.na(rVars$samplesCuttingTable_newSample[i,]$rt_max_cut_newSample)) {
          table_to_save[[i]] <- rVars$samples_data_list[[i]] %>%
            dplyr::filter(CE.time < rVars$samplesCuttingTable_newSample[i,]$rt_max_cut_newSample)
        } else if (is.na(rVars$samplesCuttingTable_newSample[i,]$rt_min_cut_newSample) &
                   is.na(rVars$samplesCuttingTable_newSample[i,]$rt_max_cut_newSample)) {
          table_to_save[[i]] <- rVars$samples_data_list[[i]]
        }
      } else if (rVars$samplesCuttingTable_newSample[i, "Unit"] == "Minute") {
        if (!is.na(rVars$samplesCuttingTable_newSample[i,]$rt_min_cut_newSample) &
            !is.na(rVars$samplesCuttingTable_newSample[i,]$rt_max_cut_newSample)) {
          table_to_save[[i]] <- rVars$samples_data_list[[i]] %>%
            dplyr::filter(
              rVars$samplesCuttingTable_newSample[i,]$rt_min_cut_newSample < CE.time / 60,
              CE.time / 60 < rVars$samplesCuttingTable_newSample[i,]$rt_max_cut_newSample
            )
        } else if (!is.na(rVars$samplesCuttingTable_newSample[i,]$rt_min_cut_newSample) &
                   is.na(rVars$samplesCuttingTable_newSample[i,]$rt_max_cut_newSample)) {
          table_to_save[[i]] <- rVars$samples_data_list[[i]] %>%
            dplyr::filter(rVars$samplesCuttingTable_newSample[i,]$rt_min_cut_newSample <
                            CE.time / 60)
        } else if (is.na(rVars$samplesCuttingTable_newSample[i,]$rt_min_cut_newSample) &
                   !is.na(rVars$samplesCuttingTable_newSample[i,]$rt_max_cut_newSample)) {
          table_to_save[[i]] <- rVars$samples_data_list[[i]] %>%
            dplyr::filter(CE.time / 60 < rVars$samplesCuttingTable_newSample[i,]$rt_max_cut_newSample)
        } else if (is.na(rVars$samplesCuttingTable_newSample[i,]$rt_min_cut_newSample) &
                   is.na(rVars$samplesCuttingTable_newSample[i,]$rt_max_cut_newSample)) {
          table_to_save[[i]] <- rVars$samples_data_list[[i]]
        }
      }


    }
    
    table_to_save


  }

})


