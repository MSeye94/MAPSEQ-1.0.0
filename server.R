# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## Extend size for inputs files
  options(shiny.maxRequestSize = 60000 * 1024 ^ 2)  
  
  # Load necessaries packages
  initPackages(session = session)
  
  #---------------------------------------------------------------#
  #-------------------Initialize all reactive vals --------------#
  #---------------------------------------------------------------#
  rVars<-reactiveValues(
    
    #---- Upload PSMs File ----#
    
    psms_file = NULL,
    psms_file_formatted = NULL,
    
    #---- Upload samples ----#
    ref_data = NULL,
    ref_data_valid = NULL,
    
    Valid_ref = TRUE,
    samples_data = NULL,
    samples_data_list = NULL,
    
    
    #---- fliter sample ----#
    samplesCuttingTable_newSample = NULL,
    sample_cutting_newSample = NULL,
    sample_cutting_viewer_ConvertTime_newSample = NULL,
    Second_Cutting_newSample = TRUE,
    
    #--- CE-time Correction ---#
    features_samples_list  = NULL,
    features_samples_KernelDensityCorrection = NULL,
    Data_Plot.newSample = NULL,
    modelKernelDensity = NULL,
    Data_Plot.after_KernelDensity = NULL,
    
    #--- Matching sequence to the refernce file ---#
    res_match_data_list = NULL,
    res_match_toSave = NULL
    
  )
  
  
  #---------------------------------------------------------------#
  #------------------- Upload files ------------------------------#
  #---------------------------------------------------------------#
  source("server/UploadFiles.server.R", local = TRUE)
  
  #---------------------------------------------------------------#
  #------------------- CE-time filter ----------------------------#
  #---------------------------------------------------------------#
  source("server/cuttingSample.server.R", local = TRUE)
  
  #---------------------------------------------------------------#
  #------------------- CE-time correction ------------------------#
  #---------------------------------------------------------------#
  source("server/CEtimeCorrection.server.R", local = TRUE)
  
  #---------------------------------------------------------------#
  #------------------- Matching sequences ------------------------#
  #---------------------------------------------------------------#
  source("server/Matching.server.R", local = TRUE)
  
  

}