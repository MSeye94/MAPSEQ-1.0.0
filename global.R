'%ni%' <- Negate('%in%')

get_r_version <- function() {
  paste(R.version$major, sep = ".", R.version$minor)
}


usePackage <- function(p){
  if (!is.element(p, installed.packages()[, 1]))
    install.packages(p,
                     dep = TRUE,
                     force = TRUE,
                     repos = "http://cran.us.r-project.org")
  require(p, character.only = TRUE)
}

## Require package to start the app
usePackage("shiny")
usePackage("shinyjs")
usePackage("shinythemes")
usePackage("shinyWidgets")
usePackage("shinycssloaders")
usePackage("shinyalert")
usePackage("shinydashboard")
usePackage("shinyFiles")
usePackage("bslib")
usePackage("progressr")
usePackage("devtools")
usePackage("reticulate")
usePackage("Rcpp")
usePackage("DT")
usePackage("shinymanager")
usePackage("stringr")


initPackages <- function(session) {
  #Bioconductor packages
  pkgsBiocManager <- c(
    "RCurl",
    "tools",
    "RSQLite",
    "colourpicker",
    "yaml",
    "utils",
    "BiocParallel"
  )
  
  #CRAN packages
  pkgsCRAN <- c(
    "bigstatsr",
    "bigreadr",
    "combinat",
    "cowplot",
    "doParallel",
    "dplyr",
    "foreach",
    "ggalt",
    "ggplot2",
    "parallel",
    "grid",
    "gridExtra",
    "hrbrthemes",
    "itertools",
    "MASS",
    "MsCoreUtils",
    "np",
    "plotly",
    "patchwork",
    "Rcpp",
    "readxl",
    "signal",
    "tidyr",
    "tidyverse",
    "viridis",
    "openxlsx",
    "DBI"
  )
  
  
  allPkgs <- c(pkgsBiocManager, pkgsCRAN)
  
  idxpkgsBiocManager_missing <-
    which(!is.element(pkgsBiocManager, installed.packages()[, 1]))
  idxpkgsCRAN_missing <-
    which(!is.element(pkgsCRAN, installed.packages()[, 1]))
  
  ## Install necessaries packages if not exit
  
  if (length(idxpkgsBiocManager_missing) >= 1) {
    message("Installing packages of Biconductor...\n")
    withProgress(message = 'Installing packages of Biconductor...', value = 0, {
      for (i in idxpkgsBiocManager_missing) {
        incProgress(
          1 / length(idxpkgsBiocManager_missing),
          detail = paste("Installing ", pkgsBiocManager[i], collapse = "")
        )
        BiocManager::install(pkgsBiocManager[i])
      }
    })
    
  }
  
  
  if (length(idxpkgsCRAN_missing) >= 1) {
    message("Installing packages of CRAN...\n")
    withProgress(message = 'Installing packages of Biconductor...', value = 0, {
      for (i in idxpkgsCRAN_missing) {
        incProgress(
          1 / length(idxpkgsCRAN_missing),
          detail = paste("Installing ", pkgsCRAN[i], collapse = "")
        )
        install.packages(pkgsCRAN[i],
                         dep = TRUE,
                         force = TRUE,
                         repos = "http://cran.us.r-project.org")
      }
    })
    
    message("Installing packages finished Ok!\n")
  }
  
  
  
  ## Loading necessaries packages
  withProgress(message = 'Loading packages...', value = 0, {
    for (i in 1:length(allPkgs)) {
      incProgress(1 / length(allPkgs),
                  detail = paste("Loading ", allPkgs[i], collapse = ""))
      require(allPkgs[i], character.only = TRUE)
    }
  })
}



theme_ben <- function(base_size = 14) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      # L'ensemble de la figure
      plot.title = element_text(
        size = rel(0.85),
        face = "bold",
        color = "blue",
        margin = margin(0, 0, 5, 0),
        hjust = 0.5
      ),
      # Zone où se situe le graphique
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Les axes
      axis.title = element_text(size = rel(0.85), face = "bold.italic"),
      axis.text = element_text(size = rel(0.70), face = "bold.italic"),
      #axis.text.y = element_text(margin = margin(r = 0.8 *(100/2) / 2), hjust = 1),
      axis.line = element_line(
        color = "black",
        arrow = arrow(length = unit(0.3, "lines"), type = "closed")
      ),
      # La légende
      legend.title = element_text(
        size = rel(0.85),
        face = "bold.italic",
        hjust = 0.5
      ),
      legend.text = element_text(size = rel(0.70), face = "bold.italic"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      # Les étiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(
        size = rel(1),
        face = "bold.italic",
        color = "white",
        margin = margin(5, 0, 5, 0)
      )
    )
}


xy_str <- function(e, x, y) {
  if (is.null(e))
    return("NULL\n")
  paste0(x, " = ", round(e$x, 0), ", ", y, " = ", round(e$y, 4), "\n")
}

######### Function to generate ID
createID <- function(ref, number) {
  ID <-
    ID <-
    sprintf(paste0(ref, "%0", ceiling(log10(number + 1L)), "d"), 1L:number)
  return(ID)
}

### Volume for shinyFiles
volumes <- c(Home = fs::path_home(), getVolumes()())