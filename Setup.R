## Install and load up libraries 
# Shiny Dashboard setup

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
library(BiocManager)

if (!requireNamespace("shiny", quietly = TRUE))
  BiocManager::install("shiny")
if (!requireNamespace("shinymeta", quietly = TRUE))
  BiocManager::install("shinymeta")
if (!requireNamespace("dashboardthemes", quietly = TRUE))
  BiocManager::install("dashboardthemes")
if (!requireNamespace("shinydashboardPlus", quietly = TRUE))
  BiocManager::install("shinydashboardPlus")
if (!requireNamespace("shinyWidgets", quietly = TRUE))
  BiocManager::install("shinyWidgets")
if (!requireNamespace("shinycssloaders", quietly = TRUE))
  BiocManager::install("shinycssloaders")
if (!requireNamespace("shinylogs", quietly = TRUE))
  BiocManager::install("shinylogs")

library(shiny) # Build shiny app
library(shinymeta) # R Script
library(shinydashboard) # For shiny dashboard layout
library(dashboardthemes) # For shiny dashboard layout
library(shinydashboardPlus) # For shiny dashboard layout
library(shinyWidgets) # For shiny dashboard layout 
library(shinycssloaders) # For loading spinners
library(shinylogs) # For input Log
library(fontawesome) # For icons

# Data Tables and Formatting of Data
if (!requireNamespace("DT", quietly = TRUE))
  BiocManager::install("DT")
if (!requireNamespace("tidyverse", quietly = TRUE))
  BiocManager::install("tidyverse")
if (!requireNamespace("fabricatr", quietly = TRUE))
  BiocManager::install("fabricatr")
if (!requireNamespace("reshape2", quietly = TRUE))
  BiocManager::install("reshape2")
if (!requireNamespace("operator.tools", quietly = TRUE))
  BiocManager::install("operator.tools")

library(DT) # Used to create scrollable datatables 
library(tidyverse) # manipulate and select and stringr, ggplot2
library(fabricatr) # function split_quantile
library(reshape2) # Melt function
library(operator.tools) # not in

# Figures (Exploratory plots, survival KM curves and Survival Trees)
 if (!requireNamespace("rpart", quietly = TRUE))
  BiocManager::install("rpart")
if (!requireNamespace("rpart.plot", quietly = TRUE))
  BiocManager::install("rpart.plot")
if (!requireNamespace("partykit", quietly = TRUE))
  BiocManager::install("partykit")
if (!requireNamespace("survminer", quietly = TRUE))
  BiocManager::install("survminer")

library(rpart) # Survival trees (rpart)
library(rpart.plot) # Survival trees (rpart)
library(partykit) # Survival trees (Ctree)
library(survminer) # Survival analysis
library(survival) # Survival analysis

# Association/Stat test 
if (!requireNamespace("rstatix", quietly = TRUE))
  BiocManager::install("rstatix")
if (!requireNamespace("DescTools", quietly = TRUE))
  BiocManager::install("DescTools")

library(stats)
library(rstatix)
library(DescTools) # Dunns Test
library(car) # leveneTest

# maftools
if (!requireNamespace("R.utils", quietly = TRUE))
  BiocManager::install("R.utils")
library(R.utils)

options(repos = BiocManager::repositories())
if (!requireNamespace("maftools", quietly = TRUE))
  BiocManager::install("maftools")
library(maftools)
library(RColorBrewer) # Color palette

validate <- shiny::validate

## Allow large file uploads, set spinner up and sanitize errors
options(shiny.maxRequestSize=30*1024^3) 
options(spinner.size=1)
options(spinner.type = 8)
options(spinner.color="#012B45")

## Functions 
# Function to get complete cases
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Get Mode 
getmode <- function(v) {uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]}

# experiment with the multiplier to find the perfect position
give.n <- function(x){return(c(y = median(x)*1.05, label = length(x)))}   

