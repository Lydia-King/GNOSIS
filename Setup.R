## MIT License
# Copyright (c) 2021 Lydia King

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

## Install and load up libraries 
# Shiny Dashboard setup

options(warn = 0)

if (!require("BiocManager", quietly = TRUE))
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
if (!requireNamespace("compareGroups", quietly = TRUE))
  BiocManager::install("compareGroups")

library(stats)
library(rstatix)
library(DescTools) # Dunns Test
library(compareGroups)
library(car) # leveneTest

if (!requireNamespace("R.utils", quietly = TRUE))
  BiocManager::install("R.utils")
library(R.utils)

if (!requireNamespace("coin", quietly = TRUE))
   BiocManager::install("coin")

library(RColorBrewer) # Color palette

# maftools
library(BiocManager)
options(repos = BiocManager::repositories())
if (!requireNamespace("maftools", quietly = TRUE))
  BiocManager::install("maftools")
library(maftools)

options("repos")

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

