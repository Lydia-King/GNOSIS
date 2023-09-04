## **GNOSIS: An R Shiny app supporting cancer genomics survival analysis with cBioPortal**

We have developed GNOSIS, an R Shiny App, incorporating a range of R packages enabling users 
to efficiently explore and visualise clinical and genomic data obtained from cBioPortal. 
GNOSIS uses an intuitive GUI and multiple tab panels supporting a range of functionalities 
including data upload and initial exploration, data recoding and subsetting, multiple visualisations, survival analysis, 
statistical analysis and mutation analysis, in addition to facilitating reproducible research.

-----

### **GNOSIS Software Tool Article**

The version of GNOSIS published in [HRB Open Research](https://hrbopenresearch.org/articles/5-8) can be found on the GNOSIS_Software_Tool_Article branch and initialised using:  

``` r
shiny::runGitHub(repo = 'GNOSIS', username = 'Lydia-King', ref="GNOSIS_Software_Tool_Article")
```

Also available at: https://lydiaking.shinyapps.io/GNOSIS/

Can also be run by cloning repo on GNOSIS_Software_Tool_Article branch, navigating into repo and running server.R file. 

### **GNOSIS Current Version** 

This version has integrated `cBioPortalData()` functions to allow users to select and analyse cBioPortal studies,
in addition to manual upload. 

The current version of GNOSIS can be installed and initialised using:

**1. runGitHub**

``` r
shiny::runGitHub(repo = 'GNOSIS', username = 'Lydia-King', ref="main")
```

**2. Bioconductor package (TBC)**

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("GNOSIS")
```
