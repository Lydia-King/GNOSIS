#' ---
#' title: 'GNOSIS Session Code'
#' ---
#'
#'
#' Load up relevant libraries
library(DT)
library(tidyverse)
library(fabricatr)
library(reshape2)
library(operator.tools)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(partykit)
library(survival)
library(survminer)
library(stats)
library(rstatix)
library(DescTools)
library(car)
library(R.utils)
library(maftools)
#'
#'
#' Complete Function
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
#' Get Mode function
getmode <- function(v) {uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]}
#'
#'
#'
setwd("/home/lydia/Desktop/METABRIC_Data_Paper")
#' Tab 1: Load up clinical patient data and preview
##' Note: Please replace temporary file path with path to uploaded file
dataInputClinicalP <- read.csv("data_clinical_patient.txt", header = TRUE, sep = "\t", quote = "\"", na.strings = c("", " ", "NA"), skip = 4L)
Patient_Validated <- dataInputClinicalP
datatable(Patient_Validated, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "410px"))
#'
##' Get total number of columns in data
Tot_Pat_Col <- ncol(dataInputClinicalP)
Tot_Pat_Col
#'
##' Get total number of rows in data
Tot_Pat_Row <- nrow(dataInputClinicalP)
Tot_Pat_Row
#'
#'
#' Tab 1: Load up clinical sample data and preview
##' Note: Please replace temporary file path with path to uploaded file
dataInputClinicalS <- read.csv("data_clinical_sample.txt", header = TRUE, sep = "\t", quote = "\"", na.strings = c("", " ", "NA"), skip = 4L)
Sample_Validated <- dataInputClinicalS
datatable(Sample_Validated, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "410px"))
#'
##' Get total number of columns in data
Tot_Samp_Col <- ncol(dataInputClinicalS)
Tot_Samp_Col
#'
##' Get total number of rows in data
Tot_Samp_Row <- nrow(dataInputClinicalS)
Tot_Samp_Row
#'
#'
#' Tab 1: Merge patient/sample data and preview
dataClinical <- merge(dataInputClinicalP, dataInputClinicalS, by.x = "PATIENT_ID", by.y = "PATIENT_ID")
datatable(dataClinical, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "410px"))
#'
#'
#' Tab 1: Load up CNA data and preview
##' Note: Please replace temporary file path with path to uploaded file
dataInputCNA <- read.csv("data_CNA.txt", header = TRUE, sep = "\t", quote = "\"", check.names = F, na.strings = c("", " ", "NA"), skip = 0L)
CNA_Validated <- dataInputCNA
datatable(CNA_Validated, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "390px"))
#'
##' Get total number of columns in data
Tot_CNA_Col <- ncol(dataInputCNA)
Tot_CNA_Col
#'
##' Get total number of rows in data
Tot_CNA_Row <- nrow(dataInputCNA)
Tot_CNA_Row
#'
#'
#' Tab 1: Load up MAF data and preview
##' Note: Please replace temporary file path with path to uploaded file
#'
##' Get total number of columns in data
#'
##' Get total number of rows in data
#'
#'
#' Tab 2: Clinical variables datatable
#'
#'
#' Tab 2: CNA datatable
#'
#'
#' Tab 2: MAF datatable
#'
#'
#' Tab 3: Convert variables
##' Check clinical variable types
Formatted_Data <- dataClinical %>%
  mutate_at(NULL, funs(as.numeric(.))) %>%
  mutate_at(c("CELLULARITY", "CHEMOTHERAPY","ER_IHC", "HER2_SNP6", 
              "HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", 
              "CLAUDIN_SUBTYPE", "THREEGENE", "LATERALITY", "RADIO_THERAPY", 
              "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED",
              "ER_STATUS", "HER2_STATUS","GRADE", "PR_STATUS", "TUMOR_STAGE"), funs(as.factor(.)))

str(Formatted_Data, list.len = ncol(Formatted_Data))
#'
##' Check clinical variable levels
sapply(Formatted_Data, levels)
#'
#'
#' Tab 3: Subset data
##' Filter and preview clinical data
Clinical_Sub_1 <- filter(Formatted_Data, Formatted_Data[, "CLAUDIN_SUBTYPE"] %in% c("LumA", "LumB"))
Clinical_Sub <- Clinical_Sub_1 %>%
  mutate_at(c("CELLULARITY", "CHEMOTHERAPY","ER_IHC", "HER2_SNP6", "HORMONE_THERAPY", 
              "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", 
              "LATERALITY", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", 
              "CANCER_TYPE_DETAILED","ER_STATUS", "HER2_STATUS","GRADE", "PR_STATUS", "TUMOR_STAGE"), funs(as.factor(as.character(.))))

datatable(Clinical_Sub, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "350px"))
#'
##' Check selected variable levels
sapply(Clinical_Sub[, c("CLAUDIN_SUBTYPE", "LYMPH_NODES_EXAMINED_POSITIVE", "NPI")], levels)
#'
#'
#' Tab 3: Survival recoding
dataClinicalSurv <- Clinical_Sub %>%
  mutate(OS = ifelse(Clinical_Sub[, "OS_STATUS"] == "DECEASED", 1, 0), DSS = ifelse(Clinical_Sub[, "VITAL_STATUS"] == "Died of Disease", 1, 0))
TableRecode_Pre <- dataClinicalSurv[, c("OS_STATUS", "VITAL_STATUS", "OS", "DSS")]
datatable(TableRecode_Pre, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "350px"))
#'
#'
#' Tab 3: CNA selection and calculation
CNA_Clin <- {
  PATIENT_ID <- colnames(dataInputCNA[, 3L:ncol(dataInputCNA)])
  Scores <- as.data.frame(PATIENT_ID)
  Scores$CNA_Score <- colSums(abs(dataInputCNA[, 3L:ncol(dataInputCNA)]), na.rm = TRUE)
  Scores <- na.omit(Scores)
  Scores$Amp_Score <- na.omit(apply(X = dataInputCNA[, 3L:ncol(dataInputCNA)], MARGIN = 2, function(x) sum(x[x > 0], na.rm = TRUE)))
  Scores$Del_Score <- na.omit(apply(X = dataInputCNA[, 3L:ncol(dataInputCNA)], MARGIN = 2, function(x) sum(x[x < 0], na.rm = TRUE)))
  Scores$Del_Score <- abs(Scores$Del_Score)
  if (TRUE == "TRUE") {
    Scores$Score_Quartile <- split_quantile(x = Scores$CNA_Score, type = 4L)
  }
  CNA_Metrics <- Scores
  CNA_Metrics_All <- merge(dataClinicalSurv, CNA_Metrics, by.x = "PATIENT_ID", by.y = "PATIENT_ID")
  if (TRUE == "TRUE") {
    CNA_Metrics_All$Subset_Score_Quartile <- split_quantile(x = CNA_Metrics_All$CNA_Score, type = 4L)
  }
  CNA_Metrics_All
}
datatable(CNA_Clin, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "400px"))
#'
#'
#' Tab 3: CNA calculation save file preview
Whole_Data <- CNA_Clin
datatable(Whole_Data, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "400px"))
#'
#'
#' Tab 4: Exploratory Plots - Boxplot
#'
#'
#' Tab 4: Exploratory Plots - Scatterplot
#'
#'
#' Tab 4: Exploratory Plots - Barplots
#'
#'
#' Tab 4: Exploratory Plots - Histogram
CNAHistogram <- ggplot(Whole_Data, aes(Whole_Data[, "CNA_Score"])) +
  geom_histogram(aes(color = "Color", fill = "Color"), show.legend = FALSE, binwidth = 1000L, alpha = 0.4) +
  ggtitle("Histogram of absolute CNA Scores") +
  ylab("Frequency") +
  xlab("CNA Scores") +
  scale_color_manual(values = c(Color = "#2ac0db")) +
  scale_fill_manual(values = c(Color = "#2ac0db")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
  theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
  theme(axis.text.x = element_text(size = 15)) +
  theme(axis.text.y = element_text(size = 15)) +
  theme(legend.title = element_text(colour = "black", size = 15, face = "bold")) +
  theme(strip.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15))
CNAHistogram
#'
#'
#' Tab 4: Exploratory Plots - Facetwrap Histogram
#'
#'
#' Tab 4: Exploratory Plots - Density Plot
CNADense <- ggplot(Whole_Data) +
  geom_density(aes(x = Whole_Data[, "CNA_Score"], color = "Color", fill = "Color"), na.rm = FALSE, alpha = 0.4) +
  xlab("CNA Scores") +
  ylab("Density") +
  ggtitle("Density plots of absolute CNA Scores") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
  theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
  theme(axis.text.x = element_text(size = 15)) +
  theme(axis.text.y = element_text(size = 15)) +
  theme(legend.title = element_text(colour = "black", size = 15, face = "bold")) +
  theme(strip.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  scale_color_manual(values = c(Color = "#2ac0db")) +
  scale_fill_manual(values = c(Color = "#2ac0db")) +
  theme(legend.position = "none") +
  xlim(range(density(Whole_Data[, "CNA_Score"], na.rm = T)$x))
CNADense
#'
#'
#' Tab 4: Exploratory Plots - Segmented Density Plot
CNADist1Plot <- {
  colourCount <- 4L
  getPalette <- colorRampPalette(brewer.pal(9, "Blues"))
  lab <- as.character(1:4L)
  dt <- data.frame(x = c(1:length(Whole_Data[, "CNA_Score"])), y = Whole_Data[, "CNA_Score"])
  dt <- na.omit(dt)
  dens <- density(dt$y)
  df <- data.frame(x = dens$x, y = dens$y)
  probs1 <- c(0:4L / 4L)
  probs <- probs1[-c(1, length(probs1))]
  quantiles <- quantile(dt$y, prob = probs)
  df$quant <- factor(findInterval(df$x, quantiles))
  ggplot(df, aes(x, y)) +
    geom_line() +
    geom_ribbon(aes(ymin = 0, ymax = y, fill = quant)) +
    scale_x_continuous(breaks = quantiles) +
    xlab("CNA Scores") +
    ylab("Density") +
    ggtitle("Segmented Density Plots of CNA Scores") +
    theme(legend.position = c(0.9, 0.5)) +
    theme(legend.key.size = unit(0.9, "cm")) +
    theme(plot.title = element_text(hjust = 0.5, size = 18)) +
    theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
    theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
    theme(axis.text.x = element_text(size = 15)) +
    theme(axis.text.y = element_text(size = 15)) +
    theme(legend.title = element_text(colour = "black", size = 15, face = "bold")) +
    theme(strip.text = element_text(size = 15)) +
    theme(legend.text = element_text(size = 15)) +
    scale_fill_manual(values = getPalette(colourCount), labels = lab, name = "Legend")
}
CNADist1Plot
#'
#'
#' Tab 4: Exploratory Plots - FacetWrap Density Plot
#'
#'
#' Tab 4: Exploratory Plots - Histogram and Density Plot
CNABoth <- ggplot(Whole_Data) +
  geom_density(aes(x = Whole_Data[, "CNA_Score"], color = "Color", fill = "Color"), na.rm = FALSE, alpha = 0.4) +
  geom_histogram(aes(x = Whole_Data[, "CNA_Score"], y = ..density.., color = "Color", fill = "Color"), alpha = 0.4, fill = "#2ac0db", position = "identity") +
  xlab("CNA Scores") +
  ylab("Density") +
  ggtitle("Histogram and density plots of absolute CNA Scores") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
  theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
  theme(axis.text.x = element_text(size = 15)) +
  theme(axis.text.y = element_text(size = 15)) +
  theme(legend.title = element_text(colour = "black", size = 15, face = "bold")) +
  theme(strip.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  xlim(range(density(Whole_Data[, "CNA_Score"], na.rm = T)$x)) +
  scale_color_manual(values = c(Color = "#2ac0db")) +
  scale_fill_manual(values = c(Color = "#2ac0db")) +
  theme(legend.position = "none")
CNABoth
#'
#'
#' Tab 4: Exploratory Plots - FacetWrap Histogram and Density Plot
#'
#'
#' Tab 5: Survival Plots - Clinical Variable KM Plots
##' KM Survival Plots
surv_data <- data.frame(Time = Whole_Data[["OS_MONTHS"]], Strata = Whole_Data[["GRADE"]], Cen = Whole_Data[["DSS"]])
datafit <- survfit(Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~ Strata, data = surv_data)
KMC1 <- ggsurvplot(datafit, censor.shape = "", data = surv_data, size = 1, conf.int = FALSE, pval = TRUE, risk.table = TRUE, legend = c("right"), xlab = "Survival Time", ylab = "Survival Probability", legend.title = "Legend", legend.labs = rownames(summary(datafit$table)), pval.size = 6, risk.table.height = 0.25, ggtheme = theme_gray() + theme(plot.title = element_text(size = 18, hjust = 0.5)) + theme(legend.title = element_text(colour = "black", size = 15, face = "bold")), break.time.by = 50, risk.table.y.text.col = T, risk.table.y.text = FALSE, title = ("Breast cancer patients in METABRIC data"), font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black"))
KMC1
#'
##' Logrank Tests
Logrank1 <- survdiff(Surv(Whole_Data[, "OS_MONTHS"], as.numeric(Whole_Data[, "DSS"])) ~ Whole_Data[, "GRADE"])
Logrank1
#'
#'
#' Tab 5: Survival Plots - CNA Quartile KM Plots
##' KM Survival Plots
surv_data_1 <- data.frame(Time = Whole_Data[["OS_MONTHS"]], Strata = Whole_Data[["Subset_Score_Quartile"]], cen = Whole_Data[["DSS"]])
SurvfitCNA <- survfit(Surv(as.numeric(Time), as.numeric(as.character(cen))) ~ Strata, data = surv_data_1)
PercentSurvPlot <- ggsurvplot(SurvfitCNA, censor.shape = "", xlab = "Survival Time", ylab = "Survival Probability", data = surv_data_1, size = 1, conf.int = FALSE, pval = TRUE, risk.table = TRUE, legend = c("right"), legend.labs = rownames(summary(SurvfitCNA$table)), risk.table.height = 0.25, pval.size = 6, ggtheme = theme_gray() + theme(plot.title = element_text(size = 18, hjust = 0.5)) + theme(legend.title = element_text(colour = "black", size = 15, face = "bold")), break.time.by = 50, risk.table.y.text.col = T, risk.table.y.text = FALSE, legend.title = "Legend", title = ("Breast cancer patients in METABRIC data"), font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black"))
PercentSurvPlot
#'
##' Logrank Tests
Logrank2 <- survdiff(Surv(Whole_Data[, "OS_MONTHS"], as.numeric(Whole_Data[, "DSS"])) ~ Whole_Data[, "Subset_Score_Quartile"])
Logrank2
#'
#'
#' Tab 5: Survival Plots - KM Plots for Treatment (Yes)
##' KM Survival Plots
#'
##' Logrank Tests
#'
#'
#' Tab 5: Survival Plots - KM Plots for Treatment (No)
##' KM Survival Plots
#'
##' Logrank Tests
#'
#'
#' Tab 6: Association Tests - Chi-Squared
##' Individual Chi-squared tests
data_Association2 <- {
  my_list <- list()
  for (i in 1:length(c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY"))) {
    my_list[[i]] <- table(Whole_Data[, "Subset_Score_Quartile"], Whole_Data[, c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY")[i]])
  }
  my_list
}
Chi_Code <- {
  Chi_List <- list()
  for (i in 1:length(c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY"))) {
    name <- c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY")[i]
    Chi_List[[name]] <- chisq.test(data_Association2[[i]])
  }
  Chi_List
}
Chi_Code
#'
##' Adjusted Chi-squared tests
data_Association1Ad <- {
  Variables <- Pval <- X <- df <- c()
  for (i in 1:length(c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY"))) {
    Variables <- c(Variables, paste("Subset_Score_Quartile", "&", c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY")[i], sep = " "))
    Test <- chisq.test(data_Association2[[i]])
    X <- c(X, round(as.numeric(Test$statistic), digits = 3))
    df <- c(df, as.numeric(Test$parameter))
    Pval <- c(Pval, signif(Test$p.value, digits = 3))
  }
  Table <- cbind.data.frame(Variables, X, df, Pval)
  Table$Adj_Pval <- signif(p.adjust(Table$Pval, method = "BH", n = length(c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY"))), digits = 3)
  Table %>%
    select(Variables, X, df, Pval, Adj_Pval)
}
datatable(data_Association1Ad, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 10, scrollX = TRUE))
#'
#'
#' Tab 6: Association Tests - FET
##' Individual FET
F_Code <- {
  F_List <- list()
  for (i in 1:length(c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY"))) {
    name <- c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY")[i]
    tryCatch(
      {
        F_List[[name]] <- fisher.test(data_Association2[[i]], simulate.p.value = F)
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )
  }
  F_List
}
F_Code
#'
##' Adjusted FET
data_Association3Ad <- {
  Variables <- Pval <- c()
  for (i in 1:length(c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY"))) {
    tryCatch(
      {
        Test <- fisher.test(data_Association2[[i]])
        Pval <- c(Pval, signif(Test$p.value, digits = 3))
        Variables <- c(Variables, paste("Subset_Score_Quartile", "&", c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY")[i], sep = " "))
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )
  }
  Table <- cbind.data.frame(Variables, Pval)
  Table$Adj_Pval <- signif(p.adjust(Table$Pval, method = "BH", n = length(c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY"))), digits = 3)
  Table %>%
    select(Variables, Pval, Adj_Pval)
}
DT::datatable(data_Association3Ad, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 10, scrollX = TRUE))
#'
#'
#' Tab 6: Association Tests - Simulated FET
##' Individual Simulated FET
SimF_Code <- {
  SimF_List <- list()
  for (i in 1:length(c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY"))) {
    name <- c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY")[i]
    SimF_List[[name]] <- fisher.test(data_Association2[[i]], simulate.p.value = T)
  }
  SimF_List
}
SimF_Code
#'
##' Adjusted Simulated FET
data_Association2Ad <- {
  Variables <- Pval <- c()
  for (i in 1:length(c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY"))) {
    Variables <- c(Variables, paste("Subset_Score_Quartile", "&", c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY")[i], sep = " "))
    Test <- fisher.test(data_Association2[[i]], simulate.p.value = T)
    Pval <- c(Pval, signif(Test$p.value, digits = 3))
  }
  Table <- cbind.data.frame(Variables, Pval)
  Table$Adj_Pval <- signif(p.adjust(Table$Pval, method = "BH", n = length(c("ER_IHC","HER2_SNP6","HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "CLAUDIN_SUBTYPE", "THREEGENE", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY", "CANCER_TYPE_DETAILED", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_STAGE", "CHEMOTHERAPY"))), digits = 3)
  Table %>%
    select(Variables, Pval, Adj_Pval)
}
DT::datatable(data_Association2Ad, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 10, scrollX = TRUE))
#'
#'
#' Tab 6: Association Tests - ANOVA Assumptions
##' Levene's Test
#'
##' Fligner test
#'
##' Shapiro Test
#'
#'
#' Tab 6: Association Tests - ANOVA
##' Individual ANOVA
#'
##' Adjusted ANOVA
#'
#'
#' Tab 6: Association Tests - Kruskal-Wallis
##' Individual Kruskal-Wallis
KW_Code <- {
  KW_List <- list()
  for (i in 1:length(c("NPI", "LYMPH_NODES_EXAMINED_POSITIVE", "AGE_AT_DIAGNOSIS", "TUMOR_SIZE"))) {
    name <- c("NPI", "LYMPH_NODES_EXAMINED_POSITIVE", "AGE_AT_DIAGNOSIS", "TUMOR_SIZE")[i]
    KW_List[[name]] <- kruskal.test(Whole_Data[, c("NPI", "LYMPH_NODES_EXAMINED_POSITIVE", "AGE_AT_DIAGNOSIS", "TUMOR_SIZE")[i]] ~ Whole_Data[, "Subset_Score_Quartile"], data = Whole_Data)
  }
  KW_List
}
KW_Code
#'
##' Adjusted Kruskal-Wallis
data_KWAdj <- {
  Variables <- Statistic <- df <- Pval <- c()
  for (i in 1:length(c("NPI", "LYMPH_NODES_EXAMINED_POSITIVE", "AGE_AT_DIAGNOSIS", "TUMOR_SIZE"))) {
    Variables <- c(Variables, paste("Subset_Score_Quartile", "&", c("NPI", "LYMPH_NODES_EXAMINED_POSITIVE", "AGE_AT_DIAGNOSIS", "TUMOR_SIZE")[i], sep = " "))
    Test <- kruskal.test(Whole_Data[, c("NPI", "LYMPH_NODES_EXAMINED_POSITIVE", "AGE_AT_DIAGNOSIS", "TUMOR_SIZE")[i]] ~ Whole_Data[, "Subset_Score_Quartile"], data = Whole_Data)
    Statistic <- c(Statistic, signif(Test$statistic[[1]], digits = 3))
    df <- c(df, Test$parameter[[1]])
    Pval <- c(Pval, signif(as.numeric(Test$p.value[[1]]), digits = 3))
  }
  Table <- cbind.data.frame(Variables, Statistic, df, Pval)
  Table$Adj_Pval <- signif(p.adjust(Table$Pval, method = "BH", n = length(c("NPI", "LYMPH_NODES_EXAMINED_POSITIVE", "AGE_AT_DIAGNOSIS", "TUMOR_SIZE"))), digits = 3)
  Table %>%
    select(Variables, Statistic, df, Pval, Adj_Pval)
}
DT::datatable(data_KWAdj, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 10, scrollX = TRUE))
#'
#'
#' Tab 6: Association Tests - Pairwise Comparisons
##' Pairwise t-test
#'
##' Pairwise Dunn's Test
#'
#'
#' Tab 7: Cox PH models - Univariate Cox Models
##' Individual univariate Cox models
surv_data_Cox <- Whole_Data %>%
  select(!!"OS_MONTHS", !!"DSS", c(!!c("LYMPH_NODES_EXAMINED_POSITIVE", "NPI", "CELLULARITY", "CHEMOTHERAPY", "ER_IHC", "HER2_SNP6", "HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "AGE_AT_DIAGNOSIS", "CLAUDIN_SUBTYPE", "THREEGENE","LATERALITY", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY","CANCER_TYPE_DETAILED", "ER_STATUS", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_SIZE", "TUMOR_STAGE")))
CoxModelOut_1 <- for (i in 1:length(c("LYMPH_NODES_EXAMINED_POSITIVE", "NPI", "CELLULARITY", "CHEMOTHERAPY", "ER_IHC", "HER2_SNP6", "HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "AGE_AT_DIAGNOSIS", "CLAUDIN_SUBTYPE", "THREEGENE","LATERALITY", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY","CANCER_TYPE_DETAILED", "ER_STATUS", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_SIZE", "TUMOR_STAGE"))) {
  cat(noquote(paste("DSS", "for", c("LYMPH_NODES_EXAMINED_POSITIVE", "NPI", "CELLULARITY", "CHEMOTHERAPY", "ER_IHC", "HER2_SNP6", "HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "AGE_AT_DIAGNOSIS", "CLAUDIN_SUBTYPE", "THREEGENE","LATERALITY", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY","CANCER_TYPE_DETAILED", "ER_STATUS", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_SIZE", "TUMOR_STAGE")[i], "\n", "\n")))
  res.cox <- coxph(formula = as.formula(paste("Surv(as.numeric(", "OS_MONTHS", "), as.numeric(as.character(", "DSS", "))) ~", noquote(paste(c("LYMPH_NODES_EXAMINED_POSITIVE", "NPI", "CELLULARITY", "CHEMOTHERAPY", "ER_IHC", "HER2_SNP6", "HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "AGE_AT_DIAGNOSIS", "CLAUDIN_SUBTYPE", "THREEGENE","LATERALITY", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY","CANCER_TYPE_DETAILED", "ER_STATUS", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_SIZE", "TUMOR_STAGE")[i], collapse = "+")))), data = surv_data_Cox)
  print(summary(res.cox))
}
#'
##' Adjusted univariate Cox models
data_UniAdj <- {
  Variables <- LRT <- Wald <- Logrank <- c()
  for (i in 1:length(c("LYMPH_NODES_EXAMINED_POSITIVE", "NPI", "CELLULARITY", "CHEMOTHERAPY", "ER_IHC", "HER2_SNP6", "HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "AGE_AT_DIAGNOSIS", "CLAUDIN_SUBTYPE", "THREEGENE","LATERALITY", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY","CANCER_TYPE_DETAILED", "ER_STATUS", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_SIZE", "TUMOR_STAGE"))) {
    Variables <- c(Variables, paste("DSS", "for", c("LYMPH_NODES_EXAMINED_POSITIVE", "NPI", "CELLULARITY", "CHEMOTHERAPY", "ER_IHC", "HER2_SNP6", "HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "AGE_AT_DIAGNOSIS", "CLAUDIN_SUBTYPE", "THREEGENE","LATERALITY", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY","CANCER_TYPE_DETAILED", "ER_STATUS", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_SIZE", "TUMOR_STAGE")[i], sep = " "))
    res.cox <- coxph(formula = as.formula(paste("Surv(as.numeric(", "OS_MONTHS", "), as.numeric(as.character(", "DSS", "))) ~", noquote(paste(c("LYMPH_NODES_EXAMINED_POSITIVE", "NPI", "CELLULARITY", "CHEMOTHERAPY", "ER_IHC", "HER2_SNP6", "HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "AGE_AT_DIAGNOSIS", "CLAUDIN_SUBTYPE", "THREEGENE","LATERALITY", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY","CANCER_TYPE_DETAILED", "ER_STATUS", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_SIZE", "TUMOR_STAGE")[i], collapse = "+")))), data = surv_data_Cox)
    LRT <- c(LRT, signif(as.numeric(summary(res.cox)$logtest[3]), digits = 3))
    Wald <- c(Wald, signif(as.numeric(summary(res.cox)$waldtest[3]), digits = 3))
    Logrank <- c(Logrank, signif(as.numeric(summary(res.cox)$sctest[3]), digits = 3))
  }
  Table <- cbind.data.frame(Variables, LRT, Wald, Logrank)
  Table$Adj_LRT <- signif(p.adjust(as.numeric(Table$LRT), method = "BH", n = length(c("LYMPH_NODES_EXAMINED_POSITIVE", "NPI", "CELLULARITY", "CHEMOTHERAPY", "ER_IHC", "HER2_SNP6", "HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "AGE_AT_DIAGNOSIS", "CLAUDIN_SUBTYPE", "THREEGENE","LATERALITY", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY","CANCER_TYPE_DETAILED", "ER_STATUS", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_SIZE", "TUMOR_STAGE"))), digits = 3)
  Table$Adj_Wald <- signif(p.adjust(as.numeric(Table$Wald), method = "BH", n = length(c("LYMPH_NODES_EXAMINED_POSITIVE", "NPI", "CELLULARITY", "CHEMOTHERAPY", "ER_IHC", "HER2_SNP6", "HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "AGE_AT_DIAGNOSIS", "CLAUDIN_SUBTYPE", "THREEGENE","LATERALITY", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY","CANCER_TYPE_DETAILED", "ER_STATUS", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_SIZE", "TUMOR_STAGE"))), digits = 3)
  Table$Adj_Logrank <- signif(p.adjust(as.numeric(Table$Logrank), method = "BH", n = length(c("LYMPH_NODES_EXAMINED_POSITIVE", "NPI", "CELLULARITY", "CHEMOTHERAPY", "ER_IHC", "HER2_SNP6", "HORMONE_THERAPY", "INFERRED_MENOPAUSAL_STATE", "INTCLUST", "AGE_AT_DIAGNOSIS", "CLAUDIN_SUBTYPE", "THREEGENE","LATERALITY", "RADIO_THERAPY", "HISTOLOGICAL_SUBTYPE", "BREAST_SURGERY","CANCER_TYPE_DETAILED", "ER_STATUS", "HER2_STATUS", "GRADE", "PR_STATUS", "TUMOR_SIZE", "TUMOR_STAGE"))), digits = 3)
  Table %>% select(Variables, LRT, Adj_LRT, Wald, Adj_Wald, Logrank, Adj_Logrank)
}
DT::datatable(data_UniAdj, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 10, scrollX = TRUE))
#'
#'
#' Tab 7: Cox PH models - Multivariable Cox Models
surv_data_CoxM <- Whole_Data %>%
  select(!!"OS_MONTHS", !!"DSS", c(!!c("HER2_STATUS", "GRADE", "TUMOR_SIZE", "AGE_AT_DIAGNOSIS", "LYMPH_NODES_EXAMINED_POSITIVE")), c(!!c("CLAUDIN_SUBTYPE", "Subset_Score_Quartile")))
CoxModel <- summary(coxph(formula = as.formula(paste("Surv(as.numeric(", "OS_MONTHS", "), as.numeric(as.character(", "DSS", "))) ~", noquote(paste(paste(c("HER2_STATUS", "GRADE", "TUMOR_SIZE", "AGE_AT_DIAGNOSIS", "LYMPH_NODES_EXAMINED_POSITIVE"), collapse = "+"), "+", paste("(", paste(c("CLAUDIN_SUBTYPE", "Subset_Score_Quartile"), collapse = "+"), ")^2", sep = ""), sep = "")))), data = surv_data_CoxM))
CoxModel
CoxModel$waldtest
CoxModel$logtest
CoxModel$sctest
#'
#'
#' Tab 7: Cox PH models - Multivariable Cox Model Assumptions
CoxAssump <- coxph(formula = as.formula(paste("Surv(as.numeric(", "OS_MONTHS", "), as.numeric(as.character(", "DSS", "))) ~", noquote(paste(paste(c("HER2_STATUS", "GRADE", "TUMOR_SIZE", "AGE_AT_DIAGNOSIS", "LYMPH_NODES_EXAMINED_POSITIVE"), collapse = "+"), "+", paste("(", paste(c("CLAUDIN_SUBTYPE", "Subset_Score_Quartile"), collapse = "+"), ")^2", sep = ""), sep = "")))), data = surv_data_CoxM)
Assump <- ggcoxzph(cox.zph(CoxAssump, terms = FALSE))
Assump
#'
#'
#' Tab 8: Adjusted Survival Curves - New Data
NewData <- {
  Levels_Vector_1 <- Levels_Rep <- c()
  for (n_levels_1 in 1:length(levels(as.factor(Whole_Data[, "Subset_Score_Quartile"])))) {
    Levels_Vector_1 <- c(Levels_Vector_1, c(rep(levels(as.factor(Whole_Data[, "Subset_Score_Quartile"]))[n_levels_1], length(levels(as.factor(Whole_Data[, "CLAUDIN_SUBTYPE"]))))))
  }
  for (n_levels_2 in 1:length(levels(as.factor(Whole_Data[, "CLAUDIN_SUBTYPE"])))) {
    Levels_Rep <- c(Levels_Rep, levels(as.factor(Whole_Data[, "CLAUDIN_SUBTYPE"]))[n_levels_2])
  }
  Levels_Vector_2 <- c(rep(Levels_Rep, length(levels(as.factor(Whole_Data[, "Subset_Score_Quartile"])))))
  Lev <- data.frame(Row = 1:length(Levels_Vector_2)) %>%
    mutate(`:=`(!!"CLAUDIN_SUBTYPE", Levels_Vector_2), `:=`(!!"Subset_Score_Quartile", Levels_Vector_1)) %>%
    select(!!"CLAUDIN_SUBTYPE", !!"Subset_Score_Quartile")
  for (constant_var in 1:length(c("GRADE", "LYMPH_NODES_EXAMINED_POSITIVE", "AGE_AT_DIAGNOSIS", "TUMOR_SIZE", "HER2_STATUS"))) {
    varname <- c("GRADE", "LYMPH_NODES_EXAMINED_POSITIVE", "AGE_AT_DIAGNOSIS", "TUMOR_SIZE", "HER2_STATUS")[constant_var]
    if (is.factor(Whole_Data[, varname])) {
      Lev[, c(varname)] <- getmode(Whole_Data[, varname])
    } else {
      Lev <- mutate(Lev, `:=`(!!varname, mean(Whole_Data[, varname], na.rm = T)))
    }
  }
  Lev
}
DT::datatable(NewData, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30, scrollX = TRUE, scrollY = "270px"))
#'
#'
#' Tab 8: Adjusted Survival Curves - All Adjusted Survival Curves
dat <- NewData
rownames(dat) <- do.call(paste,c(dat[c("CLAUDIN_SUBTYPE", "Subset_Score_Quartile")], sep="_"))
fit <- survfit(CoxAssump, newdata = dat)
fit_All <- survminer::ggsurvplot(fit, data = NewData, censor.shape = "", xlab = "Survival Time", ylab = "Survival Probability", size = 1, conf.int = FALSE, risk.table = FALSE, legend = c("right"), legend.labs = rownames(summary(fit$table)), risk.table.height = 0.25, pval.size = 6, ggtheme = theme_gray() + theme(plot.title = element_text(size = 18, hjust = 0.5)) + theme(legend.title = element_text(colour = "black", size = 15, face = "bold")), break.time.by = 50, risk.table.y.text.col = T, risk.table.y.text = FALSE, legend.title = "Legend", title = ("Breast cancer patients in METABRIC data"), font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black"))
fit_All
#'
#'
#' Tab 8: Adjusted Survival Curves - Single Adjusted Survival Curves
dat <- NewData %>% filter(CLAUDIN_SUBTYPE == "LumA")
rownames(dat) <- do.call(paste,c(dat[c("CLAUDIN_SUBTYPE", "Subset_Score_Quartile")], sep="_"))
fit <- survfit(CoxAssump, newdata = dat)
fit_All <- survminer::ggsurvplot(fit, data = NewData, censor.shape = "", xlab = "Survival Time", ylab = "Survival Probability", size = 1, conf.int = FALSE, risk.table = FALSE, legend = c("right"), legend.labs = rownames(summary(fit$table)), risk.table.height = 0.25, pval.size = 6, ggtheme = theme_gray() + theme(plot.title = element_text(size = 18, hjust = 0.5)) + theme(legend.title = element_text(colour = "black", size = 15, face = "bold")), break.time.by = 50, risk.table.y.text.col = T, risk.table.y.text = FALSE, legend.title = "Legend", title = ("Breast cancer patients in METABRIC data"), font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black"))
fit_All

dat <- NewData %>% filter(CLAUDIN_SUBTYPE == "LumB")
rownames(dat) <- do.call(paste,c(dat[c("CLAUDIN_SUBTYPE", "Subset_Score_Quartile")], sep="_"))
fit <- survfit(CoxAssump, newdata = dat)
fit_All <- survminer::ggsurvplot(fit, data = NewData, censor.shape = "", xlab = "Survival Time", ylab = "Survival Probability", size = 1, conf.int = FALSE, risk.table = FALSE, legend = c("right"), legend.labs = rownames(summary(fit$table)), risk.table.height = 0.25, pval.size = 6, ggtheme = theme_gray() + theme(plot.title = element_text(size = 18, hjust = 0.5)) + theme(legend.title = element_text(colour = "black", size = 15, face = "bold")), break.time.by = 50, risk.table.y.text.col = T, risk.table.y.text = FALSE, legend.title = "Legend", title = ("Breast cancer patients in METABRIC data"), font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black"))
fit_All
#'
#'
#' Tab 9: Recursive Partitioning Survival Trees - Rpart
##' Rpart Tree Formula
FormulaRpart <- noquote(paste(c("HER2_STATUS", "GRADE", "TUMOR_SIZE", "AGE_AT_DIAGNOSIS", "LYMPH_NODES_EXAMINED_POSITIVE", "CLAUDIN_SUBTYPE", "Subset_Score_Quartile"), collapse = "+"))
FormulaRpart
#'
##' Rpart Tree Plot
Whole_Data_Rpart <- Whole_Data %>%
  filter(eval(parse(text = "OS_MONTHS")) > 0) %>%
  completeFun(data = ., c("DSS"))
pfit <- rpart(paste("Surv(", "as.numeric(", "OS_MONTHS", ")", ",", "DSS", ") ~ ", FormulaRpart, sep = ""), data = Whole_Data_Rpart, method = "exp", model = TRUE, control = rpart.control(minsplit = 20L, minbucket = 20L, cp = 0.01, xval = 10L, maxdepth = 25L))
RpartTreePlot_Re <- plot(partykit::as.party(pfit))
RpartTreePlot_Re
#'
##' Rpart Tree Survival Curves
data_node_info <- Whole_Data_Rpart %>%
  mutate(Node_Rpart = as.factor(pfit$where)) %>%
  data.frame(Time = .[["OS_MONTHS"]], Node = .[["Node_Rpart"]], Cen = .[["DSS"]]) %>%
  select(Time, Node, Cen)
Surv_Curve <- survfit(Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~ Node, data = data_node_info)
PercentSurvPlotRpart <- ggsurvplot(Surv_Curve, censor.shape = "", xlab = "Survival Time", ylab = "Survival Probability", data = data_node_info, size = 1, conf.int = FALSE, risk.table = FALSE, pval = FALSE, legend = c("right"), legend.labs = rownames(summary(Surv_Curve$table)), risk.table.height = 0.25, pval.size = 6, ggtheme = theme_gray() + theme(plot.title = element_text(size = 18, hjust = 0.5)) + theme(legend.title = element_text(colour = "black", size = 15, face = "bold")), break.time.by = 50, risk.table.y.text.col = T, risk.table.y.text = FALSE, legend.title = "Legend", title = ("Breast cancer patients in METABRIC data"), font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black"))
PercentSurvPlotRpart
#'
#'
#' Tab 9: Recursive Partitioning Survival Trees - Ctree
##' Ctree Formula
FormulaCtree <- noquote(paste(c("HER2_STATUS", "GRADE", "TUMOR_SIZE", "AGE_AT_DIAGNOSIS", "LYMPH_NODES_EXAMINED_POSITIVE", "CLAUDIN_SUBTYPE", "Subset_Score_Quartile"), collapse = "+"))
FormulaCtree
#'
##' Ctree Plot
set.seed(1004)
Whole_Data_Ctree <- Whole_Data %>%
  completeFun(data = ., c("DSS"))
pfitctree <- partykit::ctree(as.formula(paste("Surv(", "OS_MONTHS", ",", "DSS", ") ~ ", FormulaCtree, sep = "")), data = Whole_Data_Ctree, control = ctree_control(teststat = "quadratic", splitstat = "quadratic", testtype = "Bonferroni", alpha = 0.05, minsplit = 20L, minbucket = 20L, minprob = 0.01, stump = FALSE, maxvar = 20L, maxdepth = 20L))
CtreeTreePlot_Re <- plot(pfitctree)
CtreeTreePlot_Re
#'
##' Ctree Survival Curves
data_node_info_ctree <- Whole_Data_Ctree %>%
  mutate(Node_Ctree = as.factor(predict(pfitctree, type = "node"))) %>%
  data.frame(Time = .[["OS_MONTHS"]], Node = .[["Node_Ctree"]], Cen = .[["DSS"]]) %>%
  select(Time, Node, Cen)
Surv_Curvectree <- survfit(Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~ Node, data = data_node_info_ctree)
PercentSurvPlotCtree <- ggsurvplot(Surv_Curvectree, censor.shape = "", xlab = "Survival Time", ylab = "Survival Probability", data = data_node_info_ctree, size = 1, conf.int = FALSE, risk.table = FALSE, pval = FALSE, legend = c("right"), legend.labs = rownames(summary(Surv_Curvectree$table)), risk.table.height = 0.25, pval.size = 6, ggtheme = theme_gray() + theme(plot.title = element_text(size = 18, hjust = 0.5)) + theme(legend.title = element_text(colour = "black", size = 15, face = "bold")), break.time.by = 50, risk.table.y.text.col = T, risk.table.y.text = FALSE, legend.title = "Legend", title = ("Breast cancer patients in METABRIC data"), font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black"))
PercentSurvPlotCtree
#'
#'
#' Tab 10: Maftools - Summary
##' MAF Summary
#'
##' Sample Summary
#'
##' Gene Summary
#'
##' Get Fields
#'
##' Associated Clinical Data
#'
#'
#' Tab 10: Maftools - Plots
##' MAF Summary Plot
#'
##' Oncoplot
#'
##' Oncostrip
#'
##' Transitions and Transversions
#'
##' Lollipop plot 1
#'
##' Lollipop plot 2
#'
##' Lollipop plot 3
#'
##' Mutation load plot
#'
##' Somatic interactions
