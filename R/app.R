#' #' Initialise GNOSIS
#'
#' @description
#' The shiny app will allow the user to explore and analyse clinical
#' and copy number data uploaded manually or selected from a dataframe
#' containing datasets available through cBioPortal.
#'
#'
#' @details Note. This shiny app is an updated version of the app
#' published in [HRB Open Research](https://hrbopenresearch.org/articles/5-8#).
#' For details on version updates see NEWS.
#'
#' @param ... Further arguments to the `runApp` function
#'
#' @md
#'
#' @return Mainly called for the side effect of displaying the shiny app in a
#'   browser
#'
#' @import DT fontawesome shinycssloaders cBioPortalData shinyjs reshape2 RColorBrewer survival survminer stats compareGroups rpart partykit
#' @importFrom DescTools DunnTest
#' @importFrom car leveneTest
#' @importFrom rstatix shapiro_test
#' @importFrom fabricatr split_quantile
#' @importFrom shinylogs track_usage
#' @importFrom shinylogs store_null
#'
#' @examples
#' if (interactive()) {
#'     GNOSIS()
#' }
#' @export
GNOSIS <- function(...) {
    # Global options
    options(warn = 0)
    options(shiny.maxRequestSize = 30 * 1024^3)
    options(spinner.size = 1)
    options(spinner.type = 8)
    options(spinner.color = "#012B45")

    ui <- dashboardPage(
        header = dashboardHeader(title = tagList(span(
            class = "logo-lg", "GNOSIS"
        ))),
        sidebar = dashboardSidebar(
            tags$style(".left-side, .main-sidebar {padding-top: 60px}"),
            width = 195,
            GNOSIS_sidebarUI("sidebar")
        ),
        body = dashboardBody(
            shinyDashboardThemes(theme = "blue_gradient"),
            includeCSS(system.file("www/Style_File.css", package = "GNOSIS")),
            tags$head(tags$style(
                HTML(
                    ".wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}"
                )
            )),

            # Tab 1) Input Data Files: manually or by using cBioPortal API to select study
            tabItems(
                tabItem(
                    tabName = "input_files_1",
                    tabBox(
                        width = "1000px",
                        Tab1_Input_Files_Manual_UI("tab1_input_manually"),
                        Tab1_Input_Files_API_UI("tab1_input_API")
                    )
                ),
                tabItem(
                    tabName = "input_files_2",
                    tabBox(
                        width = "1000px",
                        Tab1_Input_Files_Preview_Clin_UI("tab1_input_preview_1"),
                        Tab1_Input_Files_Preview_CNA_UI("tab1_input_preview_2"),
                        Tab1_Input_Files_Preview_MAF_UI("tab1_input_preview_3")
                    )
                ),

                # Tab 2) Exploratory Tables: space to explore uploaded data
                tabItem(
                    tabName = "tables",
                    tabBox(
                        width = "1000px",
                        Tab2_Exploaratory_Tables_Clin_UI("tab2_clin_table"),
                        Tab2_Exploaratory_Tables_CNA_UI("tab2_cna_table"),
                        Tab2_Exploaratory_Tables_MAF_UI("tab2_maf_table")
                    )
                ),

                # Tab 3) Recode/Subset Data: reformat data
                tabItem(tabName = "FactorLevels", Tab3_Factor_Levels_UI("tab3_factor_levels")),
                tabItem(tabName = "Subset", Tab3_Subset_UI("tab3_subset")),
                tabItem(tabName = "Recode", Tab3_Recode_UI("tab3_recode")),
                tabItem(tabName = "CNACalc", Tab3_CNACalc_UI("tab3_CNAcalc")),
                tabItem(tabName = "Data_Down", Tab3_Data_Down_UI("tab3_data_download")),

                # Tab 4) Exploratory Plots
                tabItem(tabName = "boxplot", Tab4_Boxplot_UI("tab4_boxplot")),
                tabItem(tabName = "scatterplot", Tab4_Scatterplot_UI("tab4_scatterplot")),
                tabItem(tabName = "barplot", Tab4_Barplot_UI("tab4_barplot")),
                tabItem(tabName = "density_hist", Tab4_Density_UI("tab4_density")),

                # Tab 5) Kaplan-Meier Plots
                tabItem(tabName = "KMplot", Tab5_KM_Clin_UI("tab5_km_plot")),
                tabItem(tabName = "KMOver", Tab5_KM_Quart_UI("tab5_km_qurt_plot")),
                tabItem(tabName = "KMplotRadio", Tab5_KM_Treatment_UI("tab5_km_treat_plot")),

                # Tab 6) Association Tests
                tabItem(
                    tabName = "ASTest",
                    Tab6_Association_Tests_UI("tab6_association_tests")
                ),

                # Tab 7) Cox PH Models
                tabItem(tabName = "UniVar", Tab7_Univariate_CoxPH("tab7_Cox_Uni")),
                tabItem(tabName = "MultiVar", Tab7_Multivariate_CoxPH("tab7_Cox_Multi")),
                tabItem(tabName = "AssumptionsOS", Tab7_Assumption_CoxPH("tab7_Cox_Multi")),

                # Tab 8) Adjusted Survival Curves
                tabItem(
                    tabName = "AdjSurvival",
                    Tab8_Adjusted_Survival_Curves_UI("tab8_Adjusted_Survival")
                ),

                ## Tab 9) Survival Trees
                tabItem(tabName = "RpartTree", Tab9_Rpart_UI("tab9_Rpart_Tree")),
                tabItem(tabName = "CtreeTree", Tab9_Ctree_UI("tab9_Ctree_Tree")),

                ## Tab 10) Maftools Summary
                tabItem(
                    tabName = "MAFText",
                    Tab10_MAF_Text_Summary_UI("tab10_mafsummary_text")
                ),
                tabItem(
                    tabName = "MAFVis",
                    Tab10_MAF_Visual_Summary_UI("tab10_mafsummary_plot")
                ),

                ## Tab 11 Download Code/Log
                tabItem(tabName = "downlog", Tab11_Download_Code_UI("tab11_download"))
            )
        )
    )

    server <- function(input, output, session) {
        # Tab 1) Input Data Files: manually or by using cBioPortal API to select study
        selectedLine <- reactive({
            if (is.null(input$`tab1_input_API-cBioData_rows_selected`)) {
                return(NULL)
            } else {
                rows_selected <-
                    as.numeric(input$`tab1_input_API-cBioData_rows_selected`)
            }
        })

        datalist <-
            Tab1_Input_Files_Manual_Server("tab1_input_manually", rowselect = selectedLine)

        output$TotalC <- metaRender(renderPrint, {
            Count_Col(..(datalist[["patient_manual_data"]]()))
        })
        output$TotalR <- metaRender(renderPrint, {
            Count_Row(..(datalist[["patient_manual_data"]]()))
        })

        output$TotalC1 <- metaRender(renderPrint, {
            Count_Col(..(datalist[["sample_manual_data"]]()))
        })
        output$TotalR2 <- metaRender(renderPrint, {
            Count_Row(..(datalist[["sample_manual_data"]]()))
        })

        output$TotalCCNA <- metaRender(renderPrint, {
            Count_Col(..(datalist[["CNA_manual_data"]]()))
        })
        output$TotalRCNA <- metaRender(renderPrint, {
            Count_Row(..(datalist[["CNA_manual_data"]]()))
        })

        output$TotalCMAF <- metaRender(renderPrint, {
            Count_Col(..(datalist[["MAF_manual_data"]]()))
        })
        output$TotalRMAF <- metaRender(renderPrint, {
            Count_Row(..(datalist[["MAF_manual_data"]]()))
        })

        Tab1_API_Files_Server("tab1_input_API")
        Tab1_Input_Files_Preview_Server(
            "tab1_input_preview_1",
            datalist,
            "Combined_clin",
            length_px = "650px",
            select_dt = "multiple"
        )
        Tab1_Input_Files_Preview_Server(
            "tab1_input_preview_2",
            datalist,
            "CNA_Val",
            length_px = "650px",
            select_dt = "multiple"
        )
        Tab1_Input_Files_Preview_Server(
            "tab1_input_preview_3",
            datalist,
            "MAF_Val",
            length_px = "650px",
            select_dt = "multiple"
        )

        # Tab 2) Exploratory Tables: space to explore uploaded data
        Tab2_Exploratory_Tables_Server(
            "tab2_clin_table",
            datalist,
            "Combined_clin",
            length_px = "450px",
            select_dt = "multiple"
        )
        Tab2_Exploratory_Tables_Server(
            "tab2_cna_table",
            datalist,
            "CNA_Val",
            length_px = "450px",
            select_dt = "multiple"
        )
        Tab2_Exploratory_Tables_Server(
            "tab2_maf_table",
            datalist,
            "MAF_Val",
            length_px = "450px",
            select_dt = "multiple"
        )

        # Tab 3) Recode/Subset Data: reformat data
        ## Check Levels
        Formatted_Data <-
            Tab3_Factor_Levels_Server("tab3_factor_levels", datalist, "Combined_clin")
        Tab3_Render_Print_Server("tab3_factor_levels",
            Formatted_Data,
            "formatted_data",
            expression_text = "str(datalist[[data]](), list.len=ncol(datalist[[data]]()))"
        )
        Tab3_Subset_Server("tab3_subset", Formatted_Data, "formatted_data")
        Subset_Data <-
            Tab3_Subset_Server("tab3_subset", Formatted_Data, "formatted_data")
        Tab3_Recode_Server("tab3_recode", Subset_Data, "subset_data")
        Recode_Data <-
            Tab3_Recode_Server("tab3_recode", Subset_Data, "subset_data")
        Tab3_CNACalc_Server("tab3_CNAcalc",
            datalist,
            Recode_Data,
            "recode_data",
            rowselect = selectedLine
        )
        Final_Data <-
            Tab3_CNACalc_Server("tab3_CNAcalc",
                datalist,
                Recode_Data,
                "recode_data",
                rowselect = selectedLine
            )
        Tab3_Download_Server("tab3_data_download", Final_Data, "Final_DF")

        # Tab 4) Exploratory Plots
        ## Boxplots
        PlotsBP <-
            Tab4_BoxPlots_Server("tab4_boxplot", Final_Data, "Final_DF")
        Tab4_DisplayPlot_Server("tab4_boxplot", PlotsBP, "boxplot")
        Download_Server("tab4_boxplot", PlotsBP, "boxplot")

        ## Scatterplots
        PlotsSP <-
            Tab4_Scatterplot_Server("tab4_scatterplot", Final_Data, "Final_DF")
        Tab4_DisplayPlot_Server("tab4_scatterplot", PlotsSP, "scatterplot")
        Download_Server("tab4_scatterplot", PlotsSP, "scatterplot")

        ## Barplots
        PlotsBar <-
            Tab4_Barplot_Server("tab4_barplot", Final_Data, "Final_DF")
        Tab4_DisplayPlot_Server("tab4_barplot", PlotsBar, "barplot")
        Download_Server("tab4_barplot", PlotsBar, "barplot")

        ## Histograms & Density plots
        PlotsHist <-
            Tab4_Hist_Server("tab4_density", Final_Data, "Final_DF")
        Tab4_DisplayPlot_Server("tab4_density", PlotsHist, "histogram")
        Tab4_DisplayPlot_Server("tab4_density", PlotsHist, "facet_histogram")

        # Tab 5) Kaplan-Meier Plots
        ## Clinical KM Curves
        Plot_KM <-
            Tab5_KM_Plot_Server("tab5_km_plot", Final_Data, "Final_DF")
        Tab5_Display_KM_Plot_Server("tab5_km_plot", Plot_KM, "surv_plot")
        Download_Server("tab5_km_plot", Plot_KM, "surv_plot")

        ## CNA KM Curves
        Plot_KM_Quart <-
            Tab5_KM_Plot_Quart_Server("tab5_km_qurt_plot", Final_Data, "Final_DF")
        Tab5_Display_KM_Plot_Server("tab5_km_qurt_plot", Plot_KM_Quart, "surv_plot")
        Download_Server("tab5_km_qurt_plot", Plot_KM_Quart, "surv_plot")

        ## Treatment KM Curves
        Plot_KM_Treat_Yes <-
            Tab5_KM_Plot_Treatment_Server("tab5_km_treat_plot", Final_Data, "Final_DF")

        # Tab 6) Association Tests
        Tab6_Chi_Square_Server("tab6_association_tests",
            datalist,
            Final_Data,
            "Final_DF",
            rowselect = selectedLine
        )
        Tab6_FE_Sim_Server("tab6_association_tests",
            datalist,
            Final_Data,
            "Final_DF",
            rowselect = selectedLine
        )
        Tab6_FE_Server("tab6_association_tests",
            datalist,
            Final_Data,
            "Final_DF",
            rowselect = selectedLine
        )
        Tab6_ANOVA_Server("tab6_association_tests",
            datalist,
            Final_Data,
            "Final_DF",
            rowselect = selectedLine
        )
        Tab6_KW_Server("tab6_association_tests",
            datalist,
            Final_Data,
            "Final_DF",
            rowselect = selectedLine
        )
        Tab6_PWC_Server("tab6_association_tests",
            datalist,
            Final_Data,
            "Final_DF",
            rowselect = selectedLine
        )
        Tab6_CG_Server("tab6_association_tests",
            datalist,
            Final_Data,
            "Final_DF",
            rowselect = selectedLine
        )

        # Tab 7) Univariate and Multivariable Cox Models
        Tab7_Uni_Cox_Server("tab7_Cox_Uni",
            datalist,
            Final_Data,
            "Final_DF",
            rowselect = selectedLine
        )
        Tab7_Multi_Cox_Server("tab7_Cox_Multi",
            datalist,
            Final_Data,
            "Final_DF",
            rowselect = selectedLine
        )
        Assumption_Plot <-
            Tab7_Multi_Cox_Server("tab7_Cox_Multi",
                datalist,
                Final_Data,
                "Final_DF",
                rowselect = selectedLine
            )
        Download_Server("tab7_Cox_Multi", Assumption_Plot, "A_Plot")

        # Tab 8) Adjusted Survival Curves
        Tab8_Adjusted_Survival_Curves_Server(
            "tab8_Adjusted_Survival",
            datalist,
            Final_Data,
            "Final_DF",
            rowselect = selectedLine,
            previous_model = Assumption_Plot[["Previous_Model"]]
        )
        PlotsAdjSurv <-
            Tab8_Adjusted_Survival_Curves_Server(
                "tab8_Adjusted_Survival",
                datalist,
                Final_Data,
                "Final_DF",
                rowselect = selectedLine,
                previous_model = Assumption_Plot[["Previous_Model"]]
            )
        Download_Server(
            "tab8_Adjusted_Survival",
            PlotsAdjSurv,
            "Adjusted_Survival_Curves"
        )

        # Tab 9)
        ## Rpart
        Tab9_RPST_Rpart_Server("tab9_Rpart_Tree",
            datalist,
            Final_Data,
            "Final_DF",
            rowselect = selectedLine
        )
        Plot_Rpart <-
            Tab9_RPST_Rpart_Server("tab9_Rpart_Tree",
                datalist,
                Final_Data,
                "Final_DF",
                rowselect = selectedLine
            )
        Download_Server_1("tab9_Rpart_Tree", Plot_Rpart, "Rpart_plot")
        Download_Server_2("tab9_Rpart_Tree", Plot_Rpart, "Rpart_KM_Curves")

        ## Ctree
        Tab9_RPST_Ctree_Server("tab9_Ctree_Tree",
            datalist,
            Final_Data,
            "Final_DF",
            rowselect = selectedLine
        )
        Plot_Ctree <-
            Tab9_RPST_Ctree_Server("tab_Ctree_Tree",
                datalist,
                Final_Data,
                "Final_DF",
                rowselect = selectedLine
            )
        Download_Server_1("tab9_Ctree_Tree", Plot_Ctree, "Ctree_plot")
        Download_Server_2("tab9_Ctree_Tree", Plot_Ctree, "Ctree_KM_Curves")

        # Tab 10) Maftools
        ## MAF summary
        Tab10_MAF_Text_Summary_Server("tab10_mafsummary_text", datalist, rowselect = selectedLine)
        Tab10_MAF_Plot_Summary_Server("tab10_mafsummary_plot", datalist, rowselect = selectedLine)

        # Tab 11) Download Log/Script
        input_logs <- reactive({
            input$.shinylogs_input
        })

        Tab11_Log_Server("tab11_download", data_inputs = input_logs)
    }

    shinyApp(ui, server, ...)
}
