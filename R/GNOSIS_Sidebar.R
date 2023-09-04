# UI function for sidebar menu
GNOSIS_sidebarUI <- function(GNOSIS_sidebar_ui) {
    ns <- NS(GNOSIS_sidebar_ui)
    tagList(
        sidebarMenu(
            menuItem(
                "Input Files",
                tabName = "input_files",
                icon = fontawesome::fa_i(name = "file-alt", verify_fa = FALSE),
                menuSubItem("File Upload", tabName = "input_files_1"),
                menuSubItem("Preview", tabName = "input_files_2")
            ),
            menuItem(
                "Exploratory Tables",
                tabName = "tables",
                icon = fontawesome::fa_i(name = "table", verify_fa = FALSE)
            ),
            menuItem(
                "Recode/Subset Data",
                tabName = "Recode1",
                icon = fontawesome::fa_i(name = "sort-alpha-down", verify_fa = FALSE),
                menuSubItem("Variable Types/Levels", tabName = "FactorLevels"),
                menuSubItem("Subset/Filter", tabName = "Subset"),
                menuSubItem("Recode Survival", tabName = "Recode"),
                menuSubItem("CNA Score Data", tabName = "CNACalc"),
                menuSubItem("File Download", tabName = "Data_Down")
            ),
            menuItem(
                "Exploratory Plots",
                tabName = "plots",
                icon = fontawesome::fa_i(name = "chart-bar", verify_fa = FALSE),
                menuSubItem(
                    "Boxplots",
                    tabName = "boxplot",
                    icon = fontawesome::fa_i(name = "align-left", verify_fa = FALSE)
                ),
                menuSubItem(
                    "Scatterplots",
                    tabName = "scatterplot",
                    icon = fontawesome::fa_i(name = "braille", verify_fa = FALSE)
                ),
                menuSubItem(
                    "Barplots",
                    tabName = "barplot",
                    icon = fontawesome::fa_i(name = "chart-bar", verify_fa = FALSE)
                ),
                menuSubItem(
                    "Histograms/Density Plots",
                    tabName = "density_hist",
                    icon = fontawesome::fa_i(name = "chart-area", verify_fa = FALSE)
                )
            ),
            menuItem(
                "Kaplan-Meier Plots",
                tabName = "survival",
                icon = fontawesome::fa_i(name = "heartbeat", verify_fa = FALSE),
                menuSubItem("KM Plot Clinical Variables", tabName = "KMplot"),
                menuSubItem("KM Plot CNA Score", tabName = "KMOver"),
                menuSubItem("KM Plot by Treatment", tabName = "KMplotRadio")
            ),
            menuItem(
                "Association Tests",
                tabName = "ASTest",
                icon = fontawesome::fa_i(name = "connectdevelop", verify_fa = FALSE)
            ),
            menuItem(
                "Cox PH Models",
                tabName = "Cox",
                icon = fontawesome::fa_i(name = "file-medical-alt", verify_fa = FALSE),
                menuSubItem("Univariate Cox Models", tabName = "UniVar"),
                menuSubItem("Multivariable Cox Models", tabName = "MultiVar"),
                menuSubItem("Cox Model Assumptions", tabName = "AssumptionsOS")
            ),
            menuItem(
                "Adjusted Survival Curves",
                tabName = "AdjSurvival",
                icon = fontawesome::fa_i(name = "heartbeat", verify_fa = FALSE)
            ),
            menuItem(
                "Survival Trees",
                tabName = "SurvTrees",
                icon = fontawesome::fa_i(name = "tree", verify_fa = FALSE),
                menuSubItem("Rpart", tabName = "RpartTree"),
                menuSubItem("Ctree", tabName = "CtreeTree")
            ),
            menuItem(
                "Maftools Summary",
                tabName = "MAFplots",
                icon = fontawesome::fa_i(name = "exclamation-triangle", verify_fa = FALSE),
                menuSubItem("MAF Text Summary", tabName = "MAFText"),
                menuSubItem("MAF Visual Summary", tabName = "MAFVis")
            ),
            menuItem(
                "Download Code/Log",
                tabName = "downlog",
                icon = fontawesome::fa_i(name = "file-download", verify_fa = FALSE)
            )
        )
    )
}
