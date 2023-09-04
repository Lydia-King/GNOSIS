# UI function to display association tests
Tab6_Association_Tests_UI <- function(id) {
    ns <- NS(id)

    box(
        collapsible = TRUE,
        title = "Association Tests",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        dropdownMenu = boxDropdown(
            icon = fontawesome::fa_i(name = "info-circle", verify_fa = FALSE),
            boxDropdownItem(
                HTML(
                    paste(
                        "Please make sure you run the appropriate statistical tests",
                        "for the question of interest, that all relevant assumptions",
                        "are met and that you are aware of how to correctly interpret",
                        "the output.",
                        sep = "<br/>"
                    ),
                    "<br/> <br/>",
                    "You can do this by using the resources for each statistical",
                    "test below:",
                    "<br/>"
                ),
                tags$a(
                    href = "https://www.statology.org/chi-square-test-of-independence/",
                    "Chi-Square Test of Independence Basics"
                ),
                tags$a(
                    href = "https://www.statology.org/chi-square-test-assumptions/",
                    "Chi-Square Test of Independence Assumptions"
                ),
                tags$a(
                    href = "https://www.statology.org/chi-square-test-of-independence-in-r/",
                    "Chi-Square Test of Independence in R"
                ),
                tags$a(
                    href = "https://www.statology.org/fishers-exact-test/",
                    "Fisher's Exact Test Basics"
                ),
                tags$a(
                    href = "https://www.statology.org/how-to-report-fishers-exact-test/",
                    "How to Report Fisher's Exact Test"
                ),
                tags$a(
                    href = "https://www.statology.org/fishers-exact-test-in-r",
                    "Fisher's Exact Test in R"
                ),
                tags$a(
                    href = "https://www.statology.org/one-way-anova/",
                    "One-Way ANOVA Basics"
                ),
                tags$a(
                    href = "https://www.statology.org/anova-assumptions/",
                    "One-Way ANOVA Assumptions"
                ),
                tags$a(
                    href = "https://www.statology.org/one-way-anova-r/",
                    "One-Way ANOVA in R"
                ),
                tags$a(
                    href = "https://www.statology.org/kruskal-wallis-test/",
                    "Kruskal-Wallis Test Basics"
                ),
                tags$a(
                    href = "https://www.statology.org/kruskal-wallis-test-in-r/",
                    "Kruskal-Wallis Test in R"
                ),
                tags$a(
                    href = "https://www.statology.org/two-sample-t-test/",
                    "Two Sample t-Test Basics"
                ),
                tags$a(
                    href = "https://www.statology.org/welchs-t-test/",
                    "Welch's t-Test Basics"
                ),
                tags$a(
                    href = "https://www.statology.org/interpret-t-test-results-in-r/",
                    "How to Interpret t-Test Results in R"
                ),
                tags$a(
                    href = "https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/pairwise.t.test",
                    "Documentation for pairwise.t.test Function"
                ),
                tags$a(
                    href = "https://www.statology.org/dunns-test/",
                    "Dunn's Test Basics"
                ),
                tags$a(
                    href = "https://www.statology.org/dunns-test-in-r/",
                    "Dunn's Test in R"
                ),
                tags$a(
                    href = "https://cran.r-project.org/web/packages/compareGroups/vignettes/compareGroups_vignette.html",
                    "compareGroups Vignette"
                )
            )
        ),
        sidebar = boxSidebar(
            id = "Tab6_Association_Test_Sidebar",
            width = 25,
            background = "#599740",
            icon = shiny::icon("rectangle-list"),
            style = "height:580px; padding:10px",
            selectInput(
                ns("Tab6_Select_Association_Test"),
                "Association Test:",
                c(
                    "Chi-squared test",
                    "Fishers exact test",
                    "Simulated Fishers exact test",
                    "ANOVA test",
                    "Kruskal-Wallis test",
                    "Pairwise t-test",
                    "Dunns test",
                    "Compare groups"
                ),
                selected = "Chi-squared test",
                width = "95%"
            ),
            conditionalPanel(
                condition = "input.Tab6_Select_Association_Test == 'Chi-squared test'",
                ns = ns,
                selectizeInput(
                    ns("Tab6_Select_Categorical_Variable_1"),
                    "Select Categorical Variable 1:",
                    choices = "",
                    width = "95%"
                ),
                selectizeInput(
                    ns("Tab6_Select_Categorical_Variable_2"),
                    "Select Categorical Variables 2:",
                    choices = "",
                    multiple = TRUE,
                    width = "95%"
                )
            ),
            conditionalPanel(
                condition = "input.Tab6_Select_Association_Test == 'Fishers exact test'",
                ns = ns,
                selectizeInput(
                    ns("Tab6_Select_Categorical_Variable_1"),
                    "Select Categorical Variable 1:",
                    choices = "",
                    width = "95%"
                ),
                selectizeInput(
                    ns("Tab6_Select_Categorical_Variable_2"),
                    "Select Categorical Variables 2:",
                    choices = "",
                    multiple = TRUE,
                    width = "95%"
                )
            ),
            conditionalPanel(
                condition = "input.Tab6_Select_Association_Test ==  'Simulated Fishers exact test'",
                ns = ns,
                selectizeInput(
                    ns("Tab6_Select_Categorical_Variable_1"),
                    "Select Categorical Variable 1:",
                    choices = "",
                    width = "95%"
                ),
                selectizeInput(
                    ns("Tab6_Select_Categorical_Variable_2"),
                    "Select Categorical Variables 2:",
                    choices = "",
                    multiple = TRUE,
                    width = "95%"
                )
            ),
            conditionalPanel(
                condition = "input.Tab6_Select_Association_Test == 'ANOVA test' |
                             input.Tab6_Select_Association_Test == 'Kruskal-Wallis test' |
                             input.Tab6_Select_Association_Test == 'Pairwise t-test' |
                             input.Tab6_Select_Association_Test == 'Dunns test'",
                ns = ns,
                selectizeInput(
                    ns("Tab6_Select_Categorical_Variable_3"),
                    "Select Categorical Variable:",
                    choices = "",
                    width = "95%"
                ),
                selectizeInput(
                    ns("Tab6_Select_Continuous_Variable_1"),
                    "Select Continuous Variables:",
                    choices = "",
                    width = "95%",
                    multiple = TRUE
                )
            ),
            conditionalPanel(
                condition = "input.Tab6_Select_Association_Test == 'Compare groups'",
                ns = ns,
                selectizeInput(
                    ns("Tab6_Select_Response_Variable"),
                    "Select Response (Group) Variable:",
                    choices = "",
                    width = "95%",
                    multiple = FALSE
                ),
                selectizeInput(
                    ns("Tab6_Select_Explanatory_Variable"),
                    "Select Explanatory Variables:",
                    choices = "",
                    width = "95%",
                    multiple = TRUE
                )
            )
        ),
        conditionalPanel(
            condition = "input.Tab6_Select_Association_Test == 'Chi-squared test'",
            ns = ns,
            h4(strong("Chi-Squared Test:")),
            verbatimTextOutput(ns("Cat1")),
            br(),
            h4(strong("Adjusted P-values:")),
            shinycssloaders::withSpinner(dataTableOutput(ns("Cat1Ad")))
        ),
        conditionalPanel(
            condition = "input.Tab6_Select_Association_Test == 'Fishers exact test'",
            ns = ns,
            h4(strong("Fisher's Exact Test:")),
            verbatimTextOutput(ns("Cat3")),
            br(),
            h4(strong("Adjusted P-values:")),
            shinycssloaders::withSpinner(dataTableOutput(ns("Cat3Ad")))
        ),
        conditionalPanel(
            condition = "input.Tab6_Select_Association_Test == 'Simulated Fishers exact test'",
            ns = ns,
            h4(strong("Simulated Fisher's Exact Test:")),
            verbatimTextOutput(ns("Cat2")),
            br(),
            h4(strong("Adjusted P-values:")),
            shinycssloaders::withSpinner(dataTableOutput(ns("Cat2Ad")))
        ),
        conditionalPanel(
            condition = "input.Tab6_Select_Association_Test == 'ANOVA test'",
            ns = ns,
            h4(strong("ANOVA Assumptions:")),
            h5(strong("Parametric Test for Equal Variance 1:")),
            verbatimTextOutput(ns("ANOVAAss1")),
            h5(strong(
                "Non-Parametric Test for Equal Variance 2:"
            )),
            verbatimTextOutput(ns("ANOVAAss2")),
            h5(strong("Test for Normality:")),
            verbatimTextOutput(ns("ANOVAAss3")),
            h4(strong("ANOVA Test:")),
            verbatimTextOutput(ns("ANOVA")),
            br(),
            h4(strong("Adjusted P-values:")),
            shinycssloaders::withSpinner(dataTableOutput(ns("ANOVAAd")))
        ),
        conditionalPanel(
            condition = "input.Tab6_Select_Association_Test == 'Kruskal-Wallis test'",
            ns = ns,
            h4(strong("Kruskal-Wallis Test:")),
            verbatimTextOutput(ns("KW")),
            br(),
            h4(strong("Adjusted P-values:")),
            shinycssloaders::withSpinner(dataTableOutput(ns("KWAd")))
        ),
        conditionalPanel(
            condition = "input.Tab6_Select_Association_Test == 'Pairwise t-test'",
            ns = ns,
            h4(strong("Pairwise Comparisons: t-test")),
            div(style = "height:500px;", verbatimTextOutput(ns("PC")))
        ),
        conditionalPanel(
            condition = "input.Tab6_Select_Association_Test == 'Dunns test'",
            ns = ns,
            h4(strong("Pairwise Comparisons: Dunn's Test")),
            div(style = "height:500px;", verbatimTextOutput(ns("Dunn")))
        ),
        conditionalPanel(
            condition = "input.Tab6_Select_Association_Test == 'Compare groups'",
            ns = ns,
            h4(strong("Compare Groups:")),
            div(style = "height:300px;", verbatimTextOutput(ns("CG"))),
            br(),
            h4(strong("Compare Groups Table:")),
            div(style = "height:300px;", verbatimTextOutput(ns("CG_Table")))
        )
    )
}
