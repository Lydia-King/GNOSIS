# UI function to produce univariate Cox models
Tab7_Univariate_CoxPH <- function(id) {
    ns <- NS(id)

    fluidRow(
        box(
            title = "Univariate Cox Proportional Hazards Model",
            dropdownMenu = boxDropdown(
                icon = fa_i(name = "info-circle", verify_fa = FALSE),
                boxDropdownItem(
                    HTML(paste(
                        "Useful Resources for Cox Models:", "<br/>"
                    )),
                    tags$a(
                        href = "http://www.sthda.com/english/wiki/cox-proportional-hazards-model",
                        "Cox PH Model Basics"
                    ),
                    tags$a(
                        href = "http://www.sthda.com/english/wiki/cox-model-assumptions",
                        "Model Assumptions"
                    )
                )
            ),
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            status = "primary",
            withSpinner(verbatimTextOutput(ns("CoxModelOut"), placeholder = TRUE)),
            br(),
            h4(strong("Adjusted P-values:")),
            withSpinner(dataTableOutput(ns("UniAdjusted"))),
            sidebar = boxSidebar(
                id = "Tab7_Univariate_Cox_Sidebar",
                width = 25,
                background = "#599740",
                icon = icon("rectangle-list"),
                selectizeInput(
                    ns("Tab7_Univariate_Cox_Survival_Time"),
                    "Survival Time:",
                    choices = "",
                    width = "95%"
                ),
                selectizeInput(
                    ns("Tab7_Univariate_Cox_Event_Status"),
                    "Event Status:",
                    choices = "",
                    width = "95%"
                ),
                selectizeInput(
                    ns("Tab7_Univariate_Cox_Select_Variables"),
                    "Select Variable:",
                    choices = "",
                    multiple = TRUE,
                    width = "95%"
                )
            )
        )
    )
}

# UI function to produce multivariable Cox models
Tab7_Multivariate_CoxPH <- function(id) {
    ns <- NS(id)

    fluidRow(
        box(
            title = "Multivariable Cox Proportional Hazards Model",
            height = "500px",
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            status = "primary",
            dropdownMenu = boxDropdown(
                icon = fa_i(name = "info-circle", verify_fa = FALSE),
                boxDropdownItem(
                    HTML(paste(
                        "Useful Resources for Cox Models:", "<br/>"
                    )),
                    tags$a(
                        href = "http://www.sthda.com/english/wiki/cox-proportional-hazards-model",
                        "Cox PH Model Basics in R"
                    ),
                    tags$a(
                        href = "http://www.sthda.com/english/wiki/cox-model-assumptions",
                        "Model Assumptions in R"
                    )
                )
            ),
            withSpinner(verbatimTextOutput(
                ns("CoxModelMultiOut"),
                placeholder = TRUE
            )),
            h5(strong("Likelihood Ratio Test:")),
            verbatimTextOutput(ns("LRTid"), placeholder = TRUE),
            h5(strong("Wald Test:")),
            verbatimTextOutput(ns("Waldtestid"), placeholder = TRUE),
            h5(strong("Logrank Test:")),
            verbatimTextOutput(ns("Logrid"), placeholder = TRUE),
            sidebar = boxSidebar(
                id = "Tab7_Multivariable_Cox_Sidebar",
                width = 25,
                background = "#599740",
                icon = icon("rectangle-list"),
                selectizeInput(
                    ns("Tab7_Multivariable_Cox_Survival_Time"),
                    "Survival Time:",
                    choices = "",
                    width = "95%"
                ),
                selectizeInput(
                    ns("Tab7_Multivariable_Cox_Event_Status"),
                    "Event Status:",
                    choices = "",
                    width = "95%"
                ),
                selectizeInput(
                    ns("Tab7_Multivariable_Cox_Select_Variables"),
                    "Select Variable:",
                    choices = "",
                    multiple = TRUE,
                    width = "95%"
                ),
                selectizeInput(
                    ns(
                        "Tab7_Multivariable_Cox_Select_Interaction_Variables_1"
                    ),
                    "Select 2-Way Interaction 1:",
                    choices = "",
                    multiple = TRUE,
                    width = "95%"
                ),
                selectizeInput(
                    ns(
                        "Tab7_Multivariable_Cox_Select_Interaction_Variables_2"
                    ),
                    "Select 2-Way Interaction 2:",
                    choices = "",
                    multiple = TRUE,
                    width = "95%"
                ),
                selectizeInput(
                    ns(
                        "Tab7_Multivariable_Cox_Select_Interaction_Variables_3"
                    ),
                    "Select 3-way Interaction:",
                    choices = "",
                    multiple = TRUE,
                    width = "95%"
                )
            )
        )
    )
}

# UI function to produce Cox model assumptions
Tab7_Assumption_CoxPH <- function(id) {
    ns <- NS(id)

    fluidRow(
        box(
            title = "Multivariable Cox PH Model Assumptions",
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            status = "primary",
            dropdownMenu = boxDropdown(
                icon = fa_i(name = "info-circle", verify_fa = FALSE),
                boxDropdownItem(
                    HTML(paste(
                        "Useful Resources for Cox Models:", "<br/>"
                    )),
                    tags$a(
                        href = "http://www.sthda.com/english/wiki/cox-proportional-hazards-model",
                        "Cox PH Model Basics in R"
                    ),
                    tags$a(
                        href = "http://www.sthda.com/english/wiki/cox-model-assumptions",
                        "Model Assumptions in R"
                    )
                )
            ),
            sidebar = boxSidebar(
                id = "Tab7_Cox_Assumptions_Sidebar",
                width = 25,
                background = "#599740",
                icon = icon("rectangle-list"),
                h5(strong("Options:")),
                prettyToggle(
                    inputId = ns("Tab7_Cox_Assumptions_Display_by_Variable"),
                    label_on = "Display by Variable",
                    label_off = "Display by Variable",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                tags$hr(width = "96%"),
                numericInput(
                    ns("Plot_Width"),
                    "Plot Width (inches):",
                    value = 8,
                    min = 1,
                    max = 50,
                    width = "95%"
                ),
                numericInput(
                    ns("Plot_Height"),
                    "Plot Height (inches):",
                    value = 5,
                    min = 1,
                    max = 50,
                    width = "95%"
                ),
                tags$hr(width = "96%"),
                downloadButton(ns("Download_PNG"),
                    "Download Plot (PNG)",
                    style = "width:96%;"
                ),
                br(),
                br(),
                downloadButton(ns("Download_SVG"),
                    "Download Plot (SVG)",
                    style = "width:96%;"
                )
            ),
            withSpinner(plotOutput(ns(
                "AssumptionsCox"
            ), height = "600px"))
        )
    )
}
