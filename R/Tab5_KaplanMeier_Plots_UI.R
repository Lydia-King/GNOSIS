# UI function to produce survival curves and log rank tests for clinical variables
Tab5_KM_Clin_UI <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = "Kaplan-Meier Plot for Clinical Variables",
            dropdownMenu = boxDropdown(
                icon = fa_i(name = "info-circle", verify_fa = FALSE),
                boxDropdownItem(
                    HTML(paste(
                        "Useful Resources for Survival Analysis:", "<br/>"
                    )),
                    tags$a(href = "http://www.sthda.com/english/wiki/survival-analysis-basics", "Survival Analysis Basics in R"),
                    tags$a(href = "https://www.datacamp.com/tutorial/survival-analysis-R", "Survival Analysis in R Tutorial For Beginners"),
                    tags$a(href = "https://www.statology.org/log-rank-test-in-r/", "How to Perform a Log Rank Test in R")
                )
            ),
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            style = "height:520px; overflow-y: hidden",
            height = "520px",
            status = "primary",
            withSpinner(plotOutput(ns("Plot"), height = "500px")),
            sidebar = boxSidebar(
                width = 25,
                id = "Tab5_KM_Clinical_Sidebar",
                background = "#599740",
                icon = icon("rectangle-list"),
                selectizeInput(
                    ns("Tab5_KM_Clinical_Survival_Time"),
                    "Survival Time:",
                    choices = ""
                ),
                selectizeInput(
                    ns("Tab5_KM_Clinical_Event_Status"),
                    "Event Status:",
                    choices = ""
                ),
                selectizeInput(
                    ns("Tab5_KM_Clinical_Select_Variable"),
                    "Select Variable:",
                    choices = ""
                ),
                tags$hr(),
                prettyToggle(
                    inputId = ns("Tab5_KM_Clinical_Display_CI"),
                    label_on = "Display CI",
                    label_off = "Display CI",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab5_KM_Clinical_Display_Risk_Table"),
                    label_on = "Display Risk Table",
                    label_off = "Display Risk Table",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab5_KM_Clinical_Display_Pval"),
                    label_on = "Display P-value",
                    label_off = "Display P-value",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                selectInput(
                    ns("Tab5_KM_Clinical_Legend_Position"),
                    "Legend Position:",
                    choices = c("top", "bottom", "left", "right", "none"),
                    selected = "right"
                ),
                tags$hr(),
                textInput(
                    ns("Tab5_KM_Clinical_Plot_Title"),
                    "Plot Title:",
                    "Breast cancer patients in METABRIC data",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab5_KM_Clinical_X_Axis_Title"),
                    "X-axis Title:",
                    "Survival Time",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab5_KM_Clinical_Y_Axis_Title"),
                    "Y-axis Title:",
                    "Survival Probability",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab5_KM_Clinical_Legend_Title"),
                    "Legend Title:",
                    "Legend",
                    placeholder = TRUE
                ),
                tags$hr(),
                numericInput(
                    ns("Plot_Width"),
                    "Plot Width (inches):",
                    value = 8,
                    min = 1,
                    max = 50
                ),
                numericInput(
                    ns("Plot_Height"),
                    "Plot Height (inches):",
                    value = 5,
                    min = 1,
                    max = 50
                ),
                tags$hr(),
                downloadButton(ns("Download_PNG"), "Download Plot (PNG)",
                    style = "width:100%;"
                ),
                br(),
                br(),
                downloadButton(ns("Download_SVG"), "Download Plot (SVG)",
                    style = "width:100%;"
                )
            )
        ),
        box(
            title = "Logrank Test",
            collapsible = TRUE,
            style = "overflow-y: hidden",
            solidHeader = TRUE,
            width = 12,
            status = "primary",
            withSpinner(verbatimTextOutput(ns("KMlogrank")))
        )
    )
}

# UI function to produce survival curves and log rank tests for quartiles
Tab5_KM_Quart_UI <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = "Kaplan-Meier Plot for CNA Scores and Quartiles",
            dropdownMenu = boxDropdown(
                icon = fa_i(name = "info-circle", verify_fa = FALSE),
                boxDropdownItem(
                    HTML(paste(
                        "Useful Resources for Survival Analysis:", "<br/>"
                    )),
                    tags$a(
                        href = "http://www.sthda.com/english/wiki/survival-analysis-basics",
                        "Survival Analysis Basics in R"
                    ),
                    tags$a(
                        href = "https://www.datacamp.com/tutorial/survival-analysis-R",
                        "Survival Analysis in R Tutorial For Beginners"
                    ),
                    tags$a(
                        href = "https://www.statology.org/log-rank-test-in-r/",
                        "How to Perform a Log Rank Test in R"
                    )
                )
            ),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            style = "height:520px; overflow-y: hidden",
            height = "520px",
            collapsible = TRUE,
            withSpinner(plotOutput(ns("Plot"), height = "500px")),
            sidebar = boxSidebar(
                width = 25,
                id = "Tab5_KM_CNA_Quartile_Sidebar",
                background = "#599740",
                icon = icon("rectangle-list"),
                selectizeInput(
                    ns("Tab5_KM_CNA_Survival_Time"),
                    "Survival Time Column:",
                    choices = ""
                ),
                selectizeInput(
                    ns("Tab5_KM_CNA_Event_Status"),
                    "Event Status Column:",
                    choices = ""
                ),
                selectizeInput(
                    ns("Tab5_KM_CNA_Select_Variable"),
                    "Select Variable:",
                    choices = ""
                ),
                tags$hr(),
                prettyToggle(
                    inputId = ns("Tab5_KM_CNA_Display_CI"),
                    label_on = "Display CI",
                    label_off = "Display CI",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab5_KM_CNA_Display_Risk_Table"),
                    label_on = "Display Risk Table",
                    label_off = "Display Risk Table",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab5_KM_CNA_Display_Pval"),
                    label_on = "Display P-value",
                    label_off = "Display P-value",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                selectInput(
                    ns("Tab5_KM_CNA_Legend_Position"),
                    "Legend Position:",
                    choices = c("top", "bottom", "left", "right", "none"),
                    selected = "right"
                ),
                tags$hr(),
                textInput(
                    ns("Tab5_KM_CNA_Plot_Title"),
                    "Plot Title:",
                    "Breast cancer patients in METABRIC data",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab5_KM_CNA_X_Axis_Title"),
                    "X-axis Title:",
                    "Survival Time",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab5_KM_CNA_Y_Axis_Title"),
                    "Y-axis Title:",
                    "Survival Probability",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab5_KM_CNA_Legend_Title"),
                    "Legend Title:",
                    "Legend",
                    placeholder = TRUE
                ),
                tags$hr(),
                numericInput(
                    ns("Plot_Width"),
                    "Plot Width (inches):",
                    value = 8,
                    min = 1,
                    max = 50
                ),
                numericInput(
                    ns("Plot_Height"),
                    "Plot Height (inches):",
                    value = 5,
                    min = 1,
                    max = 50
                ),
                tags$hr(),
                downloadButton(ns("Download_PNG"),
                    "Download Plot (PNG)",
                    style = "width:100%;"
                ),
                br(),
                br(),
                downloadButton(ns("Download_SVG"),
                    "Download Plot (SVG)",
                    style = "width:100%;"
                )
            )
        ),
        box(
            title = "Logrank Test",
            collapsible = TRUE,
            style = "overflow-y: hidden",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            withSpinner(verbatimTextOutput(ns("KMlogrank1")))
        )
    )
}

# UI function to produce survival curves and log rank tests for treatment splits (binary variable == "Yes" or "No")
Tab5_KM_Treatment_UI <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = "Kaplan-Meier Plot for Treatment - Yes",
            style = "height:500px",
            height = "500px",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            status = "primary",
            withSpinner(plotOutput(ns("Plot1"), height = "465px")),
            dropdownMenu = boxDropdown(
                icon = fa_i(name = "info-circle", verify_fa = FALSE),
                boxDropdownItem(
                    HTML(
                        paste(
                            "Note: Chosen treatment variable must be coded as YES/NO i.e. either got treatment or did not.",
                            "Useful Resources for Survival Analysis:",
                            sep = "<br/> <br/>"
                        )
                    ),
                    tags$a(
                        href = "http://www.sthda.com/english/wiki/survival-analysis-basics",
                        "Survival Analysis Basics in R"
                    ),
                    tags$a(
                        href = "https://www.datacamp.com/tutorial/survival-analysis-R",
                        "Survival Analysis in R Tutorial For Beginners"
                    ),
                    tags$a(
                        href = "https://www.statology.org/log-rank-test-in-r/",
                        "How to Perform a Log Rank Test in R"
                    ),
                    id = "dropdownItem1"
                )
            ),
            sidebar = boxSidebar(
                width = 25,
                id = "Tab5_KM_Treatment_Sidebar_Yes",
                background = "#599740",
                icon = icon("rectangle-list"),
                selectizeInput(
                    ns("Tab5_KM_Treatment_Survival_Time"),
                    "Survival Time:",
                    choices = ""
                ),
                selectizeInput(
                    ns("Tab5_KM_Treatment_Event_Status"),
                    "Event Status:",
                    choices = ""
                ),
                selectizeInput(
                    ns("Tab5_KM_Treatment_Select_Variable"),
                    "Select Variable:",
                    choices = ""
                ),
                selectizeInput(
                    ns("Tab5_KM_Treatment_Variable"),
                    "Treatment Variable:",
                    choices = ""
                ),
                tags$hr(),
                prettyToggle(
                    inputId = ns("Tab5_KM_Treatment_Yes_Display_CI"),
                    label_on = "Display CI",
                    label_off = "Display CI",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab5_KM_Treatment_Yes_Display_Risk_Table"),
                    label_on = "Display Risk Table",
                    label_off = "Display Risk Table",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab5_KM_Treatment_Yes_Display_Pval"),
                    label_on = "Display P-value",
                    label_off = "Display P-value",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                selectInput(
                    ns("Tab5_KM_Treatment_Yes_Legend_Position"),
                    "Legend Position:",
                    choices = c("top", "bottom", "left", "right", "none"),
                    selected = "right"
                ),
                tags$hr(),
                textInput(
                    ns("Tab5_KM_Treatment_Yes_Title"),
                    "Plot Title:",
                    "Breast cancer patients in METABRIC data",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab5_KM_Treatment_Yes_X_Axis_Title"),
                    "X-axis Title:",
                    "Survival Time",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab5_KM_Treatment_Yes_Y_Axis_Title"),
                    "Y-axis Title:",
                    "Survival Probability",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab5_KM_Treatment_Yes_Legend_Title"),
                    "Legend Title:",
                    "Legend",
                    placeholder = TRUE
                ),
                tags$hr(),
                numericInput(
                    ns("Tab5_KM_Treatment_Yes_Width"),
                    "Plot Width (inches):",
                    value = 8,
                    min = 1,
                    max = 50
                ),
                numericInput(
                    ns("Tab5_KM_Treatment_Yes_Height"),
                    "Plot Height (inches):",
                    value = 5,
                    min = 1,
                    max = 50
                ),
                tags$hr(),
                downloadButton(
                    ns("Tab5_Download_KM_Treatment_Yes_PNG"),
                    "Download Plot (PNG)",
                    style = "width:100%;"
                ),
                br(),
                br(),
                downloadButton(
                    ns("Tab5_Download_KM_Treatment_Yes_SVG"),
                    "Download Plot (SVG)",
                    style = "width:100%;"
                )
            )
        ),
        box(
            title = "Kaplan-Meier Plot for Treatment - No",
            style = "height:500px",
            height = "500px",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            status = "primary",
            withSpinner(plotOutput(ns("Plot2"), height = "465px")),
            sidebar = boxSidebar(
                width = 25,
                id = "Tab5_KM_Treatment_Sidebar_No",
                background = "#599740",
                icon = icon("rectangle-list"),
                prettyToggle(
                    inputId = ns("Tab5_KM_Treatment_No_Display_CI"),
                    label_on = "Display CI",
                    label_off = "Display CI",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab5_KM_Treatment_No_Display_Risk_Table"),
                    label_on = "Display Risk Table",
                    label_off = "Display Risk Table",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab5_KM_Treatment_No_Display_Pval"),
                    label_on = "Display P-value",
                    label_off = "Display P-value",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                selectInput(
                    ns("Tab5_KM_Treatment_No_Legend_Position"),
                    "Legend Position:",
                    choices = c("top", "bottom", "left", "right", "none"),
                    selected = "right"
                ),
                tags$hr(),
                textInput(
                    ns("Tab5_KM_Treatment_No_Title"),
                    "Plot Title:",
                    "Breast cancer patients in METABRIC data",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab5_KM_Treatment_No_X_Axis_Title"),
                    "X-axis Title:",
                    "Survival Time",
                    placeholder = TRUE
                ),
                textInput(
                    "Tab5_KM_Treatment_No_Y_Axis_Title",
                    "Y-axis Title:",
                    "Survival Probability",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab5_KM_Treatment_No_Legend_Title"),
                    "Legend Title:",
                    "Legend",
                    placeholder = TRUE
                ),
                tags$hr(),
                numericInput(
                    ns("Tab5_KM_Treatment_No_Width"),
                    "Plot Width (inches):",
                    value = 8,
                    min = 1,
                    max = 50
                ),
                numericInput(
                    ns("Tab5_KM_Treatment_No_Height"),
                    "Plot Height (inches):",
                    value = 5,
                    min = 1,
                    max = 50
                ),
                tags$hr(),
                downloadButton(
                    ns("Tab5_Download_KM_Treatment_No_PNG"),
                    "Download Plot (PNG)",
                    style = "width:100%;"
                ),
                br(),
                br(),
                downloadButton(
                    ns("Tab5_Download_KM_Treatment_No_SVG"),
                    "Download Plot (SVG)",
                    style = "width:100%;"
                )
            )
        ),
        box(
            title = "Logrank Test for Treatment - Yes",
            solidHeader = TRUE,
            width = 6,
            style = "overflow-y: hidden",
            status = "primary",
            collapsible = TRUE,
            withSpinner(verbatimTextOutput(ns("KMlogrankYes")))
        ),
        box(
            title = "Logrank Test for Treatment - No",
            solidHeader = TRUE,
            width = 6,
            style = "overflow-y: hidden",
            status = "primary",
            collapsible = TRUE,
            withSpinner(verbatimTextOutput(ns("KMlogrankNo")))
        )
    )
}
