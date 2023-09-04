# UI function to upload data manually
Tab1_Input_Files_Manual_UI <- function(id) {
    ns <- NS(id)
    tabPanel(
        "Manual Upload",
        fluidRow(
            box(
                title = "Input Patient Data File",
                width = 3,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                fileInput(
                    ns("Input_Patient_File"),
                    "Choose File:",
                    multiple = TRUE,
                    accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv"
                    )
                ),
                tags$hr(),
                prettyToggle(
                    inputId = ns("Tab1_Clin_Header_Yes_or_No"),
                    label_on = "Header",
                    label_off = "Header",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE),
                    value = TRUE,
                    outline = TRUE,
                    shape = c("curve"),
                    status_on = "primary"
                ),
                fluidRow(
                    column(
                        7,
                        awesomeRadio(
                            inputId = ns("Tab1_Clin_Separator"),
                            label = "Separator:",
                            c(
                                Comma = ",",
                                Semicolon = ";",
                                Tab = "\t"
                            ),
                            selected = "\t",
                            inline = FALSE,
                            status = "primary"
                        )
                    ),
                    column(
                        5,
                        awesomeRadio(
                            ns("Tab1_Clin_Quote"),
                            "Quote:",
                            choices = c(
                                None = "",
                                "Double" = '"',
                                "Single" = "'"
                            ),
                            selected = '"',
                            inline = FALSE,
                            status = "primary"
                        )
                    )
                ),
                tags$hr(),
                textInput(
                    inputId = ns("Tab1_Comment_1"),
                    label = "Comment Character:",
                    value = "#"
                ),
                numericInput(
                    inputId = ns("Tab1_Clin_Skip_Lines"),
                    label = "Number of Lines to Skip:",
                    value = 0,
                    min = 0,
                    max = 10
                ),
                tags$hr(),
                h5(strong("Total Number of Clinical Variables:")),
                verbatimTextOutput("TotalC", placeholder = TRUE),
                h5(strong("Total Number of Patients:")),
                tags$style(
                    HTML(
                        "table.dataTable.hover tbody tr:hover,
                table.dataTable.display tbody tr:hover {
                    background-color: rgba(44,222,235) !important;
                }"
                    )
                ),
                verbatimTextOutput("TotalR", placeholder = TRUE)
            ),
            box(
                collapsible = TRUE,
                title = "Input Sample Data File",
                width = 3,
                status = "primary",
                solidHeader = TRUE,
                fileInput(
                    ns("Input_Sample_File"),
                    "Choose File:",
                    multiple = TRUE,
                    accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv"
                    )
                ),
                tags$hr(),
                prettyToggle(
                    inputId = ns("Tab1_Sample_Header_Yes_or_No"),
                    label_on = "Header",
                    label_off = "Header",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE),
                    value = TRUE,
                    outline = TRUE,
                    shape = c("curve"),
                    status_on = "primary"
                ),
                fluidRow(
                    column(
                        7,
                        awesomeRadio(
                            inputId = ns("Tab1_Sample_Separator"),
                            label = "Separator:",
                            c(
                                Comma = ",",
                                Semicolon = ";",
                                Tab = "\t"
                            ),
                            selected = "\t",
                            inline = FALSE,
                            status = "primary"
                        )
                    ),
                    column(
                        5,
                        awesomeRadio(
                            ns("Tab1_Sample_Quote"),
                            "Quote:",
                            choices = c(
                                None = "",
                                "Double" = '"',
                                "Single" = "'"
                            ),
                            selected = '"',
                            inline = FALSE,
                            status = "primary"
                        )
                    )
                ),
                tags$hr(),
                textInput(
                    inputId = ns("Tab1_Comment_2"),
                    label = "Comment Character:",
                    value = "#"
                ),
                numericInput(
                    inputId = ns("Tab1_Sample_Skip_Lines"),
                    label = "Number of Lines to Skip:",
                    value = 0,
                    min = 0,
                    max = 10
                ),
                tags$hr(),
                h5(strong("Total Number of Clinical Variables:")),
                verbatimTextOutput("TotalC1", placeholder = TRUE),
                h5(strong("Total Number of Patients:")),
                verbatimTextOutput("TotalR2", placeholder = TRUE)
            ),
            box(
                title = "Input CNA File",
                status = "primary",
                width = 3,
                solidHeader = TRUE,
                collapsible = TRUE,
                fileInput(
                    ns("Input_CNA_File"),
                    "Choose File:",
                    multiple = TRUE,
                    accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv"
                    )
                ),
                tags$hr(),
                prettyToggle(
                    inputId = ns("Tab1_CNA_Header_Yes_or_No"),
                    label_on = "Header",
                    label_off = "Header",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE),
                    value = TRUE,
                    outline = TRUE,
                    shape = c("curve"),
                    status_on = "primary"
                ),
                fluidRow(
                    column(
                        7,
                        awesomeRadio(
                            inputId = ns("Tab1_CNA_Separator"),
                            label = "Separator:",
                            c(
                                Comma = ",",
                                Semicolon = ";",
                                Tab = "\t"
                            ),
                            selected = "\t",
                            inline = FALSE,
                            status = "primary"
                        )
                    ),
                    column(
                        5,
                        awesomeRadio(
                            ns("Tab1_CNA_Quote"),
                            "Quote:",
                            choices = c(
                                None = "",
                                "Double" = '"',
                                "Single" = "'"
                            ),
                            selected = '"',
                            inline = FALSE,
                            status = "primary"
                        )
                    )
                ),
                tags$hr(),
                textInput(
                    inputId = ns("Tab1_Comment_3"),
                    label = "Comment Character:",
                    value = "#"
                ),
                numericInput(
                    inputId = ns("Tab1_CNA_Skip_Lines"),
                    label = "Number of Lines to Skip:",
                    value = 0,
                    min = 0,
                    max = 10
                ),
                tags$hr(),
                h5(strong("Total Number of Columns:")),
                verbatimTextOutput("TotalCCNA", placeholder = TRUE),
                h5(strong("Total Number of Genes:")),
                verbatimTextOutput("TotalRCNA", placeholder = TRUE)
            ),
            box(
                collapsible = TRUE,
                title = "Input MAF File",
                width = 3,
                status = "primary",
                solidHeader = TRUE,
                fileInput(
                    ns("Input_MAF_File"),
                    "Choose File:",
                    multiple = TRUE,
                    accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv"
                    )
                ),
                tags$hr(),
                prettyToggle(
                    inputId = ns("Tab1_MAF_Header_Yes_or_No"),
                    label_on = "Header",
                    label_off = "Header",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE),
                    value = TRUE,
                    outline = TRUE,
                    shape = c("curve"),
                    status_on = "primary"
                ),
                fluidRow(
                    column(
                        7,
                        awesomeRadio(
                            inputId = ns("Tab1_MAF_Separator"),
                            label = "Separator:",
                            c(
                                Comma = ",",
                                Semicolon = ";",
                                Tab = "\t"
                            ),
                            selected = "\t",
                            inline = FALSE,
                            status = "primary"
                        )
                    ),
                    column(
                        5,
                        awesomeRadio(
                            ns("Tab1_MAF_Quote"),
                            "Quote:",
                            choices = c(
                                None = "",
                                "Double" = '"',
                                "Single" = "'"
                            ),
                            selected = '"',
                            inline = FALSE,
                            status = "primary"
                        )
                    )
                ),
                tags$hr(),
                textInput(
                    inputId = ns("Tab1_Comment_4"),
                    label = "Comment Character:",
                    value = "#"
                ),
                numericInput(
                    inputId = ns("Tab1_MAF_Skip_Lines"),
                    label = "Number of Lines to Skip:",
                    value = 0,
                    min = 0,
                    max = 10
                ),
                tags$hr(),
                h5(strong("Total Number of Columns:")),
                verbatimTextOutput("TotalCMAF", placeholder = TRUE),
                h5(strong("Total Number of Mutations:")),
                verbatimTextOutput("TotalRMAF", placeholder = TRUE)
            )
        )
    )
}

# UI function for API upload
Tab1_Input_Files_API_UI <- function(id) {
    ns <- NS(id)
    tabPanel(
        "API Upload",
        fluidRow(
            box(
                collapsible = TRUE,
                title = "cBioPortal Datasets",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                shinycssloaders::withSpinner(DT::dataTableOutput(ns("cBioData"))),
                style = "height:800px"
            )
        )
    )
}

# UI function to preview clinical data
Tab1_Input_Files_Preview_Clin_UI <-
    function(tab1_input_preview_clin_ui) {
        ns <- NS(tab1_input_preview_clin_ui)
        tabPanel(
            "Clinical Data",
            fluidRow(
                box(
                    collapsible = TRUE,
                    title = "Preview",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    shinyjs::useShinyjs(),
                    textOutput(ns("text")),
                    shinycssloaders::withSpinner(DT::dataTableOutput(ns("Preview"))),
                    style = "height:800px"
                )
            )
        )
    }

# UI function to preview CNA data
Tab1_Input_Files_Preview_CNA_UI <-
    function(tab1_input_preview_CNA_ui) {
        ns <- NS(tab1_input_preview_CNA_ui)
        tabPanel(
            "CNA Data",
            fluidRow(
                box(
                    collapsible = TRUE,
                    title = "Preview",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    shinyjs::useShinyjs(),
                    textOutput(ns("text")),
                    shinycssloaders::withSpinner(DT::dataTableOutput(ns("Preview"))),
                    style = "height:800px"
                )
            )
        )
    }

# UI function to preview mutation data
Tab1_Input_Files_Preview_MAF_UI <-
    function(tab1_input_preview_MAF_ui) {
        ns <- NS(tab1_input_preview_MAF_ui)
        tabPanel(
            "Mutation Data",
            fluidRow(
                box(
                    collapsible = TRUE,
                    title = "Preview",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    shinyjs::useShinyjs(),
                    textOutput(ns("text")),
                    shinycssloaders::withSpinner(DT::dataTableOutput(ns("Preview"))),
                    style = "height:800px"
                )
            )
        )
    }
