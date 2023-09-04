# UI function to convert variables i.e. numeric to factor etc.
Tab3_Factor_Levels_UI <- function(id) {
    ns <- NS(id)
    fluidRow(
        box(
            collapsible = TRUE,
            height = "500px",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            title = ("Clinical Variable Types"),
            verbatimTextOutput(ns("DataTypes")),
            sidebar = boxSidebar(
                id = "Tab3_Convert_Type_Sidebar",
                width = 25,
                background = "#599740",
                icon = icon("rectangle-list"),
                h4(strong("Convert to Numeric")),
                selectInput(
                    inputId = ns("Tab3_Variables_to_Numeric"),
                    label = "Select Variables:",
                    choices = "",
                    multiple = TRUE,
                    width = "97%"
                ),
                h4(strong("Convert to Factor")),
                selectInput(
                    inputId = ns("Tab3_Variables_to_Factor"),
                    label = "Select Variables:",
                    choices = "",
                    multiple = TRUE,
                    width = "97%"
                )
            )
        ),
        box(
            collapsible = TRUE,
            style = "height:300px; overflow-y: scroll;overflow-x: hidden",
            solidHeader = TRUE,
            width = 12,
            status = "primary",
            title = ("Clinical Variable Levels"),
            shinycssloaders::withSpinner(verbatimTextOutput(ns("DataLevels")),
                proxy.height = "280px"
            )
        )
    )
}

# UI function to subset Data (can do based on 3 factor variables)
Tab3_Subset_UI <- function(id) {
    ns <- NS(id)
    fluidRow(
        box(
            collapsible = TRUE,
            title = "Filter and Preview Clinical Data",
            height = "500px",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            shinycssloaders::withSpinner(DT::dataTableOutput(ns("TableRecode1")),
                proxy.height = "460px"
            ),
            sidebar = boxSidebar(
                id = "Tab3_Subset_Sidebar",
                width = 25,
                background = "#599740",
                icon = icon("rectangle-list"),
                h4(strong("Options")),
                selectInput(
                    ns("Tab3_Subset_Variable_1"),
                    "1) Filter Based on Variable:",
                    choices = "PATIENT_ID"
                ),
                selectInput(
                    ns("Tab3_Subset_Variable_Levels_1"),
                    label = "Choose Variable Level:",
                    choices = "None Selected",
                    selected = "None Selected",
                    multiple = TRUE
                ),
                selectInput(
                    ns("Tab3_Subset_Variable_2"),
                    "2) Filter Based on Variable:",
                    choices = "PATIENT_ID"
                ),
                selectInput(
                    ns("Tab3_Subset_Variable_Levels_2"),
                    label = "Choose Variable Level:",
                    choices = "None Selected",
                    selected = "None Selected",
                    multiple = TRUE
                ),
                selectInput(
                    ns("Tab3_Subset_Variable_3"),
                    "3) Filter Based on Variable:",
                    choices = "PATIENT_ID"
                ),
                selectInput(
                    ns("Tab3_Subset_Variable_Levels_3"),
                    label = "Choose Variable Level:",
                    choices = "None Selected",
                    selected = "None Selected",
                    multiple = TRUE
                )
            )
        ),
        box(
            collapsible = TRUE,
            solidHeader = TRUE,
            height = "300px",
            title = "Check Selected Variable Levels",
            width = 12,
            status = "primary",
            style = "height:300px; overflow-y: scroll;",
            shinycssloaders::withSpinner(verbatimTextOutput(ns("TableLevels")),
                proxy.height = "180px"
            )
        )
    )
}

# UI function for survival recoding
Tab3_Recode_UI <- function(id) {
    ns <- NS(id)
    fluidRow(
        box(
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 3,
            status = "primary",
            title = ("Select Survival Columns"),
            awesomeRadio(
                inputId = ns("Tab3_Recode_Survival_Yes_or_No"),
                label = "Recode Survival Columns:",
                choices = c("No", "Yes"),
                selected = "No",
                inline = FALSE,
                status = "primary"
            ),
            tags$hr(),
            selectInput(ns("Tab3_Select_OS_Column"),
                "Select OS Column:",
                choices = ""
            ),
            selectInput(
                ns("Tab3_Select_DSS_Column"),
                "Select DSS Column:",
                choices = ""
            ),
            tags$hr(),
            textInput(ns("Tab3_OS_Event"), "OS Event:", "DECEASED"),
            verbatimTextOutput("value"),
            textInput(ns("Tab3_DSS_Event"), "DSS Event:", "Died of Disease"),
            verbatimTextOutput("Died of Disease")
        ),
        box(
            collapsible = TRUE,
            solidHeader = TRUE,
            title = "Check Survival Recoding",
            width = 9,
            status = "primary",
            style = "height:500px",
            shinycssloaders::withSpinner(dataTableOutput(ns("TableRecode")),
                proxy.height = "480px"
            )
        )
    )
}


# UI function for CNA calculations
Tab3_CNACalc_UI <- function(id) {
    ns <- NS(id)
    fluidRow(
        box(
            style = "height:560px",
            sidebar = boxSidebar(
                id = "Tab3_CNA_Calculation_Sidebar",
                width = 25,
                background = "#599740",
                icon = icon("rectangle-list"),
                h4(strong("Options")),
                numericInput(
                    inputId = ns("Tab3_CNA_Start_Column"),
                    label = "Indicate CNA Start Column:",
                    value = 3,
                    width = "95%",
                    min = 1
                ),
                tags$hr(width = "93%"),
                awesomeRadio(
                    inputId = ns("Tab3_CNA_of_Interest"),
                    label = "CNA of Interest:",
                    choices = c("None", "Single Gene", "CNA Score"),
                    selected = "None",
                    inline = FALSE,
                    status = "primary"
                ),
                tags$hr(width = "93%"),
                awesomeRadio(
                    inputId = ns("Tab3_Merge_Column"),
                    label = "Merge on:",
                    choices = c("PATIENT_ID", "SAMPLE_ID"),
                    selected = "PATIENT_ID",
                    inline = TRUE,
                    status = "primary"
                ),
                tags$hr(width = "93%"),
                conditionalPanel(
                    condition = "input.Tab3_CNA_of_Interest == 'None'",
                    ns = ns, h5(strong("Options:"))
                ),
                conditionalPanel(
                    condition = "input.Tab3_CNA_of_Interest== 'Single Gene'",
                    ns = ns,
                    h5(strong("Options:")),
                    textInput(
                        ns("Tab3_Select_Genes"),
                        "Select Gene of Interest:",
                        "TP53, PTEN, BRCA1, A1BG",
                        placeholder = TRUE,
                        width = "95%"
                    )
                ),
                conditionalPanel(
                    condition = "input.Tab3_CNA_of_Interest == 'CNA Score'",
                    ns = ns,
                    h5(strong("Options:")),
                    prettyToggle(
                        inputId = ns("Tab3_CNA_Remove_NAs_Yes_or_No"),
                        label_on = "Remove NAs",
                        label_off = "Remove NAs",
                        icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                    ),
                    prettyToggle(
                        inputId = ns("Tab3_Segment_CNA_Yes_or_No"),
                        label_on = "Segment Data",
                        label_off = "Segment Data",
                        icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                    ),
                    numericInput(
                        inputId = ns("Tab3_Number_of_Segments"),
                        label = "Number of Segments:",
                        value = 4,
                        width = "95%"
                    )
                )
            ),
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            status = "primary",
            title = ("CNA Score Exploration"),
            shinycssloaders::withSpinner(dataTableOutput(ns("TableCNACalc")),
                proxy.height = "510px"
            )
        )
    )
}

# UI function for CNA calculation download
Tab3_Data_Down_UI <- function(id) {
    ns <- NS(id)
    fluidRow(
        box(
            style = "height:560px",
            sidebar = boxSidebar(
                id = "Tab3_CNA_Download_Sidebar",
                width = 25,
                background = "#599740",
                icon = icon("rectangle-list"),
                h4(strong("Options")),
                awesomeRadio(
                    inputId = ns("Tab3_Download_File_Separator"),
                    label = "Separator:",
                    c(
                        Comma = ",",
                        Semicolon = ";",
                        Tab = "\t"
                    ),
                    selected = "\t",
                    inline = FALSE,
                    status = "primary"
                ),
                tags$hr(width = "93%"),
                prettyToggle(
                    inputId = ns("Tab3_Download_File_Quote"),
                    label_on = "Include Quotes",
                    label_off = "Include Quotes",
                    icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab3_Download_File_Row_Names"),
                    label_on = "Include Row Names",
                    label_off = "Include Row Names",
                    icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                ),
                tags$hr(width = "93%"),
                downloadButton(ns("Tab3_Download_File"), "Download Data",
                    style = "width:95%;"
                )
            ),
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            status = "primary",
            title = ("Preview and Download Processed Data"),
            shinycssloaders::withSpinner(dataTableOutput(ns("TableData")),
                proxy.height = "510px"
            )
        )
    )
}
