# UI function to produce adjusted survival curves
Tab8_Adjusted_Survival_Curves_UI <- function(id) {
    ns <- NS(id)

    fluidRow(
        box(
            collapsible = TRUE,
            title = ("Multivariable Cox Model"),
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            shinycssloaders::withSpinner(verbatimTextOutput(ns("Interaction")))
        ),
        box(
            title = ("New Data for Adjusted Survival Curves"),
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            shinycssloaders::withSpinner(dataTableOutput(ns("preddata"))),
            dropdownMenu = boxDropdown(
                icon = fontawesome::fa_i(name = "info-circle", verify_fa = FALSE),
                boxDropdownItem(HTML(
                    paste(
                        "Please make sure you have included all variables present",
                        "in the Cox Multivariable PH model in the New Dataframe"
                    )
                ))
            ),
            sidebar = boxSidebar(
                id = "Tab8_New_Data_Sidebar",
                width = 25,
                background = "#599740",
                icon = shiny::icon("rectangle-list"),
                selectizeInput(
                    ns("Tab8_Adjusted_Curves_Select_Variable_1"),
                    "Select Model Variable 1:",
                    choices = "",
                    width = "95%"
                ),
                selectizeInput(
                    ns("Tab8_Adjusted_Curves_Select_Variable_2"),
                    "Select Model Variable 2:",
                    choices = "",
                    width = "95%"
                ),
                selectizeInput(
                    inputId = ns("Tab8_Adjusted_Curves_Select_Constant_Variable"),
                    label = "Select Variables to Keep Constant:",
                    choices = "",
                    multiple = TRUE,
                    width = "95%"
                )
            )
        ),
        box(
            title = ("View and Download Adjusted Survival Curves"),
            collapsible = TRUE,
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            shinycssloaders::withSpinner(plotOutput(ns("Pred3"))),
            height = "430px",
            style = "height:430px",
            sidebar = boxSidebar(
                id = "Tab8_Download_Adjusted_Survival_Sidebar",
                width = 25,
                background = "#599740",
                icon = shiny::icon("rectangle-list"),
                selectInput(
                    ns("Tab8_Download_Adjusted_Curves_Select"),
                    "Select Plot:",
                    choices = "",
                    width = "95%"
                ),
                h5(strong("Options:")),
                prettyToggle(
                    inputId = ns("Tab8_Download_Adjusted_Curves_Display_CI"),
                    label_on = "Display CI",
                    label_off = "Display CI",
                    icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab8_Download_Adjusted_Curves_Display_Risk_Table"),
                    label_on = "Display Risk Table",
                    label_off = "Display Risk Table",
                    icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                ),
                selectInput(
                    ns("Tab8_Download_Adjusted_Curves_Legend_Position"),
                    "Legend Position:",
                    choices = c("top", "bottom", "left", "right", "none"),
                    selected = "right"
                ),
                tags$hr(),
                textInput(
                    ns("Tab8_Download_Adjusted_Curves_Plot_Title"),
                    "Plot Title:",
                    "Breast cancer patients in METABRIC data",
                    placeholder = TRUE,
                    width = "95%"
                ),
                textInput(
                    ns("Tab8_Download_Adjusted_Curves_X_Axis_Title"),
                    "X-axis Title:",
                    "Survival Time",
                    placeholder = TRUE,
                    width = "95%"
                ),
                textInput(
                    ns("Tab8_Download_Adjusted_Curves_Y_Axis_Title"),
                    "Y-axis Title:",
                    "Survival Probability",
                    placeholder = TRUE,
                    width = "95%"
                ),
                textInput(
                    ns("Tab8_Download_Adjusted_Curves_Legend_Title"),
                    "Legend Title:",
                    "Legend",
                    placeholder = TRUE,
                    width = "95%"
                ),
                tags$hr(),
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
        )
    )
}
