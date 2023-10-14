# UI function to download log/script
Tab11_Download_Code_UI <- function(id) {
    ns <- NS(id)

    fluidRow(
        box(
            collapsible = TRUE,
            title = "Preview of Input Logs",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            style = "height:800px",
            withSpinner(dataTableOutput(ns("InputLog"))),
            sidebar = boxSidebar(
                id = "Tab11_Log_Options",
                width = 25,
                background = "#599740",
                icon = icon("rectangle-list"),
                h4(strong("Dataframe Options")),
                selectInput(
                    ns("Tab11_Order_Log_By"),
                    "Order by:",
                    choices = c("Timestamp", "Tab", "Name", "Value", "Binding"),
                    selected = c("Timestamp"),
                    multiple = TRUE,
                    width = "450px"
                ),
                prettyToggle(
                    inputId = ns("Tab11_Remove_None_Log"),
                    label_on = "Remove NULL/None Selected",
                    label_off = "Remove NULL/None Selected",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE),
                    value = TRUE
                ),
                awesomeCheckboxGroup(
                    inputId = ns("Tab11_Display_Type"),
                    label = "Display:",
                    choices = c(
                        "Number Input" = "shiny.numberInput",
                        "Select Input" = "shiny.selectInput",
                        "File Input" = "shiny.fileInputBinding",
                        "Slider Input" = "shiny.sliderInput",
                        "Checkbox Input" = "shiny.checkboxInput",
                        "Box Sidebar Input" = "box-sidebar-input",
                        "Text Input" = "shiny.textInput",
                        "Radio Button Input" = "shinyWidgets.awesomeRadio",
                        "Main Sidebar Input" = c(
                            "shinydashboard.sidebarCollapsedInputBinding",
                            "shinydashboard.sidebarmenuExpandedInputBinding"
                        )
                    ),
                    selected = c(
                        "box-sidebar-input",
                        "shiny.selectInput",
                        "shiny.numberInput",
                        "shiny.fileInputBinding",
                        "shiny.sliderInput",
                        "shiny.checkboxInput",
                        "shiny.textInput",
                        "shinyWidgets.awesomeRadio"
                    )
                ),
                tags$hr(),
                awesomeRadio(
                    inputId = ns("Tab11_Download_Log_Separator"),
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
                prettyToggle(
                    inputId = ns("Tab11_Download_Log_Quote"),
                    label_on = "Include Quotes",
                    label_off = "Include Quotes",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab11_Download_Log_Row_Names"),
                    label_on = "Include Row Names",
                    label_off = "Include Row Names",
                    icon_on = fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fa_i(name = "times", verify_fa = FALSE)
                ),
                tags$hr(),
                downloadButton(ns("Tab11_Download_Log"), "Download Input Log",
                    style = "width:98%;"
                )
            )
        )
    )
}
