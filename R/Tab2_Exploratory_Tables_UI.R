# UI function to explore clinical data
Tab2_Exploaratory_Tables_Clin_UI <- function(id) {
    ns <- NS(id)
    tabPanel(
        "All Clinical Data",
        fluidRow(
            box(
                title = "Data Table - Clinical Data",
                collapsible = TRUE,
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                shinycssloaders::withSpinner(DT::dataTableOutput(ns("Table")),
                    proxy.height = "560px"
                ),
                style = "height:580px; padding:10px",
                sidebar = boxSidebar(
                    id = "Tab2_Clin_Sidebar",
                    width = 25,
                    h4(strong("Select Columns to Display")),
                    background = "#599740",
                    icon = shiny::icon("rectangle-list"),
                    selectizeInput(
                        ns("Tab2_Column1_Variable"),
                        "Select Column 1:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    ),
                    selectizeInput(
                        ns("Tab2_Column2_Variable"),
                        "Select Column 2:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    ),
                    selectizeInput(
                        ns("Tab2_Column3_Variable"),
                        "Select Column 3:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    ),
                    selectizeInput(
                        ns("Tab2_Column4_Variable"),
                        "Select Column 4:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    ),
                    selectizeInput(
                        ns("Tab2_Column5_Variable"),
                        "Select Column 5:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    )
                )
            )
        )
    )
}

# UI function to explore CNA data
Tab2_Exploaratory_Tables_CNA_UI <- function(id) {
    ns <- NS(id)
    tabPanel(
        "CNA Data",
        fluidRow(
            box(
                title = "Data Table - CNA Data",
                collapsible = TRUE,
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                shinycssloaders::withSpinner(DT::dataTableOutput(ns("Table")),
                    proxy.height = "560px"
                ),
                style = "height:580px; padding:10px",
                sidebar = boxSidebar(
                    id = "Tab2_CNA_Sidebar",
                    width = 25,
                    h4(strong("Select Columns to Display")),
                    background = "#599740",
                    icon = shiny::icon("rectangle-list"),
                    selectizeInput(
                        ns("Tab2_Column1_Variable"),
                        "Select Column 1:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    ),
                    selectizeInput(
                        ns("Tab2_Column2_Variable"),
                        "Select Column 2:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    ),
                    selectizeInput(
                        ns("Tab2_Column3_Variable"),
                        "Select Column 3:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    ),
                    selectizeInput(
                        ns("Tab2_Column4_Variable"),
                        "Select Column 4:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    ),
                    selectizeInput(
                        ns("Tab2_Column5_Variable"),
                        "Select Column 5:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    )
                )
            )
        )
    )
}

# UI function to explore MAF data
Tab2_Exploaratory_Tables_MAF_UI <- function(id) {
    ns <- NS(id)
    tabPanel(
        "Mutation Data",
        fluidRow(
            box(
                title = "Data Table - Mutation Data",
                width = 12,
                collapsible = TRUE,
                status = "primary",
                solidHeader = TRUE,
                shinycssloaders::withSpinner(DT::dataTableOutput(ns("Table")),
                    proxy.height = "560px"
                ),
                style = "height:580px; padding:10px",
                sidebar = boxSidebar(
                    id = "Tab2_MAF_Sidebar",
                    width = 25,
                    h4(strong("Select Columns to Display")),
                    background = "#599740",
                    icon = shiny::icon("rectangle-list"),
                    selectizeInput(
                        ns("Tab2_Column1_Variable"),
                        "Select Column 1:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    ),
                    selectizeInput(
                        ns("Tab2_Column2_Variable"),
                        "Select Column 2:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    ),
                    selectizeInput(
                        ns("Tab2_Column3_Variable"),
                        "Select Column 3:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    ),
                    selectizeInput(
                        ns("Tab2_Column4_Variable"),
                        "Select Column 4:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    ),
                    selectizeInput(
                        ns("Tab2_Column5_Variable"),
                        "Select Column 5:",
                        choices = "None Selected",
                        width = "95%",
                        selected = "None Selected"
                    )
                )
            )
        )
    )
}
