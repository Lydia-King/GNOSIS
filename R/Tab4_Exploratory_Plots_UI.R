# UI function to produce exploratory boxplots
Tab4_Boxplot_UI <- function(id) {
    ns <- NS(id)
    box(
        title = "Boxplot",
        solidHeader = TRUE,
        width = 12,
        height = "650px",
        status = "primary",
        collapsible = TRUE,
        shinycssloaders::withSpinner(plotOutput(ns("Plot"), height = "590px")),
        sidebar = boxSidebar(
            width = 25,
            id = "Tab4_Boxplot_Sidebar",
            background = "#599740",
            icon = shiny::icon("rectangle-list"),
            selectizeInput(
                ns("Tab4_Boxplot_Select_X_Variable"),
                "Select Variable (x):",
                choices = ""
            ),
            selectizeInput(
                ns("Tab4_Boxplot_Select_Y_Variable"),
                "Select Variable (y):",
                choices = ""
            ),
            tags$hr(),
            prettyToggle(
                inputId = ns("Tab4_Boxplot_by_Sample_Size"),
                label_on = "Boxplot by Sample Size",
                label_off = "Boxplot by Sample Size",
                icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
            ),
            prettyToggle(
                inputId = ns("Tab4_Boxplot_Display_NAs"),
                label_on = "Display NA Values",
                label_off = "Display NA Values",
                icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
            ),
            selectInput(
                ns("Tab4_Boxplot_Legend_Position"),
                "Legend Position:",
                choices = c("top", "bottom", "left", "right", "none"),
                selected = "none"
            ),
            tags$hr(),
            textInput(
                ns("Tab4_Boxplot_Title"),
                "Plot Title:",
                "Clinical Variable Boxplot",
                placeholder = TRUE
            ),
            textInput(
                ns("Tab4_Boxplot_X_Axis_Title"),
                "X-axis Title:",
                "Clinical Variable X",
                placeholder = TRUE
            ),
            textInput(
                ns("Tab4_Boxplot_Y_Axis_Title"),
                "Y-axis Title:",
                "Clinical Variable Y",
                placeholder = TRUE
            ),
            textInput(
                ns("Tab4_Boxplot_Legend_Title"),
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
    )
}

# UI function to produce exploratory scatterplots
Tab4_Scatterplot_UI <- function(id) {
    ns <- NS(id)
    box(
        title = ("Scatterplot"),
        collapsible = TRUE,
        solidHeader = TRUE,
        width = 12,
        height = "650px",
        status = "primary",
        shinycssloaders::withSpinner(plotOutput(ns("Plot"), height = "590px")),
        sidebar = boxSidebar(
            width = 25,
            id = "Tab4_Scatterplot_Sidebar",
            background = "#599740",
            icon = shiny::icon("rectangle-list"),
            selectizeInput(
                ns("Tab4_Scatterplot_Select_X_Variable"),
                "Select Variable (x):",
                choices = ""
            ),
            selectizeInput(
                ns("Tab4_Scatterplot_Select_Y_Variable"),
                "Select Variable (y):",
                choices = ""
            ),
            selectizeInput(
                ns("Tab4_Scatterplot_Select_Colour_Var"),
                "Select Variable (colour):",
                choices = ""
            ),
            tags$hr(),
            prettyToggle(
                inputId = ns("Tab4_Scatterplot_Display_NAs"),
                label_on = "Display NA Values",
                label_off = "Display NA Values",
                icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
            ),
            selectInput(
                ns("Tab4_Scatterplot_Legend_Position"),
                "Legend Position:",
                choices = c("top", "bottom", "left", "right", "none"),
                selected = "none"
            ),
            tags$hr(),
            textInput(
                ns("Tab4_Scatterplot_Title"),
                "Plot Title:",
                "Scatterplot of Clinical Variables and Scores",
                placeholder = TRUE
            ),
            textInput(
                ns("Tab4_Scatterplot_X_Axis_Title"),
                "X-axis Title:",
                "Clinical Variable X",
                placeholder = TRUE
            ),
            textInput(
                ns("Tab4_Scatterplot_Y_Axis_Title"),
                "Y-axis Title:",
                "Clinical Variable Y",
                placeholder = TRUE
            ),
            textInput(
                ns("Tab4_Scatterplot_Legend_Title"),
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
    )
}

# UI function to produce exploratory barplots
Tab4_Barplot_UI <- function(id) {
    ns <- NS(id)
    box(
        title = ("Barplot"),
        solidHeader = TRUE,
        width = 12,
        height = "650px",
        status = "primary",
        collapsible = TRUE,
        shinycssloaders::withSpinner(plotOutput(ns("Plot"), height = "590px")),
        sidebar = boxSidebar(
            width = 25,
            id = "Tab4_Barplot_Sidebar",
            background = "#599740",
            icon = shiny::icon("rectangle-list"),
            selectizeInput(
                ns("Tab4_Barplot_Select_X_Variable"),
                "Select Variable (x):",
                choices = ""
            ),
            selectizeInput(
                ns("Tab4_Barplot_Select_Y_Variable"),
                "Select Variable (y):",
                choices = ""
            ),
            tags$hr(),
            prettyToggle(
                inputId = ns("Tab4_Barplot_Display_NAs"),
                label_on = "Display NA Values (x)",
                label_off = "Display NA Values (x)",
                icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
            ),
            selectInput(
                ns("Tab4_Barplot_Legend_Position"),
                "Legend Position:",
                choices = c("top", "bottom", "left", "right", "none"),
                selected = "none"
            ),
            tags$hr(),
            textInput(
                ns("Tab4_Barplot_Title"),
                "Plot Title:",
                "Barplot of Clinical Variables and Scores",
                placeholder = TRUE
            ),
            textInput(
                ns("Tab4_Barplot_X_Axis_Title"),
                "X-axis Title:",
                "Clinical Variable X",
                placeholder = TRUE
            ),
            textInput(
                ns("Tab4_Barplot_Y_Axis_Title"),
                "Y-axis Title:",
                "Count",
                placeholder = TRUE
            ),
            textInput(
                ns("Tab4_Barplot_Legend_Title"),
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
    )
}

# UI function to produce exploratory histograms/density plots (plain, segmented and faceted plots)
Tab4_Density_UI <- function(id) {
    ns <- NS(id)
    box(
        title = "Histogram and Density Plots",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        height = "650px",
        collapsible = TRUE,
        sidebar = boxSidebar(
            width = 25,
            id = "Tab4_Density_Histogram_Sidebar",
            background = "#599740",
            icon = shiny::icon("rectangle-list"),
            selectInput(
                ns("Tab4_Select_Plot_Variable"),
                "Select Continuous Variable:",
                choices = ""
            ),
            awesomeRadio(
                inputId = ns("Tab4_Select_Plot_Type"),
                label = "Plot Type:",
                choices = c("Histogram", "Density Plot", "Both"),
                selected = "Histogram",
                inline = FALSE,
                status = "primary"
            ),
            conditionalPanel(
                condition = "input.Tab4_Select_Plot_Type == 'Histogram'",
                ns = ns,
                h5(strong("Options:")),
                selectizeInput(
                    ns("Tab4_Select_Histogram_Type"),
                    "Select Plot Type:",
                    c("Plain", "Facet Wrap")
                ),
                conditionalPanel(
                    condition = "input.Tab4_Select_Histogram_Type == 'Plain'",
                    ns = ns,
                    tags$hr(),
                    numericInput(
                        inputId = ns("Tab4_Histogram_Number_of_Bins"),
                        label = "Bin Width:",
                        value = 10,
                        max = 3000,
                        min = 1
                    ),
                    sliderInput(
                        inputId = ns("Tab4_Histogram_Alpha"),
                        label = "alpha:",
                        min = 0,
                        max = 1,
                        value = 0.4
                    ),
                    tags$hr(),
                    selectizeInput(
                        ns("Tab4_Histogram_Select_Fill_Variable"),
                        "Select Fill Variable:",
                        choices = ""
                    ),
                    prettyToggle(
                        inputId = ns("Tab4_Histogram_Display_NAs"),
                        label_on = "Display NA Values",
                        label_off = "Display NA Values",
                        icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                    ),
                    selectInput(
                        ns("Tab4_Histogram_Legend_Position"),
                        "Legend Position:",
                        choices = c("top", "bottom", "left", "right", "none"),
                        selected = "none"
                    ),
                    tags$hr(),
                    textInput(
                        ns("Tab4_Histogram_Title"),
                        "Plot Title:",
                        "Histogram of Selected Continuous Variables",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Histogram_X_Axis_Title"),
                        "X-axis Title:",
                        "Clinical Variable X",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Histogram_Y_Axis_Title"),
                        "Y-axis Title:",
                        "Frequency",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Histogram_Legend_Title"),
                        "Legend Title:",
                        "Legend",
                        placeholder = TRUE
                    ),
                    tags$hr(),
                    numericInput(
                        ns("Tab4_Histogram_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab4_Histogram_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab4_Download_Histogram_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab4_Download_Histogram_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                ),
                conditionalPanel(
                    condition = "input.Tab4_Select_Histogram_Type == 'Facet Wrap'",
                    ns = ns,
                    selectizeInput(
                        ns("Tab4_Histogram_Select_Facet_Variable"),
                        "Select Facet Variable:",
                        choices = ""
                    ),
                    numericInput(
                        inputId = ns("Tab4_Faceted_Histogram_Number_of_Bins"),
                        label = "Bin Width:",
                        value = 10,
                        max = 3000,
                        min = 1
                    ),
                    tags$hr(),
                    sliderInput(
                        inputId = ns("Tab4_Faceted_Histogram_Alpha"),
                        label = "alpha:",
                        min = 0,
                        max = 1,
                        value = 0.4
                    ),
                    numericInput(
                        ns("Tab4_Faceted_Histogram_Number_of_Columns"),
                        label = "Number of Columns:",
                        min = 1,
                        max = 10,
                        value = 2
                    ),
                    tags$hr(),
                    prettyToggle(
                        inputId = ns("Tab4_Faceted_Histogram_Display_NAs"),
                        label_on = "Display NA Values",
                        label_off = "Display NA Values",
                        icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                    ),
                    selectInput(
                        ns("Tab4_Faceted_Histogram_Legend_Position"),
                        "Legend Position:",
                        choices = c("top", "bottom", "left", "right", "none"),
                        selected = "none"
                    ),
                    tags$hr(),
                    textInput(
                        ns("Tab4_Faceted_Histogram_Title"),
                        "Plot Title:",
                        "Histogram by Variable",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Faceted_Histogram_X_Axis_Title"),
                        "X-axis Title:",
                        "Clinical Variable X",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Faceted_Histogram_Y_Axis_Title"),
                        "Y-axis Title:",
                        "Frequency",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Faceted_Histogram_Legend_Title"),
                        "Legend Title:",
                        "Legend",
                        placeholder = TRUE
                    ),
                    tags$hr(),
                    numericInput(
                        ns("Tab4_Faceted_Histogram_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab4_Faceted_Histogram_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab4_Download_Faceted_Histogram_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab4_Download_Faceted_Histogram_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                )
            ),
            conditionalPanel(
                condition = "input.Tab4_Select_Plot_Type == 'Density Plot'",
                ns = ns,
                h5(strong("Options:")),
                selectInput(
                    ns("Tab4_Select_Density_Plot_Type"),
                    "Select Plot Type:",
                    c("Plain", "Segmented", "Facet Wrap")
                ),
                conditionalPanel(
                    condition = "input.Tab4_Select_Density_Plot_Type == 'Plain'",
                    ns = ns,
                    tags$hr(),
                    sliderInput(
                        inputId = ns("Tab4_Density_Plot_Alpha"),
                        label = "alpha:",
                        min = 0,
                        max = 1,
                        value = 0.4
                    ),
                    tags$hr(),
                    selectizeInput(
                        ns("Tab4_Density_Plot_Select_Fill_Variable"),
                        "Select Fill Variable:",
                        choices = ""
                    ),
                    prettyToggle(
                        inputId = ns("Tab4_Density_Plot_Display_NAs"),
                        label_on = "Display NA Values",
                        label_off = "Display NA Values",
                        icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                    ),
                    selectInput(
                        ns("Tab4_Density_Plot_Legend_Position"),
                        "Legend Position:",
                        choices = c("top", "bottom", "left", "right", "none"),
                        selected = "none"
                    ),
                    tags$hr(),
                    textInput(
                        ns("Tab4_Density_Plot_Title"),
                        "Plot Title:",
                        "Density Plots of Selected Continuous Variables",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Density_Plot_X_Axis_Title"),
                        "X-axis Title:",
                        "Clinical Variable X",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Density_Plot_Y_Axis_Title"),
                        "Y-axis Title:",
                        "Density",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Density_Plot_Legend_Title"),
                        "Legend Title:",
                        "Legend",
                        placeholder = TRUE
                    ),
                    tags$hr(),
                    numericInput(
                        ns("Tab4_Density_Plot_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab4_Density_Plot_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab4_Download_Density_Plot_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab4_Download_Density_Plot_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                ),
                conditionalPanel(
                    condition = "input.Tab4_Select_Density_Plot_Type == 'Segmented'",
                    ns = ns,
                    numericInput(
                        inputId = ns("Tab4_Segmented_Density_Plot_Number_of_Segments"),
                        label = "Number of Segments:",
                        value = 4
                    ),
                    tags$hr(),
                    prettyToggle(
                        inputId = ns("Tab4_Segmented_Density_Plot_Display_Legend"),
                        label_on = "Display Legend",
                        label_off = "Display Legend",
                        icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                    ),
                    tags$hr(),
                    textInput(
                        ns("Tab4_Segmented_Density_Plot_Title"),
                        "Plot Title:",
                        "Segmented Density Plots of Selected Continuous Variables",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Segmented_Density_Plot_X_Axis_Title"),
                        "X-axis Title:",
                        "Clinical Variable X",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Segmented_Density_Plot_Y_Axis_Title"),
                        "Y-axis Title:",
                        "Density",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Segmented_Density_Plot_Legend_Title"),
                        "Legend Title:",
                        "Legend",
                        placeholder = TRUE
                    ),
                    tags$hr(),
                    numericInput(
                        ns("Tab4_Segmented_Density_Plot_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab4_Segmented_Density_Plot_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab4_Download_Segmented_Density_Plot_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab4_Download_Segmented_Density_Plot_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                ),
                conditionalPanel(
                    condition = "input.Tab4_Select_Density_Plot_Type == 'Facet Wrap'",
                    ns = ns,
                    selectizeInput(
                        ns("Tab4_Density_Plot_Select_Facet_Variable"),
                        "Select Facet Variable:",
                        choices = ""
                    ),
                    sliderInput(
                        inputId = ns("Tab4_Faceted_Density_Plot_Alpha"),
                        label = "alpha:",
                        min = 0,
                        max = 1,
                        value = 0.4
                    ),
                    numericInput(
                        ns("Tab4_Faceted_Density_Plot_Number_of_Columns"),
                        label = "Number of Columns:",
                        min = 1,
                        max = 10,
                        value = 2
                    ),
                    tags$hr(),
                    prettyToggle(
                        inputId = ns("Tab4_Faceted_Density_Plot_Display_NAs"),
                        label_on = "Display NA Values",
                        label_off = "Display NA Values",
                        icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                    ),
                    selectInput(
                        ns("Tab4_Faceted_Density_Plot_Legend_Position"),
                        "Legend Position:",
                        choices = c("top", "bottom", "left", "right", "none"),
                        selected = "none"
                    ),
                    tags$hr(),
                    textInput(
                        ns("Tab4_Faceted_Density_Plot_Title"),
                        "Plot Title:",
                        "Exploration of Density Plots by Variable",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Faceted_Density_Plot_X_Axis_Title"),
                        "X-axis Title:",
                        "Clinical Variable X",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Faceted_Density_Plot_Y_Axis_Title"),
                        "Y-axis Title:",
                        "Density",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Faceted_Density_Plot_Legend_Title"),
                        "Legend Title:",
                        "Legend",
                        placeholder = TRUE
                    ),
                    tags$hr(),
                    numericInput(
                        ns("Tab4_Faceted_Density_Plot_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab4_Faceted_Density_Plot_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab4_Download_Faceted_Density_Plot_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab4_Download_Faceted_Density_Plot_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                )
            ),
            conditionalPanel(
                condition = "input.Tab4_Select_Plot_Type == 'Both'",
                ns = ns,
                h5(strong("Options:")),
                selectInput(
                    ns("Tab4_Select_Both_Plot_Type"),
                    "Select Plot Type:",
                    c("Plain", "Facet Wrap")
                ),
                conditionalPanel(
                    condition = "input.Tab4_Select_Both_Plot_Type == 'Plain'",
                    ns = ns,
                    tags$hr(),
                    sliderInput(
                        inputId = ns("Tab4_Both_Density_Plot_Alpha"),
                        label = "alpha (density):",
                        min = 0,
                        max = 1,
                        value = 0.4
                    ),
                    sliderInput(
                        inputId = ns("Tab4_Both_Histogram_Alpha"),
                        label = "alpha (histogram):",
                        min = 0,
                        max = 1,
                        value = 0.4
                    ),
                    tags$hr(),
                    selectizeInput(
                        ns("Tab4_Both_Plot_Select_Fill_Variable"),
                        "Select Fill Variable:",
                        choices = ""
                    ),
                    prettyToggle(
                        inputId = ns("Tab4_Both_Plot_Display_NAs"),
                        label_on = "Display NA Values",
                        label_off = "Display NA Values",
                        icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                    ),
                    selectInput(
                        ns("Tab4_Both_Plot_Legend_Position"),
                        "Legend Position:",
                        choices = c("top", "bottom", "left", "right", "none"),
                        selected = "none"
                    ),
                    tags$hr(),
                    textInput(
                        ns("Tab4_Both_Plot_Title"),
                        "Plot Title:",
                        "Histogram and Density Plots of Selected Continuous Variables",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Both_Plot_X_Axis_Title"),
                        "X-axis Title:",
                        "Clinical Variable X",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Both_Plot_Y_Axis_Title"),
                        "Y-axis Title:",
                        "Density",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Both_Plot_Legend_Title"),
                        "Legend Title:",
                        "Legend",
                        placeholder = TRUE
                    ),
                    tags$hr(),
                    numericInput(
                        ns("Tab4_Both_Plot_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab4_Both_Plot_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab4_Download_Both_Plot_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab4_Download_Both_Plot_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                ),
                conditionalPanel(
                    condition = "input.Tab4_Select_Both_Plot_Type == 'Facet Wrap'",
                    ns = ns,
                    selectizeInput(
                        ns("Tab4_Both_Plot_Select_Facet_Variable"),
                        "Select Facet Variable:",
                        choices = ""
                    ),
                    sliderInput(
                        inputId = ns("Tab4_Faceted_Both_Density_Plot_Alpha"),
                        label = "alpha (density):",
                        min = 0,
                        max = 1,
                        value = 0.4
                    ),
                    sliderInput(
                        inputId = ns("Tab4_Faceted_Both_Histogram_Plot_Alpha"),
                        label = "alpha (histogram):",
                        min = 0,
                        max = 1,
                        value = 0.4
                    ),
                    numericInput(
                        ns("Tab4_Faceted_Both_Plot_Number_of_Columns"),
                        label = "Number of Columns:",
                        min = 1,
                        max = 20,
                        value = 2
                    ),
                    tags$hr(),
                    prettyToggle(
                        inputId = ns("Tab4_Faceted_Both_Plot_Display_NAs"),
                        label_on = "Display NA Values",
                        label_off = "Display NA Values",
                        icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                    ),
                    selectInput(
                        ns("Tab4_Faceted_Both_Plot_Legend_Position"),
                        "Legend Position:",
                        choices = c("top", "bottom", "left", "right", "none"),
                        selected = "none"
                    ),
                    tags$hr(),
                    textInput(
                        ns("Tab4_Faceted_Both_Plot_Title"),
                        "Plot Title:",
                        "Histogram and Density Plot by Variable",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Faceted_Both_Plot_X_Axis_Title"),
                        "X-axis Title:",
                        "Clinical Variable X",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Faceted_Both_Plot_Y_Axis_Title"),
                        "Y-axis Title:",
                        "Density",
                        placeholder = TRUE
                    ),
                    textInput(
                        ns("Tab4_Faceted_Both_Plot_Legend_Title"),
                        "Legend Title:",
                        "Legend",
                        placeholder = TRUE
                    ),
                    tags$hr(),
                    numericInput(
                        ns("Tab4_Faceted_Both_Plot_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab4_Faceted_Both_Plot_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab4_Download_Faceted_Both_Plot_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab4_Download_Faceted_Both_Plot_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                )
            )
        ),
        conditionalPanel(
            condition = "input.Tab4_Select_Plot_Type == 'Histogram'",
            ns = ns,
            width = 12,
            conditionalPanel(
                condition = "input.Tab4_Select_Histogram_Type == 'Plain'",
                ns = ns,
                width = 12,
                shinycssloaders::withSpinner(plotOutput(ns("CNAHist"), height = "590px"))
            ),
            conditionalPanel(
                condition = "input.Tab4_Select_Histogram_Type == 'Facet Wrap'",
                ns = ns,
                width = 12,
                shinycssloaders::withSpinner(plotOutput(ns("CNAHist1"), height = "590px"))
            )
        ),
        conditionalPanel(
            condition = "input.Tab4_Select_Plot_Type == 'Density Plot'",
            ns = ns,
            width = 12,
            conditionalPanel(
                condition = "input.Tab4_Select_Density_Plot_Type == 'Plain'",
                ns = ns,
                width = 12,
                shinycssloaders::withSpinner(plotOutput(ns("CNADist"), height = "590px"))
            ),
            conditionalPanel(
                condition = "input.Tab4_Select_Density_Plot_Type == 'Segmented'",
                ns = ns,
                width = 12,
                shinycssloaders::withSpinner(plotOutput(ns("CNADist1"), height = "590px"))
            ),
            conditionalPanel(
                condition = "input.Tab4_Select_Density_Plot_Type == 'Facet Wrap'",
                ns = ns,
                width = 12,
                shinycssloaders::withSpinner(plotOutput(ns("CNADist2"), height = "590px"))
            )
        ),
        conditionalPanel(
            condition = "input.Tab4_Select_Plot_Type == 'Both'",
            ns = ns,
            width = 12,
            conditionalPanel(
                condition = "input.Tab4_Select_Both_Plot_Type == 'Plain'",
                ns = ns,
                width = 12,
                shinycssloaders::withSpinner(plotOutput(ns("CNABoth"), height = "590px"))
            ),
            conditionalPanel(
                condition = "input.Tab4_Select_Both_Plot_Type == 'Facet Wrap'",
                ns = ns,
                width = 12,
                shinycssloaders::withSpinner(plotOutput(ns("CNABoth1"), height = "590px"))
            )
        )
    )
}
