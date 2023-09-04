# UI function to produce survival trees (Rpart)
Tab9_Rpart_UI <- function(id) {
    ns <- NS(id)
    fluidRow(
        box(
            collapsible = TRUE,
            width = 12,
            status = "primary",
            height = "600px",
            title = "Rpart Survival Tree",
            solidHeader = TRUE,
            shinycssloaders::withSpinner(plotOutput(ns(
                "RpartTreePlot"
            ), height = "540px")),
            dropdownMenu = boxDropdown(
                icon = fontawesome::fa_i(name = "info-circle", verify_fa = FALSE),
                boxDropdownItem(
                    HTML(
                        paste(
                            "minsplit: the minimum number of observations that must exist in a node...",
                            sep = "<br/>"
                        )
                    ),
                    tags$a(
                        href = "https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf",
                        "An Introduction to Recursive Partitioning Using the Rpart Routines"
                    ),
                    tags$a(href = "https://cran.r-project.org/web/packages/rpart/rpart.pdf", "Rpart Documentation")
                )
            ),
            sidebar = boxSidebar(
                id = "Tab9_Rpart_Tree_Sidebar",
                width = 25,
                background = "#599740",
                icon = shiny::icon("rectangle-list"),
                selectizeInput(
                    ns("Tab9_Rpart_Survival_Time"),
                    "Survival Time:",
                    choices = ""
                ),
                selectizeInput(ns("Tab9_Rpart_Event_Status"), "Event Status:", choices = ""),
                selectizeInput(
                    inputId = ns("Tab9_Rpart_Select_Variables"),
                    label = "Select Variables:",
                    choices = "",
                    multiple = TRUE
                ),
                tags$hr(),
                sliderInput(
                    inputId = ns("Tab9_Rpart_Minsplit"),
                    label = "minsplit:",
                    value = 20,
                    max = 200,
                    min = 0
                ),
                sliderInput(
                    inputId = ns("Tab9_Rpart_Minbucket"),
                    label = "minbucket:",
                    value = 20,
                    min = 0,
                    max = 200
                ),
                sliderInput(
                    inputId = ns("Tab9_Rpart_Cp"),
                    label = "cp:",
                    value = .01,
                    min = 0.0001,
                    max = 0.05
                ),
                sliderInput(
                    inputId = ns("Tab9_Rpart_Xval"),
                    label = "xval:",
                    value = 10,
                    min = 0,
                    max = 20
                ),
                sliderInput(
                    inputId = ns("Tab9_Rpart_Maxdepth"),
                    label = "maxdepth:",
                    value = 25,
                    min = 0,
                    max = 50
                ),
                tags$hr(),
                numericInput(
                    ns("Plot_Width_1"),
                    "Plot Width (inches):",
                    value = 8,
                    min = 1,
                    max = 50
                ),
                numericInput(
                    ns("Plot_Height_1"),
                    "Plot Height (inches):",
                    value = 5,
                    min = 1,
                    max = 50
                ),
                tags$hr(),
                downloadButton(ns("Download_PNG_1"), "Download Plot (PNG)",
                    style = "width:100%;"
                ),
                br(),
                br(),
                downloadButton(ns("Download_SVG_1"), "Download Plot (SVG)",
                    style = "width:100%;"
                )
            )
        ),
        box(
            collapsible = TRUE,
            height = "550px",
            width = 12,
            status = "primary",
            title = "Corresponding Survival Curves",
            solidHeader = TRUE,
            shinycssloaders::withSpinner(plotOutput(ns("Surv_Curve1"), height = "510px")),
            sidebar = boxSidebar(
                id = "Tab9_Rpart_Surv_Sidebar",
                width = 25,
                background = "#599740",
                icon = shiny::icon("rectangle-list"),
                h5(strong("Options:")),
                prettyToggle(
                    inputId = ns("Tab9_Surv_Rpart_Display_CI"),
                    label_on = "Display CI",
                    label_off = "Display CI",
                    icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab9_Surv_Rpart_Display_Risk_Table"),
                    label_on = "Display Risk Table",
                    label_off = "Display Risk Table",
                    icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab9_Surv_Rpart_Display_Pval"),
                    label_on = "Display P-value",
                    label_off = "Display P-value",
                    icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                ),
                tags$hr(),
                selectInput(
                    ns("Tab9_Surv_Rpart_Legend_Position"),
                    "Legend Position:",
                    choices = c("top", "bottom", "left", "right", "none"),
                    selected = "right"
                ),
                tags$hr(),
                textInput(
                    ns("Tab9_Surv_Rpart_Plot_Title"),
                    "Plot Title:",
                    "Breast cancer patients in METABRIC data",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab9_Surv_Rpart_X_Axis_Title"),
                    "X-axis Title:",
                    "Survival Time",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab9_Surv_Rpart_Y_Axis_Title"),
                    "Y-axis Title:",
                    "Survival Probability",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab9_Surv_Rpart_Legend_Title"),
                    "Legend Title:",
                    "Legend",
                    placeholder = TRUE
                ),
                tags$hr(),
                numericInput(
                    ns("Plot_Width_2"),
                    "Plot Width (inches):",
                    value = 8,
                    min = 1,
                    max = 50
                ),
                numericInput(
                    ns("Plot_Height_2"),
                    "Plot Height (inches):",
                    value = 5,
                    min = 1,
                    max = 50
                ),
                tags$hr(),
                downloadButton(ns("Download_PNG_2"), "Download Plot (PNG)",
                    style = "width:100%;"
                ),
                br(),
                br(),
                downloadButton(ns("Download_SVG_2"), "Download Plot (SVG)",
                    style = "width:100%;"
                )
            )
        )
    )
}

# UI function to produce survival trees (Ctree)
Tab9_Ctree_UI <- function(id) {
    ns <- NS(id)

    fluidRow(
        box(
            collapsible = TRUE,
            width = 12,
            status = "primary",
            height = "600px",
            title = "Ctree Survival Tree",
            solidHeader = TRUE,
            shinycssloaders::withSpinner(plotOutput(ns("CTreePlot"), height = "540px")),
            dropdownMenu = boxDropdown(
                icon = fontawesome::fa_i(name = "info-circle", verify_fa = FALSE),
                boxDropdownItem(
                    HTML(
                        paste(
                            "teststat: a character specifying the type of the test statistic to be applied for variable selection.",
                            "splitstat: a character specifying the type of the test statistic to be applied for splitpoint selection.",
                            "testtype: a character specifying how to compute the distribution of the test statistic.",
                            "alpha: a double, the significance level for variable selection.",
                            "mincriterion: the value of the test statistic or 1 - p-value that must be exceeded in order to implement a split.",
                            "minsplit: the minimum sum of weights in a node in order to be considered for splitting.",
                            "minbucket: the minimum sum of weights in a terminal node.",
                            "minprob: proportion of observations needed to establish a terminal node.",
                            "stump: a logical determining whether a stump (a tree with a maximum of three nodes only) is to be computed.",
                            "maxvar: maximum number of variables the tree is allowed to split in.",
                            "maxdepth: maximum depth of the tree. Default maxdepth = Inf means that no restrictions are applied.",
                            "<br/>",
                            "Useful Resources:",
                            sep = "<br/>"
                        )
                    ),
                    tags$a(
                        href = "https://cran.r-project.org/web/packages/partykit/vignettes/ctree.pdf",
                        "ctree: Conditional Inference Trees"
                    ),
                    tags$a(
                        href = "https://www.rdocumentation.org/packages/partykit/versions/1.2-15/topics/ctree",
                        "Ctree Documentation"
                    )
                )
            ),
            sidebar = boxSidebar(
                id = "Tab9_Ctree_Tree_Sidebar",
                width = 25,
                background = "#599740",
                icon = shiny::icon("rectangle-list"),
                selectizeInput(
                    ns("Tab9_Ctree_Survival_Time"),
                    "Survival Time:",
                    choices = ""
                ),
                selectizeInput(ns("Tab9_Ctree_Event_Status"), "Event Status:", choices = ""),
                selectizeInput(
                    inputId = ns("Tab9_Ctree_Select_Variables"),
                    label = "Select Variables:",
                    choices = "",
                    multiple = TRUE
                ),
                tags$hr(),
                prettyToggle(
                    inputId = ns("Tab9_Ctree_Use_Complete_Cases_Only"),
                    label_on = "Complete Cases Only",
                    label_off = "Complete Cases Only",
                    icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                ),
                selectInput(
                    inputId = ns("Tab9_Ctree_Teststat"),
                    label = "teststat:",
                    choices = c("quadratic", "maximum"),
                    selected = "quadratic"
                ),
                selectInput(
                    inputId = ns("Tab9_Ctree_Splitstat"),
                    label = "splitstat:",
                    choices = c("quadratic", "maximum"),
                    selected = "quadratic"
                ),
                selectInput(
                    inputId = ns("Tab9_Ctree_Testtype"),
                    label = "testtype:",
                    choices = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"),
                    selected = "Bonferroni"
                ),
                sliderInput(
                    inputId = ns("Tab9_Ctree_Alpha"),
                    label = "alpha:",
                    value = 0.05,
                    max = 1,
                    min = 0
                ),
                sliderInput(
                    inputId = ns("Tab9_Ctree_Minsplit"),
                    label = "minsplit:",
                    value = 20,
                    min = 0,
                    max = 200
                ),
                sliderInput(
                    inputId = ns("Tab9_Ctree_Minbucket"),
                    label = "minbucket:",
                    value = 20,
                    min = 0,
                    max = 200
                ),
                sliderInput(
                    inputId = ns("Tab9_Ctree_Minprob"),
                    label = "minprob:",
                    value = .01,
                    min = 0,
                    max = 1
                ),
                sliderInput(
                    inputId = ns("Tab9_Ctree_Maxvar"),
                    label = "maxvar:",
                    value = 20,
                    min = 0,
                    max = 100
                ),
                sliderInput(
                    inputId = ns("Tab9_Ctree_Maxdepth"),
                    label = "maxdepth:",
                    value = 20,
                    min = 0,
                    max = 100
                ),
                prettyToggle(
                    inputId = ns("Tab9_Ctree_Stump"),
                    label_on = "Stump",
                    label_off = "Stump",
                    icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                ),
                tags$hr(),
                numericInput(
                    ns("Plot_Width_1"),
                    "Plot Width (inches):",
                    value = 8,
                    min = 1,
                    max = 50
                ),
                numericInput(
                    ns("Plot_Height_1"),
                    "Plot Height (inches):",
                    value = 5,
                    min = 1,
                    max = 50
                ),
                tags$hr(),
                downloadButton(ns("Download_PNG_1"), "Download Plot (PNG)",
                    style = "width:100%;"
                ),
                br(),
                br(),
                downloadButton(ns("Download_SVG_1"), "Download Plot (SVG)",
                    style = "width:100%;"
                )
            )
        ),
        box(
            collapsible = TRUE,
            width = 12,
            height = "550px",
            status = "primary",
            title = "Corresponding Survival Curves",
            solidHeader = TRUE,
            shinycssloaders::withSpinner(plotOutput(ns(
                "Surv_CurveCtree"
            ), height = "510px")),
            sidebar = boxSidebar(
                id = "Tab9_Ctree_Curv_Sidebar",
                width = 25,
                background = "#599740",
                icon = shiny::icon("rectangle-list"),
                h5(strong("Options:")),
                prettyToggle(
                    inputId = ns("Tab9_Surv_Ctree_Display_CI"),
                    label_on = "Display CI",
                    label_off = "Display CI",
                    icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab9_Surv_Ctree_Display_Risk_Table"),
                    label_on = "Display Risk Table",
                    label_off = "Display Risk Table",
                    icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                ),
                prettyToggle(
                    inputId = ns("Tab9_Surv_Ctree_Display_Pval"),
                    label_on = "Display P-value",
                    label_off = "Display P-value",
                    icon_on = fontawesome::fa_i(name = "check", verify_fa = FALSE),
                    icon_off = fontawesome::fa_i(name = "times", verify_fa = FALSE)
                ),
                tags$hr(),
                selectInput(
                    ns("Tab9_Surv_Ctree_Legend_Position"),
                    "Legend Position:",
                    choices = c("top", "bottom", "left", "right", "none"),
                    selected = "right"
                ),
                tags$hr(),
                textInput(
                    ns("Tab9_Surv_Ctree_Plot_Title"),
                    "Plot Title:",
                    "Breast cancer patients in METABRIC data",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab9_Surv_Ctree_X_Axis_Title"),
                    "X-axis Title:",
                    "Survival Time",
                    placeholder = TRUE
                ),
                textInput(
                    "Tab9_Surv_Ctree_Y_Axis_Title",
                    "Y-axis Title:",
                    "Survival Probability",
                    placeholder = TRUE
                ),
                textInput(
                    ns("Tab9_Surv_Ctree_Legend_Title"),
                    "Legend Title:",
                    "Legend",
                    placeholder = TRUE
                ),
                tags$hr(),
                numericInput(
                    ns("Plot_Width_2"),
                    "Plot Width (inches):",
                    value = 8,
                    min = 1,
                    max = 50
                ),
                numericInput(
                    ns("Plot_Height_2"),
                    "Plot Height (inches):",
                    value = 5,
                    min = 1,
                    max = 50
                ),
                tags$hr(),
                downloadButton(ns("Download_PNG_2"), "Download Plot (PNG)",
                    style = "width:100%;"
                ),
                br(),
                br(),
                downloadButton(ns("Download_SVG_2"), "Download Plot (SVG)",
                    style = "width:100%;"
                )
            )
        )
    )
}
