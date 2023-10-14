# UI function to produce MAF text summary
Tab10_MAF_Text_Summary_UI <- function(id) {
    ns <- NS(id)
    tabBox(
        height = "500px",
        width = 12,
        tabPanel(
            "MAF Summary",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            h5(strong(
                "Basic summary of inputted Mutation/MAF file:"
            )),
            withSpinner(verbatimTextOutput(ns("MAF1")))
        ),
        tabPanel(
            "Sample Summary",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            h5(
                strong("Basic sample summary of inputted Mutation/MAF file:")
            ),
            withSpinner(verbatimTextOutput(ns("MAF2")))
        ),
        tabPanel(
            "Gene Summary",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            h5(strong(
                "Basic gene summary of inputted Mutation/MAF file:"
            )),
            withSpinner(verbatimTextOutput(ns("MAF3")))
        ),
        tabPanel(
            "All Fields Summary",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            h5(strong("All fields in Mutation/MAF file:")),
            withSpinner(verbatimTextOutput(ns("MAF4")))
        ),
        tabPanel(
            "Clinical Data",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            h5(strong(
                "Clinical data associated with samples:"
            )),
            withSpinner(verbatimTextOutput(ns("MAFClin")))
        )
    )
}

# UI function to produce MAF plot summary
Tab10_MAF_Visual_Summary_UI <- function(id) {
    ns <- NS(id)

    fluidRow(tabBox(
        width = 12,
        height = "1900px",
        tabPanel(
            "MafSummary",
            box(
                collapsible = TRUE,
                title = ("MafSummary Plots"),
                height = "800px",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                dropdownMenu = boxDropdown(
                    icon = fa_i(name = "info-circle", verify_fa = FALSE),
                    boxDropdownItem(
                        HTML(paste(
                            "Useful Resources for maftools:", "<br/>"
                        )),
                        tags$a(href = "https://www.bioconductor.org/packages/devel/bioc/vignettes/maftools/inst/doc/maftools.html", "maftools Vignette")
                    )
                ),
                sidebar = boxSidebar(
                    id = "Tab10_MAF_Summary_Sidebar",
                    width = 25,
                    background = "#599740",
                    icon = icon("rectangle-list"),
                    h5(strong("Options:")),
                    prettyToggle(
                        inputId = ns("Tab10_Summary_Remove_Outlier"),
                        label_on = "Remove Outliers",
                        label_off = "Remove Outliers",
                        icon_on = fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fa_i(name = "times", verify_fa = FALSE)
                    ),
                    prettyToggle(
                        inputId = ns("Tab10_Summary_Dashboard_Style"),
                        label_on = "Dashboard Style",
                        label_off = "Dashboard Style",
                        icon_on = fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fa_i(name = "times", verify_fa = FALSE),
                        value = TRUE
                    ),
                    prettyToggle(
                        inputId = ns("Tab10_Summary_Plot_Fraction"),
                        label_on = "Plot Fraction",
                        label_off = "Plot Fraction",
                        icon_on = fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fa_i(name = "times", verify_fa = FALSE)
                    ),
                    selectInput(
                        inputId = ns("Tab10_Summary_Add_Stat"),
                        label = "Add Stat:",
                        choices = c("mean", "median"),
                        selected = ""
                    ),
                    sliderInput(
                        inputId = ns("Tab10_Summary_Display_Top_Genes"),
                        label = "Display Top Genes:",
                        value = 10,
                        min = 0,
                        max = 20
                    ),
                    tags$hr(),
                    numericInput(
                        ns("Tab10_Summary_Plot_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab10_Summary_Plot_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab10_Download_Summary_Plot_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab10_Download_Summary_Plot_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                ),
                withSpinner(plotOutput(ns("summaryMAF"), height = "720px"))
            )
        ),
        tabPanel(
            "OncoPlot/OncoStrip",
            box(
                collapsible = TRUE,
                title = "OncoPlot",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                style = "height:530px",
                height = "530px",
                dropdownMenu = boxDropdown(
                    icon = fa_i(name = "info-circle", verify_fa = FALSE),
                    boxDropdownItem(
                        HTML(paste(
                            "Useful Resources for maftools:", "<br/>"
                        )),
                        tags$a(
                            href = "https://www.bioconductor.org/packages/devel/bioc/vignettes/maftools/inst/doc/maftools.html",
                            "maftools Vignette"
                        )
                    )
                ),
                sidebar = boxSidebar(
                    id = "Tab10_OncoPlot_Sidebar",
                    width = 25,
                    background = "#599740",
                    icon = icon("rectangle-list"),
                    h5(strong("Options:")),
                    sliderInput(
                        inputId = ns("Tab10_Oncoplot_Display_Top_Genes"),
                        label = "Display Top Genes:",
                        value = 20,
                        min = 0,
                        max = 100
                    ),
                    tags$hr(),
                    numericInput(
                        ns("Tab10_Oncoplot_Plot_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab10_Oncoplot_Plot_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab10_Download_Oncoplot_Plot_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab10_Download_Oncoplot_Plot_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                ),
                withSpinner(plotOutput(ns("oncoplotMAF"), height = "530px"))
            ),
            box(
                collapsible = TRUE,
                title = "Oncostrip Of Selected Genes",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                style = "height:530px",
                height = "530px",
                dropdownMenu = boxDropdown(
                    icon = fa_i(name = "info-circle", verify_fa = FALSE),
                    boxDropdownItem(
                        HTML(paste(
                            "Useful Resources for maftools:", "<br/>"
                        )),
                        tags$a(
                            href = "https://www.bioconductor.org/packages/devel/bioc/vignettes/maftools/inst/doc/maftools.html",
                            "maftools Vignette"
                        )
                    )
                ),
                sidebar = boxSidebar(
                    id = "Tab10_Oncostrip_Sidebar",
                    width = 25,
                    background = "#599740",
                    icon = icon("rectangle-list"),
                    h5(strong("Options:")),
                    selectInput(
                        ns("Tab10_Oncostrip_Select_Gene_1"),
                        "Gene of Interest 1:",
                        choices = ""
                    ),
                    selectInput(
                        ns("Tab10_Oncostrip_Select_Gene_2"),
                        "Gene of Interest 2:",
                        choices = ""
                    ),
                    selectInput(
                        ns("Tab10_Oncostrip_Select_Gene_3"),
                        "Gene of Interest 3:",
                        choices = ""
                    ),
                    tags$hr(),
                    numericInput(
                        ns("Tab10_Oncostrip_Plot_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab10_Oncostrip_Plot_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab10_Download_Oncostrip_Plot_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab10_Download_Oncostrip_Plot_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                ),
                withSpinner(plotOutput(ns("oncostripMAF"), height = "530px"))
            ),
            box(
                collapsible = TRUE,
                title = "Transition and Transversions",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                style = "height:530px",
                height = "530px",
                dropdownMenu = boxDropdown(
                    icon = fa_i(name = "info-circle", verify_fa = FALSE),
                    boxDropdownItem(
                        HTML(paste(
                            "Useful Resources for maftools:", "<br/>"
                        )),
                        tags$a(
                            href = "https://www.bioconductor.org/packages/devel/bioc/vignettes/maftools/inst/doc/maftools.html",
                            "maftools Vignette"
                        )
                    )
                ),
                sidebar = boxSidebar(
                    id = "Tab10_TT_Sidebar",
                    width = 25,
                    background = "#599740",
                    icon = icon("rectangle-list"),
                    h5(strong("Options:")),
                    prettyToggle(
                        inputId = ns("Tab10_TT_Plot_Fraction"),
                        label_on = "Plot Fraction",
                        label_off = "Plot Fraction",
                        icon_on = fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fa_i(name = "times", verify_fa = FALSE)
                    ),
                    prettyToggle(
                        inputId = ns("Tab10_TT_Include_Synonymous_Variants"),
                        label_on = "Include Synonymous Variants",
                        label_off = "Include Synonymous Variants",
                        icon_on = fa_i(name = "check", verify_fa = FALSE),
                        icon_off = fa_i(name = "times", verify_fa = FALSE)
                    ),
                    tags$hr(),
                    numericInput(
                        ns("Tab10_TT_Plot_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab10_TT_Plot_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab10_Download_TT_Plot_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab10_Download_TT_Plot_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                ),
                withSpinner(plotOutput(ns("TandT"), height = "530px"))
            )
        ),
        tabPanel(
            "Lollipop Plots",
            fluidRow(
                box(
                    collapsible = TRUE,
                    title = ("Choose Genes to Analyse"),
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    dropdownMenu = boxDropdown(
                        icon = fa_i(name = "info-circle", verify_fa = FALSE),
                        boxDropdownItem(
                            HTML(paste(
                                "Useful Resources for maftools:", "<br/>"
                            )),
                            tags$a(
                                href = "https://www.bioconductor.org/packages/devel/bioc/vignettes/maftools/inst/doc/maftools.html",
                                "maftools Vignette"
                            )
                        )
                    ),
                    column(
                        3,
                        numericInput(
                            inputId = ns("Tab10_Lollipop_Gene_Name_Column"),
                            label = "Gene Name Column:",
                            value = 1,
                            min = 1
                        )
                    ),
                    column(3, selectInput(
                        ns("Tab10_Lollipop_Select_Gene_1"),
                        "Gene of Interest 1:",
                        choices = ""
                    )),
                    column(3, selectInput(
                        ns("Tab10_Lollipop_Select_Gene_2"),
                        "Gene of Interest 2:",
                        choices = ""
                    )),
                    column(3, selectInput(
                        ns("Tab10_Lollipop_Select_Gene_3"),
                        "Gene of Interest 3:",
                        choices = ""
                    ))
                ),
                box(
                    collapsible = TRUE,
                    title = "Lollipop Plot 1",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    height = "480px",
                    withSpinner(plotOutput(ns("lol1"), height = "480px")),
                    sidebar = boxSidebar(
                        id = "Tab10_Lollipop_Plot1_Sidebar",
                        width = 25,
                        background = "#599740",
                        icon = icon("rectangle-list"),
                        h5(strong("Options:")),
                        selectInput(
                            ns("Tab10_Lollipop_Position_Label_1"),
                            "Positions to Label:",
                            choices = c("all", "None"),
                            selected = "None"
                        ),
                        sliderInput(
                            ns("Tab10_Lollipop_Label_Size_1"),
                            "Label Size:",
                            value = 0.9,
                            min = 0,
                            max = 2,
                            step = 0.1
                        ),
                        sliderInput(
                            ns("Tab10_Lollipop_Label_Angle_1"),
                            "Label Angle:",
                            value = 0,
                            min = 0,
                            max = 90
                        ),
                        tags$hr(),
                        prettyToggle(
                            inputId = ns("Tab10_Lollipop_Show_Mutation_Rate_1"),
                            label_on = "Show Somatic Mutation Rate",
                            label_off = "Show Somatic Mutation Rate",
                            icon_on = fa_i(name = "check", verify_fa = FALSE),
                            icon_off = fa_i(name = "times", verify_fa = FALSE),
                            value = TRUE
                        ),
                        prettyToggle(
                            inputId = ns("Tab10_Lollipop_Label_Domains_1"),
                            label_on = "Label Domains",
                            label_off = "Label Domains",
                            icon_on = fa_i(name = "check", verify_fa = FALSE),
                            icon_off = fa_i(name = "times", verify_fa = FALSE),
                            value = TRUE
                        ),
                        sliderInput(
                            ns("Tab10_Lollipop_Label_Domains_Size_1"),
                            "Domain Label Size:",
                            value = 0.8,
                            min = 0,
                            max = 2,
                            step = 0.1
                        ),
                        tags$hr(),
                        prettyToggle(
                            inputId = ns("Tab10_Lollipop_Repel_Yes_or_No_1"),
                            label_on = "Repel",
                            label_off = "Repel",
                            icon_on = fa_i(name = "check", verify_fa = FALSE),
                            icon_off = fa_i(name = "times", verify_fa = FALSE),
                            value = FALSE
                        ),
                        prettyToggle(
                            inputId = ns("Tab10_Lollipop_Show_Legend_1"),
                            label_on = "Show Legend",
                            label_off = "Show Legend",
                            icon_on = fa_i(name = "check", verify_fa = FALSE),
                            icon_off = fa_i(name = "times", verify_fa = FALSE),
                            value = TRUE
                        ),
                        sliderInput(
                            ns("Tab10_Lollipop_Size_Legend_1"),
                            "Legend Text Size:",
                            value = 0.8,
                            min = 0,
                            max = 2,
                            step = 0.1
                        ),
                        tags$hr(),
                        numericInput(
                            ns("Tab10_Lollipop_Plot_Width_1"),
                            "Plot Width (inches):",
                            value = 8,
                            min = 1,
                            max = 50
                        ),
                        numericInput(
                            ns("Tab10_Lollipop_Plot_Height_1"),
                            "Plot Height (inches):",
                            value = 5,
                            min = 1,
                            max = 50
                        ),
                        tags$hr(),
                        downloadButton(
                            ns("Tab10_Download_Lollipop_Plot_1_PNG"),
                            "Download Plot (PNG)",
                            style = "width:100%;"
                        ),
                        br(),
                        br(),
                        downloadButton(
                            ns("Tab10_Download_Lollipop_Plot_1_SVG"),
                            "Download Plot (SVG)",
                            style = "width:100%;"
                        )
                    )
                ),
                box(
                    collapsible = TRUE,
                    title = "Lollipop Plot 2",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    height = "480px",
                    withSpinner(plotOutput(ns("lol2"), height = "480px")),
                    sidebar = boxSidebar(
                        id = "Tab10_Lollipop_Plot2_Sidebar",
                        width = 25,
                        background = "#599740",
                        icon = icon("rectangle-list"),
                        h5(strong("Options:")),
                        selectInput(
                            ns("Tab10_Lollipop_Position_Label_2"),
                            "Positions to Label:",
                            choices = c("all", "None"),
                            selected = "None"
                        ),
                        sliderInput(
                            ns("Tab10_Lollipop_Label_Size_2"),
                            "Label Size:",
                            value = 0.9,
                            min = 0,
                            max = 2,
                            step = 0.1
                        ),
                        sliderInput(
                            ns("Tab10_Lollipop_Label_Angle_2"),
                            "Label Angle:",
                            value = 0,
                            min = 0,
                            max = 90
                        ),
                        tags$hr(),
                        prettyToggle(
                            inputId = ns("Tab10_Lollipop_Show_Mutation_Rate_2"),
                            label_on = "Show Somatic Mutation Rate",
                            label_off = "Show Somatic Mutation Rate",
                            icon_on = fa_i(name = "check", verify_fa = FALSE),
                            icon_off = fa_i(name = "times", verify_fa = FALSE),
                            value = TRUE
                        ),
                        prettyToggle(
                            inputId = ns("Tab10_Lollipop_Label_Domains_2"),
                            label_on = "Label Domains",
                            label_off = "Label Domains",
                            icon_on = fa_i(name = "check", verify_fa = FALSE),
                            icon_off = fa_i(name = "times", verify_fa = FALSE),
                            value = TRUE
                        ),
                        sliderInput(
                            ns("Tab10_Lollipop_Label_Domains_Size_2"),
                            "Domain Label Size:",
                            value = 0.8,
                            min = 0,
                            max = 2,
                            step = 0.1
                        ),
                        tags$hr(),
                        prettyToggle(
                            inputId = ns("Tab10_Lollipop_Repel_Yes_or_No_2"),
                            label_on = "Repel",
                            label_off = "Repel",
                            icon_on = fa_i(name = "check", verify_fa = FALSE),
                            icon_off = fa_i(name = "times", verify_fa = FALSE),
                            value = FALSE
                        ),
                        prettyToggle(
                            inputId = ns("Tab10_Lollipop_Show_Legend_2"),
                            label_on = "Show Legend",
                            label_off = "Show Legend",
                            icon_on = fa_i(name = "check", verify_fa = FALSE),
                            icon_off = fa_i(name = "times", verify_fa = FALSE),
                            value = TRUE
                        ),
                        sliderInput(
                            ns("Tab10_Lollipop_Size_Legend_2"),
                            "Legend Text Size:",
                            value = 0.8,
                            min = 0,
                            max = 2,
                            step = 0.1
                        ),
                        tags$hr(),
                        numericInput(
                            ns("Tab10_Lollipop_Plot_Width_2"),
                            "Plot Width (inches):",
                            value = 8,
                            min = 1,
                            max = 50
                        ),
                        numericInput(
                            ns("Tab10_Lollipop_Plot_Height_2"),
                            "Plot Height (inches):",
                            value = 5,
                            min = 1,
                            max = 50
                        ),
                        tags$hr(),
                        downloadButton(
                            ns("Tab10_Download_Lollipop_Plot_2_PNG"),
                            "Download Plot (PNG)",
                            style = "width:100%;"
                        ),
                        br(),
                        br(),
                        downloadButton(
                            ns("Tab10_Download_Lollipop_Plot_2_SVG"),
                            "Download Plot (SVG)",
                            style = "width:100%;"
                        )
                    )
                ),
                box(
                    collapsible = TRUE,
                    title = "Lollipop Plot 3",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    withSpinner(plotOutput(ns("lol3"), height = "480px")),
                    height = "480px",
                    sidebar = boxSidebar(
                        id = "Tab10_Lollipop_Plot3_Sidebar",
                        width = 25,
                        background = "#599740",
                        icon = icon("rectangle-list"),
                        h5(strong("Options:")),
                        selectInput(
                            ns("Tab10_Lollipop_Position_Label_3"),
                            "Positions to Label:",
                            choices = c("all", "None"),
                            selected = "None"
                        ),
                        sliderInput(
                            ns("Tab10_Lollipop_Label_Size_3"),
                            "Label Size:",
                            value = 0.9,
                            min = 0,
                            max = 2,
                            step = 0.1
                        ),
                        sliderInput(
                            ns("Tab10_Lollipop_Label_Angle_3"),
                            "Label Angle:",
                            value = 0,
                            min = 0,
                            max = 90
                        ),
                        tags$hr(),
                        prettyToggle(
                            inputId = ns("Tab10_Lollipop_Show_Mutation_Rate_3"),
                            label_on = "Show Somatic Mutation Rate",
                            label_off = "Show Somatic Mutation Rate",
                            icon_on = fa_i(name = "check", verify_fa = FALSE),
                            icon_off = fa_i(name = "times", verify_fa = FALSE),
                            value = TRUE
                        ),
                        prettyToggle(
                            inputId = ns("Tab10_Lollipop_Label_Domains_3"),
                            label_on = "Label Domains",
                            label_off = "Label Domains",
                            icon_on = fa_i(name = "check", verify_fa = FALSE),
                            icon_off = fa_i(name = "times", verify_fa = FALSE),
                            value = TRUE
                        ),
                        sliderInput(
                            ns("Tab10_Lollipop_Label_Domains_Size_3"),
                            "Domain Label Size:",
                            value = 0.8,
                            min = 0,
                            max = 2,
                            step = 0.1
                        ),
                        tags$hr(),
                        prettyToggle(
                            inputId = ns("Tab10_Lollipop_Repel_Yes_or_No_3"),
                            label_on = "Repel",
                            label_off = "Repel",
                            icon_on = fa_i(name = "check", verify_fa = FALSE),
                            icon_off = fa_i(name = "times", verify_fa = FALSE),
                            value = FALSE
                        ),
                        prettyToggle(
                            inputId = ns("Tab10_Lollipop_Show_Legend_3"),
                            label_on = "Show Legend",
                            label_off = "Show Legend",
                            icon_on = fa_i(name = "check", verify_fa = FALSE),
                            icon_off = fa_i(name = "times", verify_fa = FALSE),
                            value = TRUE
                        ),
                        sliderInput(
                            ns("Tab10_Lollipop_Size_Legend_3"),
                            "Legend Text Size:",
                            value = 0.8,
                            min = 0,
                            max = 2,
                            step = 0.1
                        ),
                        tags$hr(),
                        numericInput(
                            ns("Tab10_Lollipop_Plot_Width_3"),
                            "Plot Width (inches):",
                            value = 8,
                            min = 1,
                            max = 50
                        ),
                        numericInput(
                            ns("Tab10_Lollipop_Plot_Height_3"),
                            "Plot Height (inches):",
                            value = 5,
                            min = 1,
                            max = 50
                        ),
                        tags$hr(),
                        downloadButton(
                            ns("Tab10_Download_Lollipop_Plot_3_PNG"),
                            "Download Plot (PNG)",
                            style = "width:100%;"
                        ),
                        br(),
                        br(),
                        downloadButton(
                            ns("Tab10_Download_Lollipop_Plot_3_SVG"),
                            "Download Plot (SVG)",
                            style = "width:100%;"
                        )
                    )
                )
            )
        ),
        tabPanel(
            "Other Plots",
            id = "tabsetMAFother",
            box(
                dropdownMenu = boxDropdown(
                    icon = fa_i(name = "info-circle", verify_fa = FALSE),
                    boxDropdownItem(
                        HTML(paste(
                            "Useful Resources for maftools:", "<br/>"
                        )),
                        tags$a(
                            href = "https://www.bioconductor.org/packages/devel/bioc/vignettes/maftools/inst/doc/maftools.html",
                            "maftools Vignette"
                        )
                    )
                ),
                collapsible = TRUE,
                title = "Mutation Load Plot",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                withSpinner(plotOutput(ns("Mutload"), height = "500px")),
                height = "580px",
                sidebar = boxSidebar(
                    id = "Tab10_Mutation_Load_Sidebar",
                    width = 25,
                    background = "#599740",
                    icon = icon("rectangle-list"),
                    h5(strong("Options:")),
                    numericInput(
                        ns("Tab10_Mutation_Load_Plot_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab10_Mutation_Load_Plot_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab10_Download_Mutation_Load_Plot_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab10_Download_Mutation_Load_Plot_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                )
            ),
            box(
                collapsible = TRUE,
                title = "Somatic Interaction Plot",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                withSpinner(plotOutput(ns("VAF1"), height = "500px")),
                height = "540px",
                sidebar = boxSidebar(
                    id = "Tab10_Somatic_Interaction_Sidebar",
                    width = 25,
                    background = "#599740",
                    icon = icon("rectangle-list"),
                    h5(strong("Options:")),
                    sliderInput(
                        ns("Tab10_SIP_Display_Top_Genes"),
                        "Top Genes:",
                        value = 25,
                        min = 0,
                        max = 100,
                        step = 5
                    ),
                    sliderInput(
                        ns("Tab10_SIP_Pval_Lower_Threshold"),
                        "P-value Lower Threshold:",
                        value = 0.01,
                        min = 0,
                        max = 0.5
                    ),
                    sliderInput(
                        ns("Tab10_SIP_Pval_Upper_Threshold"),
                        "P-value Upper Threshold:",
                        value = 0.05,
                        min = 0,
                        max = 0.5
                    ),
                    tags$hr(),
                    numericInput(
                        ns("Tab10_SIP_Plot_Width"),
                        "Plot Width (inches):",
                        value = 8,
                        min = 1,
                        max = 50
                    ),
                    numericInput(
                        ns("Tab10_SIP_Plot_Height"),
                        "Plot Height (inches):",
                        value = 5,
                        min = 1,
                        max = 50
                    ),
                    tags$hr(),
                    downloadButton(
                        ns("Tab10_Download_SIP_Plot_PNG"),
                        "Download Plot (PNG)",
                        style = "width:100%;"
                    ),
                    br(),
                    br(),
                    downloadButton(
                        ns("Tab10_Download_SIP_Plot_SVG"),
                        "Download Plot (SVG)",
                        style = "width:100%;"
                    )
                )
            )
        )
    ))
}
