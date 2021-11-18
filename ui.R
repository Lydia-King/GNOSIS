source("Setup.R")

# Define UI for application
ui <- dashboardPage( 
    
    # Set title, title width, logo 
    header = dashboardHeader(title = tagList(span(class = "logo-lg", "GNOSIS"))), 
    
    # Create side-bar menu with all tab options:
    sidebar = dashboardSidebar(tags$style(".left-side, .main-sidebar {padding-top: 60px}"), width = 195,
                               sidebarMenu(menuItem("Input Files", tabName = "input_files", icon = fa_i(name ="file-alt")),
                                           menuItem("Exploratory Tables", tabName = "tables", icon = fa_i(name ="table")),
                                           menuItem("Recode/Subset Data", tabName = "Recode1", icon = fa_i(name ="sort-alpha-down"),
                                                    menuSubItem("Variable Types/Levels", tabName = "FactorLevels"),
                                                    menuSubItem("Subset/Filter", tabName = "Subset"),
                                                    menuSubItem("Recode Survival", tabName = "Recode"),
                                                    menuSubItem("CNA Score Data", tabName = "CNACalc"), 
                                                    menuSubItem("File Download", tabName = "Data_Down")),
                                           menuItem("Exploratory Plots", tabName = "plots", icon = fa_i(name ="chart-bar"),
                                                    menuSubItem("Boxplots", tabName = "boxplot", icon = fa_i(name ="align-left")),
                                                    menuSubItem("Scatterplots", tabName = "scatterplot", icon = fa_i(name ="braille")),
                                                    menuSubItem("Barplots", tabName = "Explor", icon = fa_i(name ="chart-bar")),
                                                    menuSubItem("Histograms/Density Plots", tabName = "Dist", icon = fa_i(name ="chart-area"))), 
                                           menuItem("Kaplan-Meier Plots", tabName = "survival", icon = fa_i(name ="heartbeat"),
                                                    menuSubItem("KM Plot Clinical Variables", tabName = "KMplot"),
                                                    menuSubItem("KM Plot CNA Score", tabName = "KMOver"),
                                                    menuSubItem("KM Plot by Treatment", tabName = "KMplotRadio")),
                                           menuItem("Association Tests", tabName = "ASTest", icon = fa_i(name ="connectdevelop")),
                                           menuItem("Cox PH Models", tabName = "Cox", icon = fa_i(name ="file-medical-alt"),
                                                    menuSubItem("Univariate Cox Models", tabName = "UniVar"),
                                                    menuSubItem("Multivariable Cox Models", tabName = "MultiVar"),
                                                    menuSubItem("Cox Model Assumptions", tabName = "AssumptionsOS")),
                                           menuItem("Adjusted Survival Curves", tabName = "AdjSurvival", icon = fa_i(name ="heartbeat")),
                                           menuItem("Survival Trees", tabName = "SurvTrees", icon = fa_i(name ="tree"),
                                                    menuSubItem("Rpart", tabName = "RpartTree"),
                                                    menuSubItem("Ctree", tabName = "STreeCtree")),
                                           menuItem("Maftools Summary", tabName = "MAFplots", icon = fa_i(name ="exclamation-triangle"),
                                                    menuSubItem("MAF Text Summary", tabName = "MAFText"),
                                                    menuSubItem("MAF Visual Summary", tabName = "MAFVis")),
                                           menuItem("Download Code/Log", tabName = "downlog", icon = fa_i(name ="file-download")))),
    
    # Set up theme - use custom theme and import css file
    body = dashboardBody(
        shinyDashboardThemes(theme = "blue_gradient"),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "Style_File.css")), 
        tags$head(tags$style(HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}'))),
        
        ## Tab 1) Input Data Files
        # Input File Tab: Space to upload desired files (Clinical Patient and Sample, CNA Scores and MAF)
        tabItems(tabItem(tabName = "input_files",
                         tabBox(width = "1000px",
                                
                                # Clinical Patient Data Tab
                                tabPanel("Clinical Patient Data",
                                         fluidRow(box(title = "Input Patient Data File", width = 3, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                      fileInput("Input_Patient_File", "Choose File:", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), tags$hr(),
                                                      prettyToggle(inputId = "Tab1_Clin_Header_Yes_or_No", label_on = "Header", label_off = "Header", icon_on = fa_i(name ="check"), icon_off = fa_i(name ="times"), value = T, outline = T, shape = c("curve"),  status_on = "primary"), # Input: Checkbox if file has header
                                                      fluidRow(column(7, awesomeRadio(inputId = "Tab1_Clin_Separator", label = "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t", inline = F, status = "primary")), # Input: Select Separator:
                                                               column(5, awesomeRadio("Tab1_Clin_Quote", "Quote:", choices = c(None = "", "Double" = '"', "Single" = "'"), selected = '"', inline = F, status = "primary"))), # Input: Select quotes
                                                      numericInput(inputId = "Tab1_Clin_Skip_Lines", label = "Number of Lines to Skip:", value = 4, min = 0, max = 10), tags$hr(), # Input: Choose number of lines to skip
                                                      h5(strong("Total Number of Clinical Variables:")), verbatimTextOutput('TotalC', placeholder = T),  # Space to display number of rows/columns
                                                      h5(strong("Total Number of Patients:")), 
                                                      tags$style(HTML('table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {background-color: rgba(44,222,235) !important;}')),
                                                      verbatimTextOutput('TotalR', placeholder = T)),
                                                  
                                                  box(collapsible = T, title = "Preview", width = 9, status = "primary", solidHeader = TRUE, withSpinner(DT::dataTableOutput("ClinicalP")), style = "height:555px"))), 
                                
                                
                                # Clinical Sample Data Tab 
                                tabPanel("Clinical Sample Data",
                                         fluidRow(box(collapsible = T, title = "Input Sample Data File", width = 3, status = "primary", solidHeader = TRUE, 
                                                      fileInput("Input_Sample_File", "Choose File:", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), tags$hr(),
                                                      prettyToggle(inputId = "Tab1_Sample_Header_Yes_or_No", label_on = "Header", label_off = "Header", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T, outline = T, shape = c("curve"),  status_on = "primary"), # Input: Checkbox if file has header
                                                      fluidRow(column(7, awesomeRadio(inputId = "Tab1_Sample_Separator", label = "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t", inline = F, status = "primary")), # Input: Select Separator:
                                                               column(5, awesomeRadio("Tab1_Sample_Quote", "Quote:", choices = c(None = "", "Double" = '"', "Single" = "'"), selected = '"', inline = F, status = "primary"))), # Input: Select quotes
                                                      numericInput(inputId = "Tab1_Sample_Skip_Lines", label = "Number of Lines to Skip:", value = 4, min = 0, max = 10),  tags$hr(), # Input: Choose number of lines to skip
                                                      h5(strong("Total Number of Clinical Variables:")), verbatimTextOutput('TotalC1', placeholder = T), # Space to display number of rows/columns
                                                      h5(strong("Total Number of Patients:")), verbatimTextOutput('TotalR2', placeholder = T)), 
                                                  
                                                  box(collapsible = T, title = "Preview" , width = 9, status = "primary", solidHeader = TRUE, withSpinner(DT::dataTableOutput("ClinicalS")), style = "height:555px"))),
                                
                                # View Merged Dataframe (Clinical Sample and  Patient Data)
                                tabPanel("All Clinical Data", 
                                         fluidRow(box(collapsible = T, title = "Preview of Merged Clinical Files" , width = 12, status = "primary", solidHeader = TRUE, height = "100px",
                                                      withSpinner(DT::dataTableOutput("ClinicalAll"))))),
                                
                                # Input CNA Data Tab 
                                tabPanel("CNA Data",  
                                         fluidRow(box(title = "Input CNA File", status = "primary", width = 3, solidHeader = TRUE, collapsible = T,
                                                      fileInput("Input_CNA_File", "Choose File:", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),  tags$hr(),
                                                      prettyToggle(inputId = "Tab1_CNA_Header_Yes_or_No", label_on = "Header", label_off = "Header", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T, outline = T, shape = c("curve"),  status_on = "primary"), # Input: Checkbox if file has header
                                                      fluidRow(column(7, awesomeRadio(inputId = "Tab1_CNA_Separator", label = "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t", inline = F, status = "primary")), # Input: Select Separator:
                                                               column(5, awesomeRadio("Tab1_CNA_Quote", "Quote:", choices = c(None = "", "Double" = '"', "Single" = "'"), selected = '"', inline = F, status = "primary"))), # Input: Select quotes
                                                      numericInput(inputId = "Tab1_CNA_Skip_Lines", label = "Number of Lines to Skip:", value = 0, min = 0, max = 10), tags$hr(), # Input: Choose number of lines to skip
                                                      h5(strong("Total Number of Columns:")), verbatimTextOutput('TotalCCNA', placeholder = T), # Space to display number of rows/columns
                                                      h5(strong("Total Number of Genes:")), verbatimTextOutput('TotalRCNA', placeholder = T)), # Action Button to display preview 
                                                  box(collapsible = T, title = "Preview", width = 9, status = "primary", solidHeader = TRUE, withSpinner(DT::dataTableOutput("CNA")), style = "height:555px"))),
                                
                                # Input Mutation Annotation Format (MAF) File
                                tabPanel("Mutation Data", 
                                         fluidRow(box(collapsible = T, title = "Input MAF File", width = 3, status = "primary", solidHeader = TRUE,
                                                      fileInput("Input_MAF_File", "Choose File:", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), tags$hr(),
                                                      prettyToggle(inputId = "Tab1_MAF_Header_Yes_or_No", label_on = "Header", label_off = "Header", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T, outline = T, shape = c("curve"),  status_on = "primary"), # Input: Checkbox if file has header
                                                      fluidRow(column(7, awesomeRadio(inputId = "Tab1_MAF_Separator", label = "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t", inline = F, status = "primary")), # Input: Select Separator:
                                                               column(5, awesomeRadio("Tab1_MAF_Quote", "Quote:", choices = c(None = "", "Double" = '"', "Single" = "'"), selected = '"', inline = F, status = "primary"))), # Input: Select quotes
                                                      numericInput(inputId = "Tab1_MAF_Skip_Lines", label = "Number of Lines to Skip:", value = 1, min = 0, max = 10), tags$hr(), # Input: Choose number of lines to skip
                                                      h5(strong("Total Number of Columns:")), verbatimTextOutput('TotalCMAF', placeholder = T), # Space to display number of rows/columns
                                                      h5(strong("Total Number of Mutations:")), verbatimTextOutput('TotalRMAF', placeholder = T)),
                                                  
                                                  box(collapsible = T, title = "Preview", width = 9, status = "primary", solidHeader = TRUE, withSpinner(DT::dataTableOutput("MAF")), style = "height:555px"))))),
                 
                 ## Tab 2 -  Tables
                 # Exploratory Table Tab: Space to explore uploaded data -> choose 5 columns to study 
                 tabItem(tabName = "tables",
                         tabBox(width = "1000px",
                                # Clinical Data Table
                                tabPanel("All Clinical Data",  
                                         fluidRow(box(title = "Data Table - Clinical Data", collapsible = T, width = 12, status = "primary", solidHeader = TRUE, withSpinner(DT::dataTableOutput("Clinical_Table"), proxy.height = "560px"), style = "height:580px; padding:10px", # Display Data Table
                                                      sidebar = boxSidebar(id="Tab2_Clinical_Sidebar", 
                                                                           width = 25, h4(strong("Select Columns to Display")), background = "#599740",  icon = shiny::icon("list-alt"),
                                                                           selectizeInput("Tab2_Clin_Column1_Variable", "Select Column 1:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput("Tab2_Clin_Column2_Variable", "Select Column 2:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput("Tab2_Clin_Column3_Variable", "Select Column 3:", choices = "None Selected", width = "95%", selected = "None Selected"),
                                                                           selectizeInput("Tab2_Clin_Column4_Variable", "Select Column 4:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput("Tab2_Clin_Column5_Variable", "Select Column 5:", choices = "None Selected", width = "95%", selected = "None Selected"))))), # Select Input: Display desired columns 
                                
                                # CNA Data Table
                                tabPanel("CNA Data", 
                                         fluidRow(box(title = "Data Table - CNA Data", collapsible = T,  width = 12, status = "primary", solidHeader = TRUE, withSpinner(DT::dataTableOutput("CNA_Table"), proxy.height = "560px"), style = "height:580px; padding:10px", # Display Data Table 
                                                      sidebar = boxSidebar(id="Tab2_CNA_Sidebar", width = 25, h4(strong("Select Columns to Display")), background = "#599740",  icon = shiny::icon("list-alt"),
                                                                           selectizeInput("Tab2_CNA_Column1_Variable", "Select Column 1:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput("Tab2_CNA_Column2_Variable", "Select Column 2:", choices = "None Selected", width = "95%", selected = "None Selected"),
                                                                           selectizeInput("Tab2_CNA_Column3_Variable", "Select Column 3:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput("Tab2_CNA_Column4_Variable", "Select Column 4:", choices = "None Selected", width = "95%", selected = "None Selected"), # Select Input: Display desired columns 
                                                                           selectizeInput("Tab2_CNA_Column5_Variable", "Select Column 5:", choices = "None Selected", width = "95%", selected = "None Selected"))))), 
                                
                                # Mutation (MAF) Table
                                tabPanel("Mutation Data", 
                                         fluidRow(box(title = "Data Table - Mutation Data", width = 12, collapsible = T, status = "primary", solidHeader = TRUE, withSpinner(DT::dataTableOutput("MAF_Table"), proxy.height = "560px"), style = "height:580px", # Display Data Table
                                                      sidebar = boxSidebar(id="Tab2_MAF_Sidebar", width = 25, h4(strong("Select Columns to Display")), background = "#599740",  icon = shiny::icon("list-alt"),
                                                                           selectizeInput("Tab2_MAF_Column1_Variable", "Select Column 1:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput("Tab2_MAF_Column2_Variable", "Select Column 2:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput("Tab2_MAF_Column3_Variable", "Select Column 3:", choices = "None Selected", width = "95%", selected = "None Selected"),
                                                                           
                                                                           selectizeInput("Tab2_MAF_Column4_Variable", "Select Column 4:", choices = "None Selected", width = "95%", selected = "None Selected"), selectizeInput("Tab2_MAF_Column5_Variable", "Select Column 5:", choices = "None Selected", width = "95%", selected = "None Selected"))))))), # Select Input: Display desired columns 
                 
                 ## Tab 3 - Subset/Recode/Format Data 
                 # 1) Convert Variables (Numeric - Factor etc.)
                 tabItem(tabName = "FactorLevels", 
                         fluidRow(box(collapsible = T, height = "500px", width = 12, solidHeader = TRUE, status = "primary", title = ("Clinical Variable Types"), verbatimTextOutput("DataTypes"), 
                                      sidebar = boxSidebar(id= "Tab3_Convert_Type_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("list-alt"), 
                                                           h4(strong("Convert to Numeric")), selectInput(inputId = "Tab3_Variables_to_Numeric", label = "Select Variables:",  choices = "", multiple = TRUE,  width = "97%"),
                                                           h4(strong("Convert to Factor")), selectInput(inputId = "Tab3_Variables_to_Factor", label = "Select Variables:",  choices = "", multiple = TRUE,  width = "97%"))),
                                  box(collapsible = T, style = "height:300px; overflow-y: scroll;overflow-x: hidden", solidHeader = TRUE, width = 12, status = "primary", title = ("Clinical Variable Levels"), withSpinner(verbatimTextOutput("DataLevels"), proxy.height = "280px")))),
                 
                 # 2) Subset Data (Can do based on 3 factor variables)
                 tabItem(tabName = "Subset",
                         box(collapsible = T, title =  "Filter and Preview Clinical Data", height = "500px", solidHeader = T, status = "primary", width = 12, withSpinner(DT::dataTableOutput("TableRecode1"), proxy.height = "460px"), # Make sure survival recoding has been implemented 
                             sidebar = boxSidebar(id="Tab3_Subset_Sidebar", width = 25,  background = "#599740",  icon = shiny::icon("list-alt"), h4(strong("Options")),
                                                  selectInput("Tab3_Subset_Variable_1", "1) Filter Based on Variable:", choices = "PATIENT_ID"), selectInput("Tab3_Subset_Variable_Levels_1", label = "Choose Variable Level:", choices = "None Selected", selected = "None Selected", multiple = TRUE),
                                                  selectInput("Tab3_Subset_Variable_2", "2) Filter Based on Variable:", choices = "PATIENT_ID"), selectInput("Tab3_Subset_Variable_Levels_2", label = "Choose Variable Level:", choices = "None Selected", selected = "None Selected", multiple = TRUE),
                                                  selectInput("Tab3_Subset_Variable_3", "3) Filter Based on Variable:", choices = "PATIENT_ID"), selectInput("Tab3_Subset_Variable_Levels_3", label = "Choose Variable Level:", choices = "None Selected", selected = "None Selected", multiple = TRUE))),
                         box(collapsible = T, solidHeader = TRUE, height = "300px", title = "Check Selected Variable Levels", width = 12, status = "primary", style ="height:300px; overflow-y: scroll;", withSpinner(verbatimTextOutput("TableLevels"), proxy.height = "180px"))),
                 
                 # 3) Survival Recoding
                 # Recode and Subset Data  
                 tabItem(tabName = "Recode",
                         box(collapsible = T, solidHeader = TRUE, width = 3, status = "primary", title = ("Select Survival Columns"), # Select Options for recoding survival, CNA calculations start column and number of splits in data desired
                             awesomeRadio(inputId = "Tab3_Recode_Survival_Yes_or_No", label = "Recode Survival Columns:",  choices = c("No", "Yes"), selected = "No", inline = F, status = "primary"), tags$hr(),
                             selectInput("Tab3_Select_OS_Column", "Select OS Column:", choices = ""), selectInput("Tab3_Select_DSS_Column", "Select DSS Column:", choices = ""), tags$hr(),
                             textInput("Tab3_OS_Event", "OS Event:", "DECEASED"), verbatimTextOutput("value"), textInput("Tab3_DSS_Event", "DSS Event:", "Died of Disease"), verbatimTextOutput("Died of Disease")),
                         
                         box(collapsible = T, solidHeader = TRUE, title = "Check Survival Recoding", width = 9, status = "primary", style = "height:500px", withSpinner(dataTableOutput("TableRecode"), proxy.height = "480px"))),
                 
                 # 4) CNA Calculation 
                 tabItem(tabName = "CNACalc",
                         box(style = "height:550px", sidebar = boxSidebar(id = "Tab3_CNA_Calculation_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("list-alt"), h4(strong("Options")),
                                                                          numericInput(inputId = "Tab3_CNA_Start_Column", label = "Indicate CNA Start Column:", value = 3,  width = "95%", min = 1), tags$hr(width = "93%"),
                                                                          awesomeRadio(inputId = "Tab3_CNA_of_Interest", label = "CNA of Interest:", choices = c("None", "Single Gene", "CNA Score"), selected = "None", inline = F, status = "primary"), tags$hr(width = "93%"),
                                                                          conditionalPanel(condition = "input.Tab3_CNA_of_Interest == 'None'",  h5(strong("Options:"))),
                                                                          conditionalPanel(condition = "input.Tab3_CNA_of_Interest== 'Single Gene'", h5(strong("Options:")), textInput("Tab3_Select_Genes", "Select Gene of Interest:", "TP53, PTEN, BRCA1", placeholder = T,  width = "95%")),
                                                                          conditionalPanel(condition = "input.Tab3_CNA_of_Interest == 'CNA Score'", h5(strong("Options:")), prettyToggle(inputId = "Tab3_CNA_Remove_NAs_Yes_or_No", label_on = "Remove NAs", label_off = "Remove NAs", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                                                           prettyToggle(inputId = "Tab3_Segment_CNA_Yes_or_No", label_on = "Segment Data", label_off = "Segment Data", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                                                           numericInput(inputId = "Tab3_Number_of_Segments", label = "Number of Segments:", value = 4, width = "95%"))),
                             collapsible = T, solidHeader = TRUE, width = 12, status = "primary", title = ("CNA Score Exploration"), withSpinner(dataTableOutput("TableCNACalc"), proxy.height = "510px"))),
                 
                 # 5) CNA Calculation Download 
                 tabItem(tabName = "Data_Down",
                         box(style = "height:530px", sidebar = boxSidebar(id = "Tab3_CNA_Download_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("list-alt"), h4(strong("Options")), 
                                                                          awesomeRadio(inputId = "Tab3_Download_File_Separator", label = "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t", inline = F, status = "primary"), tags$hr(width = "93%"), # Input: Select Separator:
                                                                          prettyToggle(inputId = "Tab3_Download_File_Quote", label_on = "Include Quotes", label_off = "Include Quotes", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                                          prettyToggle(inputId = "Tab3_Download_File_Row_Names", label_on = "Include Row Names", label_off = "Include Row Names", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                                          tags$hr(width = "93%"), downloadButton('Tab3_Download_File', 'Download Data')),
                             collapsible = T, solidHeader = TRUE, width = 12, status = "primary", title = ("Preview and Download Processed Data"), withSpinner(dataTableOutput("TableData"), proxy.height = "510px"))),
                 
                 # Tab 4: Exploratory Plots:  1) Boxplot
                 tabItem(tabName = "boxplot", box(title = "Boxplot", solidHeader = T, width = 12, height = "630px", status = "primary", collapsible = T, withSpinner(plotOutput("plot3", height = "570px")), 
                                                  sidebar = boxSidebar(width = 25, id = "Tab4_Boxplot_Sidebar", background = "#599740",  icon = shiny::icon("list-alt"),
                                                                       selectizeInput("Tab4_Boxplot_Select_X_Variable", "Select Variable (x):", choices = ""), selectizeInput("Tab4_Boxplot_Select_Y_Variable", "Select Variable (y):", choices = ""), tags$hr(), # X and Y variable selection
                                                                       prettyToggle(inputId = "Tab4_Boxplot_by_Sample_Size", label_on = "Boxplot by Sample Size", label_off = "Boxplot by Sample Size", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                                       prettyToggle(inputId = "Tab4_Boxplot_Display_NAs", label_on = "Display NA Values", label_off = "Display NA Values", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                                       selectInput("Tab4_Boxplot_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "none"), tags$hr(), 
                                                                       textInput("Tab4_Boxplot_Title", "Plot Title:", "Clinical Variable Boxplot", placeholder = T),
                                                                       textInput("Tab4_Boxplot_X_Axis_Title", "X-axis Title:", "Clinical Variable X", placeholder = T), textInput("Tab4_Boxplot_Y_Axis_Title", "Y-axis Title:", "Clinical Variable Y", placeholder = T), 
                                                                       textInput("Tab4_Boxplot_Legend_Title", "Legend Title:", "Legend", placeholder = T), 
                                                                       tags$hr(), numericInput("Tab4_Boxplot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                                       numericInput("Tab4_Boxplot_Height", "Plot Height:", value=500, min=100, max=3000),
                                                                       tags$hr(), downloadButton('Tab4_Download_Boxplot','Download Plot')))), # Options for boxplot aesthetics 
                 
                 # 2) Scatterplots 
                 tabItem(tabName = "scatterplot", box(title = ("Scatterplot"), collapsible = T, solidHeader = T, width = 12, height= "630px", status = "primary", withSpinner(plotOutput("Scatterplot1", height = "570px")), 
                                                      sidebar = boxSidebar(width = 25, id = "Tab4_Scatterplot_Sidebar", background = "#599740",  icon = shiny::icon("list-alt"),
                                                                           selectizeInput("Tab4_Scatterplot_Select_X_Variable", "Select Variable (x):", choices = ""), selectizeInput("Tab4_Scatterplot_Select_Y_Variable", "Select Variable (y):", choices = ""), selectizeInput("Tab4_Scatterplot_Select_Colour_Var", "Select Variable (colour):", choices = ""), tags$hr(), # X and Y variable selection
                                                                           prettyToggle(inputId = "Tab4_Scatterplot_Display_NAs", label_on = "Display NA Values", label_off = "Display NA Values", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                                           selectInput("Tab4_Scatterplot_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "none"), tags$hr(), 
                                                                           textInput("Tab4_Scatterplot_Title", "Plot Title:", "Scatterplot of Clinical Variables and Scores", placeholder = T),
                                                                           textInput("Tab4_Scatterplot_X_Axis_Title", "X-axis Title:", "Clinical Variable X", placeholder = T), textInput("Tab4_Scatterplot_Y_Axis_Title", "Y-axis Title:", "Clinical Variable Y", placeholder = T), 
                                                                           textInput("Tab4_Scatterplot_Legend_Title", "Legend Title:", "Legend", placeholder = T), 
                                                                           tags$hr(), numericInput("Tab4_Scatterplot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                                           numericInput("Tab4_Scatterplot_Height", "Plot Height:", value=500, min=100, max=3000),
                                                                           tags$hr(), downloadButton('Tab4_Download_Scatterplot','Download Plot')))),  # Options for scatterplot aesthetics 
                 
                 # 3) Barplots
                 tabItem(tabName = "Explor", box(title = ("Barplot"), solidHeader = T, width = 12, height= "630px", status = "primary", collapsible = T, withSpinner(plotOutput("Association3", height = "570px")), 
                                                 sidebar = boxSidebar(width = 25, id = "Tab4_Barplot_Sidebar", background = "#599740",  icon = shiny::icon("list-alt"),
                                                                      selectizeInput("Tab4_Barplot_Select_X_Variable", "Select Variable (x):", choices = ""), # X and Y variable selection
                                                                      selectizeInput("Tab4_Barplot_Select_Y_Variable", "Select Variable (y):", choices = ""), tags$hr(),
                                                                      prettyToggle(inputId = "Tab4_Barplot_Display_NAs", label_on = "Display NA Values (x)", label_off = "Display NA Values (x)", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                                      selectInput("Tab4_Barplot_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "none"), tags$hr(),
                                                                      textInput("Tab4_Barplot_Title", "Plot Title:", "Barplot of Clinical Variables and Scores", placeholder = T),
                                                                      textInput("Tab4_Barplot_X_Axis_Title", "X-axis Title:", "Clinical Variable X", placeholder = T), textInput("Tab4_Barplot_Y_Axis_Title", "Y-axis Title:", "Count", placeholder = T), 
                                                                      textInput("Tab4_Barplot_Legend_Title", "Legend Title:", "Legend", placeholder = T), 
                                                                      tags$hr(), numericInput("Tab4_Barplot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                                      numericInput("Tab4_Barplot_Height", "Plot Height:", value=500, min=100, max=3000),
                                                                      tags$hr(), downloadButton('Tab4_Download_Barplot','Download Plot')))), # Options for Barplot aesthetics 
                 
                 
                 # 4) Density plot of CNA scores etc.
                 # Options -> Plain density plot, segmented density plot and faceted density plots
                 tabItem(tabName = "Dist", 
                         box(title = "Histogram and Density Plots", width = 12, solidHeader = T, status = "primary", height = "630px", collapsible = T,
                             sidebar = boxSidebar(width = 25, id = "Tab4_Density_Histogram_Sidebar", background = "#599740",  icon = shiny::icon("list-alt"),
                                                  selectInput("Tab4_Select_Plot_Variable", "Select Continuous Variable:", choices = ""),
                                                  awesomeRadio(inputId = "Tab4_Select_Plot_Type", label = "Plot Type:",  choices = c("Histogram", "Density Plot", "Both"), selected = "Histogram", inline = F, status = "primary"),
                                                  conditionalPanel(condition = "input.Tab4_Select_Plot_Type == 'Histogram'", h5(strong("Options:")), 
                                                                   selectizeInput("Tab4_Select_Histogram_Type", "Select Plot Type:", c("Plain", "Facet Wrap")),
                                                                   conditionalPanel(condition = "input.Tab4_Select_Histogram_Type == 'Plain'", tags$hr(), 
                                                                                    numericInput(inputId = "Tab4_Histogram_Number_of_Bins", label = "Bin Width:", value = 10, max = 3000, min = 1), 
                                                                                    sliderInput(inputId = "Tab4_Histogram_Alpha", label = "alpha:", min=0, max=1, value=0.4), tags$hr(),
                                                                                    selectizeInput("Tab4_Histogram_Select_Fill_Variable", "Select Fill Variable:", choices = ""),
                                                                                    prettyToggle(inputId = "Tab4_Histogram_Display_NAs", label_on = "Display NA Values", label_off = "Display NA Values", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                                                    selectInput("Tab4_Histogram_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "none"), tags$hr(), 
                                                                                    textInput("Tab4_Histogram_Title", "Plot Title:", "Histogram of Selected Continuous Variables", placeholder = T),
                                                                                    textInput("Tab4_Histogram_X_Axis_Title", "X-axis Title:", "Clinical Variable X", placeholder = T), textInput("Tab4_Histogram_Y_Axis_Title", "Y-axis Title:", "Frequency", placeholder = T), 
                                                                                    textInput("Tab4_Histogram_Legend_Title", "Legend Title:", "Legend", placeholder = T),
                                                                                    tags$hr(), numericInput("Tab4_Histogram_Width", "Plot Width:", value=500, min=0, max=3000), 
                                                                                    numericInput("Tab4_Histogram_Height", "Plot Height:", value=500, min=0, max=3000), tags$hr(),
                                                                                    downloadButton('Tab4_Download_Histogram','Download Plot')),
                                                                   conditionalPanel(condition = "input.Tab4_Select_Histogram_Type == 'Facet Wrap'",
                                                                                    selectizeInput("Tab4_Histogram_Select_Facet_Variable", "Select Facet Variable:", choices = ""),
                                                                                    numericInput(inputId = "Tab4_Faceted_Histogram_Number_of_Bins", label = "Bin Width:", value = 10, max = 3000, min = 1), tags$hr(),
                                                                                    sliderInput(inputId = "Tab4_Faceted_Histogram_Alpha", label = "alpha:", min=0, max=1, value=0.4), 
                                                                                    numericInput("Tab4_Faceted_Histogram_Number_of_Columns", label = "Number of Columns:", min=1, max=10, value=2), tags$hr(),
                                                                                    prettyToggle(inputId = "Tab4_Faceted_Histogram_Display_NAs", label_on = "Display NA Values", label_off = "Display NA Values", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                                                    selectInput("Tab4_Faceted_Histogram_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "none"), tags$hr(),
                                                                                    textInput("Tab4_Faceted_Histogram_Title", "Plot Title:", "Histogram by Variable", placeholder = T),
                                                                                    textInput("Tab4_Faceted_Histogram_X_Axis_Title", "X-axis Title:", "Clinical Variable X", placeholder = T), textInput("Tab4_Faceted_Histogram_Y_Axis_Title", "Y-axis Title:", "Frequency", placeholder = T), 
                                                                                    textInput("Tab4_Faceted_Histogram_Legend_Title", "Legend Title:", "Legend", placeholder = T), 
                                                                                    tags$hr(), numericInput("Tab4_Faceted_Histogram_Width", "Plot Width:", value=500, min=0, max=3000), 
                                                                                    numericInput("Tab4_Faceted_Histogram_Height", "Plot Height:", value=500, min=0, max=3000),
                                                                                    tags$hr(), downloadButton('Tab4_Download_Faceted_Histogram','Download Plot'))),
                                                  conditionalPanel(condition = "input.Tab4_Select_Plot_Type == 'Density Plot'", h5(strong("Options:")),
                                                                   selectInput("Tab4_Select_Density_Plot_Type", "Select Plot Type:", c("Plain","Segmented", "Facet Wrap")),
                                                                   conditionalPanel(condition = "input.Tab4_Select_Density_Plot_Type == 'Plain'", tags$hr(),
                                                                                    sliderInput(inputId = "Tab4_Density_Plot_Alpha", label = "alpha:", min=0, max=1, value=0.4), tags$hr(),
                                                                                    selectizeInput("Tab4_Density_Plot_Select_Fill_Variable", "Select Fill Variable:", choices = ""),
                                                                                    prettyToggle(inputId = "Tab4_Density_Plot_Display_NAs", label_on = "Display NA Values", label_off = "Display NA Values", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                                                    selectInput("Tab4_Density_Plot_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "none"), tags$hr(), 
                                                                                    textInput("Tab4_Density_Plot_Title", "Plot Title:", "Density Plots of Selected Continuous Variables", placeholder = T),
                                                                                    textInput("Tab4_Density_Plot_X_Axis_Title", "X-axis Title:", "Clinical Variable X", placeholder = T), textInput("Tab4_Density_Plot_Y_Axis_Title", "Y-axis Title:", "Density", placeholder = T), 
                                                                                    textInput("Tab4_Density_Plot_Legend_Title", "Legend Title:", "Legend", placeholder = T),
                                                                                    tags$hr(), numericInput("Tab4_Density_Plot_Width", "Plot Width:", value=500, min=0, max=3000), 
                                                                                    numericInput("Tab4_Density_Plot_Height", "Plot Height:", value=500, min=0, max=3000), tags$hr(),
                                                                                    downloadButton('Tab4_Download_Density_Plot','Download Plot')),
                                                                   conditionalPanel(condition = "input.Tab4_Select_Density_Plot_Type == 'Segmented'",
                                                                                    numericInput(inputId = "Tab4_Segmented_Density_Plot_Number_of_Segments", label = "Number of Segments:", value = 4), tags$hr(),
                                                                                    prettyToggle(inputId = "Tab4_Segmented_Density_Plot_Display_Legend", label_on = "Display Legend", label_off = "Display Legend", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), tags$hr(), 
                                                                                    textInput("Tab4_Segmented_Density_Plot_Title", "Plot Title:", "Segmented Density Plots of Selected Continuous Variables", placeholder = T),
                                                                                    textInput("Tab4_Segmented_Density_Plot_X_Axis_Title", "X-axis Title:", "Clinical Variable X", placeholder = T), textInput("Tab4_Segmented_Density_Plot_Y_Axis_Title", "Y-axis Title:", "Density", placeholder = T), 
                                                                                    textInput("Tab4_Segmented_Density_Plot_Legend_Title", "Legend Title:", "Legend", placeholder = T), 
                                                                                    tags$hr(), numericInput("Tab4_Segmented_Density_Plot_Width", "Plot Width:", value=500, min=0, max=3000), 
                                                                                    numericInput("Tab4_Segmented_Density_Plot_Height", "Plot Height:", value=500, min=0, max=3000), tags$hr(),
                                                                                    downloadButton('Tab4_Download_Segmented_Density_Plot','Download Plot')), 
                                                                   conditionalPanel(condition = "input.Tab4_Select_Density_Plot_Type == 'Facet Wrap'",
                                                                                    selectizeInput("Tab4_Density_Plot_Select_Facet_Variable", "Select Facet Variable:", choices = ""),
                                                                                    sliderInput(inputId = "Tab4_Faceted_Density_Plot_Alpha", label = "alpha:", min=0, max=1, value=0.4), 
                                                                                    numericInput("Tab4_Faceted_Density_Plot_Number_of_Columns", label = "Number of Columns:", min=1, max=10, value=2), tags$hr(),
                                                                                    prettyToggle(inputId = "Tab4_Faceted_Density_Plot_Display_NAs", label_on = "Display NA Values", label_off = "Display NA Values", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                                                    selectInput("Tab4_Faceted_Density_Plot_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "none"), tags$hr(),
                                                                                    textInput("Tab4_Faceted_Density_Plot_Title", "Plot Title:", "Exploartion of Density Plots by Variable", placeholder = T),
                                                                                    textInput("Tab4_Faceted_Density_Plot_X_Axis_Title", "X-axis Title:", "Clinical Variable X", placeholder = T), textInput("Tab4_Faceted_Density_Plot_Y_Axis_Title", "Y-axis Title:", "Density", placeholder = T), 
                                                                                    textInput("Tab4_Faceted_Density_Plot_Legend_Title", "Legend Title:", "Legend", placeholder = T), 
                                                                                    tags$hr(), numericInput("Tab4_Faceted_Density_Plot_Width", "Plot Width:", value=500, min=0, max=3000), 
                                                                                    numericInput("Tab4_Faceted_Density_Plot_Height", "Plot Height:", value=500, min=0, max=3000), tags$hr(),
                                                                                    downloadButton('Tab4_Download_Faceted_Density_Plot','Download Plot'))), 
                                                  conditionalPanel(condition = "input.Tab4_Select_Plot_Type == 'Both'", h5(strong("Options:")),
                                                                   selectInput("Tab4_Select_Both_Plot_Type", "Select Plot Type:", c("Plain", "Facet Wrap")),
                                                                   conditionalPanel(condition = "input.Tab4_Select_Both_Plot_Type == 'Plain'", tags$hr(),
                                                                                    sliderInput(inputId = "Tab4_Both_Density_Plot_Alpha", label = "alpha (density):", min=0, max=1, value=0.4), 
                                                                                    sliderInput(inputId = "Tab4_Both_Histogram_Alpha", label = "alpha (histogram):", min=0, max=1, value=0.4), tags$hr(),
                                                                                    selectizeInput("Tab4_Both_Plot_Select_Fill_Variable", "Select Fill Variable:", choices = ""),
                                                                                    prettyToggle(inputId = "Tab4_Both_Plot_Display_NAs", label_on = "Display NA Values", label_off = "Display NA Values", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                                                    selectInput("Tab4_Both_Plot_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "none"), tags$hr(), 
                                                                                    textInput("Tab4_Both_Plot_Title", "Plot Title:", "Histogram and Density Plots of Selected Continuous Variables", placeholder = T),
                                                                                    textInput("Tab4_Both_Plot_X_Axis_Title", "X-axis Title:", "Clinical Variable X", placeholder = T), textInput("Tab4_Both_Plot_Y_Axis_Title", "Y-axis Title:", "Density", placeholder = T), 
                                                                                    textInput("Tab4_Both_Plot_Legend_Title", "Legend Title:", "Legend", placeholder = T),
                                                                                    tags$hr(), numericInput("Tab4_Both_Plot_Width", "Plot Width:", value=500, min=0, max=3000), 
                                                                                    numericInput("Tab4_Both_Plot_Height", "Plot Height:", value=500, min=0, max=3000), tags$hr(),
                                                                                    downloadButton('Tab4_Download_Both_Plot','Download Plot')),
                                                                   conditionalPanel(condition = "input.Tab4_Select_Both_Plot_Type == 'Facet Wrap'",
                                                                                    selectizeInput("Tab4_Both_Plot_Select_Facet_Variable", "Select Facet Variable:", choices = ""),
                                                                                    sliderInput(inputId = "Tab4_Faceted_Both_Density_Plot_Alpha", label = "alpha (density):", min=0, max=1, value=0.4), 
                                                                                    sliderInput(inputId = "Tab4_Faceted_Both_Histogram_Plot_Alpha", label = "alpha (histogram):", min=0, max=1, value=0.4), 
                                                                                    numericInput("Tab4_Faceted_Both_Plot_Number_of_Columns", label = "Number of Columns:", min=1, max=20, value=2), tags$hr(),
                                                                                    prettyToggle(inputId = "Tab4_Faceted_Both_Plot_Display_NAs", label_on = "Display NA Values", label_off = "Display NA Values", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                                                    selectInput("Tab4_Faceted_Both_Plot_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "none"), tags$hr(),
                                                                                    textInput("Tab4_Faceted_Both_Plot_Title", "Plot Title:", "Histogram and Density Plot by Variable", placeholder = T),
                                                                                    textInput("Tab4_Faceted_Both_Plot_X_Axis_Title", "X-axis Title:", "Clinical Variable X", placeholder = T), textInput("Tab4_Faceted_Both_Plot_Y_Axis_Title", "Y-axis Title:", "Density", placeholder = T), 
                                                                                    textInput("Tab4_Faceted_Both_Plot_Legend_Title", "Legend Title:", "Legend", placeholder = T),
                                                                                    tags$hr(), numericInput("Tab4_Faceted_Both_Plot_Width", "Plot Width:", value=500, min=0, max=3000), 
                                                                                    numericInput("Tab4_Faceted_Both_Plot_Height", "Plot Height:", value=500, min=0, max=3000), tags$hr(),
                                                                                    downloadButton('Tab4_Download_Faceted_Both_Plot','Download Plot')))),
                             conditionalPanel(condition = "input.Tab4_Select_Plot_Type == 'Histogram'", width = 12,
                                              conditionalPanel(condition = "input.Tab4_Select_Histogram_Type == 'Plain'", width = 12, withSpinner(plotOutput("CNAHist", height = "540px"))), # Plain Histogram Plot
                                              conditionalPanel(condition = "input.Tab4_Select_Histogram_Type == 'Facet Wrap'", width = 12, withSpinner(plotOutput("CNAHist1", height = "600px")))), # Faceted Histogram Plot
                             conditionalPanel(condition = "input.Tab4_Select_Plot_Type == 'Density Plot'", width = 12,
                                              conditionalPanel(condition = "input.Tab4_Select_Density_Plot_Type == 'Plain'", width = 12, withSpinner(plotOutput("CNADist", height = "540px"))), # Plain Density Plot
                                              conditionalPanel(condition = "input.Tab4_Select_Density_Plot_Type == 'Segmented'",  width = 12, withSpinner(plotOutput("CNADist1", height="540px"))), # Segmented Density Plot 
                                              conditionalPanel(condition = "input.Tab4_Select_Density_Plot_Type == 'Facet Wrap'", width = 12, withSpinner(plotOutput("CNADist2", height = "600px")))), 
                             conditionalPanel(condition = "input.Tab4_Select_Plot_Type == 'Both'", width = 12,
                                              conditionalPanel(condition = "input.Tab4_Select_Both_Plot_Type == 'Plain'", width = 12, withSpinner(plotOutput("CNABoth", height = "540px"))), # Plain Both Plot
                                              conditionalPanel(condition = "input.Tab4_Select_Both_Plot_Type == 'Facet Wrap'", width = 12, withSpinner(plotOutput("CNABoth1", height = "600px")))))), # Faceted Both Plot
                 
                 ## Tab 5 - Survival Analysis
                 # 1) Survival Analysis KM plots -> 1) KM Survival curves and log rank tests (Clinical Variables)
                 tabItem(tabName = "KMplot",
                         box(title = ("Kaplan-Meier Plot for Clinical Variables"), collapsible = T, solidHeader=T, width = 12, style = "height:520px; overflow-y: hidden", height = "520px", status = "primary", withSpinner(plotOutput("KM1", height="500px")), 
                             sidebar = boxSidebar(width = 25, id = "Tab5_KM_Clinical_Sidebar",  background = "#599740",  icon = shiny::icon("list-alt"), selectizeInput("Tab5_KM_Clinical_Survival_Time", "Survival Time:", choices = ""), selectizeInput("Tab5_KM_Clinical_Event_Status", "Event Status:", choices = ""), 
                                                  selectizeInput("Tab5_KM_Clinical_Select_Variable", "Select Variable:", choices = ""), tags$hr(), # Select survival variables i.e. time to event, event status Aand variable of interest
                                                  prettyToggle(inputId = "Tab5_KM_Clinical_Display_CI", label_on = "Display CI", label_off = "Display CI", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                  prettyToggle(inputId = "Tab5_KM_Clinical_Display_Risk_Table", label_on = "Display Risk Table", label_off = "Display Risk Table", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                  prettyToggle(inputId = "Tab5_KM_Clinical_Display_Pval", label_on = "Display P-value", label_off = "Display P-value", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                  selectInput("Tab5_KM_Clinical_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right"), tags$hr(), 
                                                  textInput("Tab5_KM_Clinical_Plot_Title", "Plot Title:", "Breast cancer patients in METABRIC data", placeholder = T),
                                                  textInput("Tab5_KM_Clinical_X_Axis_Title", "X-axis Title:", "Survival Time", placeholder = T), textInput("Tab5_KM_Clinical_Y_Axis_Title", "Y-axis Title:", "Survival Probability", placeholder = T), 
                                                  textInput("Tab5_KM_Clinical_Legend_Title", "Legend Title:", "Legend", placeholder = T), tags$hr(), numericInput("Tab5_KM_Clinical_Width", "Plot Width:", value=500, min=0, max=3000), 
                                                  numericInput("Tab5_KM_Clinical_Height", "Plot Height:", value=500, min=0, max=3000), tags$hr(), downloadButton('Tab5_Download_KM_Clinical','Download Plot'))), 
                         box(title = ("Logrank Test"), collapsible = T, style="overflow-y: hidden", solidHeader = T, width = 12, status = "primary", withSpinner(verbatimTextOutput("KMlogrank")))), # Logrank Test
                 
                 
                 # 2) Segmented (Quartiled) Survival Plots  
                 tabItem(tabName = "KMOver",
                         box(title = ("Kaplan-Meier Plot for CNA Scores and Quartiles"), width = 12, status = "primary", solidHeader = T,  style = "height:520px; overflow-y: hidden", height = "520px", collapsible = T, withSpinner(plotOutput("PercentSurv", height = "500px")),  
                             sidebar = boxSidebar(width = 25, id = "Tab5_KM_CNA_Quartile_Sidebar",  background = "#599740",  icon = shiny::icon("list-alt"), selectizeInput("Tab5_KM_CNA_Survival_Time", "Survival Time Column:", choices = ""), selectizeInput("Tab5_KM_CNA_Event_Status", "Event Status Column:", choices = ""), selectizeInput("Tab5_KM_CNA_Select_Variable", "Select Variable:", choices = ""), tags$hr(), # Variables of Interest
                                                  prettyToggle(inputId = "Tab5_KM_CNA_Display_CI", label_on = "Display CI", label_off = "Display CI", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                  prettyToggle(inputId = "Tab5_KM_CNA_Display_Risk_Table", label_on = "Display Risk Table", label_off = "Display Risk Table", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                  prettyToggle(inputId = "Tab5_KM_CNA_Display_Pval", label_on = "Display P-value", label_off = "Display P-value", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                  selectInput("Tab5_KM_CNA_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right"), tags$hr(), 
                                                  textInput("Tab5_KM_CNA_Plot_Title", "Plot Title:", "Breast cancer patients in METABRIC data", placeholder = T),
                                                  textInput("Tab5_KM_CNA_X_Axis_Title", "X-axis Title:", "Survival Time", placeholder = T), textInput("Tab5_KM_CNA_Y_Axis_Title", "Y-axis Title:", "Survival Probability", placeholder = T), 
                                                  textInput("Tab5_KM_CNA_Legend_Title", "Legend Title:", "Legend", placeholder = T), 
                                                  tags$hr(), numericInput("Tab5_KM_CNA_Width", "Plot Width:", value=500, min=0, max=3000), 
                                                  numericInput("Tab5_KM_CNA_Height", "Plot Height:", value=500, min=0, max=3000), tags$hr(),
                                                  downloadButton('Tab5_Download_KM_CNA','Download Plot'))), 
                         box(title = ("Logrank Test"), collapsible = T, style = "overflow-y: hidden", width = 12, status = "primary", solidHeader = T, withSpinner(verbatimTextOutput("KMlogrank1")))), # Logrank Test
                 
                 # 3) Survival Analysis- Split on Specific Variable (Allows comparison between groups (only binary outcomes))
                 tabItem(tabName = "KMplotRadio", 
                         box(title = ("Kaplan-Meier Plot for Treatment - Yes"), style = "height:500px", height = "500px", solidHeader=T, width = 12, collapsible = T, status = "primary", withSpinner(plotOutput("KMR1", height = "465px")),
                             dropdownMenu = boxDropdown(icon = fa_i(name ="info-circle"), boxDropdownItem(
                                 HTML(paste(
                                     "Note: Chosen treatment variable must be coded as YES/NO i.e. either got treatment or did not.",
                                     sep="<br/>")), id = "dropdownItem1")), 
                             sidebar = boxSidebar(width = 25, id = "Tab5_KM_Treatment_Sidebar_Yes",  background = "#599740",  icon = shiny::icon("list-alt"), selectizeInput("Tab5_KM_Treatment_Survival_Time", "Survival Time:", choices = ""), selectizeInput("Tab5_KM_Treatment_Event_Status", "Event Status:", choices = ""),
                                                  selectizeInput("Tab5_KM_Treatment_Select_Variable", "Select Variable:", choices = ""), selectizeInput("Tab5_KM_Treatment_Variable", "Treatment Variable:", choices = ""), tags$hr(),
                                                  prettyToggle(inputId = "Tab5_KM_Treatment_Yes_Display_CI", label_on = "Display CI", label_off = "Display CI", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),  
                                                  prettyToggle(inputId = "Tab5_KM_Treatment_Yes_Display_Risk_Table", label_on = "Display Risk Table", label_off = "Display Risk Table", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                  prettyToggle(inputId = "Tab5_KM_Treatment_Yes_Display_Pval", label_on = "Display P-value", label_off = "Display P-value", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                  selectInput("Tab5_KM_Treatment_Yes_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right"), # Selection Option
                                                  tags$hr(), textInput("Tab5_KM_Treatment_Yes_Title", "Plot Title:", "Breast cancer patients in METABRIC data", placeholder = T),
                                                  textInput("Tab5_KM_Treatment_Yes_X_Axis_Title", "X-axis Title:", "Survival Time", placeholder = T), textInput("Tab5_KM_Treatment_Yes_Y_Axis_Title", "Y-axis Title:", "Survival Probability", placeholder = T), 
                                                  textInput("Tab5_KM_Treatment_Yes_Legend_Title", "Legend Title:", "Legend", placeholder = T), 
                                                  tags$hr(), numericInput("Tab5_KM_Treatment_Yes_Width", "Plot Width:", value=500, min=0, max=3000), 
                                                  numericInput("Tab5_KM_Treatment_Yes_Height", "Plot Height:", value=500, min=0, max=3000), tags$hr(),
                                                  downloadButton('Tab5_Download_KM_Treatment_Yes','Download Plot'))),
                         box(title = ("Kaplan-Meier Plot for Treatment - No"), style = "height:500px", height = "500px", solidHeader=T, width = 12, collapsible = T, status = "primary", withSpinner(plotOutput("KMR2", height = "465px")), 
                             sidebar = boxSidebar(width = 25, id = "Tab5_KM_Treatment_Sidebar_No",  background = "#599740",  icon = shiny::icon("list-alt"), 
                                                  prettyToggle(inputId = "Tab5_KM_Treatment_No_Display_CI", label_on = "Display CI", label_off = "Display CI", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),  
                                                  prettyToggle(inputId = "Tab5_KM_Treatment_No_Display_Risk_Table", label_on = "Display Risk Table", label_off = "Display Risk Table", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                  prettyToggle(inputId = "Tab5_KM_Treatment_No_Display_Pval", label_on = "Display P-value", label_off = "Display P-value", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                  selectInput("Tab5_KM_Treatment_No_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right"), # Selection Option
                                                  tags$hr(), textInput("Tab5_KM_Treatment_No_Title", "Plot Title:", "Breast cancer patients in METABRIC data", placeholder = T),
                                                  textInput("Tab5_KM_Treatment_No_X_Axis_Title", "X-axis Title:", "Survival Time", placeholder = T), textInput("Tab5_KM_Treatment_No_Y_Axis_Title", "Y-axis Title:", "Survival Probability", placeholder = T), 
                                                  textInput("Tab5_KM_Treatment_No_Legend_Title", "Legend Title:", "Legend", placeholder = T),
                                                  tags$hr(), numericInput("Tab5_KM_Treatment_No_Width", "Plot Width:", value=500, min=0, max=3000), 
                                                  numericInput("Tab5_KM_Treatment_No_Height", "Plot Height:", value=500, min=0, max=3000), tags$hr(),
                                                  downloadButton('Tab5_Download_KM_Treatment_No','Download Plot'))), # KM Plot Group 2
                         box(title = ("Logrank Test for Treatment - Yes"), solidHeader=T, width = 6, style = "overflow-y: hidden", status = "primary", collapsible = T, withSpinner(verbatimTextOutput("KMlogrankYes"))), # LRT Group 1
                         box(title = ("Logrank Test for Treatment - No"), solidHeader=T, width = 6,  style = "overflow-y: hidden",  status = "primary", collapsible = T, withSpinner(verbatimTextOutput("KMlogrankNo")))),  #LRT Group 2 # KM Plot options  
                 
                 tabItem(tabName = "ASTest", 
                         box(collapsible = T, title = "Association Tests", solidHeader = T, status = "primary", width = 12,
                             dropdownMenu = boxDropdown(icon = fa_i(name ="info-circle"), boxDropdownItem(
                                 HTML(paste("Please make sure you run the appropriate statistical tests for the question of interest,",
                                            "that all relevant assumptions are met and that you are aware of how to correctly interpret the output.",
                                            sep="<br/>")), id = "dropdownItemAssTests")), 
                             sidebar = boxSidebar(id="Tab6_Association_Test_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("list-alt"),
                                                  selectInput("Tab6_Select_Association_Test", "Association Test:", c("Chi-squared test", "Fishers exact test", "Simulated Fishers exact test", "ANOVA test", "Kruskal-Wallis test", "Pairwise t-test", "Dunns test"), width = "95%"), tags$hr(width = "93%"),
                                                  conditionalPanel(condition = "input.Tab6_Select_Association_Test == 'Chi-squared test' | input.Tab6_Select_Association_Test == 'Fishers exact test' | input.Tab6_Select_Association_Test == 'Simulated Fishers exact test'",
                                                                   selectizeInput("Tab6_Select_Categorical_Variable_1", "Select Categorical Variable 1:", choices = "", width = "95%"), 
                                                                   selectizeInput("Tab6_Select_Categorical_Variable_2", "Select Categorical Variables 2:", choices = "", multiple = T, width = "95%")),
                                                  conditionalPanel(condition = "input.Tab6_Select_Association_Test == 'ANOVA test' | input.Tab6_Select_Association_Test == 'Kruskal-Wallis test' | input.Tab6_Select_Association_Test == 'Pairwise t-test' | input.Tab6_Select_Association_Test == 'Dunns test'",
                                                                   selectizeInput("Tab6_Select_Categorical_Variable_3", "Select Categorical Variable:", choices = "", width = "95%"), 
                                                                   selectizeInput("Tab6_Select_Continuous_Variable_1", "Select Continuous Variables:", choices = "", width = "95%", multiple = T))),
                             
                             conditionalPanel(condition = "input.Tab6_Select_Association_Test == 'Chi-squared test'", h4(strong("Chi-Squared Test:")), # The null hypothesis of the Chi-Square test is that no relationship exists on the categorical variables in the population; they are independent.
                                              verbatimTextOutput("Cat1"), br(), h4(strong("Adjusted P-values:")), withSpinner(dataTableOutput("Cat1Ad"))),
                             
                             conditionalPanel(condition = "input.Tab6_Select_Association_Test == 'Fishers exact test'", h4(strong("Fisher's Exact Test:")), verbatimTextOutput("Cat3"), br(), h4(strong("Adjusted P-values:")), withSpinner(dataTableOutput("Cat3Ad"))),
                             conditionalPanel(condition = "input.Tab6_Select_Association_Test == 'Simulated Fishers exact test'", h4(strong("Simulated Fisher's Exact Test:")), verbatimTextOutput("Cat2"), br(), h4(strong("Adjusted P-values:")), withSpinner(dataTableOutput("Cat2Ad"))),
                             conditionalPanel(condition = "input.Tab6_Select_Association_Test == 'ANOVA test'",
                                              h4(strong("ANOVA Assumptions:")), h5(strong("Parametric Test for Equal Variance 1:")), verbatimTextOutput("ANOVAAss1"), h5(strong("Non-Parametric Test for Equal Variance 2:")), verbatimTextOutput("ANOVAAss2"), h5(strong("Test for Normality:")), verbatimTextOutput("ANOVAAss3"), # the null hypothesis for this test is that the population variances are equal
                                              h4(strong("ANOVA Test:")), verbatimTextOutput("ANOVA"), br(), h4(strong("Adjusted P-values:")), withSpinner(dataTableOutput("ANOVAAd"))), 
                             conditionalPanel(condition = "input.Tab6_Select_Association_Test == 'Kruskal-Wallis test'",                      
                                              h4(strong("Kruskal-Wallis Test:")), verbatimTextOutput("KW"), br(), h4(strong("Adjusted P-values:")), withSpinner(dataTableOutput("KWAd"))),
                             conditionalPanel(condition = "input.Tab6_Select_Association_Test == 'Pairwise t-test'", h4(strong("Pairwise Comparisons: t-test")), div(style="height:480px;", verbatimTextOutput("PC"))),
                             conditionalPanel(condition = "input.Tab6_Select_Association_Test == 'Dunns test'", h4(strong("Pairwise Comparisons: Dunn's Test")), div(style="height:480px;", verbatimTextOutput("Dunn"))))), 
                 
                 
                 ## Tab 7 - Cox PH models 
                 # Univariate and Multivariate Cox Tab with Assumptions:
                 # 1) Univariate Cox 
                 tabItem(tabName = "UniVar", fluidRow(
                     box(title = ("Univariate Cox Proportional Hazards Model"), collapsible = T, solidHeader = T,  width = 12, status = "primary", withSpinner(verbatimTextOutput("CoxModelOut", placeholder = T)), # Cox models OS
                         br(), h4(strong("Adjusted P-values:")), withSpinner(dataTableOutput("UniAdjusted")),
                         sidebar = boxSidebar(id="Tab7_Univariate_Cox_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("list-alt"), selectizeInput("Tab7_Univariate_Cox_Survival_Time", "Survival Time:", choices = "", width = "95%"), selectizeInput("Tab7_Univariate_Cox_Event_Status", "Event Status:", choices = "", width = "95%"),
                                              selectizeInput("Tab7_Univariate_Cox_Select_Variables", "Select Variable:", choices = "", multiple = T, width = "95%"))))), 
                 
                 # 2) Multivariable Cox Models 
                 tabItem(tabName = "MultiVar", fluidRow(
                     box(title = ("Multivariable Cox Proportional Hazards Model"), height = "500px", collapsible = T, solidHeader = T,  width = 12, status = "primary", withSpinner(verbatimTextOutput("CoxModelMultiOut", placeholder = T)), # Cox models OS
                         h5(strong("Likelihood Ratio Test:")), verbatimTextOutput("LRTid", placeholder = T),
                         h5(strong("Wald Test:")), verbatimTextOutput("Waldtestid", placeholder = T),
                         h5(strong("Logrank Test:")), verbatimTextOutput("Logrid", placeholder = T),
                         sidebar = boxSidebar(id="Tab7_Multivariable_Cox_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("list-alt"), selectizeInput("Tab7_Multivariable_Cox_Survival_Time", "Survival Time:", choices = "", width = "95%"), selectizeInput("Tab7_Multivariable_Cox_Event_Status", "Event Status:", choices = "", width = "95%"),
                                              selectizeInput("Tab7_Multivariable_Cox_Select_Variables", "Select Variable:", choices = "", multiple = T, width = "95%"), selectizeInput("Tab7_Multivariable_Cox_Select_Interaction_Variables_1", "Select 2-Way Interaction 1:", choices = "", multiple = T, width = "95%"),
                                              selectizeInput("Tab7_Multivariable_Cox_Select_Interaction_Variables_2", "Select 2-Way Interaction 2:", choices = "", multiple = T, width = "95%"), selectizeInput("Tab7_Multivariable_Cox_Select_Interaction_Variables_3", "Select 3-way Interaction:", choices = "", multiple = T, width = "95%"))))),
                 
                 # 3) Model  Assumptions
                 tabItem(tabName = "AssumptionsOS", 
                         fluidRow(box(title = ("Multivariable Cox PH Model Assumptions"), collapsible = T, solidHeader = T, width = 12, status = "primary",  
                                      sidebar = boxSidebar(id="Tab7_Cox_Assumptions_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("list-alt"), 
                                                           h5(strong("Options:")), prettyToggle(inputId = "Tab7_Cox_Assumptions_Display_by_Variable", label_on = "Display by Variable", label_off = "Display by Variable", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                           tags$hr(width = "96%"), numericInput("Tab7_Cox_Assumptions_Plot_Width", "Plot Width:", value=500, min=0, max=3000, width = "95%"), 
                                                           numericInput("Tab7_Cox_Assumptions_Plot_Height", "Plot Height:", value=500, min=0, max=3000,  width = "95%"), 
                                                           tags$hr(width = "96%"), downloadButton('Tab7_Download_Cox_Assumptions_Plot','Download Plot')),
                                      withSpinner(plotOutput("AssumptionsCox", height = "600px"))))), 
                 
                 
                 ## Tab 8: Adjusted Survival Curves (Based on Multivariable Cox Model Above)
                 tabItem(tabName = "AdjSurvival",
                         fluidRow(
                             box(collapsible = T, title = ("Multivariable Cox Model"), width = 12, solidHeader = T, status = "primary", withSpinner(verbatimTextOutput("Interaction"))),
                             box(title = ("New Data for Adjusted Survival Curves"), width = 12, solidHeader = T, collapsible = T, status = "primary", withSpinner(dataTableOutput("preddata")), 
                                 dropdownMenu = boxDropdown(icon = fa_i(name ="info-circle"), boxDropdownItem(
                                     HTML(paste("Please make sure you have included all variables present in the Cox Multivariable PH model in the New Dataframe")))),
                                 sidebar = boxSidebar(id="Tab8_New_Data_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("list-alt"), selectizeInput("Tab8_Adjusted_Curves_Select_Variable_1", "Select Model Variable 1:", choices = "", width = "95%"), selectizeInput("Tab8_Adjusted_Curves_Select_Variable_2", "Select Model Variable 2:", choices = "", width = "95%"), selectizeInput(inputId = "Tab8_Adjusted_Curves_Select_Constant_Variable", label = "Select Variables to Keep Constant:",  choices = "", multiple = TRUE, width = "95%"))), 
                             box(title = ("Adjusted Survival Curves"), collapsible = T, width = 12, solidHeader = T, status = "primary", 
                                 sidebar = boxSidebar(id="Tab8_Adjusted_Survival_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("list-alt"), h5(strong("Options:")),
                                                      prettyToggle(inputId = "Tab8_Adjusted_Curves_Display_CI", label_on = "Display CI", label_off = "Display CI", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),  
                                                      prettyToggle(inputId = "Tab8_Adjusted_Curves_Display_Risk_Table", label_on = "Display Risk Table", label_off = "Display Risk Table", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                      selectInput("Tab8_Adjusted_Curves_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right", width = "95%"), # Selection Option
                                                      tags$hr(width = "95%"), textInput("Tab8_Adjusted_Curves_Plot_Title", "Plot Title:", "Breast cancer patients in METABRIC data", placeholder = T, width = "95%"),
                                                      textInput("Tab8_Adjusted_Curves_X_Axis_Title", "X-axis Title:", "Survival Time", placeholder = T, width = "95%"), textInput("Tab8_Adjusted_Curves_Y_Axis_Title", "Y-axis Title:", "Survival Probability", placeholder = T, width = "95%"), 
                                                      textInput("Tab8_Adjusted_Curves_Legend_Title", "Legend Title:", "Legend", placeholder = T, width = "95%")),
                                 h4(strong("Adjusted Survival Curves:")), withSpinner(plotOutput("Pred1")), br(), h4(strong("Adjusted Survival Curves by Each Level of Selected Variable 1:")), withSpinner(uiOutput("Pred2"))),
                             box(title = ("Download Adjusted Survival Curves"), collapsible = T, width = 12, solidHeader = T, status = "primary", withSpinner(plotOutput("Pred3")), height = "430px", style = "height:430px",
                                 sidebar = boxSidebar(id="Tab8_Download_Adjusted_Survival_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("list-alt"), selectInput("Tab8_Download_Adjusted_Curves_Select", "Select Plot:", choices = "", width = "95%"), 
                                                      h5(strong("Options:")),
                                                      prettyToggle(inputId = "Tab8_Download_Adjusted_Curves_Display_CI", label_on = "Display CI", label_off = "Display CI", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),  
                                                      prettyToggle(inputId = "Tab8_Download_Adjusted_Curves_Display_Risk_Table", label_on = "Display Risk Table", label_off = "Display Risk Table", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                      selectInput("Tab8_Download_Adjusted_Curves_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right"), # Selection Option
                                                      tags$hr(), textInput("Tab8_Download_Adjusted_Curves_Plot_Title", "Plot Title:", "Breast cancer patients in METABRIC data", placeholder = T, width = "95%"),
                                                      textInput("Tab8_Download_Adjusted_Curves_X_Axis_Title", "X-axis Title:", "Survival Time", placeholder = T, width = "95%"), textInput("Tab8_Download_Adjusted_Curves_Y_Axis_Title", "Y-axis Title:", "Survival Probability", placeholder = T, width = "95%"), 
                                                      textInput("Tab8_Download_Adjusted_Curves_Legend_Title", "Legend Title:", "Legend", placeholder = T, width = "95%"), 
                                                      tags$hr(), numericInput("Tab8_Download_Adjusted_Curves_Plot_Width", "Plot Width:", value=500, min=0, max=3000, width = "95%"), 
                                                      numericInput("Tab8_Download_Adjusted_Curves_Plot_Height", "Plot Height:", value=500, min=0, max=3000,  width = "95%"), tags$hr(),
                                                      downloadButton('Tab8_Download_Adjusted_Curves_Plot','Download Plot'))))), 
                 
                 
                 ## Tab 9: Survival Trees - Rpart and Ctree algorithms 
                 # Rpart Tree
                 tabItem(tabName = "RpartTree",
                         fluidRow(box(collapsible = T, width = 12, status = "primary", height = "600px", title = "Rpart Survival Tree", solidHeader = TRUE, withSpinner(plotOutput("RpartTreePlot", height = "540px")), 
                                      dropdownMenu = boxDropdown(icon = fa_i(name ="info-circle"), boxDropdownItem(
                                          HTML(paste(
                                              "minsplit: the minimum number of observations that must exist in a node in order for a split to be attempted.", 
                                              "minbucket: the minimum number of observations in any terminal <leaf> node.", 
                                              "cp: complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.",
                                              "xval: number of cross-validations.",
                                              "maxdepth: Set the maximum depth of any node of the final tree, with the root node counted as depth 0.",
                                              sep="<br/>")), id = "dropdownItem1")), 
                                      sidebar = boxSidebar(id = "Tab9_Rpart_Tree_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("list-alt"),
                                                           selectizeInput("Tab9_Rpart_Survival_Time", "Survival Time:", choices =  ""), selectizeInput("Tab9_Rpart_Event_Status", "Event Status:", choices = ""), 
                                                           selectizeInput(inputId = "Tab9_Rpart_Select_Variables", label = "Select Variables:", choices = "", multiple = TRUE), tags$hr(),
                                                           prettyToggle(inputId = "Tab9_Rpart_Use_Complete_Cases_Only", label_on = "Complete Cases Only", label_off = "Complete Cases Only", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                           sliderInput(inputId = "Tab9_Rpart_Minsplit", label = "minsplit:", value = 20, max = 200, min = 0),
                                                           sliderInput(inputId = "Tab9_Rpart_Minbucket", label = "minbucket:", value = 20, min = 0, max = 200),
                                                           sliderInput(inputId = "Tab9_Rpart_Cp", label = "cp:", value = .01, min = 0.0001, max = 0.05),
                                                           sliderInput(inputId = "Tab9_Rpart_Xval", label = "xval:", value = 10, min = 0, max = 20),
                                                           sliderInput(inputId = "Tab9_Rpart_Maxdepth", label = "maxdepth:", value = 25, min = 0, max = 50),
                                                           tags$hr(), numericInput("Tab9_Rpart_Plot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                           numericInput("Tab9_Rpart_Plot_Height", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                           downloadButton("Tab9_Download_Rpart_Plot", 'Download Plot'))),
                                  box(collapsible = T, height = "550px", width = 12, status = "primary", title = "Corresponding Survival Curves", solidHeader = TRUE, withSpinner(plotOutput("Surv_Curve1", height = "510px")),
                                      sidebar = boxSidebar(id="Tab9_Rpart_Surv_Sidebar", width =25, background = "#599740",  icon = shiny::icon("list-alt"), 
                                                           h5(strong("Options:")),
                                                           prettyToggle(inputId = "Tab9_Surv_Rpart_Display_CI", label_on = "Display CI", label_off = "Display CI", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                           prettyToggle(inputId = "Tab9_Surv_Rpart_Display_Risk_Table", label_on = "Display Risk Table", label_off = "Display Risk Table", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                           prettyToggle(inputId = "Tab9_Surv_Rpart_Display_Pval", label_on = "Display P-value", label_off = "Display P-value", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), tags$hr(),
                                                           selectInput("Tab9_Surv_Rpart_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right"), tags$hr(), 
                                                           textInput("Tab9_Surv_Rpart_Plot_Title", "Plot Title:", "Breast cancer patients in METABRIC data", placeholder = T),
                                                           textInput("Tab9_Surv_Rpart_X_Axis_Title", "X-axis Title:", "Survival Time", placeholder = T), textInput("Tab9_Surv_Rpart_Y_Axis_Title", "Y-axis Title:", "Survival Probability", placeholder = T), 
                                                           textInput("Tab9_Surv_Rpart_Legend_Title", "Legend Title:", "Legend", placeholder = T),  
                                                           tags$hr(), numericInput("Tab9_Surv_Rpart_Plot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                           numericInput("Tab9_Surv_Rpart_Plot_Height", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                           downloadButton('Tab9_Download_Surv_Rpart_Plot','Download Plot'))))),
                 
                 # Ctree Tree
                 tabItem(tabName = "STreeCtree",
                         fluidRow(box(collapsible = T, width = 12, status = "primary", height = "600px", title = "Ctree Survival Tree", solidHeader = TRUE, withSpinner(plotOutput("CTreePlot", height = "540px")), 
                                      dropdownMenu = boxDropdown(icon = fa_i(name ="info-circle"), boxDropdownItem(
                                          HTML(paste(
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
                                              sep="<br/>")), id = "dropdownItem1")), 
                                      sidebar = boxSidebar(id = "Tab9_Ctree_Tree_Sidebar", width = 25, background = "#599740",  icon = shiny::icon("list-alt"), selectizeInput("Tab9_Ctree_Survival_Time", "Survival Time:", choices =  ""), selectizeInput("Tab9_Ctree_Event_Status", "Event Status:", choices = ""), 
                                                           selectizeInput(inputId = "Tab9_Ctree_Select_Variables", label = "Select Variables:", choices = "", multiple = TRUE), tags$hr(),
                                                           prettyToggle(inputId = "Tab9_Ctree_Use_Complete_Cases_Only", label_on = "Complete Cases Only", label_off = "Complete Cases Only", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                           selectInput(inputId = "Tab9_Ctree_Teststat", label = "teststat:", choices = c("quadratic", "maximum"), selected = "quadratic"),
                                                           selectInput(inputId = "Tab9_Ctree_Splitstat", label = "splitstat:", choices = c("quadratic", "maximum"), selected = "quadratic"),
                                                           selectInput(inputId = "Tab9_Ctree_Testtype", label = "testtype:", choices = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"), selected = "Bonferroni"),
                                                           sliderInput(inputId = "Tab9_Ctree_Alpha", label = "alpha:", value = 0.05, max = 1, min = 0),
                                                           sliderInput(inputId = "Tab9_Ctree_Minsplit", label = "minsplit:", value = 20, min = 0, max = 200),
                                                           sliderInput(inputId = "Tab9_Ctree_Minbucket", label = "minbucket:", value = 20, min = 0, max = 200),
                                                           sliderInput(inputId = "Tab9_Ctree_Minprob", label = "minprob:", value = .01, min = 0, max = 1),
                                                           sliderInput(inputId = "Tab9_Ctree_Maxvar", label = "maxvar:", value = 20, min = 0, max = 100),
                                                           sliderInput(inputId = "Tab9_Ctree_Maxdepth", label = "maxdepth:", value = 20, min = 0, max = 100),
                                                           prettyToggle(inputId = "Tab9_Ctree_Stump", label_on = "Stump", label_off = "Stump", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                           tags$hr(), numericInput("Tab9_Ctree_Plot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                           numericInput("Tab9_Ctree_Plot_Height", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                           downloadButton("Tab9_Download_Ctree_Plot", 'Download Plot'))),
                                  box(collapsible = T, width = 12, height = "550px", status = "primary", title = "Corresponding Survival Curves", solidHeader = TRUE, withSpinner(plotOutput("Surv_CurveCtree", height = "510px")), 
                                      sidebar = boxSidebar(id="Tab9_Ctree_Curv_Sidebar", width =25, background = "#599740",  icon = shiny::icon("list-alt"), h5(strong("Options:")),
                                                           prettyToggle(inputId = "Tab9_Surv_Ctree_Display_CI", label_on = "Display CI", label_off = "Display CI", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                           prettyToggle(inputId = "Tab9_Surv_Ctree_Display_Risk_Table", label_on = "Display Risk Table", label_off = "Display Risk Table", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                           prettyToggle(inputId = "Tab9_Surv_Ctree_Display_Pval", label_on = "Display P-value", label_off = "Display P-value", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), tags$hr(), 
                                                           selectInput("Tab9_Surv_Ctree_Legend_Position", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right"), tags$hr(), 
                                                           textInput("Tab9_Surv_Ctree_Plot_Title", "Plot Title:", "Breast cancer patients in METABRIC data", placeholder = T),
                                                           textInput("Tab9_Surv_Ctree_X_Axis_Title", "X-axis Title:", "Survival Time", placeholder = T), textInput("Tab9_Surv_Ctree_Y_Axis_Title", "Y-axis Title:", "Survival Probability", placeholder = T), 
                                                           textInput("Tab9_Surv_Ctree_Legend_Title", "Legend Title:", "Legend", placeholder = T), 
                                                           tags$hr(), numericInput("Tab9_Surv_Ctree_Plot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                           numericInput("Tab9_Surv_Ctree_Plot_Height", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                           downloadButton('Tab9_Download_Surv_Ctree_Plot','Download Plot'))))), 
                 
                 
                 # MAF Tabs: For exploration of mutation annotation format files 
                 tabItem(tabName = "MAFText", tabBox(height = "500px", width = 12, # MAF summaries (Text) 
                                                     tabPanel("MAF Summary", width = 12, status = "primary", solidHeader = TRUE, h5(strong("Basic summary of inputted Mutation/MAF file:")), withSpinner(verbatimTextOutput("MAF1"))), tabPanel("Sample Summary", width = 12, status = "primary", solidHeader = TRUE, h5(strong("Basic sample summary of inputted Mutation/MAF file:")), withSpinner(verbatimTextOutput("MAF2"))), 
                                                     tabPanel("Gene Summary", width = 12, status = "primary", solidHeader = TRUE, h5(strong("Basic gene summary of inputted Mutation/MAF file:")), withSpinner(verbatimTextOutput("MAF3"))), tabPanel("All Fields Summary", width = 12, status = "primary", solidHeader = TRUE, h5(strong("All fields in Mutation/MAF file:")), withSpinner(verbatimTextOutput("MAF4"))), 
                                                     tabPanel("Clinical Data", width = 12, status = "primary", solidHeader = TRUE, h5(strong("Clinical data associated with samples:")), withSpinner(verbatimTextOutput("MAFClin"))))),
                 
                 tabItem(tabName = "MAFVis", fluidRow(tabBox(width = 12, height = "1900px", # MAF summaries (Visual Plots)
                                                             tabPanel("MafSummary", box(collapsible = T, title=("MafSummary Plots"), height = "800px", width = 12, solidHeader = T, status = "primary", 
                                                                                        sidebar = boxSidebar(id="Tab10_MAF_Summary_Sidebar", width = 25,  background = "#599740",  icon = shiny::icon("list-alt"), h5(strong("Options:")), 
                                                                                                             prettyToggle(inputId = "Tab10_Summary_Remove_Outlier", label_on = "Remove Outliers", label_off = "Remove Outliers", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                                                                             prettyToggle(inputId = "Tab10_Summary_Dashboard_Style", label_on = "Dashboard Style", label_off = "Dashboard Style", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value=T),
                                                                                                             prettyToggle(inputId = "Tab10_Summary_Plot_Fraction", label_on = "Plot Fraction", label_off = "Plot Fraction", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                                                                             selectInput(inputId = "Tab10_Summary_Add_Stat", label = "Add Stat:", choices = c("mean", "median"), selected = ""), 
                                                                                                             sliderInput(inputId = "Tab10_Summary_Display_Top_Genes", label = "Display Top Genes:", value = 10, min=0, max=20),
                                                                                                             tags$hr(), numericInput("Tab10_Summary_Plot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                                                                             numericInput("Tab10_Summary_Plot_Height", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                                                                             downloadButton('Tab10_Download_Summary_Plot','Download Plot')),
                                                                                        withSpinner(plotOutput("summaryMAF", height = "720px")))),
                                                             tabPanel("OncoPlot/OncoStrip", box(collapsible = T, title = "OncoPlot", width = 12, solidHeader = T, status = "primary", style = "height:530px", height = "530px",
                                                                                                sidebar = boxSidebar(id="Tab10_OncoPlot_Sidebar", width = 25,  background = "#599740",  icon = shiny::icon("list-alt"), h5(strong("Options:")),
                                                                                                                     sliderInput(inputId = "Tab10_Oncoplot_Display_Top_Genes", label = "Display Top Genes:", value = 20, min=0, max=100), 
                                                                                                                     tags$hr(), numericInput("Tab10_Oncoplot_Plot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                                                                                     numericInput("Tab10_Oncoplot_Plot_Height", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                                                                                     downloadButton('Tab10_Download_Oncoplot_Plot','Download Plot')),
                                                                                                withSpinner(plotOutput("oncoplotMAF", height = "530px"))),
                                                                      box(collapsible = T, title = "Oncostrip Of Selected Genes", width  = 12, solidHeader = T, status = "primary", style = "height:530px", height = "530px",
                                                                          sidebar = boxSidebar(id="Tab10_Oncostrip_Sidebar", width = 25,  background = "#599740",  icon = shiny::icon("list-alt"), h5(strong("Options:")),
                                                                                               selectInput("Tab10_Oncostrip_Select_Gene_1", "Gene of Interest 1:", choices = ""), 
                                                                                               selectInput("Tab10_Oncostrip_Select_Gene_2", "Gene of Interest 2:", choices = ""),
                                                                                               selectInput("Tab10_Oncostrip_Select_Gene_3", "Gene of Interest 3:", choices = ""),
                                                                                               tags$hr(), numericInput("Tab10_Oncostrip_Plot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                                                               numericInput("Tab10_Oncostrip_Plot_Height", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                                                               downloadButton('Tab10_Download_Oncostrip_Plot','Download Plot')),
                                                                          withSpinner(plotOutput("oncostripMAF", height = "530px"))),
                                                                      box(collapsible = T, title = "Transition and Transversions", width = 12, solidHeader = T, status = "primary", style = "height:530px", height = "530px",
                                                                          sidebar = boxSidebar(id="Tab10_TT_Sidebar", width = 25,  background = "#599740",  icon = shiny::icon("list-alt"), h5(strong("Options:")),
                                                                                               prettyToggle(inputId = "Tab10_TT_Plot_Fraction", label_on = "Plot Fraction", label_off = "Plot Fraction", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                                                               prettyToggle(inputId = "Tab10_TT_Include_Synonymous_Variants", label_on = "Include Synonymous Variants", label_off = "Include Synonymous Variants", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), 
                                                                                               tags$hr(), numericInput("Tab10_TT_Plot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                                                               numericInput("Tab10_TT_Plot_Height", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                                                               downloadButton('Tab10_Download_TT_Plot','Download Plot')),
                                                                          withSpinner(plotOutput("TandT", height = "530px")))),
                                                             tabPanel("Lollipop Plots", fluidRow(box(collapsible = T, title = ("Choose Genes to Analyse"), width = 12, status = "primary",  solidHeader = TRUE,
                                                                                                     column(3, numericInput(inputId = "Tab10_Lollipop_Gene_Name_Column", label = "Gene Name Column:", value = 1, min=1)), column(3, selectInput("Tab10_Lollipop_Select_Gene_1", "Gene of Interest 1:", choices = "")),
                                                                                                     column(3, selectInput("Tab10_Lollipop_Select_Gene_2", "Gene of Interest 2:", choices = "")), column(3,selectInput("Tab10_Lollipop_Select_Gene_3", "Gene of Interest 3:", choices = ""))),
                                                                                                 box(collapsible = T, title = "Lollipop Plot 1", width = 12, solidHeader = T, status = "primary", height = "480px", withSpinner(plotOutput("lol1", height = "480px")), 
                                                                                                     sidebar = boxSidebar(id="Tab10_Lollipop_Plot1_Sidebar", width = 25, background = "#599740", icon = shiny::icon("list-alt"), 
                                                                                                                          h5(strong("Options:")),
                                                                                                                          selectInput("Tab10_Lollipop_Position_Label_1", "Positions to Label:", choices = c("all", "None"), selected = "None"),
                                                                                                                          sliderInput("Tab10_Lollipop_Label_Size_1", "Label Size:", value = 0.9, min=0, max=2, step=0.1),
                                                                                                                          sliderInput("Tab10_Lollipop_Label_Angle_1", "Label Angle:", value = 0, min=0, max=90), tags$hr(),
                                                                                                                          prettyToggle(inputId = "Tab10_Lollipop_Show_Mutation_Rate_1", label_on = "Show Somatic Mutation Rate", label_off = "Show Somatic Mutation Rate", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T),
                                                                                                                          prettyToggle(inputId = "Tab10_Lollipop_Label_Domains_1", label_on = "Label Domains", label_off = "Label Domains", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T),
                                                                                                                          sliderInput("Tab10_Lollipop_Label_Domains_Size_1", "Domain Label Size:", value = 0.8, min=0, max=2, step=0.1), tags$hr(),
                                                                                                                          prettyToggle(inputId = "Tab10_Lollipop_Repel_Yes_or_No_1", label_on = "Repel", label_off = "Repel", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = F),
                                                                                                                          prettyToggle(inputId = "Tab10_Lollipop_Show_Legend_1", label_on = "Show Legend", label_off = "Show Legend", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T),
                                                                                                                          sliderInput("Tab10_Lollipop_Size_Legend_1", "Legend Text Size:", value = 0.8, min=0, max=2, step=0.1),
                                                                                                                          tags$hr(), numericInput("Tab10_Lollipop_Plot_Width_1", "Plot Width:", value=500, min=100, max=3000), 
                                                                                                                          numericInput("Tab10_Lollipop_Plot_Height_1", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                                                                                          downloadButton('Tab10_Download_Lollipop_Plot_1','Download Plot'))),
                                                                                                 
                                                                                                 box(collapsible = T, title = "Lollipop Plot 2", width  = 12, solidHeader = T, status = "primary", height = "480px", withSpinner(plotOutput("lol2", height = "480px")), 
                                                                                                     sidebar = boxSidebar(id="Tab10_Lollipop_Plot2_Sidebar", width = 25, background = "#599740", icon = shiny::icon("list-alt"), 
                                                                                                                          h5(strong("Options:")),
                                                                                                                          selectInput("Tab10_Lollipop_Position_Label_2", "Positions to Label:", choices = c("all", "None"), selected = "None"),
                                                                                                                          sliderInput("Tab10_Lollipop_Label_Size_2", "Label Size:", value = 0.9, min=0, max=2, step=0.1),
                                                                                                                          sliderInput("Tab10_Lollipop_Label_Angle_2", "Label Angle:", value = 0, min=0, max=90), tags$hr(),
                                                                                                                          prettyToggle(inputId = "Tab10_Lollipop_Show_Mutation_Rate_2", label_on = "Show Somatic Mutation Rate", label_off = "Show Somatic Mutation Rate", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T),
                                                                                                                          prettyToggle(inputId = "Tab10_Lollipop_Label_Domains_2", label_on = "Label Domains", label_off = "Label Domains", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T),
                                                                                                                          sliderInput("Tab10_Lollipop_Label_Domains_Size_2", "Domain Label Size:", value = 0.8, min=0, max=2, step=0.1), tags$hr(),
                                                                                                                          prettyToggle(inputId = "Tab10_Lollipop_Repel_Yes_or_No_2", label_on = "Repel", label_off = "Repel", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = F),
                                                                                                                          prettyToggle(inputId = "Tab10_Lollipop_Show_Legend_2", label_on = "Show Legend", label_off = "Show Legend", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T),
                                                                                                                          sliderInput("Tab10_Lollipop_Size_Legend_2", "Legend Text Size:", value = 0.8, min=0, max=2, step=0.1),
                                                                                                                          tags$hr(), numericInput("Tab10_Lollipop_Plot_Width_2", "Plot Width:", value=500, min=100, max=3000), 
                                                                                                                          numericInput("Tab10_Lollipop_Plot_Height_2", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                                                                                          downloadButton('Tab10_Download_Lollipop_Plot_2','Download Plot'))),
                                                                                                 
                                                                                                 box(collapsible = T, title = "Lollipop Plot 3", width = 12, solidHeader = T, status = "primary", withSpinner(plotOutput("lol3", height = "480px")), height="480px", 
                                                                                                     sidebar = boxSidebar(id="Tab10_Lollipop_Plot3_Sidebar", width = 25, background = "#599740", icon = shiny::icon("list-alt"), 
                                                                                                                          h5(strong("Options:")),
                                                                                                                          selectInput("Tab10_Lollipop_Position_Label_3", "Positions to Label:", choices = c("all", "None"), selected = "None"),
                                                                                                                          sliderInput("Tab10_Lollipop_Label_Size_3", "Label Size:", value = 0.9, min=0, max=2, step=0.1),
                                                                                                                          sliderInput("Tab10_Lollipop_Label_Angle_3", "Label Angle:", value = 0, min=0, max=90), tags$hr(),
                                                                                                                          prettyToggle(inputId = "Tab10_Lollipop_Show_Mutation_Rate_3", label_on = "Show Somatic Mutation Rate", label_off = "Show Somatic Mutation Rate", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T),
                                                                                                                          prettyToggle(inputId = "Tab10_Lollipop_Label_Domains_3", label_on = "Label Domains", label_off = "Label Domains", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T),
                                                                                                                          sliderInput("Tab10_Lollipop_Label_Domains_Size_3", "Domain Label Size:", value = 0.8, min=0, max=2, step=0.1), tags$hr(),
                                                                                                                          prettyToggle(inputId = "Tab10_Lollipop_Repel_Yes_or_No_3", label_on = "Repel", label_off = "Repel", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = F),
                                                                                                                          prettyToggle(inputId = "Tab10_Lollipop_Show_Legend_3", label_on = "Show Legend", label_off = "Show Legend", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T),
                                                                                                                          sliderInput("Tab10_Lollipop_Size_Legend_3", "Legend Text Size:", value = 0.8, min=0, max=2, step=0.1),
                                                                                                                          tags$hr(), numericInput("Tab10_Lollipop_Plot_Width_3", "Plot Width:", value=500, min=100, max=3000), 
                                                                                                                          numericInput("Tab10_Lollipop_Plot_Height_3", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                                                                                          downloadButton('Tab10_Download_Lollipop_Plot_3','Download Plot'))))),
                                                             # Other Plots  
                                                             tabPanel("Other Plots", id = "tabsetMAFother", box(collapsible = T, title = "Mutation Load Plot", width = 12, solidHeader = T, status = "primary", withSpinner(plotOutput("Mutload", height = "500px")), height = "580px",
                                                                                                                sidebar = boxSidebar(id="Tab10_Mutation_Load_Sidebar", width = 25, background = "#599740", icon = shiny::icon("list-alt"), h5(strong("Options:")), 
                                                                                                                                     numericInput("Tab10_Mutation_Load_Plot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                                                                                                     numericInput("Tab10_Mutation_Load_Plot_Height", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                                                                                                     downloadButton('Tab10_Download_Mutation_Load_Plot','Download Plot'))),
                                                                      box(collapsible = T, title = "Somatic Interaction Plot", width  = 12, solidHeader = T, status = "primary", withSpinner(plotOutput("VAF1", height = "500px")), height="540px",
                                                                          sidebar = boxSidebar(id="Tab10_Somatic_Interaction_Sidebar", width = 25, background = "#599740", icon = shiny::icon("list-alt"), h5(strong("Options:")), 
                                                                                               sliderInput("Tab10_SIP_Display_Top_Genes", "Top Genes:", value = 25, min=0, max=100, step=5),
                                                                                               sliderInput("Tab10_SIP_Pval_Lower_Threshold", "P-value Lower Threshold:", value = 0.01, min=0, max=0.5),
                                                                                               sliderInput("Tab10_SIP_Pval_Upper_Threshold", "P-value Upper Threshold:", value = 0.05, min=0, max=0.5), tags$hr(),
                                                                                               numericInput("Tab10_SIP_Plot_Width", "Plot Width:", value=500, min=100, max=3000), 
                                                                                               numericInput("Tab10_SIP_Plot_Height", "Plot Height:", value=500, min=100, max=3000), tags$hr(),
                                                                                               downloadButton('Tab10_Download_SIP_Plot','Download Plot'))))))), 
                 
                 # Tab 11 Download Log
                 tabItem(tabName = "downlog",
                         fluidRow(box(collapsible = T, title = "Preview of Input Logs" , width = 12, status = "primary", solidHeader = TRUE, height = "500px",
                                      DT::dataTableOutput("InputLog"), 
                                      sidebar = boxSidebar(id="Tab11_Log_Options", width = 25, background = "#599740", icon = shiny::icon("list-alt"), 
                                                           h4(strong("Dataframe Options")),
                                                           selectInput("Tab11_Order_Log_By", "Order by:", choices = c("Timestamp", "Tab", "Name", "Type", "Value", "Binding"), selected = c("Timestamp"), multiple = T),
                                                           prettyToggle(inputId = "Tab11_Remove_None_Log", label_on = "Remove NULL/None Selected", label_off = "Remove NULL/None Selected", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times"), value = T),
                                                           awesomeCheckboxGroup(inputId = "Tab11_Display_Type", label = "Display:", choices = c('Number Input' = "shiny.numberInput",'Select Input' = "shiny.selectInput",'File Input' = "shiny.fileInputBinding",'Slider Input' = "shiny.sliderInput", 'Checkbox Input' = "shiny.checkboxInput", 'Box Sidebar Input' = "box-sidebar-input", 'Text Input' = "shiny.textInput", 'Radio Button Input' = "shinyWidgets.awesomeRadio", 'Main Sidebar Input' = c("shinydashboard.sidebarCollapsedInputBinding", "shinydashboard.sidebarmenuExpandedInputBinding")),
                                                                                selected = c("box-sidebar-input", "shiny.selectInput", "shiny.numberInput", "shiny.fileInputBinding", "shiny.sliderInput","shiny.checkboxInput","shiny.textInput", "shinyWidgets.awesomeRadio")),
                                                           tags$hr(),
                                                           awesomeRadio(inputId = "Tab11_Download_Log_Separator", label = "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t", inline = F, status = "primary"), # Input: Select Separator:
                                                           prettyToggle(inputId = "Tab11_Download_Log_Quote", label_on = "Include Quotes", label_off = "Include Quotes", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")),
                                                           prettyToggle(inputId = "Tab11_Download_Log_Row_Names", label_on = "Include Row Names", label_off = "Include Row Names", icon_on = fa_i(name = "check"), icon_off = fa_i(name = "times")), tags$hr(),
                                                           downloadButton('download_button_rscript', 'Download R Script'), br(), br(),
                                                           downloadButton('Tab11_Download_Log','Download Input Log')))))
                 
        )
    )
)