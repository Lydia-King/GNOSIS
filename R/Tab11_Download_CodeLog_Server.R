# Server function to download log
Tab11_Log_Server <- function(id, data_inputs) {
    moduleServer(id, function(input, output, session) {
        shinylogs::track_usage(
            storage_mode = store_null(console = TRUE),
            what = "input",
            exclude_input_regex = "tab11",
            exclude_input_id = c("InputLog")
        )

        log_vec <- reactive({
            data_inputs()
        })

        Log_DataFrame <- reactive({
            if (is.null(log_vec())) {
                df <- data.frame(
                    Timestamp = character(),
                    Tab = character(),
                    Name = character(),
                    Value = character(),
                    Binding = character(),
                    stringsAsFactors = FALSE
                )
                return(df)
            } else {
                temp_dataframe <- do.call(
                    rbind,
                    data_inputs()$inputs
                )
                temp_dataframe <- as.data.frame(temp_dataframe) %>%
                    rename(
                        "Name" = name,
                        "Timestamp" = timestamp,
                        "Value" = value,
                        "Type" = type,
                        "Binding" = binding
                    ) %>%
                    mutate(
                        Tab = gsub(".*?(tab[0-9]+).*", "\\1", Name),
                        Tab = str_to_title(Tab),
                        Tab = gsub(".*?(Tab[0-9]+).*", "\\1", Tab),
                        Timestamp = gsub("\\..*", "", Timestamp)
                    ) %>%
                    select(Timestamp, Name, Tab, Binding, Value) %>%
                    mutate(Value = ifelse(
                        Name %in% c(
                            "tab1_input_manually-Input_Patient_File",
                            "tab1_input_manually-Input_Sample_File",
                            "tab1_input_manually-Input_CNA_File",
                            "tab1_input_manually-Input_MAF_File"
                        ),
                        "uploaded file",
                        Value
                    ))

                temp_dataframe <- temp_dataframe %>%
                    mutate(Tab = ifelse(
                        Tab %in% c(
                            "Tab1",
                            "Tab2",
                            "Tab3",
                            "Tab4",
                            "Tab5",
                            "Tab6",
                            "Tab7",
                            "Tab8",
                            "Tab9",
                            "Tab10",
                            "Tab11"
                        ),
                        Tab,
                        "Other"
                    ))

                temp_dataframe$Tab <- factor(
                    temp_dataframe$Tab,
                    levels = c(
                        "Tab1",
                        "Tab2",
                        "Tab3",
                        "Tab4",
                        "Tab5",
                        "Tab6",
                        "Tab7",
                        "Tab8",
                        "Tab9",
                        "Tab10",
                        "Tab11",
                        "Other"
                    )
                )

                if (input$Tab11_Remove_None_Log == TRUE) {
                    temp_dataframe <- temp_dataframe %>%
                        filter(
                            Value %!in% c(
                                "",
                                "NULL",
                                "None Selected",
                                'list("None Selected")',
                                "list()"
                            )
                        ) %>%
                        filter(!is.null(Value))
                }

                temp_dataframe <- temp_dataframe %>%
                    filter(Binding %in% c(input$Tab11_Display_Type))
                temp_dataframe$Binding <-
                    gsub(
                        "shinydashboard.",
                        "",
                        temp_dataframe$Binding
                    )
                temp_dataframe$Binding <-
                    gsub("shinyWidgets.", "", temp_dataframe$Binding)
                temp_dataframe$Binding <-
                    gsub("shiny.", "", temp_dataframe$Binding)

                log_list <- c(input$Tab11_Order_Log_By)
                temp_dataframe_order <- temp_dataframe %>%
                    arrange_at(.vars = log_list)

                return(temp_dataframe_order)
            }
        })

        output$InputLog <- DT::renderDataTable(
            {
                Log_DataFrame()
            },
            options = list(
                lengthMenu = c(10, 30, 50, 100),
                pageLength = 30,
                scrollX = TRUE,
                scrollY = "650px",
                selector = "td:not(.not-selectable)"
            )
        )

        # Download File
        output$Tab11_Download_Log <- downloadHandler(
            "Shiny_Log.txt",
            content = function(file) {
                Table_dl <- apply(Log_DataFrame(), 2, as.character)
                write.table(
                    Table_dl,
                    file,
                    sep = input$Tab11_Download_Log_Separator,
                    quote = input$Tab11_Download_Log_Quote,
                    row.names = input$Tab11_Download_Log_Row_Names
                )
            }
        )
    })
}
