# Server function for tables to explore and compare input files
Tab2_Exploratory_Tables_Server <-
    function(id, datalist, data, length_px, select_dt) {
        moduleServer(id, function(input, output, session) {
            track_usage(store_null(console = FALSE),
                what = c("input")
            )

            observe({
                vchoices <- c(names(datalist[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab2_Column1_Variable",
                    choices = vchoices,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab2_Column2_Variable",
                    choices = vchoices,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab2_Column3_Variable",
                    choices = vchoices,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab2_Column4_Variable",
                    choices = vchoices,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab2_Column5_Variable",
                    choices = vchoices,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            Create_Table <- metaReactive2({
                if (input$Tab2_Column1_Variable == "None Selected" &
                    input$Tab2_Column2_Variable == "None Selected" &
                    input$Tab2_Column3_Variable == "None Selected" &
                    input$Tab2_Column4_Variable == "None Selected" &
                    input$Tab2_Column5_Variable == "None Selected") {
                    return(
                        validate(
                            "Please upload data and select data columns in sequential order."
                        )
                    )
                } else if (input$Tab2_Column1_Variable != "None Selected" &
                    input$Tab2_Column2_Variable == "None Selected" &
                    input$Tab2_Column3_Variable == "None Selected" &
                    input$Tab2_Column4_Variable == "None Selected" &
                    input$Tab2_Column5_Variable == "None Selected") {
                    metaExpr({
                        as_tibble(..(datalist[[data]]())[, c(..(input$Tab2_Column1_Variable))]) %>%
                            mutate(!!..(input$Tab2_Column1_Variable) := value) %>%
                            select(!!..(input$Tab2_Column1_Variable))
                    })
                } else if (input$Tab2_Column1_Variable != "None Selected" &
                    input$Tab2_Column2_Variable != "None Selected" &
                    input$Tab2_Column3_Variable == "None Selected" &
                    input$Tab2_Column4_Variable == "None Selected" &
                    input$Tab2_Column5_Variable == "None Selected") {
                    metaExpr({
                        ..(datalist[[data]]())[, c(
                            ..(input$Tab2_Column1_Variable),
                            ..(input$Tab2_Column2_Variable)
                        )]
                    })
                } else if (input$Tab2_Column1_Variable != "None Selected" &
                    input$Tab2_Column2_Variable != "None Selected" &
                    input$Tab2_Column3_Variable != "None Selected" &
                    input$Tab2_Column4_Variable == "None Selected" &
                    input$Tab2_Column5_Variable == "None Selected") {
                    metaExpr({
                        ..(datalist[[data]]())[, c(
                            ..(input$Tab2_Column1_Variable),
                            ..(input$Tab2_Column2_Variable),
                            ..(input$Tab2_Column3_Variable)
                        )]
                    })
                } else if (input$Tab2_Column1_Variable != "None Selected" &
                    input$Tab2_Column2_Variable != "None Selected" &
                    input$Tab2_Column3_Variable != "None Selected" &
                    input$Tab2_Column4_Variable != "None Selected" &
                    input$Tab2_Column5_Variable == "None Selected") {
                    metaExpr({
                        ..(datalist[[data]]())[, c(
                            ..(input$Tab2_Column1_Variable),
                            ..(input$Tab2_Column2_Variable),
                            ..(input$Tab2_Column3_Variable),
                            ..(input$Tab2_Column4_Variable)
                        )]
                    })
                } else if (input$Tab2_Column1_Variable != "None Selected" &
                    input$Tab2_Column2_Variable != "None Selected" &
                    input$Tab2_Column3_Variable != "None Selected" &
                    input$Tab2_Column4_Variable != "None Selected" &
                    input$Tab2_Column5_Variable != "None Selected") {
                    metaExpr({
                        ..(datalist[[data]]())[, c(
                            ..(input$Tab2_Column1_Variable),
                            ..(input$Tab2_Column2_Variable),
                            ..(input$Tab2_Column3_Variable),
                            ..(input$Tab2_Column4_Variable),
                            ..(input$Tab2_Column5_Variable)
                        )]
                    })
                } else {
                    validate("Please upload data and select data columns in sequential order.")
                }
            })

            output$Table <- metaRender(renderDataTable, {
                datatable(
                    ..(Create_Table()),
                    selection = select_dt,
                    options = list(
                        lengthMenu = c(10, 30, 50, 100),
                        pageLength = 30,
                        scrollX = TRUE,
                        scrollY = length_px
                    )
                )
            })
        })
    }
