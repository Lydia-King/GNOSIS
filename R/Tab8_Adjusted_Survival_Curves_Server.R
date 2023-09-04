# Server function to produce adjusted survival curves
Tab8_Adjusted_Survival_Curves_Server <-
    function(id,
             datalist,
             datalist1,
             data,
             rowselect,
             previous_model) {
        moduleServer(id, function(input, output, session) {
            ns <- session$ns

            output$Interaction <- metaRender(renderPrint, {
                ..(summary(previous_model()))
            })

            observe({
                vchoicesSurvcox <- c(names(datalist1[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab8_Adjusted_Curves_Select_Constant_Variable",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab8_Adjusted_Curves_Select_Variable_1",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab8_Adjusted_Curves_Select_Variable_2",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            NewData <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n Please select cBioPortal dataset or upload your own clinical file,",
                        "sample file and/or CNA file.",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    c(
                        is.null(datalist[["patient_manual_data"]]()) &
                            is.null(datalist[["sample_manual_data"]]()) &
                            is.null(datalist[["CNA_manual_data"]]())
                    ) | is.null(rowselect()),
                    paste(
                        "\n \n Please only select cBioPortal dataset OR upload your own data.",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab8_Adjusted_Curves_Select_Variable_1) &
                            !is.null(input$Tab8_Adjusted_Curves_Select_Variable_2) &
                            !is.null(
                                input$Tab8_Adjusted_Curves_Select_Constant_Variable
                            ),
                        "Please make sure all select boxes are filled."
                    )
                )

                if (isTRUE(
                    is.null(datalist1[[data]]()) |
                        input$Tab8_Adjusted_Curves_Select_Variable_1 == "None Selected" |
                        input$Tab8_Adjusted_Curves_Select_Variable_2 == "None Selected"
                )) {
                    data.frame(
                        Variable_1 = character(),
                        Variable_2 = character(),
                        Variable_3 = character()
                    )
                } else if (isTRUE(
                    input$Tab8_Adjusted_Curves_Select_Variable_1 != "None Selected" &
                        input$Tab8_Adjusted_Curves_Select_Variable_2 != "None Selected" &
                        input$Tab8_Adjusted_Curves_Select_Constant_Variable == "None Selected"
                )) {
                    metaExpr({
                        Levels_Vector_1 <- Levels_Rep <- c()

                        for (n_levels_1 in 1:length(levels(as.factor(..(
                            datalist1[[data]]()
                        )[, ..(input$Tab8_Adjusted_Curves_Select_Variable_2)])))) {
                            Levels_Vector_1 <- c(
                                Levels_Vector_1,
                                c(rep(
                                    levels(as.factor(
                                        ..(datalist1[[data]]())[, ..(input$Tab8_Adjusted_Curves_Select_Variable_2)]
                                    ))[n_levels_1],
                                    length(levels(
                                        as.factor(..(datalist1[[data]](

                                        ))[, ..(input$Tab8_Adjusted_Curves_Select_Variable_1)])
                                    ))
                                ))
                            )
                        }

                        for (n_levels_2 in 1:length(levels(as.factor(..(
                            datalist1[[data]]()
                        )[, ..(input$Tab8_Adjusted_Curves_Select_Variable_1)])))) {
                            Levels_Rep <- c(
                                Levels_Rep,
                                levels(as.factor(
                                    ..(datalist1[[data]]())[, ..(input$Tab8_Adjusted_Curves_Select_Variable_1)]
                                ))[n_levels_2]
                            )
                        }

                        Levels_Vector_2 <-
                            c(rep(Levels_Rep, length(levels(
                                as.factor(..(datalist1[[data]](

                                ))[, ..(input$Tab8_Adjusted_Curves_Select_Variable_2)])
                            ))))

                        Lev <-
                            data.frame("Row" = 1:length(Levels_Vector_2)) %>%
                            mutate(
                                !!..(
                                    input$Tab8_Adjusted_Curves_Select_Variable_1
                                ) := Levels_Vector_2,
                                !!..(
                                    input$Tab8_Adjusted_Curves_Select_Variable_2
                                ) := Levels_Vector_1
                            ) %>%
                            select(
                                !!..(
                                    input$Tab8_Adjusted_Curves_Select_Variable_1
                                ),
                                !!..(
                                    input$Tab8_Adjusted_Curves_Select_Variable_2
                                )
                            )
                        Lev
                    })
                } else {
                    metaExpr({
                        Levels_Vector_1 <- Levels_Rep <- c()

                        for (n_levels_1 in 1:length(levels(as.factor(..(
                            datalist1[[data]]()
                        )[, ..(input$Tab8_Adjusted_Curves_Select_Variable_2)])))) {
                            Levels_Vector_1 <- c(
                                Levels_Vector_1,
                                c(rep(
                                    levels(as.factor(
                                        ..(datalist1[[data]]())[, ..(input$Tab8_Adjusted_Curves_Select_Variable_2)]
                                    ))[n_levels_1],
                                    length(levels(
                                        as.factor(..(datalist1[[data]](

                                        ))[, ..(input$Tab8_Adjusted_Curves_Select_Variable_1)])
                                    ))
                                ))
                            )
                        }

                        for (n_levels_2 in 1:length(levels(as.factor(..(
                            datalist1[[data]]()
                        )[, ..(input$Tab8_Adjusted_Curves_Select_Variable_1)])))) {
                            Levels_Rep <- c(
                                Levels_Rep,
                                levels(as.factor(
                                    ..(datalist1[[data]]())[, ..(input$Tab8_Adjusted_Curves_Select_Variable_1)]
                                ))[n_levels_2]
                            )
                        }

                        Levels_Vector_2 <-
                            c(rep(Levels_Rep, length(levels(
                                as.factor(..(datalist1[[data]](

                                ))[, ..(input$Tab8_Adjusted_Curves_Select_Variable_2)])
                            ))))

                        Lev <-
                            data.frame("Row" = 1:length(Levels_Vector_2)) %>%
                            mutate(
                                !!..(
                                    input$Tab8_Adjusted_Curves_Select_Variable_1
                                ) := Levels_Vector_2,
                                !!..(
                                    input$Tab8_Adjusted_Curves_Select_Variable_2
                                ) := Levels_Vector_1
                            ) %>%
                            select(
                                !!..(
                                    input$Tab8_Adjusted_Curves_Select_Variable_1
                                ),
                                !!..(
                                    input$Tab8_Adjusted_Curves_Select_Variable_2
                                )
                            )

                        for (constant_var in 1:length(..(
                            input$Tab8_Adjusted_Curves_Select_Constant_Variable
                        ))) {
                            varname <-
                                ..(input$Tab8_Adjusted_Curves_Select_Constant_Variable)[constant_var]
                            if (is.factor(..(datalist1[[data]]())[, varname]) |
                                is.character(..(datalist1[[data]]())[, varname])) {
                                Lev[, c(varname)] <-
                                    getmode(..(datalist1[[data]]())[, varname])
                            } else {
                                Lev <- mutate(
                                    Lev,
                                    !!varname := mean(..(
                                        datalist1[[data]]()
                                    )[, varname], na.rm = TRUE)
                                )
                            }
                        }
                        Lev
                    })
                }
            })

            output$preddata <- metaRender(renderDataTable, {
                DT::datatable(
                    ..(NewData()),
                    options = list(
                        lengthMenu = c(10, 30, 50, 100),
                        pageLength = 30,
                        scrollX = TRUE,
                        scrollY = "270px"
                    )
                )
            })

            outVar <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n Please select cBioPortal dataset or upload your own clinical file,",
                        "sample file and/or CNA file.",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    c(
                        is.null(datalist[["patient_manual_data"]]()) &
                            is.null(datalist[["sample_manual_data"]]()) &
                            is.null(datalist[["CNA_manual_data"]]())
                    ) | is.null(rowselect()),
                    paste(
                        "\n \n Please only select cBioPortal dataset OR upload your own data.",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab8_Adjusted_Curves_Select_Variable_1) &
                            !is.null(input$Tab8_Adjusted_Curves_Select_Variable_2) &
                            !is.null(
                                input$Tab8_Adjusted_Curves_Select_Constant_Variable
                            ),
                        "Please make sure all select boxes are filled."
                    )
                )

                if (input$Tab8_Adjusted_Curves_Select_Variable_1 == "None Selected" |
                    is.null(input$Tab8_Adjusted_Curves_Select_Variable_1)) {
                    metaExpr({
                        c("None Selected")
                    })
                } else {
                    metaExpr({
                        validate(
                            need(
                                input$Tab8_Adjusted_Curves_Select_Variable_1 %in% colnames(datalist1[[data]]()),
                                "Please make sure data is loaded correctly."
                            )
                        )
                        Data <- ..(datalist1[[data]]())
                        mydata <-
                            ..(input$Tab8_Adjusted_Curves_Select_Variable_1)
                        lev <-
                            c(levels(as.factor(Data[, c(mydata)])), "None Selected")
                        lev
                    })
                }
            })

            observe({
                Adjchoice <- c("All Levels", outVar())
                updateSelectInput(
                    session,
                    "Tab8_Download_Adjusted_Curves_Select",
                    choices = Adjchoice,
                    selected = "None Selected"
                )
            })

            OutVarPlot <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "Please select cBioPortal dataset or upload your own clinical file,",
                        "sample file and/or CNA file.",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    c(
                        is.null(datalist[["patient_manual_data"]]()) &
                            is.null(datalist[["sample_manual_data"]]()) &
                            is.null(datalist[["CNA_manual_data"]]())
                    ) | is.null(rowselect()),
                    paste(
                        "Please only select cBioPortal dataset OR upload your own data.",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab8_Adjusted_Curves_Select_Variable_1) &
                            !is.null(input$Tab8_Adjusted_Curves_Select_Variable_2) &
                            !is.null(
                                input$Tab8_Adjusted_Curves_Select_Constant_Variable
                            ),
                        "Please make sure all select boxes are filled."
                    )
                )

                if (input$Tab8_Download_Adjusted_Curves_Select == "All Levels") {
                    metaExpr({
                        Data <- ..(NewData())
                        rownames(Data) <- do.call(paste, c(Data[c(
                            ..(
                                input$Tab8_Adjusted_Curves_Select_Variable_1
                            ),
                            ..(
                                input$Tab8_Adjusted_Curves_Select_Variable_2
                            )
                        )], sep = "_"))
                        fit <-
                            survival::survfit(..(previous_model()), newdata = Data)
                        survminer::ggsurvplot(
                            fit,
                            data = Data,
                            censor.shape = "",
                            xlab = ..(
                                input$Tab8_Download_Adjusted_Curves_X_Axis_Title
                            ),
                            ylab = ..(
                                input$Tab8_Download_Adjusted_Curves_Y_Axis_Title
                            ),
                            size = 1,
                            conf.int = ..(
                                input$Tab8_Download_Adjusted_Curves_Display_CI
                            ),
                            risk.table = ..(
                                input$Tab8_Download_Adjusted_Curves_Display_Risk_Table
                            ),
                            legend = c(
                                ..(
                                    input$Tab8_Download_Adjusted_Curves_Legend_Position
                                )
                            ),
                            legend.labs = rownames(summary(fit$table)),
                            risk.table.height = 0.25,
                            pval.size = 6,
                            ggtheme = theme_bw() +
                                theme(plot.title = element_text(
                                    size = 18, hjust = 0.5
                                )) +
                                theme(
                                    legend.title = element_text(
                                        colour = "black",
                                        size = 15,
                                        face = "bold"
                                    )
                                ),
                            break.time.by = 50,
                            risk.table.y.text.col = TRUE,
                            risk.table.y.text = FALSE,
                            legend.title = ..(
                                input$Tab8_Download_Adjusted_Curves_Legend_Title
                            ),
                            title = (
                                ..(
                                    input$Tab8_Download_Adjusted_Curves_Plot_Title
                                )
                            ),
                            font.main = c(18, "plain", "black"),
                            font.x = c(15, "plain", "black"),
                            font.y = c(15, "plain", "black"),
                            font.legend = c(14, "plain", "black"),
                            font.tickslab = c(12, "plain", "black")
                        )
                    })
                } else if (input$Tab8_Download_Adjusted_Curves_Select == "None Selected") {
                    ggplot() +
                        theme_void()
                } else {
                    metaExpr({
                        Data <- ..(NewData())
                        rownames(Data) <- do.call(paste, c(Data[c(
                            ..(
                                input$Tab8_Adjusted_Curves_Select_Variable_1
                            ),
                            ..(
                                input$Tab8_Adjusted_Curves_Select_Variable_2
                            )
                        )], sep = "_"))
                        fit <-
                            survival::survfit(..(previous_model()),
                                newdata = Data[Data[, c(..(
                                    input$Tab8_Adjusted_Curves_Select_Variable_1
                                ))] %in% ..(input$Tab8_Download_Adjusted_Curves_Select), ]
                            )
                        survminer::ggsurvplot(
                            fit,
                            data = Data,
                            censor.shape = "",
                            xlab = ..(
                                input$Tab8_Download_Adjusted_Curves_X_Axis_Title
                            ),
                            ylab = ..(
                                input$Tab8_Download_Adjusted_Curves_Y_Axis_Title
                            ),
                            size = 1,
                            conf.int = ..(
                                input$Tab8_Download_Adjusted_Curves_Display_CI
                            ),
                            risk.table = ..(
                                input$Tab8_Download_Adjusted_Curves_Display_Risk_Table
                            ),
                            legend = c(
                                ..(
                                    input$Tab8_Download_Adjusted_Curves_Legend_Position
                                )
                            ),
                            legend.labs = rownames(summary(fit$table)),
                            risk.table.height = 0.25,
                            pval.size = 6,
                            ggtheme = theme_bw() +
                                theme(plot.title = element_text(
                                    size = 18, hjust = 0.5
                                )) +
                                theme(
                                    legend.title = element_text(
                                        colour = "black",
                                        size = 15,
                                        face = "bold"
                                    )
                                ),
                            break.time.by = 50,
                            risk.table.y.text.col = TRUE,
                            risk.table.y.text = FALSE,
                            legend.title = ..(
                                input$Tab8_Download_Adjusted_Curves_Legend_Title
                            ),
                            title = (
                                ..(
                                    input$Tab8_Download_Adjusted_Curves_Plot_Title
                                )
                            ),
                            font.main = c(18, "plain", "black"),
                            font.x = c(15, "plain", "black"),
                            font.y = c(15, "plain", "black"),
                            font.legend = c(14, "plain", "black"),
                            font.tickslab = c(12, "plain", "black")
                        )
                    })
                }
            })

            output$Pred3 <- metaRender(renderPlot, {
                ..(OutVarPlot())
            })

            return(list(Adjusted_Survival_Curves = OutVarPlot))
        })
    }
