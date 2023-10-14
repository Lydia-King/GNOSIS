# Server function to carry out chi-squared test
Tab6_Chi_Square_Server <-
    function(id, datalist, datalist1, data, rowselect) {
        moduleServer(id, function(input, output, session) {
            observe({
                vchoicesASS <- c(names(datalist1[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_1",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_2",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Continuous_Variable_1",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            data_Association2 <- metaReactive2({
                metaExpr({
                    my_list <- list()
                    for (i in seq.int(from = 1, to = length(..(input$Tab6_Select_Categorical_Variable_2)))) {
                        my_list[[i]] <- table(
                            ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_1)],
                            ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_2)[i]]
                        )
                    }
                    my_list
                })
            })

            Chi <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n \n \n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file. \n \n \n",
                        "\n",
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
                        "\n \n \n \n \n \n Please only select cBioPortal dataset OR upload your own data. \n \n \n",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab6_Select_Categorical_Variable_2) |
                            input$Tab6_Select_Categorical_Variable_2 == "None Selected" |
                            input$Tab6_Select_Categorical_Variable_1 == "None Selected",
                        paste("Please select categorical variables of interest.")
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Categorical_Variable_1 == "None Selected" |
                    input$Tab6_Select_Categorical_Variable_2 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        for (i in seq.int(from = 1, to = length(c(
                            ..(
                                input$Tab6_Select_Categorical_Variable_2
                            )
                        )))) {
                            show_object(noquote(
                                paste(
                                    "Categorical Variable 1:",
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_1
                                    ),
                                    "and",
                                    "Categorical Variable 2:",
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_2
                                    )[i]
                                )
                            ))
                            show_object(chisq.test(..(
                                data_Association2()
                            )[[i]]))
                        }
                    })
                }
            })

            Chi_Code <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR upload your own data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab6_Select_Categorical_Variable_2) |
                            input$Tab6_Select_Categorical_Variable_2 == "None Selected" |
                            input$Tab6_Select_Categorical_Variable_1 == "None Selected",
                        paste(
                            "\n \n \n Please select categorical variables of interest.",
                            "\n",
                            "\n",
                            "\n",
                            "\n"
                        )
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Categorical_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_2 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        Chi_List <- list()
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Categorical_Variable_2
                        )))) {
                            name <- ..(input$Tab6_Select_Categorical_Variable_2)[i]
                            Chi_List[[name]] <-
                                chisq.test(..(data_Association2())[[i]])
                        }
                        Chi_List
                    })
                }
            })

            output$Cat1 <- metaRender(renderPrint, {
                ..(Chi())
            })

            data_Association1Ad <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file."
                    )
                ))
                validate(need(
                    c(
                        is.null(datalist[["patient_manual_data"]]()) &
                            is.null(datalist[["sample_manual_data"]]()) &
                            is.null(datalist[["CNA_manual_data"]]())
                    ) | is.null(rowselect()),
                    paste(
                        "Please only select cBioPortal dataset OR upload your own data."
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab6_Select_Categorical_Variable_2) |
                            "None Selected" %!in% input$Tab6_Select_Categorical_Variable_2 |
                            "None Selected" %!in% input$Tab6_Select_Categorical_Variable_1,
                        "Please select categorical variables of interest."
                    )
                )

                if (isTRUE(
                    "None Selected" %in% input$Tab6_Select_Categorical_Variable_1 |
                        "None Selected" %in% input$Tab6_Select_Categorical_Variable_2
                )) {
                    data.frame(
                        Variables = character(10),
                        X = character(10),
                        df = character(10),
                        Pval = character(10),
                        Adj_Pval = character(10)
                    )
                } else {
                    metaExpr({
                        Variables <- Pval <- X <- df <- c()

                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Categorical_Variable_2
                        )))) {
                            Variables <- c(
                                Variables,
                                paste(
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_1
                                    ),
                                    "&",
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_2
                                    )[i],
                                    sep = " "
                                )
                            )
                            Test <-
                                chisq.test(..(data_Association2())[[i]])
                            X <-
                                c(X, round(as.numeric(Test$statistic), digits = 3))
                            df <- c(df, as.numeric(Test$parameter))
                            Pval <-
                                c(Pval, signif(Test$p.value, digits = 3))
                        }
                        Table <-
                            cbind.data.frame(Variables, X, df, Pval)
                        Table$Adj_Pval <- signif(
                            p.adjust(
                                Table$Pval,
                                method = "BH",
                                n = length(
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_2
                                    )
                                )
                            ),
                            digits = 3
                        )
                        Table %>% select(Variables, X, df, Pval, Adj_Pval)
                    })
                }
            })

            # Data Table with Adjusted P-values
            output$Cat1Ad <- metaRender(renderDataTable, {
                datatable(
                    ..(data_Association1Ad()),
                    options = list(
                        lengthMenu = c(10, 30, 50, 100),
                        pageLength = 10,
                        scrollX = TRUE
                    )
                )
            })
        })
    }

# Server function to carry out simulated fishers exact test
Tab6_FE_Sim_Server <-
    function(id, datalist, datalist1, data, rowselect) {
        moduleServer(id, function(input, output, session) {
            observe({
                vchoicesASS <- c(names(datalist1[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_1",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_2",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_3",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Continuous_Variable_1",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            data_Association2 <- metaReactive2({
                metaExpr({
                    my_list <- list()
                    for (i in seq.int(from = 1, to = length(..(input$Tab6_Select_Categorical_Variable_2)))) {
                        my_list[[i]] <- table(
                            ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_1)],
                            ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_2)[i]]
                        )
                    }
                    my_list
                })
            })

            SimF <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n \n \n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file. \n \n \n",
                        "\n",
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
                        "\n \n \n \n \n \n Please only select cBioPortal dataset OR upload your own data. \n \n \n",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Categorical_Variable_2),
                    paste(
                        "\n \n \n Please select categorical variables of interest.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                if (isTRUE(
                    input$Tab6_Select_Categorical_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_2 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        for (i in seq.int(from = 1, to = length(c(
                            ..(
                                input$Tab6_Select_Categorical_Variable_2
                            )
                        )))) {
                            show_object(noquote(
                                paste(
                                    "Categorical Variable 1:",
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_1
                                    ),
                                    "and",
                                    "Categorical Variable 2:",
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_2
                                    )[i]
                                )
                            ))
                            show_object(fisher.test(
                                ..(data_Association2())[[i]],
                                simulate.p.value = TRUE
                            ))
                        }
                    })
                }
            })

            SimF_Code <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR upload your own data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Categorical_Variable_2),
                    paste(
                        "\n \n \n Please select categorical variables of interest.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                if (isTRUE(
                    input$Tab6_Select_Categorical_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_2 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        SimF_List <- list()
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Categorical_Variable_2
                        )))) {
                            name <- ..(input$Tab6_Select_Categorical_Variable_2)[i]
                            SimF_List[[name]] <- fisher.test(..(data_Association2())[[i]],
                                simulate.p.value = TRUE
                            )
                        }
                        SimF_List
                    })
                }
            })

            output$Cat2 <- metaRender(renderPrint, {
                ..(SimF())
            })

            data_Association2Ad <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file."
                    )
                ))
                validate(need(
                    c(
                        is.null(datalist[["patient_manual_data"]]()) &
                            is.null(datalist[["sample_manual_data"]]()) &
                            is.null(datalist[["CNA_manual_data"]]())
                    ) | is.null(rowselect()),
                    paste(
                        "Please only select cBioPortal dataset OR upload your own data."
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Categorical_Variable_2),
                    paste("Please select categorical variables of interest.")
                ))

                if (isTRUE(
                    input$Tab6_Select_Categorical_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_2 == "None Selected"
                )) {
                    data.frame(
                        Variables = character(10),
                        Pval = character(10),
                        Adj_Pval = character(10)
                    )
                } else {
                    metaExpr({
                        Variables <- Pval <- c()

                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Categorical_Variable_2
                        )))) {
                            tryCatch(
                                {
                                    Variables <- c(
                                        Variables,
                                        paste(
                                            ..(
                                                input$Tab6_Select_Categorical_Variable_1
                                            ),
                                            "&",
                                            ..(
                                                input$Tab6_Select_Categorical_Variable_2
                                            )[i],
                                            sep = " "
                                        )
                                    )
                                    Test <- fisher.test(..(data_Association2())[[i]],
                                        simulate.p.value = TRUE
                                    )
                                    Pval <-
                                        c(
                                            Pval,
                                            signif(Test$p.value, digits = 3)
                                        )
                                },
                                error = function(e) {
                                    show_object(paste(
                                        "ERROR :",
                                        conditionMessage(e))
                                    )
                                }
                            )
                        }

                        Table <- cbind.data.frame(Variables, Pval)
                        Table$Adj_Pval <- signif(
                            p.adjust(
                                Table$Pval,
                                method = "BH",
                                n = length(
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_2
                                    )
                                )
                            ),
                            digits = 3
                        )
                        Table %>% select(Variables, Pval, Adj_Pval)
                    })
                }
            })

            output$Cat2Ad <- metaRender(renderDataTable, {
                datatable(
                    ..(data_Association2Ad()),
                    options = list(
                        lengthMenu = c(10, 30, 50, 100),
                        pageLength = 10,
                        scrollX = TRUE
                    )
                )
            })
        })
    }

# Server function to carry out fishers exact test
Tab6_FE_Server <-
    function(id, datalist, datalist1, data, rowselect) {
        moduleServer(id, function(input, output, session) {
            observe({
                vchoicesASS <- c(names(datalist1[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_1",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_2",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_3",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Continuous_Variable_1",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            data_Association2 <- metaReactive2({
                metaExpr({
                    my_list <- list()
                    for (i in seq.int(from = 1, to = length(..(input$Tab6_Select_Categorical_Variable_2)))) {
                        my_list[[i]] <- table(
                            ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_1)],
                            ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_2)[i]]
                        )
                    }
                    my_list
                })
            })

            Fis <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n \n \n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file. \n \n \n",
                        "\n",
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
                        "\n \n \n \n \n \n Please only select cBioPortal dataset OR upload your own data. \n \n \n",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Categorical_Variable_2),
                    "Please select categorical variables of interest."
                ))
                if (isTRUE(
                    input$Tab6_Select_Categorical_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_2 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        for (i in seq.int(from = 1, to = length(c(
                            ..(
                                input$Tab6_Select_Categorical_Variable_2
                            ))
                        ))) {
                            tryCatch(
                                {
                                    show_object(noquote(
                                        paste(
                                            "Categorical Variable 1:",
                                            ..(
                                                input$Tab6_Select_Categorical_Variable_1
                                            ),
                                            "and",
                                            "Categorical Variable 2:",
                                            ..(
                                                input$Tab6_Select_Categorical_Variable_2
                                            )[i]
                                        )
                                    ))
                                    show_object(fisher.test(
                                        ..(data_Association2())[[i]],
                                        simulate.p.value = FALSE
                                    ))
                                },
                                error = function(e) {
                                    show_object(paste(
                                        "ERROR :",
                                        conditionMessage(e))
                                    )
                                }
                            )
                        }
                    })
                }
            })

            F_Code <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR upload your own data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Categorical_Variable_2),
                    paste(
                        "\n \n \n Please select categorical variables of interest.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                if (isTRUE(
                    input$Tab6_Select_Categorical_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_2 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        F_List <- list()
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Categorical_Variable_2
                        )))) {
                            name <- ..(input$Tab6_Select_Categorical_Variable_2)[i]
                            tryCatch(
                                {
                                    F_List[[name]] <- fisher.test(..(data_Association2())[[i]],
                                        simulate.p.value = FALSE
                                    )
                                },
                                error = function(e) {
                                    show_object(paste(
                                        "ERROR :",
                                        conditionMessage(e))
                                    )
                                }
                            )
                        }
                        F_List
                    })
                }
            })

            output$Cat3 <- metaRender(renderPrint, {
                ..(Fis())
            })

            data_Association3Ad <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file."
                    )
                ))
                validate(need(
                    c(
                        is.null(datalist[["patient_manual_data"]]()) &
                            is.null(datalist[["sample_manual_data"]]()) &
                            is.null(datalist[["CNA_manual_data"]]())
                    ) | is.null(rowselect()),
                    paste(
                        "Please only select cBioPortal dataset OR upload your own data."
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Categorical_Variable_2),
                    paste("Please select categorical variables of interest.")
                ))
                if (isTRUE(
                    input$Tab6_Select_Categorical_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_2 == "None Selected"
                )) {
                    data.frame(
                        Variables = character(10),
                        Pval = character(10),
                        Adj_Pval = character(10)
                    )
                } else {
                    metaExpr({
                        Variables <- Pval <- c()
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Categorical_Variable_2
                        )))) {
                            tryCatch(
                                {
                                    Test <- fisher.test(..(data_Association2())[[i]])
                                    Pval <-
                                        c(
                                            Pval,
                                            signif(Test$p.value, digits = 3)
                                        )
                                    Variables <- c(
                                        Variables,
                                        paste(
                                            ..(
                                                input$Tab6_Select_Categorical_Variable_1
                                            ),
                                            "&",
                                            ..(
                                                input$Tab6_Select_Categorical_Variable_2
                                            )[i],
                                            sep = " "
                                        )
                                    )
                                },
                                error = function(e) {
                                    show_object(paste(
                                        "ERROR :",
                                        conditionMessage(e))
                                    )
                                }
                            )
                        }
                        Table <- cbind.data.frame(Variables, Pval)
                        Table$Adj_Pval <- signif(
                            p.adjust(
                                Table$Pval,
                                method = "BH",
                                n = length(
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_2
                                    )
                                )
                            ),
                            digits = 3
                        )
                        Table %>% select(Variables, Pval, Adj_Pval)
                    })
                }
            })

            output$Cat3Ad <- metaRender(renderDataTable, {
                datatable(
                    ..(data_Association3Ad()),
                    options = list(
                        lengthMenu = c(10, 30, 50, 100),
                        pageLength = 10,
                        scrollX = TRUE
                    )
                )
            })
        })
    }

# Server function to carry out ANOVA
Tab6_ANOVA_Server <-
    function(id, datalist, datalist1, data, rowselect) {
        moduleServer(id, function(input, output, session) {
            observe({
                vchoicesASS <- c(names(datalist1[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_1",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_2",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_3",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Continuous_Variable_1",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            Assump1 <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or",
                        "upload your own clinical file, sample file",
                        "and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset",
                        "OR upload your own data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab6_Select_Continuous_Variable_1),
                        "\n \n \n Please select continuous variables of",
                        "interest., \n \n \n \n"
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            show_object(noquote(
                                paste(
                                    "Categorical Variable:",
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_3
                                    ),
                                    "and",
                                    "Continuous Variable:",
                                    ..(
                                        input$Tab6_Select_Continuous_Variable_1
                                    )[i]
                                )
                            ))
                            show_object(leveneTest(
                                ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]] ~
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                ..(datalist1[[data]]()),
                                center = mean
                            ))
                        }
                    })
                }
            })

            Levene_Code <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or",
                        "upload your own clinical file, sample file",
                        "and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset",
                        "OR upload your own data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab6_Select_Continuous_Variable_1),
                        "\n \n \n Please select continuous variables of",
                        "interest. \n \n \n \n"
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        Levene_List <- list()
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
                            Levene_List[[name]] <- leveneTest(
                                ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]] ~
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                ..(datalist1[[data]]()),
                                center = mean
                            )
                            Levene_List
                        }
                    })
                }
            })

            output$ANOVAAss1 <- metaRender(renderPrint, {
                ..(Assump1())
            })

            Assump2 <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload your own",
                        "clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR upload your own",
                        "data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab6_Select_Continuous_Variable_1),
                        "\n \n \n Please select continuous variables of interest.,",
                        "\n \n \n \n"
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            show_object(noquote(
                                paste(
                                    "Categorical Variable:",
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_3
                                    ),
                                    "and",
                                    "Continuous Variable:",
                                    ..(
                                        input$Tab6_Select_Continuous_Variable_1
                                    )[i]
                                )
                            ))
                            show_object(fligner.test(
                                ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]] ~
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                ..(datalist1[[data]]())
                            ))
                        }
                    })
                }
            })


            Fligner_Code <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload your own",
                        "clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR upload your own",
                        "data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab6_Select_Continuous_Variable_1),
                        "\n \n \n Please select continuous variables of interest.",
                        "\n \n \n \n"
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        Fligner_List <- list()
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
                            Fligner_List[[name]] <- fligner.test(
                                ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]] ~
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                ..(datalist1[[data]]())
                            )
                            Fligner_List
                        }
                    })
                }
            })


            output$ANOVAAss2 <- metaRender(renderPrint, {
                ..(Assump2())
            })

            Assump3 <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload your own",
                        "clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR upload your own",
                        "data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab6_Select_Continuous_Variable_1),
                        "\n \n \n Please select continuous variables of interest.",
                        "\n \n \n \n"
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        for (i in seq.int(from = 1, to = length(c(
                            ..(
                                input$Tab6_Select_Continuous_Variable_1
                            ))
                        ))) {
                            show_object(noquote(
                                paste(
                                    "Categorical Variable:",
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_3
                                    ),
                                    "and",
                                    "Continuous Variable:",
                                    ..(
                                        input$Tab6_Select_Continuous_Variable_1
                                    )[i]
                                )
                            ))
                            Data1 <- ..(datalist1[[data]]()) %>%
                                group_by(!!as.name(
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_3
                                    )
                                )) %>%
                                shapiro_test(!!as.name(
                                    ..(
                                        input$Tab6_Select_Continuous_Variable_1
                                    )[i]
                                ))
                            show_object(as.data.frame(Data1))
                            show_object(shapiro.test(..(datalist1[[data]](
                            ))[, ..(input$Tab6_Select_Continuous_Variable_1)[i]]))
                        }
                    })
                }
            })

            Shapiro_Code <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload your own",
                        "clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR upload your own",
                        "data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab6_Select_Continuous_Variable_1),
                        "\n \n \n Please select continuous variables of interest.",
                        "\n \n \n \n"
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        Shapiro_List <- list()
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
                            Shapiro_List[[name]] <- shapiro.test(..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]])
                            Shapiro_List
                        }
                    })
                }
            })


            output$ANOVAAss3 <- metaRender(renderPrint, {
                ..(Assump3())
            })

            # ANOVA test
            ANOVA_R <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload your own",
                        "clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR upload your own",
                        "data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab6_Select_Continuous_Variable_1),
                        "\n \n \n Please select continuous variables of interest.",
                        "\n \n \n \n"
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        for (i in seq.int(from = 1, to = length(c(
                            ..(
                                input$Tab6_Select_Continuous_Variable_1
                            ))
                        ))) {
                            res.aov <- aov(
                                ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]] ~
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                data = ..(datalist1[[data]]())
                            )
                            show_object(noquote(
                                paste(
                                    "Categorical Variable:",
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_3
                                    ),
                                    "and",
                                    "Continuous Variable:",
                                    ..(
                                        input$Tab6_Select_Continuous_Variable_1
                                    )[i]
                                )
                            ))
                            show_object(summary(res.aov))
                        }
                    })
                }
            })


            ANOVA_Code <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload your own",
                        "clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR upload your own",
                        "data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab6_Select_Continuous_Variable_1),
                        "\n \n \n Please select continuous variables of interest.",
                        "\n \n \n \n"
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        ANOVA_List <- list()
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
                            ANOVA_List[[name]] <- summary(aov(
                                ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]] ~
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                data = ..(datalist1[[data]]())
                            ))
                            ANOVA_List
                        }
                    })
                }
            })

            output$ANOVA <- metaRender(renderPrint, {
                ..(ANOVA_R())
            })

            data_ANOVAAd <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file."
                    )
                ))
                validate(need(
                    c(
                        is.null(datalist[["patient_manual_data"]]()) &
                            is.null(datalist[["sample_manual_data"]]()) &
                            is.null(datalist[["CNA_manual_data"]]())
                    ) | is.null(rowselect()),
                    paste(
                        "Please only select cBioPortal dataset OR upload your own data."
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Continuous_Variable_1),
                    "Please select continuous variables of interest."
                ))
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    data.frame(
                        Variables = character(10),
                        X = character(10),
                        df = character(10),
                        Pval = character(10),
                        Adj_Pval = character(10)
                    )
                } else {
                    metaExpr({
                        Variables <- Pval <- c()

                        for (i in seq.int(from = 1, to = length(c(
                            ..(
                                input$Tab6_Select_Continuous_Variable_1
                            ))
                        ))) {
                            Variables <-
                                c(
                                    Variables,
                                    paste(
                                        ..(
                                            input$Tab6_Select_Categorical_Variable_3
                                        ),
                                        "&",
                                        ..(
                                            input$Tab6_Select_Continuous_Variable_1
                                        )[i],
                                        sep = " "
                                    )
                                )
                            Test <- summary(aov(
                                ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]] ~
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                data = ..(datalist1[[data]]())
                            ))
                            Pval <-
                                c(Pval, signif(
                                    as.numeric(Test[[1]]$`Pr(>F)`[1]),
                                    digits = 3
                                ))
                        }

                        Table <- cbind.data.frame(Variables, Pval)
                        Table$Adj_Pval <- signif(
                            p.adjust(
                                Table$Pval,
                                method = "BH",
                                n = length(
                                    ..(
                                        input$Tab6_Select_Continuous_Variable_1
                                    )
                                )
                            ),
                            digits = 3
                        )
                        Table %>% select(Variables, Pval, Adj_Pval)
                    })
                }
            })

            output$ANOVAAd <- metaRender(renderDataTable, {
                datatable(
                    ..(data_ANOVAAd()),
                    options = list(
                        lengthMenu = c(10, 30, 50, 100),
                        pageLength = 10,
                        scrollX = TRUE
                    )
                )
            })
        })
    }

# Server function to carry out Kruskal-Wallis
Tab6_KW_Server <-
    function(id, datalist, datalist1, data, rowselect) {
        moduleServer(id, function(input, output, session) {
            observe({
                vchoicesASS <- c(names(datalist1[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_1",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_2",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_3",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Continuous_Variable_1",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            KW_R <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n \n \n \n Please select cBioPortal dataset or upload",
                        "your own clinical file, sample file and/or CNA file. \n \n \n ",
                        "\n",
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
                        "\n \n \n \n \n \n Please only select cBioPortal dataset OR",
                        "upload your own data. \n \n \n ",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Continuous_Variable_1),
                    "Please select continuous variables of interest."
                ))
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            Kw <- kruskal.test(
                                ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]] ~
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                data = ..(datalist1[[data]]())
                            )
                            show_object(noquote(
                                paste(
                                    "Categorical Variable:",
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_3
                                    ),
                                    "and",
                                    "Continuous Variable:",
                                    ..(
                                        input$Tab6_Select_Continuous_Variable_1
                                    )[i]
                                )
                            ))
                            show_object(Kw)
                        }
                    })
                }
            })

            KW_Code <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n \n \n \n Please select cBioPortal dataset or upload",
                        "your own clinical file, sample file and/or CNA file. \n \n \n",
                        "\n",
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
                        "\n \n \n \n \n \n Please only select cBioPortal dataset OR",
                        "upload your own data. \n \n \n",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Continuous_Variable_1),
                    "Please select continuous variables of interest."
                ))
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        KW_List <- list()
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
                            KW_List[[name]] <- kruskal.test(
                                ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]] ~
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                data = ..(datalist1[[data]]())
                            )
                        }
                        KW_List
                    })
                }
            })

            output$KW <- metaRender(renderPrint, {
                ..(KW_R())
            })

            data_KWAdj <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "Please select cBioPortal dataset or upload your own clinical file,",
                        "sample file and/or CNA file."
                    )
                ))
                validate(need(
                    c(
                        is.null(datalist[["patient_manual_data"]]()) &
                            is.null(datalist[["sample_manual_data"]]()) &
                            is.null(datalist[["CNA_manual_data"]]())
                    ) | is.null(rowselect()),
                    paste(
                        "Please only select cBioPortal dataset OR upload your own data."
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Continuous_Variable_1),
                    "Please select continuous variables of interest."
                ))
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    data.frame(
                        Variables = character(10),
                        Statistic = character(10),
                        df = character(10),
                        Pval = character(10),
                        Adj_Pval = character(10)
                    )
                } else {
                    metaExpr({
                        Variables <- Statistic <- df <- Pval <- c()

                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            Variables <-
                                c(
                                    Variables,
                                    paste(
                                        ..(
                                            input$Tab6_Select_Categorical_Variable_3
                                        ),
                                        "&",
                                        ..(
                                            input$Tab6_Select_Continuous_Variable_1
                                        )[i],
                                        sep = " "
                                    )
                                )
                            Test <- kruskal.test(
                                ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]] ~
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                data = ..(datalist1[[data]]())
                            )
                            Statistic <-
                                c(
                                    Statistic,
                                    signif(Test$statistic[[1]], digits = 3)
                                )
                            df <- c(df, Test$parameter[[1]])
                            Pval <-
                                c(Pval, signif(as.numeric(Test$p.value[[1]]), digits = 3))
                        }

                        Table <-
                            cbind.data.frame(Variables, Statistic, df, Pval)
                        Table$Adj_Pval <- signif(
                            p.adjust(
                                Table$Pval,
                                method = "BH",
                                n = length(
                                    ..(
                                        input$Tab6_Select_Continuous_Variable_1
                                    )
                                )
                            ),
                            digits = 3
                        )
                        Table %>% select(Variables, Statistic, df, Pval, Adj_Pval)
                    })
                }
            })

            output$KWAd <- metaRender(renderDataTable, {
                datatable(..(data_KWAdj()),
                    options = list(
                        lengthMenu = c(10, 30, 50, 100),
                        pageLength = 10,
                        scrollX = TRUE
                    )
                )
            })
        })
    }

# Server function to carry out pair-wise comparisons
Tab6_PWC_Server <-
    function(id, datalist, datalist1, data, rowselect) {
        moduleServer(id, function(input, output, session) {
            observe({
                vchoicesASS <- c(names(datalist1[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_1",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_2",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Categorical_Variable_3",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Continuous_Variable_1",
                    choices = vchoicesASS,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            PWC <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload",
                        "your own clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR",
                        "upload your own data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Continuous_Variable_1),
                    paste(
                        "\n \n \n Please select continuous variables of interest.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        input$Tab6_Select_Continuous_Variable_1 != "None Selected" &
                            input$Tab6_Select_Categorical_Variable_3 != "None Selected",
                        paste(
                            "\n \n \n Please select variables of interest.",
                            "\n",
                            "\n",
                            "\n",
                            "\n"
                        )
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            show_object(noquote(
                                paste(
                                    "Categorical Variable:",
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_3
                                    ),
                                    "and",
                                    "Continuous Variable:",
                                    ..(
                                        input$Tab6_Select_Continuous_Variable_1
                                    )[i]
                                )
                            ))
                            show_object(pairwise.t.test(
                                ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]],
                                ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                p.adjust.method = "BH"
                            ))
                        }
                    })
                }
            })

            WT_Code <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload",
                        "your own clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR",
                        "upload your own data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Continuous_Variable_1),
                    paste(
                        "\n \n \n Please select continuous variables of interest.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        input$Tab6_Select_Continuous_Variable_1 != "None Selected" &
                            input$Tab6_Select_Categorical_Variable_3 != "None Selected",
                        paste(
                            "\n \n \n Please select variables of interest.",
                            "\n",
                            "\n",
                            "\n",
                            "\n"
                        )
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        PairwiseT_List <- list()
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
                            PairwiseT_List[[name]] <-
                                pairwise.t.test(..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]],
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                    p.adjust.method = "BH"
                                )
                        }
                        PairwiseT_List
                    })
                }
            })

            output$PC <- metaRender(renderPrint, {
                ..(PWC())
            })

            PWCDunn <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload",
                        "your own clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR",
                        "upload your own data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Continuous_Variable_1),
                    paste(
                        "\n \n \n Please select continuous variables of interest.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        input$Tab6_Select_Continuous_Variable_1 != "None Selected" &
                            input$Tab6_Select_Categorical_Variable_3 != "None Selected",
                        paste(
                            "\n \n Please select variables of interest.",
                            "\n",
                            "\n",
                            "\n"
                        )
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            show_object(noquote(
                                paste(
                                    "Categorical Variable:",
                                    ..(
                                        input$Tab6_Select_Categorical_Variable_3
                                    ),
                                    "and",
                                    "Continuous Variable:",
                                    ..(
                                        input$Tab6_Select_Continuous_Variable_1
                                    )[i]
                                )
                            ))
                            show_object(
                                DunnTest(
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]],
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                    method = "BH",
                                    out.list = FALSE
                                )
                            )
                        }
                    })
                }
            })

            PWD_Code <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or upload",
                        "your own clinical file, sample file and/or CNA file.",
                        "\n",
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
                        "\n \n \n Please only select cBioPortal dataset OR",
                        "upload your own data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Continuous_Variable_1),
                    paste(
                        "\n \n \n Please select continuous variables of interest.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        input$Tab6_Select_Continuous_Variable_1 != "None Selected" &
                            input$Tab6_Select_Categorical_Variable_3 != "None Selected",
                        paste(
                            "\n \n \n Please select variables of interest.",
                            "\n",
                            "\n",
                            "\n",
                            "\n"
                        )
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Continuous_Variable_1 == "None Selected" |
                        input$Tab6_Select_Categorical_Variable_3 == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        PairwiseD_List <- list()
                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab6_Select_Continuous_Variable_1
                        )))) {
                            name <- ..(input$Tab6_Select_Continuous_Variable_1)[i]
                            PairwiseD_List[[name]] <-
                                DunnTest(
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Continuous_Variable_1)[i]],
                                    ..(datalist1[[data]]())[, ..(input$Tab6_Select_Categorical_Variable_3)],
                                    method = "BH",
                                    out.list = FALSE
                                )
                        }
                        PairwiseD_List
                    })
                }
            })

            output$Dunn <- metaRender(renderPrint, {
                ..(PWCDunn())
            })
        })
    }

# Server function to carry out compare groups
Tab6_CG_Server <-
    function(id, datalist, datalist1, data, rowselect) {
        moduleServer(id, function(input, output, session) {
            observe({
                vchoicesCG <- c(names(datalist1[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Response_Variable",
                    choices = vchoicesCG,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab6_Select_Explanatory_Variable",
                    choices = vchoicesCG,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            Compare_Code <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n Please select cBioPortal dataset or",
                        "upload your own clinical file, sample file",
                        "and/or CNA file.",
                        "\n",
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
                    ) |
                        is.null(rowselect()),
                    paste(
                        "\n \n \n Please only select cBioPortal",
                        "dataset OR upload your own data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(need(
                    !is.null(input$Tab6_Select_Response_Variable) &&
                        !is.null(input$Tab6_Select_Explanatory_Variable),
                    paste(
                        "\n \n \n Please select response and explanatory",
                        "variables. \n \n \n \n"
                    )
                ))
                validate(
                    need(
                        input$Tab6_Select_Response_Variable != "None Selected" &
                            input$Tab6_Select_Explanatory_Variable != "None Selected",
                        paste(
                            "\n \n \n Please select response and explanatory",
                            "variables. \n \n \n \n"
                        )
                    )
                )
                if (isTRUE(
                    input$Tab6_Select_Response_Variable == "None Selected" |
                        input$Tab6_Select_Explanatory_Variable == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        res <- compareGroups(
                            as.formula(paste(
                                ..(input$Tab6_Select_Response_Variable),
                                "~",
                                paste(
                                    c(
                                        ..(input$Tab6_Select_Explanatory_Variable)
                                    ),
                                    collapse = "+"
                                )
                            )),
                            data = ..(datalist1[[data]]()),
                            max.ylev = 10,
                            method = NA
                        )
                        res
                    })
                }
            })

            output$CG <- metaRender(renderPrint, {
                show_object(..(Compare_Code()))
            })
            output$CG_Table <- metaRender(renderPrint, {
                createTable(..(Compare_Code()))
            })
        })
    }
