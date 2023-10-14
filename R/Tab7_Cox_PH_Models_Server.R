# Server function to produce univariate Cox models
Tab7_Uni_Cox_Server <-
    function(id, datalist, datalist1, data, rowselect) {
        moduleServer(id, function(input, output, session) {
            observe({
                vchoicesSurvcox <- c(names(datalist1[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab7_Univariate_Cox_Survival_Time",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Univariate_Cox_Event_Status",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Univariate_Cox_Select_Variables",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Multivariable_Cox_Survival_Time",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Multivariable_Cox_Event_Status",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Multivariable_Cox_Select_Variables",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Multivariable_Cox_Select_Interaction_Variables_1",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Multivariable_Cox_Select_Interaction_Variables_2",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Multivariable_Cox_Select_Interaction_Variables_3",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            surv_data_Cox <- metaReactive2({
                metaExpr({
                    ..(datalist1[[data]]()) %>%
                        select(
                            !!..(input$Tab7_Univariate_Cox_Survival_Time), !!..(input$Tab7_Univariate_Cox_Event_Status),
                            c(
                                !!..(input$Tab7_Univariate_Cox_Select_Variables)
                            )
                        )
                })
            })

            CoxModelOut_1 <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n \n \n \n \n \n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file. \n \n \n \n",
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
                        "\n \n \n \n \n \n \n \n Please only select cBioPortal dataset OR upload your own data. \n \n \n \n",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab7_Univariate_Cox_Select_Variables),
                        "\n \n \n \n \n Please select time to event, event status and variable of interest. \n \n \n \n \n \n"
                    )
                )

                if (isTRUE(
                    is.null(datalist1[[data]]()) |
                        input$Tab7_Univariate_Cox_Survival_Time == "None Selected" |
                        input$Tab7_Univariate_Cox_Event_Status == "None Selected" |
                        input$Tab7_Univariate_Cox_Select_Variables == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        for (i in 1:length(..(
                            input$Tab7_Univariate_Cox_Select_Variables
                        ))) {
                            show_object(noquote(paste(
                                ..(
                                    input$Tab7_Univariate_Cox_Event_Status
                                ),
                                "for",
                                ..(
                                    input$Tab7_Univariate_Cox_Select_Variables
                                )[i]
                            )))
                            res.cox <- coxph(
                                formula = as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Univariate_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Univariate_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(paste(
                                            ..(
                                                input$Tab7_Univariate_Cox_Select_Variables
                                            )[i],
                                            collapse = "+"
                                        ))
                                    )
                                ),
                                data = ..(surv_data_Cox())
                            )
                            show_object(summary(res.cox))
                        }
                    })
                }
            })


            output$CoxModelOut <- metaRender(renderPrint, {
                ..(CoxModelOut_1())
            })

            data_UniAdj <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file.",
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
                        "\n Please only select cBioPortal dataset OR upload your own data.",
                        "\n",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab7_Univariate_Cox_Select_Variables),
                        "\n Please select time to event, event status and variable of interest. \n \n \n \n"
                    )
                )

                if (isTRUE(
                    is.null(datalist1[[data]]()) |
                        input$Tab7_Univariate_Cox_Survival_Time == "None Selected" |
                        input$Tab7_Univariate_Cox_Event_Status == "None Selected" |
                        input$Tab7_Univariate_Cox_Select_Variables == "None Selected"
                )) {
                    data.frame(
                        Variables = character(10),
                        LRT = character(10),
                        Adj_LRT = character(10),
                        Wald = character(10),
                        Adj_Wald = character(10),
                        Logrank = character(10),
                        Adj_Logrank = character(10)
                    )
                } else {
                    metaExpr({
                        Variables <- LRT <- Wald <- Logrank <- c()

                        for (i in seq.int(from = 1, to = length(..(
                            input$Tab7_Univariate_Cox_Select_Variables
                        )))) {
                            Variables <- c(
                                Variables,
                                paste(
                                    ..(
                                        input$Tab7_Univariate_Cox_Event_Status
                                    ),
                                    "for",
                                    ..(
                                        input$Tab7_Univariate_Cox_Select_Variables
                                    )[i],
                                    sep = " "
                                )
                            )
                            res.cox <- coxph(
                                formula = as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Univariate_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Univariate_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(paste(
                                            ..(
                                                input$Tab7_Univariate_Cox_Select_Variables
                                            )[i],
                                            collapse = "+"
                                        ))
                                    )
                                ),
                                data = ..(surv_data_Cox())
                            )
                            LRT <-
                                c(LRT, signif(
                                    as.numeric(summary(res.cox)$logtest[3]),
                                    digits = 3
                                ))
                            Wald <-
                                c(Wald, signif(
                                    as.numeric(summary(res.cox)$waldtest[3]),
                                    digits = 3
                                ))
                            Logrank <-
                                c(
                                    Logrank,
                                    signif(
                                        as.numeric(summary(res.cox)$sctest[3]),
                                        digits = 3
                                    )
                                )
                        }

                        Table <-
                            cbind.data.frame(Variables, LRT, Wald, Logrank)
                        Table$Adj_LRT <- signif(
                            p.adjust(
                                as.numeric(Table$LRT),
                                method = "BH",
                                n = length(
                                    ..(
                                        input$Tab7_Univariate_Cox_Select_Variables
                                    )
                                )
                            ),
                            digits = 3
                        )
                        Table$Adj_Wald <- signif(
                            p.adjust(
                                as.numeric(Table$Wald),
                                method = "BH",
                                n = length(
                                    ..(
                                        input$Tab7_Univariate_Cox_Select_Variables
                                    )
                                )
                            ),
                            digits = 3
                        )
                        Table$Adj_Logrank <- signif(
                            p.adjust(
                                as.numeric(Table$Logrank),
                                method = "BH",
                                n = length(
                                    ..(
                                        input$Tab7_Univariate_Cox_Select_Variables
                                    )
                                )
                            ),
                            digits = 3
                        )
                        Table %>% select(
                            Variables,
                            LRT,
                            Adj_LRT,
                            Wald,
                            Adj_Wald,
                            Logrank,
                            Adj_Logrank
                        )
                    })
                }
            })

            output$UniAdjusted <- metaRender(renderDataTable, {
                datatable(
                    ..(data_UniAdj()),
                    options = list(
                        lengthMenu = c(10, 30, 50, 100),
                        pageLength = 10,
                        scrollX = TRUE
                    )
                )
            })
        })
    }

# Server function to produce multivariate Cox models and assumptions
Tab7_Multi_Cox_Server <-
    function(id,
             datalist,
             datalist1,
             data,
             rowselect,
             inputs) {
        moduleServer(id, function(input, output, session) {
            observe({
                vchoicesSurvcox <- c(names(datalist1[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab7_Univariate_Cox_Survival_Time",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Univariate_Cox_Event_Status",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Univariate_Cox_Select_Variables",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Multivariable_Cox_Survival_Time",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Multivariable_Cox_Event_Status",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Multivariable_Cox_Select_Variables",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Multivariable_Cox_Select_Interaction_Variables_1",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Multivariable_Cox_Select_Interaction_Variables_2",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab7_Multivariable_Cox_Select_Interaction_Variables_3",
                    choices = vchoicesSurvcox,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            surv_data_CoxM <- metaReactive2({
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
                        !is.null(input$Tab7_Multivariable_Cox_Select_Variables) &
                            !is.null(
                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                            ) &
                            !is.null(
                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                            ) &
                            !is.null(
                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                            ),
                        "\n \n Please make sure all select boxes are filled \n \n \n"
                    )
                )

                # 1st
                if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        "None Selected" %!in% c(input$Tab7_Multivariable_Cox_Select_Variables) &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>% select(
                            !!..(
                                input$Tab7_Multivariable_Cox_Survival_Time
                            ), !!..(
                                input$Tab7_Multivariable_Cox_Event_Status
                            ),
                            c(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Select_Variables
                                )
                            )
                        )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>% select(
                            !!..(
                                input$Tab7_Multivariable_Cox_Survival_Time
                            ), !!..(
                                input$Tab7_Multivariable_Cox_Event_Status
                            ),
                            c(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Select_Variables
                                )
                            ),
                            c(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                )
                            )
                        )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>% select(
                            !!..(
                                input$Tab7_Multivariable_Cox_Survival_Time
                            ), !!..(
                                input$Tab7_Multivariable_Cox_Event_Status
                            ),
                            c(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                )
                            )
                        )
                    })
                }

                # 2nd
                else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>% select(
                            !!..(
                                input$Tab7_Multivariable_Cox_Survival_Time
                            ), !!..(
                                input$Tab7_Multivariable_Cox_Event_Status
                            ),
                            c(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Select_Variables
                                )
                            ),
                            c(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                )
                            )
                        )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>% select(
                            !!..(
                                input$Tab7_Multivariable_Cox_Survival_Time
                            ), !!..(
                                input$Tab7_Multivariable_Cox_Event_Status
                            ),
                            c(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Select_Variables
                                )
                            ),
                            c(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                )
                            ),
                            c(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                )
                            )
                        )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>% select(
                            !!..(
                                input$Tab7_Multivariable_Cox_Survival_Time
                            ), !!..(
                                input$Tab7_Multivariable_Cox_Event_Status
                            ),
                            c(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                )
                            ),
                            c(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                )
                            )
                        )
                    })
                }

                # 3rd
                else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>%
                            select(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Survival_Time
                                ), !!..(
                                    input$Tab7_Multivariable_Cox_Event_Status
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                    )
                                )
                            )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>%
                            select(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Survival_Time
                                ), !!..(
                                    input$Tab7_Multivariable_Cox_Event_Status
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Variables
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                    )
                                )
                            )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>%
                            select(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Survival_Time
                                ), !!..(
                                    input$Tab7_Multivariable_Cox_Event_Status
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                    )
                                )
                            )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>%
                            select(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Survival_Time
                                ), !!..(
                                    input$Tab7_Multivariable_Cox_Event_Status
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Variables
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                    )
                                )
                            )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>%
                            select(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Survival_Time
                                ), !!..(
                                    input$Tab7_Multivariable_Cox_Event_Status
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Variables
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                    )
                                )
                            )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>%
                            select(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Survival_Time
                                ), !!..(
                                    input$Tab7_Multivariable_Cox_Event_Status
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Variables
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                    )
                                )
                            )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>%
                            select(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Survival_Time
                                ), !!..(
                                    input$Tab7_Multivariable_Cox_Event_Status
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Variables
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                    )
                                )
                            )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Survival_Time != "None Selected" &
                        input$Tab7_Multivariable_Cox_Event_Status != "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>%
                            select(
                                !!..(
                                    input$Tab7_Multivariable_Cox_Survival_Time
                                ), !!..(
                                    input$Tab7_Multivariable_Cox_Event_Status
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                    )
                                ),
                                c(
                                    !!..(
                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                    )
                                )
                            )
                    })
                } else {
                    return(NULL)
                }
            })

            CoxModel <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file.",
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
                        "\n \n Please only select cBioPortal dataset OR upload your own data.",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))

                validate(
                    need(
                        !is.null(input$Tab7_Multivariable_Cox_Select_Variables) &
                            !is.null(
                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                            ) &
                            !is.null(
                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                            ) &
                            !is.null(
                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                            ),
                        "\n \n Please make sure all select boxes are filled \n \n \n"
                    )
                )

                if (isTRUE(
                    is.null(datalist1[[data]]()) |
                        input$Tab7_Multivariable_Cox_Survival_Time == "None Selected" |
                        input$Tab7_Multivariable_Cox_Event_Status == "None Selected"
                )) {
                    return(NULL)
                }

                # 1st
                else if (isTRUE(
                    "None Selected" %!in% c(input$Tab7_Multivariable_Cox_Select_Variables) &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(paste(
                                        ..(
                                            input$Tab7_Multivariable_Cox_Select_Variables
                                        ),
                                        collapse = "+"
                                    ))
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(paste(
                                        paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Variables
                                            ),
                                            collapse = "+"
                                        ),
                                        "+",
                                        paste("(", paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                            ),
                                            collapse = "+"
                                        ), ")^2", sep = ""),
                                        sep = ""
                                    ))
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(paste(
                                        "(", paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                            ),
                                            collapse = "+"
                                        ), ")^2",
                                        sep = ""
                                    ))
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                }

                # 2nd
                else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(paste(
                                        paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Variables
                                            ),
                                            collapse = "+"
                                        ),
                                        "+",
                                        paste("(", paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                            ),
                                            collapse = "+"
                                        ), ")^2", sep = ""),
                                        sep = ""
                                    ))
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(
                                        paste(
                                            paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Variables
                                                ),
                                                collapse = "+"
                                            ),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            sep = ""
                                        )
                                    )
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(paste(
                                        paste("(", paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                            ),
                                            collapse = "+"
                                        ), ")^2", sep = ""),
                                        "+",
                                        paste("(", paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                            ),
                                            collapse = "+"
                                        ), ")^2", sep = ""),
                                        sep = ""
                                    ))
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(paste(
                                        "(", paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                            ),
                                            collapse = "+"
                                        ), ")^2",
                                        sep = ""
                                    ))
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                }

                # 3rd
                else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(paste(
                                        "(", paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                            ),
                                            collapse = "+"
                                        ), ")^3",
                                        sep = ""
                                    ))
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(paste(
                                        paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Variables
                                            ),
                                            collapse = "+"
                                        ),
                                        "+",
                                        paste("(", paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                            ),
                                            collapse = "+"
                                        ), ")^3", sep = ""),
                                        sep = ""
                                    ))
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(paste(
                                        paste("(", paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                            ),
                                            collapse = "+"
                                        ), ")^2", sep = ""),
                                        "+",
                                        paste("(", paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                            ),
                                            collapse = "+"
                                        ), ")^3", sep = ""),
                                        sep = ""
                                    ))
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(paste(
                                        paste("(", paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                            ),
                                            collapse = "+"
                                        ), ")^2", sep = ""),
                                        "+",
                                        paste("(", paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                            ),
                                            collapse = "+"
                                        ), ")^3", sep = ""),
                                        sep = ""
                                    ))
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(
                                        paste(
                                            paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Variables
                                                ),
                                                collapse = "+"
                                            ),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                                ),
                                                collapse = "+"
                                            ), ")^3", sep = ""),
                                            sep = ""
                                        )
                                    )
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(
                                        paste(
                                            paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Variables
                                                ),
                                                collapse = "+"
                                            ),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                                ),
                                                collapse = "+"
                                            ), ")^3", sep = ""),
                                            sep = ""
                                        )
                                    )
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(
                                        paste(
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                                ),
                                                collapse = "+"
                                            ), ")^3", sep = ""),
                                            sep = ""
                                        )
                                    )
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        summary(coxph(
                            formula = as.formula(
                                paste(
                                    "Surv(as.numeric(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Survival_Time
                                    ),
                                    "), as.numeric(as.character(",
                                    ..(
                                        input$Tab7_Multivariable_Cox_Event_Status
                                    ),
                                    "))) ~",
                                    noquote(
                                        paste(
                                            paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Variables
                                                ),
                                                collapse = "+"
                                            ),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                                ),
                                                collapse = "+"
                                            ), ")^3", sep = ""),
                                            sep = ""
                                        )
                                    )
                                )
                            ),
                            data = ..(surv_data_CoxM())
                        ))
                    })
                } else {
                    return(NULL)
                }
            })

            output$CoxModelMultiOut <- metaRender(renderPrint, {
                ..(CoxModel())
            })
            output$Waldtestid <- metaRender(renderPrint, {
                ..(CoxModel())$waldtest
            })
            output$LRTid <- metaRender(renderPrint, {
                ..(CoxModel())$logtest
            })
            output$Logrid <- metaRender(renderPrint, {
                ..(CoxModel())$sctest
            })

            CoxAssump <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "\n \n Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file.",
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
                        "\n \n Please only select cBioPortal dataset OR upload your own data.",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))

                validate(
                    need(
                        !is.null(input$Tab7_Multivariable_Cox_Select_Variables) &
                            !is.null(
                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                            ) &
                            !is.null(
                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                            ) &
                            !is.null(
                                input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                            ),
                        "Please make sure all select boxes are filled \n \n \n"
                    )
                )

                if (isTRUE(
                    is.null(datalist1[[data]]()) |
                        input$Tab7_Multivariable_Cox_Survival_Time == "None Selected" |
                        input$Tab7_Multivariable_Cox_Event_Status == "None Selected"
                )) {
                    return(NULL)
                }

                # 1st
                else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(paste(
                                            ..(
                                                input$Tab7_Multivariable_Cox_Select_Variables
                                            ),
                                            collapse = "+"
                                        ))
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(paste(
                                            paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Variables
                                                ),
                                                collapse = "+"
                                            ),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            sep = ""
                                        ))
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(paste(
                                            "(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                                ),
                                                collapse = "+"
                                            ), ")^2",
                                            sep = ""
                                        ))
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                }

                # 2nd
                else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(paste(
                                            paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Variables
                                                ),
                                                collapse = "+"
                                            ),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            sep = ""
                                        ))
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(
                                            paste(
                                                paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Variables
                                                    ),
                                                    collapse = "+"
                                                ),
                                                "+",
                                                paste("(", paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                                    ),
                                                    collapse = "+"
                                                ), ")^2", sep = ""),
                                                "+",
                                                paste("(", paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                                    ),
                                                    collapse = "+"
                                                ), ")^2", sep = ""),
                                                sep = ""
                                            )
                                        )
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(paste(
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            sep = ""
                                        ))
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3 == "None Selected"
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(paste(
                                            "(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                                ),
                                                collapse = "+"
                                            ), ")^2",
                                            sep = ""
                                        ))
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                }

                # 3rd
                else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(paste(
                                            "(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                                ),
                                                collapse = "+"
                                            ), ")^3",
                                            sep = ""
                                        ))
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(paste(
                                            paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Variables
                                                ),
                                                collapse = "+"
                                            ),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                                ),
                                                collapse = "+"
                                            ), ")^3", sep = ""),
                                            sep = ""
                                        ))
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(paste(
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                                ),
                                                collapse = "+"
                                            ), ")^3", sep = ""),
                                            sep = ""
                                        ))
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(paste(
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                                ),
                                                collapse = "+"
                                            ), ")^2", sep = ""),
                                            "+",
                                            paste("(", paste(
                                                ..(
                                                    input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                                ),
                                                collapse = "+"
                                            ), ")^3", sep = ""),
                                            sep = ""
                                        ))
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(
                                            paste(
                                                paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Variables
                                                    ),
                                                    collapse = "+"
                                                ),
                                                "+",
                                                paste("(", paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                                    ),
                                                    collapse = "+"
                                                ), ")^2", sep = ""),
                                                "+",
                                                paste("(", paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                                    ),
                                                    collapse = "+"
                                                ), ")^3", sep = ""),
                                                sep = ""
                                            )
                                        )
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(
                                            paste(
                                                paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Variables
                                                    ),
                                                    collapse = "+"
                                                ),
                                                "+",
                                                paste("(", paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                                    ),
                                                    collapse = "+"
                                                ), ")^2", sep = ""),
                                                "+",
                                                paste("(", paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                                    ),
                                                    collapse = "+"
                                                ), ")^3", sep = ""),
                                                sep = ""
                                            )
                                        )
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else if (isTRUE(
                    input$Tab7_Multivariable_Cox_Select_Variables == "None Selected" &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(
                                            paste(
                                                paste("(", paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                                    ),
                                                    collapse = "+"
                                                ), ")^2", sep = ""),
                                                "+",
                                                paste("(", paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                                    ),
                                                    collapse = "+"
                                                ), ")^2", sep = ""),
                                                "+",
                                                paste("(", paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                                    ),
                                                    collapse = "+"
                                                ), ")^3", sep = ""),
                                                sep = ""
                                            )
                                        )
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else if (isTRUE(
                    "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Variables &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2 &
                        "None Selected" %!in% input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                )) {
                    metaExpr({
                        coxph(
                            formula =
                                as.formula(
                                    paste(
                                        "Surv(as.numeric(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Survival_Time
                                        ),
                                        "), as.numeric(as.character(",
                                        ..(
                                            input$Tab7_Multivariable_Cox_Event_Status
                                        ),
                                        "))) ~",
                                        noquote(
                                            paste(
                                                paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Variables
                                                    ),
                                                    collapse = "+"
                                                ),
                                                "+",
                                                paste("(", paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_1
                                                    ),
                                                    collapse = "+"
                                                ), ")^2", sep = ""),
                                                "+",
                                                paste("(", paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_2
                                                    ),
                                                    collapse = "+"
                                                ), ")^2", sep = ""),
                                                "+",
                                                paste("(", paste(
                                                    ..(
                                                        input$Tab7_Multivariable_Cox_Select_Interaction_Variables_3
                                                    ),
                                                    collapse = "+"
                                                ), ")^3", sep = ""),
                                                sep = ""
                                            )
                                        )
                                    )
                                ),
                            data = ..(surv_data_CoxM())
                        )
                    })
                } else {
                    return(NULL)
                }
            })

            Assump <- metaReactive2({
                if (isTRUE(
                    is.null(datalist1[[data]]()) |
                        input$Tab7_Multivariable_Cox_Survival_Time == "None Selected" |
                        input$Tab7_Multivariable_Cox_Event_Status == "None Selected" |
                        input$Tab7_Multivariable_Cox_Select_Variables == "None Selected"
                )) {
                    ggplot() +
                        theme_void()
                } else {
                    metaExpr({
                        ggcoxzph(cox.zph(
                            ..(CoxAssump()),
                            terms = ..(
                                input$Tab7_Cox_Assumptions_Display_by_Variable
                            )
                        ))
                    })
                }
            })

            output$AssumptionsCox <- metaRender(renderPlot, {
                ..(Assump())
            })

            return(list(A_Plot = Assump, Previous_Model = CoxAssump))
        })
    }
