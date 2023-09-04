# Server function to produce recursive partitioning survival trees (rpart)
Tab9_RPST_Rpart_Server <-
    function(id, datalist, datalist1, data, rowselect) {
        moduleServer(id, function(input, output, session) {
            observe({
                vchoicesRpart <- c(names(datalist1[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab9_Rpart_Select_Variables",
                    choices = vchoicesRpart,
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab9_Rpart_Survival_Time",
                    choices = vchoicesRpart,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab9_Rpart_Event_Status",
                    choices = vchoicesRpart,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            FormulaRpart <- metaReactive2({
                metaExpr({
                    noquote(paste(
                        ..(input$Tab9_Rpart_Select_Variables),
                        collapse = "+"
                    ))
                })
            })
            output$RpartTreeForm <- metaRender(renderPrint, {
                ..(FormulaRpart())
            })

            Whole_Data_Rpart <- metaReactive2({
                metaExpr({
                    ..(datalist1[[data]]())[..(datalist1[[data]]())[, ..(input$Tab9_Rpart_Survival_Time)] > 0, ] %>%
                        GNOSIS:::completeFun(data = ., c(
                            ..(input$Tab9_Rpart_Survival_Time),
                            ..(input$Tab9_Rpart_Event_Status),
                            ..(input$Tab9_Rpart_Select_Variables)
                        ))
                })
            })

            pfit <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file",
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
                        "Please only select cBioPortal dataset OR upload your own data",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab9_Rpart_Select_Variables),
                        "Please select variables to include in rpart survival tree"
                    )
                )
                if (input$Tab9_Rpart_Survival_Time == "None Selected" |
                    input$Tab9_Rpart_Event_Status == "None Selected") {
                    return(NULL)
                } else {
                    metaExpr({
                        rpart::rpart(
                            paste(
                                "survival::Surv(",
                                "as.numeric(",
                                ..(input$Tab9_Rpart_Survival_Time),
                                ")",
                                ",",
                                ..(input$Tab9_Rpart_Event_Status),
                                ") ~ ",
                                ..(FormulaRpart()),
                                sep = ""
                            ),
                            data = ..(Whole_Data_Rpart()),
                            method = "exp",
                            model = TRUE,
                            control = rpart::rpart.control(
                                minsplit = ..(input$Tab9_Rpart_Minsplit),
                                minbucket = ..(input$Tab9_Rpart_Minbucket),
                                cp = ..(input$Tab9_Rpart_Cp),
                                xval = ..(input$Tab9_Rpart_Xval),
                                maxdepth = ..(input$Tab9_Rpart_Maxdepth)
                            )
                        )
                    })
                }
            })

            RpartTreePlot_Re <- metaReactive2({
                if (is.null(pfit())) {
                    ggplot() +
                        theme_void()
                } else {
                    metaExpr({
                        partykit::as.party(..(pfit()))
                    })
                }
            })

            output$RpartTreePlot <- metaRender(renderPlot, {
                plot(..(RpartTreePlot_Re()))
            })

            data_node_info <- metaReactive2({
                metaExpr({
                    ..(Whole_Data_Rpart()) %>%
                        mutate(Node_Rpart = as.factor(..(pfit())$where)) %>%
                        data.frame(
                            Time = .[[..(input$Tab9_Rpart_Survival_Time)]],
                            Node = .[["Node_Rpart"]],
                            Cen = .[[..(input$Tab9_Rpart_Event_Status)]]
                        ) %>%
                        select(Time, Node, Cen)
                })
            })

            Surv_Curve <- metaReactive2({
                metaExpr({
                    survival::survfit(
                        survival::Surv(as.numeric(Time), as.numeric(as.character(
                            Cen
                        ))) ~ Node,
                        data = ..(data_node_info())
                    )
                })
            })

            PercentSurvPlotRpart <- metaReactive2({
                if (is.null(datalist1[[data]]()) |
                    input$Tab9_Rpart_Survival_Time == "None Selected" |
                    input$Tab9_Rpart_Event_Status == "None Selected") {
                    ggplot() +
                        theme_void()
                } else {
                    metaExpr({
                        ggsurvplot(
                            ..(Surv_Curve()),
                            censor.shape = "",
                            xlab = ..(input$Tab9_Surv_Rpart_X_Axis_Title),
                            ylab = ..(input$Tab9_Surv_Rpart_Y_Axis_Title),
                            data = ..(data_node_info()),
                            size = 1,
                            conf.int = ..(input$Tab9_Surv_Rpart_Display_CI),
                            risk.table = ..(
                                input$Tab9_Surv_Rpart_Display_Risk_Table
                            ),
                            pval = ..(input$Tab9_Surv_Rpart_Display_Pval),
                            legend = c(
                                ..(input$Tab9_Surv_Rpart_Legend_Position)
                            ),
                            legend.labs = rownames(summary(
                                ..(Surv_Curve())$table
                            )),
                            risk.table.height = 0.25,
                            pval.size = 6,
                            ggtheme = theme_bw() + theme(plot.title = element_text(
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
                            legend.title = ..(input$Tab9_Surv_Rpart_Legend_Title),
                            title = (..(
                                input$Tab9_Surv_Rpart_Plot_Title
                            )),
                            font.main = c(18, "plain", "black"),
                            font.x = c(15, "plain", "black"),
                            font.y = c(15, "plain", "black"),
                            font.legend = c(14, "plain", "black"),
                            font.tickslab = c(12, "plain", "black")
                        )
                    })
                }
            })

            output$Surv_Curve1 <- metaRender(renderPlot, {
                ..(PercentSurvPlotRpart())
            })

            return(list(Rpart_plot = RpartTreePlot_Re, Rpart_KM_Curves = PercentSurvPlotRpart))
        })
    }

# Server function to produce recursive partitioning survival trees (ctree)
Tab9_RPST_Ctree_Server <-
    function(id, datalist, datalist1, data, rowselect) {
        moduleServer(id, function(input, output, session) {
            observe({
                vchoicesRpart <- c(names(datalist1[[data]]()), "None Selected")
                updateSelectizeInput(
                    session,
                    "Tab9_Ctree_Select_Variables",
                    choices = vchoicesRpart,
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab9_Ctree_Survival_Time",
                    choices = vchoicesRpart,
                    selected = "None Selected",
                    server = TRUE
                )
                updateSelectizeInput(
                    session,
                    "Tab9_Ctree_Event_Status",
                    choices = vchoicesRpart,
                    selected = "None Selected",
                    server = TRUE
                )
            })

            FormulaCtree <- metaReactive2({
                metaExpr({
                    noquote(paste(
                        ..(input$Tab9_Ctree_Select_Variables),
                        collapse = "+"
                    ))
                })
            })
            output$CtreeTreeForm <- metaRender(renderPrint, {
                ..(FormulaCtree())
            })

            Whole_Data_Ctree <- metaReactive2({
                if (input$Tab9_Ctree_Use_Complete_Cases_Only == TRUE) {
                    metaExpr({
                        ..(datalist1[[data]]()) %>% GNOSIS:::completeFun(data = ., c(
                            ..(input$Tab9_Ctree_Survival_Time),
                            ..(input$Tab9_Ctree_Event_Status),
                            ..(input$Tab9_Ctree_Select_Variables)
                        ))
                    })
                } else {
                    metaExpr({
                        ..(datalist1[[data]]()) %>% GNOSIS:::completeFun(data = ., c(..(
                            input$Tab9_Ctree_Event_Status
                        )))
                    })
                }
            })

            pfitctree <- metaReactive2({
                validate(need(
                    !is.null(rowselect()) |
                        !is.null(datalist[["patient_manual_data"]]()) |
                        !is.null(datalist[["sample_manual_data"]]()) |
                        !is.null(datalist[["CNA_manual_data"]]()),
                    paste(
                        "Please select cBioPortal dataset or upload your own clinical file, sample file and/or CNA file",
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
                        "Please only select cBioPortal dataset OR upload your own data",
                        "\n",
                        "\n",
                        "\n"
                    )
                ))
                validate(
                    need(
                        !is.null(input$Tab9_Ctree_Select_Variables) &
                            input$Tab9_Ctree_Select_Variables != "None Selected",
                        "Please select variables to include in ctree survival tree"
                    )
                )
                if (isTRUE(
                    input$Tab9_Ctree_Survival_Time == "None Selected" |
                        input$Tab9_Ctree_Event_Status == "None Selected"
                )) {
                    return(NULL)
                } else {
                    metaExpr({
                        partykit::ctree(
                            as.formula(
                                paste(
                                    "survival::Surv(",
                                    ..(input$Tab9_Ctree_Survival_Time),
                                    ",",
                                    ..(input$Tab9_Ctree_Event_Status),
                                    ") ~ ",
                                    ..(FormulaCtree()),
                                    sep = ""
                                )
                            ),
                            data = ..(Whole_Data_Ctree()),
                            control = partykit::ctree_control(
                                teststat = ..(input$Tab9_Ctree_Teststat),
                                splitstat = ..(input$Tab9_Ctree_Splitstat),
                                testtype = ..(input$Tab9_Ctree_Testtype),
                                alpha = ..(input$Tab9_Ctree_Alpha),
                                minsplit = ..(input$Tab9_Ctree_Minsplit),
                                minbucket = ..(input$Tab9_Ctree_Minbucket),
                                minprob = ..(input$Tab9_Ctree_Minprob),
                                stump = ..(input$Tab9_Ctree_Stump),
                                maxvar = ..(input$Tab9_Ctree_Maxvar),
                                maxdepth = ..(input$Tab9_Ctree_Maxdepth)
                            )
                        )
                    })
                }
            })

            CtreeTreePlot_Re <- metaReactive2({
                if (is.null(pfitctree())) {
                    ggplot() +
                        theme_void()
                } else {
                    metaExpr({
                        ..(pfitctree())
                    })
                }
            })

            output$CTreePlot <- metaRender(renderPlot, {
                plot(..(CtreeTreePlot_Re()))
            })

            data_node_info_ctree <- metaReactive2({
                metaExpr({
                    ..(Whole_Data_Ctree()) %>%
                        mutate(Node_Ctree = as.factor(stats::predict(..(
                            pfitctree()
                        ), type = "node"))) %>%
                        data.frame(
                            Time = .[[..(input$Tab9_Ctree_Survival_Time)]],
                            Node = .[["Node_Ctree"]],
                            Cen = .[[..(input$Tab9_Ctree_Event_Status)]]
                        ) %>%
                        select(Time, Node, Cen)
                })
            })

            Surv_Curvectree <- metaReactive2({
                metaExpr({
                    survival::survfit(
                        survival::Surv(as.numeric(Time), as.numeric(as.character(
                            Cen
                        ))) ~ Node,
                        data = ..(data_node_info_ctree())
                    )
                })
            })

            PercentSurvPlotCtree <- metaReactive2({
                if (isTRUE(
                    is.null(datalist1[[data]]()) |
                        input$Tab9_Ctree_Survival_Time == "None Selected" |
                        input$Tab9_Ctree_Event_Status == "None Selected"
                )) {
                    ggplot() +
                        theme_void()
                } else {
                    metaExpr({
                        survminer::ggsurvplot(
                            ..(Surv_Curvectree()),
                            censor.shape = "",
                            xlab = ..(input$Tab9_Surv_Ctree_X_Axis_Title),
                            ylab = ..(input$Tab9_Surv_Ctree_Y_Axis_Title),
                            data = ..(data_node_info_ctree()),
                            size = 1,
                            conf.int = ..(input$Tab9_Surv_Ctree_Display_CI),
                            risk.table = ..(
                                input$Tab9_Surv_Ctree_Display_Risk_Table
                            ),
                            pval = ..(input$Tab9_Surv_Ctree_Display_Pval),
                            legend = c(
                                ..(input$Tab9_Surv_Ctree_Legend_Position)
                            ),
                            legend.labs = rownames(summary(
                                ..(Surv_Curvectree())$table
                            )),
                            risk.table.height = 0.25,
                            pval.size = 6,
                            ggtheme = theme_bw() + theme(plot.title = element_text(
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
                            legend.title = ..(input$Tab9_Surv_Ctree_Legend_Title),
                            title = (..(
                                input$Tab9_Surv_Ctree_Plot_Title
                            )),
                            font.main = c(18, "plain", "black"),
                            font.x = c(15, "plain", "black"),
                            font.y = c(15, "plain", "black"),
                            font.legend = c(14, "plain", "black"),
                            font.tickslab = c(12, "plain", "black")
                        )
                    })
                }
            })

            output$Surv_CurveCtree <- metaRender(renderPlot, {
                ..(PercentSurvPlotCtree())
            })

            return(list(Ctree_plot = CtreeTreePlot_Re, Ctree_KM_Curves = PercentSurvPlotCtree))
        })
    }
