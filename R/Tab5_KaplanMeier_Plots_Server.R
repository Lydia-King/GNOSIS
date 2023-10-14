# Server function to produce survival curves and log rank tests for clinical variables
Tab5_KM_Plot_Server <- function(id, datalist, data) {
    moduleServer(id, function(input, output, session) {
        observe({
            vchoicesSurv1 <- c(names(datalist[[data]]()), "None Selected")
            updateSelectizeInput(
                session,
                "Tab5_KM_Clinical_Survival_Time",
                choices = vchoicesSurv1,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab5_KM_Clinical_Event_Status",
                choices = vchoicesSurv1,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab5_KM_Clinical_Select_Variable",
                choices = vchoicesSurv1,
                selected = "None Selected",
                server = TRUE
            )
        })

        surv_data <- metaReactive2({
            metaExpr({
                data.frame(
                    Time = ..(datalist[[data]]())[[..(input$Tab5_KM_Clinical_Survival_Time)]],
                    Strata = ..(datalist[[data]]())[[..(input$Tab5_KM_Clinical_Select_Variable)]],
                    Cen = ..(datalist[[data]]())[[..(input$Tab5_KM_Clinical_Event_Status)]]
                )
            })
        })

        datafit <- metaReactive2({
            metaExpr({
                survfit(
                    Surv(as.numeric(Time), as.numeric(as.character(
                        Cen
                    ))) ~ Strata,
                    data = ..(surv_data())
                )
            })
        })

        KMC1 <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab5_KM_Clinical_Survival_Time == "None Selected" |
                input$Tab5_KM_Clinical_Event_Status == "None Selected" |
                input$Tab5_KM_Clinical_Select_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else {
                metaExpr({
                    ggsurvplot(
                        ..(datafit()),
                        censor.shape = "",
                        data = ..(surv_data()),
                        size = 1,
                        conf.int = ..(input$Tab5_KM_Clinical_Display_CI),
                        pval = ..(input$Tab5_KM_Clinical_Display_Pval),
                        risk.table = ..(
                            input$Tab5_KM_Clinical_Display_Risk_Table
                        ),
                        legend = c(
                            ..(input$Tab5_KM_Clinical_Legend_Position)
                        ),
                        xlab = ..(input$Tab5_KM_Clinical_X_Axis_Title),
                        ylab = ..(input$Tab5_KM_Clinical_Y_Axis_Title),
                        legend.title = ..(input$Tab5_KM_Clinical_Legend_Title),
                        legend.labs = rownames(summary(..(
                            datafit()
                        )$table)),
                        pval.size = 6,
                        risk.table.height = 0.25,
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
                        title = (..(
                            input$Tab5_KM_Clinical_Plot_Title
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

        Logrank1 <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab5_KM_Clinical_Survival_Time == "None Selected" |
                input$Tab5_KM_Clinical_Event_Status == "None Selected" |
                input$Tab5_KM_Clinical_Select_Variable == "None Selected") {
                return(NULL)
            } else {
                metaExpr({
                    survdiff(Surv(
                        ..(datalist[[data]](
                        ))[, ..(input$Tab5_KM_Clinical_Survival_Time)],
                        as.numeric(..(
                            datalist[[data]]()
                        )[, ..(input$Tab5_KM_Clinical_Event_Status)])
                    ) ~
                        ..(datalist[[data]]())[, ..(input$Tab5_KM_Clinical_Select_Variable)])
                })
            }
        })

        output$KMlogrank <- metaRender(renderPrint, {
            ..(Logrank1())
        })

        return(list(surv_plot = KMC1))
    })
}

# Server function to produce survival curves and log rank tests for quartiles
Tab5_KM_Plot_Quart_Server <- function(id, datalist, data) {
    moduleServer(id, function(input, output, session) {
        observe({
            vchoicesSurv2 <- c(names(datalist[[data]]()), "None Selected")
            updateSelectizeInput(
                session,
                "Tab5_KM_CNA_Survival_Time",
                choices = vchoicesSurv2,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab5_KM_CNA_Event_Status",
                choices = vchoicesSurv2,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab5_KM_CNA_Select_Variable",
                choices = vchoicesSurv2,
                selected = "None Selected",
                server = TRUE
            )
        })

        surv_data_1 <- metaReactive2({
            metaExpr({
                data.frame(
                    Time = ..(datalist[[data]]())[[..(input$Tab5_KM_CNA_Survival_Time)]],
                    Strata = ..(datalist[[data]]())[[..(input$Tab5_KM_CNA_Select_Variable)]],
                    cen = ..(datalist[[data]]())[[..(input$Tab5_KM_CNA_Event_Status)]]
                )
            })
        })

        survfitCNA <- metaReactive2({
            metaExpr({
                survfit(
                    Surv(as.numeric(Time), as.numeric(as.character(
                        cen
                    ))) ~ Strata,
                    data = ..(surv_data_1())
                )
            })
        })

        PercentSurvPlot <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab5_KM_CNA_Survival_Time == "None Selected" |
                input$Tab5_KM_CNA_Event_Status == "None Selected" |
                input$Tab5_KM_CNA_Select_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else {
                metaExpr({
                    ggsurvplot(
                        ..(survfitCNA()),
                        censor.shape = "",
                        xlab = ..(input$Tab5_KM_CNA_X_Axis_Title),
                        ylab = ..(input$Tab5_KM_CNA_Y_Axis_Title),
                        data = ..(surv_data_1()),
                        size = 1,
                        conf.int = ..(input$Tab5_KM_CNA_Display_CI),
                        pval = ..(input$Tab5_KM_CNA_Display_Pval),
                        risk.table = ..(input$Tab5_KM_CNA_Display_Risk_Table),
                        legend = c(..(
                            input$Tab5_KM_CNA_Legend_Position
                        )),
                        legend.labs = rownames(summary(
                            ..(survfitCNA())$table
                        )),
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
                        legend.title = ..(input$Tab5_KM_CNA_Legend_Title),
                        title = (..(
                            input$Tab5_KM_CNA_Plot_Title
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

        Logrank2 <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab5_KM_CNA_Survival_Time == "None Selected" |
                input$Tab5_KM_CNA_Event_Status == "None Selected" |
                input$Tab5_KM_CNA_Select_Variable == "None Selected") {
                return(NULL)
            } else {
                metaExpr({
                    survdiff(Surv(
                        ..(datalist[[data]](
                        ))[, ..(input$Tab5_KM_CNA_Survival_Time)],
                        as.numeric(..(
                            datalist[[data]]()
                        )[, ..(input$Tab5_KM_CNA_Event_Status)])
                    ) ~
                        ..(datalist[[data]]())[, ..(input$Tab5_KM_CNA_Select_Variable)])
                })
            }
        })

        output$KMlogrank1 <- metaRender(renderPrint, {
            ..(Logrank2())
        })

        return(list(surv_plot = PercentSurvPlot))
    })
}

# Server function to produce survival curves and log rank tests for treatment splits (binary variable == "Yes" or "No")
Tab5_KM_Plot_Treatment_Server <- function(id, datalist, data) {
    moduleServer(id, function(input, output, session) {
        observe({
            vchoicesSurvsplit <- c(names(datalist[[data]]()), "None Selected")
            updateSelectizeInput(
                session,
                "Tab5_KM_Treatment_Survival_Time",
                choices = vchoicesSurvsplit,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab5_KM_Treatment_Event_Status",
                choices = vchoicesSurvsplit,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab5_KM_Treatment_Select_Variable",
                choices = vchoicesSurvsplit,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab5_KM_Treatment_Variable",
                choices = vchoicesSurvsplit,
                selected = "None Selected",
                server = TRUE
            )
        })

        dataRYes <- metaReactive2({
            metaExpr({
                ..(datalist[[data]]())[..(datalist[[data]]())[, ..(input$Tab5_KM_Treatment_Variable)] == "YES", ]
            })
        })
        dataRNo <- metaReactive2({
            metaExpr({
                ..(datalist[[data]]())[..(datalist[[data]]())[, ..(input$Tab5_KM_Treatment_Variable)] == "NO", ]
            })
        })

        surv_data_R <- metaReactive2({
            metaExpr({
                data.frame(
                    Time = ..(dataRYes())[[..(input$Tab5_KM_Treatment_Survival_Time)]],
                    Strata = ..(dataRYes())[[..(input$Tab5_KM_Treatment_Select_Variable)]],
                    Cen = ..(dataRYes())[[..(input$Tab5_KM_Treatment_Event_Status)]]
                )
            })
        })

        datafit_R <- metaReactive2({
            metaExpr({
                survfit(
                    Surv(as.numeric(Time), as.numeric(as.character(
                        Cen
                    ))) ~ Strata,
                    data = ..(surv_data_R())
                )
            })
        })

        KMR1Plot <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab5_KM_Treatment_Survival_Time == "None Selected" |
                input$Tab5_KM_Treatment_Event_Status == "None Selected" |
                input$Tab5_KM_Treatment_Select_Variable == "None Selected" |
                input$Tab5_KM_Treatment_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else {
                metaExpr({
                    ggsurvplot(
                        ..(datafit_R()),
                        censor.shape = "",
                        xlab = ..(
                            input$Tab5_KM_Treatment_Yes_X_Axis_Title
                        ),
                        ylab = ..(
                            input$Tab5_KM_Treatment_Yes_Y_Axis_Title
                        ),
                        data = ..(surv_data_R()),
                        size = 1,
                        conf.int = ..(input$Tab5_KM_Treatment_Yes_Display_CI),
                        pval = ..(
                            input$Tab5_KM_Treatment_Yes_Display_Pval
                        ),
                        risk.table = ..(
                            input$Tab5_KM_Treatment_Yes_Display_Risk_Table
                        ),
                        legend.title = ..(
                            input$Tab5_KM_Treatment_Yes_Legend_Title
                        ),
                        legend = c(
                            ..(
                                input$Tab5_KM_Treatment_Yes_Legend_Position
                            )
                        ),
                        legend.labs = rownames(summary(
                            ..(datafit_R())$table
                        )),
                        pval.size = 6,
                        risk.table.height = 0.25,
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
                        title = (..(
                            input$Tab5_KM_Treatment_Yes_Title
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

        Logrank3 <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab5_KM_Treatment_Survival_Time == "None Selected" |
                input$Tab5_KM_Treatment_Event_Status == "None Selected" |
                input$Tab5_KM_Treatment_Select_Variable == "None Selected" |
                input$Tab5_KM_Treatment_Variable == "None Selected") {
                return(NULL)
            } else {
                metaExpr({
                    survdiff(Surv(
                        ..(dataRYes())[, ..(input$Tab5_KM_Treatment_Survival_Time)],
                        as.numeric(..(
                            dataRYes()
                        )[, ..(input$Tab5_KM_Treatment_Event_Status)])
                    ) ~
                        ..(dataRYes())[, ..(input$Tab5_KM_Treatment_Select_Variable)])
                })
            }
        })

        output$KMlogrankYes <- metaRender(renderPrint, {
            ..(Logrank3())
        })

        Logrank4 <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab5_KM_Treatment_Survival_Time == "None Selected" |
                input$Tab5_KM_Treatment_Event_Status == "None Selected" |
                input$Tab5_KM_Treatment_Select_Variable == "None Selected" |
                input$Tab5_KM_Treatment_Variable == "None Selected") {
                return(NULL)
            } else {
                metaExpr({
                    survdiff(Surv(
                        ..(dataRNo())[, ..(input$Tab5_KM_Treatment_Survival_Time)],
                        as.numeric(..(
                            dataRNo()
                        )[, ..(input$Tab5_KM_Treatment_Event_Status)])
                    ) ~
                        ..(dataRNo())[, ..(input$Tab5_KM_Treatment_Select_Variable)])
                })
            }
        })

        output$KMlogrankNo <- metaRender(renderPrint, {
            ..(Logrank4())
        })

        surv_data_NR <- metaReactive2({
            metaExpr({
                data.frame(
                    Time = ..(dataRNo())[[..(input$Tab5_KM_Treatment_Survival_Time)]],
                    Strata = ..(dataRNo())[[..(input$Tab5_KM_Treatment_Select_Variable)]],
                    Cen = ..(dataRNo())[[..(input$Tab5_KM_Treatment_Event_Status)]]
                )
            })
        })

        datafit_NR <- metaReactive2({
            metaExpr({
                survfit(
                    Surv(as.numeric(Time), as.numeric(as.character(
                        Cen
                    ))) ~ Strata,
                    data = ..(surv_data_NR())
                )
            })
        })

        KMR2Plot <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab5_KM_Treatment_Survival_Time == "None Selected" |
                input$Tab5_KM_Treatment_Event_Status == "None Selected" |
                input$Tab5_KM_Treatment_Select_Variable == "None Selected" |
                input$Tab5_KM_Treatment_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else {
                metaExpr({
                    ggsurvplot(
                        ..(datafit_NR()),
                        censor.shape = "",
                        xlab = ..(
                            input$Tab5_KM_Treatment_No_X_Axis_Title
                        ),
                        ylab = ..(
                            input$Tab5_KM_Treatment_No_Y_Axis_Title
                        ),
                        data = ..(surv_data_NR()),
                        size = 1,
                        conf.int = ..(input$Tab5_KM_Treatment_No_Display_CI),
                        pval = ..(
                            input$Tab5_KM_Treatment_No_Display_Pval
                        ),
                        risk.table = ..(
                            input$Tab5_KM_Treatment_No_Display_Risk_Table
                        ),
                        legend.title = ..(
                            input$Tab5_KM_Treatment_No_Legend_Title
                        ),
                        legend = c(
                            ..(
                                input$Tab5_KM_Treatment_No_Legend_Position
                            )
                        ),
                        legend.labs = rownames(summary(
                            ..(datafit_NR())$table
                        )),
                        pval.size = 6,
                        risk.table.height = 0.25,
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
                        title = (..(
                            input$Tab5_KM_Treatment_No_Title
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

        output$Tab5_Download_KM_Treatment_Yes_PNG <-
            downloadHandler(
                filename = function() {
                    paste("KM_Survival_Treatment_Yes", ".png", sep = "")
                },
                content = function(file) {
                    png(
                        file,
                        width = input$Tab5_KM_Treatment_Yes_Width,
                        height = input$Tab5_KM_Treatment_Yes_Height,
                        units = "in",
                        res = 1200
                    )
                    KMR1Plot()
                    dev.off()
                }
            )

        output$Tab5_Download_KM_Treatment_Yes_SVG <-
            downloadHandler(
                filename = function() {
                    paste("KM_Survival_Treatment_Yes", ".svg", sep = "")
                },
                content = function(file) {
                    svg(
                        file,
                        width = input$Tab5_KM_Treatment_Yes_Width,
                        height = input$Tab5_KM_Treatment_Yes_Height
                    )
                    KMR1Plot()
                    dev.off()
                }
            )

        output$Tab5_Download_KM_Treatment_No_PNG <- downloadHandler(
            filename = function() {
                paste("KM_Survival_Treatment_No", ".png", sep = "")
            },
            content = function(file) {
                png(
                    file,
                    width = input$Tab5_KM_Treatment_No_Width,
                    height = input$Tab5_KM_Treatment_No_Height,
                    units = "in",
                    res = 1200
                )
                KMR2Plot()
                dev.off()
            }
        )

        output$Tab5_Download_KM_Treatment_No_SVG <- downloadHandler(
            filename = function() {
                paste("KM_Survival_Treatment_No", ".svg", sep = "")
            },
            content = function(file) {
                svg(
                    file,
                    width = input$Tab5_KM_Treatment_No_Width,
                    height = input$Tab5_KM_Treatment_No_Height
                )
                KMR2Plot()
                dev.off()
            }
        )

        output$Plot1 <- metaRender(renderPlot, {
            ..(KMR1Plot())
        })
        output$Plot2 <- metaRender(renderPlot, {
            ..(KMR2Plot())
        })
    })
}

# Server function to render plot
Tab5_Display_KM_Plot_Server <- function(id, datalist, plot) {
    moduleServer(id, function(input, output, session) {
        output$Plot <- metaRender(renderPlot, {
            ..(datalist[[plot]]())
        })
    })
}
