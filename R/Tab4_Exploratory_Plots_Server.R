# Server function to produce exploratory boxplots
Tab4_BoxPlots_Server <- function(id, datalist, data) {
    moduleServer(id, function(input, output, session) {
        observe({
            vchoicesbox1 <- c(names(datalist[[data]]()), "None Selected")
            updateSelectizeInput(
                session,
                "Tab4_Boxplot_Select_X_Variable",
                choices = vchoicesbox1,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab4_Boxplot_Select_Y_Variable",
                choices = vchoicesbox1,
                selected = "None Selected",
                server = TRUE
            )
        })

        boxplot <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab4_Boxplot_Select_X_Variable == "None Selected" |
                input$Tab4_Boxplot_Select_Y_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else {
                metaExpr({
                    ggplot(
                        data = ..(datalist[[data]]()),
                        mapping = aes(
                            x = ..(datalist[[data]]())[, ..(input$Tab4_Boxplot_Select_X_Variable)],
                            y = ..(datalist[[data]]())[, ..(input$Tab4_Boxplot_Select_Y_Variable)],
                            color = ..(datalist[[data]]())[, ..(input$Tab4_Boxplot_Select_X_Variable)]
                        )
                    ) +
                        geom_boxplot(
                            varwidth = ..(input$Tab4_Boxplot_by_Sample_Size),
                            na.rm = TRUE,
                            aes(fill = ..(datalist[[data]](
                            ))[, ..(input$Tab4_Boxplot_Select_X_Variable)]),
                            alpha = 0.35
                        ) +
                        geom_jitter(alpha = 0.3) +
                        ggtitle(..(input$Tab4_Boxplot_Title)) +
                        ylab(..(input$Tab4_Boxplot_Y_Axis_Title)) +
                        xlab(..(input$Tab4_Boxplot_X_Axis_Title)) +
                        theme_bw() +
                        theme(
                            plot.title = element_text(hjust = 0.5, size = 18),
                            axis.title.x = element_text(hjust = 0.5, size = 18),
                            axis.title.y = element_text(hjust = 0.5, size = 18),
                            axis.text.x = element_text(size = 15),
                            axis.text.y = element_text(size = 15),
                            legend.title = element_text(
                                colour = "black",
                                size = 15,
                                face = "bold"
                            ),
                            strip.text = element_text(size = 15),
                            legend.text = element_text(size = 15)
                        ) +
                        stat_summary(
                            fun.data = give.n,
                            geom = "text",
                            fun = median,
                            col = "black",
                            size = 4
                        ) +
                        labs(fill = ..(input$Tab4_Boxplot_Legend_Title)) +
                        scale_color_discrete(guide = FALSE) +
                        scale_x_discrete(na.translate = ..(input$Tab4_Boxplot_Display_NAs)) +
                        theme(legend.position = ..(input$Tab4_Boxplot_Legend_Position))
                })
            }
        })
        return(list(boxplot = boxplot))
    })
}

# Server function to produce exploratory scatterplots
Tab4_Scatterplot_Server <- function(id, datalist, data) {
    moduleServer(id, function(input, output, session) {
        observe({
            vchoicesASS <- c(names(datalist[[data]]()), "None Selected")
            updateSelectizeInput(
                session,
                "Tab4_Scatterplot_Select_X_Variable",
                choices = vchoicesASS,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab4_Scatterplot_Select_Y_Variable",
                choices = vchoicesASS,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab4_Scatterplot_Select_Colour_Var",
                choices = vchoicesASS,
                selected = "None Selected",
                server = TRUE
            )
        })

        scatterplot <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab4_Scatterplot_Select_X_Variable == "None Selected" |
                input$Tab4_Scatterplot_Select_Y_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else if (input$Tab4_Scatterplot_Select_X_Variable != "None Selected" &
                input$Tab4_Scatterplot_Select_Y_Variable != "None Selected" &
                input$Tab4_Scatterplot_Select_Colour_Var == "None Selected") {
                metaExpr({
                    ggplot(..(datalist[[data]]())) +
                        geom_point(
                            aes(
                                x = ..(datalist[[data]]())[, ..(input$Tab4_Scatterplot_Select_X_Variable)],
                                y = ..(datalist[[data]]())[, ..(input$Tab4_Scatterplot_Select_Y_Variable)]
                            ),
                            size = 1.5
                        ) +
                        ggtitle(..(input$Tab4_Scatterplot_Title)) +
                        ylab(..(input$Tab4_Scatterplot_Y_Axis_Title)) +
                        xlab(..(input$Tab4_Scatterplot_X_Axis_Title)) +
                        theme_bw() +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.text.x = element_text(size = 15)) +
                        theme(axis.text.y = element_text(size = 15)) +
                        theme(legend.title = element_text(
                            colour = "black",
                            size = 15,
                            face = "bold"
                        )) +
                        theme(strip.text = element_text(size = 15)) +
                        theme(legend.text = element_text(size = 15)) +
                        scale_colour_discrete(
                            ..(input$Tab4_Scatterplot_Legend_Title),
                            na.translate = ..(input$Tab4_Scatterplot_Display_NAs)
                        ) +
                        theme(legend.position = input$Tab4_Scatterplot_Legend_Position)
                })
            } else {
                metaExpr({
                    ggplot(..(datalist[[data]]())) +
                        geom_point(
                            aes(
                                x = ..(datalist[[data]]())[, ..(input$Tab4_Scatterplot_Select_X_Variable)],
                                y = ..(datalist[[data]]())[, ..(input$Tab4_Scatterplot_Select_Y_Variable)],
                                colour = ..(datalist[[data]]())[, ..(input$Tab4_Scatterplot_Select_Colour_Var)]
                            ),
                            size = 1.5
                        ) +
                        ggtitle(..(input$Tab4_Scatterplot_Title)) +
                        ylab(..(input$Tab4_Scatterplot_Y_Axis_Title)) +
                        xlab(..(input$Tab4_Scatterplot_X_Axis_Title)) +
                        theme_bw() +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.text.x = element_text(size = 15)) +
                        theme(axis.text.y = element_text(size = 15)) +
                        theme(legend.title = element_text(
                            colour = "black",
                            size = 15,
                            face = "bold"
                        )) +
                        theme(strip.text = element_text(size = 15)) +
                        theme(legend.text = element_text(size = 15)) +
                        scale_colour_discrete(
                            ..(input$Tab4_Scatterplot_Legend_Title),
                            na.translate = ..(input$Tab4_Scatterplot_Display_NAs)
                        ) +
                        theme(legend.position = ..(input$Tab4_Scatterplot_Legend_Position))
                })
            }
        })
        return(list(scatterplot = scatterplot))
    })
}

# Server function to produce exploratory barplots
Tab4_Barplot_Server <- function(id, datalist, data) {
    moduleServer(id, function(input, output, session) {
        observe({
            vchoicesASS <- c(names(datalist[[data]]()), "None Selected")
            updateSelectizeInput(
                session,
                "Tab4_Barplot_Select_X_Variable",
                choices = vchoicesASS,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab4_Barplot_Select_Y_Variable",
                choices = vchoicesASS,
                selected = "None Selected",
                server = TRUE
            )
        })

        barplot <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab4_Barplot_Select_X_Variable == "None Selected" |
                input$Tab4_Barplot_Select_Y_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else {
                metaExpr({
                    ggplot(
                        ..(datalist[[data]]()),
                        aes(
                            x = factor(..(datalist[[data]](
                            ))[, ..(input$Tab4_Barplot_Select_X_Variable)]),
                            fill = factor(..(datalist[[data]](
                            ))[, ..(input$Tab4_Barplot_Select_Y_Variable)])
                        )
                    ) +
                        geom_bar(position = "fill") +
                        ggtitle(..(input$Tab4_Barplot_Title)) +
                        ylab(..(input$Tab4_Barplot_Y_Axis_Title)) +
                        xlab(..(input$Tab4_Barplot_X_Axis_Title)) +
                        theme_bw() +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.text.x = element_text(size = 15)) +
                        theme(axis.text.y = element_text(size = 15)) +
                        theme(legend.title = element_text(
                            colour = "black",
                            size = 15,
                            face = "bold"
                        )) +
                        theme(strip.text = element_text(size = 15)) +
                        theme(legend.text = element_text(size = 15)) +
                        labs(fill = ..(input$Tab4_Barplot_Legend_Title)) +
                        theme(legend.position = ..(input$Tab4_Barplot_Legend_Position)) +
                        scale_x_discrete(na.translate = ..(input$Tab4_Barplot_Display_NAs))
                })
            }
        })
        return(list(barplot = barplot))
    })
}

# Server function to produce exploratory histograms and density plots -> CNA Score Density
Tab4_Hist_Server <- function(id, datalist, data) {
    moduleServer(id, function(input, output, session) {
        observe({
            vchoicesDist <- c(names(datalist[[data]]()), "None Selected")
            updateSelectizeInput(
                session,
                "Tab4_Select_Plot_Variable",
                choices = vchoicesDist,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab4_Both_Plot_Select_Facet_Variable",
                choices = vchoicesDist,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab4_Histogram_Select_Facet_Variable",
                choices = vchoicesDist,
                selected = "None Selected",
                server = TRUE
            )
        })

        observe({
            vchoicesASS <- c(names(datalist[[data]]()), "None Selected")
            updateSelectizeInput(
                session,
                "Tab4_Density_Plot_Select_Fill_Variable",
                choices = vchoicesASS,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab4_Both_Plot_Select_Fill_Variable",
                choices = vchoicesASS,
                selected = "None Selected",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "Tab4_Histogram_Select_Fill_Variable",
                choices = vchoicesASS,
                selected = "None Selected",
                server = TRUE
            )
        })

        CNAHistogram <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab4_Select_Plot_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else if (input$Tab4_Select_Plot_Variable != "None Selected" &
                input$Tab4_Histogram_Select_Fill_Variable == "None Selected") {
                metaExpr({
                    ggplot(..(datalist[[data]]()), aes(..(datalist[[data]](
                    ))[, ..(input$Tab4_Select_Plot_Variable)])) +
                        geom_histogram(
                            aes(color = "Color", fill = "Color"),
                            show.legend = FALSE,
                            binwidth = ..(input$Tab4_Histogram_Number_of_Bins),
                            alpha = ..(input$Tab4_Histogram_Alpha)
                        ) +
                        ggtitle(..(input$Tab4_Histogram_Title)) +
                        ylab(..(input$Tab4_Histogram_Y_Axis_Title)) +
                        xlab(..(input$Tab4_Histogram_X_Axis_Title)) +
                        scale_color_manual(values = c("Color" = "#2ac0db")) +
                        scale_fill_manual(values = c("Color" = "#2ac0db")) +
                        theme_bw() +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.text.x = element_text(size = 15)) +
                        theme(axis.text.y = element_text(size = 15)) +
                        theme(legend.title = element_text(
                            colour = "black",
                            size = 15,
                            face = "bold"
                        )) +
                        theme(strip.text = element_text(size = 15)) +
                        theme(legend.text = element_text(size = 15))
                })
            } else {
                if (input$Tab4_Histogram_Display_NAs == FALSE) {
                    metaExpr({
                        Data <-
                            completeFun(..(datalist[[data]]()), c(
                                ..(
                                    input$Tab4_Histogram_Select_Fill_Variable
                                )
                            ))
                        ggplot(Data, aes(Data[, ..(input$Tab4_Select_Plot_Variable)])) +
                            geom_histogram(
                                aes(
                                    fill = Data[, ..(input$Tab4_Histogram_Select_Fill_Variable)],
                                    color = Data[, ..(input$Tab4_Histogram_Select_Fill_Variable)]
                                ),
                                binwidth = ..(
                                    input$Tab4_Histogram_Number_of_Bins
                                ),
                                alpha = ..(input$Tab4_Histogram_Alpha)
                            ) +
                            ggtitle(..(input$Tab4_Histogram_Title)) +
                            ylab(..(input$Tab4_Histogram_Y_Axis_Title)) +
                            xlab(..(input$Tab4_Histogram_X_Axis_Title)) +
                            theme(legend.position = ..(input$LegPlainhisttab)) +
                            theme_bw() +
                            theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.text.x = element_text(size = 15)) +
                            theme(axis.text.y = element_text(size = 15)) +
                            theme(legend.title = element_text(
                                colour = "black",
                                size = 15,
                                face = "bold"
                            )) +
                            theme(strip.text = element_text(size = 15)) +
                            theme(legend.text = element_text(size = 15)) +
                            theme(legend.position = ..(
                                input$Tab4_Histogram_Legend_Position
                            )) +
                            scale_fill_discrete(..(input$Tab4_Histogram_Legend_Title)) +
                            scale_color_discrete(..(input$Tab4_Histogram_Legend_Title))
                    })
                } else {
                    metaExpr({
                        ggplot(..(datalist[[data]]()), aes(..(datalist[[data]](
                        ))[, ..(input$Tab4_Select_Plot_Variable)])) +
                            geom_histogram(
                                aes(
                                    fill = ..(datalist[[data]]())[, ..(input$Tab4_Histogram_Select_Fill_Variable)],
                                    color = ..(datalist[[data]]())[, ..(input$Tab4_Histogram_Select_Fill_Variable)]
                                ),
                                binwidth = ..(
                                    input$Tab4_Histogram_Number_of_Bins
                                ),
                                alpha = ..(input$Tab4_Histogram_Alpha)
                            ) +
                            ggtitle(..(input$Tab4_Histogram_Title)) +
                            ylab(..(input$Tab4_Histogram_Y_Axis_Title)) +
                            xlab(..(input$Tab4_Histogram_X_Axis_Title)) +
                            theme_bw() +
                            theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.text.x = element_text(size = 15)) +
                            theme(axis.text.y = element_text(size = 15)) +
                            theme(legend.title = element_text(
                                colour = "black",
                                size = 15,
                                face = "bold"
                            )) +
                            theme(strip.text = element_text(size = 15)) +
                            theme(legend.text = element_text(size = 15)) +
                            theme(legend.position = ..(
                                input$Tab4_Histogram_Legend_Position
                            )) +
                            scale_fill_discrete(..(input$Tab4_Histogram_Legend_Title)) +
                            scale_color_discrete(..(input$Tab4_Histogram_Legend_Title))
                    })
                }
            }
        })


        # histogram facetwrap
        long_fileHist <- metaReactive2({
            if (input$Tab4_Faceted_Histogram_Display_NAs == FALSE) {
                metaExpr({
                    melt(..(datalist[[data]]())[, c(
                        ..(input$Tab4_Select_Plot_Variable),
                        ..(
                            input$Tab4_Histogram_Select_Facet_Variable
                        )
                    )]) %>%
                        filter(!is.na(eval(parse(
                            text = ..(
                                input$Tab4_Histogram_Select_Facet_Variable
                            )
                        ))))
                })
            } else {
                metaExpr({
                    melt(..(datalist[[data]]())[, c(
                        ..(input$Tab4_Select_Plot_Variable),
                        ..(
                            input$Tab4_Histogram_Select_Facet_Variable
                        )
                    )])
                })
            }
        })

        CNAHisttab <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab4_Histogram_Select_Facet_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else {
                metaExpr({
                    ggplot(..(long_fileHist()), aes(x = value)) +
                        geom_histogram(
                            aes(
                                color = factor(..(long_fileHist())[, ..(input$Tab4_Histogram_Select_Facet_Variable)]),
                                fill = factor(..(long_fileHist())[, ..(input$Tab4_Histogram_Select_Facet_Variable)])
                            ),
                            alpha = ..(input$Tab4_Faceted_Histogram_Alpha),
                            binwidth = ..(
                                input$Tab4_Faceted_Histogram_Number_of_Bins
                            )
                        ) +
                        facet_wrap(
                            factor(..(long_fileHist())[, ..(input$Tab4_Histogram_Select_Facet_Variable)]) ~ .,
                            ncol = ..(
                                input$Tab4_Faceted_Histogram_Number_of_Columns
                            )
                        ) +
                        ggtitle(..(input$Tab4_Faceted_Histogram_Title)) +
                        ylab(..(
                            input$Tab4_Faceted_Histogram_Y_Axis_Title
                        )) +
                        xlab(..(
                            input$Tab4_Faceted_Histogram_X_Axis_Title
                        )) +
                        theme_bw() +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.text.x = element_text(size = 15)) +
                        theme(axis.text.y = element_text(size = 15)) +
                        theme(legend.title = element_text(
                            colour = "black",
                            size = 15,
                            face = "bold"
                        )) +
                        theme(strip.text = element_text(size = 15)) +
                        theme(legend.text = element_text(size = 15)) +
                        theme(
                            legend.position = ..(
                                input$Tab4_Faceted_Histogram_Legend_Position
                            )
                        ) +
                        scale_fill_discrete(..(
                            input$Tab4_Faceted_Histogram_Legend_Title
                        )) +
                        guides(color = FALSE)
                })
            }
        })

        output$CNAHist <- metaRender(renderPlot, {
            ..(CNAHistogram())
        })

        output$Tab4_Download_Histogram_PNG <- downloadHandler(
            filename = function() {
                paste("Histogram", ".png", sep = "")
            },
            content = function(file) {
                png(
                    file,
                    width = input$Tab4_Histogram_Width,
                    height = input$Tab4_Histogram_Height,
                    units = "in",
                    res = 1200
                )
                plot(CNAHistogram())
                dev.off()
            }
        )

        output$Tab4_Download_Histogram_SVG <- downloadHandler(
            filename = function() {
                paste("Histogram", ".svg", sep = "")
            },
            content = function(file) {
                svg(
                    file,
                    width = input$Tab4_Histogram_Width,
                    height = input$Tab4_Histogram_Height
                )
                CNAHistogram()
                dev.off()
            }
        )

        output$CNAHist1 <- metaRender(renderPlot, {
            ..(CNAHisttab())
        })

        output$Tab4_Download_Faceted_Histogram_PNG <-
            downloadHandler(
                filename = function() {
                    paste("Histogram_Facet", ".png", sep = "")
                },
                content = function(file) {
                    png(
                        file,
                        width = input$Tab4_Faceted_Histogram_Width,
                        height = input$Tab4_Faceted_Histogram_Height,
                        units = "in",
                        res = 1200
                    )
                    plot(CNAHisttab())
                    dev.off()
                }
            )

        output$Tab4_Download_Faceted_Histogram_SVG <-
            downloadHandler(
                filename = function() {
                    paste("Histogram_Facet", ".svg", sep = "")
                },
                content = function(file) {
                    svg(
                        file,
                        width = input$Tab4_Faceted_Histogram_Width,
                        height = input$Tab4_Faceted_Histogram_Height
                    )
                    CNAHisttab()
                    dev.off()
                }
            )


        # plain density plot
        CNADense <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab4_Select_Plot_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else if (input$Tab4_Select_Plot_Variable != "None Selected" &
                input$Tab4_Density_Plot_Select_Fill_Variable == "None Selected") {
                metaExpr({
                    ggplot(..(datalist[[data]]())) +
                        geom_density(
                            aes(
                                x = ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                                color = "Color",
                                fill = "Color"
                            ),
                            na.rm = ..(input$Tab4_Density_Plot_Display_NAs),
                            alpha = ..(input$Tab4_Density_Plot_Alpha)
                        ) +
                        xlab(..(input$Tab4_Density_Plot_X_Axis_Title)) +
                        ylab(..(input$Tab4_Density_Plot_Y_Axis_Title)) +
                        ggtitle(..(input$Tab4_Density_Plot_Title)) +
                        theme_bw() +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.text.x = element_text(size = 15)) +
                        theme(axis.text.y = element_text(size = 15)) +
                        theme(legend.title = element_text(
                            colour = "black",
                            size = 15,
                            face = "bold"
                        )) +
                        theme(strip.text = element_text(size = 15)) +
                        theme(legend.text = element_text(size = 15)) +
                        scale_color_manual(values = c("Color" = "#2ac0db")) +
                        scale_fill_manual(values = c("Color" = "#2ac0db")) +
                        theme(legend.position = "none") +
                        xlim(range(density(
                            ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                            na.rm = TRUE
                        )$x))
                })
            } else {
                metaExpr({
                    if (input$Tab4_Density_Plot_Display_NAs == FALSE) {
                        Data <-
                            completeFun(..(datalist[[data]]()), c(
                                ..(
                                    input$Tab4_Density_Plot_Select_Fill_Variable
                                )
                            ))
                        ggplot(Data) +
                            geom_density(
                                aes(
                                    x = Data[, ..(input$Tab4_Select_Plot_Variable)],
                                    fill = Data[, ..(input$Tab4_Density_Plot_Select_Fill_Variable)],
                                    color = Data[, ..(input$Tab4_Density_Plot_Select_Fill_Variable)]
                                ),
                                alpha = ..(input$Tab4_Density_Plot_Alpha)
                            ) +
                            xlab(..(
                                input$Tab4_Density_Plot_X_Axis_Title
                            )) +
                            ylab(..(
                                input$Tab4_Density_Plot_Y_Axis_Title
                            )) +
                            ggtitle(..(input$Tab4_Density_Plot_Title)) +
                            theme_bw() +
                            theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.text.x = element_text(size = 15)) +
                            theme(axis.text.y = element_text(size = 15)) +
                            theme(legend.title = element_text(
                                colour = "black",
                                size = 15,
                                face = "bold"
                            )) +
                            theme(strip.text = element_text(size = 15)) +
                            theme(legend.text = element_text(size = 15)) +
                            theme(
                                legend.position = ..(
                                    input$Tab4_Density_Plot_Legend_Position
                                )
                            ) +
                            scale_fill_discrete(..(
                                input$Tab4_Density_Plot_Legend_Title
                            )) +
                            scale_color_discrete(..(
                                input$Tab4_Density_Plot_Legend_Title
                            )) +
                            xlim(range(density(
                                ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                                na.rm = TRUE
                            )$x))
                    } else {
                        ggplot(..(datalist[[data]]())) +
                            geom_density(
                                aes(
                                    x = ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                                    fill = ..(datalist[[data]]())[, ..(input$Tab4_Density_Plot_Select_Fill_Variable)],
                                    color = ..(datalist[[data]]())[, ..(input$Tab4_Density_Plot_Select_Fill_Variable)]
                                ),
                                alpha = ..(input$Tab4_Density_Plot_Alpha)
                            ) +
                            xlab(..(
                                input$Tab4_Density_Plot_X_Axis_Title
                            )) +
                            ylab(..(
                                input$Tab4_Density_Plot_Y_Axis_Title
                            )) +
                            ggtitle(..(input$Tab4_Density_Plot_Title)) +
                            theme_bw() +
                            theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.text.x = element_text(size = 15)) +
                            theme(axis.text.y = element_text(size = 15)) +
                            theme(legend.title = element_text(
                                colour = "black",
                                size = 15,
                                face = "bold"
                            )) +
                            theme(strip.text = element_text(size = 15)) +
                            theme(legend.text = element_text(size = 15)) +
                            theme(
                                legend.position = ..(
                                    input$Tab4_Density_Plot_Legend_Position
                                )
                            ) +
                            scale_fill_discrete(..(
                                input$Tab4_Density_Plot_Legend_Title
                            )) +
                            scale_color_discrete(..(
                                input$Tab4_Density_Plot_Legend_Title
                            )) +
                            xlim(range(density(
                                ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                                na.rm = TRUE
                            )$x))
                    }
                })
            }
        })

        output$CNADist <- metaRender(renderPlot, {
            ..(CNADense())
        })

        output$Tab4_Download_Density_Plot_PNG <- downloadHandler(
            filename = function() {
                paste("Density_Plot", ".png", sep = "")
            },
            content = function(file) {
                png(
                    file,
                    width = input$Tab4_Density_Plot_Width,
                    height = input$Tab4_Density_Plot_Height,
                    units = "in",
                    res = 1200
                )
                plot(CNADense())
                dev.off()
            }
        )


        output$Tab4_Download_Density_Plot_SVG <- downloadHandler(
            filename = function() {
                paste("Density_Plot", ".svg", sep = "")
            },
            content = function(file) {
                svg(
                    file,
                    width = input$Tab4_Density_Plot_Width,
                    height = input$Tab4_Density_Plot_Height
                )
                CNADense()
                dev.off()
            }
        )

        # segmented density plot
        CNADist1Plot <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab4_Select_Plot_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else if (input$Tab4_Segmented_Density_Plot_Display_Legend == TRUE) {
                metaExpr({
                    colourCount <-
                        ..(input$Tab4_Segmented_Density_Plot_Number_of_Segments)
                    getPalette <-
                        colorRampPalette(brewer.pal(9, "Blues"))
                    lab <-
                        as.character(seq.int(from = 1, 
                                             to = ..(input$Tab4_Segmented_Density_Plot_Number_of_Segments)
                        ))
                    dt <- data.frame(
                        x = c(seq.int(from = 1, to = length(..(
                            datalist[[data]]()
                        )[, ..(input$Tab4_Select_Plot_Variable)]))),
                        y = ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)]
                    )
                    dt <- na.omit(dt)
                    dens <- density(dt$y)
                    df <- data.frame(x = dens$x, y = dens$y)
                    probs1 <-
                        c(
                            0:..(
                                input$Tab4_Segmented_Density_Plot_Number_of_Segments
                            ) / ..(
                                input$Tab4_Segmented_Density_Plot_Number_of_Segments
                            )
                        )
                    probs <- probs1[-c(1, length(probs1))]
                    quantiles <- quantile(dt$y, prob = probs)
                    df$quant <-
                        factor(findInterval(df$x, quantiles))
                    ggplot(df, aes(x, y)) +
                        geom_line() +
                        geom_ribbon(aes(
                            ymin = 0,
                            ymax = y,
                            fill = quant
                        )) +
                        scale_x_continuous(breaks = quantiles) +
                        xlab(..(
                            input$Tab4_Segmented_Density_Plot_X_Axis_Title
                        )) +
                        ylab(..(
                            input$Tab4_Segmented_Density_Plot_Y_Axis_Title
                        )) +
                        ggtitle(..(
                            input$Tab4_Segmented_Density_Plot_Title
                        )) +
                        theme_bw() +
                        theme(legend.position = c(0.9, 0.5)) +
                        theme(legend.key.size = unit(0.9, "cm")) +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.text.x = element_text(size = 15)) +
                        theme(axis.text.y = element_text(size = 15)) +
                        theme(legend.title = element_text(
                            colour = "black",
                            size = 15,
                            face = "bold"
                        )) +
                        theme(strip.text = element_text(size = 15)) +
                        theme(legend.text = element_text(size = 15)) +
                        scale_fill_manual(
                            values = getPalette(colourCount),
                            labels = lab,
                            name = ..(
                                input$Tab4_Segmented_Density_Plot_Legend_Title
                            )
                        )
                })
            } else {
                metaExpr({
                    colourCount <-
                        ..(input$Tab4_Segmented_Density_Plot_Number_of_Segments)
                    getPalette <-
                        colorRampPalette(brewer.pal(9, "Blues"))
                    dt <- data.frame(
                        x = c(seq.int(from = 1, to = length(..(
                            datalist[[data]]()
                        )[, ..(input$Tab4_Select_Plot_Variable)]))),
                        y = ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)]
                    )
                    dt <- na.omit(dt)
                    dens <- density(dt$y)
                    df <- data.frame(x = dens$x, y = dens$y)
                    probs1 <-
                        c(
                            0:..(
                                input$Tab4_Segmented_Density_Plot_Number_of_Segments
                            ) / ..(
                                input$Tab4_Segmented_Density_Plot_Number_of_Segments
                            )
                        )
                    probs <- probs1[-c(1, length(probs1))]
                    quantiles <- quantile(dt$y, prob = probs)
                    df$quant <-
                        factor(findInterval(df$x, quantiles))
                    ggplot(df, aes(x, y)) +
                        geom_line() +
                        geom_ribbon(aes(
                            ymin = 0,
                            ymax = y,
                            fill = quant
                        )) +
                        scale_x_continuous(breaks = quantiles) +
                        theme_bw() +
                        xlab(..(
                            input$Tab4_Segmented_Density_Plot_X_Axis_Title
                        )) +
                        ylab(..(
                            input$Tab4_Segmented_Density_Plot_Y_Axis_Title
                        )) +
                        ggtitle(..(
                            input$Tab4_Segmented_Density_Plot_Title
                        )) +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.text.x = element_text(size = 15)) +
                        theme(axis.text.y = element_text(size = 15)) +
                        theme(legend.title = element_text(
                            colour = "black",
                            size = 15,
                            face = "bold"
                        )) +
                        theme(strip.text = element_text(size = 15)) +
                        theme(legend.text = element_text(size = 15)) +
                        theme(legend.position = c(0.9, 0.5)) +
                        theme(legend.key.size = unit(0.9, "cm")) +
                        scale_fill_manual(
                            values = getPalette(colourCount),
                            name = ..(
                                input$Tab4_Segmented_Density_Plot_Legend_Title
                            )
                        ) +
                        guides(fill = FALSE)
                })
            }
        })

        output$CNADist1 <- metaRender(renderPlot, {
            ..(CNADist1Plot())
        })

        output$Tab4_Download_Segmented_Density_Plot_PNG <-
            downloadHandler(
                filename = function() {
                    paste("Segmented_Density_Plot", ".png", sep = "")
                },
                content = function(file) {
                    png(
                        file,
                        width = input$Tab4_Segmented_Density_Plot_Width,
                        height = input$Tab4_Segmented_Density_Plot_Height,
                        units = "in",
                        res = 1200
                    )
                    plot(CNADist1Plot())
                    dev.off()
                }
            )

        output$Tab4_Download_Segmented_Density_Plot_SVG <-
            downloadHandler(
                filename = function() {
                    paste("Segmented_Density_Plot", ".svg", sep = "")
                },
                content = function(file) {
                    svg(
                        file,
                        width = input$Tab4_Segmented_Density_Plot_Width,
                        height = input$Tab4_Segmented_Density_Plot_Height
                    )
                    CNADist1Plot()
                    dev.off()
                }
            )


        # density plot facetwrap
        observe({
            vchoicesDistfacet <- c(names(datalist[[data]]()), "None Selected")
            updateSelectizeInput(
                session,
                "Tab4_Density_Plot_Select_Facet_Variable",
                choices = vchoicesDistfacet,
                selected = "None Selected",
                server = TRUE
            )
        })

        long_file <- metaReactive2({
            if (input$Tab4_Faceted_Density_Plot_Display_NAs == FALSE) {
                metaExpr({
                    melt(..(datalist[[data]]())[, c(
                        ..(input$Tab4_Select_Plot_Variable),
                        ..(
                            input$Tab4_Density_Plot_Select_Facet_Variable
                        )
                    )]) %>%
                        filter(!is.na(eval(parse(
                            text = ..(
                                input$Tab4_Density_Plot_Select_Facet_Variable
                            )
                        ))))
                })
            } else {
                metaExpr({
                    melt(..(datalist[[data]]())[, c(
                        ..(input$Tab4_Select_Plot_Variable),
                        ..(
                            input$Tab4_Density_Plot_Select_Facet_Variable
                        )
                    )])
                })
            }
        })

        CNADist2Plot <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab4_Density_Plot_Select_Facet_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else {
                metaExpr({
                    ggplot(..(long_file())) +
                        geom_density(
                            aes(
                                x = value,
                                color = factor(..(long_file())[, ..(input$Tab4_Density_Plot_Select_Facet_Variable)]),
                                fill = factor(..(long_file())[, ..(input$Tab4_Density_Plot_Select_Facet_Variable)])
                            ),
                            alpha = ..(input$Tab4_Faceted_Density_Plot_Alpha)
                        ) +
                        facet_wrap(
                            factor(..(long_file())[, ..(input$Tab4_Density_Plot_Select_Facet_Variable)]) ~ .,
                            ncol = ..(
                                input$Tab4_Faceted_Density_Plot_Number_of_Columns
                            )
                        ) +
                        ggtitle(..(input$Tab4_Faceted_Density_Plot_Title)) +
                        ylab(..(
                            input$Tab4_Faceted_Density_Plot_Y_Axis_Title
                        )) +
                        xlab(..(
                            input$Tab4_Faceted_Density_Plot_X_Axis_Title
                        )) +
                        theme_bw() +
                        theme(
                            plot.title = element_text(hjust = 0.5, size = 18),
                            axis.title.x = element_text(hjust = 0.5, size = 18),
                            axis.title.y = element_text(hjust = 0.5, size = 18),
                            axis.text.x = element_text(size = 15),
                            axis.text.y = element_text(size = 15),
                            legend.title = element_text(
                                colour = "black",
                                size = 15,
                                face = "bold"
                            ),
                            strip.text = element_text(size = 15),
                            legend.text = element_text(size = 15),
                            legend.position = ..(
                                input$Tab4_Faceted_Density_Plot_Legend_Position
                            )
                        ) +
                        scale_fill_discrete(name = ..(
                            input$Tab4_Faceted_Density_Plot_Legend_Title
                        )) +
                        xlim(range(density(
                            ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                            na.rm = TRUE
                        )$x)) +
                        guides(color = FALSE)
                })
            }
        })

        output$CNADist2 <- metaRender(renderPlot, {
            ..(CNADist2Plot())
        })

        output$Tab4_Download_Faceted_Density_Plot_PNG <-
            downloadHandler(
                filename = function() {
                    paste("Faceted_Density_Plot1", ".png", sep = "")
                },
                content = function(file) {
                    png(
                        file,
                        width = input$Tab4_Faceted_Density_Plot_Width,
                        height = input$Tab4_Faceted_Density_Plot_Height,
                        units = "in",
                        res = 1200
                    )
                    plot(CNADist2Plot())
                    dev.off()
                }
            )

        output$Tab4_Download_Faceted_Density_Plot_SVG <-
            downloadHandler(
                filename = function() {
                    paste("Faceted_Density_Plot1", ".svg", sep = "")
                },
                content = function(file) {
                    svg(
                        file,
                        width = input$Tab4_Faceted_Density_Plot_Width,
                        height = input$Tab4_Faceted_Density_Plot_Height
                    )
                    CNADist2Plot()
                    dev.off()
                }
            )


        # histogram and density plot together
        CNABoth <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab4_Select_Plot_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else if (input$Tab4_Select_Plot_Variable != "None Selected" &
                input$Tab4_Both_Plot_Select_Fill_Variable == "None Selected") {
                metaExpr({
                    ggplot(..(datalist[[data]]())) +
                        geom_density(
                            aes(
                                x = ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                                color = "Color",
                                fill = "Color"
                            ),
                            na.rm = ..(input$Tab4_Both_Plot_Display_NAs),
                            alpha = ..(input$Tab4_Both_Density_Plot_Alpha)
                        ) +
                        geom_histogram(
                            aes(
                                x = ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                                y = ..density..,
                                color = "Color",
                                fill = "Color"
                            ),
                            alpha = ..(input$Tab4_Both_Histogram_Alpha),
                            fill = "#2ac0db",
                            position = "identity"
                        ) +
                        xlab(..(input$Tab4_Both_Plot_X_Axis_Title)) +
                        ylab(..(input$Tab4_Both_Plot_Y_Axis_Title)) +
                        ggtitle(..(input$Tab4_Both_Plot_Title)) +
                        theme_bw() +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.text.x = element_text(size = 15)) +
                        theme(axis.text.y = element_text(size = 15)) +
                        theme(legend.title = element_text(
                            colour = "black",
                            size = 15,
                            face = "bold"
                        )) +
                        theme(strip.text = element_text(size = 15)) +
                        theme(legend.text = element_text(size = 15)) +
                        xlim(range(density(
                            ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                            na.rm = TRUE
                        )$x)) +
                        scale_color_manual(values = c("Color" = "#2ac0db")) +
                        scale_fill_manual(values = c("Color" = "#2ac0db")) +
                        theme(legend.position = "none")
                })
            } else {
                metaExpr({
                    if (input$Tab4_Both_Plot_Display_NAs == FALSE) {
                        Data <-
                            completeFun(..(datalist[[data]]()), c(
                                ..(
                                    input$Tab4_Both_Plot_Select_Fill_Variable
                                )
                            ))
                        ggplot(Data) +
                            geom_density(
                                aes(
                                    x = Data[, ..(input$Tab4_Select_Plot_Variable)],
                                    fill = Data[, ..(input$Tab4_Both_Plot_Select_Fill_Variable)],
                                    color = Data[, ..(input$Tab4_Both_Plot_Select_Fill_Variable)]
                                ),
                                alpha = ..(input$Tab4_Both_Density_Plot_Alpha)
                            ) +
                            geom_histogram(
                                aes(
                                    x = Data[, ..(input$Tab4_Select_Plot_Variable)],
                                    y = ..density..,
                                    fill = Data[, ..(input$Tab4_Both_Plot_Select_Fill_Variable)],
                                    color = Data[, ..(input$Tab4_Both_Plot_Select_Fill_Variable)]
                                ),
                                alpha = ..(input$Tab4_Both_Histogram_Alpha),
                                position = "identity"
                            ) +
                            xlab(..(input$Tab4_Both_Plot_X_Axis_Title)) +
                            ylab(..(input$Tab4_Both_Plot_Y_Axis_Title)) +
                            ggtitle(..(input$Tab4_Both_Plot_Title)) +
                            xlim(range(density(
                                ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                                na.rm = TRUE
                            )$x)) +
                            theme_bw() +
                            theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.text.x = element_text(size = 15)) +
                            theme(axis.text.y = element_text(size = 15)) +
                            theme(legend.title = element_text(
                                colour = "black",
                                size = 15,
                                face = "bold"
                            )) +
                            theme(strip.text = element_text(size = 15)) +
                            theme(legend.text = element_text(size = 15)) +
                            theme(legend.position = ..(
                                input$Tab4_Both_Plot_Legend_Position
                            )) +
                            scale_fill_discrete(..(input$Tab4_Both_Plot_Legend_Title)) +
                            scale_color_discrete(..(input$Tab4_Both_Plot_Legend_Title))
                    } else {
                        ggplot(..(datalist[[data]]())) +
                            geom_density(
                                aes(
                                    x = ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                                    fill = ..(datalist[[data]]())[, ..(input$Tab4_Both_Plot_Select_Fill_Variable)],
                                    color = ..(datalist[[data]]())[, ..(input$Tab4_Both_Plot_Select_Fill_Variable)]
                                ),
                                alpha = ..(input$Tab4_Both_Density_Plot_Alpha)
                            ) +
                            geom_histogram(
                                aes(
                                    x = ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                                    y = ..density..,
                                    fill = ..(datalist[[data]]())[, ..(input$Tab4_Both_Plot_Select_Fill_Variable)],
                                    color = ..(datalist[[data]]())[, ..(input$Tab4_Both_Plot_Select_Fill_Variable)]
                                ),
                                alpha = ..(input$Tab4_Both_Histogram_Alpha),
                                position = "identity"
                            ) +
                            xlab(..(input$Tab4_Both_Plot_X_Axis_Title)) +
                            ylab(..(input$Tab4_Both_Plot_Y_Axis_Title)) +
                            ggtitle(..(input$Tab4_Both_Plot_Title)) +
                            theme_bw() +
                            theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                            theme(axis.text.x = element_text(size = 15)) +
                            theme(axis.text.y = element_text(size = 15)) +
                            theme(legend.title = element_text(
                                colour = "black",
                                size = 15,
                                face = "bold"
                            )) +
                            theme(strip.text = element_text(size = 15)) +
                            theme(legend.text = element_text(size = 15)) +
                            xlim(range(density(
                                ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                                na.rm = TRUE
                            )$x)) +
                            theme(legend.position = ..(
                                input$Tab4_Both_Plot_Legend_Position
                            )) +
                            scale_fill_discrete(..(input$Tab4_Both_Plot_Legend_Title)) +
                            scale_color_discrete(..(input$Tab4_Both_Plot_Legend_Title))
                    }
                })
            }
        })

        output$CNABoth <- metaRender(renderPlot, {
            ..(CNABoth())
        })

        output$Tab4_Download_Both_Plot_PNG <- downloadHandler(
            filename = function() {
                paste("Density_Hist_Plot", ".png", sep = "")
            },
            content = function(file) {
                png(
                    file,
                    width = input$Tab4_Both_Plot_Width,
                    height = input$Tab4_Both_Plot_Height,
                    units = "in",
                    res = 1200
                )
                plot(CNABoth())
                dev.off()
            }
        )

        output$Tab4_Download_Both_Plot_SVG <- downloadHandler(
            filename = function() {
                paste("Density_Hist_Plot", ".svg", sep = "")
            },
            content = function(file) {
                svg(
                    file,
                    width = input$Tab4_Both_Plot_Width,
                    height = input$Tab4_Both_Plot_Height
                )
                CNABoth()
                dev.off()
            }
        )


        # histogram and density plot facewrap
        long_file1 <- metaReactive2({
            if (input$Tab4_Faceted_Both_Plot_Display_NAs == FALSE) {
                metaExpr({
                    melt(..(datalist[[data]]())[, c(
                        ..(input$Tab4_Select_Plot_Variable),
                        ..(
                            input$Tab4_Both_Plot_Select_Facet_Variable
                        )
                    )]) %>%
                        filter(!is.na(eval(parse(
                            text = ..(
                                input$Tab4_Both_Plot_Select_Facet_Variable
                            )
                        ))))
                })
            } else {
                metaExpr({
                    melt(..(datalist[[data]]())[, c(
                        ..(input$Tab4_Select_Plot_Variable),
                        ..(
                            input$Tab4_Both_Plot_Select_Facet_Variable
                        )
                    )])
                })
            }
        })

        CNADist2Plot1 <- metaReactive2({
            if (is.null(datalist[[data]]()) |
                input$Tab4_Both_Plot_Select_Facet_Variable == "None Selected") {
                ggplot() +
                    theme_void()
            } else {
                metaExpr({
                    ggplot(..(long_file1()), aes(x = value)) +
                        geom_histogram(
                            aes(
                                x = value,
                                y = ..density..,
                                color = factor(..(long_file1())[, ..(input$Tab4_Both_Plot_Select_Facet_Variable)]),
                                fill = factor(..(long_file1())[, ..(input$Tab4_Both_Plot_Select_Facet_Variable)])
                            ),
                            position = "identity",
                            alpha = ..(
                                input$Tab4_Faceted_Both_Histogram_Plot_Alpha
                            )
                        ) +
                        geom_density(
                            aes(
                                x = value,
                                color = factor(..(long_file1())[, ..(input$Tab4_Both_Plot_Select_Facet_Variable)]),
                                fill = factor(..(long_file1())[, ..(input$Tab4_Both_Plot_Select_Facet_Variable)])
                            ),
                            show.legend = ..(
                                input$Tab4_Faceted_Both_Plot_Legend_Position
                            ),
                            alpha = ..(
                                input$Tab4_Faceted_Both_Density_Plot_Alpha
                            )
                        ) +
                        facet_wrap(
                            factor(..(long_file1())[, ..(input$Tab4_Both_Plot_Select_Facet_Variable)]) ~ .,
                            ncol = ..(
                                input$Tab4_Faceted_Both_Plot_Number_of_Columns
                            )
                        ) +
                        ggtitle(..(input$Tab4_Faceted_Both_Plot_Title)) +
                        ylab(..(
                            input$Tab4_Faceted_Both_Plot_Y_Axis_Title
                        )) +
                        xlab(..(
                            input$Tab4_Faceted_Both_Plot_X_Axis_Title
                        )) +
                        theme_bw() +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.x = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.title.y = element_text(hjust = 0.5, size = 18)) +
                        theme(axis.text.x = element_text(size = 15)) +
                        theme(axis.text.y = element_text(size = 15)) +
                        theme(legend.title = element_text(
                            colour = "black",
                            size = 15,
                            face = "bold"
                        )) +
                        theme(strip.text = element_text(size = 15)) +
                        theme(legend.text = element_text(size = 15)) +
                        scale_fill_discrete(name = ..(
                            input$Tab4_Faceted_Both_Plot_Legend_Title
                        )) +
                        xlim(range(density(
                            ..(datalist[[data]]())[, ..(input$Tab4_Select_Plot_Variable)],
                            na.rm = TRUE
                        )$x)) +
                        guides(color = FALSE) +
                        theme(
                            legend.position = ..(
                                input$Tab4_Faceted_Both_Plot_Legend_Position
                            )
                        )
                })
            }
        })

        output$CNABoth1 <- metaRender(renderPlot, {
            ..(CNADist2Plot1())
        })

        # download plots
        output$Tab4_Download_Faceted_Both_Plot_PNG <-
            downloadHandler(
                filename = function() {
                    paste("Faceted_Density_Plot2", ".png", sep = "")
                },
                content = function(file) {
                    png(
                        file,
                        width = input$Tab4_Faceted_Both_Plot_Width,
                        height = input$Tab4_Faceted_Both_Plot_Height,
                        units = "in",
                        res = 1200
                    )
                    plot(CNADist2Plot1())
                    dev.off()
                }
            )

        output$Tab4_Download_Faceted_Both_Plot_SVG <-
            downloadHandler(
                filename = function() {
                    paste("Faceted_Density_Plot2", ".svg", sep = "")
                },
                content = function(file) {
                    svg(
                        file,
                        width = input$Tab4_Faceted_Both_Plot_Width,
                        height = input$Tab4_Faceted_Both_Plot_Height
                    )
                    CNADist2Plot1()
                    dev.off()
                }
            )
    })
}

# Server function to render plots
Tab4_DisplayPlot_Server <- function(id, datalist, plot) {
    moduleServer(id, function(input, output, session) {
        output$Plot <- metaRender(renderPlot, {
            ..(datalist[[plot]]())
        })
    })
}
