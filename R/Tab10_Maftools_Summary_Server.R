# Server function to produce MAF text summary
Tab10_MAF_Text_Summary_Server <- function(id, datalist, rowselect) {
    moduleServer(id, function(input, output, session) {
        dataInputMAFPLOT <- metaReactive2({
            if (is.null(datalist[["MAF_manual_data"]]()) &
                is.null(rowselect())) {
                validate(
                    need(
                        !is.null(datalist[["MAF_manual_data"]]()) | !is.null(rowselect()),
                        "Please select cBioPortal dataset or upload your own mutation data."
                    )
                )
            } else if (!is.null(datalist[["MAF_manual_data"]]()) &
                is.null(rowselect())) {
                if (is.null(datalist[["patient_manual_data"]]()) &
                    is.null(datalist[["sample_manual_data"]]())) {
                    metaExpr({
                        read.maf(maf = ..(datalist[["MAF_Val"]]()))
                    })
                } else {
                    metaExpr({
                        Data_Clin <-
                            ..(datalist[["Combined_clin"]]()) %>% mutate(Tumor_Sample_Barcode = PATIENT_ID)
                        read.maf(
                            maf = ..(datalist[["MAF_Val"]]()),
                            clinicalData = Data_Clin
                        )
                    })
                }
            } else if (is.null(datalist[["MAF_manual_data"]]()) &
                !is.null(rowselect())) {
                metaExpr({
                    Data_Clin <-
                        ..(datalist[["Combined_clin"]]()) %>% mutate(Tumor_Sample_Barcode = PATIENT_ID)
                    read.maf(
                        maf = ..(datalist[["MAF_Val"]]()),
                        clinicalData = Data_Clin
                    )
                })
            } else {
                validate(
                    need(
                        !is.null(datalist[["MAF_manual_data"]]()) | !is.null(rowselect()),
                        "Please input MAF file or select cBioPortal dataset"
                    )
                )
            }
        })
        output$MAF1 <- metaRender(renderPrint, {
            ..(dataInputMAFPLOT())
        })
        output$MAF2 <- metaRender(renderPrint, {
            getSampleSummary(..(dataInputMAFPLOT()))
        })
        output$MAF3 <- metaRender(renderPrint, {
            getGeneSummary(..(dataInputMAFPLOT()))
        })
        output$MAF4 <- metaRender(renderPrint, {
            getFields(..(dataInputMAFPLOT()))
        })
        output$MAFClin <- metaRender(renderPrint, {
            getClinicalData(..(dataInputMAFPLOT()))
        })
    })
}

# Server function to produce MAF plot summary
Tab10_MAF_Plot_Summary_Server <- function(id, datalist, rowselect) {
    moduleServer(id, function(input, output, session) {
        dataInputMAFPLOT <- metaReactive2({
            if (is.null(datalist[["MAF_manual_data"]]()) &
                is.null(rowselect())) {
                validate(
                    need(
                        !is.null(datalist[["MAF_manual_data"]]()) | !is.null(rowselect()),
                        "Please select cBioPortal dataset or upload your own mutation data."
                    )
                )
            } else if (!is.null(datalist[["MAF_manual_data"]]()) &
                is.null(rowselect())) {
                if (is.null(datalist[["patient_manual_data"]]()) &
                    is.null(datalist[["sample_manual_data"]]())) {
                    metaExpr({
                        read.maf(maf = ..(datalist[["MAF_Val"]]()))
                    })
                } else {
                    metaExpr({
                        Data_Clin <-
                            ..(datalist[["Combined_clin"]]()) %>% mutate(Tumor_Sample_Barcode = PATIENT_ID)
                        read.maf(
                            maf = ..(datalist[["MAF_Val"]]()),
                            clinicalData = Data_Clin
                        )
                    })
                }
            } else if (is.null(datalist[["MAF_manual_data"]]()) &
                !is.null(rowselect())) {
                metaExpr({
                    Data_Clin <-
                        ..(datalist[["Combined_clin"]]()) %>% mutate(Tumor_Sample_Barcode = PATIENT_ID)
                    read.maf(
                        maf = ..(datalist[["MAF_Val"]]()),
                        clinicalData = Data_Clin
                    )
                })
            } else {
                validate(
                    need(
                        !is.null(datalist[["MAF_manual_data"]]()) | !is.null(rowselect()),
                        "Please input MAF file or select cBioPortal dataset"
                    )
                )
            }
        })

        sumMAF <- metaReactive2({
            if (is.null(datalist[["MAF_manual_data"]]()) &
                is.null(rowselect())) {
                ggplot() +
                    theme_void()
            } else {
                metaExpr({
                    plotmafSummary(
                        ..(dataInputMAFPLOT()),
                        rmOutlier = ..(input$Tab10_Summary_Remove_Outlier),
                        addStat = ..(input$Tab10_Summary_Add_Stat),
                        dashboard = ..(input$Tab10_Summary_Dashboard_Style),
                        titvRaw = ..(input$Tab10_Summary_Plot_Fraction),
                        top = ..(input$Tab10_Summary_Display_Top_Genes)
                    )
                })
            }
        })

        output$summaryMAF <- metaRender(renderPlot, {
            ..(sumMAF())
        })

        output$Tab10_Download_Summary_Plot_PNG <- downloadHandler(
            filename = function() {
                paste("MAF_Summary_Plot", ".png", sep = "")
            },
            content = function(file) {
                png(
                    file,
                    width = input$Tab10_Summary_Plot_Width,
                    height = input$Tab10_Summary_Plot_Height,
                    units = "in",
                    res = 1200
                )
                plotmafSummary(
                    dataInputMAFPLOT(),
                    rmOutlier = input$Tab10_Summary_Remove_Outlier,
                    addStat = input$Tab10_Summary_Add_Stat,
                    dashboard = input$Tab10_Summary_Dashboard_Style,
                    titvRaw = input$Tab10_Summary_Plot_Fraction,
                    top = input$Tab10_Summary_Display_Top_Genes
                )
                dev.off()
            }
        )

        output$Tab10_Download_Summary_Plot_SVG <- downloadHandler(
            filename = function() {
                paste("MAF_Summary_Plot", ".svg", sep = "")
            },
            content = function(file) {
                svg(
                    file,
                    width = input$Tab10_Summary_Plot_Width,
                    height = input$Tab10_Summary_Plot_Height
                )
                plotmafSummary(
                    dataInputMAFPLOT(),
                    rmOutlier = input$Tab10_Summary_Remove_Outlier,
                    addStat = input$Tab10_Summary_Add_Stat,
                    dashboard = input$Tab10_Summary_Dashboard_Style,
                    titvRaw = input$Tab10_Summary_Plot_Fraction,
                    top = input$Tab10_Summary_Display_Top_Genes
                )
                dev.off()
            }
        )

        Onco <- metaReactive2({
            validate(need(
                !is.null(datalist[["MAF_manual_data"]]()) | is.null(rowselect()),
                "Please input MAF file"
            ))
            if (is.null(datalist[["MAF_manual_data"]]()) &
                is.null(rowselect())) {
                return(ggplot() +
                    theme_void())
            } else {
                metaExpr({
                    oncoplot(
                        ..(dataInputMAFPLOT()),
                        top = ..(input$Tab10_Oncoplot_Display_Top_Genes)
                    )
                })
            }
        })

        output$oncoplotMAF <- metaRender(renderPlot, {
            ..(Onco())
        })

        output$Tab10_Download_Oncoplot_Plot_PNG <- downloadHandler(
            filename = function() {
                paste("Oncoplot", ".png", sep = "")
            },
            content = function(file) {
                png(
                    file,
                    width = input$Tab10_Oncoplot_Plot_Width,
                    height = input$Tab10_Oncoplot_Plot_Height,
                    units = "in",
                    res = 1200
                )
                oncoplot(dataInputMAFPLOT(),
                    top = input$Tab10_Oncoplot_Display_Top_Genes
                )
                dev.off()
            }
        )

        output$Tab10_Download_Oncoplot_Plot_SVG <- downloadHandler(
            filename = function() {
                paste("Oncoplot", ".svg", sep = "")
            },
            content = function(file) {
                svg(
                    file,
                    width = input$Tab10_Oncoplot_Plot_Width,
                    height = input$Tab10_Oncoplot_Plot_Height
                )
                oncoplot(dataInputMAFPLOT(),
                    top = input$Tab10_Oncoplot_Display_Top_Genes
                )
                dev.off()
            }
        )

        observe({
            vchoices1 <-
                c(unique(datalist[["MAF_Val"]]()[, input$Tab10_Lollipop_Gene_Name_Column]), "None Selected")
            updateSelectInput(
                session,
                "Tab10_Oncostrip_Select_Gene_1",
                choices = vchoices1,
                selected = vchoices1[1]
            )
            updateSelectInput(
                session,
                "Tab10_Oncostrip_Select_Gene_2",
                choices = vchoices1,
                selected = vchoices1[2]
            )
            updateSelectInput(
                session,
                "Tab10_Oncostrip_Select_Gene_3",
                choices = vchoices1,
                selected = "None Selected"
            )
        })

        OncoStrip <- metaReactive2({
            validate(need(
                !is.null(datalist[["MAF_manual_data"]]()) | is.null(rowselect()),
                "Please input MAF file"
            ))
            if (input$Tab10_Oncostrip_Select_Gene_1 != "None Selected" &
                input$Tab10_Oncostrip_Select_Gene_2 != "None Selected" &
                input$Tab10_Oncostrip_Select_Gene_3 == "None Selected") {
                metaExpr({
                    oncostrip(..(dataInputMAFPLOT()),
                        genes = c(
                            ..(input$Tab10_Oncostrip_Select_Gene_1),
                            ..(input$Tab10_Oncostrip_Select_Gene_2)
                        )
                    )
                })
            } else if (input$Tab10_Oncostrip_Select_Gene_1 != "None Selected" &
                input$Tab10_Oncostrip_Select_Gene_2 != "None Selected" &
                input$Tab10_Oncostrip_Select_Gene_3 != "None Selected") {
                metaExpr({
                    oncostrip(..(dataInputMAFPLOT()),
                        genes = c(
                            ..(input$Tab10_Oncostrip_Select_Gene_1),
                            ..(input$Tab10_Oncostrip_Select_Gene_2),
                            ..(input$Tab10_Oncostrip_Select_Gene_3)
                        )
                    )
                })
            }
        })

        output$oncostripMAF <- metaRender(renderPlot, {
            ..(OncoStrip())
        })

        output$Tab10_Download_Oncostrip_Plot_PNG <- downloadHandler(
            filename = function() {
                paste("Oncostrip", ".png", sep = "")
            },
            content = function(file) {
                png(
                    file,
                    width = input$Tab10_Oncostrip_Plot_Width,
                    height = input$Tab10_Oncostrip_Plot_Height,
                    units = "in",
                    res = 1200
                )
                oncostrip(
                    dataInputMAFPLOT(),
                    genes = c(
                        input$Tab10_Oncostrip_Select_Gene_1,
                        input$Tab10_Oncostrip_Select_Gene_2,
                        input$Tab10_Oncostrip_Select_Gene_3
                    )
                )
                dev.off()
            }
        )

        output$Tab10_Download_Oncostrip_Plot_SVG <- downloadHandler(
            filename = function() {
                paste("Oncostrip", ".svg", sep = "")
            },
            content = function(file) {
                svg(
                    file,
                    width = input$Tab10_Oncostrip_Plot_Width,
                    height = input$Tab10_Oncostrip_Plot_Height
                )
                oncostrip(
                    dataInputMAFPLOT(),
                    genes = c(
                        input$Tab10_Oncostrip_Select_Gene_1,
                        input$Tab10_Oncostrip_Select_Gene_2,
                        input$Tab10_Oncostrip_Select_Gene_3
                    )
                )
                dev.off()
            }
        )

        TandTPlot <- metaReactive2({
            validate(need(
                !is.null(datalist[["MAF_manual_data"]]()) | is.null(rowselect()),
                "Please input MAF file"
            ))
            metaExpr({
                laml.Tab10_Summary_Plot_Fraction <- titv(
                    maf = ..(dataInputMAFPLOT()),
                    plot = ..(input$Tab10_TT_Plot_Fraction),
                    useSyn = ..(input$Tab10_TT_Include_Synonymous_Variants)
                )
                plotTiTv(res = laml.Tab10_Summary_Plot_Fraction)
            })
        })

        output$TandT <- metaRender(renderPlot, {
            ..(TandTPlot())
        })

        output$Tab10_Download_TT_Plot_PNG <- downloadHandler(
            filename = function() {
                paste("Transitions_and_Transversions", ".png", sep = "")
            },
            content = function(file) {
                png(
                    file,
                    width = Tab10_TT_Plot_Width,
                    height = input$Tab10_TT_Plot_Height,
                    units = "in",
                    res = 1200
                )
                laml.Tab10_Summary_Plot_Fraction <-
                    Tab10_Summary_Plot_Fraction(
                        maf = dataInputMAFPLOT(),
                        plot = input$Tab10_TT_Plot_Fraction,
                        useSyn = input$Tab10_TT_Include_Synonymous_Variants
                    )
                plotTiTv(res = laml.Tab10_Summary_Plot_Fraction)
                dev.off()
            }
        )

        output$Tab10_Download_TT_Plot_SVG <- downloadHandler(
            filename = function() {
                paste("Transitions_and_Transversions", ".svg", sep = "")
            },
            content = function(file) {
                svg(file,
                    width = Tab10_TT_Plot_Width,
                    height = input$Tab10_TT_Plot_Height
                )
                laml.Tab10_Summary_Plot_Fraction <-
                    Tab10_Summary_Plot_Fraction(
                        maf = dataInputMAFPLOT(),
                        plot = input$Tab10_TT_Plot_Fraction,
                        useSyn = input$Tab10_TT_Include_Synonymous_Variants
                    )
                plotTiTv(res = laml.Tab10_Summary_Plot_Fraction)
                dev.off()
            }
        )


        # Lollipop plot
        observe({
            vchoiceslol <-
                unique(datalist[["MAF_Val"]]()[, input$Tab10_Lollipop_Gene_Name_Column])
            updateSelectInput(
                session,
                "Tab10_Lollipop_Select_Gene_1",
                choices = vchoiceslol,
                selected = vchoiceslol[1]
            )
            updateSelectInput(
                session,
                "Tab10_Lollipop_Select_Gene_2",
                choices = vchoiceslol,
                selected = vchoiceslol[2]
            )
            updateSelectInput(
                session,
                "Tab10_Lollipop_Select_Gene_3",
                choices = vchoiceslol,
                selected = vchoiceslol[3]
            )
        })

        # Lollipop 1
        Lol1 <- metaReactive2({
            validate(need(
                !is.null(datalist[["MAF_manual_data"]]()) | is.null(rowselect()),
                "Please input MAF file"
            ))
            if (input$Tab10_Lollipop_Position_Label_1 == "None") {
                metaExpr({
                    lollipopPlot(
                        ..(dataInputMAFPLOT()),
                        gene = ..(input$Tab10_Lollipop_Select_Gene_1),
                        AACol = "HGVSp_Short",
                        labPosSize = ..(input$Tab10_Lollipop_Label_Size_1),
                        showMutationRate = ..(
                            input$Tab10_Lollipop_Show_Mutation_Rate_1
                        ),
                        showDomainLabel = ..(input$Tab10_Lollipop_Label_Domains_1),
                        repel = ..(input$Tab10_Lollipop_Repel_Yes_or_No_1),
                        showLegend = ..(input$Tab10_Lollipop_Show_Legend_1),
                        legendTxtSize = ..(input$Tab10_Lollipop_Size_Legend_1),
                        labPosAngle = ..(input$Tab10_Lollipop_Label_Angle_1),
                        domainLabelSize = ..(
                            input$Tab10_Lollipop_Label_Domains_Size_1
                        )
                    )
                })
            } else {
                metaExpr({
                    lollipopPlot(
                        ..(dataInputMAFPLOT()),
                        gene = ..(input$Tab10_Lollipop_Select_Gene_1),
                        AACol = "HGVSp_Short",
                        labelPos = ..(input$Tab10_Lollipop_Position_Label_1),
                        labPosSize = ..(input$Tab10_Lollipop_Label_Size_1),
                        showMutationRate = ..(
                            input$Tab10_Lollipop_Show_Mutation_Rate_1
                        ),
                        showDomainLabel = ..(input$Tab10_Lollipop_Label_Domains_1),
                        repel = ..(input$Tab10_Lollipop_Repel_Yes_or_No_1),
                        showLegend = ..(input$Tab10_Lollipop_Show_Legend_1),
                        legendTxtSize = ..(input$Tab10_Lollipop_Size_Legend_1),
                        labPosAngle = ..(input$Tab10_Lollipop_Label_Angle_1),
                        domainLabelSize = ..(
                            input$Tab10_Lollipop_Label_Domains_Size_1
                        )
                    )
                })
            }
        })

        output$lol1 <- metaRender(renderPlot, {
            ..(Lol1())
        })

        # Download
        output$Tab10_Download_Lollipop_Plot_1_PNG <-
            downloadHandler(
                filename = function() {
                    paste("Lollipop_Plot_1", ".png", sep = "")
                },
                content = function(file) {
                    png(
                        file,
                        width = input$Tab10_Lollipop_Plot_Width_1,
                        height = input$Tab10_Lollipop_Plot_Height_1,
                        units = "in",
                        res = 1200
                    )
                    if (input$Tab10_Lollipop_Position_Label_1 == "None") {
                        lollipopPlot(
                            dataInputMAFPLOT(),
                            gene = input$Tab10_Lollipop_Select_Gene_1,
                            AACol = "HGVSp_Short",
                            labPosSize = input$Tab10_Lollipop_Label_Size_1,
                            showMutationRate = input$Tab10_Lollipop_Show_Mutation_Rate_1,
                            showDomainLabel = input$Tab10_Lollipop_Label_Domains_1,
                            repel = input$Tab10_Lollipop_Repel_Yes_or_No_1,
                            showLegend = input$Tab10_Lollipop_Show_Legend_1,
                            legendTxtSize = input$Tab10_Lollipop_Size_Legend_1,
                            labPosAngle = input$Tab10_Lollipop_Label_Angle_1,
                            domainLabelSize = input$Tab10_Lollipop_Label_Domains_Size_1
                        )
                    } else {
                        lollipopPlot(
                            dataInputMAFPLOT(),
                            gene = input$Tab10_Lollipop_Select_Gene_1,
                            AACol = "HGVSp_Short",
                            labelPos = input$Tab10_Lollipop_Position_Label_1,
                            labPosSize = input$Tab10_Lollipop_Label_Size_1,
                            showMutationRate = input$Tab10_Lollipop_Show_Mutation_Rate_1,
                            showDomainLabel = input$Tab10_Lollipop_Label_Domains_1,
                            repel = input$Tab10_Lollipop_Repel_Yes_or_No_1,
                            showLegend = input$Tab10_Lollipop_Show_Legend_1,
                            legendTxtSize = input$Tab10_Lollipop_Size_Legend_1,
                            labPosAngle = input$Tab10_Lollipop_Label_Angle_1,
                            domainLabelSize = input$Tab10_Lollipop_Label_Domains_Size_1
                        )
                    }
                    dev.off()
                }
            )

        output$Tab10_Download_Lollipop_Plot_1_SVG <-
            downloadHandler(
                filename = function() {
                    paste("Lollipop_Plot_1", ".svg", sep = "")
                },
                content = function(file) {
                    svg(
                        file,
                        width = input$Tab10_Lollipop_Plot_Width_1,
                        height = input$Tab10_Lollipop_Plot_Height_1
                    )
                    if (input$Tab10_Lollipop_Position_Label_1 == "None") {
                        lollipopPlot(
                            dataInputMAFPLOT(),
                            gene = input$Tab10_Lollipop_Select_Gene_1,
                            AACol = "HGVSp_Short",
                            labPosSize = input$Tab10_Lollipop_Label_Size_1,
                            showMutationRate = input$Tab10_Lollipop_Show_Mutation_Rate_1,
                            showDomainLabel = input$Tab10_Lollipop_Label_Domains_1,
                            repel = input$Tab10_Lollipop_Repel_Yes_or_No_1,
                            showLegend = input$Tab10_Lollipop_Show_Legend_1,
                            legendTxtSize = input$Tab10_Lollipop_Size_Legend_1,
                            labPosAngle = input$Tab10_Lollipop_Label_Angle_1,
                            domainLabelSize = input$Tab10_Lollipop_Label_Domains_Size_1
                        )
                    } else {
                        lollipopPlot(
                            dataInputMAFPLOT(),
                            gene = input$Tab10_Lollipop_Select_Gene_1,
                            AACol = "HGVSp_Short",
                            labelPos = input$Tab10_Lollipop_Position_Label_1,
                            labPosSize = input$Tab10_Lollipop_Label_Size_1,
                            showMutationRate = input$Tab10_Lollipop_Show_Mutation_Rate_1,
                            showDomainLabel = input$Tab10_Lollipop_Label_Domains_1,
                            repel = input$Tab10_Lollipop_Repel_Yes_or_No_1,
                            showLegend = input$Tab10_Lollipop_Show_Legend_1,
                            legendTxtSize = input$Tab10_Lollipop_Size_Legend_1,
                            labPosAngle = input$Tab10_Lollipop_Label_Angle_1,
                            domainLabelSize = input$Tab10_Lollipop_Label_Domains_Size_1
                        )
                    }
                    dev.off()
                }
            )

        # Lollipop 2
        Lol2 <- metaReactive2({
            validate(need(
                !is.null(datalist[["MAF_manual_data"]]()) | is.null(rowselect()),
                "Please input MAF file"
            ))
            if (input$Tab10_Lollipop_Position_Label_2 == "None") {
                metaExpr({
                    lollipopPlot(
                        ..(dataInputMAFPLOT()),
                        gene = ..(input$Tab10_Lollipop_Select_Gene_2),
                        AACol = "HGVSp_Short",
                        labPosSize = ..(input$Tab10_Lollipop_Label_Size_2),
                        showMutationRate = ..(
                            input$Tab10_Lollipop_Show_Mutation_Rate_2
                        ),
                        showDomainLabel = ..(input$Tab10_Lollipop_Label_Domains_2),
                        repel = ..(input$Tab10_Lollipop_Repel_Yes_or_No_2),
                        showLegend = ..(input$Tab10_Lollipop_Show_Legend_2),
                        legendTxtSize = ..(input$Tab10_Lollipop_Size_Legend_2),
                        labPosAngle = ..(input$Tab10_Lollipop_Label_Angle_2),
                        domainLabelSize = ..(
                            input$Tab10_Lollipop_Label_Domains_Size_2
                        )
                    )
                })
            } else {
                metaExpr({
                    lollipopPlot(
                        ..(dataInputMAFPLOT()),
                        gene = ..(input$Tab10_Lollipop_Select_Gene_2),
                        AACol = "HGVSp_Short",
                        labelPos = ..(input$Tab10_Lollipop_Position_Label_2),
                        labPosSize = ..(input$Tab10_Lollipop_Label_Size_2),
                        showMutationRate = ..(
                            input$Tab10_Lollipop_Show_Mutation_Rate_2
                        ),
                        showDomainLabel = ..(input$Tab10_Lollipop_Label_Domains_2),
                        repel = ..(input$Tab10_Lollipop_Repel_Yes_or_No_2),
                        showLegend = ..(input$Tab10_Lollipop_Show_Legend_2),
                        legendTxtSize = ..(input$Tab10_Lollipop_Size_Legend_2),
                        labPosAngle = ..(input$Tab10_Lollipop_Label_Angle_2),
                        domainLabelSize = ..(
                            input$Tab10_Lollipop_Label_Domains_Size_2
                        )
                    )
                })
            }
        })

        output$lol2 <- metaRender(renderPlot, {
            ..(Lol2())
        })

        # Download
        output$Tab10_Download_Lollipop_Plot_2_PNG <-
            downloadHandler(
                filename = function() {
                    paste("Lollipop_Plot_2", ".png", sep = "")
                },
                content = function(file) {
                    png(
                        file,
                        width = input$Tab10_Lollipop_Plot_Width_2,
                        height = input$Tab10_Lollipop_Plot_Height_2,
                        units = "in",
                        res = 1200
                    )
                    if (input$Tab10_Lollipop_Position_Label_2 == "None") {
                        lollipopPlot(
                            dataInputMAFPLOT(),
                            gene = input$Tab10_Lollipop_Select_Gene_2,
                            AACol = "HGVSp_Short",
                            labPosSize = input$Tab10_Lollipop_Label_Size_2,
                            showMutationRate = input$Tab10_Lollipop_Show_Mutation_Rate_2,
                            showDomainLabel = input$Tab10_Lollipop_Label_Domains_2,
                            repel = input$Tab10_Lollipop_Repel_Yes_or_No_2,
                            showLegend = input$Tab10_Lollipop_Show_Legend_2,
                            legendTxtSize = input$Tab10_Lollipop_Size_Legend_2,
                            labPosAngle = input$Tab10_Lollipop_Label_Angle_2,
                            domainLabelSize = input$Tab10_Lollipop_Label_Domains_Size_2
                        )
                    } else {
                        lollipopPlot(
                            dataInputMAFPLOT(),
                            gene = input$Tab10_Lollipop_Select_Gene_2,
                            AACol = "HGVSp_Short",
                            labelPos = input$Tab10_Lollipop_Position_Label_2,
                            labPosSize = input$Tab10_Lollipop_Label_Size_2,
                            showMutationRate = input$Tab10_Lollipop_Show_Mutation_Rate_2,
                            showDomainLabel = input$Tab10_Lollipop_Label_Domains_2,
                            repel = input$Tab10_Lollipop_Repel_Yes_or_No_2,
                            showLegend = input$Tab10_Lollipop_Show_Legend_2,
                            legendTxtSize = input$Tab10_Lollipop_Size_Legend_2,
                            labPosAngle = input$Tab10_Lollipop_Label_Angle_2,
                            domainLabelSize = input$Tab10_Lollipop_Label_Domains_Size_2
                        )
                    }
                    dev.off()
                }
            )

        output$Tab10_Download_Lollipop_Plot_2_SVG <-
            downloadHandler(
                filename = function() {
                    paste("Lollipop_Plot_2", ".svg", sep = "")
                },
                content = function(file) {
                    svg(
                        file,
                        width = input$Tab10_Lollipop_Plot_Width_2,
                        height = input$Tab10_Lollipop_Plot_Height_2
                    )
                    if (input$Tab10_Lollipop_Position_Label_2 == "None") {
                        lollipopPlot(
                            dataInputMAFPLOT(),
                            gene = input$Tab10_Lollipop_Select_Gene_2,
                            AACol = "HGVSp_Short",
                            labPosSize = input$Tab10_Lollipop_Label_Size_2,
                            showMutationRate = input$Tab10_Lollipop_Show_Mutation_Rate_2,
                            showDomainLabel = input$Tab10_Lollipop_Label_Domains_2,
                            repel = input$Tab10_Lollipop_Repel_Yes_or_No_2,
                            showLegend = input$Tab10_Lollipop_Show_Legend_2,
                            legendTxtSize = input$Tab10_Lollipop_Size_Legend_2,
                            labPosAngle = input$Tab10_Lollipop_Label_Angle_2,
                            domainLabelSize = input$Tab10_Lollipop_Label_Domains_Size_2
                        )
                    } else {
                        lollipopPlot(
                            dataInputMAFPLOT(),
                            gene = input$Tab10_Lollipop_Select_Gene_2,
                            AACol = "HGVSp_Short",
                            labelPos = input$Tab10_Lollipop_Position_Label_2,
                            labPosSize = input$Tab10_Lollipop_Label_Size_2,
                            showMutationRate = input$Tab10_Lollipop_Show_Mutation_Rate_2,
                            showDomainLabel = input$Tab10_Lollipop_Label_Domains_2,
                            repel = input$Tab10_Lollipop_Repel_Yes_or_No_2,
                            showLegend = input$Tab10_Lollipop_Show_Legend_2,
                            legendTxtSize = input$Tab10_Lollipop_Size_Legend_2,
                            labPosAngle = input$Tab10_Lollipop_Label_Angle_2,
                            domainLabelSize = input$Tab10_Lollipop_Label_Domains_Size_2
                        )
                    }
                    dev.off()
                }
            )

        # Lollipop 3
        Lol3 <- metaReactive2({
            validate(need(
                !is.null(datalist[["MAF_manual_data"]]()) | is.null(rowselect()),
                "Please input MAF file"
            ))
            if (input$Tab10_Lollipop_Position_Label_3 == "None") {
                metaExpr({
                    lollipopPlot(
                        ..(dataInputMAFPLOT()),
                        gene = ..(input$Tab10_Lollipop_Select_Gene_3),
                        AACol = "HGVSp_Short",
                        labPosSize = ..(input$Tab10_Lollipop_Label_Size_3),
                        showMutationRate = ..(
                            input$Tab10_Lollipop_Show_Mutation_Rate_3
                        ),
                        showDomainLabel = ..(input$Tab10_Lollipop_Label_Domains_3),
                        repel = ..(input$Tab10_Lollipop_Repel_Yes_or_No_3),
                        showLegend = ..(input$Tab10_Lollipop_Show_Legend_3),
                        legendTxtSize = ..(input$Tab10_Lollipop_Size_Legend_3),
                        labPosAngle = ..(input$Tab10_Lollipop_Label_Angle_3),
                        domainLabelSize = ..(
                            input$Tab10_Lollipop_Label_Domains_Size_3
                        )
                    )
                })
            } else {
                metaExpr({
                    lollipopPlot(
                        ..(dataInputMAFPLOT()),
                        gene = ..(input$Tab10_Lollipop_Select_Gene_3),
                        AACol = "HGVSp_Short",
                        labelPos = ..(input$Tab10_Lollipop_Position_Label_3),
                        labPosSize = ..(input$Tab10_Lollipop_Label_Size_3),
                        showMutationRate = ..(
                            input$Tab10_Lollipop_Show_Mutation_Rate_3
                        ),
                        showDomainLabel = ..(input$Tab10_Lollipop_Label_Domains_3),
                        repel = ..(input$Tab10_Lollipop_Repel_Yes_or_No_3),
                        showLegend = ..(input$Tab10_Lollipop_Show_Legend_3),
                        legendTxtSize = ..(input$Tab10_Lollipop_Size_Legend_3),
                        labPosAngle = ..(input$Tab10_Lollipop_Label_Angle_3),
                        domainLabelSize = ..(
                            input$Tab10_Lollipop_Label_Domains_Size_3
                        )
                    )
                })
            }
        })

        output$lol3 <- metaRender(renderPlot, {
            ..(Lol3())
        })

        # Download
        output$Tab10_Download_Lollipop_Plot_3_PNG <-
            downloadHandler(
                filename = function() {
                    paste("Lollipop_Plot_3", ".png", sep = "")
                },
                content = function(file) {
                    png(
                        file,
                        width = input$Tab10_Lollipop_Plot_Width_3,
                        height = input$Tab10_Lollipop_Plot_Height_3,
                        units = "in",
                        res = 1200
                    )
                    if (input$Tab10_Lollipop_Position_Label_3 == "None") {
                        lollipopPlot(
                            dataInputMAFPLOT(),
                            gene = input$Tab10_Lollipop_Select_Gene_3,
                            AACol = "HGVSp_Short",
                            labPosSize = input$Tab10_Lollipop_Label_Size_3,
                            showMutationRate = input$Tab10_Lollipop_Show_Mutation_Rate_3,
                            showDomainLabel = input$Tab10_Lollipop_Label_Domains_3,
                            repel = input$Tab10_Lollipop_Repel_Yes_or_No_3,
                            showLegend = input$Tab10_Lollipop_Show_Legend_3,
                            legendTxtSize = input$Tab10_Lollipop_Size_Legend_3,
                            labPosAngle = input$Tab10_Lollipop_Label_Angle_3,
                            domainLabelSize = input$Tab10_Lollipop_Label_Domains_Size_3
                        )
                    } else {
                        lollipopPlot(
                            dataInputMAFPLOT(),
                            gene = input$Tab10_Lollipop_Select_Gene_3,
                            AACol = "HGVSp_Short",
                            labelPos = input$Tab10_Lollipop_Position_Label_3,
                            labPosSize = input$Tab10_Lollipop_Label_Size_3,
                            showMutationRate = input$Tab10_Lollipop_Show_Mutation_Rate_3,
                            showDomainLabel = input$Tab10_Lollipop_Label_Domains_3,
                            repel = input$Tab10_Lollipop_Repel_Yes_or_No_3,
                            showLegend = input$Tab10_Lollipop_Show_Legend_3,
                            legendTxtSize = input$Tab10_Lollipop_Size_Legend_3,
                            labPosAngle = input$Tab10_Lollipop_Label_Angle_3,
                            domainLabelSize = input$Tab10_Lollipop_Label_Domains_Size_3
                        )
                    }
                    dev.off()
                }
            )

        output$Tab10_Download_Lollipop_Plot_3_SVG <-
            downloadHandler(
                filename = function() {
                    paste("Lollipop_Plot_3", ".svg", sep = "")
                },
                content = function(file) {
                    svg(
                        file,
                        width = input$Tab10_Lollipop_Plot_Width_3,
                        height = input$Tab10_Lollipop_Plot_Height_3
                    )
                    if (input$Tab10_Lollipop_Position_Label_3 == "None") {
                        lollipopPlot(
                            dataInputMAFPLOT(),
                            gene = input$Tab10_Lollipop_Select_Gene_3,
                            AACol = "HGVSp_Short",
                            labPosSize = input$Tab10_Lollipop_Label_Size_3,
                            showMutationRate = input$Tab10_Lollipop_Show_Mutation_Rate_3,
                            showDomainLabel = input$Tab10_Lollipop_Label_Domains_3,
                            repel = input$Tab10_Lollipop_Repel_Yes_or_No_3,
                            showLegend = input$Tab10_Lollipop_Show_Legend_3,
                            legendTxtSize = input$Tab10_Lollipop_Size_Legend_3,
                            labPosAngle = input$Tab10_Lollipop_Label_Angle_3,
                            domainLabelSize = input$Tab10_Lollipop_Label_Domains_Size_3
                        )
                    } else {
                        lollipopPlot(
                            dataInputMAFPLOT(),
                            gene = input$Tab10_Lollipop_Select_Gene_3,
                            AACol = "HGVSp_Short",
                            labelPos = input$Tab10_Lollipop_Position_Label_3,
                            labPosSize = input$Tab10_Lollipop_Label_Size_3,
                            showMutationRate = input$Tab10_Lollipop_Show_Mutation_Rate_3,
                            showDomainLabel = input$Tab10_Lollipop_Label_Domains_3,
                            repel = input$Tab10_Lollipop_Repel_Yes_or_No_3,
                            showLegend = input$Tab10_Lollipop_Show_Legend_3,
                            legendTxtSize = input$Tab10_Lollipop_Size_Legend_3,
                            labPosAngle = input$Tab10_Lollipop_Label_Angle_3,
                            domainLabelSize = input$Tab10_Lollipop_Label_Domains_Size_3
                        )
                    }
                    dev.off()
                }
            )

        # 1) Mutation Load Plot
        Mut <- metaReactive2({
            validate(need(
                !is.null(datalist[["MAF_manual_data"]]()) | is.null(rowselect()),
                "Please input MAF file"
            ))
            metaExpr({
                laml.mutload <-
                    tcgaCompare(
                        maf = ..(dataInputMAFPLOT()),
                        cohortName = "Our Data"
                    )
            })
        })

        output$Mutload <- metaRender(renderPlot, {
            ..(Mut())
        })

        # Download
        output$Tab10_Download_Mutation_Load_Plot_PNG <-
            downloadHandler(
                filename = function() {
                    paste("Mutation_Load", ".png", sep = "")
                },
                content = function(file) {
                    png(
                        file,
                        width = input$Tab10_Mutation_Load_Plot_Width,
                        height = input$Tab10_Mutation_Load_Plot_Height,
                        units = "in",
                        res = 1200
                    )
                    laml.mutload <-
                        tcgaCompare(maf = dataInputMAFPLOT(), cohortName = "Our Data")
                    dev.off()
                }
            )

        output$Tab10_Download_Mutation_Load_Plot_SVG <-
            downloadHandler(
                filename = function() {
                    paste("Mutation_Load", ".svg", sep = "")
                },
                content = function(file) {
                    svg(
                        file,
                        width = input$Tab10_Mutation_Load_Plot_Width,
                        height = input$Tab10_Mutation_Load_Plot_Height
                    )
                    laml.mutload <-
                        tcgaCompare(maf = dataInputMAFPLOT(), cohortName = "Our Data")
                    dev.off()
                }
            )

        # 2) Somatic Interactions Plot
        VAF <- metaReactive2({
            validate(need(
                !is.null(datalist[["MAF_manual_data"]]()) | is.null(rowselect()),
                "Please input MAF file"
            ))
            metaExpr({
                somaticInteractions(
                    maf = ..(dataInputMAFPLOT()),
                    top = ..(input$Tab10_SIP_Display_Top_Genes),
                    pvalue = c(
                        ..(input$Tab10_SIP_Pval_Lower_Threshold),
                        ..(input$Tab10_SIP_Pval_Upper_Threshold)
                    )
                )
            })
        })

        output$VAF1 <- metaRender(renderPlot, {
            ..(VAF())
        })

        # Download
        output$Tab10_Download_SIP_Plot_PNG <- downloadHandler(
            filename = function() {
                paste("Somatic_Interactions", ".png", sep = "")
            },
            content = function(file) {
                png(
                    file,
                    width = input$Tab10_SIP_Plot_Width,
                    height = input$Tab10_SIP_Plot_Height,
                    units = "in",
                    res = 1200
                )
                somaticInteractions(
                    maf = dataInputMAFPLOT(),
                    top = input$Tab10_SIP_Display_Top_Genes,
                    pvalue = c(
                        input$Tab10_SIP_Pval_Lower_Threshold,
                        input$Tab10_SIP_Pval_Upper_Threshold
                    )
                )
                dev.off()
            }
        )

        output$Tab10_Download_SIP_Plot_SVG <- downloadHandler(
            filename = function() {
                paste("Somatic_Interactions", ".svg", sep = "")
            },
            content = function(file) {
                svg(
                    file,
                    width = input$Tab10_SIP_Plot_Width,
                    height = input$Tab10_SIP_Plot_Height
                )
                somaticInteractions(
                    maf = dataInputMAFPLOT(),
                    top = input$Tab10_SIP_Display_Top_Genes,
                    pvalue = c(
                        input$Tab10_SIP_Pval_Lower_Threshold,
                        input$Tab10_SIP_Pval_Upper_Threshold
                    )
                )
                dev.off()
            }
        )
    })
}
