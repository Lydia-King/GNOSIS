# Server function for cBioPortalData API
Tab1_API_Files_Server <- function(API) {
    moduleServer(API, function(input, output, session) {
        API_data <- metaReactive2({
            metaExpr({
                tryCatch(
                    {
                        cbio <- cBioPortal(
                            hostname = "www.cbioportal.org",
                            protocol = "https",
                            api. = "/api/api-docs"
                        )
                        as.data.frame(getStudies(cbio))
                    },
                    error = function(e) {
                        message(e)
                        return(NULL)
                    }
                )
            })
        })

        output$cBioData <- metaRender(renderDataTable, {
            metaExpr({
                datatable(
                    ..(API_data()),
                    selection = "single",
                    options = list(
                        lengthMenu = c(10, 30, 50, 100),
                        pageLength = 30,
                        scrollX = TRUE,
                        scrollY = "650px"
                    )
                )
            })
        })
    })
}

# Server function to upload data either manually or using API
Tab1_Input_Files_Manual_Server <- function(id, rowselect) {
    moduleServer(id, function(input, output, session) {
        # Clinical data
        patient_data <- metaReactive2({
            if (is.null(input$Input_Patient_File)) {
                return(NULL)
            } else {
                metaExpr({
                    read.delim(
                        ..(input$Input_Patient_File$datapath),
                        header = ..(input$Tab1_Clin_Header_Yes_or_No),
                        sep = ..(input$Tab1_Clin_Separator),
                        quote = ..(input$Tab1_Clin_Quote),
                        na.strings = c("", " ", "NA"),
                        comment.char = ..(input$Tab1_Comment_1),
                        skip = ..(input$Tab1_Clin_Skip_Lines)
                    )
                })
            }
        })

        sample_data <- metaReactive2({
            if (is.null(input$Input_Sample_File)) {
                return(NULL)
            } else {
                metaExpr({
                    read.delim(
                        ..(input$Input_Sample_File$datapath),
                        header = ..(input$Tab1_Sample_Header_Yes_or_No),
                        sep = ..(input$Tab1_Sample_Separator),
                        quote = ..(input$Tab1_Sample_Quote),
                        na.strings = c("", " ", "NA"),
                        comment.char = ..(input$Tab1_Comment_2),
                        skip = ..(input$Tab1_Sample_Skip_Lines)
                    )
                })
            }
        })

        # CNA data
        CNA_data <- metaReactive2({
            if (is.null(input$Input_CNA_File)) {
                return(NULL)
            } else {
                metaExpr({
                    read.delim(
                        ..(input$Input_CNA_File$datapath),
                        header = ..(input$Tab1_CNA_Header_Yes_or_No),
                        sep = ..(input$Tab1_CNA_Separator),
                        quote = ..(input$Tab1_CNA_Quote),
                        na.strings = c("", " ", "NA"),
                        check.names = FALSE,
                        comment.char = ..(input$Tab1_Comment_3),
                        skip = ..(input$Tab1_CNA_Skip_Lines)
                    )
                })
            }
        })


        # MAF data
        MAF_data <- metaReactive2({
            if (is.null(input$Input_MAF_File)) {
                return(NULL)
            } else {
                tryCatch(
                    {
                        metaExpr({
                            read.delim(
                                ..(input$Input_MAF_File$datapath),
                                header = ..(input$Tab1_MAF_Header_Yes_or_No),
                                sep = ..(input$Tab1_MAF_Separator),
                                quote = ..(input$Tab1_MAF_Quote),
                                na.strings = c("", " ", "NA"),
                                comment.char = ..(input$Tab1_Comment_4),
                                skip = ..(input$Tab1_MAF_Skip_Lines)
                            )
                        })
                    },
                    error = function(e) {
                        message(e)
                        return(NULL)
                    }
                )
            }
        })

        # API cBioPortalData
        API_Out <- metaReactive2({
            if (is.null(rowselect())) {
                return(NULL)
            } else {
                tryCatch(
                    {
                        metaExpr({
                            cbio <- cBioPortal(
                                hostname = "www.cbioportal.org",
                                protocol = "https",
                                api. = "/api/api-docs"
                            )
                            API <-
                                as.data.frame(getStudies(cbio))
                            samp <- API[..(rowselect()), "studyId"]
                            download <-
                                downloadStudy(samp, ask = FALSE)
                            return(list(
                                download = download,
                                samp = samp
                            ))
                        })
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            }
        })

        API_data_output <- metaReactive2({
            if (is.null(rowselect())) {
                return(NULL)
            } else if (is.null(API_Out())) {
                validate(
                    need(
                        !is.null(API_Out()),
                        "Error: Download failed, please select another dataset to analyse."
                    )
                )
                return(NULL)
            } else {
                tryCatch(
                    {
                        metaExpr({
                            study <- ..(API_Out())[["download"]]
                            samp <- ..(API_Out())[["samp"]]
                            file_dir <-
                                untarStudy(study, tempdir())

                            patient_clin_file <-
                                if (file.exists(file.path(
                                    file_dir,
                                    samp,
                                    "data_clinical_patient.txt"
                                ))) {
                                    read.delim(
                                        file.path(
                                            file_dir,
                                            samp,
                                            "data_clinical_patient.txt"
                                        ),
                                        header = ..(
                                            input$Tab1_Clin_Header_Yes_or_No
                                        ),
                                        sep = ..(input$Tab1_Clin_Separator),
                                        quote = ..(input$Tab1_Clin_Quote),
                                        na.strings = c("", " ", "NA"),
                                        comment.char = ..(input$Tab1_Comment_1)
                                    )
                                } else {
                                    NULL
                                }

                            sample_clin_file <-
                                if (file.exists(file.path(
                                    file_dir,
                                    samp,
                                    "data_clinical_sample.txt"
                                ))) {
                                    read.delim(
                                        file.path(
                                            file_dir,
                                            samp,
                                            "data_clinical_sample.txt"
                                        ),
                                        header = ..(
                                            input$Tab1_Sample_Header_Yes_or_No
                                        ),
                                        sep = ..(input$Tab1_Sample_Separator),
                                        quote = ..(input$Tab1_Sample_Quote),
                                        na.strings = c("", " ", "NA"),
                                        comment.char = ..(input$Tab1_Comment_2)
                                    )
                                } else {
                                    NULL
                                }

                            CNA_file <-
                                if (file.exists(file.path(file_dir, samp, "data_cna.txt"))) {
                                    read.delim(
                                        file.path(
                                            file_dir,
                                            samp,
                                            "data_cna.txt"
                                        ),
                                        header = ..(
                                            input$Tab1_CNA_Header_Yes_or_No
                                        ),
                                        sep = ..(input$Tab1_CNA_Separator),
                                        quote = ..(input$Tab1_CNA_Quote),
                                        na.strings = c("", " ", "NA"),
                                        check.names = FALSE,
                                        comment.char = ..(input$Tab1_Comment_3)
                                    )
                                } else {
                                    NULL
                                }

                            MAF_file <-
                                if (file.exists(file.path(
                                    file_dir,
                                    samp,
                                    "data_mutations.txt"
                                ))) {
                                    read.delim(
                                        file.path(
                                            file_dir,
                                            samp,
                                            "data_mutations.txt"
                                        ),
                                        header = ..(
                                            input$Tab1_MAF_Header_Yes_or_No
                                        ),
                                        sep = ..(input$Tab1_MAF_Separator),
                                        quote = ..(input$Tab1_MAF_Quote),
                                        na.strings = c("", " ", "NA"),
                                        comment.char = ..(input$Tab1_Comment_4)
                                    )
                                } else {
                                    NULL
                                }

                            return(
                                list(
                                    patient = patient_clin_file,
                                    sample = sample_clin_file,
                                    CNA = CNA_file,
                                    MAF = MAF_file
                                )
                            )
                        })
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )
            }
        })

        # Validated data
        dataClinical <- metaReactive2({
            if (is.null(input$Input_Sample_File) &
                !is.null(input$Input_Patient_File) &
                is.null(API_data_output())) {
                validate(
                    need(
                        ncol(patient_data()) > 2,
                        "Inputted file only has one column, please select file delimiters and options carefully."
                    )
                )
                validate(
                    need(
                        "PATIENT_ID" %in% colnames(patient_data()),
                        "Inputted file does not have PATIENT_ID information - Are you using the right file?"
                    )
                )
                metaExpr({
                    ..(patient_data())
                })
            } else if (is.null(input$Input_Patient_File) &
                !is.null(input$Input_Sample_File) &
                is.null(API_data_output())) {
                validate(
                    need(
                        ncol(sample_data()) > 2,
                        "Inputted file only has one column, please select file delimiters and options carefully."
                    )
                )
                validate(
                    need(
                        "PATIENT_ID" %in% colnames(sample_data()),
                        "Inputted file does not have PATIENT_ID information - Are you using the right file?"
                    )
                )
                metaExpr({
                    ..(sample_data())
                })
            } else if (is.null(input$Input_Sample_File) &
                is.null(input$Input_Patient_File) &
                is.null(API_data_output())) {
                validate(
                    need(
                        !is.null(API_data_output()) |
                            !is.null(input$Input_Sample_File) |
                            !is.null(input$Input_Patient_File),
                        "Please select cBioPortal dataset or upload your own data."
                    )
                )
            } else if (!is.null(input$Input_Sample_File) &
                !is.null(input$Input_Patient_File) &
                is.null(API_data_output())) {
                validate(
                    need(
                        ncol(patient_data()) > 2,
                        "Inputted file only has one column, please select file delimiters and options carefully."
                    )
                )
                validate(
                    need(
                        "PATIENT_ID" %in% colnames(patient_data()),
                        "Inputted file does not have PATIENT_ID information - Are you using the right file?"
                    )
                )
                validate(
                    need(
                        ncol(sample_data()) > 2,
                        "Inputted file only has one column, please select file delimiters and options carefully."
                    )
                )
                validate(
                    need(
                        "PATIENT_ID" %in% colnames(sample_data()),
                        "Inputted file does not have PATIENT_ID information - Are you using the right file?"
                    )
                )
                metaExpr({
                    merge(
                        ..(patient_data()),
                        ..(sample_data()),
                        by.x = "PATIENT_ID",
                        by.y = "PATIENT_ID"
                    )
                })
            } else if (is.null(input$Input_Sample_File) &
                is.null(input$Input_Patient_File) &
                !is.null(API_data_output())) {
                validate(need(
                    !is.null(API_data_output()),
                    "Please select cBioPortal dataset."
                ))
                validate(
                    need(
                        !is.null(API_data_output()[["patient"]]) |
                            !is.null(API_data_output()[["sample"]]),
                        "Selected cBioPortal dataset does not contain clinical data, please select another dataset"
                    )
                )
                metaExpr({
                    if (!is.null(API_data_output()[["patient"]]) &
                        is.null(API_data_output()[["sample"]])) {
                        validate(
                            need(
                                ncol(API_data_output()[["patient"]]) > 2,
                                "Inputted file only has one column, please select file delimiters and options carefully."
                            )
                        )
                        validate(
                            need(
                                "PATIENT_ID" %in% colnames(API_data_output()[["patient"]]),
                                "Inputted file does not have PATIENT_ID information - Are you using the right file?"
                            )
                        )
                        API_data_output()[["patient"]]
                    } else if (is.null(API_data_output()[["patient"]]) &
                        !is.null(API_data_output()[["sample"]])) {
                        validate(
                            need(
                                ncol(API_data_output()[["sample"]]) > 2,
                                "Inputted file only has one column, please select file delimiters and options carefully."
                            )
                        )
                        validate(
                            need(
                                "PATIENT_ID" %in% colnames(API_data_output()[["sample"]]),
                                "Inputted file does not have PATIENT_ID information - Are you using the right file?"
                            )
                        )
                        API_data_output()[["sample"]]
                    } else {
                        validate(
                            need(
                                ncol(API_data_output()[["patient"]]) > 2,
                                "Inputted file only has one column, please select file delimiters and options carefully."
                            )
                        )
                        validate(
                            need(
                                "PATIENT_ID" %in% colnames(API_data_output()[["patient"]]),
                                "Inputted file does not have PATIENT_ID information - Are you using the right file?"
                            )
                        )
                        validate(
                            need(
                                ncol(API_data_output()[["sample"]]) > 2,
                                "Inputted file only has one column, please select file delimiters and options carefully."
                            )
                        )
                        validate(
                            need(
                                "PATIENT_ID" %in% colnames(API_data_output()[["sample"]]),
                                "Inputted file does not have PATIENT_ID information - Are you using the right file?"
                            )
                        )
                        merge(
                            ..(API_data_output()[["patient"]]),
                            ..(API_data_output()[["sample"]]),
                            by.x = "PATIENT_ID",
                            by.y = "PATIENT_ID"
                        )
                    }
                })
            } else {
                validate(
                    need(
                        is.null(API_data_output()) |
                            c(
                                is.null(input$Input_Sample_File) &
                                    is.null(input$Input_Patient_File)
                            ),
                        "Please only select cBioPortal dataset OR upload your own data."
                    )
                )
            }
        })

        CNA_Validated <- metaReactive2({
            if (is.null(input$Input_CNA_File) & is.null(API_data_output())) {
                validate(
                    need(
                        !is.null(input$Input_CNA_File) |
                            !is.null(API_data_output()),
                        "Please select cBioPortal dataset or upload your own CNA data."
                    )
                )
            } else if (!is.null(input$Input_CNA_File) &
                is.null(API_data_output())) {
                validate(
                    need(
                        ncol(CNA_data()) > 1,
                        "Inputted file only has one column, please select file delimiters and options carefully."
                    )
                )
                validate(
                    need(
                        "Hugo_Symbol" %in% colnames(CNA_data()),
                        "Inputted file does not have Gene information (Hugo_Symbol) - Are you using the right file?"
                    )
                )
                metaExpr({
                    ..(CNA_data())
                })
            } else if (is.null(input$Input_CNA_File) &
                !is.null(API_data_output())) {
                validate(
                    need(
                        !is.null(API_data_output()[["CNA"]]),
                        "Selected cBioPortal dataset does not contain the desired CNA data, please select another dataset"
                    )
                )
                validate(
                    need(
                        ncol(API_data_output()[["CNA"]]) > 1,
                        "Inputted file only has one column, please select file delimiters and options carefully."
                    )
                )
                validate(
                    need(
                        "Hugo_Symbol" %in% colnames(API_data_output()[["CNA"]]),
                        "Inputted file does not have Gene information (Hugo_Symbol) - Are you using the right file?"
                    )
                )
                metaExpr({
                    ..(API_data_output()[["CNA"]])
                })
            } else {
                validate(
                    need(
                        is.null(API_data_output()) | is.null(input$Input_CNA_File),
                        "Please only select cBioPortal dataset OR upload your own data."
                    )
                )
            }
        })

        MAF_Validated <- metaReactive2({
            if (is.null(input$Input_MAF_File) & is.null(API_data_output())) {
                validate(
                    need(
                        !is.null(input$Input_MAF_File) |
                            !is.null(API_data_output()),
                        "Please select cBioPortal dataset or upload your own mutation data."
                    )
                )
            } else if (!is.null(input$Input_MAF_File) &
                is.null(API_data_output())) {
                validate(
                    need(
                        ncol(MAF_data()) > 1,
                        "Inputted file only has one column, please select file delimiters and options carefully."
                    )
                )
                validate(
                    need(
                        "Hugo_Symbol" %in% colnames(MAF_data()),
                        "Inputted file does not have Gene information (Hugo_Symbol) - Are you using the right file?"
                    )
                )
                metaExpr({
                    ..(MAF_data())
                })
            } else if (is.null(input$Input_MAF_File) &
                !is.null(API_data_output())) {
                validate(
                    need(
                        !is.null(API_data_output()[["MAF"]]),
                        "Selected cBioPortal dataset does not contain mutation data, please select another dataset"
                    )
                )
                validate(
                    need(
                        ncol(API_data_output()[["MAF"]]) > 1,
                        "Inputted file only has one column, please select file delimiters and options carefully."
                    )
                )
                validate(
                    need(
                        "Hugo_Symbol" %in% colnames(API_data_output()[["MAF"]]),
                        "Inputted file does not have Gene information (Hugo_Symbol) - Are you using the right file?"
                    )
                )
                metaExpr({
                    ..(API_data_output()[["MAF"]])
                })
            } else {
                validate(
                    need(
                        is.null(API_data_output()) | is.null(input$Input_MAF_File),
                        "Please only select cBioPortal dataset OR upload your mutation own data."
                    )
                )
            }
        })


        rlist <- list(
            patient_manual_data = patient_data,
            sample_manual_data = sample_data,
            CNA_manual_data = CNA_data,
            MAF_manual_data = MAF_data,
            Combined_clin = dataClinical,
            CNA_Val = CNA_Validated,
            MAF_Val = MAF_Validated,
            API_Out = API_Out,
            API_data_output = API_data_output
        )
        return(rlist)
    })
}

# Functions to display dimensions
Count_Col <- function(dataset) {
    moduleServer(dataset, function(input, output, session) {
        metaExpr({
            ncol(..(dataset))
        })
    })
}

Count_Row <- function(dataset) {
    moduleServer(dataset, function(input, output, session) {
        metaExpr({
            nrow(..(dataset))
        })
    })
}

# Function to preview uploaded or selected cBioPortalData
Tab1_Input_Files_Preview_Server <-
    function(id, datalist, data, length_px, select_dt) {
        moduleServer(id, function(input, output, session) {
            loading_API <- function() {
                req(
                    !is.null(datalist[["API_Out"]]()) &
                        is.null(input$Input_Sample_File) &
                        is.null(input$Input_Patient_File)
                )
                try(message(datalist[["API_Out"]](), silent = TRUE))

                message("Loading: Please wait for data to display")
                Sys.sleep(2)
                message("Displaying Data")
            }

            observe({
                withCallingHandlers(
                    {
                        html("text", "")
                        loading_API()
                    },
                    message = function(m) {
                        m$message
                        if (m$message != "Displaying Data\n") {
                            hide("Preview")
                            show("text")
                            html(
                                id = "text",
                                html = paste(m$message, "<br>", sep = " "),
                                add = TRUE
                            )
                        } else {
                            hide("text")
                            show("Preview")
                        }
                    }
                )
            })

            output$Preview <- metaRender(renderDataTable, {
                metaExpr({
                    datatable(
                        ..(datalist[[data]]()),
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
        })
    }
