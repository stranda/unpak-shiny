source("upload.experiment.R")


#### By default, the file size limit is 5MB. It can be changed by
#### setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output) {
    output$contents <- renderDataTable({
                                        # input$file1 will be NULL initially. After the user selects
                                        # and uploads a file, it will be a data frame with 'name',
                                        # 'size', 'type', and 'datapath' columns. The 'datapath'
                                        # column will contain the local filenames where the data can
                                        # be found.
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        megin <- read.csv(inFile$datapath, header = T,
                          sep = ',')
        megin
        
    })
    
 output$downloadExample <- downloadHandler(
                                        filename = function() {
                                          paste("example-input-file.csv")
                                        },
                                        content = function(file) {
a  <- structure(list(expt.type = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L), .Label = "EXPT", class = "factor"), expt.id = structure(c(1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "3pt2", class = "factor"), 
    accession = structure(c(1L, 4L, 4L, 4L, 3L, 2L, 2L, 2L), .Label = c("CS70000", 
    "SALK_012144C", "SALK_123484C", "SALK_126593C"), class = "factor"), 
    parent.id = structure(c(4L, 3L, 3L, 3L, 2L, 1L, 1L, 1L), .Label = c("CofC_SF05_0275", 
    "CofC_SF05_209", "CofC_SF05_80", "new_seeds"), class = "factor"), 
    institution = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "CofC", class = "factor"), 
    facility = structure(c(2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L), .Label = c("GC1", 
    "GC2"), class = "factor"), expt.plantnum = c(64L, 112L, 650L, 
    848L, 10L, 133L, 206L, 361L), flat = c(13L, 14L, 22L, 24L, 
    13L, 14L, 15L, 18L), row = structure(c(5L, 3L, 1L, 4L, 2L, 
    5L, 5L, 1L), .Label = c("A", "B", "G", "J", "K"), class = "factor"), 
    column = c(4L, 4L, 2L, 2L, 4L, 1L, 2L, 1L), date = structure(c(1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "2013-01-01", class = "factor"), 
    investigator = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
    ), .Label = "anonymous unpak", class = "factor"), seeds.sown = c(2L, 
    2L, 2L, 2L, 2L, 2L, 2L, 2L), germinants.7d = c(2L, 2L, 2L, 
    2L, 1L, 2L, 2L, 2L), germinants.14d = c(1L, 2L, 2L, 2L, 1L, 
    2L, 2L, 2L), days.to.bolt = c(-1L, 0L, 0L, 0L, 0L, 0L, 0L, 
    0L), diameter.at.bolt = c(58.1, 58.3, 98.8, 81.2, 55.8, 59.1, 
    79.3, 73.3), mainbranch = c(3L, 2L, 3L, 3L, 2L, 3L, 2L, 2L
    ), ttl.maininfl.branch = c(3L, 8L, 6L, 6L, 2L, 8L, 3L, 4L
    ), basal.branch = c(0L, 4L, 0L, 0L, 1L, 2L, 5L, 1L), branch.basalbranch = c(0L, 
    8L, 0L, 0L, 1L, 2L, 7L, 1L), fruitnum = c(71L, 114L, 91L, 
    110L, 81L, 83L, 135L, 53L), aborted.fruits = c(7L, 98L, 20L, 
    20L, 13L, 67L, 49L, 27L), inflorescence.height = c(32.5, 
    36.5, 37.6, 39, 33.8, 33.1, 39, 27.1), basalfruit.length = c(12.9, 
    11.6, 15, 14.5, 14.2, 13.4, 13.2, 11), midfruit.length = c(9.8, 
    13.5, 14.3, 14.6, 13.8, 10.4, 10.9, 12.3), upperfruit.length = c(9.3, 
    10.4, 11.3, 10.1, 9.9, 8.8, 6.5, 8.9), treatment = structure(c(1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "control", class = "factor")), .Names = c("expt.type", 
"expt.id", "accession", "parent.id", "institution", "facility", 
"expt.plantnum", "flat", "row", "column", "date", "investigator", 
"seeds.sown", "germinants.7d", "germinants.14d", "days.to.bolt", 
"diameter.at.bolt", "mainbranch", "ttl.maininfl.branch", "basal.branch", 
"branch.basalbranch", "fruitnum", "aborted.fruits", "inflorescence.height", 
"basalfruit.length", "midfruit.length", "upperfruit.length", 
"treatment"), class = "data.frame", row.names = c(NA, -8L))
write.csv(file=file,sep=",",row.names=F,a)
                                        }
                                        )
 output$downloadDoc <- downloadHandler(
                                        filename = function() {
                                          paste("uploadDoc.pdf")
                                        },
                                        content = function(file) {
#                                            writeBin(readBin(con="upload-format/upload-format.pdf",what="raw"),con=file)
                                            file.copy("upload-format/upload-format.pdf",file,T)
                                        }
#                                        content = function(file) {
#                                            pdf(file=file)
#                                            hist(rnorm(100))
#                                            dev.off()
#                                        }
                                        )

    output$results <- renderTable({
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)

        res <- upload.exp(csvfile=inFile$datapath,commit=ifelse(input$commit=="yes",TRUE,FALSE),pedantic=ifelse(input$pedantic=="yes",TRUE,FALSE),
                          insert_accession=ifelse(input$insert_accession=='yes',TRUE,FALSE))
        ret <- NULL
        if (!is.null(res$messages)) {ret <- rbind(ret,cbind(rep("status update",length(res$messages)),res$messages))}
        if (!is.null(res$error)) {ret <- rbind(ret,cbind(rep("***ERROR***",length(res$error)),res$error))}

        ret <- data.frame(ret)
        names(ret) <- c("Feeback.Type","Messages")
        ret
   })
})
    
    
