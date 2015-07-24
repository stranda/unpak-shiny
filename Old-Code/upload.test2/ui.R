shinyUI(
    fluidPage(
        titlePanel("Test an input file for uploading into the unPAK db"),
        sidebarLayout(
            sidebarPanel(
                fileInput('file1', 'Choose file to upload',
                          accept = c(
                              'text/csv',
                              'text/comma-separated-values',
                              'text/tab-separated-values',
                              'text/plain',
                              '.csv',
                              '.tsv'
                          )
                          ),
                tags$hr(),
                
                p('If any row on the "Messages and errors" table',
                  'starts with ***ERROR***, something needs to ',
                  'be altered in your file.  The "Contents" table ',
                  'shows what the file you uploaded initially looks',
                  'like to the db'
                   ),
                
                
                
                radioButtons('commit',"Commit data? (If FALSE, then just check file against database)",c("No"="no")),
                radioButtons('pedantic',"Insist that all uploaded individuals have parents already in db (pedantic)",c("Yes"="yes","No"="no")),
                radioButtons('insert_accession',"Insist that all uploaded Accessions (SALK or other lines) already in db",c("Yes"="yes","No"="no"))
                
            ),
            mainPanel(
                downloadLink("downloadDoc","Download a pdf of file format documentation"),
                br(),
                downloadLink("downloadExample","Download an example of a file that passess checks"),
                br(),
                tabsetPanel(
                    tabPanel("Messages and errors",
                             tableOutput('results')
                             ),
                    tabPanel("Contents of uploaded file",
                             dataTableOutput('contents')
                             )
                )
            )
        ) 
    )
)

