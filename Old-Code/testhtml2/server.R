
shinyServer(function(input, output) {
    output$test_table <- renderText( {

        str <- "<table border=1>\n"
        for (i in 1:input$n) {
          str <- paste(str, "  <tr>\n", sep = "")
          str <- paste(str, "    <td>",i,"</td>", sep = "")
        }
        str
    })

output$test_table_url <- renderText( {
    str <- "<table border = 1>"
    for (i in 1:input$n) {
      str <- paste0(str, "<tr>")
      str <- paste0(str, "<td>", "<a href='http://google.com'>google</a>", "</td></tr>")
    }
    str
  })

    
})
