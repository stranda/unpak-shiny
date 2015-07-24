shinyUI(pageWithSidebar(
  headerPanel("HTML output"), 
  sidebarPanel(sliderInput("n", 
    "Number of on each side:", min = 0, max = 10, value = 5)), 
    mainPanel(
        htmlOutput("test_table")
        ,
        htmlOutput("test_table_url")
        ,
        tableOutput("test_table_url2")
        )
))
