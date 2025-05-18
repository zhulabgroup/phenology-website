library(shiny)
shinyApp(ui = fluidPage("OK"), 
         server = function(input, output) {},
         options = list(
           host = "0.0.0.0",
           port = as.numeric(Sys.getenv("PORT", unset = 3838))
         )
         )