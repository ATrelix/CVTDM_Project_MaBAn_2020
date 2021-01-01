library(shinyjs)
library(shinythemes)
library(shiny)

Q1 <- c("Yes", "No")

ui <- fluidPage(theme = shinytheme("flatly"),
                
                
                
                titlePanel("Weight Prediction"),
                
                mainPanel(
                    
                    
                    radioButtons("Q1", "Q1 - Do you smoke ?", Q1, selected = character(0)),
                    br(),
                    
                    span(textOutput("text"), style='color:red'),
                    actionButton(inputId = "submit", label = "Know your score !", class="btn btn-secondary", icon = icon("child"), width = NULL),
                )

)

server <- function(input, output) {
    
    observeEvent(input$submit, {
    output$text <- renderText({ 
        games1 <- switch(input$Q1,
                         "Yes" = "I am quite disappointed...",
                         "No" = "That's very good!",
                         )    
    
    
})
    }
)
}


shinyApp(ui, server)


