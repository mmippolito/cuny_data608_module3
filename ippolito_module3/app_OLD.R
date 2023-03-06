library(tidyverse)
library(shiny)
library(plotly)

# Define UI ----
ui <- fluidPage(
    titlePanel("ICD Crude Mortality Rates"),
    
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                id='tp',
                type='tabs',
                tabPanel('Question 1', value=1,
                    selectInput("selYear", h4("Year"), choices = unique(df$Year), selected = 2010),
                    selectInput("selChapter1", h3("ICD Chapter"), choices = levels(df$ICD.Chapter), selected = 1)
                ),
                tabPanel('Question 2', value=2,
                    selectInput("selState", h4("State"), choices = unique(df$State), selected = 'AL'),
                    selectInput("selChapter2", h3("ICD Chapter"), choices = levels(df$ICD.Chapter), selected = 1)
                )
            )
        ),
        mainPanel(
            plotOutput("plt")
        )
    )
)

# Define server logic ----
server <- function(input, output) {
    
        output$plt <- renderPlot(
            {
                
                # Old way with ggplot
                if (1 == 0) {
                    
                    if (input$tp == 1) {
                        
                        # Question 1
                        df %>%
                            filter(Year==input$selYear) %>%
                            filter(ICD.Chapter==input$selChapter1) %>%
                            ggplot(aes(x=reorder(State, Crude.Rate), y=Crude.Rate)) +
                            geom_bar(stat='identity') + 
                            coord_flip() +
                            theme_minimal() +
                            xlab("State") +
                            ylab("Crude Mortality Rate %") +
                            ggtitle("Crude Mortality Rate", subtitle = input$selChapter1)
                        
                    } else {
                        
                        # Question 2
                        df %>%
                            filter(State==input$selState) %>%
                            filter(ICD.Chapter==input$selChapter2) %>%
                            ggplot(aes(x=Year, y=Crude.Rate)) +
                            geom_point() +
                            geom_line() +
                            theme_minimal() +
                            xlab("") +
                            ylab("Crude Mortality Rate %") +
                            ggtitle(paste0("Crude Mortality Rate - ", input$selState), subtitle = input$selChapter2)
                    }
                }
                
                # New way with plotly
                else if (1 == 1) {
                    
                    if (input$tp == 1) {
                        
                        plot_ly(
                            type='bar',
                            x=df$State,
                            y=df$Crude.Rate
                        )
                        #fig
                        
                    } else {
                        
                    }
                    
                    
                }
            },
            width=600, height=600
        )

}

# Run the app ----
shinyApp(ui=ui, server=server)
