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
            plotlyOutput("plt")
        )
    )
)

# Define server logic ----
server <- function(input, output) {
    
        output$plt <- renderPlotly(
            {
                
                if (input$tp == 1) {
                    
                    df1 <- df %>%
                        filter(Year==input$selYear) %>%
                        filter(ICD.Chapter==input$selChapter1)
                    fig <- plot_ly(
                        type='bar',
                        x=df1$Crude.Rate,
                        y=reorder(df1$State, df1$Crude.Rate),
                        width=500, 
                        height=840
                    )
                    fig %>% layout(
                        autosize=F, 
                        xaxis=list(title=paste0('Crude Mortality Rate\n', input$selChapter1)),
                        yaxis=list(title='State')
                    )
                    
                } else {
                    
                    df2 <- df %>%
                        filter(ICD.Chapter==input$selChapter2) %>%
                        filter(State==input$selState)
                    dfn <- df %>%
                        #filter(ICD.Chapter==input$selChapter2) %>%
                        filter(ICD.Chapter=='Diseases of the nervous system') %>%
                        group_by(Year) %>%
                        summarize(natl_avg=weighted.mean(Crude.Rate, Population)) %>%
                        ungroup()
                    dfnew <- cbind(df2, dfn)
                    fig <- plot_ly(
                        x=dfnew$Year,
                        y=dfnew$Crude.Rate,
                        type='scatter',
                        name=input$selState,
                        mode='lines+markers',
                        width=800, 
                        height=400
                    )
                    fig %>% 
                        add_trace(x=dfnew$Year, y=dfnew$natl_avg, name='Natl Weighted Avg', mode='lines+markers') %>%
                        layout(
                            autosize=F,
                            margin=list(t=80),
                            xaxis=list(title=''),
                            yaxis=list(title='Crude Mortality Rate'),
                            title=paste0("Crude Mortality Rate - ", input$selState, '\n', input$selChapter2)
                        )
                    
                }
                    
                    
            }
        )

}

# Run the app ----
shinyApp(ui=ui, server=server)
