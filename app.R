Test <- ToothGrowth

library(shiny)
library(ggplot2)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Tooth Growth Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderTextInput("dose",
                        "Dosage Size:",
                        choices = c(0.5, 1, 2)),
            
            selectInput("supp",
                        "Supplement Type:",
                        choices = c("All", levels(Test$supp)))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    myData = reactive({
        Set <- Test 
        
        if(input$supp != "All")
        {
            Set <- subset(Set, Set$supp == input$supp)
        }
        
        Set <- subset(Set, Set$dose == input$dose)
      
        return(Set)  
    })

    output$distPlot <- renderPlot({
       
        ggplot(data = myData(), aes(x = len))+
            geom_density(fill = "blue")+
            theme_classic()+
            scale_x_continuous(name="Tooth Length", limits=c(0, 35))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
