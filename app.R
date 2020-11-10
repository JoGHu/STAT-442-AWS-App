Test <- ToothGrowth

library(shiny)
library(ggplot2)
library(shinyWidgets)

# Define UI for application that draws a density plot
ui <- fluidPage(

    # Application title
    titlePanel("Tooth Growth Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(    
            
            # Slider for dosage size
            sliderTextInput("dose",
                        "Dosage Size:",
                        choices = c(0.5, 1, 2)),
            
            # Drop down box for supplement type
            selectInput("supp",
                        "Supplement Type:",
                        choices = c("All", levels(Test$supp)))
            
        ),

        # Define the plot
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a density plot
server <- function(input, output) {
    
    # Reactive function to update the data based on the filters
    myData = reactive({
        Set <- Test 
        
        if(input$supp != "All")
        {
            Set <- subset(Set, Set$supp == input$supp)
        }
        
        Set <- subset(Set, Set$dose == input$dose)
      
        return(Set)  
    })

    # Generate the density plot
    output$distPlot <- renderPlot({
       
        ggplot(data = myData(), aes(x = len))+
            geom_density(fill = "blue")+
            theme_classic()+
            scale_x_continuous(name="Tooth Length", limits=c(0, 35))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
