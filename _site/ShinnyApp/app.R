library("shiny")
library("stringi")
library("ggplot2")
library("tidyverse")
library("tidyr")

options(shiny.maxRequestSize = 9*1024^2)

server <- function(input, output) {
    
    # the data read in by read.csv will now be accessible throughout the app
    # by calling this reactive, e.g. in_data().
    in_data.Beer <- reactive({
        shiny::validate(
            need(input$beers, "Select a file!")
        )
        
        read.csv(input$beers$datapath, header = input$header.beers)
    })
    
    in_data.Breweries <- reactive({
        shiny::validate(
            need(input$breweries, "Select a file!")
        )
        
        read.csv(input$breweries$datapath, header = input$header.breweries)
    })
    
    in_data.Merged <- reactive({
        beers <- in_data.Beer()
        breweries <- in_data.Breweries()
        
        if (is.null(beers) | is.null(breweries))
            return
        
        bdat = merge(beers, breweries, by.x = "Brewery_id", by.y = "Brew_ID")
        names(bdat)[names(bdat) == "Name.x"] <- "Drink_name"
        names(bdat)[names(bdat) == "Name.y"] <- "Brewery"
        bdat$State = as.factor(bdat$State)
        bdat
    })
    
    # an example, we're calling the reactive to access the data loaded
    output$beersFileInfo <- renderTable({
        if(input$show.data) {
            data.frame(in_data.Merged())
        }
    })
    
    # an example, we're calling the reactive to access the data loaded
    output$breweriesFileInfo <- renderTable({
        if(input$show.breweries.data) {
            data.frame(in_data.Breweries())
        }
    })
    
    output$beerHistogram.IBU <- renderPlot({
        beers <- in_data.Merged()
        
        if (is.null(beers))
            return(NULL)
        
        if(!is.na(input$stateFilter) && input$stateFilter != "") {
            print(input$stateFilter)
            beers <- beers %>% filter(stri_detect_fixed(State, toupper(input$stateFilter)))    
        }
        
        
        beersplot <- beers %>%  ggplot(aes(x=IBU, fill=State))
        if(input$plotSelector == "histogram"){
            return(beersplot + geom_histogram() + ggtitle("Histogram of IBU for craft Beers"))
        }
        
        if(input$plotSelector == "boxplot"){
            return(beersplot + geom_boxplot() + ggtitle("boxplot of IBU for craft Beers"))
        }
    })
    
    output$beerScatterPot <- renderPlot({
        
        if(!input$show.scatter)
            return(NULL)
        
        beers <- in_data.Merged()
        
        if (is.null(beers))
            return(NULL)
        
        if(!is.na(input$stateFilter) && input$stateFilter != "") {
            print(input$stateFilter)
            beers <- beers %>% filter(stri_detect_fixed(State, toupper(input$stateFilter)))    
        }
        
        beers %>% ggplot(aes(x=ABV, y=IBU)) + geom_point() + geom_smooth(method="lm") + ggtitle("IBU vs ABV")
    })
    
    output$beerPieChart <- renderPlot({
        
        if(!input$show.scatter)
            return(NULL)
        
        beers <- in_data.Merged()
        
        if (is.null(beers))
            return(NULL)
        
        if(!is.na(input$stateFilter) && input$stateFilter != "") {
            print(input$stateFilter)
            beers <- beers %>% filter(stri_detect_fixed(State, toupper(input$stateFilter)))    
        }
        
        brewNum = beers %>% 
            filter(!is.na(Brewery_id)) %>% 
            group_by(State) %>% summarize(unique_breweries=length(unique(Brewery_id))) 
        
        brewNum %>%  ggplot(aes(x=unique_breweries, y="",fill=State)) + geom_bar(stat = "identity", colour="black") + coord_polar("x", start=0) + ggtitle("Unique Breweries By state")
    })
    
    output$beerHistogram.ABV <- renderPlot({
        beers <- in_data.Merged()
        
        if (is.null(beers))
            return(NULL)
        
        if(!is.na(input$stateFilter) && input$stateFilter != "") {
            print(input$stateFilter)
            beers <- beers %>% filter(stri_detect_fixed(State, toupper(input$stateFilter)))    
        }
        
        beersplot <- beers %>%  ggplot(aes(x=ABV, fill=State))
        if(input$plotSelector == "histogram"){
            return(beersplot + geom_histogram() + ggtitle("Histogram of ABV for craft Beers"))  
        }
        
        if(input$plotSelector == "boxplot"){
            return(beersplot + geom_boxplot() + ggtitle("boxplot of ABV for craft Beers"))
        }
    })
}

ui <- fluidPage(
    tabPanel('File Inputs',
             sidebarLayout(
                 sidebarPanel(
                     fileInput('beers', 'Choose Beers CSV File', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                     checkboxInput('header.beers', 'Header in Beers', TRUE),
                     fileInput('breweries', 'Choose Breweries CSV File', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                     checkboxInput('header.breweries', 'Header in Breweries', TRUE),
                     checkboxInput('show.data', 'show beer data', FALSE),
                     checkboxInput('show.scatter', 'show scatter plot', TRUE),
                     selectInput("plotSelector", "plot:", choices=c("histogram", "boxplot")),
                     textInput("stateFilter", "Filter Plots By State Containing:")
                 ),
                 shiny::mainPanel(
                     plotOutput("beerHistogram.IBU"),
                     plotOutput("beerHistogram.ABV"),
                     plotOutput("beerScatterPot"),
                     plotOutput("beerPieChart"),
                     tableOutput("beersFileInfo")
                     
                 )
             )
    )
)



shinyApp(ui, server)