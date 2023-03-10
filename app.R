#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#load data
library (tidyverse)
library(plotly)
library(ggplot2)

airbnbData <- read_delim ("seattle_01.csv")

NRows=nrow(airbnbData)
NCols=ncol(airbnbData)
data <- head(airbnbData, n = 500)
selectedData<-head(data,6)
udata<-unique(airbnbData$address)
roomType<-unique(airbnbData$room_type)
roomType_listings <- airbnbData %>%
  group_by(room_type,address) %>%
  summarize(listings_count = n())

cities = airbnbData %>%
  select(address) %>%
  unique()

listings_count <- airbnbData %>%
  group_by(address) %>%
  summarize(listings = n())


ui <- fluidPage(
  # Create a tabsetPanel with two tabs
  tabsetPanel(
    # About tab content
    tabPanel("About: Greater Seattle Airbnb Listings",
             sidebarLayout(
               sidebarPanel(
             tags$h1("This App uses", em("Seattle Airbnb Listings Data")),
             tags$h2("Airbnb Listings Data for Seattle and Surrounding Regions"),
             tags$p("This data set is from the site", strong("Kaggle"), "and displays the number of bedrooms, address, and reviews", em("for each of the listings")),
             tags$p("This Data set has", NRows, "rows & ", NCols, "columns"),
             tags$p("When traveling to any location, one of the main components to organize is where you will stay. Airbnb has become a popular choice for travelers when choosing a place to stay. Aspects such as price, area/location, and number of bedrooms is important when making a decision on which Airbnb to pick. This project provides information answering important questions on Greater Seattle Airbnbs so travelers can make informed decisions. From this project, they will be able to find Airbnbs in the Greater Seattle area that are affordable for them, which listings can accommodate them with the number of bedrooms, and what location is ideal for them.
"),
             tags$p(strong("Here is some sample data")),
             # Display data in a table
             tableOutput("about_table")
               ),
             mainPanel(
               img(src="airbnb.jpg", align="right", width = 400)
             )
             )
             
    ),
    # Plot tab content
    tabPanel("Listings Per City",
             sidebarLayout(
               sidebarPanel(
                 # Add radio buttons
                 radioButtons("color_input", "Select color:",
                              choices = c("Red", "Purple", "Blue", "Green"),
                              selected = "Red"),
                 checkboxGroupInput("Cities",
                                    "Select Cities",
                                    choices = unique(airbnbData$address),
                                    selected = unique(airbnbData$address),
                 )
               ),
               mainPanel(
                 plotOutput("Plot"),
                 htmlOutput("PlotSummaryText"),
                 tags$p("In this plot, users can alter the values shown on the graph by the cities that are displayed. This allows users to identify which airbnbs are suitable for their trip based on what tourist attractions/events/activites they plan to visit and where they are located. They can easily find the airbnb listings that are closest to where they will be spending time. From this graph we can conclude that it may be more suitable for tourists to stay in Seattle because most tourist attractions are there. Additionally it is easier to find an airbnb in Seattle comparatively to surrounding cities for cities.
")
               )
             )
    ),
    # Plot price and satisfaction for airbnb listings
    tabPanel("Airbnb Price and Satisfaction",
             
             
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("n","How many Airbnb listings? ",
                             min = 1,
                             max = nrow(airbnbData),
                             value = 200),
                 p("We have", nrow(airbnbData), "listings total. The number of past customer
                   reviews can tell you about the popularity of a particular listing.
                   More reviews may mean that a particular listing has experienced
                   hosts, and that customers are satisfied with their experience."),
                 p("There are ",textOutput(outputId = "result", inline=T),
                   "listings currently being filtered, and all N/A values have been
                   removed. The listings for your 
                   selected satisfaction levels are shown to the right."),
                 fluidRow(
                   column(6,
                          radioButtons("color","Choose color",
                                       choices = c("skyblue","lawngreen","orangered",
                                                            "purple","gold"))
                   ),
                   column(6,
                          uiOutput("checkboxSatisfaction")
                          
                          
                   )
                 )
               ),
               mainPanel(
                 plotOutput("plot"),
                 tags$p("In this plot, users can filter the listings by level of customer satisfaction (good reviews vs. bad reviews) and see what price those high or low-reviewed listings fall under. The number of past customer reviews can tell you about the popularity of a particular listing. More reviews may mean that a particular listing has experienced
hosts, and that customers are satisfied with their experience. In conclusion, this plot can tell you about the quality of a property at a particular price point.
")
                 
               ) )
             
             
    ),
    
    tabPanel("Housing by Family Size",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("m", "# of options you'd like to view at a time:",
                             min = 1,
                             max = 20,
                             value = 10),
             
             fluidRow(column(
               6,
               radioButtons("accommodates", "How many people: ",
                            choices = (unique(airbnbData$accommodates)))
             ))),
             mainPanel(
               plotlyOutput("plot2"),
               tags$p("In this plot, users may select how many people they are traveling with in order to find AirBnbs that can fit everyone in their party, and they can view which areas to search the most in according to how many AirBnb openings show up on the scatterplot. This can save time during their search for housing because users will know which cities to spend the mosting time AirBnb-searching in. 
")
             ))
    ),
    
    # Tables tab content
    tabPanel("Conclusion",
             
             sidebarLayout(sidebarPanel(
               selectInput("cityName", "City", choices = unique(airbnbData$address)),
               p(
                 "It's important to pick where you'll stay based on your unique
                   budget. With many wonderful cities in the Greater Seattle
                   area with Airbnb listings at various price points, several are
                   sure to meet your needs!"
               ),
               p(
                 "For the selected city, the average price",
                 textOutput(outputId = "aboveBelow", inline = T),
                 "the average room price of $125."
               ),
               fluidRow(column(
                 4,
                 radioButtons(
                   "minMax",
                   "Choose price type",
                   choices = c("minimum list price", "maximum list price")
                 )
               ))
             ),
             
             
             mainPanel(tableOutput("table"),
             tags$p("One notable insight discovered form this data is that while larger, more urban cities have overall more listings, and more reviews, and are probably closer to tourist destinations, they are also more expensive.
                    Having less affordable listings in popular areas of the region may prevent travelers of a lower budget from exploring famous parts of Greater Seattle.
"),
tags$p("Our data set is of a reasonable quality but is flawed as certain cities have multiple and potentially overlapping entries because they are put in slightly differently, such as “Seattle, Wa” versus “Seattle, Washington,” which makes it hard to draw unbiased conclusions as it is difficult to tell whether or not these entries refer to the same variables. By having duplicate listings of a particular city with a particular prevalence and affordability, this visualization might be harmful to travelers by misleading them into thinking that there is more availability than there is in the city that they think meets their needs accommodation-wise.
"),
tags$p("Some ideas on how to advance this project in the future is to find out which data values overlap and removing duplicates. This may require research into how the source was compiled, and figuring out which entries are unique and which hold the same data. We could also add in more dimensions like square footage and when the airbnb was built to help the customer make decisions about quality."))))
    
    
  )
)

server <- function(input, output) {
  output$result <- renderText({
    return(input$n)
  })
  # Define a reactive object to manipulate the data for the table
  about_data <- reactive({
    filtered_data <- selectedData
    filtered_data
  })
  
  # Define an output to display the table for about page
  output$about_table <- renderTable({
    about_data()
  })
  
  output$Plot <- renderPlot({
    fitlered_listing <- listings_count %>%
      filter(address %in% input$Cities)
    ggplot(fitlered_listing, aes(x = listings, y = address)) + #like groupby, but you don't need to use it twice
      geom_bar(stat = "identity", width = 0.6, fill = input$color_input) +
      labs(title = "Number of Airbnb Listings by City in Seattle Area",
           x = "Number of Listings",
           y = "Cities")
  })
  
  output$PlotSummaryText <- renderText({
    fitlered_listing <- listings_count %>%
      filter(address %in% input$Cities)
    HTML("Count of Selected Addresses : ", "<b>",length(input$Cities), "</b>", "Total Listings are ", "<b>",sum(fitlered_listing$listings), "</b>")
    
  })
  
  output$roomtype_listingdata <- renderTable({
    roomslection = input$radio_input;
    # Filter data based on radio button input
    filtered_data <- roomType_listings %>%
      filter (room_type == roomslection)
  })
  
  output$tableSummaryText <- renderText({
    roomslection = input$radio_input;
    filtered_data <- roomType_listings %>%
      filter (room_type == roomslection)
    HTML("For Room Type", "<b>",roomslection, "</b>", " Number of Listings : ", "<b>",(sum(filtered_data$listings_count)),"</b>")
  })
  
  sample <- reactive({
    
    
    s1 <- airbnbData %>% 
      filter(overall_satisfaction %in% input$satisfaction)
    if(nrow(s1)> input$n) 
      sample_n(s1, input$n)
    else
      s1
  })
  
  output$aboveBelow <- renderText({
    MP <- airbnbData %>% 
      group_by(address) %>% 
      summarise(meanPrice=mean(price)) %>% 
      
      
      filter(address==input$cityName) 
    
    
    if(MP$meanPrice>125){
      return(" IS GREATER THAN")}
    else if (MP$meanPrice<125){
      return("IS LESS THAN")}
    else{
      return("IS EQUAL TO")
    }
  })
  
  output$table <- renderTable({
    
    
    if(input$minMax =="minimum list price"){
      s3 <- airbnbData %>%
        filter(address==input$cityName) %>% 
        summarise(`minimum list price of the city:`= min(price, na.rm=TRUE))
    } else{
      s3 <- airbnbData %>%
        filter(address==input$cityName) %>% 
        summarise(`maximum list price of the city:`= max(price, na.rm=TRUE))
    }
    
    
    
    
    s3
  })
  output$checkboxSatisfaction <- renderUI({
    satisf <- airbnbData %>% 
      filter(!is.na(overall_satisfaction)) 
    checkboxGroupInput("satisfaction","Choose customer satisfaction level",
                       choices = unique(satisf$overall_satisfaction)
    )
  })
  
  output$plot <- renderPlot({
    p <- sample() %>%
      ggplot(aes (price, reviews))+
      geom_point(col=input$color)+
      labs(x = "Airbnb List Prices", y = "Number of Reviews", title= "The Number of Reviews vs. Airbnb List Prices")
    
    
    if(nrow(sample())==0){
      p <- p+
        labs(subtitle= "Please select a satisfaction level")
    }
    p
  })
  
  sample2 <- reactive({
    s1 <- airbnbData %>%
      filter(accommodates %in% input$accommodates)
    if (nrow(s1) > input$m)
      sample_n(s1, input$m)
    else
      s1
  })
  
  output$plot2 <- renderPlotly({
    plot_ly(data = sample2(),
            x = ~address, y = ~bedrooms, color = ~address, type = "scatter")
  })
  
  
}

shinyApp(ui, server)