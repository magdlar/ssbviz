library("shiny")
library("ggplot2")
library("dplyr")
library("mapproj")

map_df <- readRDS(file="./data/mapdf.RDA")

mapTheme <- theme(legend.position = c(0.7,.3),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  panel.grid = element_blank(),
                  panel.background = element_blank())

## Define the UI
ui <- fluidPage(
  
  ## Add a title and a horizontal line
  titlePanel("Visualizing county level SSB data"),
  hr(),

  ## Create a three column layout
  fluidRow(
    column(3,
           wellPanel(
             helpText("Change the map by selecting a variable and year of interest."),
             uiOutput("var"),
             uiOutput("year"),
             actionButton("dec", tags$b("<")),
             actionButton("inc", tags$b(">")),
             hr(),
             helpText("You can also select a county to see more details."),
             uiOutput("fylke")
           )),
    
    column(9,
           column(6, plotOutput("map", click="mapClick")),
           column(6, plotOutput("details", click="plotClick"))
           )
    ),
  
  ## This prevents the plot elements from fading out when they are being updated
  tags$style(type="text/css",
             ".recalculating { opacity: 1.0; }"
  )
)


## Define server logic required to draw the map
server <- function(input, output, session) {
  
  ## Tracks which years are available
  availYears <- reactive({
    #print("availYears")
    map_df[which(!is.na(map_df[,which(names(map_df)==input$var)])),"Year"] %>% unlist() %>% as.numeric() %>% unique()      
  })
  
  ## Subsets the DF to only contain the years available for my curent variable
  currDf <- reactive({
    #print("currDf")
    map_df[which(map_df$Year %in% availYears()),]
  })
  
  ## Finds all the values for the current variable
  currVarVals <- reactive({
    #print("currVarVals")
    vals <- currDf()[,which(names(currDf())==input$var)] %>% unlist()
  })
  
  
  ## finds the min and max values of the current variable
  currVals <- reactive({
    #print("currVals")
    minVal <- min(currVarVals()*100) %>% floor()/100
    maxVal <- max(currVarVals()*100) %>% ceiling()/100
    retVals <- list(minVal,maxVal)
    names(retVals) <- c("min","max")
    return(retVals)
  }) 
  
  
  ## Find the specific value when a year and a county is selected
  currVal <- reactive({
    #print("currVal")
    nowVal <- currVarVals()[which(currDf()$Year==input$year & currDf()$Region==input$fylke)][1] %>% as.numeric()
    return(nowVal)
  })
  
  ## Finds the actual variables and creats the UI element to pick one
  output$var <- renderUI({
    selectInput("var",
                label="Pick a variable",
                choices=names(map_df)[-c(1:9)])
  })
  
  ## Find the available years and create the UI element to pick one
  output$year <- renderUI({
    selectInput("year",
                label="Pick a year",
                choices=availYears(),
                ## Make sure the year stays the same if the variable changes and the year is available for the new variable
                selected = ifelse(input$year %in% availYears(),input$year,min(availYears())))
  })
  
  ## Find the available regions and create the UI element to pick one
  output$fylke <- renderUI({
    selectInput("fylke",
                label="Pick a county",
                choices=c("None",sort(unique(map_df$Region))),
                selected="None")
  })
  
  ## Observe clicks to the decrease year button
  observeEvent(input$inc, {
    if((as.numeric(input$year)+1) %in% as.numeric(availYears())){
      updateSelectInput(session, "year", "selected"=as.character(as.numeric(input$year)+1))
    }
  })
  
  ## Observe clicks to the increase year button
  observeEvent(input$dec, {
    if((as.numeric(input$year)-1) %in% as.numeric(availYears())){
      updateSelectInput(session, "year", "selected"=as.character(as.numeric(input$year)-1))
    }
  })
  
  output$map <- renderPlot({
    
    req(input$var,input$year %in% availYears())
    
    ## Somehwat dumb way to default to a blue colour and pick red for absenteeism
    ## Should be changed for improved generalization
    if(input$var=="Absenteeism"){
      colHigh <- "#431313"
      colLow <- "#f75656"
    }else{
      colHigh <- "#132B43"
      colLow <- "#56B1F7"
    }
    
    ## Reduce dataframe to the relevant scope
    currData <- map_df[which(map_df$Year==input$year),]
    
    ## Plot map using ggplot
    ggplot() +
      geom_polygon(data=currData, aes(x=long,y=lat,group=group, fill=!!sym(input$var)), color="black", size=0.5) +
      scale_fill_gradient(limits=c(currVals()$min,currVals()$max), 
                          breaks=seq(currVals()$min+0.01,currVals()$max-0.01,0.02), high=colHigh, low=colLow, 
                          labels=scales::percent_format(accuracy=1)) +
      geom_polygon(data=currData[which(currData$Region==input$fylke),], 
                   aes(x=long,y=lat,group=group), color="black", size=1.5, fill=NA) +
      geom_polygon(data=currData[which(currData$Region==input$fylke),], 
                   aes(x=long,y=lat,group=group), color="white", size=1, fill=NA) +
      mapTheme +
      coord_cartesian(ylim=c(58.5,70.6),xlim = c(5.9,30))

  }, height = function() {session$clientData$output_map_width}) ## This function makes sure the height scales with the width
  
  output$details <- renderPlot({
    
    req(input$year %in% availYears(),input$var,input$fylke!="None")

    ## Reduce the dataframe to the relevant scope
    detData <- unique(map_df[which(map_df$Region==input$fylke & map_df$Year %in% availYears()),-c(1:7)])

    ## Then I plot the graph
    ggplot() +
      geom_line(data=detData, 
                aes(x=Year, y=!!sym(input$var))) +
      scale_y_continuous(labels=scales::percent_format(accuracy=0.1), limits = c(0,currVals()$max+0.01)) +
      scale_x_continuous(breaks=availYears()[seq(1,max(availYears()),by=2)]) +
      annotate("point", x=as.numeric(input$year), y=currVal(), size = 2) +
      theme(panel.grid.minor.x = element_blank(),
            axis.line = element_line(colour = "black",size=1,lineend = "square"),
            plot.title = element_text()) +
      ggtitle(paste0(input$var," in ",input$fylke)) +
      geom_label(aes(x = max(availYears())-2, y = .005), 
                 label=paste0(" ", round(currVal()*100, digits = 1),"%"), fill="white", size=5, label.size= NA)

  }, height = function() {session$clientData$output_details_width}) ## This function makes sure the height scales with the width
  
  observeEvent(input$mapClick, {
    
    ## Most of the following was taken from http://playshiny.com/2016/10/17/shiny-click-on-a-map/
    mapData <- currDf()
    mapData$long_diff <- mapData$long - input$mapClick$x
    mapData$lat_diff <- mapData$lat - input$mapClick$y
    
    ## calculate the angle between the vector from this clicked point to border and c(1, 0)
    vLong <- mapData$long_diff
    vLat <- mapData$lat_diff
    mapData$angle <- acos(vLong / sqrt(vLong^2 + vLat^2))
    
    ## calculate range of the angle and select the state with largest range
    rangeAngle <- tapply(mapData$angle, mapData$Region, function(x) max(x) - min(x))

    ## I had trouble selecting Oslo, so I made it more likely simply by multiplying the value here
    rangeAngle[[which(names(rangeAngle)=="Oslo")]] <- rangeAngle[[which(names(rangeAngle)=="Oslo")]]*1.2
    
    selCounty <- names(sort(rangeAngle, decreasing = TRUE))[1]
    
    ## Look for clicks far away from any region, and if so set value to "None"
    if(rangeAngle[[which(names(rangeAngle)==selCounty)]]<2){
      updateSelectInput(session, "fylke", "selected"="None")
    }else{
      updateSelectInput(session, "fylke", "selected"=as.character(selCounty))
    }
  })
  
  observeEvent(input$plotClick, {
    clickedYear <- round(input$plotClick$x, digits = 0)
    
    if(clickedYear %in% availYears()){
      updateSelectInput(session, "year", "selected"=clickedYear)
    }
  })
}

## Run the application 
shinyApp(ui = ui, server = server)