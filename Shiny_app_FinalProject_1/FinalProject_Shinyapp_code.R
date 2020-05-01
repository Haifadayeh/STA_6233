library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library(DT)
library(readr)
library(ggplot2)
library(gridExtra)
#library(hrbrthemes)


load(url("https://raw.githubusercontent.com/Haifadayeh/STA_6233/master/Shiny_app_FinalProject_1/Data_collected.RData"))

Data_collected$Date_fmt=as.Date(Data_collected$Date,format = "%m/%d/%y")
Data_collected$total_homes_sold_yoy_1=Data_collected$total_homes_sold_yoy*100

###### Define UI for application that plots features of fake data -----------########
ui <- fluidPage(
  plotOutput("plot", height = "90%", width="90%"),
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
       selectInput(inputId = "Reg", 
                  label = "Region:",
                  choices = c("Dallas", "San Antonio","Austin","El Paso","Houston"), 
                  selected = "San Antonio"),
       
       hr(), #Horizontal Line for visual separation
       
       radioButtons("Y2", "What to plot?",choices = c('COVID19 cases vs. Homes sold','Homes sold year over year')),
       
      # Select Colors
      selectInput(inputId = "color_p", 
                  label = "Choose Point Color:",
                  choices = c("Red", "Blue", "Black", "Green"), 
                  selected = "Black"),
      
      hr(), #Horizontal Line for visual separation
      
      
      # Set alpha level
      sliderInput(inputId = "alpha", 
                  label = "Point Transparency:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      hr() #Horizontal Line for visual separation
    
    ),
    # Output: Show scatterplot --------------------------------------
    mainPanel(
        HTML(
          paste(
            h3("COVID19 effect on home sales"),'<br/>',
            h4("This app shows how increasing COVID19 cases has affected home sale activity in 5 major Texas cities:
               San Antonio, Austin, Dallas, Houston, and El Paso"),'<br/>',
            h4("For each city, two plots are presented: the first one shows the home sale trend with the increasing 
               number of confirmed COVID19 cases, and the second one highlights the home sale year-over-year trend
               before and after the first reported case")
          )
        ),
      
      tabsetPanel(
        tabPanel("Trend plots", 
                 plotlyOutput(outputId = "scatterplot_1", height=400, width=600) #500 700
                 ),
        
        tabPanel("Data",  DT::dataTableOutput(outputId="datasheet")),
        tabPanel("About", 
                 textOutput("text1")
        )
      )
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  
  dat1 <- reactive({
    ds1 <- Data_collected[Data_collected$Region %in% input$Reg, ]
    return(ds1)
  })
  
  # Create scatterplot object the plotOutput function is expecting --
  
  
  output$scatterplot_1 <- renderPlotly({
   
      if(input$Y2 == "COVID19 cases vs. Homes sold") {
        ggplot(data = dat1(), aes_string(x = dat1()$Date_fmt)) +
      geom_line(aes(y=Cases, colour="COVID19 Cases"), stat = "identity", lwd=1.) +
      geom_line(aes(y=total_homes_sold,colour="Home_sold"),stat = "identity", lwd=1.)+
      geom_point(aes(y=total_homes_sold),colour=input$color_p, alpha=input$alpha,shape=21,stat = "identity",color="black",lwd=0.7)+
      geom_point(aes(y=Cases),colour=input$color_p, alpha=input$alpha,shape=21,stat = "identity",color="purple",lwd=0.7)+
      scale_y_continuous(name ="Counts", sec.axis = dup_axis()) +ggtitle(" COVID19 cases vs. Homes sold")+xlab("Date")+
      theme(
        axis.title.y = element_text(color = "red"),
        axis.title.y.right = element_text(color = "blue")) +
      scale_x_date(date_breaks = "1 month", 
                   labels=date_format("%b-%Y"),
                   limits = as.Date(c('2020-01-07','2020-04-04'))) 
      }
    else 
    {  ggplot(data = dat1(), aes_string(x = dat1()$Date_fmt))+
        geom_line(aes(y=total_homes_sold_yoy_1,colour="Home_sold_yoy"),stat = "identity", lwd=1.)+
        geom_point(aes(y=total_homes_sold_yoy_1),colour=input$color_p, alpha=input$alpha,shape=21,stat = "identity",color="black",lwd=0.7)+
        geom_vline(xintercept=as.numeric(as.Date("2020-03-19")),linetype="dashed",color = "blue", size=1)+
        geom_text(aes(x=as.Date("2020-03-19"), label="First case reported", y=25),colour="blue",hjust = 1,text=element_text(size=6))+ 
        scale_y_continuous(name ="Counts", sec.axis = dup_axis()) +ggtitle(" Homes sold year over year")+xlab("Date")+
        theme(
          axis.title.y = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue")) +
        scale_x_date(date_breaks = "1 month", 
                     labels=date_format("%b-%Y"),
                     limits = as.Date(c('2020-01-07','2020-04-04'))) 
    }

  })
  
  output$datasheet<-DT::renderDataTable({
    DT::datatable(data=Data_collected[,1:6],
                  options=list(pageLength= 20),
                  rownames=FALSE)
  })
  output$text1<- renderText({ 
paste("The final dataset visualized here is a combination of home-sale moving weekly data in the cities of 
     interest and the confirmed COVID-19 cases in the containing counties.
    Home sale data source (https://www.redfin.com/blog/data-center/). COVID19 cases data source (https://dshs.texas.gov/coronavirus/additionaldata/)")
    
  }) 
  
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)

