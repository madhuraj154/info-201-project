library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(tidyr)

source("data_wrangling.R")


# PAGE 1: INTRO PAGE
intro_pg <- tabPanel("Intro",
                     fluidPage(
                       titlePanel("Introduction"),
                       mainPanel(
                         #What is the analysis you are doing?
                         h4("Analysis"),
                         p("The primary story we are seeking to tell is the correlation between crime incidence rates (as seen through the incarcerated population) and 
      unemployment rates. The hypothesis is that states with high unemployment consequently have higher rates of crime, as poverty is one of the underlying 
      causes of criminality."),
                         p("There may, however, be states that deviate from this pattern with high incarceration and low unemployment rates or low incarceration and high 
      unemployment. To further examine these cases, we could turn to the social and economic conditions in those states that may explain the negative 
      correlation between crime and unemployment in outlier states."),
                         
                         #Why should someone care/why is this interesting?
                         h4("Why is this interesting?"),
                         p("This data analysis provides valuable information for individuals who are considering relocating to a different state or region. By comparing 
        the safety levels of various states and regions, it helps potential movers make informed decisions. Additionally, the analysis includes the 
        timeline of incarceration and unemployment numbers, which can give insights into the job market and overall economic outlook of a state."),
                         p("With this information, people can assess whether a state offers favorable job prospects and if it is becoming safer or more dangerous over time. 
        By considering the trends in unemployment and incarceration rates, individuals can evaluate the potential for job growth and assess the safety 
        conditions in a particular state or region."),
                         
                         #What data will you use to answer those questions?
                         h4("About the Data"),
                         p("The data used to answer these questions are crime and incarceration rates in each of the 50 states in the US from the years 2001 to 2016."),
                         p("The dataset we used to create this analysis consists of curated statistics on state crime and incarceration rates published by the Bureau of Justice 
      Statistics and the FBI Uniform Crime Program. The data provided also includes annual statistics on average unemployment rates in each state 
      published by the Bureau of Labor Statistics. This dataset brings together unemployment and incarceration to help research explore the relationship 
      between the two.")
                       ),
                       sidebarPanel(
                         img(src = "image1.png", height = 200, width = 200),
                         br(""),
                         img(src = "image2.png", height = 200, width = 200),
                         br(""),
                         img(src = "image3.png", height = 200, width = 200)
                       )
                     )
)

#PAGE 2: DRILL DOWN ANALYSIS
controls <- sidebarPanel(
  h2("Control Panel"),
  selectInput(
    inputId = "region",
    label = "Select a Region",
    choices = drill_data_df$Region
  )
)
plot_view <- tabPanel("States in the Chosen Region Data", plotOutput(outputId = "state_barchart"))
region_plot_view <- tabPanel("All Regions Data", plotOutput(outputId = "reg_barchart")) 

drill_down_pg <- tabPanel("Drill Down",
                          fluidPage(
                            titlePanel("States vs Unemployment and Crime"),
                            sidebarLayout(
                              controls,
                              
                              mainPanel(
                                tabsetPanel(
                                  plot_view,
                                  region_plot_view
                                )
                              )
                              
                            )
                          )
)
#PAGE 3: CHANGE OVER TIME
controlsforuser <- sidebarPanel(
  h2("Control Panel"),
  selectInput(
    inputId = "state",
    label = "Select a State",
    choices = df$State,
  )
)

change_over_time_pg <- tabPanel("Change over Time",
                                fluidPage(
                                  titlePanel("Unemployment and Crime vs Years for each State"),
                                  sidebarLayout(
                                    controlsforuser,
                                    mainPanel(
                                      plotOutput(outputId = "lineplot")
                                    )
                                  )
                                )
                                
)
#PAGE 4: OUTLIERS
outliers_pg <- tabPanel("Outliers",
                        fluidPage(
                          titlePanel("Unemployment and Crime in the US: 2001-2016"),
                          sidebarLayout(
                            sidebarPanel(
                              h3("Control Panel"),
                              p("This chart checks to see if there is any correlation between the average percent of labor force unemploymed and average incarcerated population in 
      each US state, sorted by their region."),
                              p("Choose any year from 2001-2016, as that is the range of time covered by the data. The purpose of this graph is 
        to identify states that deviate from the general relationship between unemployment and incarceration."),
                              sliderInput(
                                inputId = "Year",
                                label = "Filter by year",
                                min = 2001, 
                                max = 2016,
                                value = 2016
                              )
                            ),
                            mainPanel(
                              plotOutput("scatter_plot")
                              
                            )
                          )
                        )
)

#PAGE 5: CONCLUSIONS AND TAKEAWAYS
conclusion_pg <- tabPanel("Conclusion",
                          h1("Takeaways"),  
                          p("Further examination of the Outliers data analysis reveals that outlier states with disproportionately high prisoner counts are exclusively in the Southern
    and Western regions. Additionally, when looking at the changes in crime over time there seems to be an increase overall as the years progress to 2016. 
    It seems crime and unemployment has increased in frequency and aggressivity becuase of the increase in overall population from 2001.
    Lastly, when examining the crime and unemployment in the 4 regions there seems to be a substantial increase in crime and unemployment in the South. 
    However, when closely looking at the states California has the highest amount of unemployment than any other state and crime is similar to average of the South"),
                          h2("Data Quality"),
                          p("Our dataset was of reasonable quality, but is not expansive seeing as we only cover years within the range 2001 to 2016.
      Limitations of the sources we derived our data from include: the crime statistics consist of data voluntarily submitted by law enforcement agencies. 
      This implies the prisoner count is likely a larger number in reality. In terms of the unemployment statistics, the data was 
      also collected on a voluntary basis, consequently leaving out individuals who did not wish to reveal their status of employment 
      or they were unaware that such census data collection was taking place. In both cases of incarceration and unemployment, the data
      is likely an undercount of the actual number. When it comes to voluntary data there is a potential margin of error 
      in collection biases, underreporting, and calculation errors in census data. Additionally, the large scale nature of this
      dataset risks homogenizing individual experience with incarceration and unemployment. The sole use of quantitative data as 
      opposed to qualitative data may also risk desensitization of both issues."),
                          p("Created by Madhu Rajesh, Spandana Kannam, and Shanzay Shabi")
) 

#SHINY OUTLINE
#define UI
ui <- navbarPage("Final Project",
                 intro_pg,
                 drill_down_pg,
                 change_over_time_pg,
                 outliers_pg,
                 conclusion_pg
)

#define server
server <- function(output, input){
  #PAGE 2 CHARTS
  #regions barchart
  output$reg_barchart <- renderPlot({
    df_long <- regions_df %>%
      pivot_longer(cols = c(Avg.Unemployment, total_crime),
                   names_to = "Variable",
                   values_to = "Value")
    p <- ggplot(df_long, aes(x = Region, fill = Variable, y = Value)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Grouped Bar Chart",
           x = "Region",
           y = "Population",
           fill = "Variable")
    return(p)
  })
  #state barchart
  output$state_barchart <- renderPlot({
    state_df <- filter(drill_data_df, Region == input$region)
    df_long <- state_df %>%
      pivot_longer(cols = c(Avg.Unemployment, total_crime),
                   names_to = "Variable",
                   values_to = "Value")
    bar <- ggplot(df_long, aes(x = Value, fill = Variable, y = State)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Grouped Bar Chart",
           x = "Population",
           y = "State",
           fill = "Variable")
    return(bar)
  })
  
  #PAGE 3 GRAPH  
  #state line graph
  output$lineplot <- renderPlot({
    state <- input$state
    
    filtered_df <- df[df$State == state, ]
    
    ggplot(filtered_df, aes(x = Year)) +
      geom_line(aes(y = Avg.Unemployment, color = "Average Unemployment")) +
      geom_line(aes(y = total_crime, color = "Total Crime Count")) +
      labs(x = "Year", y = "Average count") +
      ggtitle(paste("Unemployment and Crime in", state)) +
      scale_color_manual(values = c("blue", "red"), 
                         labels = c("Total Crime Count", "Average Unemployment")) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  #PAGE 4 PLOT
  #outliers scatter plot
  output$scatter_plot <- renderPlot({
    filt_df <- filter(df, Year == input$Year)
    scatter <- ggplot(data = filt_df, aes(x = Avg.Unemployment, y = total_crime, color = Region)) +
      geom_point(aes(col=Region)) + 
      ggtitle("Avg Unemployment vs. Crime Count") +
      labs(y = "Crime Count", x = "Avg Unemployment", color = "Regions") 
    
    return(scatter)
  })
  
}

#run the app
shinyApp(ui, server)