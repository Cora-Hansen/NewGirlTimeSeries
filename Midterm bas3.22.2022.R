library(shiny)
library(fpp3)
library(plotly)
library(ggeasy)
library(ggpubr)
library(shinyWidgets)
library(shinydashboard)
library(quantmod)
library(plotly)
library(DT)
library(shinythemes)
library(forecast)
library(rsconnect)

rsconnect::setAccountInfo(name='corahansen', token='1891466137B763D31A82BE5C9CC7BAF2', secret='kwdxOCRQeWYQmjqOF7o/s1yJIJUbGN12GvD21ZQC')

file_path <- "NewGirl.csv"
g_trends <- read.csv(file_path, skip = 2)
names(g_trends) <- c("Month", "Interest")
g_trends$Month <- yearmonth(g_trends$Month)
g_trends$Interest <- as.numeric(ifelse(g_trends$Interest == "<1",
                                       0,
                                       g_trends$Interest))
g_trends <- tsibble(g_trends)
g_trends %>% filter(year(Month) >= 2011) -> g_trends_1
g_trends_1$Month <- as.Date(g_trends_1$Month)

cols <- c("Season 1 Start",
          "Season 2 Start",
          "Season 3 Start",
          "Season 4 Start",
          "Season 5 Start",
          "Season 6 Start",
          "Season 7 Start")

cols_1 <- c("Season 1 End",
            "Season 2 End",
            "Season 3 End",
            "Season 4 End",
            "Season 5 End",
            "Season 6 End",
            "Season 7 End")

file_path2 <- "GameofThrones.csv"
g_trends2 <- read.csv(file_path2, skip = 2)
names(g_trends2) <- c("Month", "Interest_GOT")
g_trends2$Month <- yearmonth(g_trends2$Month)
g_trends2$Interest_GOT <- as.numeric(ifelse(g_trends2$Interest_GOT == "<1",
                                            0,
                                            g_trends2$Interest_GOT))
g_trends2 <- tsibble(g_trends2)
g_trends2 %>% filter(year(Month) >= 2011) -> g_trends2_1

cols2 <- c("Season 1",
           "Season 2",
           "Season 3",
           "Season 4",
           "Season 5",
           "Season 6",
           "Season 7",
           "Season 8")

g_trends_1 %>% 
  mutate(Month = yearmonth(Month)) %>% 
  as_tsibble(index = Month) -> NGTS

train <- NGTS %>% 
  filter(Month <= yearmonth("2021 March"))

test <- NGTS %>% 
  filter(Month > yearmonth("2021 March"))

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "New Girl Time Series Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions", icon = icon("fish"), tabName = "instructions"),
      menuItem("Full Time Series Plot", icon = icon("pepper-hot"), tabName = "tsplot"),
      menuItem("Plots", icon = icon("bug"), tabName = "scdplots"),
      menuItem("Comparison to GOT", icon = icon("dragon"), tabName = "got",
               menuSubItem("Separate Plots", tabName = "separate", icon = icon("smog")),
               menuSubItem("Combined Plot", tabName = "combined", icon = icon("hat-wizard")),
               menuSubItem("Residuals", tabName = "resid", icon = icon("hand-sparkles"))),
      menuItem("Simple Models", icon = icon("disease"), tabName = "sm",
               menuSubItem("Naive Model", tabName = "nm", icon = icon("umbrella-beach")),
               menuSubItem("Seasonal Naive Model", tabName = "snm", icon = icon("cloud-sun-rain")),
               menuSubItem("Mean Model", tabName = "mm", icon = icon("sun")),
               menuSubItem("Drift Model", tabName = "dm", icon = icon("water"))),
      menuItem("Exponential Smoothing Models", icon = icon("plane"), tabName = "esm",
               menuSubItem("Holt's Model", tabName = "hm", icon = icon("bus")),
               menuSubItem("Holt-Winter's Model", tabName = "hwm", icon = icon("caravan"))),
      menuItem("ARIMA Models", icon = icon("dollar-sign"), tabName = "arima",
               menuSubItem("Auto ARIMA", tabName = "auto", icon = icon("coins")),
               menuSubItem("Maunally Selected ARIMA", tabName = "manual", icon = icon("money-bill"))),
      menuItem("Best Prediction Model!", icon = icon("gift"), tabName = "bpm")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "instructions",
              fluidPage(
                box(
                  title = "Instructions for Navigating New Girl App!",
                  width = 12,
                  solidHeader = TRUE,
                  background = "light-blue",
                  status = "primary",
                  textOutput("ins")
                ),
                
                tags$img(
                  src = "https://wallpapercave.com/wp/wp2153921.jpg",
                  align = 'center',
                  style="display: block; margin-left: auto; margin-right: auto;",
                  width = "355",
                  height = "250"
                ),
                
                box(
                  title = "Information for Full Time Series Plot!",
                  width = 12,
                  solidHeader = TRUE,
                  background = "green",
                  status = "primary",
                  textOutput("ins1")
                ),
                
                box(
                  title = "Information for Plots!",
                  width = 12,
                  solidHeader = TRUE,
                  background = "purple",
                  status = "primary",
                  textOutput("ins2")
                ),
                
                box(
                  title = "Information for Comparison to GOT!",
                  width = 12,
                  solidHeader = TRUE,
                  background = "maroon",
                  status = "primary",
                  textOutput("ins3"), 
                ),
                
                box(
                  title = "Information for Simple Models!",
                  width = 12,
                  solidHeader = TRUE,
                  background = "light-blue",
                  status = "primary",
                  textOutput("ins4"), 
                ),
                
                box(
                  title = "Information for Exponential Smoothing Models!",
                  width = 12,
                  solidHeader = TRUE,
                  background = "green",
                  status = "primary",
                  textOutput("ins5"), 
                ),
                
                box(
                  title = "Information for ARIMA Models!",
                  width = 12,
                  solidHeader = TRUE,
                  background = "purple",
                  status = "primary",
                  textOutput("ins6"), 
                ),
                
                box(
                  title = "Information for Best Model!",
                  width = 12,
                  solidHeader = TRUE,
                  background = "maroon",
                  status = "primary",
                  textOutput("ins7"), 
                )
              )
      ),
      
      tabItem(tabName = "tsplot",
              fluidRow(
                box(
                  title = "New Girl Time Series Data",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("NG_Ori", height = 250)),
                
                box(
                  title = "New Girl Season Duration",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotlyOutput("NG_Box", height = 250)),
                
                box(
                  title = "Interpretation of these Plots",
                  width = 12,
                  solidHeader = TRUE,
                  background = "light-blue",
                  status = "primary",
                  textOutput("timeseriestab")
                )
              )),
      
      tabItem(tabName = "scdplots",
              fluidRow(
                box(
                  title = "Seasonality, Autocorrelation, and Decomposition of Frequency of New Girl Searches", 
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("myplot", height = 350)),
                  radioButtons(inputId = "plot_type",
                               label = "Please select which plot you would like to see!",
                               choices = c("Seasonality", "Autocorrelation", "Decomposition" )),
                  
                  conditionalPanel(condition = "input.plot_type == 'Autocorrelation'",
                                   materialSwitch(
                                     inputId = "diff",
                                     label = "Difference? (You must unselect Difference in order to see other plots!)",
                                     status="primary",
                                     right = FALSE)),

                box(
                  title = "Interpretation of Selected Plot", 
                  width = 12,
                  background = "olive",
                  solidHeader = TRUE,
                  status = "primary",
                textOutput("plotstab")
              
              ))),
      
      tabItem(tabName = "separate",
              fluidRow(
                box(
                  title = "New Girl Time Series Data",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("NG", height = 250)),
                
                box(
                  title = "Game of Thrones Time Series Data",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("GOT", height = 250)),
                
                box(
                  title = "Interpretation of these Plots",
                  width = 12,
                  solidHeader = TRUE,
                  background = "purple",
                  status = "primary",
                  textOutput("separatetab")
                ))),
      
      
      tabItem(tabName = "combined",
              fluidRow(
                box(
                  title = "Direct Comparison of New Girl and Game of Thrones Search Frequency",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotlyOutput("Co", height = 350)),
                
                box(
                  title = "Interpretation of this Combined Plot",
                  width = 12,
                  solidHeader = TRUE,
                  background = "green",
                  status = "primary",
                  textOutput("combinedtab")
                )
              )
      ),
      
      tabItem(tabName = "resid",
              fluidRow(
                box(
                  title = "Residuals of New Girl Time Series",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("NGR", height = 250)),
                
                box(
                  title = "TSLM of New Girl Time Series",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("NGR2", height = 250)
                ),
                
                box(
                  title = "Residuals of Game of Thrones Time Series",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("GOTR", height = 250)
                ),
                
                box(
                  title = "TSLM of Game of Thrones Time Series",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("GOTR2", height = 250)
                ),
                
                box(
                  title = "Residuals Interpretation",
                  width = 12,
                  solidHeader = TRUE,
                  background = "light-blue",
                  status = "primary",
                  textOutput("restext"),
                  textOutput("restext3"),
                  textOutput("restext2")
                )
              )),
      
      tabItem(tabName = "nm",
              fluidRow(
                box(
                  title = "Naive Model Predictions of New Girl Time Series",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("naive", height = 450)),
                
                box(
                  title = "Interpretation of Naive Model",
                  width = 12,
                  solidHeader = TRUE,
                  background = "green",
                  status = "primary",
                  textOutput("nmw")
                )
              )),
      
      tabItem(tabName = "snm",
              fluidRow(
                box(
                  title = "Seasonal Naive Model Predictions of New Girl Time Series",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("seasonal", height = 450)),
                
                box(
                  title = "Interpretation of Seasonal Naive Model",
                  width = 12,
                  solidHeader = TRUE,
                  background = "purple",
                  status = "primary",
                  textOutput("snmw")
                )
              )),
      
      tabItem(tabName = "mm",
              fluidRow(
                box(
                  title = "Mean Model Predictions of New Girl Time Series",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("mean", height = 450)),
                
                box(
                  title = "Interpretation of Mean Model",
                  width = 12,
                  solidHeader = TRUE,
                  background = "maroon",
                  status = "primary",
                  textOutput("mmw")
                )
              )),
      
      tabItem(tabName = "dm",
              fluidRow(
                box(
                  title = "Drift Model Predictions of New Girl Time Series",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("drift", height = 450)),
                
                box(
                  title = "Interpretation of Drift Model",
                  width = 12,
                  solidHeader = TRUE,
                  background = "light-blue",
                  status = "primary",
                  textOutput("dmw")
                )
              )),
      
      tabItem(tabName = "hm",
              fluidRow(
                box(
                  title = "Holt's Model Predictions of New Girl Time Series",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("holt", height = 450)),
                
                box(
                  title = "Interpretation of Holt's Model",
                  width = 12,
                  solidHeader = TRUE,
                  background = "green",
                  status = "primary",
                  textOutput("hmw")
                )
              )),
      
      tabItem(tabName = "hwm",
              fluidRow(
                box(
                  title = "Holt-Winter's Model Predictions of New Girl Time Series",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("holtwin", height = 450)),
                
                box(
                  title = "Interpretation of Holt-Winter's Model",
                  width = 12,
                  solidHeader = TRUE,
                  background = "purple",
                  status = "primary",
                  textOutput("hwmw")
                )
              )),
      
      tabItem(tabName = "auto",
              fluidRow(
                tags$img(
                  src = "realautoarima.jpg",
                  align = 'center',
                  style="display: block; margin-left: auto; margin-right: auto;",
                  width = "750",
                  height = "450"
                ),
                
                box(
                  title = "Interpretation of Auto ARIMA Model",
                  width = 12,
                  solidHeader = TRUE,
                  background = "maroon",
                  status = "primary",
                  textOutput("aaw")
                )
              )),
      
      tabItem(tabName = "manual",
              fluidRow(
                box(
                  title = "(0,1,1)(0,1,1) ARIMA Predictions of New Girl Time Series",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("manar", height = 450)),
                
                box(
                  title = "Interpretation of Manually Selected ARIMA Model",
                  width = 12,
                  solidHeader = TRUE,
                  background = "light-blue",
                  status = "primary",
                  textOutput("maw")
                )
              )),
      
      tabItem(tabName = "bpm",
              fluidRow(
                box(
                  title = "The Best Model for Predicting New Girl Interest- THE MEAN MODEL!!!",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("bmp", height = 450)),
                
                box(
                  title = "Why is it the best model?",
                  width = 12,
                  solidHeader = TRUE,
                  background = "green",
                  status = "primary",
                  textOutput("bestw"),
                  textOutput("bestw2"),
                  textOutput("bestw3")
                ),
                
                tags$img(
                  src = "stretch.jpg",
                  align = 'center',
                  style="display: block; margin-left: auto; margin-right: auto;",
                  width = "550",
                  height = "250"
                )
              ))
      
      )))

server <- function(input, output, session) {
  
  output$timeseriestab <- renderText({
    "With these two plots, we can see the full time series of New Girl search frequency or interest from when it aired to present day. It appears that this data could have some seasonality, but if we look at the New Girl season duration plot, we can see that it is seasonality based on new seasons airing. I would say that based on these graphs, this data does not follow an intense cyclical pattern or trend. Additionally, there is a consistent drop in the middle of the seasons followed by a rise (big or small) towards the end of the season. Also, we can see that there is decreasing interest in the show as the seasons progress, but a drastically lower interest in the even seasons (2,4,6) and a higher interest in the odd seasons (1,3,5,7). Additionally, we can see a rise in interest before the new seasons premiered as anticipation for the show was growing. Finally, I think it is interesting to see the rise in interest in early 2020 which was when COVID-19 and quarantine began. "
  })
  
  output$separatetab <- renderText({
    "These two plots show the interest in New Girl and Game of Thrones from the time they aired to present day. As stated in the Instructions tab, New Girl and Game of Thrones are both popular shows that began in the same year and finished around the same time. I thought it would be interesting to see a comparison in the interest of these two shows over time. First, we can see that New Girl and Game of Thrones have opposite trends when it comes to interest. We have seen that New Girl had decreasing interest as the show progressed, yet Game of Thrones had drastically increasing interest as the show progressed. Another difference is that although Game of Thrones has more dramatic peaks as the seasons come out, their “off-season” interest is consistently very low around 15. Whereas New Girl has a lower interest as seasons come out, their “off-season” interest is consistently higher around 25. The last obvious difference is that after Game of Thrones finished creating new seasons, their interest dropped and has not since risen. On the other hand, after New Girl finished creating seasons, there was another rise in interest beginning in 2020 and rising through 2021. Although we can see a lot of differences, there are also a lot of similarities. Obviously, both shows had peaks in interest as they were releasing seasons and drops in interest after the season that was airing ended. I think it is also interesting to look at how there was a rise in interest in both shows before the new seasons aired as there was an anticipation growing for the show and people were preparing to watch the new seasons. Finally, both New Girl and Game of Thrones have some seasons where there is a drop in interest in the middle of the season most likely while viewers were waiting for new episodes to air. "
  })
  
  output$combinedtab <- renderText({
    "This is a combined plot of New Girl and Game of Thrones interest from when they first aired through today. We can see all the same patterns as we did in the Separate Plots tab, but I think this graphic really shows the consistently higher New Girl interest in comparison to Game of Thrones.  "
  })
  
  output$ins <- renderText({
    " This is an app looking at the Google Trends of the TV show New Girl! Within this app there are 7 different tabs which are, Full Time Series Plot, Plots, Comparison to GOT, Simple Models, Exponential Smoothing Models, ARIMA Models, and Best Prediction Model! In the Full Time Series Plot tab, you will be able to see the full data of New Girl interest from when the show first aired to present day. Additionally, you will see a plot that shows the same data but with the season duration dates shown on the graph. "
  })
  
  output$ins1 <- renderText({
    "In the Full Time Series Plot tab, you will be able to see the full data of New Girl interest from when the show first aired to present day. Additionally, you will see a plot that shows the same data but with the season duration dates shown on the graph."
  })
  
  output$ins2 <- renderText({
    "In the Plots tab, there are 3 different plots available for viewing, seasonality, autocorrelation, and decomposition. You must select from the buttons which graph you would like to see! Additionally, if you choose the autocorrelation plot, there is an option to view the difference of the autocorrelation plot!"
  })
  
  output$ins3 <- renderText({
    "Finally, in the Comparison to GOT tab, there are three sub-tabs called Separate Plots, Combined Plots, and Residuals. The point of the Comparison to GOT tab is to compare the interest or search frequency of the TV show Game of Thrones compared to New Girl. These are two shows that started in the same year and finished within one year of each other. In the Separate Plots sub-tab, you can see the time series plot of both New Girl and Game of Thrones. Although, in the Combined Plot sub-tab, you can see a plot of both New Girl and Game of Thrones in the same graphic! Finally, in the Residuals sub-tab you can see a comparison of the residuals for New Girl and Game of Thrones."
  })
  
  output$ins4 <- renderText({
    " In the Simple Models tab, there are 4 separate sub-tabs called Naïve Model, Seasonal Naïve Model, Mean Model, and Drift Model. In each one of these you will see a 12-month prediction in New Girl interest based on the 4 models listed above. "
  })
  
  output$ins5 <- renderText({
    " Included in the Exponential Smoothing tab there are 2 sub-tabs called Holt’s and Holt-Winters’. In each of these sub-tabs you will see a 12-month prediction of New Girl interest based on these 2 exponential smoothing models.   "
  })
  
  output$ins6 <- renderText({
    " In the ARIMA Models tab there are 2 sub-tabs called Auto ARIMA and Manual ARIMA. Within these sub-tabs you will see 12-month predictions of New Girl interest based on ARIMA models with either auto-selected parameters or manually selected parameters.   "
  })
  
  output$ins7 <- renderText({
    " In the Best Model tab, you will see the best model that we have made in predicting New Girl interest in the future!  "
  })
  
  output$restext <- renderText({
    "Residuals of a time series is the difference between the model and the observed data, with the model being the combination of the trend and seasonality component. The residual graph shows us how well the observed data matches the model! With these two times series we made a TSLM model which is fitting a linear model, including trend and seasonality components, to a time series. When looking at the results for New Girl, we can see in the top plot, or Innovation residuals, that towards 2020-2022 the plot gets thinner compared to the beginning of the time which is concerning. Ideally, there would be no pattern in the plot. When looking at the bottom left or ACF plot, we can see that there is large issue with lag. This means that the data is extremely autocorrelated at some points which is also concerning. Additionally, we can see that with the New Girl data there is super high first order autocorrelation which means that observations that are one apart are correlated. When looking at the bar chart on the bottom left, the chart roughly follows a bell-shaped curve. Overall, I would say that the residuals for New Girl are concerning which means that this model does not do a great job in fitting the data. "
  })
  
  output$restext2 <- renderText({
    "When looking at the TSLM for Game of Thrones, we can see that unlike the New Girl model, this data follows a distinct pattern, and the pattern thickens as the years go on. Although this data has a pattern where it is not supposed to, there are a few significant issues with lag, but there are smaller issues with lag compared to the New Girl model. Finally, when looking at the count bar chart, we can see that it does not follow a bell-shaped curve and has large issues with outliers which is very concerning. This means that this model for Game of Thrones does not fit the data well and could be determined as a worse model than the New Girl model. In conclusion, the biggest take away from these models are that they are extremely underfit. "
  })
  
  output$restext3 <- renderText({
    " -  "
  })
  
  output$nmw <- renderText({
    " Naïve models predict into the future using the most recent point of data. When using the naïve model to predict interest in New Girl for the next 12 months, the typical miss we expect is about 5.06 in level of interest.  "
  })
  
  output$snmw <- renderText({
    " Seasonal naïve models predict into the future using the last observed value in the same season as the current value we are predicting. When using the seasonal naïve model to predict interest in New Girl for the next 12 months, the typical miss we expect is about 12.95 in level of interest. "
  })
  
  output$mmw <- renderText({
    " Mean models predict into the future using the average value (or average interest in this case) from the historical data. When using the mean model to predict interest in New Girl for the next 12 months, the typical miss we expect is about 11.76 in level of interest.  "
  })
  
  output$dmw <- renderText({
    " Drift models predict into the future by connecting the first observed value and last observed value and continuing that line into the future. When using the drift model to predict interest in New Girl for the next 12 months, the typical miss we expect is about 5.06 in level of interest.  "
  })
  
  output$hmw <- renderText({
    " Holt’s model predicts into the future using exponential smoothing considering trend. When using Holt’s model to predict interest in New Girl for the next 12 months, the typical miss we expect is about 8.19 in level of interest.  "
  })
  
  output$hwmw <- renderText({
    " Holt-Winters’ model predicts into the future using exponential smoothing considering trend and seasonality. When using Holt-Winters’ model to predict interest in New Girl for the next 12 months, the typical miss we expect is about 14.92 in level of interest.  "
  })
  
  output$aaw <- renderText({
    " An ARIMA model predicts into the future by observing the difference between values in a time series. When using auto ARIMA to predict interest in New Girl for the next 12 months, the typical miss we expect is about 7.67 in level of interest.  "
  })
  
  output$maw <- renderText({
    " An ARIMA model predicts into the future by observing the difference between values in a time series. When using this ARIMA to predict interest in New Girl for the next 12 months, the typical miss we expect is about 9.34 in level of interest. "
  })
  
  output$bestw <- renderText({
    " When comparing all of the RMSEs of the models we have made  (naïve, seasonal naïve, mean, drift, Holt’s, Holt-Winters’, and 2 ARIMA models) using time series cross-validation we find that the mean model is the best model in predicting New Girl interest because of its lowest RMSE. Using time series cross-validation methods, we expect the mean model to typically miss by about 11.6 in predicting New Girl interest.   "
  })
  
  output$bestw2 <- renderText({
    " -  "
  })
  
  output$bestw3 <- renderText({
    " The results of the time series cross-validation is shown below!  "
  })
  
  output$myplot <- renderPlot({
    if (input$diff == "TRUE"){
      g_trends %>%  
        filter(year(Month) >= 2011) %>% 
        mutate(diff = difference(Interest)) %>% 
        autoplot(colour = "purple") 
      
    } else if (input$plot_type == "Autocorrelation") {
      g_trends %>%  
        filter(year(Month) >= 2011) %>% 
        ACF() %>% 
        autoplot(colour = "navy")
      
    } else if (input$plot_type == "Seasonality") {
      g_trends %>%
        filter(year(Month) >= 2011) %>% 
        gg_season(polar = 1)
    }
    else if (input$plot_type == "Decomposition") {
      g_trends %>% 
        filter(year(Month) >= 2011) %>% 
        model(classical_decomposition(Interest, type = "additive")) %>% 
        components() %>%
        autoplot(color = "navy")
    }
  })
  
  output$plotstab <- renderText({
    if (input$diff == "TRUE"){
      "Differencing an autocorrelation is what you do when there is extreme autocorrelation, or the data has a strong trend. Although differencing is not necessarily necessary in this situation, I thought it would be interesting to look at. With this differencing plot, we can see similar observations to what we saw in the time series plot from the Full Time Series Plot tab. We can see a slight seasonal trend based on seasons airing, as well as a decreasing interest in the show as it progressed. " 
      
    } else if (input$plot_type == "Autocorrelation") {
      "Autocorrelation is how correlated a series is to itself by looking back a certain amount of lag, or the relationship of the data at current and past values. With this plot, we can see that the data has significant autocorrelation on lags 1, 2, 3, 4, and 12. Additionally there does not seem to be a large trend in the data."
      
    } else if (input$plot_type == "Seasonality") {
      "Seasonality is when there are cycles in data that repeat consistently overtime. With this seasonality plot, we can see that there does not appear to be intense seasonality, but there appears to be seasonality based on release dates and when the show was airing. As seen in the Full Time Series Plot tab, on average the show began airing in September and ended around May (with a few exceptions). With this information, we can see the growth in interest between the months of September to May. We are also able to see the decline in interest in New Girl as the years progressed through this chart. "
    }
    else if (input$plot_type == "Decomposition") {
      "Decomposition is when you deconstruct a time series into the patterns that make up the time series. With this decomposition chart we can first look at the interest graph which is the combined graph of all the patterns. With this graph we can see that it does not have much linearity at all. Additionally, we see heavy influences of seasonal patterns and we are also able to see the decreasing trend. When looking at the trend graph, we can see that there is some linearity as well as a decreasing trend. This mirrors what we have seen through the rest of the plots of New Girl losing interest as the seasons progressed. When looking at the seasonal graph, we can see that there is a lot of seasonality. There appears to be a great jump and consistency throughout the season, as well as a peak right before the new seasons air. We are also able to see the decrease in interest during the off season. Finally, when looking at the random graph, we see relatively what we have seen in the previous decomposition graphs."
    }
  })
  
  output$NG_Ori <- renderPlot({
    ggplot(g_trends_1, aes(x=Month, y=Interest)) +
      geom_line(aes(y = Interest),col = "Black") +
      ggtitle("New Girl Search Frequency Over Time") +
      easy_center_title()
  })
  
  output$NG_Box <- renderPlotly({
    ggplotly(ggplot(g_trends_1, aes(x=Month, y=Interest)) +
               geom_line(aes(y = Interest),col = "Black") +
               geom_vline(aes(xintercept=as.numeric(as.Date("2011-09-20")),
                              colour=cols[1]),
                          linetype=4) +
               geom_vline(aes(xintercept=as.numeric(as.Date("2012-05-08")),
                              colour=cols_1[1]),
                          linetype=4) +
               geom_rect(aes(xmin = as.Date("2011-09-20", format = "%Y-%m-%d"), 
                             xmax = as.Date("2012-05-08", format = "%Y-%m-%d"),
                             ymin=0,ymax=100),color="orange",alpha=0.1, fill = "orange") +
               geom_vline(aes(xintercept=as.numeric(as.Date("2012-09-25")),
                              colour=cols[2]),
                          linetype=4) +
               geom_vline(aes(xintercept=as.numeric(as.Date("2013-05-14")),
                              colour=cols_1[2]),
                          linetype=4) +
               geom_rect(aes(xmin = as.Date("2012-09-25", format = "%Y-%m-%d"), 
                             xmax = as.Date("2013-05-14", format = "%Y-%m-%d"),
                             ymin=0,ymax=100),color="tan",alpha=0.1, fill = "tan") +
               geom_vline(aes(xintercept=as.numeric(as.Date("2013-09-17")),
                              colour=cols[3]),
                          linetype=4) +
               geom_vline(aes(xintercept=as.numeric(as.Date("2014-05-06")),
                              colour=cols_1[3]),
                          linetype=4) +
               geom_rect(aes(xmin = as.Date("2013-09-17", format = "%Y-%m-%d"), 
                             xmax = as.Date("2014-05-06", format = "%Y-%m-%d"),
                             ymin=0,ymax=100),color="green",alpha=0.1, fill = "green") +
               geom_vline(aes(xintercept=as.numeric(as.Date("2014-09-16")),
                              colour=cols[4]),
                          linetype=4) +
               geom_vline(aes(xintercept=as.numeric(as.Date("2015-05-05")),
                              colour=cols_1[4]),
                          linetype=4) +
               geom_rect(aes(xmin = as.Date("2014-09-16", format = "%Y-%m-%d"), 
                             xmax = as.Date("2015-05-05", format = "%Y-%m-%d"),
                             ymin=0,ymax=100),color="blue",alpha=0.1, fill = "blue") +
               geom_vline(aes(xintercept=as.numeric(as.Date("2016-01-05")),
                              colour=cols[5]),
                          linetype=4) +
               geom_vline(aes(xintercept=as.numeric(as.Date("2016-05-10")),
                              colour=cols_1[5]),
                          linetype=4) +
               geom_rect(aes(xmin = as.Date("2016-01-05", format = "%Y-%m-%d"), 
                             xmax = as.Date("2016-05-10", format = "%Y-%m-%d"),
                             ymin=0,ymax=100),color="light blue",alpha=0.1, fill = "light blue") +
               geom_vline(aes(xintercept=as.numeric(as.Date("2016-09-20")),
                              colour=cols[6]),
                          linetype=4) +
               geom_vline(aes(xintercept=as.numeric(as.Date("2017-04-04")),
                              colour=cols_1[6]),
                          linetype=4) +
               geom_rect(aes(xmin = as.Date("2016-09-20", format = "%Y-%m-%d"), 
                             xmax = as.Date("2017-04-04", format = "%Y-%m-%d"),
                             ymin=0,ymax=100),color="purple",alpha=0.1, fill = "purple") +
               geom_vline(aes(xintercept=as.numeric(as.Date("2018-04-10")),
                              colour=cols[7]),
                          linetype=4) +
               geom_vline(aes(xintercept=as.numeric(as.Date("2018-05-15")),
                              colour=cols_1[7]),
                          linetype=4) +
               geom_rect(aes(xmin = as.Date("2018-04-10", format = "%Y-%m-%d"), 
                             xmax = as.Date("2018-05-15", format = "%Y-%m-%d"),
                             ymin=0,ymax=100),color="pink",alpha=0.1, fill = "pink") +
               ggtitle("New Girl Season Duration- Search Frequency") +
               easy_center_title()  +
               scale_color_discrete("Seasons", labels=c('Season 1 Start',
                                                        'Season 1 End',
                                                        'Season 2 Start',
                                                        'Season 2 End',
                                                        'Season 3 Start',
                                                        'Season 3 End',
                                                        'Season 4 Start',
                                                        'Season 4 End',
                                                        'Season 5 Start',
                                                        'Season 5 End',
                                                        'Season 6 Start',
                                                        'Season 6 End',
                                                        'Season 7 Start',
                                                        'Season 7 End')))
  })
  
  output$NG <- renderPlot ({
    ggplot(g_trends_1, aes(x=Month, y=Interest)) +
      geom_line(aes(y = Interest),col = "Black") +
      geom_vline(aes(xintercept=as.numeric(as.Date("2011-09-20")),
                     colour=cols[1]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2012-09-25")),
                     colour=cols[2]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2013-09-17")),
                     colour=cols[3]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2014-09-16")),
                     colour=cols[4]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2016-01-05")),
                     colour=cols[5]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2016-09-20")),
                     colour=cols[6]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2018-04-10")),
                     colour=cols[7]),
                 linetype=4) +
      ggtitle("Start of a New Girl Season - Search Frequency") +
      easy_center_title()  +
      scale_color_discrete("Seasons", labels=c('Season 1',
                                               'Season 2',
                                               'Season 3',
                                               'Season 4',
                                               'Season 5',
                                               'Season 6',
                                               'Season 7'))
  })
  
  output$GOT <- renderPlot({
    ggplot(g_trends2_1, aes(x=Month, y=Interest_GOT)) +
      geom_line(aes(y = Interest_GOT),col = "Black") +
      geom_vline(aes(xintercept=as.numeric(as.Date("2011-04-17")),
                     colour=cols2[1]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2012-04-01")),
                     colour=cols2[2]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2013-03-31")),
                     colour=cols2[3]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2014-04-06")),
                     colour=cols2[4]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2015-04-12")),
                     colour=cols2[5]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2016-04-24")),
                     colour=cols2[6]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2017-07-16")),
                     colour=cols2[7]),
                 linetype=4) +
      geom_vline(aes(xintercept=as.numeric(as.Date("2019-04-14")),
                     colour=cols2[8]),
                 linetype=4) +
      ggtitle("Start of a Game of Thrones Season - Search Frequency") +
      easy_center_title()  +
      scale_color_discrete("Seasons", labels=c('Season 1',
                                               'Season 2',
                                               'Season 3',
                                               'Season 4',
                                               'Season 5',
                                               'Season 6',
                                               'Season 7',
                                               'Season 8'))
  })
  
  output$Co <- renderPlotly({
    ggplotly(ggplot() +
               geom_line(data = g_trends %>% filter(year(Month) >= 2011), aes(x= Month, y= Interest), color = "magenta") + 
               geom_line(data = g_trends2 %>% filter(year(Month) >= 2011), aes(x= Month, y= Interest_GOT), color = "dark green") +
               ggtitle("Comparison of New Girl and Game of Thrones Search Frequency") +
               easy_center_title() +
               scale_color_discrete(name = "Show",
                                    labels = c('New Girl',
                                               'Game of Thrones'))
    )
  })
  
  output$NGR <- renderPlot({
    residuals(naive(g_trends)) %>% 
               autoplot(color = "purple") +
               ggtitle("Residuals for New Girl") +
               easy_center_title() 
    
  })
  
  output$GOTR <- renderPlot({
    residuals(naive(g_trends2)) %>% 
               autoplot(color = "navy") + 
               ggtitle("Residuals for Game of Thrones") +
               easy_center_title() 
    
  })
  
  output$NGR2 <- renderPlot({
    fit_consNG <- g_trends %>%
    filter(year(Month) >= 2011) %>% 
    model(lm = TSLM(Interest ~ Month))
  
  fit_consNG %>% gg_tsresiduals()
  
  })
  
  output$GOTR2 <- renderPlot({
    fit_consGOT <- g_trends2 %>%
      filter(year(Month) >= 2011) %>% 
      model(lm = TSLM(Interest_GOT ~ Month))
    
    fit_consGOT %>% gg_tsresiduals()
    
  })
  
  output$naive <- renderPlot({
    NaiveFit <- train %>% 
      model(naive =  NAIVE(Interest))
    
    NaiveFit %>% 
      forecast(test) %>% 
      autoplot(NGTS)
    
  })
  
  output$seasonal <- renderPlot({
    SNaiveFit <- train %>% 
      model(snaive = SNAIVE(Interest)) 
    
    SNaiveFit %>% 
      forecast(test) %>% 
      autoplot(NGTS)
    
  })
  
  output$mean <- renderPlot({
    MeanFit <- train %>% 
      model(mean = MEAN(Interest)) 
    
    MeanFit %>% 
      forecast(test) %>% 
      autoplot(NGTS)
    
  })
  
  output$drift <- renderPlot({
    DriftFit <- train %>% 
      model(drift = RW(Interest ~ drift())) 
    
    DriftFit %>% 
      forecast(test) %>% 
      autoplot(NGTS)
    
  })
  
  output$holt <- renderPlot({
    HoltFit <- train %>% 
      model(holts = ETS(Interest ~ error("A") + trend("A") + season("N"))) 
    
    HoltFit %>% 
      forecast(test) %>% 
      autoplot(NGTS)
    
  })
  
  output$holtwin <- renderPlot({
    HWFit <- train %>% 
      model(HW = ETS(Interest ~ error("A") + trend("A") + season("A")))
    
    HWFit %>% 
      forecast(test) %>% 
      autoplot(NGTS)
    
  })
  
  output$autoar <- renderPlot({
    AAFit <- train %>% 
      model(auto = ARIMA(Interest, stepwise = FALSE, approx = FALSE))
    
    AAFit %>% 
      forecast(test) %>% 
      autoplot(NGTS)
    
  })
  
  output$manar <- renderPlot({
    AMFit <- train %>% 
      model(arima011011 = ARIMA(Interest ~ pdq(0,1,1) + PDQ(0,1,1)))
    
    AMFit %>% 
      forecast(test) %>% 
      autoplot(NGTS)
    
  })
  
  output$bmp <- renderPlot({
    MeanFit <- train %>% 
      model(mean = MEAN(Interest)) 
    
     MeanFit %>% 
      forecast(test) %>% 
      autoplot(NGTS)
    
  })
}

shinyApp(ui, server)

