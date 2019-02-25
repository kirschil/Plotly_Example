#install.packages("plotly")
#install.packages("tidyverse")
#install.packages("crosstalk")
#install.packages("forcats")
#install.packages("lubridate")
#install.packages("zoo")

library(plotly)
library(tidyverse)
library(crosstalk)
library(forcats)
library(lubridate)
library(zoo)

Sys.setenv("plotly_username"="kirschil")
Sys.setenv("plotly_api_key"="Yhkw6FvAAABYRuOocguT")

load("results.now.RData")

results.now<- results.now %>% 
  mutate(Date= as.yearmon(as.Date(date_decimal(Date)), format="%Y-%m-%d"))
  

sd <- highlight_key(results.now, ~MSA, "Select an MSA")

base <- plot_ly(sd, color = I("black"), height = 400) %>%
  group_by(MSA)

p1 <- base %>%
  summarise(months = sum(Actual, na.rm=TRUE)) %>%
  add_markers(x = ~months, y = ~forcats::fct_reorder(MSA, months, fun=sum), text = ~paste("MSA :", MSA, "<br> Months:", months),
              hoverinfo = "text") %>%                                                                                   
  layout(
    barmode = "overlay",
    xaxis = list(title = "Number of Months in Recession"),
    yaxis = list(title = "")
  ) 

p2 <- base %>%
  add_lines(x = ~Date, y = ~Probability, alpha = 0.3,  
            text = ~paste("Probability :", round(Probability*100,2),"%", "<br> Date :", Date),
            hoverinfo = "text") %>% 
  layout(xaxis = list(title = ""))

p3 <- base %>%
  add_lines(x = ~Date, y = ~Actual, alpha = 0.3,  
            text = ~paste("Probability :", Actual*100,"%", "<br> Date :", Date),
            hoverinfo = "text") %>% 
  layout(xaxis = list(title = ""))

t <- list(
  family = "sans serif",
  size = 14,
  color = 'firebrick')

recursive_subplot_1 <- subplot(p2, p3, titleX = TRUE, widths = 0.7, nrows = 2, shareX = TRUE) %>% 
  hide_legend() %>%
  highlight(dynamic = FALSE, selectize = TRUE, color="firebrick") %>% 
  layout(font=t)


plotexample <- subplot(p1, recursive_subplot_1, titleX = TRUE, nrows = 1, widths=c(0.3, 0.7)) %>% 
  hide_legend() %>%
  highlight(dynamic = FALSE, selectize = TRUE, color="firebrick") %>% 
  layout(font=t)

# This would work with the paid version
chart_link = api_create(plotexample, filename="plotexample")

plotexample
