#install.packages("plotly", "tidyverse", "crosstalk", "forcats")
library(plotly)
library(tidyverse)
library(crosstalk)
library(forcats)

Sys.setenv("plotly_username"="kirschil")
Sys.setenv("plotly_api_key"="Yhkw6FvAAABYRuOocguT")

load("results.now.RData")

sd <- highlight_key(results.now, ~MSA, "Select an MSA")

base <- plot_ly(sd, color = I("black"), height = 400) %>%
  group_by(MSA)

p1 <- base %>%
  summarise(miss = sum(Actual, na.rm=TRUE)) %>%
  add_markers(x = ~miss, y = ~forcats::fct_reorder(MSA, miss, fun=sum), text = ~paste("MSA :", MSA, "<br> Months:", miss),
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

t <- list(
  family = "sans serif",
  size = 14,
  color = 'firebrick')

plotexample <- subplot(p1, p2, titleX = TRUE, widths = c(0.3, 0.7)) %>% 
  hide_legend() %>%
  highlight(dynamic = FALSE, selectize = TRUE, color="firebrick") %>% 
  layout(font=t)

# This would work with the paid version
chart_link = api_create(plotexample, filename="plotexample")

plotexample