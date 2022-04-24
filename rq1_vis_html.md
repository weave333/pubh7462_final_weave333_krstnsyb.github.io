---
title: 'RQ 1 Visualization 2'
output: 
  html_document:
    keep_md: true
---



### RQ1 data visualization #2

At what times of day was initiation of oral feeding most common?


```r
# Read in data
df_final <- read_rds("./source/df_final.rds")
```


```r
# Create data set with variables to count oral feed initiation at every hour
df_vis2 <- df_final %>%
  mutate(
    oral_t = as.POSIXct(enteral_t_oral_initiate),
    oral_count = floor_date(oral_t, "hour"), # round time to nearest hour
    hr = lubridate::hour(oral_count)
  ) %>% # pull out the hour for counting
  count(hr) %>% # create variable with counts per hour
  mutate(
    hr = hms::hms(hours = hr), # turn integer into hms type
    hr2 = as.character(hr), # create second variable to use in hover text
    hr2 = str_sub(hr, start = 1, end = 5), # keep only hours and minutes for hover text
    text_label = str_c( # adding text for the tooltip
      "\nHour of day - ", hr2,
      "\n# starting oral feeds - ", n
    )
  )

# Create function to be able to truncate x axis labels to hh:mm in ggplot
format_hm <- function(sec) stringr::str_sub(format(sec), end = -4L)

# Create original ggplot
p <- df_vis2 %>%
  ggplot(aes(x = hr, y = n)) +
  geom_area(fill = "#6d3d6f", alpha = 0.7, linetype = 1, color = "black") +
  geom_point(aes(text = text_label), alpha = 0.5) +
  labs(title = "Time of day postoperative oral feeding was initiated \n for infants undergoing cardiac surgery") +
  xlab("Time of day") +
  ylab("# of infants") +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
    )
  ) +
  scale_x_time(labels = format_hm) # truncate x axis to hh:mm

# Pass to ggplotly
ggplotly(p, tooltip = "text")
```

```{=html}
<div id="htmlwidget-b085ec5611e245d9765f" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-b085ec5611e245d9765f">{"x":{"data":[{"x":[0,3600,7200,10800,14400,18000,21600,25200,28800,32400,36000,39600,43200,46800,50400,54000,57600,61200,64800,68400,72000,75600,79200,82800,82800,82800,79200,75600,72000,68400,64800,61200,57600,54000,50400,46800,43200,39600,36000,32400,28800,25200,21600,18000,14400,10800,7200,3600,0,0],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,8,17,17,6,8,11,11,12,12,6,11,12,21,14,16,9,7,10,9,5,14,16,16,0],"text":"","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,0,0.7)","dash":"solid"},"fill":"toself","fillcolor":"rgba(109,61,111,0.7)","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,3600,7200,10800,14400,18000,21600,25200,28800,32400,36000,39600,43200,46800,50400,54000,57600,61200,64800,68400,72000,75600,79200,82800],"y":[16,16,14,5,9,10,7,9,16,14,21,12,11,6,12,12,11,11,8,6,17,17,8,9],"text":["<br />Hour of day - 00:00<br /># starting oral feeds - 16","<br />Hour of day - 01:00<br /># starting oral feeds - 16","<br />Hour of day - 02:00<br /># starting oral feeds - 14","<br />Hour of day - 03:00<br /># starting oral feeds - 5","<br />Hour of day - 04:00<br /># starting oral feeds - 9","<br />Hour of day - 05:00<br /># starting oral feeds - 10","<br />Hour of day - 06:00<br /># starting oral feeds - 7","<br />Hour of day - 07:00<br /># starting oral feeds - 9","<br />Hour of day - 08:00<br /># starting oral feeds - 16","<br />Hour of day - 09:00<br /># starting oral feeds - 14","<br />Hour of day - 10:00<br /># starting oral feeds - 21","<br />Hour of day - 11:00<br /># starting oral feeds - 12","<br />Hour of day - 12:00<br /># starting oral feeds - 11","<br />Hour of day - 13:00<br /># starting oral feeds - 6","<br />Hour of day - 14:00<br /># starting oral feeds - 12","<br />Hour of day - 15:00<br /># starting oral feeds - 12","<br />Hour of day - 16:00<br /># starting oral feeds - 11","<br />Hour of day - 17:00<br /># starting oral feeds - 11","<br />Hour of day - 18:00<br /># starting oral feeds - 8","<br />Hour of day - 19:00<br /># starting oral feeds - 6","<br />Hour of day - 20:00<br /># starting oral feeds - 17","<br />Hour of day - 21:00<br /># starting oral feeds - 17","<br />Hour of day - 22:00<br /># starting oral feeds - 8","<br />Hour of day - 23:00<br /># starting oral feeds - 9"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","opacity":0.5,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Time of day postoperative oral feeding was initiated <br /> for infants undergoing cardiac surgery","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-4140,86940],"tickmode":"array","ticktext":["00:00","04:00","08:00","12:00","16:00","20:00","24:00"],"tickvals":[0,14400,28800,43200,57600,72000,86400],"categoryorder":"array","categoryarray":["00:00","04:00","08:00","12:00","16:00","20:00","24:00"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Time of day","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-1.05,22.05],"tickmode":"array","ticktext":["0","5","10","15","20"],"tickvals":[0,5,10,15,20],"categoryorder":"array","categoryarray":["0","5","10","15","20"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"# of infants","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"9205f2e1815":{"x":{},"y":{},"type":"scatter"},"92051b2f127c":{"x":{},"y":{},"text":{}}},"cur_data":"9205f2e1815","visdat":{"9205f2e1815":["function (y) ","x"],"92051b2f127c":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```
