RQ 1 Visualization 2
================

### RQ1 data visualization \#2

At what times of day was initiation of oral feeding most common?

``` r
# Read in data
df_final <- read_rds("./source/df_final.rds")
```

``` r
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

<img src="rq1_vis2_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />
