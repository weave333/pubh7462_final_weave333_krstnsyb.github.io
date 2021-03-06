RQ 1 Visualization 1
================

``` r
# Read in data
df_final <- read_rds("./source/df_final.rds")
```

### RQ1 data visualization \#1

On average, how long (days) did it take to initiate the first oral feed
in the ICU after extubation?

``` r
# Create original ggplot
p <- ggplot() +
  geom_histogram(
    data = df_final,
    aes(
      x = enteral_d_oral_initiate_day,
      fill = diagnosis
    ), 
    bins = 12, alpha = 0.8
  ) +
  scale_fill_manual(
    name = " ",
    labels = c("Coarcation of the aorta", "Tetralogy of Fallot"),
    values = c(CoA = "#371c38", ToF = "#e3c559")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(0, 0, 30, 0)
    ),
    plot.caption = element_text(
      face = "italic",
      size = 7
    ),
    legend.position = "bottom"
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, 1), lim = c(-1, 10)
  ) +
  labs(
    title = "Postextubation days to oral feeding initiation after infant surgical repair for \n Coarctation of the aorta or Tetralogy of Fallot",
    caption = "Note: 4 outliers not pictured at 18, 26, 31, and 58 days"
  ) +
  xlab("Postextubation day of oral feeding initiation") +
  ylab("Number of infants")

# Create data frame with mean and median
df_stats <- df_final %>%
  summarize(
    mean = mean(enteral_d_oral_initiate_day),
    median = median(enteral_d_oral_initiate_day)
  )

# Add mean and median lines to original ggplot
p +
  geom_vline( # Add mean line 
    data = df_stats,
    aes(xintercept = mean),
    col = "black",
    linetype = "dashed",
    alpha = 0.5
  ) +
  geom_vline( # Add median line
    data = df_stats,
    aes(xintercept = median),
    col = "black",
    linetype = "solid",
    alpha = 0.5
  ) +
  annotate("text", x = 1, y = 190, label = "median", size = 2.5) + # Add label
  annotate("text", x = 1.90, y = 190, label = "mean", size = 2.5) + # Add label
  coord_cartesian(ylim = c(0, 170), clip = "off") # Allow for annotation outside of the plot 
```

<img src="rq1_vis_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />
