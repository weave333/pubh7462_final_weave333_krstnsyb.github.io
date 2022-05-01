#Convert factors to numeric for summary statistics
sum_stats <- df_final %>% 
  mutate(diagnosis = ifelse(df_final$diagnosis == "CoA", 0 ,1),
         early_extub = ifelse(df_final$early_extub == "No", 0, 1))


# Summary stats
sum_stats_final <- sum_stats %>% 
  group_by(blind_id) %>% 
  summarise(
    "# Days to Post-op Feeding" = unique(enteral_d_oral_initiate_day),
    "% Early Extubated (Yes)" = (mean(early_extub) * 100),
    "% Diagnosis (Tetralogy)" = (mean(diagnosis) * 100)) %>% 
    ungroup() %>% 
      pivot_longer(
        cols = contains(c("#", "%")),
          names_to = "Category",
          values_to = "Value"
      )

#gt() output
sum_stats_final %>% 
  group_by(Category) %>% 
  summarise(
    across(
      contains(c("Value")),
      list(Mean = mean, SD = sd), na.rm = TRUE,
      .names = "{.fn}")) %>% 
    gt() %>% 
    fmt_number(columns = 3, decimals = 2) %>% 
    fmt_number(columns = 2, decimals = 2) %>% 
    tab_header("Summary Statistics") 


##################################################
##################################################
##################################################
##################################################

#DF for visualizing extubation and days until first post-op feeding
rq2_figure <- df_rq2 %>% 
  group_by(blind_id, enteral_d_oral_initiate_day, diagnosis, early_extub) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    diagnosis = recode(diagnosis, 'ToF' = 'Tetralogy of Fallout', 'CoA' = 'Coarctation of the Aorta')
  ) %>% 
  ggplot(aes(x = enteral_d_oral_initiate_day, y = n, fill = early_extub)) + 
  geom_bar(stat = "identity",  position = "stack") + 
  labs(title = "Frequency of Days until Post-Op Oral Feeding\n By Early Extubation Status",
       y = '# Patients Receiving \nFirst Post-Op Oral Feeding',
       x = '# of Days Until First Post-Op Oral Feeding') +
  viridis::scale_fill_viridis(
    name = 'Early Extubation',
    discrete = T
  )

#generate ggplotly for first figure
ggplotly(rq2_figure)

#DF for visualizing extubation and days until first post-op feeding by diagnosis group
rq2_figure_diag <- df_rq2 %>% 
  group_by(blind_id, enteral_d_oral_initiate_day, diagnosis, early_extub) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    diagnosis = recode(diagnosis, 'ToF' = 'Tetralogy of Fallout', 'CoA' = 'Coarctation of the Aorta')
  ) %>% 
  ggplot(aes(x = enteral_d_oral_initiate_day, y = n, fill = early_extub)) + 
  geom_bar(stat = "identity",  position = "stack") + 
  labs(title = "Frequency of Days until Post-Op Oral Feeding\n By Early Extubation Status and Diagnosis Group",
       y = '# Patients Receiving \nFirst Post-Op Oral Feeding',
       x = '# of Days Until First Post-Op Oral Feeding') +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 5)) +
  facet_grid(~diagnosis) + 
  viridis::scale_fill_viridis(
    name = 'Early Extubation',
    discrete = T
  )

#generate ggplotly for second figure
ggplotly(rq2_figure_diag)

rq2_figure_diag_ext <- df_rq2 %>% 
  group_by(blind_id, enteral_d_oral_initiate_day, diagnosis, early_extub) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    diagnosis = recode(diagnosis, 'ToF' = 'Tetralogy of Fallout', 'CoA' = 'Coarctation of the Aorta')
  ) %>% 
  ggplot(aes(x = enteral_d_oral_initiate_day, y = n)) + 
  geom_bar(stat = "identity",  position = "stack") + 
  labs(y = '# Patients Receiving \nFirst Post-Op Oral Feeding',
       x = '# of Days Until First Post-Op Oral Feeding') +
  facet_grid(early_extub ~ diagnosis) + 
  scale_y_continuous(sec.axis = sec_axis(name = 'number of stones')) +
  viridis::scale_fill_viridis(
    name = 'Early Extubation',
    discrete = T
  )

ggplotly(rq2_figure_diag_ext)


sum_stats %>%
  plot_ly(
  y      = ~enteral_d_oral_initiate_day,
  color  = ~diagnosis,
  type   = "box",
  colors = viridis::viridis_pal(option = "C")(2) 
) %>%
  layout(
    title  = "Distribution of Airbnb Price by Neighbourhood",
    xaxis  = list(title = "Price")
  )


# plot in ggplot
rq2_plot <- sum_stats %>%
  ggplot(aes(x = enteral_d_oral_initiate_day, y = diagnosis, colour = oral_postextub)) + 
  geom_point(alpha = 0.3, size = 2) + 
  theme(axis.text.y = element_text(color = "black", 
                                   size = 10,  hjust = 1), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, size = 10)) +
  labs(
    x = "Time",
    y = "Number of Bites per Month",
    title = "Number of Bites Over Time"
  ) 

rq2_plot 

# print plot as plotly
ggplotly(rq2_plot)

##################################################
##################################################
##################################################
##################################################






#### POISSON ######

poiss_model <- glm(enteral_d_oral_initiate_day ~ early_extub +
                     diagnosis, data = df_final,
                   family=poisson(link="log")) 

poiss_model %>%
  broom::tidy() %>% 
  select(term, estimate, p.value) %>% 
  knitr::kable()

exp(coef(poiss_model)) %>% 
  broom::tidy() %>% 
  knitr::kable()

#Relative risk
exp(confint(poiss_model)) %>% 
  knitr::kable(caption = "95% Confidence Interval")

###################
