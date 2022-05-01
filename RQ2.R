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




# plot in ggplot
time.overall.plot <- time.overall.df %>%
  ggplot(aes(x = date, y = `number of bites`, colour = breed)) + 
  geom_point(alpha = 0.3, size = 2) + 
  geom_line(size = 1, alpha = 0.5) + 
  geom_smooth(size = 1.5, alpha = 0.5, se = F, method = "lm") + 
  theme(axis.text.y = element_text(color = "black", 
                                   size = 10,  hjust = 1), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, size = 10)) +
  labs(
    x = "Time",
    y = "Number of Bites per Month",
    title = "Number of Bites Over Time"
  ) + 
  viridis::scale_colour_viridis(
    name = "Dog Breed", 
    discrete = TRUE
  ) +
  xlim(c(2015, 2018)) 
# print plot as plotly
ggplotly(time.overall.plot)







#### POISSON ######

poiss_model <- glm(enteral_d_oral_initiate_day ~ early_extub +
                     diagnosis, data = df_final,
                   family=poisson(link="log")) 

library(gt)



poiss_model %>%
  broom::tidy() %>% 
  select(term, estimate, p.value) %>% 
  knitr::kable()

table(df_full$enteral_d_oral_initiate_day)


###################
