# Corr plot
corr_data <- joined %>%
  select(-c(UniversalPlayerId, Player, Season, seasons, plays, total_points))
cors <- cor(corr_data)[11, ]
cor_names <- c("College Scheme", "Passing Yards per Attempt", "Passing TDs",
               "Average Depth per Attempt", "Air Yards per Attempt",
               "Completion Percentage", "On Target Throw Percentage",
               "Interceptions", "Dropped Interceptions", "CFB Points per Play", "NFL PPP")
correlation_df <- data.frame(variable = cor_names,
                             `Correlation Coefficient` = cors) %>%
  rename("Correlation Coefficient" = Correlation.Coefficient)
correlation_df <- correlation_df[-11, ]
# Plot with color gradient bars
corr_plot <- ggplot(correlation_df, aes(x = reorder(variable, `Correlation Coefficient`), y = `Correlation Coefficient`, fill = `Correlation Coefficient`)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "viridis") +
  labs(title = "Correlation Coefficients with NFL SIS Points Per Play",
       x = "Variables",
       y = "Correlation Coefficient") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
# Clean data set
final <- final_df %>%  
  mutate(predicted_scheme = paste("Scheme", scheme_num)) %>%
  select(Player, predicted_scheme, cfb_ppp, yr1_x_ppp = `1`, yr2_x_ppp = `2`, yr3_x_ppp = `3`)
final <- final %>%
  rename("Predicted Scheme" = predicted_scheme)
# Use pivot longer to transpose points values via each scheme
data_long <- pivot_longer(final, cols = c(cfb_ppp, yr1_x_ppp, yr2_x_ppp, yr3_x_ppp), names_to = "Prediction", values_to = "Points")
data_long <- data_long %>%
  mutate(Prediction = ifelse(Prediction == "cfb_ppp", "Actual CFB Points", Prediction),
         Prediction = ifelse(Prediction == "yr1_x_ppp", "Expected Year 1 Points", Prediction),
         Prediction = ifelse(Prediction == "yr2_x_ppp", "Expected Year 2 Points", Prediction),
         Prediction = ifelse(Prediction == "yr3_x_ppp", "Expected Year 3 Points", Prediction))
# Create barplot
bar_chart <- data_long %>%
  rename("Predicted Scheme" = pred_scheme) %>%
  filter(Prediction == "Actual CFB Points") %>%
  ggplot(aes(x = Player, y = Points, fill = `Predicted Scheme`)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Player College Average Points per Play by Each Scheme",
    subtitle = "Points = Passer Points or Rusher Points based the play"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5))
# Create a data set that lists player, scheme, cfb points, and projections
final <- final %>%
  select(Player, pred_scheme = `Predicted Scheme`, cfb_ppp, yr1_x_ppp, yr2_x_ppp, yr3_x_ppp) %>%
  arrange(-cfb_ppp) %>%
  mutate(headshot = ifelse(Player == "J.J. McCarthy", "https://a.espncdn.com/combiner/i?img=/i/headshots/college-football/players/full/4433970.png&w=350&h=254", NA),
         headshot = ifelse(Player == "Caleb Williams", "https://a.espncdn.com/combiner/i?img=/i/headshots/college-football/players/full/4431611.png&w=350&h=254", headshot),
         headshot = ifelse(Player == "Drake Maye", "https://a.espncdn.com/combiner/i?img=/i/headshots/college-football/players/full/4431452.png&w=350&h=254", headshot),
         headshot = ifelse(Player == "Jayden Daniels", "https://a.espncdn.com/combiner/i?img=/i/headshots/college-football/players/full/4426348.png&w=350&h=254", headshot),
         headshot = ifelse(Player == "Bo Nix", "https://a.espncdn.com/combiner/i?img=/i/headshots/college-football/players/full/4426338.png&w=350&h=254", headshot),
         headshot = ifelse(Player == "Michael Penix Jr.", "https://a.espncdn.com/combiner/i?img=/i/headshots/college-football/players/full/4360423.png&w=350&h=254", headshot)
  ) # Create headshot column w/ player url
# Create a function that lets you input a scheme number to make a table that displays:
# Player, headshot, scheme number, cfb points, and each yr proj.
create_table <- function(num) {
  final %>%
    filter(pred_scheme == paste("Scheme", num)) %>%
    select(Player, headshot, pred_scheme, cfb_ppp, yr1_x_ppp, yr2_x_ppp, yr3_x_ppp) %>%
    gt() %>%
    cols_align(align = "center", columns = everything()) %>%
    cols_label( # Change colnames
      cfb_ppp = "SIS Points",
      pred_scheme = "Predicted Scheme",
      yr1_x_ppp = "Year 1 Predicted Points",
      yr2_x_ppp = "Year 2 Predicted Points",
      yr3_x_ppp = "Year 3 Predicted Points",
      headshot = "") %>%
    data_color(
      columns = vars("cfb_ppp", "yr1_x_ppp", "yr2_x_ppp", "yr3_x_ppp"), colors = scales::col_numeric(c("red3", "green3"), # Create color scale
                                                                                                     domain = NULL)
    ) %>%
    gt_img_rows(headshot) %>%
    gt_theme_538() %>%
    tab_header(title = paste("SIS Points per Play and Expected Points Per Play in Scheme", num)) # Create title that functionally labels each scheme
}
# Apply function
scheme_1 <- create_table(1)
scheme_2 <- create_table(2)
scheme_3 <- create_table(3)
# Create a data frame with player, headshot and points data regardless of scheme
plotty <- final %>%
  group_by(Player, headshot) %>%
  summarise(
    sis = mean(cfb_ppp),
    yr1 = mean(yr1_x_ppp),
    yr2 = mean(yr2_x_ppp),
    yr3 = mean(yr3_x_ppp)
  ) %>%
  arrange(-sis) %>% ungroup()
avg_table <- plotty %>%
  gt() %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_label(
    sis = "Average SIS Points",
    yr1 = "Average Year 1 Predicted Points",
    yr2 = "Average Year 2 Predicted Points",
    yr3 = "Average Year 3 Predicted Points",
    headshot = "") %>%
  data_color(
    columns = vars("sis", "yr1", "yr2", "yr3"), colors = scales::col_numeric(c("red3", "green3"), 
                                                                             domain = NULL)
  ) %>%
  gt_img_rows(headshot) %>%
  gt_theme_538() %>%
  tab_header(title = paste("Player Average Point Performance"))
# Create a transposed data frame from "plotty" that displays projections for each year
proj <- pivot_longer(plotty, cols = c(yr1, yr2, yr3), names_to = "Prediction", values_to = "Points")
proj_plot <- proj %>%
  mutate(  # Change up variable appearance
    Prediction = ifelse(Prediction == "yr1", "Year 1", Prediction),
    Prediction = ifelse(Prediction == "yr2", "Year 2", Prediction),
    Prediction = ifelse(Prediction == "yr3", "Year 3", Prediction)
  ) %>%
  ggplot(aes(x = Prediction, y = Points, label = Player, image = headshot, group = Player)) +
  geom_image(size = 0.05) +
  geom_line() +
  labs(x = "Year", y = "Projected Points", title = "Quarterback Projected Points in Their First Three Seasons") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
# Save visuals
ggsave("Corr Plot.png", corr_plot)
ggsave("Bar Chart.png", bar_chart)
gtsave(scheme_1, "Scheme 1 table.png")
gtsave(scheme_2, "Scheme 2 table.png")
gtsave(scheme_3, "Scheme 3 table.png")
gtsave(avg_table, "Scheme Ind.png")
ggsave("Player Plot Projections.png", proj_plot)