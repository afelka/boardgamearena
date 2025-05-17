#load libraries
library(dplyr)
library(ggplot2)
library(tibble)
library(showtext)

#add fonts to use
font_add_google("Anton", "Anton")
font_add_google("Montserrat", "Montserrat")
showtext_auto()

# function to create Monopoly style board game template
create_custom_board_data <- function() {
  side <- 5
  
  bottom <- tibble(
    id = 1:side,
    x = rev(1:side),
    y = 1
  )
  
  left <- tibble(
    id = (side + 1):(2 * side),
    x = 1,
    y = 2:(side + 1)
  )
  
  top <- tibble(
    id = (2 * side + 1):(3 * side),
    x = 2:(side + 1),
    y = side + 1
  )
  
  right <- tibble(
    id = (3 * side + 1):(4 * side),
    x = side + 1,
    y = rev(1:side)
  )
  
  bind_rows(bottom, left, top, right)
}

# Games with no of plays. GO is given high number to place correctly
game_stats <- tibble(
  name = c(
    "Luxor", "The Castles of Burgundy", "Kingdomino", "Takenoko", "Alhambra",
    "CuBirds", "Butterfly", "Wingspan", "Ark Nova", "Can't Stop",
    "CATAN", "Tokaido", "Terraforming Mars", "Ticket to Ride", "7 Wonders Duel",
    "Lucky Numbers", "Sushi Go!", "Jaipur", "7 Wonders", "GO"
  ),
  plays = c(
    1705, 1237, 1180, 977, 875,
    719, 708, 692, 577, 563,
    496, 423, 400, 368, 347,
    312, 265, 256, 216, 2000
  )
)

# Arrange ascending by play count
sorted_games <- game_stats %>%
  arrange(plays)

# create the board game add id, add games, add label_size
board_data <- create_custom_board_data() %>%
  arrange(id) %>%
  bind_cols(sorted_games)  %>%
  mutate(label_size = ifelse(name == "GO", 10, 3.5))  # Make GO larger

# Define color groups to give Monopoly style feeling
color_groups <- c(
  rep("darkviolet", 2),   # Group 1
  rep("deepskyblue", 3),  # Group 2
  rep("darkorange", 3),   # Group 3
  rep("red", 3),          # Group 4
  rep("green", 3),        # Group 5
  rep("blue", 3),         # Group 6
  rep("brown", 2)         # Group 7
)
  
# Add to board_data (GO should have no group color)
board_data$group_color <- c(color_groups, NA)

# Calculate center x and center y to put text later
center_x <- mean(board_data$x)
center_y <- mean(board_data$y)

# create ggplot
monopoly_style_ggplot <- ggplot(board_data, aes(x = x, y = y)) +
  # Base monopoly green tiles
  geom_tile(fill = "#BFDBAE", color = "black", width = 1, height = 1) +
  
  # Colored top strip (1/5 tile height)
  geom_tile(
    data = board_data %>% filter(name != "GO"),
    aes(fill = group_color),
    width = 1,
    height = 0.2,  # 1/5 height
    position = position_nudge(y = 0.4)  # move to top part of tile
  ) +
  
  # Game labels
  geom_text(
    aes(label = ifelse(
      name == "GO",
      name,
  # add no of plays like Price in Monopoly
      paste0(name, "\n\n\nNo of Plays: ", plays)
    )),
    size = board_data$label_size,
    lineheight = 0.9,
    family = "Montserrat",
    fontface = "bold"
  ) +
  
  # Center Monopoly-style label
  geom_label(
    data = tibble(x = center_x, y = center_y, label = "TOP GAMES PLAYED"),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 15,
    family = "Anton",
    fontface = "bold",
    fill = "red",
    color = "white",
    label.size = 0
  ) +
  
  # Manual fill for color strips only
  scale_fill_manual(values = c(
    "darkviolet" = "darkviolet",
    "deepskyblue" = "deepskyblue",
    "darkorange" = "darkorange",
    "red" = "red",
    "green" = "green",
    "blue" = "blue",
    "brown" = "brown"
  ), na.value = "white") +
  
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#BFDBAE", color = NA)
  )

ggsave("monopoly_style_plot.png", plot = monopoly_style_ggplot, width = 4, height = 4, dpi = 300)