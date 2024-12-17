
source("setup.R")
source("data.R")

# dartmouth_play <- dartmouth_play_df


# segments_transformed_f <- segments %>%
#   mutate(
#     y = y_range[1] + (group * (y_range[2] - y_range[1]) / (n_groups + 1)),
#   ) |> 
#   group_by(group) %>%
#   # arrange(x) %>%
#   mutate(
#     x = seq(x_range[1], x_range[2], length.out = n()),
#     xend = x + dx,
#     yend = y + dy,
#     state = "f",
#   ) %>%
#   ungroup()

segments_transformed_e <- segments |> 
  mutate(state = "e")

# segments_transformed_d <- segments |> 
#   filter(group != 6) |> 
#   bind_rows(group_i |> filter(group == 6)) |> 
#   mutate(state = "d")

largest_group_n <- max(table(segments$group)) # 47
group_i <- segments %>% # TODO
  mutate(
    y = y_range[1] + ((n_groups + 1 - group) * (y_range[2] - y_range[1]) / (n_groups + 1)),
  ) |> 
  group_by(group) %>%
  mutate(
    x = head(seq(x_range[1], x_range[2], by = x_wide/largest_group_n), n()) + x_wide,
    # x = head(seq(x_range[1], x_range[2], by = x_wide/largest_group_n), n()),
    xend = x + dx,
    yend = y + dy,
  ) |> 
  ungroup()

all_states <- sort(c(LETTERS, letters))
anim_df_ls <- vector("list", length = n_groups + 1) 
anim_df_ls[[1]] <- segments |> 
  mutate(state = all_states[[1]])
for (i in seq_along(anim_df_ls)){
  anim_df_ls[[i + 1]] <- bind_rows(
    anim_df_ls[[i]] |> filter(group != i - 1) ,
    group_i |> filter(group == i - 1)
  ) |> 
    mutate(state = all_states[[i + 1]])
}
anim_df <- bind_rows(anim_df_ls)



animated_plot <- # |> 
  ggplot() +
  geom_segment(
    data = segments |> select(-state),
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "grey90",
    arrow = arrow(type = "closed", length = unit(0.1, "cm")),
    alpha = 0.8
  ) +
  # bind_rows(segments_transformed_d, segments_transformed_e, segments_transformed_f) |> 
  geom_segment(
    data = anim_df,
    aes(x = x, y = y, xend = xend, yend = yend, color = nwbw, group = group),
    arrow = arrow(type = "closed", length = unit(0.1, "cm")),
    alpha = 0.8
  ) +
  scale_color_manual(values = c("#004B59", "#f07178"), guide = "none") +
  transition_states(
    states = state,
    transition_length = 2,
    state_length = 1,
    wrap = FALSE
  ) +
  coord_dartmouth_play +
  # coord_dartmouth +
  labs(title = "Animation of Segments: {closest_state}") +
  theme_minimal() +
  guides(color = "none")
animate(animated_plot, nframes = 200, height = 7, width = 12, units = "in", 
        res = 100, end_pause = 20)
# animate(animated_plot, nframes = 100, height = 7, width = 9, units = "in", 
#         res = 100, renderer = gifski_renderer(loop = FALSE))











segments_transformed_f |> 
  # bind_rows(segments_transformed_f) |> 
  # bind_rows(segments_transformed_d, segments_transformed_e, segments_transformed_f) |> 
  ggplot() +
  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend, color = nwbw),
    arrow = arrow(type = "closed", length = unit(0.1, "cm")),
    alpha = 0.8
  ) +
  # transition_states(
  #   states = state,
  #   transition_length = 2,
  #   state_length = 1
  # ) +
  # labs(title = "Animation of Segments: {closest_state}") +
  theme_minimal()

dartmouth_play_plot <- dartmouth_play |> 
  ggplot() +
  geom_segment(
    # color = "#f07178",
    alpha = 0.8,
    aes(x = x, y = y, xend = xend, yend = yend, color = nwbw),
    arrow = arrow(type = "closed", length = unit(0.1, "cm"))
  ) +
  theme_minimal() +
  # coord_dartmouth +
  scale_color_manual(values = c("#004B59", "#f07178"), guide = "none") +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
  ) +
  coord_fixed(
    xlim = x_range - 0.0022,
    ylim = y_range + 0.00015,
    ratio = ratio
  ) +
  bg_transparent() +
  transition_states(state, transition_length = c(2,1,2), state_length = c(2, 1, 4)) +
  ease_aes("cubic-in-out") +
  NULL
animate(dartmouth_play_plot)


# Map segments within each group to equally spaced positions
dartmouth_play <- segments %>%
  group_by(group) %>%
  arrange(x) %>%
  mutate(
    new_x = seq(x_range[1], x_range[2], length.out = n()),
    new_y = as.numeric(group)
  ) %>%
  ungroup()

# Combine original and new positions for animation
dartmouth_play_long <- dartmouth_play %>%
  pivot_longer(
    cols = c(x, y, xend, yend, new_x, new_y),
    names_to = c(".value", "state"),
    names_pattern = "(.*)_(.*)"
  )

# Create the animated plot
animated_plot <- ggplot(dartmouth_play_long) +
  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend, color = nwbw),
    arrow = arrow(type = "closed", length = unit(0.1, "cm")),
    alpha = 0.8
  ) +
  transition_states(
    states = state,
    transition_length = 2,
    state_length = 1
  ) +
  labs(title = "Animation of Segments: {closest_state}") +
  theme_minimal()

# Render the animation
animate(animated_plot, duration = 5, fps = 10)
library(tidyr)


coord_dartmouth <- coord_cartesian(
  xlim = c(-72.12, -72.0794),
  ylim = c(43.7781, 43.7902)
)

dart <- read_parquet("data/dartmouth_runs.parquet") |> 
  group_by(run_id)
dartmouth_play <- read_parquet("data/dartmouth_play.parquet")
segments <- dartmouth_play |> 
  filter(state == "a")
dartmouth_img <- png::readPNG("images/dartmouth.png")

dart |> 
  arrange(index) |> 
  ggplot() +
  aes(x = longitude, y = latitude) +
  background_image(dartmouth_img) +
  geom_point(color = "#f07178") +
  coord_dartmouth +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(),
  ) +
  bg_transparent()
  

dart |> 
  arrange(index) |> 
  ggplot() +
  aes(x = longitude, y = latitude) +
  geom_point(color = "#f07178") +
  coord_dartmouth +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(),
  ) +
  bg_transparent()
library(ggplot2)
library(forcats)
library(gghighlight)
segments |> 
  mutate(group = as_factor(group)) |> 
ggplot() +
  geom_segment(
    # color = "#f07178",
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(type = "open", length = unit(0.2, "cm"))
  ) +
  # theme_minimal() +
  gghighlight() +
  facet_wrap(~ group) +
  coord_dartmouth +
  labs(x = NULL, y = NULL) +
  theme(
    # legend.position = "none",
    axis.text = element_blank(),
  ) 


p <- ggplot(dartmouth_play) +
  geom_segment(
    color = "#f07178",
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(type = "closed", length = unit(0.2, "cm"))
  ) +
  theme_minimal() +
  # coord_dartmouth +
  theme(
    legend.position = "none"
  ) +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(),
  ) +
  bg_transparent()

p_play = p +
  transition_states(state, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out') 
# Render the animation
animate(p_play, width = 600, height = 400, nframes = 100, fps = 20)

# Plot with arrows for each segment






# Convert to sf object
df_sf <- st_as_sf(dart, coords = c("longitude", "latitude"), 
                  crs = 4326)

# Plot with tmap
tmap_mode("view") # Switch to interactive mode
tm_shape(df_sf) +
  tm_dots(col = "#f07178", size = 0.01) +
  tm_basemap("OpenStreetMap")
tm_shape(df_sf) +
  tm_dots(col = "#f07178", size = 0.01) +
  tm_basemap(NULL)


dat <- read_parquet("data/ski_area_metrics.parquet")
bearings_ls <- dat |>
  filter(
    run_count >= 3, combined_vertical >= 50, ski_area_name != "",
    country == "United States", nchar(ski_area_name) < 20
  ) |>
  sample_n(48) |>
  bind_rows(dat |> filter(ski_area_name == "Whaleback Mountain")) |>
  arrange(ski_area_name) |>
  select(ski_area_name, bearings) |>
  deframe() |>
  lapply(
    \(x) filter(x, num_bins == 32) |>
      mutate(
        color = if_else(bin_index == 2, "#f07178", "grey85"),
      )
  )



plot_rose <- function(dat, ski_area_name, highlight = FALSE, labels = NULL) {
  plot <- dat |>
    ggplot() +
    aes(x = bin_center, y = bin_count) +
    coord_radial(start = -pi / 32, expand = FALSE) +
    scale_x_continuous(
      breaks = seq(0, 270, 90),
      labels = labels
    ) +
    scale_y_continuous(
      breaks = max(dat$bin_count)
    ) +
    labs(title = ski_area_name) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      # axis.text.x = element_text(size = 20),
      plot.title = element_text(hjust = 0.5)
    )
  if (!highlight) {
    plot <- plot + geom_col(fill = "#f07178")
  } else {
    plot <- plot +
      geom_col(aes(fill = color)) +
      scale_fill_identity()
  }
  plot
}
whaleback <- bearings_ls[["Whaleback Mountain"]]
# plot_rose(whaleback, "Whaleback Mountain", labels = c("N", "E", "S", "W"))
# plot_rose(whaleback, "Whaleback Mountain", TRUE, labels = c("N", "E", "S", "W"))
cowplot::plot_grid(plotlist = purrr::map2(bearings_ls, names(bearings_ls), plot_rose, size_title = 9, size_x = 7))

library(gganimate)
dat <- bind_rows(replicate(32, whaleback, simplify = FALSE)) |>
  mutate(
    year = rep(1:32, each = 32),
    alpha = if_else((row_number() - 1) %% 33 == 0, 1, 0.7),
    focus = if_else((row_number() - 1) %% 33 == 0, 1, 0),
  )
dat |>
  ggplot() +
  aes(x = bin_center, y = bin_count) +
  geom_col(fill = "#f07178", aes(alpha = alpha)) +
  scale_alpha_identity() +
  coord_radial(start = -pi / 32, expand = FALSE) +
  scale_x_continuous(
    breaks = seq(0, 270, 90),
    labels = labels
  ) +
  scale_y_continuous(
    breaks = max(dat$bin_count)
  ) +
  labs(title = "") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    # axis.text.x = element_text(size = 20),
    plot.title = element_text(hjust = 0.5)
  ) +
  transition_states(year, transition_length = 1, state_length = 2)
