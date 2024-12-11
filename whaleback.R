library(arrow)
library(dplyr)
library(tibble)
library(ggplot2)
library(ggpubr)
set.seed(1618)

runs <- read_parquet("data/runs.parquet")
# dartmouth <- runs |>
#   tidyr::unnest(ski_area_ids) |>
#   filter(ski_area_ids == "74e0060a96e0399ace1b1e5ef5af1e5197a19752") |>
#   select(run_id, run_name, run_difficulty, run_coordinates_clean)
# dart <- dartmouth |>
#   mutate(run_coordinates_clean = purrr::map(run_coordinates_clean, ~ select(.x, -segment_hash))) |> 
#   tidyr::unnest(
#     run_coordinates_clean 
#   )
# write_parquet(dart, "data/dartmouth_runs.parquet")

dart <- read_parquet("data/dartmouth_runs.parquet") |> 
  group_by(run_id) |> 
  arrange(latitude, longitude)
dartmouth_img <- png::readPNG("images/dartmouth.png")
runs <-  unique(dart$run_id)
chec = dart |> 
  # filter(run_id == runs[[7]]) |>
  arrange(index) 
chec |> 
  ggplot() +
  aes(x = longitude, y = latitude) +
  # background_image(dartmouth_img) +
  geom_point(color = "#f07178") +
  # geom_text(aes(label = index)) +
  geom_path(aes(group = run_id)) +
  coord_cartesian(
    # expand =
    xlim = c(-72.12, -72.0792),
    ylim = c(43.7781, 43.7902)
  ) +
  theme_minimal() +
  NULL

segments <- dart %>%
  rename(path = run_id, x = longitude, y = latitude) |> 
  group_by(path) %>%
  arrange(index) |> 
  mutate(
    xend = lead(x), # Next x-coordinate
    yend = lead(y)  # Next y-coordinate
  ) %>%
  filter(!is.na(xend)) # Remove rows where there is no "next" point
plot_center <- segments %>%
  ungroup() |> 
  summarise(
    center_x = (min(x, xend) + max(x, xend)) / 2,
    center_y = (min(y, yend) + max(y, yend)) / 2
  )
# Center the plot at calculated center
segments_transformed <- segments %>%
  mutate(
    dx = xend - x,  # x-direction
    dy = yend - y,  # y-direction
    x = plot_center$center_x,  # Shift start to plot center
    y = plot_center$center_y,  # Shift start to plot center
    xend = x + dx,      # Adjust endpoint
    yend = y + dy       # Adjust endpoint
  )

segments_transformed_c <- segments_transformed %>%
  mutate(
    xend = x + 7*dx,      # Adjust endpoint
    yend = y + 7*dy,       # Adjust endpoint
    state = "c"
  )

# Combine original and transformed data for animation
animation_data <- bind_rows(
  segments %>% mutate(state = "a"),
  segments_transformed %>% mutate(state = "b"),
  segments_transformed_c,
)


# Create animation
p <- ggplot(animation_data) +
  geom_segment(
    color = "#f07178",
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(type = "closed", length = unit(0.2, "cm"))
  ) +
  # aes(color = factor(path)) +
  theme_minimal() +
  # rcartocolor::scale_color_carto_d() +
  # scale_color_viridis_d() +
  # labs(color = "Path") +
  theme(
    legend.position = "none"
    
  ) +
  # geom_segment(
  #   aes(x = x, y = y, xend = xend, yend = yend, color = factor(path)),
  #   arrow = arrow(type = "closed", length = unit(0.2, "cm"))
  # ) +
  # theme_minimal() +
  # labs(color = "Path") +
  coord_fixed() +
  ggtitle("Shifting Arrows: {closest_state}")
p
p_play = p +
  transition_states(state, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out') 
# Render the animation
animate(p_play, width = 600, height = 400, nframes = 100, fps = 20)

# Plot with arrows for each segment
# ggplot(segments) +
#   geom_segment(
#     color = "#f07178",
#     aes(x = x, y = y, xend = xend, yend = yend),
#     arrow = arrow(type = "closed", length = unit(0.2, "cm"))
#   ) +
#   # aes(color = factor(path)) +
#   theme_minimal() +
#   # rcartocolor::scale_color_carto_d() +
#   # scale_color_viridis_d() +
#   # labs(color = "Path") +
#   theme(
#     legend.position = "none"
#   
#   )

dart |> 
  # filter(run_id == runs[[1]]) |>
  arrange(index) |> 
  ggplot() +
  aes(x = longitude, y = latitude) +
  background_image(dartmouth_img) +
  geom_point(color = "#f07178") +
  geom_path(aes(group = run_id)) +
  coord_cartesian(
    # expand =
    xlim = c(-72.12, -72.0794),
    ylim = c(43.7781, 43.7902)
  ) +
  theme_minimal() +
  NULL


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
cowplot::plot_grid(plotlist = purrr::map2(bearings_ls, names(bearings_ls), plot_rose))

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
