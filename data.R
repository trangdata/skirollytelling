library(dplyr)

# runs <- arrow::read_parquet("data/runs.parquet")
# dartmouth <- runs |>
#   tidyr::unnest(ski_area_ids) |>
#   dplyr::filter(ski_area_ids == "74e0060a96e0399ace1b1e5ef5af1e5197a19752") |>
#   dplyr::select(run_id, run_name, run_difficulty, run_coordinates_clean)
# dart <- dartmouth |>
#   dplyr::mutate(run_coordinates_clean = purrr::map(run_coordinates_clean, ~ dplyr::select(.x, -segment_hash))) |>
#   tidyr::unnest(run_coordinates_clean)
# arrow::write_parquet(dart, "data/dartmouth_runs.parquet")

n_groups <- 32 # number of spokes
step_deg <- 360/n_groups  # Step in degrees

# Create a sequence of boundaries in radians
boundaries <- c(
  seq(-step_deg/2, 360, by = step_deg)
)

segments <- dart %>%
  rename(path = run_id, x = longitude, y = latitude) |>
  group_by(path) %>%
  arrange(index) |>
  mutate(
    index0 = index,
    xend = lead(x), # Next x-coordinate
    yend = lead(y),  # Next y-coordinate
    dx = xend - x,  # x-direction
    dy = yend - y,  # y-direction
    scaled_dx = lead(distance_vertical)*dx/4,
    scaled_dy = lead(distance_vertical)*dy/4,
    tg = (atan2(dx, dy)*180/pi)%%360,
    group = if_else(
      dy < 0,  # For dx < 0, offset group numbers by 16
      # 10-(33- findInterval(tg, boundaries)), # TODO
      findInterval(tg, boundaries) - 1,
      findInterval(tg, boundaries) - 1
    ),
    group = group %% n_groups + 1,
    state = "a",
  ) %>%
  ungroup() |> 
  mutate(index = row_number()) |> 
  group_by(group) %>%
  arrange(tg) %>%
  ungroup() |> 
  relocate(xend, yend, state, scaled_dx, scaled_dy, dx, dy,tg, group) |>
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
    x = plot_center$center_x,  # Shift start to plot center
    y = plot_center$center_y,  # Shift start to plot center
    xend = x + dx,      # Adjust endpoint
    yend = y + dy       # Adjust endpoint
  )

segments_transformed_c <- segments_transformed %>%
  arrange(path, index) |>
  mutate(
    xend = x + scaled_dx,      # Adjust endpoint
    yend = y + scaled_dy,       # Adjust endpoint
    state = "c"
  )

# Combine original and transformed data for animation
dartmouth_play <- bind_rows(
  segments,
  segments_transformed %>% mutate(state = "b"),
  segments_transformed_c,
)

write_parquet(dartmouth_play, "data/dartmouth_play.parquet")

