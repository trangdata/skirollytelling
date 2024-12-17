library(arrow)
library(dplyr)
library(tibble)
library(ggplot2)
library(gganimate)
library(ggpubr)
set.seed(1618)

dat <- read_parquet("data/ski_area_metrics.parquet")
dart <- read_parquet("data/dartmouth_runs.parquet") |>
  group_by(run_id)
dartmouth_play_df <- read_parquet("data/dartmouth_play.parquet")
segments <- dartmouth_play_df |>
  filter(state == "a")
dartmouth_img <- png::readPNG("images/dartmouth.png", native = TRUE)

x_range <- c(-72.1128, -72.081)
y_range <- c(43.7781, 43.7902)
x_wide <- x_range[2] - x_range[1]

length_x <- diff(x_range) # Length in x-direction
length_y <- diff(y_range) # Length in y-direction

desired_yx_ratio <- dim(dartmouth_img)[1] / dim(dartmouth_img)[2]
ratio <- (length_x / length_y) * desired_yx_ratio
coord_dartmouth <- coord_fixed(
  xlim = x_range,
  ylim = y_range,
  ratio = ratio
)
coord_dartmouth_play <- coord_fixed(
  xlim = c(x_range[1], x_range[2] + x_wide),
  ylim = y_range,
  ratio = ratio
)

bearings_ls <- dat |>
  filter(
    run_count >= 3, combined_vertical >= 50, ski_area_name != "",
    country == "United States", nchar(ski_area_name) < 20
  ) |>
  sample_n(48) |>
  # bind_rows(dat |> filter(ski_area_name == "Whaleback Mountain")) |>
  bind_rows(dat |> filter(ski_area_name == "Dartmouth Skiway")) |>
  arrange(ski_area_name) |>
  select(ski_area_name, bearings) |>
  deframe() |>
  lapply(
    \(x) filter(x, num_bins == 32) |>
      mutate(
        color = if_else(bin_index == 2, "#f07178", "#004B59"),
      )
  )

colors <- c("#f07178", "#004B59", "#FFC857", "#36B37E", "#FF8C42", "#F4F1E9", "#8A9393", "#2A2D34")

bg_transparent <- function() {
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
  )
}
plot_rose <- function(dat, ski_area_name, size_title = 24, size_x = 20, highlight = FALSE, labels = NULL, type = NULL) {
  plot <- dat |>
    ggplot() +
    aes(x = bin_center, y = bin_count) +
    coord_radial(start = -pi / 32, expand = FALSE) +
    # coord_polar(start = -pi / 32) +
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
      axis.text.x = element_text(size = size_x, color = "#EBEBEB"),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      plot.title = element_text(hjust = 0.5, size = size_title, color = "#EBEBEB")
    )
  if (!is.null(type)) {
    plot <- plot + geom_col(fill = "#f07178") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
      )
  } else if (!highlight) {
    plot <- plot + geom_col(color = "#EBEBEB", fill = "#f07178")
  } else {
    plot <- plot +
      geom_col(color = "#EBEBEB", aes(fill = color)) +
      scale_fill_identity()
  }
  plot
}
whaleback <- bearings_ls[["Whaleback Mountain"]]
dartmouth_bears <- bearings_ls[["Dartmouth Skiway"]]
killington <- bearings_ls[["Killington Resort"]]

# NWbW


