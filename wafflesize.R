#Script to create a waffle chart where the size of the box and then number of the boxes is proportional to a data binding.
#Created by Dan Gunn
#2024

#libraries
library(tidyverse)

#First attemp
#dummy data
data <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(50, 30, 15, 5) # Proportions or sizes
)

# Normalize values to create proportional sizes
data <- data %>%
  mutate(
    proportion = value / sum(value),          # Calculate proportion
    num_tiles = round(proportion * 100)      # Scale to 100 total tiles
  )

# Generate grid data for the waffle chart
waffle_data <- data %>%
  uncount(num_tiles) %>% # Create a row for each tile
  mutate(
    row = rep(1:10, each = 10, length.out = n()),  # Set grid rows
    col = rep(1:10, times = 10, length.out = n()) # Set grid columns
  )

# Plot the waffle chart
ggplot(waffle_data, aes(x = col, y = row, fill = category)) +
  geom_tile(color = "white") +
  scale_y_reverse() +       # Flip Y-axis for typical waffle orientation
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(fill = "Category")  # Add legend for categories


# Add unequal tile sizes
waffle_data <- waffle_data %>%
  mutate(
    wproportion = value / sum(value),     # Calculate proportion
    width = sqrt(proportion),           # Map width to square root of proportion
    height = sqrt(proportion)  # Scale width relative to values
  )

#Strange result
ggplot(waffle_data, aes(x = col, y = row, fill = category)) +
  geom_tile(aes(width = width), color = "white") +
 # scale_y_reverse() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(fill = "Category")





# Input data
data2 <- data.frame(
  bucket_type = c("A", "B", "C"), #Category of bucket
  number = c(117, 1566, 2779),       # Number of buckets
  marbles = c(159000, 1071000, 663000), # Total marbles
  marbles_per_bucket = c(1358.97, 683.91, 238.57) # Marbles per bucket
)

#Calculate the size of each square
data2 <- data2 %>%
  mutate(
    square_size = sqrt(marbles / number)  # Size (width/height) of each square
  )

#Generate grid positions for each square
grid_data <- data2 %>%
  mutate(
    grid_size = ceiling(sqrt(number)),  # Precompute grid size
    square_positions = map(number, ~ tibble(
      x = rep(1:ceiling(sqrt(.x)), each = ceiling(sqrt(.x)))[1:.x],
      y = rep(1:ceiling(sqrt(.x)), times = ceiling(sqrt(.x)))[1:.x]
    ))
  ) %>%
  unnest(square_positions)  # Expand the list column into rows

# Step 3: Adjust positions for categories based on actual grid width
# Calculate the total grid width for each category
category_offsets <- data2 %>%
  mutate(
    grid_width = ceiling(sqrt(number)) * square_size,  # Total width of each category's grid
    x_offset = cumsum(lag(grid_width, default = 0) + max(square_size)), # Properly stagger categories
    y_offset = 0
  )

# Join offsets and resolve any potential duplicates
grid_data <- grid_data %>%
  left_join(category_offsets, by = "bucket_type") %>%
  mutate(
    square_size = coalesce(square_size.x, square_size.y),  # Resolve duplicated square_size
    x = (x - 1) * square_size + x_offset,                 # Scale and offset x
    y = (y - 1) * square_size + y_offset                  # Scale y
  ) %>%
  select(-square_size.x, -square_size.y)  # Remove duplicate columns

# Step 4: Add dimensions for `geom_rect`
grid_data <- grid_data %>%
  mutate(
    xmin = x,                   # Bottom-left x
    xmax = x + square_size,     # Top-right x
    ymin = y,                   # Bottom-left y
    ymax = y + square_size      # Top-right y
  )

custom_colors <- c("A" = "#05A137", # Red for A
                   "B" = "#3C7E79", # Blue for B
                   "C" = "#05ACA8") # Green for C

# Step 5: Plot the waffle chart
ggplot(grid_data) +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = bucket_type
    ),
    color = "white"
  ) +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  coord_fixed() +  # Keep square proportions
  theme_void() +   # Clean up the plot
  #theme(legend.position = "none") +   
  labs(fill = "Bucket Type")  # Add legend

#save
if (!dir.exists("charts")) dir.create("charts")  # Ensure the directory exists
ggsave(
  filename = "charts/waffle_chart.svg",   # Output file name
  plot = last_plot(),                    # Plot to save
  device = "svg",                        # Specify SVG as the device
  width = 10,                            # Width of the output file (in inches)
  height = 7                             # Height of the output file (in inches)
)



# Now with CIRCLES!
ggplot(grid_data) +
  geom_point(
    aes(
      x = x + square_size / 2,  # Center x for circles
      y = y + square_size / 2,  # Center y for circles
      size = square_size,       # Map size to square_size
      fill = bucket_type        # Map fill color to bucket_type
    ),
    color = "white",            # Border color for the points
    shape = 21                  # Circle shape with a fill
  ) +
  scale_fill_manual(values = custom_colors) +  # Map custom colors to bucket_type
  coord_fixed() +                              # Ensure equal aspect ratio
  theme_void() +                               # Clean up the plot
  theme(legend.position = "right") +           # Place the legend (or remove if not needed)
  labs(fill = "Bucket Type",                   # Add legend title
       size = "Square Size")                   # Optional: add legend for size


#save
if (!dir.exists("charts")) dir.create("charts")  # Ensure the directory exists
ggsave(
  filename = "charts/circle_waffle_chart.svg",   # Output file name
  plot = last_plot(),                    # Plot to save
  device = "svg",                        # Specify SVG as the device
  width = 10,                            # Width of the output file (in inches)
  height = 7                             # Height of the output file (in inches)
)


# Normalize the square_size for the plot
grid_data <- grid_data %>%
  mutate(
    scaled_size = scales::rescale(square_size, to = c(2, 10))  # Scale sizes to a visual range
  )

# Compute representative sizes for the legend
legend_sizes <- grid_data %>%
  group_by(bucket_type) %>%
  summarize(
    legend_size = mean(scaled_size),  # Use the scaled size for the legend
    .groups = "drop"
  )

# Plot with fixed scaling
ggplot(grid_data) +
  geom_point(
    aes(
      x = x + square_size / 2,  # Center x for circles
      y = y + square_size / 2,  # Center y for circles
      size = scaled_size,       # Use scaled sizes for the plot
      fill = bucket_type        # Map fill to bucket_type
    ),
    color = "white",            # Border color for the points
    shape = 21                  # Circle shape with a fill
  ) +
  scale_fill_manual(
    values = custom_colors,     # Map colors to bucket_type
    guide = "legend"            # Ensure the fill legend is included
  ) +
  scale_size_identity() +        # Use literal values for size
  guides(
    fill = guide_legend(
      override.aes = list(
        size = legend_sizes$legend_size  # Use the scaled sizes in the legend
      ),
      title = "Bucket Type & Size"
    )
  ) +
  coord_fixed() +                # Keep proportions
  theme_void() +                 # Simplify the plot
  theme(legend.position = "right") +  # Position legend
  labs(fill = "Bucket Type & Size")   # Unified legend title

#save
if (!dir.exists("charts")) dir.create("charts")  # Ensure the directory exists
ggsave(
  filename = "charts/circle_waffle_chart.svg",   # Output file name
  plot = last_plot(),                    # Plot to save
  device = "svg",                        # Specify SVG as the device
  width = 10,                            # Width of the output file (in inches)
  height = 7                             # Height of the output file (in inches)
)
