library(tidyverse)


# Example data
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

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Example data
data <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(50, 30, 15, 5) # Values to scale sizes
)

# Normalize the values and calculate square dimensions
data <- data %>%
  mutate(
    proportion = value / sum(value),    # Calculate proportion
    size = sqrt(proportion),           # Scale dimensions based on square root of proportion
    area = proportion                  # Area proportional to value
  )

# Arrange squares in a compact grid
grid_layout <- data %>%
  mutate(
    x_start = lag(cumsum(size), default = 0), # Starting x positions
    y_start = cumsum(lag(size, default = 0)), # Adjust y positions to arrange in a grid
    x_end = x_start + size,                   # Ending x positions
    y_end = y_start + size                    # Ending y positions
  )

# Plot the scaled squares in a grid layout
ggplot(grid_layout) +
  geom_rect(
    aes(
      xmin = x_start,
      xmax = x_end,
      ymin = y_start,
      ymax = y_end,
      fill = category
    ),
    color = "white"
  ) +
  coord_fixed() +  # Maintain square proportions
  theme_void() +   # Clean up unnecessary plot elements
  labs(fill = "Category")


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Example data: replace this with your actual dataset
data <- data.frame(
  category = c("Federal", "State", "Local"),
  squares = c(117, 1566, 2779),  # Number of squares
  area = c(1358, 683, 238) # Total area for each category
)

# Step 1: Calculate size of each square
data <- data %>%
  mutate(
    square_size = sqrt(area / squares)  # Size (width and height) of each square
  )

# Step 2: Generate grid positions for each category
# We create a grid for each category using the number of squares and scaled size
grid_data <- data %>%
  rowwise() %>%
  mutate(
    square_positions = list(
      expand.grid(
        x = (1:ceiling(sqrt(squares))) * square_size,  # x positions based on square size
        y = (1:ceiling(sqrt(squares))) * square_size  # y positions based on square size
      )[1:squares, ] # Limit to the actual number of squares
    )
  ) %>%
  unnest(square_positions)

# Step 3: Add category to each square for plotting
grid_data <- grid_data %>%
  mutate(
    xmin = x - square_size,  # Bottom-left x coordinate
    xmax = x,                # Top-right x coordinate
    ymin = y - square_size,  # Bottom-left y coordinate
    ymax = y,                # Top-right y coordinate
    category = rep(data$category, data$squares) # Repeat categories based on number of squares
  )

# Step 4: Plot the scaled squares
ggplot(grid_data) +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = category
    ),
    color = "white"
  ) +
  coord_fixed() +  # Ensure squares are proportional
  theme_void() +   # Clean up the plot
  labs(fill = "Category")  # Add a legend for categories

# Example data: replace this with your actual dataset
data <- data.frame(
  category = c("Federal", "State", "Local"),
  squares = c(117, 1566, 2779),  # Number of squares
  area = c(1358, 683, 238) # Total area for each category
)

# Step 1: Calculate the size of each square
data <- data %>%
  mutate(
    square_size = sqrt(area / squares)  # Size (width/height) of each square
  )

# Step 2: Generate grid positions for each square within each category
grid_data <- data %>%
  rowwise() %>%
  mutate(
    square_positions = list(
      data.frame(
        x = rep(1:ceiling(sqrt(squares)), each = ceiling(sqrt(squares))),
        y = rep(1:ceiling(sqrt(squares)), times = ceiling(sqrt(squares)))
      )[1:squares, ] # Limit to the actual number of squares
    )
  ) %>%
  unnest(square_positions)

# Step 3: Adjust positions for categories to ensure compact layout
# Offset each category's starting position
category_offsets <- data %>%
  mutate(
    x_offset = cumsum(lag(sqrt(area), default = 0) + 1),  # Add padding between categories
    y_offset = 0                                         # All categories start from y=0
  )

grid_data <- grid_data %>%
  left_join(category_offsets, by = "category") %>%
  mutate(
    x = x * square_size.x + x_offset,  # Adjust x based on category offset
    y = y * square_size.x + y_offset   # Adjust y based on square size
  )

# Step 4: Add dimensions for `geom_rect`
grid_data <- grid_data %>%
  mutate(
    xmin = x - square_size.x,  # Bottom-left x
    xmax = x,                # Top-right x
    ymin = y - square_size.x,  # Bottom-left y
    ymax = y                 # Top-right y
  )

# Step 5: Plot the scaled waffle chart
ggplot(grid_data) +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = category
    ),
    color = "white"
  ) +
  coord_fixed() +  # Keep square proportions
  theme_void() +   # Clean up the plot
  labs(fill = "Category")  # Add legend

# Example data: replace this with your actual dataset
data <- data.frame(
  category = c("Federal", "State", "Local"),
  squares = c(117, 1566, 2779),  # Number of squares
  area = c(1358, 683, 238) # Total area for each category
)

# Step 1: Calculate the size of each square
data <- data %>%
  mutate(
    square_size = sqrt(area / squares)  # Size (width/height) of each square
  )

# Step 2: Generate grid positions for each square within each category
grid_data <- data %>%
  rowwise() %>%
  mutate(
    square_positions = list(
      data.frame(
        x = rep(1:ceiling(sqrt(squares)), each = ceiling(sqrt(squares))),
        y = rep(1:ceiling(sqrt(squares)), times = ceiling(sqrt(squares)))
      )[1:squares, ] # Limit to the actual number of squares
    )
  ) %>%
  unnest(square_positions)

# Step 3: Adjust positions for categories to ensure compact layout
# Offset each category's starting position
category_offsets <- data %>%
  mutate(
    x_offset = cumsum(lag(ceiling(sqrt(squares)), default = 0) * max(square_size) + max(square_size)), 
    y_offset = 0
  )

grid_data <- grid_data %>%
  left_join(category_offsets, by = "category") %>%
  mutate(
    x = (x - 1) * square_size.x + x_offset,  # Scale and offset x
    y = (y - 1) * square_size.x + y_offset   # Scale y
  )

# Step 4: Add dimensions for `geom_rect`
grid_data <- grid_data %>%
  mutate(
    xmin = x,                   # Bottom-left x
    xmax = x + square_size.x,     # Top-right x
    ymin = y,                   # Bottom-left y
    ymax = y + square_size.x      # Top-right y
  )

# Step 5: Plot the scaled waffle chart
ggplot(grid_data) +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = category
    ),
    color = "white"
  ) +
  coord_fixed() +  # Keep square proportions
  theme_void() +   # Clean up the plot
  labs(fill = "Category")  # Add legend


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Input data
data <- data.frame(
  facility_type = c("A", "B", "C"),
  number = c(117, 1566, 2779),       # Number of buckets
  prisoners = c(159000, 1071000, 663000), # Total 
  prisoners_per_facility = c(1358.97, 683.91, 238.57) # Number per bucket
)

# Step 1: Calculate the size of each square
data <- data %>%
  mutate(
    square_size = sqrt(prisoners / number)  # Size (width/height) of each square
  )

# Step 2: Generate grid positions for each square
grid_data <- data %>%
  rowwise() %>%
  mutate(
    square_positions = list(
      data.frame(
        x = rep(1:ceiling(sqrt(number)), each = ceiling(sqrt(number))),
        y = rep(1:ceiling(sqrt(number)), times = ceiling(sqrt(number)))
      )[1:number, ]  # Limit to the actual number of squares
    )
  ) %>%
  unnest(square_positions)

category_offsets <- data %>%
  mutate(
    x_offset = cumsum(lag(ceiling(sqrt(number)) * max(square_size), default = 0) + max(square_size)),
    y_offset = 0
  )

grid_data <- grid_data %>%
  left_join(category_offsets, by = "category") %>%
  mutate(
    x = (x - 1) * square_size.x + x_offset,  # Scale and offset x
    y = (y - 1) * square_size.x + y_offset   # Scale y
  )

# Step 4: Add dimensions for `geom_rect`
grid_data <- grid_data %>%
  mutate(
    xmin = x,                   # Bottom-left x
    xmax = x + square_size.x,     # Top-right x
    ymin = y,                   # Bottom-left y
    ymax = y + square_size.x      # Top-right y
  )

# Step 5: Plot the waffle chart
ggplot(grid_data) +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = facility_type
    ),
    color = "white"
  ) +
  coord_fixed() +  # Keep square proportions
  theme_void() +   # Clean up the plot
  labs(fill = "Facility Type")  # Add legend

ggsave(
  filename = "charts/waffle_chart.svg",   # Output file name
  plot = last_plot(),             # Plot to save (can replace with a specific ggplot object)
  device = "svg",                 # Specify SVG as the device
  width = 10,                     # Width of the output file (in inches)
  height = 7                      # Height of the output file (in inches)
)


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Input data
data <- data.frame(
  facility_type = c("A", "B", "C"),
  number = c(117, 1566, 2779),       # Number of buckets
  prisoners = c(159000, 1071000, 663000), # Total
  prisoners_per_facility = c(1358.97, 683.91, 238.57) # Number per bucket
)

# Step 1: Calculate the size of each square
data <- data %>%
  mutate(
    square_size = sqrt(prisoners / number)  # Size (width/height) of each square
  )

# Step 2: Generate grid positions for each square
grid_data <- data %>%
  rowwise() %>%
  mutate(
    square_positions = list(
      data.frame(
        x = rep(1:ceiling(sqrt(number)), each = ceiling(sqrt(number))),
        y = rep(1:ceiling(sqrt(number)), times = ceiling(sqrt(number)))
      )[1:number, ]  # Limit to the actual number of squares
    )
  ) %>%
  unnest(square_positions)

# Step 3: Adjust positions for categories based on actual grid width
# Calculate the total grid width for each category
category_offsets <- data %>%
  mutate(
    grid_width = ceiling(sqrt(number)) * square_size,  # Total width of each category's grid
    x_offset = cumsum(lag(grid_width, default = 0) + max(square_size)), # Properly stagger categories
    y_offset = 0
  )

# Join offsets and resolve any potential duplicates
grid_data <- grid_data %>%
  left_join(category_offsets, by = "facility_type") %>%
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
      fill = facility_type
    ),
    color = "white"
  ) +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  coord_fixed() +  # Keep square proportions
  theme_void() +   # Clean up the plot
  theme(legend.position = "none") +   
  labs(fill = NULL)  # Add legend


# ggplot(grid_data) +
#   geom_point(
#     aes(
#       x = x + square_size / 2,  # Center x for circles
#       y = y + square_size / 2,  # Center y for circles
#       color = facility_type
#     ),
#     size = 5,                   # Size of circles (adjust as needed)
#     shape = 21,                 # Circle shape with fill
#     fill = custom_colors[grid_data$facility_type]  # Fill color
#   ) +
#   scale_color_manual(values = custom_colors) +  # Apply custom colors
#   coord_fixed() +  # Keep proportions
#   theme_void() +   # Clean up the plot
#   labs(color = "Facility Type")  # Add legend for categories

# Step 6: Save the waffle chart as an SVG
if (!dir.exists("charts")) dir.create("charts")  # Ensure the directory exists
ggsave(
  filename = "charts/waffle_chart.svg",   # Output file name
  plot = last_plot(),                    # Plot to save
  device = "svg",                        # Specify SVG as the device
  width = 10,                            # Width of the output file (in inches)
  height = 7                             # Height of the output file (in inches)
)
