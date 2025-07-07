# ===============================
#     RESTAURANT DATA ANALYSIS 
# ==============================



#    Load libraries
library(GGally)  #for pairplot
library(ggplot2)
library(gganimate) #for annimation
library(dplyr)
library(ggstream)
library(gifski)
library(gridExtra)
library(ggcorrplot)
library(knitr)
library(plotrix)
library(gganimate)
library(plotly)

library(maps)
library(fmsb)





# ===============================
#   Load and Explore the Dataset
# ===============================
zomato <- read.csv("ZomatoData.csv", stringsAsFactors = FALSE)

# Basic structure
kable(head(zomato[, c("Restaurant.Name", "City", "Cuisines", "Rating")], 5))
kable(tail(zomato[, c("Restaurant.Name", "City", "Cuisines", "Rating")], 5))
dim(zomato)
names(zomato)
str(zomato)
#formatting the str command
glimpse(zomato)  
# to understand data of str(zomato) we can use next two
table(sapply(zomato, class))
kable(sapply(zomato, class))
summary(zomato)

# ===============================
#         Data Cleaning
# ===============================
#
# Check for missing values before cleaning
cat("Total missing values before cleaning:", sum(is.na(zomato)), "\n")
kable(colSums(is.na(zomato)))

# Clean the data by removing rows with missing values
zomato_cleaned <- na.omit(zomato)

# Check for missing values after cleaning (on the cleaned dataset)
cat("Total missing values after cleaning:", sum(is.na(zomato_cleaned)), "\n")
kable(colSums(is.na(zomato_cleaned)))

# Check for duplicate rows before cleaning
cat("Total duplicate rows before cleaning:", sum(duplicated(zomato)), "\n")
duplicated_rows_before <- zomato[duplicated(zomato), ]
kable(duplicated_rows_before)

# Remove duplicate rows
zomato_cleaned <- zomato[!duplicated(zomato), ]

# Check for duplicate rows after cleaning (on the cleaned dataset)
cat("Total duplicate rows after cleaning:", sum(duplicated(zomato_cleaned)), "\n")
duplicated_rows_after <- zomato_cleaned[duplicated(zomato_cleaned), ]
kable(duplicated_rows_after)





# ===============================
#       Data Transformation
# ===============================
# Convert 'Has.Table.booking' to numeric
zomato$Has.Table.booking.numeric <- as.numeric(as.factor(zomato$Has.Table.booking))
zomato$Has.Table.booking.numeric

# Standardize numeric columns
zomato_scaled <- as.data.frame(scale(zomato[, c("Votes", "Average.Cost.for.two", "Rating")]))
kable(head(zomato_scaled, 5))

# ===============================
# Outliers Detection (IQR Method)
# ===============================
find_outliers <- function(data) {
  Q1 <- quantile(data, 0.25)
  Q3 <- quantile(data, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(data[data < lower_bound | data > upper_bound])
}

outliers_rating <- find_outliers(zomato$Rating)
cat("Outlier count in Ratings:", length(outliers_rating), "\n")
zomato[zomato$Rating %in% outliers_rating, c("Restaurant.Name", "Rating")]

# ===============================
#       Correlation Analysis
# ===============================
# Pairwise correlation
cor(zomato$Votes, zomato$Rating, use = "complete.obs")
cor(zomato[, c("Votes", "Average.Cost.for.two", "Rating", "Price.range")], use = "complete.obs")
numeric_cols <- zomato[, c("Votes", "Average.Cost.for.two", "Price.range", "Rating")]
corr <- round(cor(numeric_cols, use = "complete.obs"), 2)

ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           colors = c("orchid", "white", "seagreen"),
           title = "Correlation Matrix",
           ggtheme = theme_minimal())

# ===============================
#       Summary Statistics
# ===============================
summary_stats <- sapply(zomato, function(x) {
  if (is.numeric(x)) {
    c(Min = min(x, na.rm = TRUE), 
      Mean = mean(x, na.rm = TRUE), 
      Max = max(x, na.rm = TRUE))
  } else {
    c(Levels = length(unique(x)))
  }
})
kable(print(summary_stats, caption = "Summary Statistics for Zomato Data"))

# ===============================
#       Regression Models
# ===============================
# Simple Linear Regression
SLM <- lm(Rating ~ Votes, data = zomato, na.action = na.omit)
summary(SLM)

# Multiple Linear Regression
MLM <- lm(Rating ~ Votes + Average.Cost.for.two + Price.range, data = zomato, na.action = na.omit)
summary(MLM)

# ===============================
#         Visualizations
# ===============================


# Histogram of Ratings
table(zomato$Rating)
summary(zomato$Rating)
unique(zomato$Rating)
#hist(zomato$Rating, col = "coral", xlab = "Rating", main = "Histogram of Restaurant Ratings")
ggplot(zomato, aes(x = Rating)) +
  geom_histogram(binwidth = 0.3, fill = "coral", color = "black") +
  ggtitle("Histogram of Restaurant Ratings") + xlab("Ratings") + ylab("No. of Restaurants")

#piechart
top_cuisines <- sort(table(zomato$Cuisines), decreasing = TRUE)[1:5]
# Specify colors
colors <- c("#8da0cb", "#fc8d62", "#66c2a5", "#e78ac3", "#a6d854")
# Create labels for the pie chart
labels <- names(top_cuisines)
# Create a 3D pie chart with customized colors and labels
pie3D(top_cuisines, labels = labels, main = "Top 5 Cuisines", explode = 0.1, col = colors, labelcex = 0.8)



# Boxplots by Price Range
par(mfrow = c(1, 3))
boxplot(Votes ~ Price.range, data = zomato, col = "lightpink3", main = "Votes by Price")
boxplot(Rating ~ Price.range, data = zomato, col = "skyblue", main = "Rating by Price")
boxplot(Average.Cost.for.two ~ Price.range, data = zomato, col = "lightgreen", main = "Cost by Price")
par(mfrow = c(1, 1))

# Scatter plot: Rating vs Votes
ggplot(zomato, aes(x = Votes, y = Rating)) +
  geom_point(color = "tomato") +
  geom_smooth(method = 'lm', se = FALSE, color = "darkgreen") +
  ggtitle("Votes vs Rating") +
  xlab("Votes") + ylab("Rating")


#colnames(zomato)
#Pair Plots of Average.Cost.for.two , Rating , Votes , Price.range
# Remove rows with missing data
zomato <- na.omit(zomato)
ggpairs(
  zomato[, c("Average.Cost.for.two", "Rating", "Votes", "Price.range")],
  mapping = aes(color = as.factor(Price.range)),
  upper = list(continuous = wrap("cor", size = 3)),
  lower = list(continuous = wrap("points", size = 0.5))
) +
  ggtitle("Pair Plot Colored by Price Range") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 12),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),  # rotate x-axis labels
    axis.text.y = element_text(size = 7)  # shrink y-axis labels
  )




# heatmap  by price range
heatmap_data <- zomato %>%
  group_by(City, Price.range) %>%
  summarise(AverageRating = mean(Rating, na.rm = TRUE)) %>%
  ungroup()

# Plot the heatmap
gg <- ggplot(heatmap_data, aes(x = factor(Price.range), y = City, fill = AverageRating)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkred", na.value = "grey80") +
  labs(title = "Average Rating by City and Price Range",
       x = "Price Range", y = "City", fill = "Avg Rating") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 5),  # Adjust angle and size
        axis.text.x = element_text(angle = 0, hjust = 1, size = 8))  # Rotate x-axis labels for better spacing

# Display the plot
gg

#Heatmap of Cuisine by Rating
heatmap_data <- zomato %>%
  group_by(City, Rating) %>%
  summarise(AverageRating = mean(Rating, na.rm = TRUE)) %>%
  ungroup()

# Plot the heatmap
gg <- ggplot(heatmap_data, aes(x = factor(Rating), y = City, fill = AverageRating)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "darkblue", high = "green", na.value = "grey80") +
  labs(title = "Average Rating by City and Rating",
       x = "Rating", y = "City", fill = "Avg Rating") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 5),  # Adjust angle and size
        axis.text.x = element_text(angle = 0, hjust = 1, size = 8))  # Rotate x-axis labels for better spacing

# Display the plot
gg


#Time Series Plot for Rating Trends Over Price Range
ggplot(zomato, aes(x = Price.range, y = Rating)) +
  geom_line(stat = "summary", fun = "mean", aes(group = 1), size = 1.5, color = "blue") +  # Adjust size and color
  labs(title = "Rating Trends Over Price Range", x = "Price Range", y = "Average Rating") +
  theme_minimal()





#Bar Plot for Top 5 Cuisines by Number of Restaurants
# Count the occurrences of each cuisine
top_cuisines_count <- head(sort(table(zomato$Cuisines), decreasing = TRUE), 5)

# Convert the table into a data frame
top_cuisines_df <- data.frame(Cuisine = names(top_cuisines_count), Count = as.vector(top_cuisines_count))

# Plot the bar chart
ggplot(top_cuisines_df, aes(x = reorder(Cuisine, Count), y = Count, fill = Cuisine)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axis to make the labels easier to read
  labs(title = "Top 5 Cuisines by Restaurant Count", x = "Cuisine", y = "Count") +
  theme_minimal()


#Bubble Chart: Votes vs Average Cost for Two
ggplot(zomato, aes(x = Votes, y = Average.Cost.for.two, size = Rating)) +
  geom_point(color = "pink", alpha = 0.6) +
  labs(title = "Bubble Chart: Votes vs Cost for Two", x = "Votes", y = "Average Cost for Two") +
  theme_minimal()



# Histogram with mean line
ggplot(zomato, aes(x = Rating)) +
  geom_histogram(binwidth = 0.3, fill = "orange", color = "black") +
  geom_vline(aes(xintercept = mean(Rating, na.rm = TRUE)), linetype = "dashed", color = "red") +
  ggtitle("Distribution of Ratings") + xlab("Rating") + ylab("Frequency")

# Density plot by Price Range
ggplot(zomato, aes(x = Rating, fill = as.factor(Price.range))) +
  geom_density(alpha = 0.5) +
  theme(legend.title = element_blank()) +
  labs(title = "Rating Density by Price Range", x = "Rating", y = "Density")

# Violin plot: Cost vs Price Range
ggplot(zomato, aes(x = as.factor(Price.range), y = Average.Cost.for.two, fill = as.factor(Price.range))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  labs(title = "Cost for Two by Price Range", x = "Price Range", y = "Cost")

# Correlation Matrix
numeric_cols <- zomato[, c("Votes", "Average.Cost.for.two", "Price.range", "Rating")]
corr <- round(cor(numeric_cols, use = "complete.obs"), 2)
ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           colors = c("orchid", "white", "seagreen"),
           title = "Correlation Matrix",
           ggtheme = theme_minimal())

# interactive plot
interactive_plot <- plot_ly(
  data = zomato,
  x = ~Votes,
  y = ~Rating,
  type = 'scatter',
  mode = 'markers',
  color = ~as.factor(Price.range),
  text = ~paste("Restaurant:", Restaurant.Name,
                "<br>City:", City,
                "<br>Cuisine:", Cuisines,
                "<br>Cost for Two:", Average.Cost.for.two),
  marker = list(size = 10, opacity = 0.6)
) %>%
  layout(title = "Interactive: Votes vs Rating",
         xaxis = list(title = "Votes"),
         yaxis = list(title = "Rating"))

interactive_plot

#2

# Filter top cities and cuisines
top_cities <- names(sort(table(zomato$City), decreasing = TRUE))[1:5]
top_cuisines <- names(sort(table(zomato$Cuisines), decreasing = TRUE))[1:5]

# Create streamgraph-style dataset
zomato_stream <- zomato %>%
  filter(City %in% top_cities, Cuisines %in% top_cuisines) %>%
  mutate(Time = as.numeric(Price.range)) %>%
  group_by(Time, Cuisines) %>%
  summarise(Count = n(), .groups = 'drop')

# Create streamgraph-style plot
ggplot(zomato_stream, aes(x = Time, y = Count, fill = Cuisines)) +
  geom_stream(type = "ridge", bw = 0.75, extra_span = 0.1) +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Cuisine Distribution Flow Across Price Ranges",
       x = "Price Range (simulated time)",
       y = "Restaurant Count",
       fill = "Cuisine") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

#3 a better version of above one and an animation

# Filter top cities and cuisines to keep the plot readable
top_cities <- names(sort(table(zomato$City), decreasing = TRUE))[1:5]
top_cuisines <- names(sort(table(zomato$Cuisines), decreasing = TRUE))[1:5]

zomato_alluvial <- zomato %>%
  filter(City %in% top_cities, Cuisines %in% top_cuisines) %>%
  group_by(City, Cuisines, Price.range) %>%
  summarise(Count = n()) %>%
  ungroup()






# Alluvial chart: City → Cuisine → Price Range
ggplot(zomato_alluvial,
       aes(axis1 = City, axis2 = Cuisines, axis3 = as.factor(Price.range), y = Count)) +
  scale_x_discrete(limits = c("City", "Cuisine", "Price Range"), expand = c(.1, .05)) +
  geom_alluvium(aes(fill = City), width = 1/12, alpha = 0.8) +
  geom_stratum(width = 1/8, fill = "gray90", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  theme_minimal() +
  ggtitle("Zomato Cuisine Flow by City and Price Range") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

# Add pseudo-time phase by Price Range
zomato_animate <- zomato %>%
  filter(City %in% top_cities, Cuisines %in% top_cuisines) %>%
  mutate(Phase = paste0("Price Level ", Price.range)) %>%
  group_by(Phase, City, Cuisines) %>%
  summarise(Count = n(), .groups = 'drop')

# Animate how cuisine popularity shifts across cities at each price level
anim <- ggplot(zomato_animate,
               aes(x = City, y = Count, fill = Cuisines)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Cuisines, scales = "free_y") +
  labs(title = 'Cuisine Trends Across Cities by Price Level',
       subtitle = 'Phase: {closest_state}', y = "Restaurant Count", x = "City") +
  theme_minimal(base_size = 14) +
  transition_states(Phase, transition_length = 2, state_length = 2) +
  ease_aes('sine-in-out')







# Render animation
animate(anim, nframes = 50, fps = 10, width = 800, height = 500)
