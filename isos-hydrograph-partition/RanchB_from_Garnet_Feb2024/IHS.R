library(ggplot2)
library(plotly)


# Convert DateTime Match column to POSIXct if not already in that format
X2023_RB_IHS$`DateTime Match` <- as.POSIXct(X2023_RB_IHS$`DateTime Match`, format = "%m/%d/%y %H:%M")
X2023_RB_IHS$`DateTimeSample` <- as.POSIXct(X2023_RB_IHS$`DateTimeSample`, format = "%m/%d/%y %H:%M")

ggplot(X2023_RB_IHS, aes(x = `DateTime Match`, y = Discharge)) +
  geom_line(size = 0.5, color = "black") +  # Line plot for Discharge with width of 0.5
  geom_line(aes(x = DateTimeSample, y = Sample), size = 0.5, color = "blue") +  # Second line for Sample with width of 0.5 and blue color
  labs(x = "Date", y = "Discharge (cfs)") +
  ggtitle("Ranch Brook New Water 2023") +
  theme_classic() +
  scale_x_datetime(date_labels = "%m/%d", 
                   date_breaks = "5 day",  # Add ticks for each day
                   limits = as.POSIXct(c("2023-04-01 00:00", "2023-04-20 00:00"))) +
  theme(plot.title = element_text(hjust = 0.5))  # Centered plot title

# Convert DateTime Match column to POSIXct if not already in that format
X2023_RB_IHS$`DateTime Match` <- as.POSIXct(X2023_RB_IHS$`DateTime Match`, format = "%m/%d/%y %H:%M")
X2023_RB_IHS$`DateTimeSample` <- as.POSIXct(X2023_RB_IHS$`DateTimeSample`, format = "%m/%d/%y %H:%M")

plot_title <- "Ranch Brook New Water 2023"

# Plot the data
plot <- ggplot(X2023_RB_IHS, aes(x = `DateTime Match`, y = Discharge)) +
  geom_line(size = 0.5, color = "black") +  # Line plot for Discharge with width of 0.5
  geom_line(aes(x = DateTimeSample, y = Sample), size = 0.5, color = "blue") +  # Second line for Sample with width of 0.5 and blue color
  geom_ribbon(data = X2023_RB_IHS %>% filter(!is.na(Sample)),
              aes(x = DateTimeSample, ymin = 0, ymax = Sample),
              fill = "blue", alpha = 0.3) +  # Shaded area under the blue line
  labs(title = plot_title, x = "Date", y = "Discharge (cfs)") +
  theme_classic() +
  scale_x_datetime(date_labels = "%m/%d", 
                   date_breaks = "5 day",  # Add ticks for each day
                   limits = as.POSIXct(c("2023-02-01 00:00", "2023-04-20 00:00"))) +
  theme(plot.title = element_text(hjust = 0.5))  # Centered plot title

# Save the plot
ggsave(paste0("Library/CloudStorage/OneDrive-UniversityofVermont/CRREL/Ranch_Brook_2023_Analysis/IHS/", gsub(" ", "_", plot_title), "_plot.png"), plot = plot, device = "png", width = 10, height = 6, units = "in")


#Interactive Plot
plot_title <- "Ranch Brook New Water 2023"

p <- ggplot(X2023_RB_IHS, aes(x = `DateTime Match`, y = Discharge)) +
  geom_line(size = 0.5, color = "black") +  # Line plot for Discharge with width of 0.5
  geom_line(aes(x = DateTimeSample, y = Sample), size = 0.5, color = "blue") +  # Second line for Sample with width of 0.5 and blue color
  geom_ribbon(data = X2023_RB_IHS %>% filter(!is.na(Sample)),
              aes(x = DateTimeSample, ymin = 0, ymax = Sample),
              fill = "blue", alpha = 0.3) +  # Shaded area under the blue line
  labs(title = plot_title, x = "Date", y = "Discharge (cfs)") +
  theme_classic() +
  scale_x_datetime(date_labels = "%m/%d", 
                   date_breaks = "5 day",  # Add ticks for each day
                   limits = as.POSIXct(c("2023-02-01 00:00", "2023-04-20 00:00"))) +
  theme(plot.title = element_text(hjust = 0.5))  # Centered plot title

# Convert ggplot to plotly object
p_plotly <- ggplotly(p)

# Show the Plotly plot
p_plotly


