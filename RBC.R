library(tidyverse)
library(tidyquant)
library(timetk)
library(mFilter)
library(neverhpfilter)
library(stargazer)
library(ggtext)

# Get / Clean Data ----------------------------------------------------------------

fred_series <- c("GDPC1", "PCECC96", "GPDIC1", "AWHI", "AHETPI", "CPIAUCSL") # FRED series we are interested in

data <- lapply(fred_series, tq_get,    # Obtain FRED data as list of tibbles; returns a list of tibbles 
               get = "economic.data", 
               from = "1947-01-01", 
               to = "2020-09-01") %>%  
  lapply(pivot_wider,                  # Each tibble in the list has one variable ('symbol') corresponding to the data series in question (i.e. GDP), and one variable ('price') corresponding to the value of the variable, as well as a 'date' variable. The 'symbol' column is pretty useless (and we dont really want in our final data frame), while the 'price' variable is identical across dataframes in the list, so we replace the column name of 'price' with the values of the 'symbol' variable. Makes it easier to join later on. 
         id_cols = "date", 
         names_from = "symbol", 
         values_from = "price") %>%  
  reduce(full_join,                    # Join lists into one tibble, with observations based on date
         by = "date") %>%  
  tk_xts() %>%                                   # Convert to time series object
  rollapply(width=3,                             # Collapse to quarterly data by taking avg of every 3 rows
            FUN=function(x) mean(x, na.rm=TRUE), 
            by=3,                                # Means we only take rolling mean only for every distinct set of 3 observations, rather than shifting the window by only one obs. at a time
            by.column=TRUE, 
            align="left",                        # Means we use the date at the start of the window of 3 obs. 
            fill = NULL) %>%
  replace(is.nan(.), NA)              # Left with some NaNs because for some columns we are taking avg of 3 NAs; Convert them to NAs to make easier for later

colnames(data) <- c("gdp", "consumption", "investment", "hours", "wage_nom", "cpi_1982")  # Convert columns to something legible

data <- tk_tbl(data)                  # Convert back to tibble to use dplyr functions

data <- mutate(data,                  # Convert CPI to be indexed to 2012 dollars
               cpi_2012 = 100 * cpi_1982 / cpi_1982[data$index == "2012-01-01"], 
               .keep = "unused") %>% 
  mutate(wage = 100 * wage_nom / cpi_2012,       # Get real wages
         .keep = "unused") %>% 
  tk_xts()                            # Convert back to time series object


# Get Hodrick-Prescott Filter -------------------------------

hp_fun <- function(x) {       # Function to get HP Filter trend and cycle components as xts objects
  result <- hpfilter(100 * log(x[complete.cases(x)]), 
                     freq = 1600)
  output <- cbind.data.frame(trend = result[['trend']],
                             cycle = result[['cycle']])
  colnames(output) <- c("trend", "cycle")
  as.xts(output, dateFormat = "Date")
}

hp_list <- lapply(data, hp_fun) # Get list of HP filtered time series

hp_cycle <- do.call(cbind.xts, lapply(hp_list, '[', ,2)) # Bind together cycle component for each variable
colnames(hp_cycle) <- colnames(data)                     # Make column names legible again


# Get Hamilton Filter -----------------------------------------------------

ham_fun <- function(x) {    # Function to get Hamilton Filter as xts objects
  yth_filter(100*log(x), h = 8, p = 4)
}

ham_list <- lapply(data, ham_fun) # Get list of Hamilton filtered time series

ham_cycle <- do.call(cbind.xts, lapply(ham_list, '[', , 3))  # Bind together cycle component for each variable
colnames(ham_cycle) <- colnames(data)  # Make column names legible again


# Data Viz ! --------------------------------------------------------------

final_tbl <- cbind.data.frame(    # Summary Statistics table
  sd_hp = apply(hp_cycle, 2, sd, na.rm = TRUE),      # Standard Deviations of HP detrended data
  auto_hp = unlist(map(map(apply(hp_cycle, 2, acf, na.action = na.omit, plot = FALSE), 1), 2)), # First order autocorrelation of HP detrended data
  cor_hp = cor(hp_cycle, use = "pairwise.complete.obs")[,1],   # Correlation matrix of HP detrended data; first column selected in order to get correlations only with gdp
  sd_ham = apply(ham_cycle, 2, sd, na.rm = TRUE),             # Now repeat the above for Hamilton detrended data 
  auto_ham = unlist(map(map(apply(ham_cycle, 2, acf, na.action = na.omit, plot = FALSE), 1), 2)),
  cor_ham = cor(ham_cycle, use = "pairwise.complete.obs")[,1],
  corr_cross = diag(cor(ham_cycle, hp_cycle, use = 'pairwise.complete.obs'))  # Correlation matrix of HP vs Hamilton detrended date; select diagonal of matrix to get correlations between same variables
)

stargazer(final_tbl, type = 'html', out = 'table1.htm',  # Save summary table
          title = "Hodrick-Prescott vs Hamilton Filters", colnames = TRUE, 
          covariate.labels = c("", "Standard Deviation", "First Order Auto-Correlation", 
                               "Correlation with Output", "Standard Deviation", "First Order Auto-Correlation", 
                               "Correlation with Output", "Cross Correlation"))

# GDP Plot
gdp_plot <- ggplot() + 
  geom_function(data = hp_cycle, mapping = aes(x = Index, y = gdp), fun = ~0) +
  geom_line(data = hp_cycle, mapping = aes(x = Index, y = gdp), color = 'steelblue4', size = 1) + 
  geom_line(data = ham_cycle, mapping = aes(x = Index, y = gdp), color = 'plum3', size = 1) + 
  theme_minimal() +
  theme(plot.title = element_markdown(hjust = .5)) +
  labs(title = "<b style = 'color:steelblue4'>HP</b> and <b style = 'color:plum3'>Hamilton</b> Filters, Cyclical Components",
       x = 'Year',
       y = 'Log GDP',
       caption = 'Data: FRED(GDPC1) | Visualization: Jonathan Kabel | Code: github.com/Jonnie0')

# Consumption Plot
consumption_plot <- ggplot() + 
  geom_function(data = hp_cycle, mapping = aes(x = Index, y = consumption), fun = ~0) +
  geom_line(data = hp_cycle, mapping = aes(x = Index, y = consumption), color = 'steelblue4', size = 1) + 
  geom_line(data = ham_cycle, mapping = aes(x = Index, y = consumption), color = 'plum3', size = 1) + 
  theme_minimal() +
  theme(plot.title = element_markdown(hjust = .5)) +
  labs(title = "<b style = 'color:steelblue4'>HP</b> and <b style = 'color:plum3'>Hamilton</b> Filters, Cyclical Components",
       x = 'Year',
       y = 'Log Consumption',
       caption = 'Data: FRED(PCECC96) | Visualization: Jonathan Kabel | Code: github.com/Jonnie0')

# Wage Plot
wage_plot <- ggplot() + 
  geom_function(data = hp_cycle, mapping = aes(x = Index, y = wage), fun = ~0) +
  geom_line(data = hp_cycle, mapping = aes(x = Index, y = wage), color = 'steelblue4', size = 1) + 
  geom_line(data = ham_cycle, mapping = aes(x = Index, y = wage), color = 'plum3', size = 1) + 
  theme_minimal() +
  theme(plot.title = element_markdown(hjust = .5)) +
  labs(title = "<b style = 'color:steelblue4'>HP</b> and <b style = 'color:plum3'>Hamilton</b> Filters, Cyclical Components",
       x = 'Year',
       y = 'Log Real Hourly Wage (2012 Dollars)',
       caption = 'Data: FRED(AHETPI, CPIAUCSL) | Visualization: Jonathan Kabel | Code: github.com/Jonnie0')

# Save Plots
ggsave('cyclical_gdp.png', plot = gdp_plot, dpi = 300)  
ggsave('cyclical_consumption.png', plot = consumption_plot, dpi = 300)
ggsave('cyclical_wages.png', plot = wage_plot, dpi = 300)