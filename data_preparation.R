
library(readxl)
library(rio)
library(waiter)

local_file_path <- "data/online_retail_II.xlsx"

#use_waitress(color = "grey", percent_color = "black")

#waitress <- Waitress$new("#import")

df0910 <- readxl::read_excel(local_file_path, sheet = 1)
#waitress$inc(25)

df1011 <- readxl::read_excel(local_file_path, sheet = 2)
#waitress$inc(25)

retail_data <- rbind(df0910, df1011)
#waitress$inc(25)

retail_data <- retail_data %>%
  filter(!is.na("Customer ID")) %>%  # Remove missing customers
  mutate(Revenue = Quantity * Price) %>%  # Calculate revenue
  arrange(InvoiceDate) %>%  # Sorting revenue values (not required, just for data pre-visualization)
  mutate(InvoiceDateOnly = date(InvoiceDate), InvoiceYear = year(InvoiceDateOnly))
#waitress$inc(25)
#waitress$close()

rio::export(retail_data, "data/online_retail_II_prepared.csv", "csv") # data prepared = min.cleaning + sorted + some pre-calculations
