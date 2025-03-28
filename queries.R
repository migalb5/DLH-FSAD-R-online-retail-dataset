
library(RSQLite)
library(DBI)

db_connect <- function(path = "data/shiny_app.db") {
  DBI::dbConnect(RSQLite::SQLite(), path)
}

db_disconnect <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
    message("DB connection closed.")
  } else {
    message("DB connection already closed or not valid.")
  }
}

conn <- db_connect("data/retail_db.sqlite")

total_revenue = DBI::dbGetQuery(conn, "SELECT SUM(Revenue) FROM transactions")
print(total_revenue)

highest_revenue_product = dbGetQuery(conn, "SELECT Description, StockCode, SUM(Revenue) FROM transactions GROUP BY Description, StockCode ORDER BY SUM(Revenue) DESC")
print(highest_revenue_product)

top5_countries_sales = dbGetQuery(conn, "SELECT Country, SUM(Revenue) FROM transactions WHERE Revenue >= 0 GROUP BY Country ORDER BY SUM(Revenue) DESC LIMIT 5")
print(top5_countries_sales)

total_sales_per_day = dbGetQuery(conn, "SELECT InvoiceDateOnly, SUM(Revenue) FROM transactions WHERE Revenue >= 0 GROUP BY InvoiceDateOnly ORDER BY InvoiceDateOnly ASC")
print(total_sales_per_day)

highest_customer_transations = dbGetQuery(conn, "SELECT CustomerID, COUNT(CustomerID) FROM transactions GROUP BY CustomerID ORDER BY COUNT(CustomerID) DESC LIMIT 3")
print(highest_customer_transations)

conn <- db_disconnect(conn)

