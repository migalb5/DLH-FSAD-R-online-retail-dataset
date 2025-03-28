SELECT SUM(Revenue) FROM transactions;

SELECT Description, StockCode, SUM(Revenue) FROM transactions GROUP BY Description, StockCode ORDER BY SUM(Revenue) DESC;

SELECT Country, SUM(Revenue) FROM transactions WHERE Revenue >= 0 GROUP BY Country ORDER BY SUM(Revenue) DESC LIMIT 5;

SELECT SUBSTR(InvoiceDateOnly, 1, 7), SUM(Revenue) FROM transactions WHERE Revenue >= 0
GROUP BY SUBSTR(InvoiceDateOnly, 1, 7)
ORDER BY SUBSTR(InvoiceDateOnly, 1, 7) ASC;

SELECT CustomerID, COUNT(CustomerID) FROM transactions GROUP BY CustomerID ORDER BY COUNT(CustomerID) DESC LIMIT 3;

SELECT Country, InvoiceYear, SUM(Revenue) FROM transactions WHERE Revenue >= 0
GROUP BY Country, InvoiceYear
ORDER BY Country, InvoiceYear ASC;

SELECT Description, StockCode, AVG(Revenue) FROM transactions WHERE Revenue >= 0
GROUP BY Description, StockCode
ORDER BY AVG(Revenue) DESC
LIMIT 6;

SELECT Description, AVG(Quantity) FROM transactions WHERE Revenue >= 0
GROUP BY Description
ORDER BY AVG(Quantity) DESC
LIMIT 3;

