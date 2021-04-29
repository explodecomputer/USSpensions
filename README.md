# USSpensions
Calculations for USS pensions
This is a fork Gib's pension model in which I'm going to add an adjustment for inflation on the DB pension accrual. 

Steps
1. I'll calculate average inflation in the UK 1947-2015 using ONS data and a 0.7pp conversion between RPI and CPI.
2. Estimate mean and SD of inflation to get distribution
3. Create two distributions of inflation, a) a randomly drawn distribution of inflation, and b) historical inflation randomly reordered for future inflation.
4. Adjust the uprating of DB benefits by inflation each year. Current code uses incr variable to increment by 1 each year. This allows for DB benefits to increase in line with inflation. This is not techincally correct as the USS only increases up to 5% and then 50% of increases above 5% to 15%, giving a max increase of 10%. 


Theoretically we could use Bank of England forecasts of expected future inflation. But I think the SD of this will be way too low. So I won't consider it further. 

Potentially will add an option to allow people to set their own inflation assumptions.
