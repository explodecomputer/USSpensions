


# Inputs

# Date of birth
# Single or joint annuity
# Future path of income
# Investment returns
# Male or female

library(tidyverse)
library(lubridate)


annuity_rates(sex="women", type="joint", 50)
income_projection(43600, 0.02, years=50, upper_limit=55000)


benefits <- pension_calculation(
	income=income_projection(43600, 0.02, years=50, upper_limit=55000), 
	annuity=annuity_rates("men", "joint", 50),
	employee_cont=0.08, 
	employer_cont=0.1325, 
	prudence = 67, 
	fund = "USS"
)

pension_summary(benefits, "06/09/1984")

