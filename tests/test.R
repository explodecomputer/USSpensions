


# Inputs

# Date of birth
# Single or joint annuity
# Future path of income
# Investment returns
# Male or female

library(tidyverse)
library(lubridate)

library(devtools)
load_all()

annuity_rates(sex="women", type="joint", 50)
income_projection(43600, 0.02, years=50, upper_limit=55000)


pension_calculation(
	income=income_projection(43600, 0.02, years=50, upper_limit=1000000), 
	annuity=annuity_rates("men", "joint", 50),
	employee_cont=0.08, 
	employer_cont=0.12, 
	prudence = 67, 
	fund = "USS"
) %>% as.data.frame() %>% pension_summary("1984-09-06")

