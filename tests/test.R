library(tidyverse)
library(lubridate)

library(devtools)
load_all()


nyears <- years_left("1984/09/06")
annuity <- annuity_rates(sex="women", type="joint", nyears)
income <- income_projection(55750.00, 0.04, years=nyears, upper_limit=1000000)
prudence = 67
fund = "USS"


pension_calculation(
	income=income, 
	annuity=annuity,
	prudence = prudence, 
	fund = fund
) %>% as.data.frame() %>% pension_summary("1984-09-06")


conts <- contributions_model(income)


library(tidyr)
library(ggplot2)

conts %>%
group_by(model) %>%
slice_tail(n=1) %>%
select(model, employee_cumsum, employee_tax_cumsum) %>%
pivot_longer(c(employee_tax_cumsum, employee_cumsum)) %>%
ggplot(aes(y=value, x=model)) +
geom_bar(stat="identity", aes(fill=name), position="dodge")

ggplot(., aes())



