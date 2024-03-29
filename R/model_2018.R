# USS pension model in a web app
# Copyright (C) 2018 Gibran Hemani

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.



#' Calculation for investment returns
#' 
#' @export
#' @return Data frame for different schemes and over time
investment_returns <- function()
{
	rbind(
		dplyr::tibble(
			Prudence=50,
			Fund="USS",
			Year=2018:2067,
			growth=rep(c(0.0097, 0.0403, 0.025, 0.03, 0.03), each=10)
		),
		dplyr::tibble(
			Prudence=50,
			Fund="Growth fund",
			Year=2018:2067,
			growth=rep(c(0.0141, 0.0381, 0.0261, 0.03, 0.03), each=10)
		),
		dplyr::tibble(
			Prudence=50,
			Fund="Moderate growth fund",
			Year=2018:2067,
			growth=rep(c(0.0074, 0.032, 0.0197, 0.0238, 0.0238), each=10)
		),
		dplyr::tibble(
			Prudence=50,
			Fund="Cautious growth fund",
			Year=2018:2067,
			growth=rep(c(0, 0.0268, 0.0134, 0.0178, 0.0178), each=10)
		),
		dplyr::tibble(
			Prudence=50,
			Fund="Cash fund",
			Year=2018:2067,
			growth=rep(c(-0.011, -0.0029, -0.007, -0.0056, -0.0056), each=10)
		),
		dplyr::tibble(
			Prudence=67,
			Fund="USS",
			Year=2018:2067,
			growth=rep(c(-0.0053, 0.028, 0.017, 0.017, 0.017), each=10)
		),
		dplyr::tibble(
			Prudence=67,
			Fund="Growth fund",
			Year=2018:2067,
			growth=rep(c(-0.001, 0.0258, 0.0181, 0.0166, 0.0166), each=10)
		),
		dplyr::tibble(
			Prudence=67,
			Fund="Moderate growth fund",
			Year=2018:2067,
			growth=rep(c(-0.0076, -0.0197, -0.0117, -0.0104, -0.0104), each=10)
		),
		dplyr::tibble(
			Prudence=67,
			Fund="Cautious growth fund",
			Year=2018:2067,
			growth=rep(c(-0.015, 0.0145, 0.0054, 0.0044, 0.0044), each=10)
		),
		dplyr::tibble(
			Prudence=67,
			Fund="Cash fund",
			Year=2018:2067,
			growth=rep(c(-0.026, -0.0152, -0.015, -0.019, -0.019), each=10)
		)
	)
}

#' Income calculator
#' 
#' Given a starting income and an annual growth, will return projected income for given number of years. Upper limit can be set also.
#' 
#' @param start Starting income
#' @param increase Percentage increase
#' @param increases_per_year Number of increases per year. Default 1
#' @param years Number of years to calculate
#' @param upper_limit Maximum income. Default 1000000
#' @export
#' @return vector of incomes
income_projection <- function(start, increase, increases_per_year=1, years, upper_limit=1000000)
{
	c(start, pmin(start * (1 + increase)^(1:(years-1)), upper_limit))
}

#' Savings calculator
#' 
#' Given a total savings target calculalate how much per 
#' year needed to achieve target savings total
#'  
#' @param amount Total savings target
#' @param years Number of years to calculate
#' @param prudence Parameter for \code{investment_returns}, 50 or 65
#' @param fund Parameter for \code{investment_returns}, "USS", "Growth fund", "Moderate growth fund", "Cautious growth fund", or "Cash fund"
#' @export
#' @return Dollars needed to save per year. 
savings_required_constant <- function(amount,years,prudence,fund)
{
  
#savings <- investment_savings_total(10000,years,prudence,fund)
 #
requiredSavingsPerYear = optimize(constantCostFunction,c(0, 2*amount/years),amount,years,prudence,fund)

return(as.numeric(requiredSavingsPerYear[1]))
}


#' Savings calculator
#' 
#' Given a total savings target calculalate how much per 
#' year needed to achieve target savings total
#'  
#' @param amount Total savings target
#' @param income Projected income each year
#' @param prudence Parameter for \code{investment_returns}, 50 or 65
#' @param fund Parameter for \code{investment_returns}, "USS", "Growth fund", "Moderate growth fund", "Cautious growth fund", or "Cash fund"
#' @export
#' @return Dollars needed to save per year. 
savings_required_percentage <- function(amount,income,prudence,fund)
{
  
  #savings <- investment_savings_total(10000,years,prudence,fund)
  #
  requiredSavingsPerYear = optimize(percentageCostFunction,c(0, 1),amount,income,prudence,fund)
  
  return(as.numeric(requiredSavingsPerYear[1]))
}
#' Savings calculator
#' 
#' Given constant amount saved per year calculate total saved at end of years 
#'  
#' @param savedPerYear Constant amount saved per year
#' @param years Number of years to calculate
#' @param prudence Parameter for \code{investment_returns}, 50 or 65
#' @param fund Parameter for \code{investment_returns}, "USS", "Growth fund", "Moderate growth fund", "Cautious growth fund", or "Cash fund"
#' @export
#' @return Dollars needed to save per year. 
investment_savings_total <- function(savedPerYear,years,prudence,fund)
{
  
  ret <- subset(investment_returns(), Prudence==prudence & Fund==fund)$growth
  

  totalSavings = numeric(years)
  totalSavings[1] <- savedPerYear
  for(i in 2:years)
  {
    totalSavings[i] <- totalSavings[i-1] * (1 + ret[i]) + savedPerYear
  }
  
  return(totalSavings[years])
}

#' Savings calculator
#' 
#' Given percent of salary saved per year calculate total saved at end of years 
#'  
#' @param savedPerYear Percentage saved per year
#' @param income vector of projected income values for each year
#' @param prudence Parameter for \code{investment_returns}, 50 or 65
#' @param fund Parameter for \code{investment_returns}, "USS", "Growth fund", "Moderate growth fund", "Cautious growth fund", or "Cash fund"
#' @export
#' @return Dollars needed to save per year. 
percentage_savings_total <- function(savedPerYear,income,prudence,fund)
{

  ret <- subset(investment_returns(), Prudence==prudence & Fund==fund)$growth
 

    totalSavings = numeric(length(income))
  totalSavings[1] <- savedPerYear*income[1]
  
  for(i in 2:length(income))
  {
    totalSavings[i] <- totalSavings[i-1] * (1 + ret[i]) + savedPerYear*income[i]
  }
  

  return(totalSavings[length(income)])
}

#' Cost function for constant amount
#' 
#' This function is the cost function used for determining how much is needed 
#' to be saved as a constant amount per year. 
#'  
#' @param savedPerYear Total savings target
#' @param years Number of years to calculate
#' @param prudence Parameter for \code{investment_returns}, 50 or 65
#' @param fund Parameter for \code{investment_returns}, "USS", "Growth fund", "Moderate growth fund", "Cautious growth fund", or "Cash fund"
#' @export
#' @return Dollars needed to save per year. 
constantCostFunction <- function(savedPerYear,targetTotal,years,prudence,fund)
{
  totalSavings = investment_savings_total(savedPerYear,years,prudence,fund)
  
  targetError = sum((totalSavings-targetTotal)^2)
  
  return(targetError)
}


#' Cost function for percetage saved
#' 
#' This function is the cost function used for determining how much is needed 
#' to be saved as a percentage of income each year. 
#'  
#' @param savedPerYear Percentage saved each year
#' @param targetTotal  Target total savings amount
#' @param income Income earned in each year
#' @param prudence Parameter for \code{investment_returns}, 50 or 65
#' @param fund Parameter for \code{investment_returns}, "USS", "Growth fund", "Moderate growth fund", "Cautious growth fund", or "Cash fund"
#' @export
#' @return Dollars needed to save per year. 
percentageCostFunction <- function(savedPerYear,targetTotal,income,prudence,fund)
{
  totalSavings = percentage_savings_total(savedPerYear,income,prudence,fund)
  
  targetError = sum((totalSavings-targetTotal)^2)
  
  return(targetError)
}
#' Calculate annuity rates
#' 
#' Different rates for sex, joint vs single, and expected annual life expectancy increase
#' 
#' @param sex "men" or "women"
#' @param type "single" or "joint"
#' @param years number of years to calculate
#' @param le_increase Expected increase in life expectancy per year. Default 0.015
#' 
#' @export
#' @return vector of annuities
annuity_rates <- function(sex, type, years, le_increase=0.015)
{
	annuities_quote <- expand.grid(age=65:68, Type=c("single", "joint"), Sex=c("men", "women"))
	annuities_quote$val <- c(
		3064.00, 3006.00, 3126.00, 3272.00, 
		2565.00, 2647.00, 2733.00, 2778.00, 
		3064.00, 3006.00, 3126.00, 3272.00, 
		2778.00, 2838.00, 2928.00, 3021.00
	)

	annuity <- rep(0, years)
	annuity[1] <- subset(annuities_quote, Sex==sex & Type==type & age==65)$val

	for(i in 2:years)
	{
		annuity[i] <- annuity[i-1] / (1 + le_increase)
		if(i == 3)
		{
			annuity[i] <- subset(annuities_quote, Sex==sex & Type==type & age==66)$val / (1 + le_increase)^(i-1)
		} else if(i == 10){
			annuity[i] <- subset(annuities_quote, Sex==sex & Type==type & age==67)$val / (1 + le_increase)^(i-1)
		} else if(i == 11){
			annuity[i] <- annuity[i-1] / (1 + le_increase) * 1.15
		} else if(i == 28){
			annuity[i] <- subset(annuities_quote, Sex==sex & Type==type & age==68)$val / (1 + le_increase)^(i-1) * 1.13
		}
	}
	return(annuity)
}


#' Calculate pension with combined DB and DC components
#'
#' @param ret Investment returns per year
#' @param annuity output from \code{annuity_rates}
#' @param income output from \code{income_projection}
#' @param employee_cont Percentage of salary contributed by employee
#' @param employer_cont Percentage of salary contributed by employer
#' @param prop_salary Proportion of salary contributing to DB
#' @param db_cutoff Salary cutoff from DB to DC
#' @param incr Either a column name from inflation_data, or a numeric value
#' @param infl_assump DB revaluing inflation assumption \code{inflation_incr} 
#' @param mult DB Parameter
#'
#' @export
#' @return Tibble
calc_db_dc <- function(ret, annuity, income, employee_cont, employer_cont, prop_salary, db_cutoff, incr, infl_assump, mult, dato=NULL)
{ 

	nyears <- length(income)

	if(is.numeric(incr))
	{
		stopifnot(length(incr) == 1)
		incr <- rep(incr, nyears)
	} else {
		stopifnot(incr %in% names(inflation_data))
		incr <- inflation_data[[incr]][1:nyears]
	}

	dat <- dplyr::tibble(
		ret = ret,
		annuity = annuity,
		income = income,
		income_thresh = pmin(income, db_cutoff),
		income_dc = income - income_thresh,
		db_pension = c(income_thresh[1] * prop_salary, rep(0, nyears-1)),
		dc_pot_thresh = 0,
		incr = incr
	)

	if(!is.null(dato))
	{
		ind <- c(1:nyears)[-c(1:nrow(dato))]
		dat <- bind_rows(dato, dat[ind,])
		start <- nrow(dato)+1
	} else {
		ind <- c(1:nyears)
		start <- 2
	}

	for(i in start:nyears)
	{
		dat$db_pension[i] <- dat$db_pension[i-1] * dat$incr[i] + dat$income_thresh[i] * prop_salary
	}
	dat$db_pot <- dat$db_pension / annuity * 100000 + dat$db_pension * mult
	dat$cont_employer <- dat$income_dc * employer_cont
	dat$cont_employee <- dat$income_dc * employee_cont
	dat$dc_pot_thresh[1] <- dat$cont_employee[1] + dat$cont_employer[1]
	for(i in 2:nyears)
	{
		dat$dc_pot_thresh[i] <- dat$dc_pot_thresh[i-1] * (1 + dat$ret[i]) + dat$cont_employer[i] + dat$cont_employee[i]
	}
	dat$dc_pension_thresh <- dat$dc_pot_thresh / 100000 * annuity
	dat$total_pot <- dat$db_pot + dat$dc_pot_thresh
	dat$total_pension <- dat$db_pension + dat$dc_pension_thresh
	return(dat[ind,])
}


#' Calculate pensions for different schemes
#' 
#' Calculates total pot and annual benefits for DB, DC and TPS pensions
#' 
#' @param income output from \code{income_projection}
#' @param annuity output from \code{annuity_rates}
#' @param prudence Parameter for \code{investment_returns}, 50 or 65
#' @param fund Parameter for \code{investment_returns}, "USS", "Growth fund", "Moderate growth fund", "Cautious growth fund", or "Cash fund"
#' @param employee_cont Percentage of salary contributed by employee
#' @param employer_cont Percentage of salary contributed by employer
#' 
#' @export
#' @return Data frame of years and pension pots and annual benefits
pension_calculation <- function(income, annuity, prudence, fund, employer_cont=0.08, employee_cont=0.12)
{
	ret <- subset(investment_returns(), Prudence==prudence & Fund==fund)$growth
	
	if(length(ret) > length(income))
	{
		ret <- ret[1:length(income)]
	} else if(length(ret) < length(income)) {
		rem <- length(income) - length(ret)
		ret <- c(ret, rep(ret[length(ret)], rem))
	}

	tps_2018 <- calc_db_dc(ret, annuity, income, 
		prop_salary = 1/57, 
		incr = 1.016, 
		mult = 0,
		employee_cont = 0,
		employer_cont = 0,
		db_cutoff = Inf
	)

	dc_2018 <- calc_db_dc(ret, annuity, income,
		employee_cont=0.08,
		employer_cont=0.1325,
		db_cutoff=0,
		prop_salary=0,
		incr=0,
		mult=0
	)

	db_orig <- calc_db_dc(ret, annuity, income, 
		employee_cont = employee_cont,
		employer_cont = employer_cont,
		prop_salary = 1/75,
		db_cutoff = 55000,
		incr = 1,
		mult = 3
	)

	db_2018 <- calc_db_dc(ret, annuity, income,
		employee_cont = 0.08,
		employer_cont = 0.12,
		prop_salary = 1/85,
		incr = 1,
		mult = 3,
		db_cutoff = 42000
	)

	##########

	dat <- dplyr::tibble(
		year=1:length(income) + today() %>% year(),
		income=income, 
		db_pot=db_orig$total_pot,
		db_pension=db_orig$total_pension,
		dc_pot=dc_2018$total_pot,
		dc_pension=dc_2018$total_pension,
		tps_pot=tps_2018$total_pot,
		tps_pension=tps_2018$total_pension,
		db_pot2=db_2018$total_pot,
		db_pension2=db_2018$total_pension
	)
	dat$prudence = prudence
	dat$fund = fund
	return(dat)
}


####
# #Calculate how percentage of salary needed to match the DB benefits

# #' @export
# #' @return Data frame of years and pension pots and annual benefits
# required_savings_calculation <- function(income, prudence, fund, dob, db_pot, dc_pot, db_pot2,tps_pot)
# {
# }
  

#' Retirement date calculator
#' 
#' @param dob e.g. "06/09/1984" for 6 september 1984
#' 
#' @export
#' @return date object
retirement_date <- function(dob)
{
	dob <- lubridate::ymd(dob)
	if(lubridate::year(dob) < 1954) {
		return(dob + lubridate::years(65))
	} else if(lubridate::year(dob) < 1961) {
		return(dob + lubridate::years(66))
	} else if(lubridate::year(dob) < 1977) {
		return(dob + lubridate::years(67))
	} else {
		return(dob + lubridate::years(68))
	}
}

#' Years of work left until retirement
#'
#' @param dob e.g. "06/09/1984" for 6 september 1984
#'
#' @export
#' @return Numeric value
years_left <- function(dob)
{
	{retirement_date(dob) %>% year()} - {today() %>% year()}
}

#' Pension summary
#' 
#' Calculate various differences
#' 
#' @param benefits Output from \code{pension_calculation}
#' @param dob e.g. "06/09/1984" for 6 september 1984
#' 
#' @export
#' @return relevant row from output of \code{pension_calculation}
pension_summary <- function(benefits, dob)
{
  retirementYearIdx <- which(benefits$year == lubridate::year(retirement_date(dob)))
  incomeWorkingYears <- benefits$income[1:retirementYearIdx]


	benefits <- subset(benefits, year == lubridate::year(retirement_date(dob)))
  
	#Calculate how much percentage of income would need to be saved to match
	#the total pension values. 
  #First for the DC only proposal
  targetSavings <- benefits$db_pot - benefits$dc_pot
  #targetSavings <- subset(targetSavings, year == lubridate::year(retirement_date(dob)))
  

  benefits$dc_salary_percent <-savings_required_percentage(targetSavings,incomeWorkingYears,
                                                  prudence = benefits$prudence, 
                                                  fund = benefits$fund)
  
  incomeNow <- incomeWorkingYears[1]
  incomeFinal <- incomeWorkingYears[length(incomeWorkingYears)]
  benefits$dc_salary_cut_now <- benefits$dc_salary_percent*incomeNow
  benefits$dc_salary_cut_final <- benefits$dc_salary_percent*incomeFinal
  
#  dc_salary_percent <- rep(dc_salary_percent, length(db_pot))
  # #Now for the march proposal
  # 
  targetSavings <- benefits$db_pot - benefits$db_pot2

  benefits$dc_salary_percent2 <-savings_required_percentage(targetSavings,incomeWorkingYears,
                                                            prudence = benefits$prudence, 
                                                            fund = benefits$fund)
  
  benefits$dc_salary_cut_now2 <- benefits$dc_salary_percent2*incomeNow
  benefits$dc_salary_cut_final2 <- benefits$dc_salary_percent2*incomeFinal
  
  # #Now for the TPS 
  targetSavings <- benefits$tps_pot - benefits$db_pot2
  benefits$tps_salary_percent <-savings_required_percentage(targetSavings,incomeWorkingYears,
                                                    prudence = benefits$prudence, 
                                                    fund = benefits$fund)

  benefits$tps_salary_cut_now <- benefits$tps_salary_percent*incomeNow
  benefits$tps_salary_cut_final <- benefits$tps_salary_percent*incomeFinal
  
  
  return(benefits)
}
