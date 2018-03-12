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


#' Calculate pensions for different schemes
#' 
#' Calculates total pot and annual benefits for DB, DC and TPS pensions
#' 
#' @param income output from \code{income_projection}
#' @param annuity output from \code{annuity_rates}
#' @param employee_cont Percentage of salary contributed by employee. Default 0.08
#' @param employer_cont Percentage of salary contributed by employer. Default 0.1325
#' @param prudence Parameter for \code{investment_returns}, 50 or 65
#' @param fund Parameter for \code{investment_returns}, "USS", "Growth fund", "Moderate growth fund", "Cautious growth fund", or "Cash fund"
#' 
#' @export
#' @return Data frame of years and pension pots and annual benefits
pension_calculation <- function(income, annuity, employee_cont=0.08, employer_cont=0.12, prudence, fund)
{

	ret <- subset(investment_returns(), Prudence==prudence & Fund==fund)$growth
	if(length(ret) > length(income))
	{
		ret <- ret[1:length(income)]
	} else if(length(ret) < length(income)) {
		rem <- length(income) - length(ret)
		ret <- c(ret, rep(ret[length(ret)], rem))
	}

	# True up to income of 55k. Afterwards add on DC
	prop_salary <- 1/75
	incr <- 1
	mult <- 3
	db_pension <- rep(0, length(income))
	income_thresh <- pmin(income, 55000)
	income_dc <- income - income_thresh
	db_pension[1] <- income_thresh[1] * prop_salary
	for(i in 2:length(income_thresh))
	{
		db_pension[i] <- db_pension[i-1] * incr + income_thresh[i] * prop_salary
	}
	db_pot <- db_pension / annuity * 100000 + mult * db_pension

	cont <- income_dc * employer_cont + income_dc * employee_cont
	dc_pot_thresh <- rep(0, length(income_dc))
	dc_pot_thresh[1] <- cont[1]
	for(i in 2:length(income_dc))
	{
		dc_pot_thresh[i] <- dc_pot_thresh[i-1] * (1 + ret[i]) + cont[i]
	}
	dc_pension_thresh <- dc_pot_thresh / 100000 * annuity

	db_pension <- db_pension + dc_pension_thresh
	db_pot <- db_pot + dc_pot_thresh

	prop_salary <- 1/57
	incr <- 1.016
	mult <- 0
	tps_pension <- rep(0, length(income))
	tps_pension[1] <- income[1] * prop_salary
	for(i in 2:length(income))
	{
		tps_pension[i] <- tps_pension[i-1] * incr + income[i] * prop_salary
	}
	tps_pot <- tps_pension / annuity * 100000 + mult * tps_pension


	# cont <- income * employer_cont + income * employee_cont
	cont <- income * 0.1325 + income * 0.08
	dc_pot <- rep(0, length(income))
	dc_pot[1] <- cont[1]
	for(i in 2:length(income))
	{
		dc_pot[i] <- dc_pot[i-1] * (1 + ret[i]) + cont[i]
	}
	dc_pension <- dc_pot / 100000 * annuity


	############

	# True up to income of 42k. Afterwards add on DC
	prop_salary <- 1/85
	incr <- 1
	mult <- 3
	db_pension2 <- rep(0, length(income))
	income_thresh <- pmin(income, 42000)
	income_dc <- income - income_thresh
	db_pension2[1] <- income_thresh[1] * prop_salary
	for(i in 2:length(income_thresh))
	{
		db_pension2[i] <- db_pension2[i-1] * incr + income_thresh[i] * prop_salary
	}
	db_pot2 <- db_pension2 / annuity * 100000 + mult * db_pension2

	cont <- income_dc * employer_cont + income_dc * employee_cont
	dc_pot_thresh <- rep(0, length(income_dc))
	dc_pot_thresh[1] <- cont[1]
	for(i in 2:length(income_dc))
	{
		dc_pot_thresh[i] <- dc_pot_thresh[i-1] * (1 + ret[i]) + cont[i]
	}
	dc_pension_thresh <- dc_pot_thresh / 100000 * annuity

	db_pension2 <- db_pension2 + dc_pension_thresh
	db_pot2 <- db_pot2 + dc_pot_thresh




	##########

	dat <- dplyr::tibble(year=1:length(income) + 2018, income=income, db_pot=db_pot, db_pension=db_pension, dc_pot=dc_pot, dc_pension=dc_pension, tps_pot=tps_pot, tps_pension=tps_pension, db_pot2=db_pot2, db_pension2=db_pension2)
	return(dat)
}


#' Retirement date calculator
#' 
#' Just adds 68 years. Needs to be made more accurate based on birth year
#' 
#' @param dob e.g. "06/09/1984" for 6 september 1984
#' 
#' @export
#' @return date object
retirement_date <- function(dob)
{
	dob <- lubridate::ymd(dob)
	message(years(dob))
	if(lubridate::year(dob) < 1954) {
		message(65)
		return(dob + lubridate::years(65))
	} else if(lubridate::year(dob) < 1961) {
		message(66)
		return(dob + lubridate::years(66))
	} else if(lubridate::year(dob) < 1977) {
		message(67)
		return(dob + lubridate::years(67))
	} else {
		message(68)
		return(dob + lubridate::years(68))
	}
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
	subset(benefits, year == lubridate::year(retirement_date(dob)))
}
