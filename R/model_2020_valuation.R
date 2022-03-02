#' Investment returns for the 2020 valuation
#'
#' @export
#' @return Data frame
investment_returns_2020 <- function(years)
{
	rbind(
		dplyr::tibble(
			Scenario="Scenario 1",
			Year=2022:2070,
			growth=0
		),
		dplyr::tibble(
			Scenario="Scenario 2a",
			Year=2022:2070,
			growth=0.001
		),
		dplyr::tibble(
			Scenario="Scenario 2b",
			Year=2022:2070,
			growth=0.001
		),
		dplyr::tibble(
			Scenario="Scenario 3a",
			Year=2022:2070,
			growth=0.002
		),
		dplyr::tibble(
			Scenario="Scenario 3b",
			Year=2022:2070,
			growth=0.002
		)
	) %>%
		filter(Year %in% years)
}



#' Calculate annuity rates for 2020 valuation
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
annuity_rates <- function(sex, type, years, le_increase=0.005)
{
	annuities_quote <- tribble(
		~age, ~Type, ~Sex, ~val,
		66, "single", "men", 2875.00,
		67, "single", "men", 3028.00,
		68, "single", "men", 3179.00,
		66, "single", "women", 2875.00,
		67, "single", "women", 3028.00,
		68, "single", "women", 3179.00,
		66, "joint", "men", 2469.00, 
		67, "joint", "men", 2588.00, 
		68, "joint", "men", 2733.00, 
		66, "joint", "women", 2720.00,
		67, "joint", "women", 2877.00,
		68, "joint", "women", 3045.00
	)

	annuity <- rep(0, years)
	annuity[1] <- subset(annuities_quote, Sex==sex & Type==type & age==66)$val
	annuity[2] <- annuity[1] / (1+le_increase)
	annuity[3] <- subset(annuities_quote, Sex==sex & Type==type & age==66)$val / (1+le_increase)^2
	for(i in 4:6)
	{
		annuity[i] <- annuity[i-1] / (1 + le_increase)
	}
	annuity[7] <- subset(annuities_quote, Sex==sex & Type==type & age==67)$val / (1+le_increase)^6
	annuity[8] <- annuity[7] / (1+le_increase)
	annuity[9] <- annuity[8] / (1+le_increase)
	annuity[10] <- subset(annuities_quote, Sex==sex & Type==type & age==67)$val / (1+le_increase)^9
	for(i in 10:24)
	{
		annuity[i] <- annuity[i-1] / (1 + le_increase)
	}
	annuity[25] <- subset(annuities_quote, Sex==sex & Type==type & age==68)$val / (1+le_increase)^27
	for(i in 26:49)
	{
		annuity[i] <- annuity[i-1] / (1 + le_increase)
	}
	return(annuity[1:years])
}


scenarios <- function(years, current_scenario, incr)
{
	list(
		scenario_1 = list(
			ret=subset(investment_returns_2020(years), Scenario=="Scenario 1")$growth,
			prop_salary=0,
			employee_cont = 0.096,
			employer_cont = 0.018,
			db_cutoff = 0,
			incr=incr,
			mult = 3
		),
		scenario_2a = list(
			ret=subset(investment_returns_2020(years), Scenario=="Scenario 2a")$growth,
			prop_salary = 1/170,
			employee_cont = 0.12,
			employer_cont = 0,
			db_cutoff = 40000,
			incr=incr,
			mult = 3
		),
		scenario_2b = list(
			ret=subset(investment_returns_2020(years), Scenario=="Scenario 2b")$growth,
			prop_salary = 1/165,
			employee_cont = 0.12,
			employer_cont = 0,
			db_cutoff = 30000,
			incr=incr,
			mult = 3
		),
		scenario_3a = list(
			ret=subset(investment_returns_2020(years), Scenario=="Scenario 3a")$growth,
			prop_salary = 1/115,
			employee_cont = 0.16,
			employer_cont = 0,
			db_cutoff = 40000,
			incr=incr,
			mult = 3
		),
		scenario_3b = list(
			ret=subset(investment_returns_2020(years), Scenario=="Scenario 3b")$growth,
			prop_salary = 1/110,
			employee_cont = 0.16,
			employer_cont = 0,
			db_cutoff = 30000,
			incr=incr,
			mult = 3
		),
		`current` = list(
			ret=subset(investment_returns_2020(years), Scenario==current_scenario)$growth,
			employee_cont = 0.08,
			employer_cont = 0.12,
			prop_salary = 1/75,
			db_cutoff =  59883.65,
			incr = "incr_5",
			mult = 3
		),
		`uuk1` = list(
			ret=rep(0.004, length(years)),
			employee_cont = 0.096,
			employer_cont = 0.104,
			prop_salary = 1/85,
			db_cutoff =  40000,
			incr = incr,
			mult = 3
		),
		`uuk2` = list(
			ret=rep(0.004, length(years)),
			employee_cont = 0.096,
			employer_cont = 0.104,
			prop_salary = 1/75,
			db_cutoff =  30000,
			incr = incr,
			mult = 3
		)
	)
}


#' Pension scenarios 2020
#'
#'
#' @param income Output from income_projection()
#' @param annuity Output from annuity_rates()
#' @param scenario Investment scenario for the DC component of the current scheme ("Scenario 1", "Scenario 2a", "Scenario 2b", "Scenario 3a", "Scenario 3b")
#' @param incr Either a column name from inflation_data, or a numeric value
#'
#' @export
#' @return List of data frames for each scenario
pension_calculation_2020 <- function(income, annuity, scenario, incr=1)
{
	years <- 1:length(income) + today() %>% year()

	sc <- scenarios(years, scenario, incr)
	out <- lapply(sc, function(s)
	{
		s$annuity <- annuity
		s$income <- income
		do.call(calc_db_dc, s) %>%
			mutate(years = years)
	})
	names(out) <- names(sc)

	return(out)
}


pension_calculation_2020_changes <- function(income, annuity, scenario1, scenario2, year_change, incr)
{
	years <- 1:length(income) + today() %>% year()
	sc <- scenarios(years, scenario1, incr)
	sc$Current$annuity <- annuity
	sc$Current$income <- income
	dat1 <- do.call(calc_db_dc, sc$Current)
	l <- sc[[scenario2]]
	l$annuity <- annuity
	l$income <- income
	l$dato <- dat1[1:which(years==year_change),]
	dat2 <- do.call(calc_db_dc, l)
	dat1$scenario <- "Current"
	dat1$years <- years
	dat2$scenario <- scenario2
	dat2$years <- c((year_change+1):max(years))
	dat <- bind_rows(dat1, dat2)
	return(dat)
}


#' Summarise the output from pension_calculation_2020()
#'
#' @param pension Output from pension_calculation_2020()
#'
#' @export
#' @return data frame
pension_calculation_2020_summary <- function(pension)
{
	lapply(names(pension), function(x)
	{
		pension[[x]] %>% 
			dplyr::slice_tail(n=1) %>%
			dplyr::select(year=years, total_pot, total_pension) %>%
			dplyr::mutate(scenario=x)
	}) %>% bind_rows()
}


#' Plot pension
#'
#' @param pension Output from pension_calculation_2020()
#' @param column Defailt="total_pot"
#'
#' @export
#' @return plot
plot_pension <- function(pension, column="total_pot")
{
	lapply(names(pension), function(x)
	{
		dplyr::tibble(
			value=pension[[x]][[column]],
			scenario=x,
			year=pension[[x]][["years"]]
		)
	}) %>% 
		dplyr::bind_rows() %>%
		ggplot2::ggplot(., ggplot2::aes(x=year, y=value)) +
		ggplot2::geom_line(ggplot2::aes(colour=scenario)) +
		ggplot2::scale_colour_brewer(type="qual") +
		ggplot2::labs(y=column)
}


#' Plot pension
#'
#' @param pension Output from pension_calculation_2020()
#' @param column Defailt="total_pot"
#'
#' @export
#' @return plot
plot_pension_change <- function(dat, column="total_pot")
{
	dplyr::tibble(
		value=dat[[column]],
		scenario=dat[["scenario"]],
		year=dat[["years"]]
	) %>%
		ggplot2::ggplot(., ggplot2::aes(x=year, y=value)) +
		ggplot2::geom_line(ggplot2::aes(colour=scenario)) +
		ggplot2::scale_colour_brewer(type="qual") +
		ggplot2::labs(y=column)
}
