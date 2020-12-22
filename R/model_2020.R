#' Calculate cutoff
#'
#' @param income output from \code{income_projection}
#' @param db_cutoff1 income cutoff year 1
#' @param inflation inflation
#'
#' @export
#' @return Tibble
calc_db_cutoff <- function(income, db_cutoff1, inflation)
{
	nyears <- length(income)
	dplyr::tibble(
		year = 1:nyears,
		income = income,
		db_cutoff = db_cutoff1 * (1 + inflation)^(0:(nyears-1)),
		income_below_threshold = dplyr::case_when(
			income < db_cutoff ~ income,
			TRUE ~ db_cutoff
		)
	)
}

#' Contribution rates
#'
#' <full description>
#'
#' @param nyears how many years to calculate
#' @param contribution_model Default ="USS Trustee current plans"
#'
#' @export
#' @return Tibble
contribution_rates <- function(nyears, contribution_model="USS Trustee current plans")
{
	list(
		dplyr::tibble(
			model = "USS Trustee current plans",
			year = 1:nyears,
			employee_rate = c(0.096, 0.096, rep(0.11, nyears - 2)),
			employer_rate = c(0.211, 0.211, rep(0.237, nyears - 2))
		),
		dplyr::tibble(
			model = "USS Trustee minimum",
			year = 1:nyears,
			employee_rate = c(0.096, 0.107, rep(0.131, nyears - 2)),
			employer_rate = c(0.21, 0.23, rep(0.277, nyears - 2))
		),
		dplyr::tibble(
			model = "USS Trustee maximum",
			year = 1:nyears,
			employee_rate = c(0.096, 0.096, rep(0.226, nyears - 2)),
			employer_rate = c(0.21, 0.211, rep(0.453, nyears - 2))
		),
		dplyr::tibble(
			model = "No change",
			year = 1:nyears,
			employee_rate = c(0.096, rep(0.08, nyears - 1)),
			employer_rate = c(0.21, rep(0.18, nyears - 1))
		)
	) %>% bind_rows() %>% subset(., `model` == contribution_model)
}


#' Tax thresholds
#'
#' @param income output from \code{income_projection}
#' @param inflation inflation
#'
#' @export
#' @return
calc_tax_thresholds <- function(income, inflation)
{
	nyears <- length(income)
	tax_thresholds <- dplyr::tibble(
		year = 1:nyears,
		lower = 12500 * (1 + inflation)^(0:(nyears-1)),
		higher = 50000 * (1 + inflation)^(0:(nyears-1)),
		additional = 150000 * (1 + inflation)^(0:(nyears-1)),
		income = income,
		above_lower = dplyr::case_when(
			income > higher ~ higher - lower,
			income > lower ~ income - lower,
			TRUE ~ 0
		),
		above_higher = dplyr::case_when(
			income >= additional ~ additional - higher,
			income > higher ~ income - higher,
			TRUE ~ 0
		),
		above_additional = dplyr::case_when(
			income >= additional ~ income - additional,
			TRUE ~ 0
		)
	)
}

#' Contributions
#'
#' @param income output from \code{income_projection}
#' @param db_cutoff1 income cutoff year 1 default=58589.7
#' @param inflation default=0.02
#' @param contribution_model default="USS Trustee current plans"
#' @param lower_rate default= 0.2
#' @param higher_rate default =0.4 
#' @param additional_rate default=0.45
#'
#' @export
#' @return Tibble
calculate_contributions <- function(income, db_cutoff1=58589.7, inflation=0.02, contribution_model="USS Trustee current plans", lower_rate = 0.2, higher_rate=0.4, additional_rate=0.45)
{
	nyears <- length(income)
	db_cutoff <- calc_db_cutoff(income, db_cutoff1, inflation)
	contributions <- contribution_rates(nyears, contribution_model=contribution_model)

	contributions$employee <- contributions$employee_rate * db_cutoff$income_below_threshold/(1+inflation)^(db_cutoff$year-1)
	contributions$employer <- contributions$employer_rate * db_cutoff$income_below_threshold/(1+inflation)^(db_cutoff$year-1)

	contributions$employee_cumsum <- cumsum(contributions$employee)
	contributions$employer_cumsum <- cumsum(contributions$employer)


	tax_thresholds <- calc_tax_thresholds(income, inflation) %>% dplyr::mutate(
		employee_additional = case_when(
			contributions$employee < above_additional ~ contributions$employee,
			TRUE ~ above_additional
		),
		employee_higher = case_when(
			(contributions$employee - employee_additional) < above_higher ~ (contributions$employee - employee_additional),
			TRUE ~ above_higher
		),
		employee_lower = case_when(
			(contributions$employee - employee_additional - employee_higher) < above_lower ~ (contributions$employee - employee_additional - employee_higher),
			TRUE ~ above_lower
		)
	)
	contributions$employee_tax = tax_thresholds$employee_lower * (1 - lower_rate) + tax_thresholds$employee_higher * (1 - higher_rate) + tax_thresholds$employee_additional * (1 - additional_rate)
	contributions$employee_tax_cumsum = cumsum(contributions$employee_tax)
	return(contributions)
}


#' Contributions over all models
#'
#' @param income output from \code{income_projection}
#' @param db_cutoff1 income cutoff year 1 default=58589.7
#' @param inflation default=0.02
#' @param lower_rate default= 0.2
#' @param higher_rate default =0.4 
#' @param additional_rate default=0.45
#'
#' @export
#' @return Tibble
contributions_model <- function(income, db_cutoff1=58589.7, inflation=0.02, lower_rate = 0.2, higher_rate=0.4, additional_rate=0.45)
{
	contribution_models <- c("USS Trustee current plans", "USS Trustee minimum", "USS Trustee maximum", "No change")
	dat <- lapply(contribution_models, function(x)
	{
		calculate_contributions(income, db_cutoff1, inflation, contribution_model = x, lower_rate, higher_rate, additional_rate) %>%
		mutate(model = x)
	}) %>% bind_rows()
	return(dat)
}

