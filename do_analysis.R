# do_analysis.R

source('~/BA/analysis_functions.R')

do_analysis_normal <- function(){
	raw_data <- get_data(FILEPATH_NORMAL)
	df <- filter_data(raw_data)
	View(df)
	plot_decile_scores_normal(df)
	return(get_logistic_regression_normal_compas(df))
}

do_analysis_violent <- function(){
	raw_data <- get_data(FILEPATH_VIOLENT)
	df_v <- filter_data_violent(raw_data)
	plot_decile_scores_violent(df_v)
	return(get_logistic_regression_violent(df_v))
}