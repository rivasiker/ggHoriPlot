#' Peaks times for sports and leisure activities
#'
#' A dataset containing the peak time for doing 29 sports
#' and leisure activities.
#'
#' @format A data frame with 8,092 rows and 3 variables:
#' \describe{
#'   \item{activity}{name of the activity}
#'   \item{time}{time of the day, in hhmm format}
#'   \item{p}{standardized peak}
#' }
#' @source \url{https://github.com/halhen/viz-pub/blob/master/sports-time-of-day/activity.tsv}
#' @source \url{https://eagereyes.org/blog/2017/joy-plots}
"sports_time"


#' Distribution of COVID-19 cases in Asia
#'
#' A dataset containing the geographic distribution of COVID-19 cases in Asia during 2020.
#'
#' @format A data frame with 12,695 rows and 3 variables:
#' \describe{
#'   \item{date_mine}{date of the measurement in yyyy-mm-dd format}
#'   \item{y}{standardized number of cases}
#'   \item{countriesAndTerritories}{countries and territories in Asia}
#' }
#' @source \url{https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide}
"COVID"

#' Distribution of repeats along the human genome
#'
#' A dataset containing the percentage of simple repeats in 100 kb windows along the human genome (hg38).
#'
#' @format A data frame with 30,885 rows and 4 variables:
#' \describe{
#'   \item{genoName}{chromosome name}
#'   \item{bin}{starting coordinate of window}
#'   \item{bin_2}{end coordinate of window}
#'   \item{p_repeat}{percentage of repeats}
#' }
#' @source \url{https://genome.ucsc.edu/cgi-bin/hgTrackUi?g=rmsk}
"rmsk"

#' Average temperature in major cities of the US
#'
#' A dataset containing the average temperature in Fahrenheit for major cities in the US for the year 2000.
#'
#' @format A data frame with 57,828 rows and 9 variables:
#' \describe{
#'   \item{Region}{geographic region}
#'   \item{Country}{country}
#'   \item{State}{state or territory}
#'   \item{City}{city or town}
#'   \item{Month}{month}
#'   \item{Day}{day}
#'   \item{Year}{year}
#'   \item{AvgTemperature}{average temperature in Fahrenheit}
#'   \item{date_mine}{data in yyy-mm-dd format}
#' }
#' @source \url{https://www.kaggle.com/sudalairajkumar/daily-temperature-of-major-cities}
#' @source \url{https://benschmidt.org/2014/06/05/optimally-ordering-geographical-entities-in-linear-space/}
"climate_US"


#' Average temperature in Copenhagen
#'
#' A dataset containing the average temperature in degrees Celsius for Copenhagen between the year 1995 and 2019.
#'
#' @format A data frame with 9,132 rows and 9 variables:
#' \describe{
#'   \item{Region}{geographic region}
#'   \item{Country}{country}
#'   \item{State}{state or territory}
#'   \item{City}{city or town}
#'   \item{Month}{month}
#'   \item{Day}{day}
#'   \item{Year}{year}
#'   \item{AvgTemperature}{average temperature in Fahrenheit}
#'   \item{date_mine}{data in yyy-mm-dd format}
#' }
#' @source \url{https://www.kaggle.com/sudalairajkumar/daily-temperature-of-major-cities}
"climate_CPH"
