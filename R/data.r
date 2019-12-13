#' United Kingdom 2019 General Election Results
#'
#' A tibble containing candidate-level vote data by constituency on the UK general election of 2019
#'
#' @format A tibble with 3,320 rows and 13 columns
#'
#' \describe{
#' \item{\code{cid}}{Constituency TOPO id}
#' \item{\code{constituency}}{Name of Constituency}
#' \item{\code{electorate}}{Number of registered voters}
#' \item{\code{party_name}}{Political party of candidate}
#' \item{\code{candidate}}{Candidate name}
#' \item{\code{votes}}{Votes received}
#' \item{\code{vote_share_percent}}{Percent of all ballots cast}
#' \item{\code{vote_share_change}}{Percentage point change from
#'     previous election}
#' \item{\code{vrank}}{Rank of candidate by votes cast in constituency}
#' \item{\code{turnout}}{Total number of votes cast in constituency}
#' \item{\code{fname}}{First name of candidate}
#' \item{\code{lname}}{Last name of candidate}
#'}
#'
#' @docType data
#' @keywords datasets
#' @name ukvote2019
#' @source BBC https://www.bbc.com/news/politics/constituencies
'ukvote2019'

