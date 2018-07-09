
#' Alpha Miner
#'
#' @param eventlog Event log
#' @param flavor Algorithm flavor, currently only the traditional algoritm
#'
#' @return A Petri Net
#' @importFrom bupaR group_by_case
#' @importFrom bupaR first_n
#' @importFrom bupaR last_n
#' @importFrom processmapR precedence_matrix
#' @importFrom bupaR filter
#' @importFrom bupaR activity_labels
#' @importFrom petrinetR create_PN
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom stringr str_c
#' @importFrom tidyr unnest
#' @importFrom purrr map2
#' @importFrom purrr map
#' @importFrom bupaR n_activities
#' @importFrom purrr list_along
#' @export
#'
alpha_miner <- function(eventlog, flavor = c("original", "+")) {

    antecedent <- NULL
    consequent <- NULL
    n_mirror <- NULL
    type <- NULL
    key <- NULL
    activity <- NULL
    n_con <- NULL
    n_ante <- NULL
    a <- NULL
    pair_id <- NULL
    place <- NULL
    con_equal <- NULL
    ante_a <- NULL
    ante_b <- NULL
    con_a <- NULL
    con_b <- NULL
    pair_id_a <- NULL
    pair_id_b <- NULL
    ante_equal <- NULL
    ante_pref_equal <- NULL
    con_pref_equal <- NULL



    flavor = match.arg(flavor)

    if(flavor == "original") {
        alpha_miner_base(eventlog)
    } else if(flavor == "+") {
        alpha_miner_plus(eventlog)
    }

}


