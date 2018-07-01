
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
alpha_miner <- function(eventlog, flavor = c("traditional")) {

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

    transitions <- activity_labels(eventlog) %>% as.character()

    eventlog %>%
        group_by_case() %>%
        first_n(1) %>%
        activity_labels() %>%
        as.character() -> start_transitions

    eventlog %>%
        group_by_case() %>%
        last_n(1) %>%
        activity_labels() %>%
        as.character() -> end_transitions

    eventlog %>%
        precedence_matrix %>%
        filter(antecedent != "Start",consequent != "End") %>%
        mutate_if(is.factor, as.character) -> pmatrix

    pmatrix %>%
        rename(temp = antecedent,
               antecedent = consequent) %>%
        rename(consequent = temp) %>%
        rename(n_mirror = n) -> pmatrix_mirror

    pmatrix %>%
        full_join(pmatrix_mirror, by = c("antecedent","consequent")) %>%
        mutate(n = ifelse(is.na(n), 0, n),
               n_mirror = ifelse(is.na(n_mirror), 0, n_mirror)) %>%
        mutate(type = case_when(n > 0 & n_mirror == 0 ~"forward",
                                n == 0 & n_mirror > 0 ~ "backward",
                                n > 0 & n_mirror > 0 ~ "parallel")) %>%
        select(-n, -n_mirror) %>%
        spread(consequent, type, fill = "no") %>%
        gather(consequent, type, -antecedent) %>%
        mutate(from_id = dense_rank(antecedent), to_id = dense_rank(consequent))  -> footprint


    if(flavor == "traditional") {
        footprint %>%
            filter(antecedent == consequent) %>%
            filter(type != "parallel") %>%
            select(antecedent, consequent) %>%
            gather(key, activity, antecedent, consequent) %>%
            pull(activity) %>%
            unique() -> non_selfloops_transitions

        footprint %>%
            filter(type == "forward") %>%
            filter(antecedent %in% non_selfloops_transitions) %>%
            filter(consequent %in% non_selfloops_transitions) %>%
            mutate(antecedent = map(antecedent, c)) %>%
            mutate(consequent = map(consequent, c)) %>%
            mutate(pair_id = 1:n()) -> input_pairs

    } else if (flavor == "alpha_plus") {


        footprint %>%
            filter(antecedent == consequent) %>%
            filter(type == "parallel") %>%
            select(antecedent, consequent) %>%
            gather(key, activity, antecedent, consequent) %>%
            pull(activity) %>%
            unique() -> selfloops_transitions

        footprint %>%
            filter(antecedent == consequent) %>%
            filter(type != "parallel") %>%
            select(antecedent, consequent) %>%
            gather(key, activity, antecedent, consequent) %>%
            pull(activity) %>%
            unique() -> non_selfloops_transitions

        footprint %>%
            filter(type == "forward") %>%
            filter(!(antecedent %in% selfloops_transitions)) %>%
            filter(!(consequent %in% selfloops_transitions)) %>%
            mutate(antecedent = map(antecedent, c)) %>%
            mutate(consequent = map(consequent, c)) %>%
            mutate(pair_id = 1:n()) -> input_pairs


        footprint %>%
            filter(type == "forward") %>%
            filter((antecedent %in% selfloops_transitions) | (consequent %in% selfloops_transitions)) ->
            flows_one


        #additional_flows <- data_frame(from = )
    }

    create_new_pairs <- function(input_pairs) {

        input_pairs %>%
            rename(ante_a = antecedent, con_a = consequent) %>%
            rename(pair_id_a = pair_id) %>%
            select(ante_a, con_a, pair_id_a) -> pairs_a

        input_pairs %>%
            rename(ante_b = antecedent, con_b = consequent) %>%
            rename(pair_id_b = pair_id) %>%
            select(ante_b, con_b, pair_id_b) -> pairs_b

        merge(pairs_a, pairs_b) %>%
            tbl_df() %>%
            rowwise() %>%
            mutate(ante_equal = all(ante_a == ante_b)) %>%
            mutate(con_equal = all(con_a == con_b)) %>%
            mutate(ante_pref_equal = all(ante_a[-length(ante_a)] == ante_b[-length(ante_b)])) %>%
            mutate(con_pref_equal = all(con_a[-length(con_a)] == con_b[-length(con_b)])) %>%
            filter(pair_id_b < pair_id_a) -> all_combinations

        all_combinations %>%
            filter((con_equal & ante_pref_equal) | (ante_equal & con_pref_equal)) -> temp

        if(nrow(temp) > 0) {
        temp %>%
            mutate(new_a = ifelse(con_equal, sort(setdiff(ante_b, ante_a)), sort(setdiff(con_b, con_a)))) %>%
            mutate(new_b = ifelse(con_equal, sort(setdiff(ante_a, ante_b)), sort(setdiff(con_a, con_b)))) %>%
            left_join(footprint, by = c("new_a" = "antecedent","new_b" = "consequent")) %>%
            filter(type == "no") %>%
            mutate(n_ante = map2(ante_a,ante_b, ~sort(unique(c(.x, .y))))) %>%
            mutate(n_con = map2(con_a, con_b,  ~sort(unique(c(.x, .y))))) -> new_pairs
        } else {
            data_frame() -> new_pairs
        }


        return(new_pairs)

    }
    prune_pairs <- function(input_pairs, new_pairs) {

        new_pairs %>%
            select(pair_id_a, pair_id_b) %>%
            gather(key, pair_id) %>%
            select(-key) %>%
            unique() -> remove_pairs

        input_pairs %>%
            filter(!(pair_id %in% remove_pairs$pair_id)) -> final_pairs

        return(final_pairs)
    }


    output <- list_along(1:n_activities(eventlog)-1)
    i <- 1

    while(nrow(input_pairs) > 0) {
        create_new_pairs(input_pairs) -> new_pairs

        if(nrow(new_pairs) > 0) {

        input_pairs %>%
            prune_pairs(new_pairs) -> final_pairs


            new_pairs %>%
                ungroup() %>%
                transmute(antecedent = n_ante,
                          consequent = n_con) %>%
                mutate(pair_id = seq_len(n())) -> input_pairs
        } else {
            input_pairs -> final_pairs
            input_pairs <- data.frame()
        }

        output[[i]] <- final_pairs
        i <- i + 1

    }



    output %>%
        bind_rows() %>%
        select(antecedent, consequent) %>%
        mutate(place = str_c("p", 1:n())) %>%
        mutate(flows = map2(antecedent, consequent,  ~data_frame(a = .x, c = .y))) %>%
        select(place, flows) %>%
        unnest(flows) -> temp

    bind_rows(
        temp %>%
            select(from = a, to = place) %>%
            unique() %>%
            mutate_all(as.character),
        temp %>%
            select(from = place, to = c) %>%
            unique() %>%
            mutate_all(as.character),
        data_frame(from = "p_i", to = start_transitions),
        data_frame(from = end_transitions, to = "p_o")
    ) -> flows

    if(flavor == "alpha_plus") {
        bind_rows(
            flows,
            data_frame(from = c("g","p_e"), to = c("p_e","f"))
        ) -> flows
    }

    create_PN(
        places = c(temp %>%
                       pull(place) %>%
                       unique() %>%
                       as.character(),
                   "p_i",
                   "p_o"),
        transitions = activity_labels(eventlog) %>% as.character,
        flows = flows,
        marking = "p_i"
    ) -> PN

    return(PN)

}


