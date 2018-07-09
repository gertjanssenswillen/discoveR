
alpha_miner_plus <- function(eventlog) {

t_log <- eventlog %>%
    activity_labels() %>%
    as.character()

eventlog %>%
    precedence_matrix -> precedence_matrix

precedence_matrix %>%
    filter(antecedent == as.character(consequent)) %>%
    pull(antecedent) %>%
    unique() %>%
    as.character() -> l_one_l

t_prime <- setdiff(t_log, l_one_l)

f_l_one_l <- data_frame(from = character(), to = character())

for(t in l_one_l) {
    A <- precedence_matrix %>%
        filter(consequent == t) %>%
        filter(antecedent %in% t_prime) %>%
        pull(antecedent) %>%
        as.character()
    B <- precedence_matrix %>%
        filter(antecedent == t) %>%
        filter(consequent %in% t_prime) %>%
        pull(consequent) %>%
        as.character()


    a_prime <- setdiff(A,B)
    b_prime <- setdiff(B,A)

    a_prime <- paste(a_prime, collapse = ",")
    b_prime <- paste(b_prime, collapse = ",")

    f_l_one_l %>%
        bind_rows(
            tribble(~from, ~to,
                    t, paste0("p_",a_prime,"_",b_prime),
                    paste0("p_",a_prime,"_",b_prime), t)
        )  -> f_l_one_l
}

log_filtered <- eventlog %>%
    filter_activity(l_one_l, reverse = T)

alpha_miner_base(log_filtered) -> base_PN

create_PN(places = pull(places(base_PN), id),
          transitions = c(pull(transitions(base_PN), id), l_one_l),
          flows = bind_rows(flows(base_PN), f_l_one_l),
          marking = marking(base_PN)) -> new_PN

    return(new_PN)
}
