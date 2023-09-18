remove_non_unique <- function(df) {
  df %>%
    select(where(~ n_distinct(.) != 1))
}
