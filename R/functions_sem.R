spatial_psem_call <- function(data) {

  # todo : metaprogramming to parse any passed formula list directly
  sem_obj <- psem(
    lme(
      fixed = sd_r_average_log ~ HII_focal  + H_fine  + mu_SR,
      random = ~ 1|dummy,
      correlation = corExp(form = ~ lon + lat),
      method = "ML",
      data = data
    ),
    lme(
      fixed = abs_trend_average_log ~  HII_focal  + H_fine   + mu_SR,
      random = ~ 1|dummy,
      correlation = corExp(form = ~ lon + lat),
      method = "ML",
      data = data
    ),
    lme(
      fixed = mu_SR ~HII_focal  + H_fine,
      random = ~ 1|dummy,
      correlation = corExp(form = ~ lon + lat),
      method = "ML",
      data = data
    ),
    data = data
  )

  return(list(
    out_sem = sem_obj,
    summary_out = summary(sem_obj)
  ))
}

# SEM call function when working on MC replicates
spatial_psem_call_mc <- function(data) {

  # todo : metaprogramming to parse any passed formula list directly
  sem_obj <- psem(
    lme(
      fixed = sd_r_average_log ~ HII_focal  + H_fine  + mu_SR,
      random = ~ 1|dummy,
      correlation = corExp(form = ~ X + Y),
      method = "ML",
      data = data
    ),
    lme(
      fixed = ata_log ~  HII_focal  + H_fine   + mu_SR,
      random = ~ 1|dummy,
      correlation = corExp(form = ~ X + Y),
      method = "ML",
      data = data
    ),
    lme(
      fixed = mu_SR ~HII_focal  + H_fine,
      random = ~ 1|dummy,
      correlation = corExp(form = ~ X + Y),
      method = "ML",
      data = data
    ),
    data = data
  )

  return(list(
    out_sem = sem_obj,
    summary_out = summary(sem_obj)
  ))
}

run_spatial_sems <- function(sem_data, sems_type = "no_cv_com", fun_runsem = spatial_psem_call, habitat_col = "HABITAT_GROUP") {
  if (nrow(sem_data) == 0) return(list())

  # split by category to a list of dataframes
  datalist <- sem_data %>%
    group_by(!!sym(habitat_col)) %>%
    mutate(
      dummy = factor(rep(1, n()))
    ) %>%
    group_split

  categories <- sem_data %>%
    group_by(!!sym(habitat_col)) %>%
    group_keys %>%
    pull(!!sym(habitat_col))

  datalist %>%
    set_names(categories) %>%
    map(\(d) catchConditions({fun_runsem(data = d)}))
}



spatial_psem_buffered <- function(data) {
  # todo : metaprogramming to parse flist directly
  sem_obj <- psem(
    lme(
      fixed = sd_r_average_log ~ HII_focal + HII_buffer + H_fine  + mu_SR,
      random = ~ 1|dummy,
      correlation = corExp(form = ~ lon + lat),
      method = "ML",
      data = data
    ),
    lme(
      fixed = abs_trend_average_log ~  HII_focal + HII_buffer + H_fine   + mu_SR,
      random = ~ 1|dummy,
      correlation = corExp(form = ~ lon + lat),
      method = "ML",
      data = data
    ),
    lme(
      fixed = mu_SR ~HII_focal + HII_buffer + H_fine,
      random = ~ 1|dummy,
      correlation = corExp(form = ~ lon + lat),
      method = "ML",
      data = data
    ),
    data = data
  )

  return(list(
    out_sem = sem_obj,
    summary_out = summary(sem_obj)
  ))
}

#' Tiny util because symbol replacement did not work in a tar_map scope
convert_hii_raw <- function(df, buffer_col) {
  df %>%
    mutate(
      HII_buffer := !!sym(buffer_col)
    )
}
