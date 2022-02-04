if(!is.na(seed)){
  set.seed(seed)
}

###Load data:
df_overall <- loadCounterfactualData(c("No Vaccines", "Baseline-Direct",
                                       "Baseline-Direct & No Healthcare Surging",
                                       "No Vaccines-No Healthcare Surging",
                                       "Baseline-No Healthcare Surging"),
                                     group_by = NULL,
                                     exclude_iso3cs = exclude_iso3cs
)


###Figure 1 daily deaths over time:
#sort data
df_sorted <- df_overall %>%
  select(counterfactual, deaths_avg) %>%
  rbind(df_overall %>%
          select(baseline_deaths_avg) %>%
          rename(deaths_avg = baseline_deaths_avg) %>%
          unique() %>%
          mutate(counterfactual = "Baseline")) %>%
  pivot_wider(names_from = counterfactual, values_from = deaths_avg)

#calculate levels, need to isolate vaccine impacts:
df_averted <- df_sorted %>%
  transmute(
    averted_reduced_burden_hospital_indirect_max = `No Vaccines` - `Baseline`, #total deaths averted
    averted_reduced_burden_hospital_direct_min = `No Vaccines-No Healthcare Surging` - `Baseline-No Healthcare Surging`, #these two bound deaths averted through hosptial admission reductions
    #calculate deaths averted by direct and indirect effects in the absence of hospital burden
    averted_indirect_max = averted_reduced_burden_hospital_direct_min,
    averted_indirect_min = `No Vaccines-No Healthcare Surging` - `Baseline-Direct & No Healthcare Surging`,
    averted_direct_max = averted_indirect_min,
    averted_direct_min = rep(0, length(averted_direct_max)),
    #split deaths averted by reduced burden into direct/indirect
    averted_reduced_burden_hospital_indirect_min = (`No Vaccines` - `Baseline-Direct`) - #deaths averted by direct with both hospital reduction and direct
      averted_direct_max + #subtract the deaths averted with no reduction in burden
      averted_reduced_burden_hospital_direct_min, #scale up so area is correct
    averted_reduced_burden_hospital_direct_max = averted_reduced_burden_hospital_indirect_min,
  ) %>%
  pivot_longer(cols = contains("averted"), names_to = c("mechanism", "bound"),
               names_sep = "_(?![\\s\\S]*_)") %>%
  pivot_wider(id_cols = c(mechanism), names_from = bound, values_from = value) %>%
  #rename and fitler for the plot
  mutate(
    mechanism = case_when(
      # mechanism == "averted_direct" ~ "Protection against Disease",
      # mechanism == "averted_indirect" ~ "Protection against Transmission and Infection",
      # mechanism == "averted_reduced_burden_hospital_direct" ~
      #   "Reduction in healthcare burden\n(from Protection against Disease)",
      # mechanism == "averted_reduced_burden_hospital_indirect" ~
      #   "Reduction in healthcare burden\n(from Protection against Transmission and Infection)"
      mechanism == "averted_direct" ~ "Direct Protection",
      mechanism == "averted_indirect" ~ "Indirect Protection",
      mechanism == "averted_reduced_burden_hospital_direct" ~
        "Reduced Healthcare Burden\n(Direct)",
      mechanism == "averted_reduced_burden_hospital_indirect" ~
        "Reduced Healthcare Burden\n(Indirect)"
    )
  ) %>%
  mutate(averted = max - min)

saveRDS(
  list(
    "hosp_overall" = df_overall,
    "hosp_total" = df_averted
  ),
  "hospital_numbers.Rds"
)
