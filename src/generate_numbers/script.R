if(!is.na(seed)){
  set.seed(seed)
}

df_overall <- loadCounterfactualData(c("No Vaccines", "Baseline-Direct"),
                                          group_by = NULL,
                                          exclude_iso3cs = exclude_iso3cs
                                          )

#get deaths averted due to direct protection, on average
total <- df_overall %>% filter(counterfactual == "No Vaccines") %>% pull(averted_deaths_avg)
indirect <-  df_overall %>% filter(counterfactual == "Baseline-Direct") %>% pull(averted_deaths_avg)
percent_direct <- (total-indirect)/total * 100

saveRDS(
  data.frame(
    percent_averted_direct = percent_direct
  ),
  "report_numbers.Rds"
)
