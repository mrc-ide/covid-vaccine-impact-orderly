if(!is.na(seed)){
  set.seed(seed)
}

###Load data:
fig3_df_weekly <- loadCounterfactualData("No Vaccines",
                                         group_by = c("iso3c", "week")) %>%
  filter(week > "2020-12-01")

fig3_df_overall <- loadCounterfactualData("No Vaccines",
                                          group_by = c("iso3c"))


fig3_df_income_weekly <- loadCounterfactualData("No Vaccines",
                                                group_by = c("income_group", "week")) %>%
  filter(week > "2020-12-01")

fig3_df_income_overall <- loadCounterfactualData("No Vaccines",
                                                 group_by = c("income_group"))

#standardise by country
fig3_df_standard <- fig3_df_weekly %>%
  right_join(
    expand.grid(week = unique(fig3_df_weekly$week),
                iso3c = unique(fig3_df_weekly$iso3c))
  ) %>%
  mutate(
    averted_deaths_avg = if_else(is.na(averted_deaths_avg), 0, averted_deaths_avg)
  ) %>%
  left_join(
    readRDS(
      "who_region.Rds"
    )
  ) %>%
  #assume <0 = 0
  mutate(averted_deaths_avg = if_else(averted_deaths_avg < 0, 0, averted_deaths_avg)) %>%
  group_by(iso3c) %>%
  mutate(averted_deaths_avg_std = averted_deaths_avg/sum(averted_deaths_avg),
         averted_deaths_avg_std = if_else(
           is.nan(averted_deaths_avg_std),
           0,
           averted_deaths_avg_std
         )) %>%
  arrange(who_region) %>%
  ungroup()

##plot tile plot
#first we get the break etc
startIndex <- 0
gapDist <- 3
regions <- unique(fig3_df_standard$who_region)
breaks <- rep(NA, length(unique(fig3_df_standard$iso3c)))
for(i in 1:length(regions)){
  regionSize <- length(fig3_df_standard %>% filter(who_region == regions[i]) %>% pull(iso3c) %>% unique())
  breaks[startIndex + (1:regionSize)] <- gapDist*(i-1) + startIndex + (1:regionSize)
  startIndex <- startIndex + regionSize
}

fig3_df_plot <- fig3_df_standard %>%
  rowwise() %>%
  mutate(aux = breaks[which(iso3c == unique(fig3_df_standard$iso3c))])

fig3_who_labels <- fig3_df_plot %>%
  group_by(who_region) %>%
  summarise(
    line1s = min(aux),
    line1e = max(aux),
    aux = mean(aux)
  )

plotfrom <- as.Date("2020-12-01")
plotto <- max(fig3_df_plot$week) + 4

fig3_tile <- ggplot(fig3_df_plot, aes(x=week, y=aux, fill=averted_deaths_avg_std))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  #scale_fill_viridis_c()+
  scale_y_discrete(name="", expand=c(0,0)) +
  scale_x_date(name="Week", limits=as.Date(c(plotfrom-2, plotto)), expand=c(0,0),
               date_breaks = "2 weeks",
               date_labels = "%y-%b") +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_text(colour="Black"),
        plot.margin = unit(c(1, 0, 1, 0), "cm")) +
  geom_text(
    inherit.aes = FALSE,
    aes(x = plotfrom, y = aux, label = who_region),
    size = 3,
    data = fig3_who_labels,
    alpha = 0.5,
    angle = 90
  ) +
  geom_segment(
    inherit.aes = FALSE,
    aes(x = plotfrom+2, xend = plotfrom+2,
        y = line1s, yend = line1e),
    alpha = 0.5,
    data = fig3_who_labels,
  )

fig3_iso3c <-
  ggplot(fig3_df_plot, aes(x=0, y=aux, label = iso3c)) +
  geom_text(
  ) +
  theme_classic()+
  labs(x = "", y = "") +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.line.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(1, 0, 1, 0), "cm")) +
  scale_y_discrete(expand=expansion(0.0025))

fig3_col <- ggplot(fig3_df_overall %>%
                     left_join(
                       fig3_df_plot %>%
                         select(iso3c, aux) %>%
                         distinct()
                     ) %>%
                     mutate(averted_deaths_avg = if_else(averted_deaths_avg < 0, 0, averted_deaths_avg)) %>%
                     mutate(death_prop = averted_deaths_avg/sum(averted_deaths_avg)),
                   aes(x=averted_deaths_avg, y=aux, fill=death_prop))+
  geom_col(show.legend=FALSE, orientation = "y")+
  theme_classic()+
  #scale_fill_viridis_c()+
  scale_x_continuous(name="Total deaths averted",
                     labels = function(x){format(x, scientific = FALSE,
                                                 big.mark = ",")}) +
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"),
        plot.margin = unit(c(1, 0, 1, 0), "cm")) +
  scale_y_discrete(expand=c(0,0))

dir.create("plots", showWarnings = FALSE)
saveRDS(plot_grid(fig3_tile, fig3_iso3c, fig3_col, align="h", rel_widths=c(1,0.1,0.5),
                  nrow = 1), "plots/tile_plot.Rds")

##tile plot for income groups
fig3_df_income_standard <- fig3_df_income_weekly %>%
  right_join(
    expand.grid(week = unique(fig3_df_income_weekly$week),
                income_group = unique(fig3_df_income_weekly$income_group))
  ) %>%
  mutate(
    averted_deaths_avg = if_else(is.na(averted_deaths_avg), 0, averted_deaths_avg)
  ) %>%
  #assume <0 = 0
  mutate(averted_deaths_avg = if_else(averted_deaths_avg < 0, 0, averted_deaths_avg)) %>%
  group_by(income_group) %>%
  mutate(averted_deaths_avg_std = averted_deaths_avg/sum(averted_deaths_avg),
         averted_deaths_avg_std = if_else(
           is.nan(averted_deaths_avg_std),
           0,
           averted_deaths_avg_std
         )) %>%
  arrange(income_group) %>%
  ungroup()


fig3_income_tile <- ggplot(fig3_df_income_standard, aes(x=week, y=income_group, fill=averted_deaths_avg_std))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0),
                   limits = rev(levels(fig3_df_income_standard$income_group))) +
  scale_x_date(name="Week", limits=as.Date(c(plotfrom-2, plotto)), expand=c(0,0),
               date_breaks = "2 weeks",
               date_labels = "%y-%b") +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_text(colour="Black"),
        plot.margin = unit(c(1, 0, 1, 0), "cm"))

fig3_income_col <- ggplot(fig3_df_income_overall %>%
                            mutate(averted_deaths_avg = if_else(averted_deaths_avg < 0, 0, averted_deaths_avg)) %>%
                            mutate(death_prop = averted_deaths_avg/sum(averted_deaths_avg)),
                          aes(x=averted_deaths_avg, y=income_group, fill=death_prop))+
  geom_col(show.legend=FALSE, orientation = "y")+
  theme_classic()+
  #scale_fill_viridis_c()+
  scale_x_continuous(name="Total deaths averted",
                     labels = function(x){format(x, scientific = FALSE,
                                                 big.mark = ",")}) +
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"),
        plot.margin = unit(c(1, 0, 1, 0), "cm")) +
  scale_y_discrete(expand=c(0,0),
                   limits = rev(levels(fig3_df_income_standard$income_group)))

saveRDS(plot_grid(fig3_income_tile, fig3_income_col, align="h", rel_widths=c(1,0.5),
                  nrow = 1), "plots/tile_plot_income.Rds")
