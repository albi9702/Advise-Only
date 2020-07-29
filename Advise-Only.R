library(tidyverse)


# IMPORT ------------------------------------------------------------------


#-- Set Working Directory
#setwd("G:\Il mio Drive\Università\Data Science\1° Anno\Data Science Lab\Progetto\Advise Only")
setwd("C:\\root\\Universita\\dslab\\project")


#-- Read Original File
client_clustering_original <- read_csv("DatasetClientClustering.csv") %>%
  select(-c(1:8)) %>%
  replace_na(list(Prov = 'NA')) %>%
  mutate(IncomeHighLow = recode(IncomeHighLow,
                                `1` = 'high', `0` = 'low'),
         Sex = recode(Sex, `1` = 'male', `0` = 'female'),
         NoTrustInBanks = recode(NoTrustInBanks, `1` = 'yes', `0` = 'no'),
         PanicMood = recode(PanicMood, `-1` = 'yes', `0` = 'no'))

colnames(client_clustering_original) <- colnames(client_clustering_original) %>%
  snakecase::to_snake_case(sep_out = '_')


#-- Ci sono 20 persone che non investono
client_clustering_original %>%
  mutate(sum_perc_inv = bond_investments + equity_investments + money_market_investments + other_investments + cash,
         categ_perc = ifelse(sum_perc_inv == 0, 'x=0',
                             ifelse(sum_perc_inv > 0 & sum_perc_inv < 1, '0<x<1',
                                    ifelse(sum_perc_inv == 1, 'x=1',
                                           ifelse(ifelse(sum_perc_inv > 1, 'x>1',
                                                         NA)))))) %>%
  group_by(categ_perc) %>%
  summarise(row_number(categ_perc))

#1      > 0 & < 1   = 0   > 1
#2948   1246        20    786

#se somma a 0 significa che non investono, se non somma a 100 e un errore,
#quindi si deve normalizzare per riga


client_clustering_original <- client_clustering_original %>%
  mutate(is_investor = ifelse(bond_investments + equity_investments + money_market_investments
                              + other_investments + cash == 0, "no", "yes"))

client_clustering_original %>%
  arrange(au_m) %>%
  filter(is_investor == 'no') %>%
  select(au_m)
#quando non sommano a normalizziamo o facciamo diviso la somma?

client_all_vars <- client_clustering_original %>%
  select(client_id, bond_investments, equity_investments,
         money_market_investments, other_investments, cash,
         is_investor) %>%
  gather(key = 'key', value = 'value',
         bond_investments, equity_investments,
         money_market_investments, other_investments, cash) %>%
  group_by(client_id) %>%
  mutate(tot = sum(value),
         value = ifelse(is_investor == 'yes', scales::rescale(value), 0#value/tot, 0
         )
  ) %>%
  as_tibble() %>%
  select(-tot) %>%
  spread(key = key, value = value) %>%
  inner_join(select(client_clustering_original,
                    -c(bond_investments, equity_investments, is_investor,
                       money_market_investments, other_investments, cash)),
             by = 'client_id')


# Explanatory Analysis ----------------------------------------------------


#-- NA is not a missing values (NA = Napoli)
#-- Change this
funModeling::df_status(client_all_vars)
summary(client_all_vars)


#-- Check PortfolioRisk: Outliers??
boxplot(client_all_vars$portfolio_risk)
hist(client_all_vars$portfolio_risk, breaks = 100)
plot(density(client_all_vars$portfolio_risk))
#-- Chiedere i valori del Portfolio Risk (non si capisce)



library(caret)
caret::nzv(client_all_vars) #-- 23 = panic_mood
client_all_vars$panic_mood %>%
  table()
#-- nessuno che non investe e che non va in panico
table(client_all_vars$is_investor, client_all_vars$panic_mood)

#-- nessuno che non investe e che non si fida delle banche
table(client_all_vars$is_investor, client_all_vars$no_trust_in_banks)

#-- Variabili non Considerate
client <- client_all_vars %>%
  select(-c(prov, panic_mood, is_investor,
            sex, client_date_start, no_trust_in_banks))

p_variables <- c('ClientID', 'RiskPropension',
                 'ClientInvestmentHorizon','ClientKnowledgeExperience',
                 'ClientPotentialIndex', 'IncomeHighLow',
                 'Sex', 'Age', 'IncomeNeed', 'LongTermCareNeed',
                 'ProtectionNeed', 'PensionNeed',
                 'InheritanceIndex', 'PanicMood',
                 'ClientDateStart', 'NoTrustInBanks') %>%
  snakecase::to_snake_case(sep_out = '_')

i_variables <- c('ClientID', 'PortfolioRisk', 'PortfolioHorizon', 'AuM',
                 'BondInvestments', 'EquityInvestments',
                 'MoneyMarketInvestments', 'OtherInvestments','Cash', "IsInvestor") %>%
  snakecase::to_snake_case(sep_out = '_')


client_personal <- client %>%
  select(which(colnames(client) %in% p_variables)) %>%
  mutate_if(is.character, list(as.factor))

client_investment <- client %>%
  select(which(colnames(client) %in% i_variables)) %>%
  mutate_if(is.character, list(as.factor))



library(GGally)

client_personal_data <- client_personal %>%
  mutate(income_high_low = ifelse(income_high_low == 'high', 1, 0)) %>%
  rename(`Risk Propension`=risk_propension,
         `Investment Horizon`=client_investment_horizon,
         `Experience`=client_knowledge_experience,
         `Potential Index`=client_potential_index,
         `Income`=income_high_low,
         `Age`=age,
         `Income Need`=income_need,
         `Care Need`=long_term_care_need,
         `Protection Need`=protection_need,
         `Pension Need`=pension_need,
         `Inheritance Opt.Need`=inheritance_index)

ggc <- select(client_personal_data, -client_id) %>%
  ggcorr(method = c("everything", "pearson"), label = T, hjust = .6, vjust = .45,
         low = "#E74C3C", high = '#2980B9',layout.exp = 5, size = 4.5)

g <- ggplot_build(ggc)
g$data[[3]]$label = gsub("_", "\n", g$data[[3]]$label )
grid::grid.draw(ggplot_gtable(g))


client_investment_data <- client_investment %>%
  rename(`Risk Index`=portfolio_risk,
         `Implicit Horizon`=portfolio_horizon,
         `Money Invested`=au_m,
         `Bond Investment`=bond_investments,
         `Equity Investment`=equity_investments,
         `Money Market Investment`=money_market_investments,
         `Other Investment`=other_investments,
         `Cash Investment`=cash)

ggc <- select(client_investment_data, -client_id) %>%
  ggcorr(method = c("everything", "pearson"), label = T, hjust = .51,
         low = "#E74C3C", high = '#2980B9',layout.exp = 2, size = 4.5)

g <- ggplot_build(ggc)
g$data[[3]]$label = gsub("_", "\n", g$data[[3]]$label )
grid::grid.draw(ggplot_gtable(g))


#ggcorrplot(method = "circle",
#             color = c("#E74C3C" , "white", '#16A085'),#E46726
#             type = 'upper')



cross_cor_data <- expand.grid(colnames(select(client_investment_data,
                                              -client_id)),
                              colnames(select(client_personal_data,
                                              -client_id))) %>%
  as_tibble() %>%
  rename('Investment Variables' = Var1,
         'Profile Variables' = Var2)

corr_matrix_cross <- client_investment_data %>%
  inner_join(client_personal_data, by = 'client_id') %>%
  select(-client_id) %>%
  cor()

corr_matrix_cross['Cash Investment', 'Investment Horizon']
i <- 1
cross_cor_data$Correlation <- sapply(1:nrow(cross_cor_data),
                                     function (i) {
                                       corr_matrix_cross[as.character(cross_cor_data[i,][[1]]),
                                                         as.character(cross_cor_data[i,][[2]])]
                                     }, simplify = T)

cross_corr_p <- cross_cor_data %>%
  ggplot(aes(`Investment Variables`,
             `Profile Variables`, fill = Correlation)) + 
  geom_tile(color = '#ECF0F1') +
  geom_text(data = filter(cross_cor_data, abs(Correlation) >.25),
            aes(`Investment Variables`,
                `Profile Variables`,
                label = round(Correlation, 2)),
            color = 'black', size = 6) +
  scale_fill_gradient2(limits = c(-1,1), low = "#E74C3C", high = '#2980B9',
                       mid = 'white') +
  theme_minimal() +
  scale_x_discrete(labels = str_replace_all(c('Bond\nInvestment', 'Cash Investment',
                                              'Equity Investment', 'Money Market Investment',
                                              'Other Investment', 'Risk Index',
                                              'Implicit Horizon', 'Money Invested'), ' ', '\n')) +
  theme(axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        legend.key.width = unit(1.2, 'cm'),
        legend.key.height = unit(1.3, 'cm'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))
cross_corr_p

tiff(filename = str_c(getwd(),'/plot/cross_corr.tif'),
     width = 1000, height = 850, res = 85)
cross_corr_p
dev.off()
dev.new()
# OUTLIERS ----------------------------------------------------------------



dist_mat_personal_dbscan <- client_personal %>%
  select(-client_id) %>%
  mutate(income_high_low = ifelse(income_high_low == 'high', 1, 0)) %>%
  mutate_all(scales::rescale) %>%
  dist(method = 'euclidean')


#dbscan outliers
client_personal %>%
  select(-client_id) %>%
  mutate(income_high_low = ifelse(income_high_low == 'high', 1, 0)) %>%
  mutate_all(scales::rescale) %>%
  dbscan::kNNdistplot(k = log(5000))
abline(h = 0.34, lty = 2)
dbscan_personal <- dbscan::dbscan(dist_mat_personal_dbscan,
                                  eps = 0.34, minPts = log(5000))



dist_mat_inv_dbscan <- client_investment %>%
  select(-client_id) %>%
  mutate_all(scales::rescale) %>%
  dist(method = 'euclidean')


#dbscan outliers
client_investment %>%
  select(-client_id) %>%
  mutate_all(scales::rescale) %>%
  dbscan::kNNdistplot(k = log(5000))
abline(h = 0.25, lty = 2)
dbscan_investments <- dbscan::dbscan(dist_mat_personal_dbscan,
                                     eps = 0.25, minPts = log(5000))


table(dbscan_investments$cluster, dbscan_personal$cluster)

outliers_data <- tibble(Investment = ifelse(dbscan_investments$cluster == 0,
                                            'Outlier', 'No outliers'),
                        Profile = ifelse(dbscan_personal$cluster == 0,
                                         'Outlier', 'No outliers'))


outliers_data %>%
  group_by(Profile) %>%
  summarise(Frequency = n(),
            Percentage = n()/50)
outliers_data %>%
  group_by(Investment) %>%
  summarise(Frequency = n(),
            Percentage = n()/50)

# CA - PERSONAL VARIABLES -------------------------------------------------

library(cluster)
library(fpc)
library(kmed)



client_personal <- client_personal %>%
  mutate(is_outlier = ifelse(dbscan_personal$cluster == 0, 'yes', 'no'))

client_pers_no_outliers <- client_personal %>%
  filter(is_outlier == 'no')

dist_mat_personal <- client_pers_no_outliers %>%
  select(-client_id, -is_outlier) %>%
  mutate(income_high_low = ifelse(income_high_low == 'high', 1, 0)) %>%
  mutate_all(scales::rescale, to = c(0,1)) %>%
  dist(method = "euclidean")

#diss_mat_personal <- client_personal_fct %>%
#  daisy(metric = "gower")
#
#dist_mat_personal <- diss_mat_personal
#class(dist_mat_personal) <- 'dist'


hier_partition_pers <- hclust(dist_mat_personal,
                              method = 'ward.D2')

sd <- 1234
list_out_pers <- list()
list_clustering_personal <- list()
i <- 1
for (k in 3:8) {
  
  groups_hier <- cutree(hier_partition_pers, k = k)
  stats <- cluster.stats(dist_mat_personal,
                         groups_hier, silhouette = T)
  
  list_clustering_personal[[paste('ward-', k, collapse = '')]] <- groups_hier
  
  list_out_pers[[i]] <- tibble(
    k = k,
    algorithm = 'ward',
    dunn2 = stats$dunn2,
    wb = stats$wb.ratio,
    entropy = stats$entropy,
    pearsongamma = stats$pearsongamma,
    silhouette = stats$avg.silwidth,
    ch = stats$ch,
    Connectivity = clValid::connectivity(dist_mat_personal, groups_hier)
  )
  
  
  i <- i + 1
  
  set.seed(sd)
  sota_al <- client_pers_no_outliers %>%
    select(-client_id, -is_outlier) %>%
    mutate(income_high_low = ifelse(income_high_low == 'high', 1, 0)) %>%
    mutate_all(scales::rescale, to = c(0,1)) %>%
    as.matrix() %>%
    clValid::sota(maxCycles = k-1, maxEpochs = 1200)
  
  
  stats <- cluster.stats(dist_mat_personal,
                         sota_al$clust, silhouette = T)
  list_clustering_personal[[paste('sota-', k, collapse = '')]] <- sota_al
  list_out_pers[[i]] <- tibble(
    k = k,
    algorithm = 'sota',
    dunn2 = stats$dunn2,
    wb = stats$wb.ratio,
    entropy = stats$entropy,
    pearsongamma = stats$pearsongamma,
    silhouette = stats$avg.silwidth,
    ch = stats$ch,
    Connectivity = clValid::connectivity(dist_mat_personal, sota_al$clust)
  )
  i <- i+1
  
  #-- Medoide
  set.seed(sd)
  km <- pam(dist_mat_personal, k = k)
  groups_kmed <- km$clustering
  #groups_kmed <- km$cluster
  
  kmed_stats <- cluster.stats(dist_mat_personal,
                              groups_kmed, silhouette = T)
  
  list_clustering_personal[[paste('pam-', k, collapse = '')]] <- km
  
  list_out_pers[[i]] <- tibble(
    k = k,
    algorithm = 'pam',
    dunn2 = kmed_stats$dunn2,
    wb = kmed_stats$wb.ratio,
    entropy = kmed_stats$entropy,
    pearsongamma = kmed_stats$pearsongamma,
    silhouette = kmed_stats$avg.silwidth,
    ch = kmed_stats$ch,
    Connectivity = clValid::connectivity(dist_mat_personal, groups_kmed)
  )
  
  i <- i + 1
  
  #-- kmeans
  set.seed(sd)
  km <- kmeans(dist_mat_personal,
               k, iter.max = 800)
  groups_kmed <- km$cluster
  
  kmed_stats <- cluster.stats(dist_mat_personal,
                              groups_kmed, silhouette = T)
  
  list_clustering_personal[[paste('kmeans-', k,collapse = '')]] <- km
  
  list_out_pers[[i]] <- tibble(
    k = k,
    algorithm = 'kmean',
    dunn2 = kmed_stats$dunn2,
    wb = kmed_stats$wb.ratio,
    entropy = kmed_stats$entropy,
    pearsongamma = kmed_stats$pearsongamma,
    silhouette = kmed_stats$avg.silwidth,
    ch = kmed_stats$ch,
    Connectivity = clValid::connectivity(dist_mat_personal, groups_kmed)
  )
  print(k)
  i <- i + 1
}

save(list_clustering_personal, list_out_pers,
     file = str_c(getwd(), '/objects/list_clustering_personal.RData'))
load(file = str_c(getwd(), '/objects/list_clustering_personal.RData'))

cluster_stats_personal <- bind_rows(list_out_pers) %>%
  as_tibble()
cluster_stats_personal <- gather(cluster_stats_personal, key = 'metric', value = 'value',
                                 silhouette, ch, wb, dunn2, pearsongamma,
                                 entropy, Connectivity)

#       connectivity, silhouette, dunn)
personal_metrics <- cluster_stats_personal %>%
  filter(!(metric %in% c('wb','dunn2'))) %>%
  rename(Algorithm = algorithm) %>%
  mutate(metric = recode(metric, 'silhouette' = 'Silhouette', 'dunn2' = 'Dunn',
                         'entropy' = 'Entropy', 'pearsongamma' = 'Pearsongamma',
                         'wb' = 'WB Ratio', 'ch' = 'Calinski-Harabasz'))

ggplot(personal_metrics, aes(x = k, y = value, color = Algorithm)) +
  geom_line(aes(group = Algorithm)) +
  geom_point(aes(group = Algorithm)) +
  facet_wrap(.~metric, ncol=2, scale="free") +
  scale_color_manual(values = c('#2E86C1', '#F39C12', '#E74C3C', '#27AE60')) +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),#element_text(size = 17),
        axis.text = element_text(size = 15),
        legend.position = 'bottom',
        axis.ticks.x = element_line(size = 1, colour = 'grey')) +
  scale_x_continuous(labels = 3:8)


#silhouette
p_sil <- ggplot(filter(personal_metrics, metric == 'Silhouette'),
                aes(x = k, y = value, color = Algorithm)) +
  geom_line(aes(group = Algorithm)) +
  geom_point(aes(group = Algorithm)) +
  #facet_wrap(.~metric, ncol=2, scale="free") +
  scale_color_manual(values = c('#2E86C1', '#F39C12', '#E74C3C', '#27AE60')) +
  theme_minimal()  +
  ggtitle('Silhouette') +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),#element_text(size = 17),
        axis.text = element_text(size = 15),
        legend.position = 'none',
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 14),
        axis.ticks.x = element_line(size = 1, colour = 'grey')) +
  scale_x_continuous(labels = 3:8) +
  scale_y_continuous(limits = c(.15,.3),
                     labels = str_pad(seq(.15,.3, by = .025), 5, 'right', '0') ,
                     breaks = seq(.15,.3, by = .025))

#Connectivity
p_con <- ggplot(filter(personal_metrics, metric == 'Connectivity'),
                aes(x = k, y = value, color = Algorithm)) +
  geom_line(aes(group = Algorithm)) +
  geom_point(aes(group = Algorithm)) +
  scale_color_manual(values = c('#2E86C1', '#F39C12', '#E74C3C', '#27AE60')) +
  theme_minimal()  +
  ggtitle('Connectivity') +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),#element_text(size = 17),
        axis.text = element_text(size = 15),
        legend.position = 'none',
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 14),
        axis.ticks.x = element_line(size = 1, colour = 'grey')) +
  scale_x_continuous(labels = 3:8) +
  scale_y_continuous(limits = c(0,1600),
                     labels = seq(0,1600, by = 400),
                     breaks = seq(0,1600, by = 400))

#Pearsongamma
p_pg <- ggplot(filter(personal_metrics, metric == 'Pearsongamma'),
               aes(x = k, y = value, color = Algorithm)) +
  geom_line(aes(group = Algorithm)) +
  geom_point(aes(group = Algorithm)) +
  scale_color_manual(values = c('#2E86C1', '#F39C12', '#E74C3C', '#27AE60')) +
  theme_minimal()  +
  ggtitle('Pearsongamma') +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),#element_text(size = 17),
        axis.text = element_text(size = 15),
        legend.position = 'none',
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 14),
        axis.ticks.x = element_line(size = 1, colour = 'grey')) +
  scale_x_continuous(labels = 3:8) +
  scale_y_continuous(limits = c(.4,.65),
                     labels = str_pad(seq(.4,.65, by = .05), 4, 'right', '0'),
                     breaks = seq(.4,.65, by = .05))


#ch
p_ch <- ggplot(filter(personal_metrics, metric == 'Calinski-Harabasz'),
               aes(x = k, y = value, color = Algorithm)) +
  geom_line(aes(group = Algorithm)) +
  geom_point(aes(group = Algorithm)) +
  scale_color_manual(values = c('#2E86C1', '#F39C12', '#E74C3C', '#27AE60')) +
  theme_minimal()  +
  ggtitle('Calinski-Harabasz') +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),#element_text(size = 17),
        axis.text = element_text(size = 15),
        legend.position = 'left',
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 24),
        axis.ticks.x = element_line(size = 1, colour = 'grey')) +
  scale_x_continuous(labels = 3:8) +
  scale_y_continuous(limits = c(1200, 2000),
                     labels = seq(1200, 2000, by = 200),
                     breaks = seq(1200, 2000, by = 200))

tiff(filename = str_c(getwd(),'/plot/legend_pers.tif'),width = 1200, height = 900, res = 180)
p_ch
dev.off()
dev.new()

library(cowplot)
stats_pers_p <- ggdraw() +
  draw_plot(p_sil, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(p_ch, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(p_con, x = 0, y = 0, width = .5, height = .5) +
  draw_plot(p_pg, x = .5, y = 0, width = .5, height = .5)
stats_pers_p
#kmeans o sota
tiff(filename = str_c(getwd(),'/plot/valid_pers.tif'),
     width = 1200, height = 900, res = 180)
stats_pers_p
dev.off()
dev.new()
# CA - INVESTMENT VARIABLES -------------------------------------------------


client_investment <- client_investment %>%
  mutate(is_outlier = ifelse(dbscan_personal$cluster == 0, 'yes', 'no'))

client_inv_no_outliers <- client_investment %>%
  filter(is_outlier == 'no')

dist_mat_investment <- client_inv_no_outliers %>%
  select(-client_id, -is_outlier) %>%
  mutate_all(scales::rescale, to = c(0,1)) %>%
  dist(method = "euclidean")


hier_partition_inv <- hclust(dist_mat_investment,
                             method = 'ward.D2')

sd <- 1234
list_out_inv <- list()
list_clustering_inv <- list()
i <- 1
for (k in 3:8) {
  
  groups_hier <- cutree(hier_partition_inv, k = k)
  stats <- cluster.stats(dist_mat_investment,
                         groups_hier, silhouette = T)
  
  list_clustering_inv[[paste('ward-', k, collapse = '')]] <- groups_hier
  
  list_out_inv[[i]] <- tibble(
    k = k,
    algorithm = 'ward',
    dunn2 = stats$dunn2,
    wb = stats$wb.ratio,
    entropy = stats$entropy,
    pearsongamma = stats$pearsongamma,
    silhouette = stats$avg.silwidth,
    ch = stats$ch,
    Connectivity = clValid::connectivity(dist_mat_investment, groups_hier))
  i <- i + 1
  
  set.seed(sd)
  sota_al <- client_inv_no_outliers %>%
    select(-client_id, -is_outlier) %>%
    mutate_all(scales::rescale, to = c(0,1)) %>%
    as.matrix() %>%
    clValid::sota(maxCycles = k-1, maxEpochs = 1200)
  
  
  stats <- cluster.stats(dist_mat_investment,
                         sota_al$clust, silhouette = T)
  list_clustering_inv[[paste('sota-', k, collapse = '')]] <- sota_al
  list_out_inv[[i]] <- tibble(
    k = k,
    algorithm = 'sota',
    dunn2 = stats$dunn2,
    wb = stats$wb.ratio,
    entropy = stats$entropy,
    pearsongamma = stats$pearsongamma,
    silhouette = stats$avg.silwidth,
    ch = stats$ch,
    Connectivity = clValid::connectivity(dist_mat_investment, sota_al$clust)
  )
  i <- i+1
  
  #-- Medoide
  set.seed(sd)
  km <- pam(dist_mat_investment, k = k)
  groups_kmed <- km$clustering
  
  kmed_stats <- cluster.stats(dist_mat_investment,
                              groups_kmed, silhouette = T)
  
  list_clustering_inv[[paste('pam-', k, collapse = '')]] <- km
  
  list_out_inv[[i]] <- tibble(
    k = k,
    algorithm = 'pam',
    dunn2 = kmed_stats$dunn2,
    wb = kmed_stats$wb.ratio,
    entropy = kmed_stats$entropy,
    pearsongamma = kmed_stats$pearsongamma,
    silhouette = kmed_stats$avg.silwidth,
    ch = kmed_stats$ch,
    Connectivity = clValid::connectivity(dist_mat_investment,
                                         groups_kmed)
  )
  
  i <- i + 1
  
  #-- kmeans
  set.seed(sd)
  km <- kmeans(dist_mat_investment,
               k, iter.max = 800)
  groups_kmed <- km$cluster
  
  kmed_stats <- cluster.stats(dist_mat_investment,
                              groups_kmed, silhouette = T)
  
  list_clustering_inv[[paste('kmeans-', k, collapse = '')]] <- km
  
  list_out_inv[[i]] <- tibble(
    k = k,
    algorithm = 'kmean',
    dunn2 = kmed_stats$dunn2,
    wb = kmed_stats$wb.ratio,
    entropy = kmed_stats$entropy,
    pearsongamma = kmed_stats$pearsongamma,
    silhouette = kmed_stats$avg.silwidth,
    ch = kmed_stats$ch,
    Connectivity = clValid::connectivity(dist_mat_investment,
                                         groups_kmed)
  )
  print(k)
  i <- i + 1
}

save(list_clustering_inv, list_out_inv,
     file = str_c(getwd(), '/objects/list_clustering_investment.RData'))
load(file = str_c(getwd(), '/objects/list_clustering_investment.RData'))

cluster_stats_investment <- bind_rows(list_out_inv) %>%
  as_tibble()
cluster_stats_investment <- gather(cluster_stats_investment, key = 'metric', value = 'value',
                                   silhouette, ch, wb, dunn2, pearsongamma,
                                   entropy, Connectivity)

#       connectivity, silhouette, dunn)
investment_metrics <- cluster_stats_investment %>%
  filter(!(metric %in% c('wb','dunn2'))) %>%
  rename(Algorithm = algorithm) %>%
  mutate(metric = recode(metric, 'silhouette' = 'Silhouette', 'dunn2' = 'Dunn',
                         'entropy' = 'Entropy', 'pearsongamma' = 'Pearsongamma',
                         'wb' = 'WB Ratio', 'ch' = 'Calinski-Harabasz'))

ggplot(investment_metrics, aes(x = k, y = value, color = Algorithm)) +
  geom_line(aes(group = Algorithm)) +
  geom_point(aes(group = Algorithm)) +
  facet_wrap(.~metric, ncol=2, scale="free") +
  scale_color_manual(values = c('#2E86C1', '#F39C12', '#E74C3C', '#27AE60')) +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),#element_text(size = 17),
        axis.text = element_text(size = 15),
        legend.position = 'bottom',
        axis.ticks.x = element_line(size = 1, colour = 'grey')) +
  scale_x_continuous(labels = 3:8)


#silhouette
p_sil <- ggplot(filter(investment_metrics, metric == 'Silhouette'),
                aes(x = k, y = value, color = Algorithm)) +
  geom_line(aes(group = Algorithm)) +
  geom_point(aes(group = Algorithm)) +
  #facet_wrap(.~metric, ncol=2, scale="free") +
  scale_color_manual(values = c('#2E86C1', '#F39C12', '#E74C3C', '#27AE60')) +
  theme_minimal()  +
  ggtitle('Silhouette') +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),#element_text(size = 17),
        axis.text = element_text(size = 15),
        legend.position = 'none',
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 14),
        axis.ticks.x = element_line(size = 1, colour = 'grey')) +
  scale_x_continuous(labels = 3:8) +
  scale_y_continuous(limits = c(.3,.55),
                     labels = str_pad(seq(.3,.55, by = .05), 4, 'right', '0') ,
                     breaks = seq(.3,.55, by = .05))
p_sil


#Connectivity
p_con <- ggplot(filter(investment_metrics, metric == 'Connectivity'),
                aes(x = k, y = value, color = Algorithm)) +
  geom_line(aes(group = Algorithm)) +
  geom_point(aes(group = Algorithm)) +
  scale_color_manual(values = c('#2E86C1', '#F39C12', '#E74C3C', '#27AE60')) +
  theme_minimal()  +
  ggtitle('Connectivity') +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),#element_text(size = 17),
        axis.text = element_text(size = 15),
        legend.position = 'none',
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 14),
        axis.ticks.x = element_line(size = 1, colour = 'grey')) +
  scale_x_continuous(labels = 3:8) +
  scale_y_continuous(limits = c(0,800),
                     labels = seq(0,800, by = 200),
                     breaks = seq(0,800, by = 200))
p_con
#Pearsongamma
p_pg <- ggplot(filter(investment_metrics, metric == 'Pearsongamma'),
               aes(x = k, y = value, color = Algorithm)) +
  geom_line(aes(group = Algorithm)) +
  geom_point(aes(group = Algorithm)) +
  scale_color_manual(values = c('#2E86C1', '#F39C12', '#E74C3C', '#27AE60')) +
  theme_minimal()  +
  ggtitle('Pearsongamma') +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),#element_text(size = 17),
        axis.text = element_text(size = 15),
        legend.position = 'none',
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 14),
        axis.ticks.x = element_line(size = 1, colour = 'grey')) +
  scale_x_continuous(labels = 3:8) +
  scale_y_continuous(limits = c(.4,.8),
                     labels = str_pad(seq(.4,.8, by = .1), 3, 'right', '0'),
                     breaks = seq(.4,.8, by = .1))
p_pg

#ch
p_ch <- ggplot(filter(investment_metrics, metric == 'Calinski-Harabasz'),
               aes(x = k, y = value, color = Algorithm)) +
  geom_line(aes(group = Algorithm)) +
  geom_point(aes(group = Algorithm)) +
  scale_color_manual(values = c('#2E86C1', '#F39C12', '#E74C3C', '#27AE60')) +
  theme_minimal()  +
  ggtitle('Calinski-Harabasz') +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),#element_text(size = 17),
        axis.text = element_text(size = 15),
        legend.position = 'right',
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 24),
        axis.ticks.x = element_line(size = 1, colour = 'grey')) +
  scale_x_continuous(labels = 3:8) +
  scale_y_continuous(limits = c(900, 5000),
                     labels = seq(1000, 5000, by = 1000),
                     breaks = seq(1000, 5000, by = 1000))
p_ch

tiff(filename = str_c(getwd(),'/plot/legend_inv.tif'),width = 1200, height = 900, res = 180)
p_ch
dev.off()
dev.new()


library(cowplot)
stats_inv_p <- ggdraw() +
  draw_plot(p_sil, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(p_ch, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(p_con, x = 0, y = 0, width = .5, height = .5) +
  draw_plot(p_pg, x = .5, y = 0, width = .5, height = .5)
stats_inv_p
#kmean 2 che ha silhouette piu bassa ma comunque una buona conn e un buon
#pearsongamma
tiff(filename = str_c(getwd(),'/plot/valid_inv.tif'),
     width = 1200, height = 900, res = 180)
stats_inv_p
dev.off()
dev.new()

str_detect(names(list_clustering_inv), '.*3$')
names(list_clustering_inv)[[1]]
table(list_clustering_inv[[1]])

names(list_clustering_inv)[[2]]
table(list_clustering_inv[[2]]$clust)

names(list_clustering_inv)[[3]]
table(list_clustering_inv[[3]]$clustering)

names(list_clustering_inv)[[4]]
table(list_clustering_inv[[4]]$cluster)

# RANDOM POSITION TEST ----------------------------------------------------

H0_cluster_index <- c()
for (r in 1:knime.flow.in[['n_samples']]) {
  
  X_qualit <- as.data.frame(sapply(input_mc_qualit,
                                   function(x) {
                                     #set.seed(randoms[r])
                                     #set.seed(r+1)
                                     sample(unique(x), n_records, replace = T)}
  ))
  X_quantit <- as.data.frame(sapply(input_mc_quantit,
                                    function(x) {
                                      #set.seed(randoms[r])
                                      #scales::rescale(rexp(n_records, rate = 1),
                                      #                     c(0,1))
                                      #set.seed(r+1)
                                      runif(n_records, 0, 1)
                                    }
  ))
  
  X <- bind_cols(X_qualit, X_quantit) %>%
    as.data.frame()
  
  dist_mat_X <- daisy(X, metric = "gower")
  
  hier_X <- hclust(dist_mat_X, method = 'ward.D2')
  groups_hier_X <- cutree(hier_X, k = k_ger)
  
  random_stats <- cluster.stats(dist_mat_X, groups_hier_X, silhouette = T)
  
  H0_cluster_index <- c(H0_cluster_index, random_stats$avg.silwidth)  
  
}

H0_quantile <- quantile(H0_cluster_index, c(1-knime.flow.in[["alpha"]]))

dt_test <- tibble(H0 = H0_cluster_index)

ggplot(data = dt_test, aes(x = H0)) +
  geom_density(fill = '#154360', color = '#154360', alpha = .2,) +
  geom_histogram(aes(y=..density..), 
                 color = '#154360', fill = '#154360', alpha = .7,
                 binwidth = .0003) +
  geom_point(aes(x = H0_quantile, y = 10), color = 'red', size = 5) +
  geom_point(aes(x = H1_cluster_index, y = 10), color = 'green', size = 5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  xlab('Silhouette') +
  ylab('Frequenza')



# EXTERNAL VALIDATION ---------------------------------------------------------------


#library(ClusterR)
#external_methods <- c('rand_index', 'jaccard_index', 'fowlkes_mallows_index')
#
#external_measures <- sapply(external_methods,
#                            function (x) {
#                              
#                              external_validation(
#                                client_clusters$cluster_investment,
#                                client_clusters$cluster_personal,
#                                method = x,
#                                summary_stats = FALSE
#                              ) %>%
#                                as.numeric()
#                              
#                            }, simplify = T, USE.NAMES = F)
#external_measures


#0 - fare delle prove e vedere cosa è meglio rispetto alle misure esterne

#1 - caretterizzazione dei gruppi e poi studio i confronti
#es. ho un gruppo con Potenziale di crescita del cliente alto -> di questi vedo se alcuni
#appartengo al gruppo con pochi investimenti in azioni -> se ne trovo, procedo con iniziative di marketing per far si che investa in
#azioni

#2 - correlazione fra i gruppi di variabili entro i gruppi

#3 - lavorare sulla tabella di contingenza con a,b,c,d


library(ClusterR)
names_investment <- names(list_clustering_inv)
names_personal <- names(list_clustering_personal)

external_methods <- c('rand_index', 'jaccard_index',
                      'fowlkes_mallows_index', 'purity')
external_validation(list_clustering_personal[[name_p]],
                    list_clustering_inv[[name_i]],
                    method = 'purity',
                    summary_stats = FALSE)
i <- 1
external_methods_list <- list()
for (name_i in names_investment) {
  #print(name_i)
  k_i <- as.numeric(str_extract(name_i, '[0-9]'))
  for (name_p in names_personal) {
    #print(name_p)
    k_p <- as.numeric(str_extract(name_p, '[0-9]'))
    metrics <- sapply(external_methods,
                      function (x) {
                        
                        inv_clusters <- list_clustering_inv[[name_i]]
                        if (str_detect(name_i,'^(pam).*')) {
                          inv_clusters <- list_clustering_inv[[name_i]]$clustering
                        } else if (str_detect(name_i,'^((kmeans)).*')) {
                          inv_clusters <- list_clustering_inv[[name_i]]$cluster
                        } else if (str_detect(name_i,'^((sota)).*')) {
                          inv_clusters <- list_clustering_inv[[name_i]]$clust
                        }
                        
                        pers_clusters <- list_clustering_personal[[name_p]]
                        if (str_detect(name_p,'^(pam).*')) {
                          pers_clusters <- list_clustering_personal[[name_p]]$clustering
                        } else if (str_detect(name_p,'^((kmeans)).*')) {
                          pers_clusters <- list_clustering_personal[[name_p]]$cluster
                        } else if (str_detect(name_p,'^((sota)).*')) {
                          pers_clusters <- list_clustering_personal[[name_p]]$clust
                        }
                        external_validation(
                          inv_clusters,
                          pers_clusters,
                          method = x,
                          summary_stats = FALSE
                        ) %>%
                          as.numeric()
                        
                      }, simplify = T, USE.NAMES = F)
    tib_out <- tibble(method_investment = str_extract(name_i, '[^0-9- ]+'),
                      k_investment = str_extract(name_i, '[0-9]'),
                      method_personal = str_extract(name_p, '[^0-9- ]+'),
                      k_personal = str_extract(name_p, '[0-9]'),
                      measure = c('rand_index',
                                  'jaccard_index',
                                  'fowlkes_mallows_index',
                                  'purity'),
                      value = metrics)
    external_methods_list[[i]] <- tib_out
    i <- i + 1
  }
  
}

external_methods_data <- external_methods_list %>%
  bind_rows() %>%
  filter(k_investment == 3 & k_personal == 3)


fm_data <- filter(external_methods_data,measure == 'fowlkes_mallows_index')
midpoint <- (max(fm_data$value)+min(fm_data$value))/2
fm_p <- ggplot(fm_data,
               aes(x = method_personal, y = method_investment, fill = value)) +
  geom_tile(color = '#ECF0F1') +
  geom_text(data = filter(fm_data, abs(value) >.44),
            aes(x = method_personal, y = method_investment,label = round(value, 2)),
            color = 'black', size = 6) +
  theme_minimal() +
  scale_fill_gradient2(low = "#D6EAF8", high = '#1B4F72',
                       mid = '#5499C7', midpoint = midpoint,
                       limits = c(min(fm_data$value), max(fm_data$value))) +
  theme(axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        legend.key.width = unit(1.2, 'cm'),
        legend.key.height = unit(1.3, 'cm'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 23),
        legend.position = 'left') +
  xlab('Profile') +
  ylab('Investment') +
  ggtitle('Fowlkes Mallows Index')
fm_p

j_data <- filter(external_methods_data,measure == 'jaccard_index')
midpoint <- (max(j_data$value)+min(j_data$value))/2
j_p <- ggplot(j_data,
              aes(x = method_personal, y = method_investment, fill = value)) +
  geom_tile(color = '#ECF0F1') +
  geom_text(data = filter(j_data, abs(value) >.29),
            aes(x = method_personal, y = method_investment,label = round(value, 2)),
            color = 'black', size = 6) +
  theme_minimal() +
  scale_fill_gradient2(low = "#D6EAF8", high = '#1B4F72',
                       mid = '#5499C7', midpoint = midpoint,
                       limits = c(min(j_data$value), max(j_data$value))) +
  theme(axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        legend.key.width = unit(1.2, 'cm'),
        legend.key.height = unit(1.3, 'cm'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 23),
        legend.position = 'right') +
  xlab('Profile') +
  ylab('Investment') +
  ggtitle('Jaccard Index')
j_p


r_data <- filter(external_methods_data,measure == 'rand_index')
midpoint <- (max(r_data$value)+min(r_data$value))/2
r_p <- ggplot(r_data,
              aes(x = method_personal, y = method_investment, fill = value)) +
  geom_tile(color = '#ECF0F1') +
  geom_text(data = filter(r_data, round(abs(value)) >.51),
            aes(x = method_personal, y = method_investment,label = round(value, 2)),
            color = 'black', size = 6) +
  theme_minimal() +
  scale_fill_gradient2(low = "#D6EAF8", high = '#1B4F72',
                       mid = '#5499C7',
                       midpoint = midpoint) +
  theme(axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        legend.key.width = unit(1.2, 'cm'),
        legend.key.height = unit(1.3, 'cm'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 23),
        legend.position = 'left') +
  xlab('Profile') +
  ylab('Investment') +
  ggtitle('Rand Index')
r_p


p_data <- filter(external_methods_data,measure == 'purity')
midpoint <- (max(p_data$value)+min(p_data$value))/2
p_p <- ggplot(p_data,
              aes(x = method_personal, y = method_investment, fill = value)) +
  geom_tile(color = '#ECF0F1') +
  geom_text(data = filter(p_data, round(value) > .56),
            aes(x = method_personal, y = method_investment,label = round(value, 2)),
            color = 'black', size = 6) +
  theme_minimal() +
  scale_fill_gradient2(low = "#D6EAF8", high = '#1B4F72',
                       mid = '#5499C7',
                       midpoint = midpoint) +
  theme(axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        legend.key.width = unit(1.2, 'cm'),
        legend.key.height = unit(1.3, 'cm'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 23),
        legend.position = 'right') +
  xlab('Profile') +
  ylab('Investment') +
  ggtitle('Purity Index')
p_p


library(cowplot)
external_p <- ggdraw() +
  draw_plot(r_p, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(j_p, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(fm_p, x = 0, y = 0, width = .5, height = .5) +
  draw_plot(p_p, x = .5, y = 0, width = .5, height = .5)
external_p
#kmean 2 che ha silhouette piu bassa ma comunque una buona conn e un buon
#pearsongamma
tiff(filename = str_c(getwd(),'/plot/external.tif'),
     width = 1200, height = 900, res = 100)
external_p
dev.off()
dev.new()



# CARATTERIZZAZIONE DEI GRUPPI --------------------------------------------

# Investment --------------------------------------------------------------

#-- median personal


table(list_clustering_personal[['kmeans- 3']]$cluster,
      list_clustering_inv[['sota- 3']]$clust)

client_pers_no_outliers$cluster_personal <- list_clustering_personal[['kmeans- 3']]$cluster
client_inv_no_outliers$cluster_investment <- list_clustering_inv[['sota- 3']]$clust


client_outliers <- filter(client_investment, is_outlier == 'yes') %>%
  select(-is_outlier) %>%
  left_join(filter(client_personal, is_outlier == 'yes'), by = 'client_id')

client_clusters <-  client_pers_no_outliers %>%
  select(-is_outlier) %>%
  inner_join(client_inv_no_outliers, by = 'client_id') %>%
  bind_rows(client_outliers) %>%
  mutate(cluster_investment = ifelse(is.na(cluster_investment), 0, cluster_investment),
         cluster_personal = ifelse(is.na(cluster_personal), 0, cluster_personal))

table(client_clusters$cluster_investment,
      client_clusters$cluster_personal)



#investimenti
client_investment_gathered <- client_clusters %>%
  select(which(colnames(client_clusters) %in% c(i_variables, 'cluster_investment'))) %>%
  gather(key = 'variable', value = 'value', -client_id, -cluster_investment) %>%
  mutate(cluster_investment = as.character(cluster_investment))

client_investment_gathered_norecode <- client_investment_gathered %>%
  filter(!(variable %in% c('portfolio_risk', 'portfolio_horizon', 'au_m'))) %>%
  group_by(client_id) %>%
  mutate(tot = sum(value),
         new_value = ifelse(tot != 0, value/tot, 0)) %>%
  as.tibble() %>%
  select(-value,-tot) %>%
  rename(value = new_value) %>%
  bind_rows(filter(client_investment_gathered,
                   variable %in% c('portfolio_risk', 'portfolio_horizon', 'au_m')))
client_investment_gathered <- client_investment_gathered_norecode %>%
  mutate(variable = recode(variable,
                           'portfolio_risk' = 'Risk Index',
                           'portfolio_horizon' = 'Implicit Horizon',
                           'au_m' = 'Money Invested',
                           'bond_investments' = 'Bond Investment',
                           'equity_investments' = 'Equity Investment',
                           'money_market_investments' = 'Money Market Investment',
                           'other_investments' = 'Other Investment',
                           'cash' = 'Cash Investment'))


#client_investment_gathered <- client_investment_gathered %>%
#  filter(cluster_investment != 0) %>%
#  mutate(cluster_investment = '4') %>%
#  bind_rows(client_investment_gathered)




# Percentuali -------------------------------------------------------------
filter(client_investment_gathered, cluster_investment != 0 &
         !(variable %in% c('Risk Index', 'Implicit Horizon', 'Money Invested'))) %>%
  .$variable %>%
  unique()
paste(unique(client_investment_gathered$variable), collapse = "', '")
inv_cp <- ggplot(filter(client_investment_gathered, cluster_investment != 0 &
                          !(variable %in% c('Risk Index', 'Implicit Horizon', 'Money Invested'))),
                 aes(x = cluster_investment, y = value,
                     fill = variable)) +
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        legend.position = 'right',
        axis.ticks.x = element_line(size = 1, colour = 'grey'),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 19)
  ) +
  scale_x_discrete(breaks = 1:3) +
  ylab('% of Total Investment') +
  xlab('Cluster') +
  ggtitle('Investment') +
  scale_fill_manual(name = 'Investment Type',
                    values = c('#E74C3C', '#F39C12', '#17A589', '#2E86C1', '#34495E'))

tiff(filename = str_c(getwd(),'/plot/cluster_inv.tif'),
     width = 1400, height = 900, res = 200)
inv_cp
dev.off()
dev.new()


# Altre -------------------------------------------------------------------



options(scipen = 9999999)

aum_p <- filter(client_investment_gathered, cluster_investment != 0 &
                  variable == 'Money Invested') %>%
  rename(`Cluster` = cluster_investment) %>%
  ggplot(aes(x = as.ordered(Cluster), y = log(value),
             fill = Cluster, group = Cluster)) +
  #geom_violin(color = 'black') +
  geom_violin(alpha = .5,
              aes(color = `Cluster`), size = .6) +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        legend.position = 'right',
        axis.ticks.x = element_line(size = 1, colour = 'grey'),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 24)
  ) +
  scale_y_continuous(limits = c(9, 17),
                     labels = seq(9, 17, by = 2),
                     breaks = seq(9, 17, by = 2)) +
  scale_x_discrete(breaks = 1:3) +
  scale_color_manual(values = c('#D35400', '#273746', '#0B5345')) +
  scale_fill_manual(values = c('#F39C12', '#2E86C1', '#17A589')) +
  ylab('log(Amount Invested)') +
  xlab('Cluster') +
  ggtitle('Amount Invested')# +
#scale_fill_manual(values = c('#F39C12', '#2E86C1', '#17A589'))
aum_p




aum_p <- filter(client_investment_gathered, cluster_investment != 0 &
                  variable == 'Money Invested') %>%
  rename(`Cluster` = cluster_investment) %>%
  ggplot(aes(x = log(value), fill = `Cluster`,
             group = `Cluster`)) +
  geom_density(alpha = .5,
               aes(color = `Cluster`), size = .6) +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        #axis.title.x = element_blank(),
        legend.position = 'none',
        axis.ticks.x = element_line(size = 1, colour = 'grey'),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 19)
  ) +
  ylab('Density') +
  xlab('log(Amount Invested)') +
  ggtitle('Amount Invested') +
  scale_color_manual(values = c('#D35400', '#273746', '#0B5345')) +
  scale_fill_manual(values = c('#F39C12', '#2E86C1', '#17A589'))# +
scale_x_continuous(breaks = seq(0,.3,by = .1),
                   labels = seq(0,.3,by = .1),
                   limits = c(0,.3))
aum_p

risk_p <- filter(client_investment_gathered, cluster_investment != 0 &
                   variable == 'Risk Index') %>%
  rename(`Cluster` = cluster_investment) %>%
  ggplot(aes(x = value, fill = `Cluster`,
             group = `Cluster`)) +
  geom_density(alpha = .5,
               aes(color = `Cluster`), size = .6) +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        #axis.title.x = element_blank(),
        legend.position = 'right',
        axis.ticks.x = element_line(size = 1, colour = 'grey'),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 19)
  ) +
  ylab('Density') +
  xlab('Risk Index') +
  ggtitle('Investment - Risk Index') +
  scale_color_manual(values = c('#78281F', '#F4D03F', '#A569BD')) +
  scale_fill_manual(values = c('#E74C3C', '#9A7D0A', '#4A235A')) +
  scale_x_continuous(breaks = seq(0,.3,by = .1),
                     labels = seq(0,.3,by = .1),
                     limits = c(0,.3))# +
#annotate("rect", xmin = .18, xmax = .24, ymin = 28, ymax = 39,
#         alpha = .2) +
annotate("text", x = (.18+.24)/2, y = (22+45)/2,
         label = '15 obs. with values\ngrather than 0.3\nhave been removed',
         size = 5)
risk_p



hor_p <- filter(client_investment_gathered, cluster_investment != 0 &
                  variable == 'Implicit Horizon') %>%
  rename(`Cluster` = cluster_investment) %>%
  ggplot(aes(x = value, fill = `Cluster`,
             group = `Cluster`)) +
  geom_density(alpha = .5,
               aes(color = `Cluster`), size = .6) +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        #axis.title.x = element_blank(),
        legend.position = 'right',
        axis.ticks.x = element_line(size = 1, colour = 'grey'),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 19)
  ) +
  ylab('Density') +
  xlab('Implicit Horizon') +
  ggtitle('Investment - Implicit Horizon') +
  #scale_color_manual(values = c('#D35400', '#273746', '#0B5345')) +
  #scale_fill_manual(values = c('#F39C12', '#2E86C1', '#17A589')) +
  scale_color_manual(values = c('#78281F', '#F4D03F', '#A569BD')) +
  scale_fill_manual(values = c('#E74C3C', '#9A7D0A', '#4A235A')) +
  scale_x_continuous(breaks = seq(0,12,by = 3),
                     labels = seq(0,12,by = 3),
                     limits = c(0,12))# +
#annotate("rect", xmin = 6.5, xmax = 11.5, ymin = 1.2, ymax = 1.8,
#         alpha = .2) +
annotate("text", x = 9, y = 1.675,
         label = '10 obs. with values\ngrather than 12\nhave been removed',
         size = 5)
hor_p


invcl_p <- ggdraw() +
  draw_plot(risk_p, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(hor_p, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(aum_p, x = 0, y = 0, width = .61, height = .5)
invcl_p
#kmean 2 che ha silhouette piu bassa ma comunque una buona conn e un buon
#pearsongamma
tiff(filename = str_c(getwd(),'/plot/inv_ovars.tif'),
     width = 1200, height = 800, res = 100)
invcl_p
dev.off()
dev.new()




# Profile -----------------------------------------------------------------

client_personal_gathered_norec <- client_clusters %>%
  select(which(colnames(client_clusters) %in% c(p_variables, 'cluster_personal'))) %>%
  gather(key = 'variable', value = 'value', -client_id, -cluster_personal,
         -income_high_low) %>%
  mutate(cluster_personal = as.character(cluster_personal))

client_personal_gathered <- client_personal_gathered_norec %>%
  mutate(variable = recode(variable,
                           risk_propension = 'Risk Propension',
                           client_investment_horizon = 'Investment Horizon',
                           client_knowledge_experience = 'Experience',
                           client_potential_index = 'Potential Index',
                           income_high_low = 'Income',
                           age = 'Age',
                           income_need = 'Income Need',
                           long_term_care_need = 'Care Need',
                           protection_need = 'Protection Need',
                           pension_need = 'Pension Need',
                           inheritance_index = 'Inheritance Opt.Need'))
paste(unique(client_personal_gathered$variable), collapse = "', '")
#client_personal_gathered <- client_personal_gathered %>%
#  filter(cluster_personal != 0) %>%
#  mutate(cluster_personal = '4') %>%
#  bind_rows(client_personal_gathered)
c('Risk Propension','Potential Index',  #density
  'Experience', 'Investment Horizon',#violin
  'Income Need', 'age')

rpers_p <- client_personal_gathered %>%
  filter(cluster_personal != 0 & variable == 'Risk Propension') %>%
  rename(`Cluster` = cluster_personal) %>%
  ggplot(aes(x = value, fill = `Cluster`,
             group = `Cluster`)) +
  geom_density(alpha = .5,
               aes(color = `Cluster`), size = .6) +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        #axis.title.x = element_blank(),
        legend.position = 'right',
        axis.ticks.x = element_line(size = 1, colour = 'grey'),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 19)
  ) +
  ylab('Density') +
  xlab('Risk Propension') +
  ggtitle('Profile - Risk Propension') +
  scale_color_manual(values = c('#D35400', '#273746', '#0B5345')) +
  scale_fill_manual(values = c('#F39C12', '#2E86C1', '#17A589')) +
  scale_x_continuous(breaks = seq(0,1,by = .25),
                     labels = seq(0,1,by = .25),
                     limits = c(0,1))
rpers_p


pindex_p <- client_personal_gathered %>%
  filter(cluster_personal != 0 & variable == 'Potential Index') %>%
  rename(`Cluster` = cluster_personal) %>%
  ggplot(aes(x = value, fill = `Cluster`,
             group = `Cluster`)) +
  geom_density(alpha = .5,
               aes(color = `Cluster`), size = .6) +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        #axis.title.x = element_blank(),
        legend.position = 'right',
        axis.ticks.x = element_line(size = 1, colour = 'grey'),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 19)
  ) +
  ylab('Density') +
  xlab('Potential Index') +
  ggtitle('Profile - Potential Index') +
  scale_color_manual(values = c('#D35400', '#273746', '#0B5345')) +
  scale_fill_manual(values = c('#F39C12', '#2E86C1', '#17A589')) +
  coord_cartesian(ylim = c(0,5)) +
  scale_x_continuous(breaks = seq(0,1,by = .25),
                     labels = seq(0,1,by = .25),
                     limits = c(0,1))
pindex_p

in_p <- client_personal_gathered %>%
  filter(cluster_personal != 0 & variable == 'Income Need') %>%
  rename(`Cluster` = cluster_personal) %>%
  ggplot(aes(x = value, fill = `Cluster`,
             group = `Cluster`)) +
  geom_density(alpha = .5,
               aes(color = `Cluster`), size = .6) +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title.y = element_text(colour = 'black', size = 18),
        axis.title.x = element_blank(),
        legend.position = 'none',
        axis.ticks.x = element_line(size = 1, colour = 'grey'),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 19)
  ) +
  ylab('Density') +
  xlab('Income Need') +
  ggtitle('Income Need') +
  scale_color_manual(values = c('#D35400', '#273746', '#0B5345')) +
  scale_fill_manual(values = c('#F39C12', '#2E86C1', '#17A589')) +
  coord_cartesian(ylim = c(0,6), default = T) +
  scale_y_continuous(breaks = 0:6,
                     labels = 0:6) +
  scale_x_continuous(breaks = seq(0,1,by = .25),
                     labels = seq(0,1,by = .25),
                     limits = c(0,1))
in_p



age_p <- client_personal_gathered %>%
  filter(cluster_personal != 0 & variable == 'Age') %>%
  rename(`Cluster` = cluster_personal) %>%
  ggplot(aes(x = value, fill = `Cluster`,
             group = `Cluster`)) +
  geom_density(alpha = .5,
               aes(color = `Cluster`), size = .6) +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title.y = element_text(colour = 'black', size = 18),
        axis.title.x = element_blank(),
        legend.position = 'right',
        axis.ticks.x = element_line(size = 1, colour = 'grey'),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 19)
  ) +
  ylab('Density') +
  xlab('Age') +
  ggtitle('Profile - Age') +
  scale_color_manual(values = c('#D35400', '#273746', '#0B5345')) +
  scale_fill_manual(values = c('#F39C12', '#2E86C1', '#17A589')) +
  coord_cartesian(ylim = c(0,.04), default = T) +
  scale_y_continuous(breaks = seq(0,.4, by = .01),
                     labels = seq(0,.4, by = .01))
age_p


ihor_p <- client_personal_gathered %>%
  filter(cluster_personal != 0 & variable == 'Investment Horizon') %>%
  rename(`Cluster` = cluster_personal) %>%
  ggplot(aes(x = as.ordered(Cluster), y = value,
             fill = Cluster, group = Cluster)) +
  #geom_violin(color = 'black') +
  geom_violin(alpha = .5,
              aes(color = `Cluster`), size = .6) +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        legend.position = 'none',
        axis.ticks.x = element_line(size = 1, colour = 'grey'),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 24)
  ) +
  scale_color_manual(values = c('#D35400', '#273746', '#0B5345')) +
  scale_fill_manual(values = c('#F39C12', '#2E86C1', '#17A589')) +
  ylab('Investment Horizon') +
  xlab('Cluster') +
  ggtitle('Investment Horizon') +
  scale_x_discrete(breaks = 1:3) +
  scale_y_continuous(limits = c(0, 70),
                     labels = seq(0, 70, by = 10),
                     breaks = seq(0, 70, by = 10))
ihor_p


exp_p <- client_personal_gathered %>%
  filter(cluster_personal != 0 & variable == 'Experience') %>%
  rename(`Cluster` = cluster_personal) %>%
  ggplot(aes(x = as.ordered(Cluster), y = value,
             fill = Cluster, group = Cluster)) +
  #geom_violin(color = 'black') +
  geom_violin(alpha = .5,
              aes(color = `Cluster`), size = .6) +
  theme_minimal()  +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 23),
        axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        legend.position = 'right',
        axis.ticks.x = element_line(size = 1, colour = 'grey'),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 24)
  ) +
  scale_color_manual(values = c('#D35400', '#273746', '#0B5345')) +
  scale_fill_manual(values = c('#F39C12', '#2E86C1', '#17A589')) +
  ylab('Experience') +
  xlab('Cluster') +
  ggtitle('Profile - Experience') +
  scale_x_discrete(breaks = 1:3) +
  scale_y_continuous(limits = c(0, 1),
                     labels = seq(0, 1, by = .25),
                     breaks = seq(0, 1, by = .25))
exp_p

client_personal_gathered %>%
  filter(cluster_personal != 0 & variable == 'Experience') %>%
  .$value %>%
  unique()


perscl_p <- ggdraw() +
  draw_plot(rpers_p, x = 0, y = .5, width = .333, height = .5) +
  draw_plot(pindex_p, x = .333, y = .5, width = .333, height = .5) +
  draw_plot(in_p, x = .666, y = .5, width = .333, height = .5) +
  draw_plot(age_p, x = 0, y = 0, width = .333, height = .5) +
  draw_plot(ihor_p, x = .333, y = 0, width = .333, height = .5) +
  draw_plot(exp_p, x = .666, y = 0, width = .333, height = .5)
perscl_p
#kmean 2 che ha silhouette piu bassa ma comunque una buona conn e un buon
#pearsongamma
tiff(filename = str_c(getwd(),'/plot/pers_pindex.tif'),
     width = 1200, height = 800, res = 100)
pindex_p
dev.off()
dev.new()


tiff(filename = str_c(getwd(),'/plot/pers_risk.tif'),
     width = 1200, height = 900, res = 200)
rpers_p
dev.off()
dev.new()


tiff(filename = str_c(getwd(),'/plot/pers_age.tif'),
     width = 1200, height = 900, res = 200)
age_p
dev.off()
dev.new()

tiff(filename = str_c(getwd(),'/plot/inv_ihor.tif'),
     width = 1200, height = 900, res = 200)
hor_p
dev.off()
dev.new()


tiff(filename = str_c(getwd(),'/plot/pers_exp.tif'),
     width = 1200, height = 900, res = 200)
exp_p
dev.off()
dev.new()


tiff(filename = str_c(getwd(),'/plot/inv_risk.tif'),
     width = 1200, height = 800, res = 100)
risk_p
dev.off()
dev.new()

rpers_p
pindex_p
in_p
age_p
inhor_p
exp_p



# CONFRONTO ---------------------------------------------------------------


cross_cor_data_cluster <- expand.grid(unique(client_investment_gathered_norecode$variable),
                                      unique(client_personal_gathered$variable),
                                      c('1', '2', '3'),
                                      c('1', '2', '3')) %>%
  as_tibble() %>%
  rename('Investment Variables' = Var1,
         'Profile Variables' = Var2,
         'Cluster Investment' = Var3,
         'Cluster Profile' = Var4) %>%
  mutate(Correlation = 100) %>%
  filter(`Profile Variables` == 'income_high_low')
unique(client_personal_gathered$variable)

#client_clusters
i <- 1
for (i in 1:nrow(cross_cor_data_cluster)) {
  
  inv_var <- as.character(cross_cor_data_cluster[i,'Investment Variables'][[1]])
  pers_var <- as.character(cross_cor_data_cluster[i,'Profile Variables'][[1]])
  
  cluster_inv <- as.character(cross_cor_data_cluster[i,'Cluster Investment'][[1]])
  cluster_pers <- as.character(cross_cor_data_cluster[i,'Cluster Profile'][[1]])
  
  current_data <- client_clusters %>%
    mutate(income_high_low = ifelse(income_high_low == 'Yes', 1, 0)) %>%
    filter(cluster_investment == cluster_inv & cluster_personal == cluster_pers)
  
  cross_cor_data_cluster[i,'Correlation'] <- cor(current_data[which(colnames(current_data) == inv_var)][[1]],
                                                 current_data[which(colnames(current_data) == pers_var)][[1]])
  
}


cross_corr_p <- cross_cor_data_cluster %>%
  ggplot(aes(`Cluster Investment`,
             `Cluster Profile`,
             fill = Correlation)) + 
  geom_tile(color = '#ECF0F1') +
  facet_grid(`Investment Variables`~`Profile Variables`) +
  geom_text(data = filter(cross_cor_data_cluster, abs(Correlation) >.45),
            aes(`Cluster Investment`,
                `Cluster Profile`,
                label = round(Correlation, 2)),
            color = 'black', size = 3) +
  scale_fill_gradient2(limits = c(-1,1), low = "#E74C3C", high = '#2980B9',
                       mid = 'white') +
  theme_minimal() +
  #scale_x_discrete(labels = str_replace_all(c('Bond\nInvestment', 'Cash Investment',
  #                                            'Equity Investment', 'Money Market Investment',
  #                                            'Other Investment', 'Risk Index',
  #                                            'Implicit Horizon', 'Money Invested'), ' ', '\n')) +
  theme(axis.text = element_text(colour = '#555B5F', size = 14),
        axis.title = element_text(colour = 'black', size = 18),
        legend.key.width = unit(1.2, 'cm'),
        legend.key.height = unit(1.3, 'cm'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))
cross_corr_p

tiff(filename = str_c(getwd(),'/plot/cross_corr_cluster.tif'),
     width = 1000, height = 850, res = 85)
cross_corr_p
dev.off()
dev.new()



#RISK PROPENSION VS CASH

client_clusters %>%
  filter(cluster_investment == '2' & cluster_personal == '2') %>%
  ggplot(aes(x = risk_propension, y = cash, color = bond_investments)) +
  geom_point()

client_clusters %>%
  filter(cluster_investment == '3' & cluster_personal == '1') %>%
  ggplot(aes(x = risk_propension, y = cash, color = bond_investments)) +
  geom_point()

#INVESTMENT ORIZON - PORTFOLIO RISK
#fare matrice di scatterplot
client_clusters %>%
  gather(key = 'variable', value = 'value', -cluster_investment,
         -cluster_personal,-client_id, -client_investment_horizon) %>%
  filter(cluster_investment == '2' & cluster_personal == '1' &
           variable %in% c("bond_investments", "cash", "equity_investments",
                           "money_market_investments")) %>%
  ggplot(aes(x = client_investment_horizon,
             y = value#, color = bond_investments
  )) +
  geom_point() +
  facet_wrap(.~variable)

hist(client_clusters$income_need)

#stacked barchart no cluster con questo filtro
client_clusters %>%
  filter(income_need > .6)


#POTENTIAL INDEX VS AUM
client_clusters %>%
  filter(cluster_investment == '2' & cluster_personal == '2') %>%
  ggplot(aes(x = client_potential_index,
             y = au_m#, color = bond_investments
  )) +
  geom_point()

client_clusters %>%
  filter(cluster_investment == '2' & cluster_personal == '1') %>%
  ggplot(aes(x = client_potential_index,
             y = au_m#, color = bond_investments
  )) +
  geom_point()
