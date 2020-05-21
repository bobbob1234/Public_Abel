library(ChannelAttribution)
library(reshape)
library(ggplot2)
library(dplyr)
library(multidplyr)
library(markovchain)
#markov_data <- fread(w_d3)
markov_data <- ALL_FLAGS
### High/Value Customer Flag Setting
markov_data <- markov_data[,c("id","channel","rank","revenue")] 
markov_data <-  markov_data[order(rank),]
rank_freq_table <-table(markov_data$rank) %>% as.data.frame()
frequency_stats <- summary(rank_freq_table$Freq)
q3_number_reduce <- frequency_stats["3rd Qu."] %>% as.numeric()
markov_data2 <- subset(markov_data,markov_data$rank <= q3_number_reduce)
id_rev_agg <- aggregate(markov_data2$revenue,by = list(markov_data2$id),sum)
colnames(id_rev_agg) <- c("id","revenue")
cluster2 <- new_cluster(4)
markov_data2 <- markov_data2 %>% group_by(markov_data2$id)%>% partition(cluster2)
markov_df <- markov_data2 %>% group_by(id) %>% summarise(channel = toString(sort(channel))) %>% collect()
markov_df2 <- dplyr::left_join(markov_df,id_rev_agg,"id")
markov_df2$sale_occured <- ifelse(markov_df2$revenue >0,"Y","N")
markov_df3 <- markov_df2
markov_df3$id <- gsub("-.*","",markov_df3$id)
markov_null <- subset(markov_df3,markov_df3$sale_occured == "N")
markov_null$sale_occured <- 1
markov_conv <- subset(markov_df3,markov_df3$sale_occured == "Y")
markov_conv$sale_occured <- 1
null_paths <- aggregate(markov_null$sale_occured,by = list(markov_null$channel),sum)
colnames(null_paths) <- c("channel","var_null")
fwrite(null_paths,"C:/Users/cebojo01/Desktop/abel/ml_datasets/null_paths.csv")
converted_paths <- aggregate(markov_conv$sale_occured,by = list(markov_conv$channel),sum)
colnames(converted_paths) <- c("channel","var_conv")
fwrite(converted_paths,"C:/Users/cebojo01/Desktop/abel/ml_datasets/conv_paths.csv")
all_paths <- dplyr::full_join(converted_paths,null_paths,by = "channel")
all_paths$var_conv <-ifelse(is.na(all_paths$var_conv),0,all_paths$var_conv) 
all_paths$var_null <-ifelse(is.na(all_paths$var_null),0,all_paths$var_null)
all_paths$obs_size <- all_paths$var_conv + all_paths$var_null
all_paths$conversion_rate <- all_paths$var_conv / all_paths$obs_size
all_paths2 <- subset(all_paths,all_paths$conversion_rate != 1)
conv_stats <- summary(all_paths2$conversion_rate)
q3_conv <- conv_stats["3rd Qu."] %>% as.numeric()
all_paths3 <- subset(all_paths,all_paths$conversion_rate >= q3_conv)
all_paths4 <- subset(all_paths,all_paths3$obs_size >1)
all_paths5 <- all_paths4[,c("channel","var_conv","var_null")]
all_paths5$channel <- gsub(",",">",all_paths5$channel)
markov_chain <- markov_model(all_paths5,var_path = "channel",var_conv = "var_conv",var_null = "var_null",out_more = T)
saveRDS(markov_chain, "./markov_chain.rds")
markov_chain_model <- readRDS("./markov_chain.rds")
markov_chain_model_tm <- markov_chain_model$result


df_plot_trans <- markov_chain_model$transition_matrix
df_plot_trans <- subset(df_plot_trans,df_plot_trans$transition_probability >= 0.45)
cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a",
          "#e29421", "#e29421", "#f05336", "#ce472e")
t <- max(df_plot_trans$transition_probability)

ggplot(df_plot_trans, aes(y = channel_from, x = channel_to, fill = transition_probability)) +
  theme_minimal() +
  geom_tile(colour = "white", width = .9, height = .9) +
  scale_fill_gradientn(colours = cols, limits = c(0, t),
                       breaks = seq(0, t, by = t/4),
                       labels = c("0", round(t/4*1, 2), round(t/4*2, 2), round(t/4*3, 2), round(t/4*4, 2)),
                       guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_text(aes(label = round(transition_probability, 2)), fontface = "bold", size = 4) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
  ggtitle("Transition matrix heatmap")

