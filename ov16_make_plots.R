# start with Gabon - fit the data
# no treatment in Gabon

library(dplyr)
library(ggplot2)

allOutputs <- data.frame(matrix(ncol=9))
colnames(allOutputs) <- c("age", "sex", "ov16_pos","mf_prev", "age_pre", "sex_pre", "ov16_pos_pre", "mf_prev_pre", "run_num")

files <- c('ov16_output_1/', 'ov16_output_l3/', 'ov16_output_l3_2/', 'ov16_output_l3_3/', 'ov16_output_mating/')
fileToUse <- paste("data/", files[5], sep="")

i <- 1
for (file in list.files(fileToUse)) {
  print(i)
  tmpRDSData <- readRDS(paste(fileToUse, file,sep=""))
  age <- tmpRDSData$all_infection_burdens[,2]
  age_pre <- tmpRDSData$all_infection_burdens_pre_treatment[,2]

  sex <- ifelse(tmpRDSData$all_infection_burdens[,3]==1, "Male", "Female")

  sex_pre <- ifelse(tmpRDSData$all_infection_burdens_pre_treatment[,3]==1, "Male", "Female")

  mf_prev <- tmpRDSData$mf_indv_prevalence

  mf_prev_pre <- tmpRDSData$mf_indv_prevalence_pre_treatment

  ov16_seropos <- tmpRDSData$ov16_seropositive

  ov16_seropos_pre <- tmpRDSData$ov16_seropositive_pre_treatment

  #ov16_seropos_l3_estab <- tmpRDSData$ov16_seropositive_l3_estab

  #ov16_seropos_l3_estab_pre <- tmpRDSData$ov16_seropositive_l3_estab_pre_treatment

  #ov16_juvy <- tmpRDSData$ov16_seropositive_juvy_adult

  #ov16_juvy_pre <- tmpRDSData$ov16_seropositive_juvy_adult_pre_treatment


  tmpNumRows <- length(age)
  startIndex <- 1+tmpNumRows*(i-1)
  endIndex <- tmpNumRows*i
  allOutputs[startIndex:endIndex,-9] <- list(age, sex, ov16_seropos, mf_prev, age_pre, sex_pre, ov16_seropos_pre, mf_prev_pre)
  allOutputs[startIndex:endIndex,9] <- i
  i <- i + 1
}


# viz

tmpAllOutputs <- allOutputs

allOutputs <- tmpAllOutputs %>% mutate(age_groups = case_when(
                                        age < 20 ~ round(age),
                                        age < 25 ~ 25,
                                        age < 30 ~ 30,
                                        age < 35 ~ 35,
                                        age < 40 ~ 40,
                                        age < 50 ~ 50,
                                        age < 60 ~ 60,
                                        age < 70 ~ 70,
                                        TRUE ~ 80
                                      ),
                                      age_groups_pre = case_when(
                                        age_pre < 20 ~ round(age_pre),
                                        age_pre < 25 ~ 25,
                                        age_pre < 30 ~ 30,
                                        age_pre < 35 ~ 35,
                                        age_pre < 40 ~ 40,
                                        age_pre < 50 ~ 50,
                                        age_pre < 60 ~ 60,
                                        age_pre < 70 ~ 70,
                                        TRUE ~ 80
                                      ))

tmpDf <- allOutputs %>% dplyr::group_by(age_groups, sex) %>% dplyr::summarise(ov16_prev=mean(ov16_pos), mf_prev=mean(mf_prev)) %>% as.data.frame() #%>% tidyr::pivot_longer(c(ov16_prev, mf_prev), names_to="treatment", values_to="ov16_prev") %>% as.data.frame()
tmpDf[(dim(tmpDf)[1]+1),] <- list(0, 'Female', 0, 0)
tmpDf[(dim(tmpDf)[1]+1),] <- list(0, 'Male', 0, 0)


tmpDf2 <- allOutputs %>% dplyr::group_by(age_groups_pre, sex_pre) %>% dplyr::summarise(ov16_prev_pre=mean(ov16_pos_pre), mf_prev_pre=mean(mf_prev_pre)) %>% as.data.frame() #%>% tidyr::pivot_longer(c(mf_prev, mf_prev_pre), names_to="treatment", values_to="mf_prev") %>% as.data.frame()
tmpDf2[(dim(tmpDf2)[1]+1),] <- list(0, 'Female', 0, 0)
tmpDf2[(dim(tmpDf2)[1]+1),] <- list(0, 'Male', 0, 0)

ov16_graph <- ggplot() +
  geom_line(aes(x=age_groups_pre, y=ov16_prev_pre*100, color="Pre Treatment", linetype=sex_pre), linewidth=1.2, data=tmpDf2) +
  geom_line(aes(x=age_groups, y=ov16_prev*100, color='Post Treatment', linetype=sex), linewidth=1.2, data=tmpDf) +
  xlab("Age") +
  ylab("OV16 Seroprevalence (%)") +
  ggtitle("Ov16 Seroprevalence w/ Mating Worm Pair") +
  scale_x_continuous(breaks=c(seq(0,20,5), seq(30, 80, 10))) +
  scale_y_continuous(breaks=seq(0,101,20), limits=c(0, 100)) +
  scale_linetype_manual(values=c("dashed", "dotted")) +
  theme(
    axis.text = element_text(size=15),
    axis.title= element_text(size=15)
  ) +
  scale_color_manual(values=c("red", "black"))

ov16_graph



ggsave("ov16_mating.png", ov16_graph, width=7000, height = 4000, units="px", dpi=600)


mf_prev_graph <- ggplot()  +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line( linewidth=.1, color=rgb(0, 0, 0, 20, maxColorValue=255)),
    panel.grid.major.y = element_line( linewidth=.1, color=rgb(0, 0, 0, 20, maxColorValue=255)),
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text = element_text(size=15),
    axis.title= element_text(size=15)
  ) +
  geom_line(aes(x=age_groups_pre, y=mf_prev_pre*100, color='Pre Treatment', linetype=sex_pre), linewidth=1.2, data=tmpDf2) +
  geom_line(aes(x=age_groups, y=mf_prev*100, color='Post Treatment', linetype=sex), linewidth=1.2, data=tmpDf) +
  xlab("Age") +
  ylab("mf prevalence (%)") +
  scale_x_continuous(breaks=seq(0,80,10)) +
  scale_y_continuous(breaks=seq(0,101,20), limits=c(0, 100)) +
  scale_linetype_manual(values=c("dashed", "dotted")) +
  scale_color_manual(values=c("red", "black"))
mf_prev_graph

mf_nested <- ggplot() +
  geom_line(aes(x=age_groups, y=mf_prev*100, color='Post Treatment', linetype=sex), linewidth=1.2, data=tmpDf) +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks=seq(0,80,10)) +
  scale_y_continuous(breaks=seq(0, 15, 5), limits=c(0, 15)) +
  scale_linetype_manual(values=c("dashed", "dotted")) +
  scale_color_manual(values=c("red", "black")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line( linewidth=.1, color=rgb(0, 0, 0, 20, maxColorValue=255)),
    panel.grid.major.y = element_line( linewidth=.1, color=rgb(0, 0, 0, 20, maxColorValue=255)),
    panel.background = element_rect(fill = 'white', colour = 'black'),
    legend.position='none',
    axis.text = element_text(size=7)
  )

mf_prev_graph <- mf_prev_graph +
  ggtitle("MF Prevalence vs Age") +
  geom_rect(aes(xmin=14, xmax=70, ymin=15, ymax=50), alpha=0.3, color="black") +
  annotation_custom(ggplotGrob(mf_nested), xmin = 10, xmax = 70, ymin = 10, ymax = 50)

mf_prev_graph


ggsave("mf_prev_graph.png", mf_prev_graph, width=3500, height = 2000, units="px", dpi=600)


grid_graph <- grid.arrange(mf_prev_graph, ov16_graph, ncol=2)
grid_graph

ggsave("both_graphs.png", grid_graph, width=7000, height = 2000, units="px", dpi=600)
