library(dplyr)

processRCSFiles <- function () {
  allOutputs <- data.frame(matrix(ncol=13))
  colnames(allOutputs) <- c("age", "sex", "ov16_pos", "mf_prev", "age_pre", "sex_pre", "ov16_pos_pre", "mf_prev_pre", "age_after_year", "sex_after_year", "ov16_pos_after_year", "mf_prev_after_year", "run_num")

  # 36 - unsure, but we get the curve we want, 37 - gamma dist 1, coi.in, 38 - different exposure method, 39 - just coi.in, 40 - -10 to s.a.exp calc, 41 - -10 to s.a.exp calc, gamma dist = 1
  files <- c('ov16_output_any_worm_gamm_2/')
  fileToUse <- paste("data/", files[1], sep="")

  i <- 1
  mda_vals <- c()
  abr_vals <- c()
  mf_prev_vals <- c()
  for (file in list.files(fileToUse)) {
    print(i)
    tmpRDSData <- readRDS(paste(fileToUse, file,sep=""))
    age <- tmpRDSData$ov16_seropositive_matrix[,7]#tmpRDSData$all_infection_burdens[,2]

    age_pre <- tmpRDSData$all_infection_burdens_pre_treatment[,2]

    sex <- ifelse(tmpRDSData$ov16_seropositive_matrix[,8] == 1, "Male", "Female")#ifelse(tmpRDSData$all_infection_burdens[,3]==1, "Male", "Female")

    sex_pre <- ifelse(tmpRDSData$all_infection_burdens_pre_treatment[,3]==1, "Male", "Female")

    mf_prev <- tmpRDSData$mf_indv_prev_matrix[,9]#tmpRDSData$mf_indv_prevalence

    mf_prev_pre <- tmpRDSData$mf_indv_prevalence_pre_treatment

    ov16_seropos <- tmpRDSData$ov16_seropositive_matrix[,9]#tmpRDSData$ov16_seropositive

    ov16_seropos_pre <- tmpRDSData$ov16_seropositive_pre_treatment

    age_after_year <- tmpRDSData$ov16_seropositive_matrix[,16]

    sex_after_year <- ifelse(tmpRDSData$ov16_seropositive_matrix[,17]==1, 'Male', 'Female')

    ov_after_year <- tmpRDSData$ov16_seropositive_matrix[,18]

    mf_prev_after_year <- tmpRDSData$mf_indv_prev_matrix[,18]

    tmpNumRows <- length(age)
    startIndex <- 1+tmpNumRows*(i-1)
    endIndex <- tmpNumRows*i
    allOutputs[startIndex:endIndex,-13] <- list(age, sex, ov16_seropos, mf_prev, age_pre, sex_pre, ov16_seropos_pre, mf_prev_pre, age_after_year, sex_after_year, ov_after_year, mf_prev_after_year)
    allOutputs[startIndex:endIndex,13] <- i

    mda_vals <- c(mda_vals, tmpRDSData$MDA)

    abr_vals <- c(abr_vals, tmpRDSData$ABR)

    mf_prev_vals <- c(mf_prev_vals, tmpRDSData$mf_prev[36600])
    i <- i + 1
  }

  print(paste("Overall Pre Treatment MF Prevalence:", mean(mf_prev_vals)))
  print(paste("Avg MDA Years:", mean(mda_vals)))
  par(mfrow=c(1,2))
  hist(mda_vals)
  print(paste("Avg ABR Value:", mean(abr_vals)))
  hist(abr_vals)
  par(mfrow=c(1,1))

  ggplot() + geom_boxplot(aes(x=factor(abr_vals), y=mf_prev_vals)) +
    scale_y_continuous(breaks=seq(0.6, 0.7, 0.01), limits = c(0.6, 0.7))

  returnVal <- list(allOutputs, mean(mf_prev_vals), mean(abr_vals), mean(mda_vals))
  names(returnVal) <- c("allOutputs", "mf_prev", "abr_vals", "mda_vals")
  return(returnVal)
}

readGabonData <- function() {
  fileName <- "../Ov16 Data/MW_ML_GAB_data.csv"
  data <- read.csv(fileName, check.names = FALSE)
  data <- data[,1:(length(data)-16)]
  cols_to_keep <- c(2, 8, 9, 10, 14, 16, 17, 20, 21, 33, 34)
  data <- data[,cols_to_keep]
  data <- data %>% filter(Country == "Gabon") %>% mutate(
    ov16_seropos = ifelse(get("Ov16 result") == 1, 1, 0),
    mf_pos = ifelse(Oncho_MF_mean_by_snip > 0, 1, 0),
    sex = ifelse(get("Sex (1=M, 2=F)") == 1, 'Male', 'Female'),
    age_groups = case_when(
      Age <= 80 ~ round(Age/10)*10,
      TRUE ~ 80
    )
  ) %>% filter(!is.na(ov16_seropos))

  print(paste("Overall Gabon Treatment MF Prevalence:", mean(data$mf_pos)))

  returnVal <- list(data, mean(data$mf_pos))
  names(returnVal) <- c("data", "mf_prev")
  return(returnVal)
}

ABR ~ 150


