rm(list=ls(all=TRUE))
#function to calculate distribution
caculateDist <- function(data_in, varName) {
    freq <- data.frame(table((data_in), exclude = NULL));
    colnames(freq)[1] = varName;
    freq$Perc <- freq$Freq/sum(freq$Freq);
    print(sprintf("the distribution of %s:", varName));
    print(freq);
    freq;
}

nesar_pds <- read.csv('nesarc_pds.csv');

 
age_group <-rep('middle', dim(nesar_pds)[1]);
age_group[nesar_pds[,'AGE'] < 30] <- 'young';
age_group[nesar_pds[,'AGE'] >= 60] <- 'old';
freq_agegroup <- caculateDist(age_group, 'AgeGroup');

#recode NA, this NA is not a missing data
nesar_pds$S4AQ55[is.na(nesar_pds$S4AQ55)] <- 0; 
#recode number 9, it is unknow, which is missing data
nesar_pds$S4AQ55[nesar_pds$S4AQ55 == 9] <- NA;

#recode NA, this NA is not a missing data
nesar_pds$S4AQ56[is.na(nesar_pds$S4AQ56)] <- 0; 
#recode number 9, it is unknow, which is missing data
nesar_pds$S4AQ56[nesar_pds$S4AQ56 == 9] <- NA;

#recode NA, this NA is not a missing data
nesar_pds$S4AQ57[is.na(nesar_pds$S4AQ57)] <- 0; 
#recode number 9, it is unknow, which is missing data
nesar_pds$S4AQ57[nesar_pds$S4AQ57 == 9] <- NA;

# NA is missing data
#recode number 9, it is unknow, which is missing data
nesar_pds$S4AQ4A8[nesar_pds$S4AQ4A8 == 9] <- NA;

# NA is missing data
#recode number 9, it is unknow, which is missing data
nesar_pds$S4AQ4A6[nesar_pds$S4AQ4A6 == 9] <- NA;

# NA is missing data
#recode number 9, it is unknow, which is missing data
nesar_pds$S4AQ4A5[nesar_pds$S4AQ4A5 == 9] <- NA;

# NA is missing data
#recode number 9, it is unknow, which is missing data
nesar_pds$S4AQ4A9[nesar_pds$S4AQ4A9 == 9] <- NA;

# NA is missing data
#recode number 9, it is unknow, which is missing data
nesar_pds$S4AQ4A14[nesar_pds$S4AQ4A14 == 9] <- NA;

# NA is missing data
#recode number 9, it is unknow, which is missing data
nesar_pds$S4AQ4A15[nesar_pds$S4AQ4A15 == 9] <- NA;

#recode NA, this NA is not a missing data
nesar_pds$S4AQ51[is.na(nesar_pds$S4AQ51)] <- 0; 
#recode number 9, it is unknow, which is missing data
nesar_pds$S4AQ51[nesar_pds$S4AQ51 == 9] <- NA;

#recode number 9, it is unknow, which is missing data
nesar_pds$S4BQ1[nesar_pds$S4BQ1 == 9] <- NA;

#recode number 9, it is unknow, which is missing data
nesar_pds$S4BQ2[nesar_pds$S4BQ2 == 9] <- NA;

#NA is missing data, NA in S4AQ6A meaning no major depression
#recode number 9, it is unknow, which is missing data
nesar_pds$S4AQ6A[nesar_pds$S4AQ6A == 99] <- NA;

#NA is missing data, NA in S4AQ7 meaning no major depression
#recode number 9, it is unknow, which is missing data
nesar_pds$S4AQ7[nesar_pds$S4AQ7 == 99] <- NA;

##caculate distribution percentage
freq_S4AQ1 <- caculateDist(nesar_pds[,'S4AQ1'], 'S4AQ1');
freq_S4AQ2 <- caculateDist(nesar_pds[,'S4AQ2'], 'S4AQ2');
freq_S4AQ56 <- caculateDist(nesar_pds[,'S4AQ56'], 'S4AQ56');
freq_S4AQ57 <- caculateDist(nesar_pds[,'S4AQ57'], 'S4AQ57');
freq_S4AQ55 <- caculateDist(nesar_pds[,'S4AQ55'], 'S4AQ55');
freq_S4AQ54 <- caculateDist(nesar_pds[,'S4AQ54'], 'S4AQ54');
freq_S4AQ4A8 <- caculateDist(nesar_pds[,'S4AQ4A8'], 'S4AQ4A8');
freq_S4AQ4A6 <- caculateDist(nesar_pds[,'S4AQ4A6'], 'S4AQ4A6');
freq_S4AQ4A5 <- caculateDist(nesar_pds[,'S4AQ4A5'], 'S4AQ4A5');
freq_S4AQ4A9 <- caculateDist(nesar_pds[,'S4AQ4A9'], 'S4AQ4A9');
freq_S4AQ4A14 <- caculateDist(nesar_pds[,'S4AQ4A14'], 'S4AQ4A14');
freq_S4AQ4A15 <- caculateDist(nesar_pds[,'S4AQ4A15'], 'S4AQ4A15');
freq_S4AQ51 <- caculateDist(nesar_pds[,'S4AQ51'], 'S4AQ51');
freq_S4BQ1 <- caculateDist(nesar_pds[,'S4BQ1'], 'S4BQ1');
freq_S4BQ2 <- caculateDist(nesar_pds[,'S4BQ2'], 'S4BQ2');

nesar_pds$S4AQ6A_GRP <-rep('middle', dim(nesar_pds)[1]);
nesar_pds$S4AQ6A_GRP[nesar_pds[,'S4AQ6A'] < 30] <- 'young';
nesar_pds$S4AQ6A_GRP[nesar_pds[,'S4AQ6A'] >= 60] <- 'old';
freq_S4AQ6A <- caculateDist(nesar_pds[,'S4AQ6A_GRP'], 'S4AQ6A_GRP');

#group number of epsode
nesar_pds$S4AQ7_GRP[nesar_pds$S4AQ7 < 5] <- 'Low';
nesar_pds$S4AQ7_GRP[nesar_pds$S4AQ7 >= 5  & nesar_pds$S4AQ7 < 20] <- 'Moderate';
nesar_pds$S4AQ7_GRP[nesar_pds$S4AQ7 >= 20] <- 'High';
freq_S4AQ7 <- caculateDist(nesar_pds[,'S4AQ7_GRP'], 'S4AQ7_GRP');


