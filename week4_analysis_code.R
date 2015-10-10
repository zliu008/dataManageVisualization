rm(list=ls(all=TRUE))
library(ggplot2)
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
##caculate distribution percentage
nesar_pds$S4AQ1[nesar_pds$S4AQ1 == 9] <-NA;
nesar_pds$S4AQ2[nesar_pds$S4AQ2 == 9] <-NA;
freq_S4AQ1 <- caculateDist(nesar_pds[,'S4AQ1'], 'S4AQ1');
freq_S4AQ2 <- caculateDist(nesar_pds[,'S4AQ2'], 'S4AQ2');
 
##caculate the frequency of age groups of the subjects
age_group <-rep('middle', dim(nesar_pds)[1]);
age_group[nesar_pds[,'AGE'] < 30] <- 'young';
age_group[nesar_pds[,'AGE'] >= 60] <- 'old';
freq_agegroup <- caculateDist(age_group, 'AgeGroup');

##caculate the frequency of the age start to have trouble
nesar_pds$S4AQ6A[nesar_pds$S4AQ6A == 99] <- NA
nesar_pds$S4AQ6A_GRP <-rep('middle', dim(nesar_pds)[1]);
nesar_pds$S4AQ6A_GRP[nesar_pds[,'S4AQ6A'] < 30] <- 'young';
nesar_pds$S4AQ6A_GRP[nesar_pds[,'S4AQ6A'] >= 60] <- 'old';
freq_S4AQ6A <- caculateDist(nesar_pds[,'S4AQ6A_GRP'], 'S4AQ6A_GRP');

#group number of epsodes of major depression
nesar_pds$S4AQ7[nesar_pds$S4AQ7 == 99] <- NA
nesar_pds$S4AQ7_GRP[nesar_pds$S4AQ7 < 5] <- 'Low';
nesar_pds$S4AQ7_GRP[nesar_pds$S4AQ7 >= 5  & nesar_pds$S4AQ7 < 20] <- 'Moderate';
nesar_pds$S4AQ7_GRP[nesar_pds$S4AQ7 >= 20] <- 'High';
freq_S4AQ7 <- caculateDist(nesar_pds[,'S4AQ7_GRP'], 'S4AQ7_GRP');

## social phobia in 12 month
freq_DSOCPDXSNI12 <- caculateDist(nesar_pds[,'DSOCPDXSNI12'], 'DSOCPDXSNI12');

## genetical mom has issue
nesar_pds$S4BQ2[nesar_pds$S4BQ2 == 9] <-NA;
freq_S4BQ2 <- caculateDist(nesar_pds[,'S4BQ2'], 'S4BQ2');
## genmetical father has issue 
nesar_pds$S4BQ2[nesar_pds$S4BQ2 == 9] <-NA;
freq_S4BQ1 <- caculateDist(nesar_pds[,'S4BQ1'], 'S4BQ1');

##sleeping disorder - woke up early than usual
nesar_pds$S4AQ4A6[nesar_pds$S4AQ4A6 == 9] <- NA;
freq_S4AQ4A6 <- caculateDist(nesar_pds[,'S4AQ4A6'], 'S4AQ4A6');

##sleeping disorder - trouble fall in sleep
nesar_pds$S4AQ4A5[nesar_pds$S4AQ4A5 == 9] <- NA;
freq_S4AQ4A5 <- caculateDist(nesar_pds[,'S4AQ4A5'], 'S4AQ4A5');

##to caculate the correlations
cor.test(nesar_pds[,'S4AQ1'], nesar_pds[,'S4AQ2']);
cor.test(nesar_pds$S4AQ6A, nesar_pds$S4AQ7);
#S4CQ5 is the age to have trouble with minor depression
#Here is to test the correlation between major depression and minor depression
nesar_pds$S4CQ5[nesar_pds$S4CQ5 == 99] <- NA
cor.test(nesar_pds$S4AQ6A, nesar_pds$S4CQ5);
major_vs_mino <- data.frame(S4AQ6A = nesar_pds$S4AQ6A, S4CQ5 = nesar_pds$S4CQ5); 
g1<-ggplot(major_vs_mino, aes(x=S4CQ5, y=S4AQ6A))+ geom_point() + labs(title='Age at onset of minor vs major depression')



