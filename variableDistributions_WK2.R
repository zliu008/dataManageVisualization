rm(list=ls(all=TRUE))
caculateDist <- function(data_in, varName) {
    freq <- data.frame(table(data_in));
    colnames(freq)[1] = varName;
    freq$Perc <- freq$Freq/sum(freq$Freq);
    print(sprintf("the distribution of %s:", varName));
    print(freq);
    freq;
}
nesar_pds <- read.csv('nesarc_pds.csv');
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



