snpinfo.file <- "C:/Dropbox/seqtools/data/snpinfo/snpinfo_ChargeSFreeze3Freeze4_ESP_RS_ERF_Broad_Analytic_04112014.RData"
genotype.file <- "C:/Dropbox/seqtools/data/genotype/AA/AA_ARIC_ExFrz41_all_10.RData"
phenotype.file <- "C:/Dropbox/seqtools/data/phenotype/AA_lnfibr_ExFrz41_phen.csv"

load(snpinfo.file)
load(genotype.file)


################
# test seqtools
################
library(seqtools)

gt <- genotype(GT[, 1:7000])
sGT.time <- system.time(sGT <- summary(gt))
sGT.time
print(object.size(gt), units="Mb")
#rm(gt)
#gc()



################
# test dplyr
################
gt.df <- data.frame(ID=rownames(GT), GT[ , 1:7000], stringsAsFactors=FALSE, check.names=FALSE)
gt.df$ID <- rownames(gt.df)
print(object.size(gt.df), units="Mb")

library(tidyr)


# convert from genotype matrix to long format
dGT <- gt.df%>% gather(SNP, GT, -ID) %>% tbl_df

GT_grp_ID<- group_by(dGT, ID)
GT_grp_SNP <- group_by(dGT, SNP)

dGT.time <- system.time({
# subject level stats
subject_QC <- summarise(GT_grp_ID, 
                        count = n(),
                        N = sum(!is.na(GT)),
                        RR = sum(GT == 0, na.rm=TRUE),
                        RA = sum(GT == 1, na.rm=TRUE),
                        AA = sum(GT == 2, na.rm=TRUE),
                        nMiss = sum(is.na(GT)),
                        MRate = nMiss/count
)
# site level stats
snp_QC <- summarise(GT_grp_SNP,
                    count = n(),
                    N = sum(!is.na(GT)),
                    RR = sum(GT == 0, na.rm=TRUE),
                    RA = sum(GT == 1, na.rm=TRUE),
                    AA = sum(GT == 2, na.rm=TRUE),
                    nMiss = sum(is.na(GT)),
                    MRate = nMiss/count,
                    AAC = sum(GT, na.rm=TRUE),
                    AAF = mean(GT, na.rm=TRUE)/2.0
)

})