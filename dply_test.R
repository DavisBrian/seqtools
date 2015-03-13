df <- data.frame(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)
df
dim(df)
nrow(df)
nrow(distinct(df))

distinct(df, x)
distinct(df, y)

# note snp 5 is in 2 genes
si <- data.frame(
  CHROM = c(rep(1, 12L), rep(2, 8L), rep(3, 5L)),
  SNP = c(paste0("SNP_", 1:5), paste0("SNP_", 5:24)),
  GENE = c(rep("GENE1", 5), rep("GENE2", 7), rep("GENE3", 4), rep("GENE4", 4), rep("GENE5", 5)),
  stringsAsFactors=FALSE
  )

si

gt <- matrix(sample(c(0,1,2), 77, rep=TRUE), nrow=7)
rownames(gt) <- paste0("SUBJECT", 1:nrow(gt))
colnames(gt) <- sample(unique(si$SNP), ncol(gt))

idx <- sample(1:(nrow(gt)*ncol(gt)), 4)
gt[idx] <- NA

gt.df <- data.frame(gt, stringsAsFactors=FALSE)
gt.df$ID <- rownames(gt)

library(tidyr)

# convert from genotype matrix to long format
GT <- gt.df %>% gather(SNP, GT, -ID) %>% tbl_df

GT_grp_ID<- group_by(GT, ID)
GT_grp_SNP <- group_by(GT, SNP)

# subject level stats
rowSums(gt, na.rm=TRUE)

subject_QC <- summarise(GT_grp_ID, 
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

filter(subject_QC, total >= 10)


# site level stats
colSums(gt)

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


filter(snp_QC, AAC > 3)



# gene level stuff
gt_x_si <- GT %>%left_join(si, by="SNP") %>% group_by(GENE)

gene_stats <- summarise(gt_x_si, 
                        count = n(),
                        AAC = sum(GT),
                        c2 = length(GT))

