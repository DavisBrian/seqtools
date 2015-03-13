library(seqtools)

## phenotype inputs
phenotype.file <- "C:/Projects/ExomeChip/phenodata/EA_EC2_phen_DBP10.csv"
p <- read.csv(phenotype.file, stringsAsFactors=FALSE)
phenox <- phenotype(p, formula=DBP10~v1age01+sex+bmi01+pc1, id="id", gender="sex")
summary(phenox)


# snpinfo stuff
snpinfo.file <- "C:/Projects/ExomeChip/data/SNPInfo_HumanExome-12v1_rev5_analysisCols.RData"
load(snpinfo.file)

mysi <- snpinfo(snpinfo, .snpNames="Name", .chr="Chr", .aggregateBy="SKATgene", .filterBy="sc_nonsynSplice")


# genotype stuff
genotype.file <- "C:/Projects/ExomeChip/data/EA/RData/EA_ARIC_plus_JHS_v2f_chr13.Rdata"
load(genotype.file)
gt <- genotype(GT)
head(gt)
sGT <- summary(gt)
head(sGT)

cmn <- Intersect(geno=gt, pheno=phenox, si=mysi)
cmn2 <- Intersect(geno=gt, pheno=phenox)
cmn3 <- Intersect(geno=gt, si=mysi)
cmn4 <- Intersect(pheno=phenox, si=mysi)



genes_stats <- mysi %>% group_by(SKATgene) %>% summarise(count = n(),
                         NSNPS = sum(.keep))
