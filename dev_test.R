library(seqtools)

## phenotype inputs
phenotype.file <- "C:/Projects/ExomeChip/phenodata/EA_EC2_phen_DBP10.csv"
p <- read.csv(phenotype.file, stringsAsFactors=FALSE)

pheno <- phenotype(p, .formula=DBP10~v1age01+sex+bmi01+pc1, .id="id")
pheno_grpd <- phenotype(p, .formula=DBP10~v1age01+sex+bmi01+pc1, .id="id", .groupBy="sex")

tmp1 <- summary(pheno)
tmp1
write.summary_phenotype(tmp1, file="pheno.txt")
tmp1

tmp2 <- summary(pheno_grpd, max.levels=0L)
tmp2
write.summary_phenotype(tmp2, file="pheno2.txt")
tmp2

# snpinfo stuff
snpinfo.file <- "C:/Projects/ExomeChip/data/SNPInfo_HumanExome-12v1_rev5_analysisCols.RData"
load(snpinfo.file)

mysi <- snpinfo(snpinfo, .snpNames="Name", .chr="Chr", .aggregateBy="SKATgene", .filterBy="sc_nonsynSplice")
mysi

# genotype stuff
genotype.file <- "C:/Projects/ExomeChip/data/EA/RData/EA_ARIC_plus_JHS_v2f_chr13.Rdata"
load(genotype.file)
geno <- genotype(GT)

head(geno)
sGT <- summary(geno)
head(sGT, 10)


foo <- function(x) x[[attr(x, "snpNames")]]

length(get_snps(mysi))
length(get_snps(geno))
length(get_snps(geno, excluded=TRUE))

length(get_subjects(geno))
length(get_subjects(geno, excluded=TRUE))

length(get_subjects(pheno))
length(get_subjects(pheno, excluded=TRUE))

length(get_subjects(pheno_grpd))


cmn <- Intersect(geno=gt, pheno=phenox, si=mysi)
cmn2 <- Intersect(geno=gt, pheno=phenox)
cmn3 <- Intersect(geno=gt, si=mysi)
cmn4 <- Intersect(pheno=phenox, si=mysi)



genes_stats <- mysi %>% group_by(SKATgene) %>% summarise(count = n(),
                         NSNPS = sum(.keep))
