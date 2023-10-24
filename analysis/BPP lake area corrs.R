
#### Doing basic correlations between pigments and lake area
library(ggpubr) ## correlation plots
library(cowplot) ## more plotting

## data
bpcorr <- read.csv(file.choose(), header = TRUE)
bpcorr = bpcorr[-1,]

Pheo_a <-
  ggscatter(bpcorr, x="area", y="pheoa",
          add = "reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson",
          cor.coeff.args=list(label.x=27.2, label.y = 200))+
        xlab(bquote(Lake~surface~area~(km^2)))+ 
        ylab(bquote(Pheophytin~a~(nmol~g^-1~C)))
Pheo_a

Bcar <-
  ggscatter(bpcorr, x="area", y="bcar",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=27.5, label.y =220))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(B-carotene~(nmol~g^-1~C)))
Bcar

Canth <- 
  ggscatter(bpcorr, x="area", y="canth",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=27.2, label.y = 60))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Canthaxanthin~(nmol~g^-1~C)))
Canth

Aphan <-
  ggscatter(bpcorr, x="area", y="aphan",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=27.2, label.y = 6.5))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Aphanizophyll~(nmol~g^-1~C)))
Aphan

Ech <-
  ggscatter(bpcorr, x="area", y="echine",
          add = "reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson",
          cor.coeff.args=list(label.x=27.2, label.y = 40))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Echinenone~(nmol~g^-1~C)))
Ech

LutZ <-
  ggscatter(bpcorr, x="area", y="lutzea",
          add = "reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson",
          cor.coeff.args=list(label.x=27.2, label.y =150))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Lutein-Zeaxanthin~(nmol~g^-1~C)))
LutZ

Pheob <-
  ggscatter(bpcorr, x="area", y="pheob",
          add = "reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson",
          cor.coeff.args=list(label.x=27.2, label.y = 400))+ 
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Pheophytin~b~(nmol~g^-1~C)))
Pheob

Diato <-
  ggscatter(bpcorr, x="area", y="diato",
          add = "reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson",
          cor.coeff.args=list(label.x=27.2, label.y = 50))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Diatoxanthin~(nmol~g^-1~C)))
Diato

Allo <-
  ggscatter(bpcorr, x="area", y="allo",
          add = "reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson",
          cor.coeff.args=list(label.x=27.2, label.y =50))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Alloxanthin~(nmol~g^-1~C)))
Allo

bp_corr <- plot_grid(align='hv', Bcar, Pheo_a, Diato, Allo, Pheob, LutZ, 
                     Ech, Canth, Aphan, nrow=3, ncol=3)
bp_corr
ggsave("BP_area_pig_cor.pdf", bp_corr, dpi=300, scale=1)


############## Same thing with geochem data
d15N <-
  ggscatter(bpcorr, x="area", y="X15n",
          add = "reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson", 
          cor.coeff.args=list(label.x=27.2, label.y = 6.5))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(expression(paste(delta^{15}, "N (\u2030)")))
d15N          

d13C <-
  ggscatter(bpcorr, x="area", y="X13c",
          add = "reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson",
          cor.coeff.args=list(label.x=27.2, label.y =-22.5))+
  xlab(bquote(Lake~surface~area~(km^2)))+
  ylab(expression(paste(delta^{13}, "C (\u2030)")))
d13C

N <-
  ggscatter(bpcorr, x="area", y="N",
          add = "reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson",
          cor.coeff.args=list(label.x=27.2, label.y = 0.55))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab("N%")
N

C <-
  ggscatter(bpcorr, x="area", y="C",
          add = "reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson",
          cor.coeff.args=list(label.x=27.2, label.y = 6.5))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab("C%")
C

CN <-
  ggscatter(bpcorr, x="area", y="C.N",
          add = "reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson",
          cor.coeff.args=list(label.x=27.2, label.y = 11.5))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab("C:N ratio")
CN

## tp separate file due to offset samples and different age-depths
tp <- read.csv(file.choose(), header=TRUE)

TP <- 
  ggscatter(tp, x="area", y="tp",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=27.25, label.y = 1.7))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(TP~(mg~P~g^-1)))
TP

bp_geochem_corr <- plot_grid(align='hv', C, d13C, N, d15N, CN, TP, ncol=2, nrow=3)
bp_geochem_corr

ggsave("BP_area_geochem_cor.pdf", bp_geochem_corr, dpi=300, scale=1)

########################################################### 
# Do the same with Pelican
pelcorr <- read.csv(file.choose(), header = TRUE)

pelcorr = pelcorr[-1,]

Pheo_a <-
  ggscatter(pelcorr, x="area", y="pheoa",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y = 300))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Pheophytin~a~(nmol~g^-1~C)))
Pheo_a

Bcar <-
  ggscatter(pelcorr, x="area", y="bcar",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y =500))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(B-carotene~(nmol~g^-1~C)))
Bcar

Canth <- 
  ggscatter(pelcorr, x="area", y="canth",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y = 80))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Canthaxanthin~(nmol~g^-1~C)))
Canth

Aphan <-
  ggscatter(pelcorr, x="area", y="aphan",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y = 9))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Aphanizophyll~(nmol~g^-1~C)))
Aphan

Ech <-
  ggscatter(pelcorr, x="area", y="echine",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y = 90))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Echinenone~(nmol~g^-1~C)))
Ech

LutZ <-
  ggscatter(pelcorr, x="area", y="lutzea",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y =500))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Lutein-Zeaxanthin~(nmol~g^-1~C)))
LutZ

Pheob <-
  ggscatter(pelcorr, x="area", y="pheob",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y = 400))+ 
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Pheophytin~b~(nmol~g^-1~C)))
Pheob

Diato <-
  ggscatter(pelcorr, x="area", y="diato",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y = 250))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Diatoxanthin~(nmol~g^-1~C)))
Diato

Allo <-
  ggscatter(pelcorr, x="area", y="allo",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y =75))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(Alloxanthin~(nmol~g^-1~C)))
Allo

library(cowplot)
pel_corr <- plot_grid(align='hv', Bcar, Pheo_a, Diato, Allo, Pheob, LutZ, 
                     Ech, Canth, Aphan, nrow=3, ncol=3)
pel_corr
ggsave("pel_area_pig_cor.pdf", pel_corr, dpi=300, scale=1)


################ geochemi
##
d15N <-
  ggscatter(pelcorr, x="area", y="X15n",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson", 
            cor.coeff.args=list(label.x=2, label.y = 9.75))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(expression(paste(delta^{15}, "N (\u2030)")))
d15N          

d13C <-
  ggscatter(pelcorr, x="area", y="X13c",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y =-21.7))+
  xlab(bquote(Lake~surface~area~(km^2)))+
  ylab(expression(paste(delta^{13}, "C (\u2030)")))
d13C

N <-
  ggscatter(pelcorr, x="area", y="N",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y = 0.35))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab("N%")
N
 
C <-
  ggscatter(pelcorr, x="area", y="C",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y = 5))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab("C%")
C

CN <-
  ggscatter(pelcorr, x="area", y="C.N",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y = 13))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab("C:N ratio")
CN

## pelican tp
tp <- read.csv(file.choose(), header=TRUE)

TP <- 
  ggscatter(tp, x="area", y="tp",
            add = "reg.line", conf.int = TRUE,
            cor.coef=TRUE, cor.method = "pearson",
            cor.coeff.args=list(label.x=2, label.y = 1.2))+
  xlab(bquote(Lake~surface~area~(km^2)))+ 
  ylab(bquote(TP~(mg~P~g^-1)))
TP

pel_geochem_corr <- plot_grid(align='hv', C, d13C, N, d15N, CN, TP, ncol=2, nrow=3)
pel_geochem_corr

ggsave("pel_area_geochem_cor.pdf", pel_geochem_corr, dpi=300, scale=1)
