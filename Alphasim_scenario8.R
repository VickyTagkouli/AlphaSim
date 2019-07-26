rm(list = ls())

library(AlphaSimR)
######Scenario 8 for selection of 800 best plants in each cycle with 25 seeds per plant to generate a pop of 40000 for the next cycle#####

#Define parameters################################################################
Founders=1600 ; fabachr=6 ; segSites= fabasegSites= 300
fabaQtl=300 ; minSnpFreq= fabaSnpFreq= 0.1 
a= 5 ; mean=b=0 ; c=0.7 ; d=0.2 ; e=0.15 
f=0.5 #A new variable for the residual error
selection=800 ; selectedSeeds=25 ; fabaSelf=0.25
##################################################################################

#Simulate founder population and add trait
founderPop = runMacs(nInd=Founders,nChr=fabachr,segSites=fabasegSites,
                     inbred=FALSE) 
SP = SimParam$new(founderPop)
SP$restrSegSites(maxQtl = fabaQtl,  overlap = FALSE, minSnpFreq = fabaSnpFreq, force = FALSE)
SP$addTraitAG(nQtlPerChr=a, mean=b, var=c, varEnv=d, varGxE = e, gamma = TRUE, shape = 1.5)

GH1=newPop(founderPop)

GH1 = setPheno(GH1, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage1 = selectOP(GH1,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG1_scenario2= meanG(Cage1)
GV1_scenario2= genicVarG(Cage1)
GV1_scenario2
GG1_scenario2

Cage1 = setPheno(Cage1, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage2 = selectOP(Cage1,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG2_scenario2= meanG(Cage2)
GV2_scenario2= genicVarG(Cage2)
GV2_scenario2
GG2_scenario2

Cage2 = setPheno(Cage2, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage3 = selectOP(Cage2,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG3_scenario2= meanG(Cage3)
GV3_scenario2= genicVarG(Cage3)
GV3_scenario2
GG3_scenario2

Cage3 = setPheno(Cage3, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage4 = selectOP(Cage3,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG4_scenario2= meanG(Cage4)
GV4_scenario2= genicVarG(Cage4)
GV4_scenario2
GG4_scenario2

Cage4 = setPheno(Cage4, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage5 = selectOP(Cage4,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG5_scenario2= meanG(Cage5)
GV5_scenario2= genicVarG(Cage5)
GV5_scenario2
GG5_scenario2

Cage5 = setPheno(Cage5, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage6 = selectOP(Cage5,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG6_scenario2= meanG(Cage6)
GV6_scenario2= genicVarG(Cage6)
GV6_scenario2
GG6_scenario2

Cage6 = setPheno(Cage6, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage7 = selectOP(Cage6,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG7_scenario2= meanG(Cage7)
GV7_scenario2= genicVarG(Cage7)
GV7_scenario2
GG7_scenario2

Cage7 = setPheno(Cage7, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage8 = selectOP(Cage7,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG8_scenario2= meanG(Cage8)
GV8_scenario2= genicVarG(Cage8)
GV8_scenario2
GG8_scenario2

Cage8 = setPheno(Cage8, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage9 = selectOP(Cage8,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG9_scenario2= meanG(Cage9)
GV9_scenario2= genicVarG(Cage9)
GV9_scenario2
GG9_scenario2

Cage9 = setPheno(Cage9, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage10 = selectOP(Cage9,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG10_scenario2= meanG(Cage10)
GV10_scenario2= genicVarG(Cage10)
GV10_scenario2
GG10_scenario2

Cage10 = setPheno(Cage10, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage11 = selectOP(Cage10,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG11_scenario2= meanG(Cage11)
GV11_scenario2= genicVarG(Cage11)
GV11_scenario2
GG11_scenario2

Cage11 = setPheno(Cage11, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage12 = selectOP(Cage11,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG12_scenario2= meanG(Cage12)
GV12_scenario2= genicVarG(Cage12)
GV12_scenario2
GG12_scenario2

Cage12 = setPheno(Cage12, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage13 = selectOP(Cage12,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG13_scenario2= meanG(Cage13)
GV13_scenario2= genicVarG(Cage13)
GV13_scenario2
GG13_scenario2

Cage13 = setPheno(Cage13, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage14 = selectOP(Cage13,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG14_scenario2= meanG(Cage14)
GV14_scenario2= genicVarG(Cage14)
GV14_scenario2
GG14_scenario2

Cage14 = setPheno(Cage14, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage15 = selectOP(Cage14,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG15_scenario2= meanG(Cage15)
GV15_scenario2= genicVarG(Cage15)
GV15_scenario2
GG15_scenario2

Cage15 = setPheno(Cage15, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage16 = selectOP(Cage15,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG16_scenario2= meanG(Cage16)
GV16_scenario2= genicVarG(Cage16)
GV16_scenario2
GG16_scenario2

Cage16 = setPheno(Cage16, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage17 = selectOP(Cage16,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG17_scenario2= meanG(Cage17)
GV17_scenario2= genicVarG(Cage17)
GV17_scenario2
GG17_scenario2

Cage17 = setPheno(Cage17, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage18 = selectOP(Cage17,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG18_scenario2= meanG(Cage18)
GV18_scenario2= genicVarG(Cage18)
GV18_scenario2
GG18_scenario2

Cage18 = setPheno(Cage18, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage19 = selectOP(Cage18,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG19_scenario2= meanG(Cage19)
GV19_scenario2= genicVarG(Cage19)
GV19_scenario2
GG19_scenario2

Cage19 = setPheno(Cage19, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage20 = selectOP(Cage19,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG20_scenario2= meanG(Cage20)
GV20_scenario2= genicVarG(Cage20)
GV20_scenario2
GG20_scenario2

Cage20 = setPheno(Cage20, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage21 = selectOP(Cage20,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG21_scenario2= meanG(Cage21)
GV21_scenario2= genicVarG(Cage21)
GV21_scenario2
GG21_scenario2

Cage21 = setPheno(Cage21, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage22 = selectOP(Cage21,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG22_scenario2= meanG(Cage22)
GV22_scenario2= genicVarG(Cage22)
GV22_scenario2
GG22_scenario2

Cage22 = setPheno(Cage22, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage23 = selectOP(Cage22,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG23_scenario2= meanG(Cage23)
GV23_scenario2= genicVarG(Cage23)
GV23_scenario2
GG23_scenario2

Cage23 = setPheno(Cage23, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage24 = selectOP(Cage23,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG24_scenario2= meanG(Cage24)
GV24_scenario2= genicVarG(Cage24)
GV24_scenario2
GG24_scenario2

Cage24 = setPheno(Cage24, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage25 = selectOP(Cage24,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG25_scenario2= meanG(Cage25)
GV25_scenario2= genicVarG(Cage25)
GV25_scenario2
GG25_scenario2

Cage25 = setPheno(Cage25, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage26 = selectOP(Cage25,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG26_scenario2= meanG(Cage26)
GV26_scenario2= genicVarG(Cage26)
GV26_scenario2
GG26_scenario2

Cage26 = setPheno(Cage26, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage27 = selectOP(Cage26,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG27_scenario2= meanG(Cage27)
GV27_scenario2= genicVarG(Cage27)
GV27_scenario2
GG27_scenario2

Cage27 = setPheno(Cage27, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage28 = selectOP(Cage27, nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG28_scenario2= meanG(Cage28)
GV28_scenario2= genicVarG(Cage28)
GV28_scenario2
GG28_scenario2

Cage28 = setPheno(Cage28, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage29 = selectOP(Cage28,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG29_scenario2= meanG(Cage29)
GV29_scenario2= genicVarG(Cage29)
GV29_scenario2
GG29_scenario2

Cage29 = setPheno(Cage29, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage30 = selectOP(Cage29,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG30_scenario2= meanG(Cage29)
GV30_scenario2= genicVarG(Cage29)
GV30_scenario2
GG30_scenario2


Genetic_gain_scenario2<-c(GG1_scenario2,GG2_scenario2,GG3_scenario2,GG4_scenario2,GG5_scenario2,GG6_scenario2,GG7_scenario2,GG8_scenario2,GG9_scenario2,GG10_scenario2,GG11_scenario2,GG12_scenario2,GG13_scenario2,GG14_scenario2,GG15_scenario2,GG16_scenario2,GG17_scenario2,GG18_scenario2,GG19_scenario2,GG20_scenario2,GG21_scenario2,GG22_scenario2,GG23_scenario2,GG24_scenario2,GG25_scenario2,GG26_scenario2,GG27_scenario2,GG28_scenario2,GG29_scenario2,GG30_scenario2)
Genetic_variation_scenario2<-c(GV1_scenario2,GV2_scenario2,GV3_scenario2,GV4_scenario2,GV5_scenario2,GV6_scenario2,GV7_scenario2,GV8_scenario2,GV9_scenario2,GV10_scenario2,GV11_scenario2,GV12_scenario2,GV13_scenario2,GV14_scenario2,GV15_scenario2,GV16_scenario2,GV17_scenario2,GV18_scenario2,GV19_scenario2,GV20_scenario2,GV21_scenario2,GV22_scenario2,GV23_scenario2,GV24_scenario2,GV25_scenario2,GV26_scenario2,GV27_scenario2,GV28_scenario2,GV29_scenario2,GV30_scenario2)
par(mfrow=c(1,2))
plot(Genetic_gain_scenario2,main="scenario 2",
     xlab="Generations", ylab="Genetic Gain",
     xlim=c(0, 30), ylim=c(0, 8),pch=20)
lines(Genetic_gain_scenario2)
plot(Genetic_variation_scenario2,main="scenario 2",
     xlab="Generations", ylab="Genetic Gain",
     xlim=c(0, 30), ylim=c(0, 1),pch=20)
lines(Genetic_variation_scenario2)


write.table(Genetic_variation_scenario2, "/Users/vickytagkouli/Desktop/AlphaSim_R_scripts/R.Variation_values8.10.txt", sep="\t")
write.table(Genetic_gain_scenario2, "/Users/vickytagkouli/Desktop/AlphaSim_R_scripts/R.Gain_values8.10.txt", sep="\t")




