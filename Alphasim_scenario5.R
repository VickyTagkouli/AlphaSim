rm(list = ls())

library(AlphaSimR)
######Scenario 5 for selection of 400 best plants in each cycle with 25 seeds per plant to generate a pop of 10000 for the next cycle#####

#Define parameters################################################################
Founders=1600 ; fabachr=6 ; segSites= fabasegSites= 300
fabaQtl=300 ; minSnpFreq= fabaSnpFreq= 0.1 
a= 5 ; mean=b=0 ; c=0.7 ; d=0.2 ; e=0.15 
f=0.5 #A new variable for the residual error
selection=400 ; selectedSeeds=25 ; fabaSelf=0.25
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
GG1= meanG(Cage1)
GV1= genicVarG(Cage1)
GV1
GG1

Cage1 = setPheno(Cage1, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage2 = selectOP(Cage1,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG2= meanG(Cage2)
GV2= genicVarG(Cage2)
GV2
GG2

Cage2 = setPheno(Cage2, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage3 = selectOP(Cage2,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG3= meanG(Cage3)
GV3= genicVarG(Cage3)
GV3
GG3

Cage3 = setPheno(Cage3, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage4 = selectOP(Cage3,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG4= meanG(Cage4)
GV4= genicVarG(Cage4)
GV4
GG4

Cage4 = setPheno(Cage4, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage5 = selectOP(Cage4,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG5= meanG(Cage5)
GV5= genicVarG(Cage5)
GV5
GG5

Cage5 = setPheno(Cage5, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage6 = selectOP(Cage5,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG6= meanG(Cage6)
GV6= genicVarG(Cage6)
GV6
GG6

Cage6 = setPheno(Cage6, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage7 = selectOP(Cage6,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG7= meanG(Cage7)
GV7= genicVarG(Cage7)
GV7
GG7

Cage7 = setPheno(Cage7, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage8 = selectOP(Cage7,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG8= meanG(Cage8)
GV8= genicVarG(Cage8)
GV8
GG8

Cage8 = setPheno(Cage8, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage9 = selectOP(Cage8,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG9= meanG(Cage9)
GV9= genicVarG(Cage9)
GV9
GG9

Cage9 = setPheno(Cage9, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage10 = selectOP(Cage9,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG10= meanG(Cage10)
GV10= genicVarG(Cage10)
GV10
GG10

Cage10 = setPheno(Cage10, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage11 = selectOP(Cage10,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG11= meanG(Cage11)
GV11= genicVarG(Cage11)
GV11
GG11

Cage11 = setPheno(Cage11, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage12 = selectOP(Cage11,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG12= meanG(Cage12)
GV12= genicVarG(Cage12)
GV12
GG12

Cage12 = setPheno(Cage12, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage13 = selectOP(Cage12,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG13= meanG(Cage13)
GV13= genicVarG(Cage13)
GV13
GG13

Cage13 = setPheno(Cage13, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage14 = selectOP(Cage13,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG14= meanG(Cage14)
GV14= genicVarG(Cage14)
GV14
GG14

Cage14 = setPheno(Cage14, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage15 = selectOP(Cage14,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG15= meanG(Cage15)
GV15= genicVarG(Cage15)
GV15
GG15

Cage15 = setPheno(Cage15, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage16 = selectOP(Cage15,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG16= meanG(Cage16)
GV16= genicVarG(Cage16)
GV16
GG16

Cage16 = setPheno(Cage16, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage17 = selectOP(Cage16,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG17= meanG(Cage17)
GV17= genicVarG(Cage17)
GV17
GG17

Cage17 = setPheno(Cage17, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage18 = selectOP(Cage17,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG18= meanG(Cage18)
GV18= genicVarG(Cage18)
GV18
GG18

Cage18 = setPheno(Cage18, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage19 = selectOP(Cage18,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG19= meanG(Cage19)
GV19= genicVarG(Cage19)
GV19
GG19

Cage19 = setPheno(Cage19, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage20 = selectOP(Cage19,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG20= meanG(Cage20)
GV20= genicVarG(Cage20)
GV20
GG20

Cage20 = setPheno(Cage20, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage21 = selectOP(Cage20,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG21= meanG(Cage21)
GV21= genicVarG(Cage21)
GV21
GG21

Cage21 = setPheno(Cage21, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage22 = selectOP(Cage21,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG22= meanG(Cage22)
GV22= genicVarG(Cage22)
GV22
GG22

Cage22 = setPheno(Cage22, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage23 = selectOP(Cage22,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG23= meanG(Cage23)
GV23= genicVarG(Cage23)
GV23
GG23

Cage23 = setPheno(Cage23, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage24 = selectOP(Cage23,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG24= meanG(Cage24)
GV24= genicVarG(Cage24)
GV24
GG24

Cage24 = setPheno(Cage24, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage25 = selectOP(Cage24,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG25= meanG(Cage25)
GV25= genicVarG(Cage25)
GV25
GG25

Cage25 = setPheno(Cage25, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage26 = selectOP(Cage25,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG26= meanG(Cage26)
GV26= genicVarG(Cage26)
GV26
GG26

Cage26 = setPheno(Cage26, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage27 = selectOP(Cage26,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG27= meanG(Cage27)
GV27= genicVarG(Cage27)
GV27
GG27

Cage27 = setPheno(Cage27, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage28 = selectOP(Cage27, nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG28= meanG(Cage28)
GV28= genicVarG(Cage28)
GV28
GG28

Cage28 = setPheno(Cage28, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage29 = selectOP(Cage28,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG29= meanG(Cage29)
GV29= genicVarG(Cage29)
GV29
GG29

Cage29 = setPheno(Cage29, varE=f, p=runif(1)) #Sets a phenotype using the GxE model
Cage30 = selectOP(Cage29,nInd=selection,nSeeds=selectedSeeds,probSelf=fabaSelf,pollenControl = FALSE, trait = 1, use ="pheno" ,selectTop = TRUE, simParam = SP )
GG30= meanG(Cage29)
GV30= genicVarG(Cage29)
GV30
GG30


Genetic_gain<-c(GG1,GG2,GG3,GG4,GG5,GG6,GG7,GG8,GG9,GG10,GG11,GG12,GG13,GG14,GG15,GG16,GG17,GG18,GG19,GG20,GG21,GG22,GG23,GG24,GG25,GG26,GG27,GG28,GG29,GG30)
Genetic_variation<-c(GV1,GV2,GV3,GV4,GV5,GV6,GV7,GV8,GV9,GV10,GV11,GV12,GV13,GV14,GV15,GV16,GV17,GV18,GV19,GV20,GV21,GV22,GV23,GV24,GV25,GV26,GV27,GV28,GV29,GV30)
par(mfrow=c(1,2))
plot(Genetic_gain,main="scenario 1",
     xlab="Generations", ylab="Genetic Gain",
     xlim=c(0, 30), ylim=c(0, 6),pch=20)
lines(Genetic_gain)
plot(Genetic_variation,main="scenario 1",
     xlab="Generations", ylab="Genetic Gain",
     xlim=c(0, 30), ylim=c(0, 1),pch=20)
lines(Genetic_variation)

library(lattice)

xyplot(Genetic_gain+Genetic_gain_scenario2+Genetic_gain_scenario3~1:30,auto.key=T, type='l',scales=list(x=list(at=1:30)))
xyplot(Genetic_variation+Genetic_variation_scenario2+Genetic_variation_scenario3~1:30,auto.key=T, type='l',scales=list(x=list(at=1:30)))


write.table(Genetic_variation, "/Users/vickytagkouli/Desktop/AlphaSim_R_scripts/R.Variation_values5.10.txt", sep="\t")
write.table(Genetic_gain, "/Users/vickytagkouli/Desktop/AlphaSim_R_scripts/R.Gain_values5.10.txt", sep="\t")


