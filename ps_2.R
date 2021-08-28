################################################################################
#Problem 4
################################################################################


#Reading in data
output_dir <- "/Volumes/GoogleDrive/My Drive/classes_spring_2021/Quant/problem_sets/ps_2/output"
econ_data <- read.dta("/Volumes/GoogleDrive/My Drive/classes_spring_2021/Quant/problem_sets/ps_2/data/ps2_CountryEconGrowth.dta")

growth_school_plot <- ggplot(econ_data, aes(x=yearsschool, 
                                            y=growth
                                            )
                             ) + geom_point(aes(color=europe))
growth_assasin_plot <- ggplot(econ_data, aes(y=growth,
                                              x=assasinations
                                              )
                               ) + geom_point(aes(color=europe))


print(growth_school_plot)
print(growth_assasin_plot)

#Regressions
growth_school_lm <-lm(growth ~ yearsschool, data=econ_data))
growth_school_lm_output = glance(growth_school_lm)
write.csv(growth_school_lm_output,file.path(output_dir,"growth_school_lm_output.csv"))


#Adding assasinations in
growth_school_assasin_lm <- lm(growth ~ yearsschool + assasinations,
                                data=econ_data)
summary(growth_school_assasin_lm)
write.csv(tidy(growth_school_assasin_lm), file.path(output_dir,"growth_school_assasin_lm_output.csv"))

################################################################################
#Problem 5
################################################################################
data_dir <- "/Volumes/GoogleDrive/My Drive/classes_spring_2021/Quant/problem_sets/ps_2/data"

ed_data <- read.dta(file.path(data_dir,"ps2data_TNStar.dta"))

ed_lm <- lm(mathscore ~ freelunch, data=ed_data)
summary(ed_lm)

ed_multi_lm <- lm(mathscore ~ freelunch + innercity, data=ed_data)
summary(ed_multi_lm)

#outputting files
write.csv(tidy(ed_lm), file.path(output_dir,"math_lunch_lm_output.csv"))
write.csv(tidy(ed_multi_lm), file.path(output_dir,"math_lunch_city_output.csv"))



