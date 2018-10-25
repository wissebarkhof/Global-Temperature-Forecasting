# CO2 Emssion
co2 <- ts(read.csv('data/mole_fraction_of_carbon_dioxide_in_air_input4MIPs_GHGConcentrations_CMIP_UoM-CMIP-1-1-0_gr3-GMNHSH_000001-201412.csv'), start = 0000, frequency = 12)
co2 <- window(co2, start = 1960)

temp <- read.csv('data/temp_emissions_1960_2014.csv')
temp['montly_co2'] <- co2[, 'data_mean_global']

write.csv(temp, 'data/montly_temp_emissions_1960_2014.csv')
