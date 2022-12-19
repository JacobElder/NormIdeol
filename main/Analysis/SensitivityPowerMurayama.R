########

# SENSITIVITY ANALYSIS BASED ON MURAYA ET AL 2022

########

# Study 1

P = 1 # Number of cross-level interactions in model
J = 384 # N
# r = sqrt( (t^2) / (J - p12 - 1 + t^2) ) # Original Murayama Formula for converting MLM t-to-r, assuming as correlation

r_S1 = pwr::pwr.r.test(n=J,sig.level = .05, power=.80)$r  # Minimum r detectable, from GPower
t_S1 = (r_S1*sqrt(J-P-1))/(sqrt(-r_S1+1)*sqrt(r_S1+1))  # Murayama formula flipped to convert r-to-t
paste0("Minimum t = ",t_S1)



# Study 2

P = 1 # Number of cross-level interactions in model
J = 380 # N
# r = sqrt( (t^2) / (J - p12 - 1 + t^2) ) # Original Murayama Formula for converting MLM t-to-r, assuming as correlation

r_S2 = pwr::pwr.r.test(n=J,sig.level = .05, power=.80)$r  # Minimum r detectable, from GPower
t_S2 = (r_S2*sqrt(J-P-1))/(sqrt(-r_S2+1)*sqrt(r_S2+1)) # Murayama formula flipped to convert r-to-t
paste0("Minimum t = ",t_S2)


