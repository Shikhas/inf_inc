############################################################################################################################
# scatter plot and correlation matrix
import::here(grwth_county, .from="data_preparation.R")
pairs(~income + sex_ratio + pct_empd + pct_bpl + pct_wrk_class + pct_dep_class, data=grwth_county)
pairs(~income + pct_occ01 + pct_occ02 + pct_occ03 + pct_occ04, data=grwth_county)
pairs(~income + pct_cow01 + pct_cow02 + pct_cow03 + pct_cow04, data=grwth_county)
pairs(~income + pct_lessthan_highgrad + pct_highschool_grad + pct_bachdeg_or_highr, data=grwth_county)
pairs(~income + tax_in + tax_sr, data=grwth_county)
