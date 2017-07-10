* Pull out wealth index for interpolation exercise

global pathin2013 "C:\Users\tessam\Documents\Zambia\0_rawdata_notfordistribution\ZM_2013-14_DHS_05252017_933_89151"
global pathout "C:\Users\tessam\Documents\Zambia"


use $pathin2013\zmhr61dt\ZMHR61FL.DTA, clear

g hhwealth = hv271/100000
sum hhwealth, d

* Keep relevant variables
keep hhid hv000 hv001 hv002 hv021 hv024 hv271 hhwealth

clonevar dhsclust = hv001

* Save subset
save "$pathout\hh_wealth_subset.dta", replace
export delimited "$pathout\hh_wealth_subset_txt.txt", replace

* Import the exported shapefile data
import delimited "$pathout\ZMB_DHS2013_14_latlon.txt", clear 

* Merge in DHS household information and export results
merge 1:m dhsclust using "$pathout\hh_wealth_subset.dta", gen(hh_wealth_merge)
export delimited "$pathout\hh_wealth_subset_geo.txt", replace

