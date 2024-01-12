clear 
/*cd "C:\Users\WB200957\OneDrive - WBG\DEC\Github projects\bdipovmap"
import delimited using "data-clean/geodata.csv"
destring rpc, replace ignore("NA")
drop if rpc==. 
destring ntl_all_2015-normal_ntl_mean2020, replace ignore("NA")

save "data-clean/geodata", replace  
*/

cd "D:/Ify/GitProjects/bdipovmap"

use "data.dta", replace 

//gen  lnrpc=log(rpc)
//drop swe_change~5  swe_chang~10 swe_hist_dev swe_sq_his~v
//tab admin1pcod, gen(BD)

lasso linear lnrpc_tot_cons ntl_all_2015-BDI018 [iw=hhweight], select(bic, postsel) cluster(admin2Pcod)
global model_vars = e(allvars_sel)
regress lnrpc_tot_cons $model_vars [iw=hhweight]

