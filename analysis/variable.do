use "D:\Ify\GitProjects\bdipovmap\data.dta", replace 

lasso linear lnrpc_tot_cons ntl_all_2015-BDI018 [iw=hhweight], select(bic, postsel) cluster(admin4Pcod)

qui eststo

estimates store lasso_model

esttab lasso_model using "D:\Ify\GitProjects\bdipovmap\selected_variables.txt", varwidth(20) nomtitles nonumber replace 