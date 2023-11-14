cap program drop lassowrapper 
program define lassowrapper 
syntax varlist, weights(string) select(string) output(string) input(string) cluster(string) [force(varlist)]
gettoken depvar candidates : varlist 
if "`force'"~="" {
	local candidates : list candidates - force
	local candidates "(`force') `candidates'"
}
lasso linear `depvar' `candidates' [iw=`weights'], select(`select') cluster(`cluster')
local selected=e(allvars_sel)
regress `depvar' `selected' [iw=`weights'], vce(cluster `cluster')
cap file close R 
file open R using "`output'", write replace 
file write R "`selected'" _n
file close R
end 
