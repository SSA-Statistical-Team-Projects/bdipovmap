cap program drop lassoareawrapper
program define lassoareawrapper
syntax varlist, select(string) output(string) input(string) [force(varlist)]
gettoken depvar candidates : varlist
if "`force'"~="" {
	local candidates : list candidates - force
	local candidates "(`force') `candidates'"
}
lasso linear `depvar' `candidates', select(`select')
local selected=e(allvars_sel)
regress `depvar' `selected'
cap file close R
file open R using "`output'", write replace
file write R "`selected'" _n
file close R
end
