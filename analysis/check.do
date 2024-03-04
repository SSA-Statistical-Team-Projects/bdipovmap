use "D:\Ify\GitProjects\bdipovmap\data.dta", replace

cap program drop lassowrapper
program define lassowrapper
    syntax varlist, weights(string) select(string) output(string) input(string) cluster(string) [force(varlist)]

    // Parse input variables
    gettoken depvar candidates : varlist

    // Remove force variables from candidates if specified
    if "`force'" != "" {
        local candidates : list candidates - force
        local candidates "(`force') `candidates'"
    }

    // Run Lasso regression
    lasso linear `depvar' `candidates' [iw=`weights'], select(`select') cluster(`cluster')

    // Extract selected variables
    local selected e(allvars_sel)

    // Run linear regression with selected variables
    regress `depvar' `selected' [iw=`weights'], vce(cluster `cluster')

    // Open output file
    capture noisily file open R using "`output'", write replace

    // Write selected variables to file
    noisily file write R "`selected'" _n

    // Close output file
    noisily file close R
end
