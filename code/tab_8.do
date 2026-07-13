* Table 1 - Colonial Origins (Stata)
* Study repo: rep-10.1257-aer.91.5.1369
*
* This file is the RUNNER. Shiny merges it with code/maketable8.do for a
* single copy-paste script. To run manually from the study repo root:
*   do "code/tab_1.do"
*
* ============================================================
* SETUP â€” paths (edit maindir if you run from elsewhere)
* ============================================================
version 17
set more off, permanently

local root "`c(pwd)'"
local root : subinstr local root "\" "/", all
global maindir "`root'"
if "${REPLICATE_STATA_RESULT}" != "" {
    global result "${REPLICATE_STATA_RESULT}"
}
else {
    global result "${maindir}/outputs/staging"
}
cap mkdir "${maindir}/outputs"
cap mkdir "${result}"
global datadir "${maindir}/data"

capture log close _all
local oldpwd "`c(pwd)'"
cd "${result}"
log using "tab_8_stata", replace text

* Analysis is in code/maketable8.do (shown in full in the Code tab).
do "${maindir}/code/maketable8.do"

capture log close
cd "`oldpwd'"
