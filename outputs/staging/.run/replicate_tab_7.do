version 17
clear all
set more off, permanently
local root "C:/WZB Dropbox/Macartan Humphreys/5_github/replicate_everything/rep-10.1257-aer.91.5.1369"
cd "`root'"
global REPLICATE_STATA_RESULT "C:/WZB Dropbox/Macartan Humphreys/5_github/replicate_everything/rep-10.1257-aer.91.5.1369/artifacts/staging"
cap mkdir "C:/WZB Dropbox/Macartan Humphreys/5_github/replicate_everything/rep-10.1257-aer.91.5.1369/artifacts/staging"
do "C:/WZB Dropbox/Macartan Humphreys/5_github/replicate_everything/rep-10.1257-aer.91.5.1369/code/tab_7.do"
