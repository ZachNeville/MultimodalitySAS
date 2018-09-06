/* ========================================== 
NOTE: Do NOT execute this file. It is only meant to be executed by the DIP macro.
This file contains all necessary R code (plus necessary setup in SAS) to perform the Dip Test. 
============================================= */

/* This file is part of the dip and silverman macros released with the paper 
"Macros to Conduct Tests of Multimodality in SAS" by Zachariah Neville and
Naomi Brownstein. For full license and copyright information, please see 
the accompanying COPYING.txt file.

These macros are free software: you can redistribute them and/or modify
them under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

These macros are distributed in the hope that they will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with these macros.  If not, see <http://www.gnu.org/licenses/>. */



proc iml;
call ExportDataSetToR("dipdata2", "dipdata");

/* Setup to prepare the variables for the SUBMIT statement */
dspv = &simulatepvalue;
dreps = &reps;
dsname = "&dipdata";
vname = "&finalvarname";

	submit dspv dreps dsname vname / R;

	if(!is.numeric(as.matrix(dipdata))) {
		cat("ERROR: Your data was input in SAS as numeric, but R did not recognize it as numeric. This may be due to the use of SAS formats, such as dates, times, or custom formats. If you decide that removing a format is appropriate, then documentation with directions is available at the following link:\n http://support.sas.com/documentation/cdl/en/lrdict/64316/HTML/default/viewer.htm#a000178212.htm")
	} else {
		
		# Convert simulatepvalue into a boolean for use in dip.test
		spv <- TRUE
		if(&dspv == 0) { 
		  spv <- FALSE
		}

		# Load necessary packages. Using code from: https://stackoverflow.com/questions/15155814/check-if-r-package-is-installed-then-load-library
		# Modified from the source since we are only installing one package.  
		
		# if package is installed locally, load
		if("diptest" %in% rownames(installed.packages())) {
			do.call("library", list("diptest"))
		} else { # if package is not installed locally, download, then load
			cat("Installing diptest package.\n\n")
			install.packages("diptest")
			do.call("library", list("diptest"))
		}
		
		# Perform dip test
		dipresult <- dip.test(as.matrix(dipdata), spv, &dreps)
		
		# Print results to window
		cat(paste("------------------------\n", "Dip Test of Unimodality\n", "------------------------\n\n", sep = ""))

		cat(paste("Data: ", "&dsname\n", sep = ""))
		cat(paste("Variable: ", "&vname\n\n", sep = ""))
		
		cat("Null Hypothesis: Unimodal\n")
		cat("Alternative Hypothesis: Non-unimodal, i.e. at least bimodal\n\n")
		cat(paste("Dip Statistic: ", prettyNum(dipresult$statistic, digits = 6), "\n", sep = ""))
		cat(paste("p-value: ", prettyNum(dipresult$p.value, digits = 6), "\n", sep = ""))

		if(spv) {
			cat(paste("(simulated p-value based on", &dreps, "replicates)", sep = " "))
		}

		# Store results in R data frame (used to import results from R back into SAS if the out argument was specified)
		dipdf <- data.frame(AlternativeHypothesis = dipresult$alternative, DipStatistic = unname(dipresult$statistic), PValue = dipresult$p.value)
	}
	endsubmit;

	/* If out argument was specified, then import dip results back into SAS */
	if compare(&out1, "NULL") ^= 0 then call ImportDataSetFromR(&out1, "dipdf");
quit;
