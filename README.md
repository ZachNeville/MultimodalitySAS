# MultimodalitySAS
********************************************
			Instructions for Use
********************************************

The following is a brief setup guide to use the dip 
and silverman macros. For full details, please see 
the paper "Macros to Conduct Tests of Multimodality 
in SAS" by Zachariah Neville and Naomi Brownstein.


***************
Initial Setup
***************

1. Ensure that the R software is installed on the machine.

2. Open the SASV9.cfg file and ensure that the RLANG option has been inserted.

3. If using the dip macro or the adjusted option of the silverman macro, verify
that the machine can install new R packages from the Internet, or that the appropriate
R packages are installed on the machine (diptest package for the dip macro, splines
package for the adjusted option of the silverman macro).

4. Store silverman.sas and dip.sas in a safe location for future use.

5. Execute the macroDefinitions.sas file to add the macros to the operating environment.

From here, the macros can be used as described in the paper.

samples.sas contains sample code to replicate results from the paper.

NOTE: Before executing samples.sas, the dipfile and silvfile filerefs need to be changed to 
reflect the locations of the dip.sas and silverman.sas files on your machine.


*************************************************
Using Macros in the Future (After Initial Setup)
*************************************************

Steps 1-4 only need to be completed once. In all future SAS sessions, only Step 5 must be 
completed in order to use the macros.