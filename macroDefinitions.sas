/* ==========================================  
NOTE: This macro definition file must be executed in order to use the macros in the current SAS session. 
==========================================  */

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




%macro dip(dipdata, varname, simulatepvalue = 0, reps = 2000, out = , completecase = 0, include = );
/* dipdata: a SAS data set to use in the analysis.
   varname: the name of a column (variable) in dipdata to use in the Dip Test. If left blank, then the first column of dipdata will be used.
   simulatepvalue: whether or not to simulate pvalues by Monte Carlo simulation. 0 for FALSE, 1 for TRUE.
   reps: an integer specifying the number of replicates used in the Monte Carlo test.
   out: the name of a SAS data set to store the result from Dip test
   completecase: whether to use complete case analysis. 0 for FALSE, 1 for TRUE.
   include: complete file path to dip.sas as a string or a fileref (ex: "C:\Documents\SAS\dip.sas")
*/

/* Warning messages for the Results Window and for the Log must be formatted separately because we are using colors
in the Log */
option minoperator; /* Necessary to use the IN operator within a macro */
option formchar = "|----|+|---+=|-/\<>*"; /* SAS-recommended. Ensures portability to other systems */
option noquotelenmax; /* Prevent SAS from putting warning in log when multiple warnings from the macro occur */
%let warnmsg_l =; /* Initialize all warning-related macro variables */
%let warnmsg_w =;
%let warncount = 0; 

title Dip Test: &&dipdata;

/* Verify file path is valid and was specified. http://support.sas.com/resources/papers/proceedings09/022-2009.pdf  */
%if %sysevalf(%superq(include)=,boolean) %then %do;
	%let errmsg = ERROR: File path for include parameter was not specified. Please provide a complete file path to the dip.sas file in quotes. For example, include = %bquote("C:\Documents\SAS\dip.sas");
	%goto exit;
%end;
%if not %sysfunc(fileexist(&include)) %then %do;
	%if not %sysfunc(fexist(&include)) %then %do;
		%let errmsg = ERROR: File path for include parameter is not valid. Please use the full file path in quotes and include the extension %bquote(".sas"). For example, include = %bquote("C:\Documents\SAS\dip.sas");
		%goto exit;
	%end;
%end;


/* Verify data set exists */
%if not %sysfunc(exist(&dipdata)) %then %do;
	%let errmsg = ERROR: Data set &dipdata does not exist. Make sure the data set name is spelled correctly.;
	%goto exit;
%end;

/* Check for zero-observation dataset or other issues with the data set. */
%let dsid = %sysfunc(open(&dipdata, i));
%let obs1 = %sysfunc(fetchobs(&dsid, 1));

%if &obs1 = -1 %then %do;
	%let errmsg = ERROR: Data set &dipdata has 0 observations.;
	%goto exit;
%end;

%if &obs1 > 0 %then %do;
	%let errmsg = %sysfunc(sysmsg());
	%goto exit;
%end;


/* If varname was not provided, then just default to use the first column */
%if %sysevalf(%superq(varname)=,boolean) %then %do;
	%let varposition = 1;

	/* Warn them if varname was not provided AND the data is multidimensional */
	%let numvars = %sysfunc(attrn(&dsid, nvars));
	%if &numvars > 1 %then %do;
		%let errmsg = ERROR: The data set &dipdata has multiple variables and the varname argument was not specified. Please specify a variable to use in the analysis using the varname argument.;
		%goto exit;
	%end;
%end;
%else %do;
	/* If varname was provided then check to see if it exists */
	%let varposition = %sysfunc(varnum(&dsid, &varname));
	%if &varposition = 0 %then %do;
		/* If varname does not exist, then warn the user */
		%let errmsg = ERROR: The variable &varname was not found in the data set &dipdata.. Please check the spelling and do not use quotation marks.;
		%goto exit;
	%end;
%end;



/* Extract the varposition'th column from &dipdata and store it in a temporary data set */
%let finalvarname = %sysfunc(varname(&dsid, &varposition));

data dipdata_1d;
	set &dipdata;
	keep &finalvarname;
run;

/* Check for non-numeric data */
%let dsid2 = %sysfunc(open(dipdata_1d, i));

%if %sysfunc(vartype(&dsid2, 1)) = C %then %do;
	%let errmsg = ERROR: The variable &finalvarname in &dipdata is non-numeric. These methods were not designed for non-numeric data.;
	%goto exit;
%end;

/* Checking for missing data. */
proc iml;
	use dipdata_1d;
	read all into x;
	close dipdata_1d;
	nmissing = countmiss(x);
	if nmissing > 0 then call symputx("nmiss", 1);
	else call symputx("nmiss", 0);
quit;

%if %sysevalf(&nmiss) ~= 0 %then %do;
	%if &completecase ~= 1 %then %do; /* If completecase was not set and missing data was found, then display error */
		%let errmsg = ERROR: The variable &finalvarname on the data set &dipdata has missing values and the completecase parameter was not set. To perform the Dip Test with complete case analysis, set completecase = 1.;
		%goto exit;
	%end;
	%else %do; /* If completecase = 1 was set and missing data found, then display note */
		%let warncount = %eval(&warncount + 1);
		%let warnmsg_l = &warnmsg_l NOTE: A complete case analysis was used when performing the Dip Test.%str(;);
		%let warnmsg_w = &warnmsg_w NOTE: A complete case analysis was used when performing the Dip Test.%str(;);
	%end;
%end;

/* Remove missing observations. Placement of DATA step here is necessary to ensure dipdata2 is defined when we
call ExportDataSetToR in dip.sas. If no missing observations, then data set will be unchanged. */
data dipdata2;
	set dipdata_1d;
	if nmiss(of _NUMERIC_)=0;
run;

/* Ensure valid value for simulatepvalue */
%if not(&simulatepvalue in (0 1)) %then %do;
	%let simulatepvalue = 0;
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l WARNING- NOTE: simulatepvalue must be set to 0 or 1. Continuing using default value of 0.%str(;);
	%let warnmsg_w = &warnmsg_w NOTE: simulatepvalue must be set to 0 or 1. Continuing using default value of 0.%str(;);
%end;


/* Ensure valid value for reps */
%if %sysfunc(notdigit(&reps)) ^= 0 %then %do;
	%let reps = 2000;
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l WARNING- NOTE: reps must be a positive integer. Continuing using default value of 2000.%str(;);
	%let warnmsg_w = &warnmsg_w NOTE: reps must be a positive integer. Continuing using default value of 2000.%str(;);
%end;
%else %if not(%sysfunc(mod(&reps, 1)) = 0 and &reps > 0) %then %do;
	%let reps = 2000;
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l WARNING- NOTE: reps must be a positive integer. Continuing using default value of 2000.%str(;);
	%let warnmsg_w = &warnmsg_w NOTE: reps must be a positive integer. Continuing using default value of 2000.%str(;);
%end;

/* Setup for out data set */
%if not %sysevalf(%superq(out)=,boolean) %then %do;
	%let out1 = %str("&&out");
%end;
%else %do;
	%let out1 = %str("NULL");
%end;

/* Print warning messages. In most case, there will be either warnings or errors, but not both. */
%if &warncount > 0 %then %do;
	title Dip Test: Warnings;
	data _NULL_;
	/* Print to log */
		do i = 1 to &warncount;
			word2 = scan("&warnmsg_l.", i, ";");
			put word2;
			put;
		end;
	/* Print to window */
	file print;
	array x[50] $100;
		do i = 1 to &warncount;
			word2 = scan("&warnmsg_w.", i, ";");


			k = 1;
			do j = 1 by 1 while(scan(word2, k, " ") ^= "");
				x[j] = scan(word2, j, " ");
				k = k + 1;
			end;

			put x{*};

			do m = 1 to 50;
				x[m] = "";
			end;
		end;
	run;
%end;

/* Titles make output more readable */
title Dip Test: &&dipdata;

/* Run the R code */
%include &include;


%exit: /* Exit macro */
%if %symexist(dsid) %then %do; /* If a data set was opened, close it. */
	%if &dsid > 0 %then %do; 
		%let rc = %sysfunc(close(&dsid));
	%end;
%end;

%if %symexist(dsid2) %then %do; 
	%if &dsid2 > 0 %then %do; 
		%let rc = %sysfunc(close(&dsid2));
	%end;
%end;

/* Print error message if applicable */
%if %symexist(errmsg) %then %do;
	title Dip Test: ERROR;
	%put &errmsg;
	data _NULL_;
		file print;
			array x[50] $100;
			k = 1;
			do j = 1 by 1 while(scan("&errmsg", k, " ") ^= "");
				x[j] = scan("&errmsg", j, " ");
				k = k + 1;
			end;

			put x{*};

			do m = 1 to 50;
				x[m] = "";
			end;
	run;
%end;

title; /* Reset title */
%mend dip;



%macro silverman(silvdata, varname, k = 1, m = 999, adjust = 0, digits = 6, setseed = , showseed = 0, outseed = , out = , completecase = 0, include = );
/* silvdata: a SAS data set to use in the analysis.
   varname: the name of a column (variable) in silvdata to use in the Silverman Test. If left blank, then the first column of silvdata will be used.
   k: number of modes to be tested. H0: number of modes <= k.
   m: number of bootstrap replications.
   adjust: boolean (0 for FALSE, 1 for TRUE) to activate the adjusting of the p-value (valid if k = 1) (see Hall and York)
   digits: number of digits of the p-value. Only applicable when k = 1 and adjust = 1.
   setseed: a seed to be passed as an argument to set.seed().
   showseed: whether or not to return the RNG seed. 0 for FALSE, 1 for TRUE.
   outseed: a SAS data set where the current seed (obtained from .Random.Seed in R) will be stored
   out: a SAS data set where output from the Silverman test will be stored. 
   completecase: whether to use complete case analysis. 0 for FALSE, 1 for TRUE.
   include: complete file path to silverman.sas as a string or a fileref (ex: "C:\Documents\SAS\silverman.sas")
*/
option minoperator; /* Necessary to use the IN operator in a macro. */
option FORMCHAR="|----|+|---+=|-/\<>*"; /* SAS-recommended. Ensures portability to other systems */
option noquotelenmax; /* Prevent SAS from putting warning in log when multiple warnings from the macro occur */
%let warnmsg_l =; /* Initialize all warning macro variables */
%let warnmsg_w =;
%let warncount = 0; 


/* Set the title for readability. */
title Silverman Test: &&silvdata;

/* Verify file path is valid and was specified. http://support.sas.com/resources/papers/proceedings09/022-2009.pdf */
%if %sysevalf(%superq(include)=,boolean) %then %do;
	%let errmsg = ERROR: File path for include parameter was not specified. Please provide a complete file path to the silverman.sas file in quotes. For example, include = %bquote("C:\Documents\SAS\silverman.sas");
	%goto exit;
%end;
%if not %sysfunc(fileexist(&include)) %then %do;
	%if not %sysfunc(fexist(&include)) %then %do;
		%let errmsg = ERROR: File path for include parameter is not valid. Please use the full file path in quotes and include the extension %bquote(".sas"). For example, include = %bquote("C:\Documents\SAS\silverman.sas");
		%goto exit;
	%end;
%end;

/* Verify data set exists */
%if not %sysfunc(exist(&silvdata)) %then %do;
	%let errmsg = ERROR: Data set &silvdata does not exist. Make sure the data set name is spelled correctly.;
	%goto exit;
%end;

/* Check for zero-observation dataset or other data set problems. */
%let dsid = %sysfunc(open(&silvdata, i));
%let obs1 = %sysfunc(fetchobs(&dsid, 1));

%if &obs1 = -1 %then %do;
	%let errmsg = ERROR: Data set &silvdata has 0 observations.;
	%goto exit;
%end;

%if &obs1 > 0 %then %do;
	%let errmsg = %sysfunc(sysmsg());
	%goto exit;
%end;


/* If varname was not provided, then put error only if it's multivariate data*/
%if %sysevalf(%superq(varname)=,boolean) %then %do;
	%let varposition = 1;

	%let numvars = %sysfunc(attrn(&dsid, nvars));

	%if &numvars > 1 %then %do;
		%let errmsg = ERROR: The data set &silvdata has multiple variables and the varname argument was not specified. Please specify a variable to use in the analysis using the varname argument.;
		%goto exit;	
	%end;
%end;
%else %do;
	/* If varname was provided then check to see if it exists */
	%let varposition = %sysfunc(varnum(&dsid, &varname));
	%if &varposition = 0 %then %do;
		/* If varname does not exist, then warn the user and use the first column */
		%let errmsg = ERROR: The variable &varname was not found in the data set &silvdata.. Please check the spelling and do not use quotation marks.;
		%goto exit;
	%end;
%end;

/* Extract the varposition'th column from &silvdata and store it in a temporary data set */
%let finalvarname = %sysfunc(varname(&dsid, &varposition));

data silvdata_1d;
	set &silvdata;
	keep &finalvarname;
run;

/* Check for non-numeric data */
%let dsid2 = %sysfunc(open(silvdata_1d, i));

%if %sysfunc(vartype(&dsid2, 1)) = C %then %do;
	%let errmsg = ERROR: The variable &finalvarname in &silvdata is non-numeric. These methods were not designed for non-numeric data.;
	%goto exit;
%end;


/* Checking for missing data. */
proc iml;
	use silvdata_1d;
	read all into x;
	close silvdata_1d;
	nmissing = countmiss(x);
	if nmissing > 0 then call symputx("nmiss", 1);
	else call symputx("nmiss", 0);
quit;

%if %sysevalf(&nmiss) ~= 0 %then %do;
	%if &completecase ~= 1 %then %do; /* If completecase was not set and missing data was found, then display error */
		%let errmsg = ERROR: The variable &finalvarname in the data set &silvdata has missing values and the completecase parameter was not set. To perform the Silverman Test with complete case analysis, set completecase = 1.;
		%goto exit;
	%end;
	%else %do; /* If completecase = 1 was set and missing data found, display note */
		%let warncount = %eval(&warncount + 1);
		%let warnmsg_l = &warnmsg_l NOTE: A complete case analysis was used when performing the Silverman Test.%str(;);
		%let warnmsg_w = &warnmsg_w NOTE: A complete case analysis was used when performing the Silverman Test.%str(;);
	%end;
%end;

/* Remove missing observations. Placement of DATA step here is necessary to ensure silvData2 is defined when we
call ExportDataSetToR in silverman.sas. If no missing observations, then data set will be unchanged. */
data silvdata2;
	set silvdata_1d;
	if nmiss(of _NUMERIC_)=0;
run;

/* Check for valid values of k. IF structure needs to be like this because SAS does not use short circuit evaluation 
and we will get an unhandled SAS error if a non-numeric is passed to mod() */
%if %sysfunc(notdigit(&k)) ^= 0 %then %do;
	%let k = 1;
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l WARNING- NOTE: k must be a positive integer. Continuing using default value of 1.%str(;);
	%let warnmsg_w = &warnmsg_w NOTE: k must be a positive integer. Continuing using default value of 1.%str(;);
%end;
%else %if not(%sysfunc(mod(&k, 1)) = 0 and &k > 0) %then %do;
	%let k = 1;
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l WARNING- NOTE: k must be a positive integer. Continuing using default value of 1.%str(;);
	%let warnmsg_w = &warnmsg_w NOTE: k must be a positive integer. Continuing using default value of 1.%str(;);
%end;

/* Ensure valid value for m */
%if %sysfunc(notdigit(&m)) ^= 0 %then %do;
	%let m = 999;
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l WARNING- NOTE: m must be a positive integer. Continuing using default value of 999.%str(;);
	%let warnmsg_w = &warnmsg_w NOTE: m must be a positive integer. Continuing using default value of 999.%str(;);
%end;
%else %if not(%sysfunc(mod(&m, 1)) = 0 and &M > 0) %then %do;
	%let m = 999;
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l WARNING- NOTE: m must be a positive integer. Continuing using default value of 999.%str(;);
	%let warnmsg_w = &warnmsg_w NOTE: m must be a positive integer. Continuing using default value of 999.%str(;);
%end;

/* Ensure valid value for adjust */
%if not(&adjust in (0 1)) %then %do;
	%let adjust = 0;
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l WARNING- NOTE: adjust must be set to 0 or 1. Continuing using default value of 0.%str(;);
	%let warnmsg_w = &warnmsg_w NOTE: adjust must be set to 0 or 1. Continuing using default value of 0.%str(;);
%end;

/* Ensure valid value for digits */
%if %sysfunc(notdigit(&digits)) ^= 0 %then %do;
	%let digits = 6;
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l WARNING- NOTE: digits must be positive and numeric. Continuing using default value of 6.%str(;);
	%let warnmsg_w = &warnmsg_w NOTE: digits must be positive and numeric. Continuing using default value of 6.%str(;);
%end;

/* Ensure valid value for showSeed */
%if not(&showseed in (0 1)) %then %do;
	%let showseed = 0;
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l WARNING- NOTE: showseed must be set to 0 or 1. Continuing using default value of 0.%str(;);
	%let warnmsg_w = &warnmsg_w NOTE: showseed must be set to 0 or 1. Continuing using default value of 0.%str(;);
%end;

/* Check for valid values for setseed */
%if %sysevalf(%superq(setseed)=,boolean) %then %do;
	%let setseed = %str("NULL");
%end;
%else %if (%sysfunc(notdigit(&setseed)) ^= 0) and (%sysfunc(compare(&setseed, "NULL")) ^= 0) %then %do;
	%let setseed = %str("NULL");
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l WARNING- NOTE: setseed must be an integer. The seed will not be set.%str(;);
	%let warnmsg_w = &warnmsg_w NOTE: setseed must be an integer. The seed will not be set.%str(;);
%end;

/* Print warning if k = 1 and unadjusted test used */
%if &adjust = 0 and &k = 1 %then %do;
	%let warncount = %eval(&warncount + 1);
	%let warnmsg_l = &warnmsg_l WARNING: When k = 1, the adjusted test by Hall and York (2001) has better asymptotic accuracy than the unadjusted test. Your implementation is unadjusted. To use the adjusted test, change the parameter to adjust = 1.%str(;);
	%let warnmsg_w = &warnmsg_w WARNING: When k = 1, the adjusted test by Hall and York (2001) has better asymptotic accuracy than the unadjusted test. Your implementation is unadjusted. To use the adjusted test, change the parameter to adjust = 1.%str(;);
%end;

/* Setup for outseed */
%if not %sysevalf(%superq(outseed)=,boolean) %then %do;
	%let outSeed1 = %str("&&outseed");
%end;
%else %do;
	%let outSeed1 = %str("NULL");
%end;	

/* Setup for out data set */
%if not %sysevalf(%superq(out)=,boolean) %then %do;
	%let out1 = %str("&&out");
%end;
%else %do;
	%let out1 = %str("NULL");
%end;

/* Print warning messages. In most cases, there will be either warnings or errors, but not both. */
%if &warncount > 0 %then %do;
	title Silverman Test: Warnings;
	data _NULL_;
	/* Print to log */
		do i = 1 to &warncount;
			word2 = scan("&warnmsg_l.", i, ";");
			put word2;
			put;
		end;
	/* Print to window */
	file print;
	array x[50] $100;
		do i = 1 to &warncount;
			word2 = scan("&warnmsg_w.", i, ";");

			k = 1;
			do j = 1 by 1 while(scan(word2, k, " ") ^= "");
				x[j] = scan(word2, j, " ");
				k = k + 1;
			end;

			put x{*};

			do m = 1 to 50;
				x[m] = "";
			end;
		end;
	run;
%end;

/* Titles make output more readable */
title Silverman Test: &&silvdata;

/* Execute the setup and R code */
%include &include;


%exit: /* Exit macro */
%if %symexist(dsid) %then %do; /* If a data set was opened, close it. */
	%if &dsid > 0 %then %do; 
		%let rc = %sysfunc(close(&dsid));
	%end;
%end;

%if %symexist(dsid2) %then %do; 
	%if &dsid2 > 0 %then %do; 
		%let rc = %sysfunc(close(&dsid2));
	%end;
%end;

/* Print error message if applicable */
%if %symexist(errmsg) %then %do;
	title Silverman Test: ERROR;
	%put &errmsg;
	data _NULL_;
		file print;
			array x[50] $100;
			k = 1;
			do j = 1 by 1 while(scan("&errmsg", k, " ") ^= "");
				x[j] = scan("&errmsg", j, " ");
				k = k + 1;
			end;

			put x{*};

			do m = 1 to 50;
				x[m] = "";
			end;
	run;
%end;

title; /* Reset title */
%mend;
