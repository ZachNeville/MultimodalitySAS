/* ========================================== 
This file contains sample calls to the DIP and SILVERMAN macros. 
The data sets are the same as those used in the paper. 
========================================== */

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






/* NOTE: These must be updated in order to run this code on your machine. 
Change these to the full file paths to dip.sas and silverman.sas, respectively. */
filename dipfile "C:\Documents\SAS\dip.sas";
filename silvfile "C:\Documents\SAS\silverman.sas";

/* Normal data for Example 1 */
%let N = 300;
%let N2 = 150;
%let N3 = 100;

data oneNorm (drop = i);
call streaminit(123);
	do i = 1 to &N;
		x = rand("Normal", 0, 1);
		output;
	end;
run;

/* To prevent all the normal data sets coming out too similarly, we will use different but "obvious" seeds each time */
data twoNorms1 (drop = i);
call streaminit(1234);
	do i = 1 to &N2;
		x = rand("Normal", 0, 1);
		output;
		x = rand("Normal", 2, 1);
		output;
	end;
run;

data twoNorms2 (drop = i);
call streaminit(12345);
	do i = 1 to &N2;
		x = rand("Normal", 0, 1);
		output;
		x = rand("Normal", 4, 1);
		output;
	end;
run;

data threeNorms (drop = i);
call streaminit(123456);
	do i = 1 to &N3;
		x = rand("Normal", 0, 1);
		output;
		x = rand("Normal", 3.5, 1);
		output;
		x = rand("Normal", 7, 1);
		output;
	end;
run;

/* Sample macro calls */

/* Note that the 'include' argument can either be the full path to the dip.sas file (as a string) or a fileref:
Ex: %dip(twoNorms1, include = "C:\Documents\SAS\dip.sas");
or
Ex: %dip(twoNorms1, include = dipfile);
*/

/* Normally Distributed Data */
%dip(oneNorm, include = dipfile);
%dip(twoNorms1, include = dipfile);
%dip(twoNorms2, include = dipfile);
%dip(threeNorms, include = dipfile);

%silverman(oneNorm, setSeed = 1234, include = silvfile);
%silverman(twoNorms1, setSeed = 1234, include = silvfile);
%silverman(twoNorms2, setSeed = 1234, include = silvfile);
%silverman(threeNorms, setSeed = 1234, include = silvfile);

%silverman(oneNorm, adjust = 1, setSeed = 1234, include = silvfile);
%silverman(twoNorms1, adjust = 1, setSeed = 1234, include = silvfile);
%silverman(twoNorms2, adjust = 1, setSeed = 1234, include = silvfile);
%silverman(threeNorms, adjust = 1, setSeed = 1234, include = silvfile);

%silverman(twoNorms2, k = 2, setSeed = 1234, include = silvfile);
%silverman(threeNorms, k = 2, setSeed = 1234, include = silvfile);
%silverman(threeNorms, k = 3, setSeed = 1234, include = silvfile);

/* Iris Petal Width data */
%dip(sashelp.iris, PetalWidth, include = dipfile);
%silverman(sashelp.iris, PetalWidth, setSeed = 1234, include = silvfile);
%silverman(sashelp.iris, PetalWidth, adjust = 1, setSeed = 1234, include = silvfile);
%silverman(sashelp.iris, PetalWidth, k = 2, setSeed = 1234, include = silvfile);
