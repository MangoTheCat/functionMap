/*Unit test example*/

/*Prompts the user for username and password*/
%macr1;

/*Checks out Data folder from ModSpace entry 433*/
%macr2(libname=MyLib, Identifier=433, folder=First Entry/Data);

/*Copy dataset suppex from sashelp to MyLib*/
data MyLib.suppex;
  set sashelp.suppex;
run;

/*Set variable QVAL in MyLib.suppex to 5*/
data MyLib.suppex;
  set MyLib.suppex;
  QVAL = 5;
run;

/*Prints out all variables in MyLib.suppex to check changes*/
proc print data = MyLib.suppex;
run;

/*Checks in MyLib*/
%macr3(libname=MyLib, msg="Copy dataset suppex from First Entry to SAS_Structured");

/*Synchronize ModSpace entry 433*/
%macr4(Identifier = 433);



