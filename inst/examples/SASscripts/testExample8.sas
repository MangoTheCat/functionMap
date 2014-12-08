/*Example8*/


/*Prompts the user for username and password*/

%login;


/*Checks out from ModSpace entry 433*/

%libname(libname=MyLib8a, Identifier=433, folder=First Entry/Data);


/*Checks out Outputs subfolder from ModSpace entry 433*/

%libname(libname=MyLib8b, Identifier=433, folder=First Entry/Source Data);


/*Copy dataset suppex from MyLib8a to MyLib8b*/

data MyLib8b.suppex;
  set MyLib8a.suppex;
run;


/*Checks in MyLib8b*/

%checkin(libname=MyLib8b, msg="Copy dataset suppex from Data  to Source Data");


/*Synchronize ModSpace entry 433*/

%finalize(Identifier = 433);
