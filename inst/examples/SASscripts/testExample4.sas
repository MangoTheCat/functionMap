/*Example4*/


/*Prompts the user for username and password*/

%login;


/*Checks out Data subfolder from ModSpace entry 1101*/

%libname(libname=MyLib4a, Identifier=1101, folder=SAS_Structured/Data);


/*Checks out Outputs subfolder from ModSpace entry 1101*/

%libname(libname=MyLib4b, Identifier=1101, folder=SAS_Structured/Outputs);


/*Copy dataset dm from MyLib4a to MyLib4b*/

data MyLib4b.dm;
  set MyLib4a.dm;
run;


/*Checks in MyLib4b*/

%checkin(libname=MyLib4b, msg="Copy dataset dm from Data to Outputs");


/*Synchronize ModSpace entry 1101*/

%finalize(Identifier = 1101);
