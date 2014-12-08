/*Example6*/


/*Prompts the user for username and password*/

%login;


/*Checks out Data subfolder from ModSpace entry 1101*/

%libname(libname=MyLib6a, Identifier=1101, folder=SAS_Structured/Data);


/*Checks out Outputs subfolder from ModSpace entry 1101*/

%libname(libname=MyLib6b, Identifier=1101, folder=SAS_Structured/Outputs);


/*Copy dataset pe from MyLib6a to MyLib6b*/

data MyLib6b.pe;
  set MyLib6a.pe;
run;


/*Checks in MyLib6b*/
/*Synchronize ModSpace entry 1101*/

%complete (libname=MyLib6b, msg="Copy dataset pe from Data to Outputs", Identifier=1101);
