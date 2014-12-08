/*Example9*/


/*Prompts the user for username and password*/

%login;


/*Checks out Data folder from ModSpace entry 433*/

%libname(libname=MyLib9a, Identifier=433, folder=First Entry/Data);


/*Checks out Outputs subfolder from ModSpace entry 1101*/

%libname(libname=MyLib9b, Identifier=1101, folder=SAS_Structured/Outputs);


/*Copy dataset mh from MyLib9a to MyLib9b*/

data MyLib9b.mh;
  set MyLib9a.mh;
run;


/*Checks in MyLib9b*/

%checkin(libname=MyLib9b, msg="Copy dataset mh from First Entry to SAS_Structured");


/*Synchronize ModSpace entry 433*/

%finalize(Identifier = 433);


/*Synchronize ModSpace entry 1101*/

%finalize(Identifier = 1101);

