/*Example5*/


/*Prompts the user for username and password*/

%login;


/*Checks out Data subfolder from ModSpace entry 1101*/

%libname(libname=MyLib5a, Identifier=1101, folder=SAS_Structured/Data/Source_Data);


/*Checks out Outputs subfolder from ModSpace entry 1101*/

%libname(libname=MyLib5b, Identifier=1101, folder=SAS_Structured/Data);


/*Copy dataset tv from MyLib5b to MyLib5a*/

data MyLib5a.tv;
  set MyLib5b.tv;
run;


/*Checks in MyLib5a*/

%checkin(libname=MyLib5a, msg="Copy dataset tv from Data to Source_Data");


/*Synchronize ModSpace entry 1101*/

%finalize(Identifier = 1101);
