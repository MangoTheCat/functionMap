/*Example3*/


/*Prompts the user for username and password*/

%login;



/*Checks out root folder from ModSpace entry 1022*/

%libname(libname=MyLib3, Identifier=1022, folder=SAS_Basic);



/*Adds dataset iris in MyLib3*/

data MyLib3.iris;
  set sashelp.iris;
run;



/*Checks in MyLib3*/

%checkin(libname=MyLib3, msg="Adds dataset iris");



/*Synchronize ModSpace entry 1022*/

%finalize(Identifier = 1022);
