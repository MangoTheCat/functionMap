/*Example7*/

/*Prompts the user for username and password*/

%login;



/*Checks out root folder from ModSpace entry 1022*/

%libname(libname=MyLib7, Identifier=1022, folder=SAS_Basic);



/*Set variable QVAL in MyLib7.suppex to 5*/

data MyLib7.suppex;
  set MyLib7.suppex;
  QVAL = 5;
run;



/*Prints out all variables in MyLib7.suppex to check changes*/

proc print data = MyLib7.suppex;
run;



/*Checks in MyLib7*/

%checkin(libname=MyLib7);



/*Synchronize ModSpace entry 1022*/

%finalize(Identifier = 1022);
