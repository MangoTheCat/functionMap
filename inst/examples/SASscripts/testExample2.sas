/*Example2*/

/*Prompts the user for username and password*/

%login;



/*Checks out root folder from ModSpace entry 1022*/

%libname(libname=MyLib2, Identifier=1022, folder=SAS_Basic);



/*Set variable QVAL in mylib1.suppex to 0*/

data MyLib2.suppex;
  set MyLib2.suppex;
  QVAL = 0;
run;



/*Prints out all variables in mylib1.suppex to check changes*/

proc print data = MyLib2.suppex;
run;



/*Checks in mylib1*/

%checkin(libname=MyLib2);



/*Synchronize ModSpace entry 1022*/

%finalize(Identifier = 1022);
