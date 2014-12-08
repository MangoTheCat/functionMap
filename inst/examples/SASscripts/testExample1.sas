/*Example1*/

/*Prompts the user for username and password*/

%login;



/*Checks out root folder from ModSpace entry 1022*/

%libname(libname=MyLib1, Identifier=1022, folder=SAS_Basic);



/*Prints out all variables in mylib1.suppex*/

proc print data = MyLib1.suppex;
run;



/*Synchronize ModSpace entry 1022*/

%finalize(Identifier = 1022);
