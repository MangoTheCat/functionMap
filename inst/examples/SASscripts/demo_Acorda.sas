%libname(libname=MyLib,
        url=http://modspace-win/svn/modspace/trunk/6/ExampleFiles/Data, 
        user=&user., pass=&pass.);

* SAS CODE HERE;
data MyLib.DA_back;
    set MyLib.DA;
RUN;

data MyLib.DA;
    set MyLib.DA;
    DAORRES = 0;
RUN;

* END OF ANALYSIS;
%checkIn(libname=MyLib, msg="Back up DA and set DAORRES to 0");

