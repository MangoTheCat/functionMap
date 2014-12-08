/*****************************************************************************|
| SAS AND MODSPACE INTERACTION (Acorda)                                       |
|                                                                             |
| @author: consultants@mango-solutions.com                                    |
|                                                                             |
| @license: TODO: Licence                                                     |
|                                                                             |
|                                                                             |
/*****************************************************************************/
/*Put these headers in autoexec.sas*/
/*Specify location of modspace macros*/
options mstored sasmstore=modmacr;
libname modmacr "E:\PROJECT\Acorda\testlib";

/*Specify modspace server*/
options set=modserver="w2008r2-mod";

/*Define global variables of username, password and temp directory*/
%global user pass tmpDir;

/*****************************************************************************|
|  checkDir: Check if a directory exists                                      |
|                                                                             |
|    @param dir Directory to be checked                                       |
|                                                                             |
|    @examples:                                                               |
|                                                                             |
|     %LET CHECKDIR = %chk_dir(dir=C:\temp\);                                 |
|     %put &CHECKDIR.;                                                        |
|                                                                             |
|     %LET CHECKDIR = %chk_dir(dir=C:\temp\sdgdfgd);                          |
|     %put &CHECKDIR.;                                                        |
/*****************************************************************************/
%MACRO chk_dir(dir=)/store source;
    /* -- Init exit code;*/
    %LOCAL exitCode;
    %LET exitCode = 1;

    %LOCAL rc fileref;
    %LET rc = %SYSFUNC(filename(fileref,&dir)) ;
    %IF %SYSFUNC(fexist(&fileref))  %THEN %DO;
       %put NOTE: The directory "&dir" exists ;
       %LET exitCode = 0;
    %END;
    %ELSE %DO;
       %put NOTE: The directory "&dir" does not exists ;
       %LET exitCode = 1;
    %END;
    &exitCode.
%MEND chk_dir ;
/*****************************************************************************|
|  dirInDir: Return the name of the directory inside the dir                  |
|                                                                             |
|    @param dir Directory                                                     |
|                                                                             |
|    @note  It will return only one name (ie the lastest order by alphabetic  |
|           order                                                             |
|                                                                             |
|    @examples:                                                               |
|        %LET DIRNAME =%dirInDir("C:\");                                      |
|        %put &DIRNAME.;                                                      |
/*****************************************************************************/
%MACRO dirInDir(dir,ext);
    %LET filrf=mydir;
    /* -- Assigns the fileref of mydir to the directory and opens the directory;*/
    %LET rc=%SYSFUNC(filename(filrf,&dir));
    %LET did=%SYSFUNC(dopen(&filrf));
    /* -- Returns the number of members in the directory;*/
    %LET memcnt=%SYSFUNC(dnum(&did));
    /* -- Loops through entire directory;*/
    %DO i = 1 %to &memcnt;
        /* -- Returns the name of the file;*/
        %LET fileName = %qsysfunc(dread(&did,&i));
    %END;
    /* -- Closes the directory;*/
    %LET rc=%SYSFUNC(dclose(&did));
    &filename
%MEND dirInDir;                                                                                                                           

/*****************************************************************************|
| removeDir: Remove a directory                                               |
|                                                                             |
|    @param dir Directory to be removed                                       |
|                                                                             |
|    @return                                                                  |
|        0: Directory successfully removed                                    |
|        1: Direcory does not exists                                          |
|        2: Fail to remove directory                                          |
|    @examples                                                                |
|        %LET CREATEDIR = %createDir(dir=C:\temp\sasMod21565);                |
|        %LET REMOVEDIR = %removeDir(C:\temp\sasMod21565);                    |
|        %put &REMOVEDIR.;                                                    |
/*****************************************************************************/
%MACRO removeDir(dir=)/store source;
    /* -- Init exit code*/
    %LOCAL exitCode;
    %LET exitCode = 1;
    /* --  Check if folder exists;*/
    %LET CHECKDIR = %chk_dir(dir=&dir.);
    %IF &CHECKDIR. EQ 1 %THEN %DO;
        %LET exitCode = 1;
    %END;
    %ELSE %DO;
        /* --  Remove directory and sub-directory trought system call;*/
        %sysexec cmd /C rmdir /Q/S "&dir.";
        %IF &sysrc. EQ 0  %THEN %DO;
            %put The directory &dir and its sub-directories have been Deleted.;
            %LET exitCode = 0;
        %END;
        %ELSE %DO;
            %put There was a problem while Removing the directory &dir.;
            %LET exitCode = 2;
        %END;
    %END;
    &exitCode
%MEND ;

/*****************************************************************************|
|  createDir: Macro to Create a directory                                     |
|                                                                             |
|    @param dir Directory to be created                                       |
|                                                                             |
|    @return:                                                                 |
|        0: Directory successfully created                                    |
|        1: The directory already exists                                      |
|        2: There was a problem while creating the directory                  |
|    @example                                                                 |
|        %LET CREATEDIR = %createDir(dir=C:\temp\sasMod21565);                |
|        %put &CREATEDIR.;                                                    |
/*****************************************************************************/
%MACRO createDir(dir=)/store source;
    /* -- Init exit code;*/
    %LOCAL exitCode;
/*    %LET exitCode = 1;*/
    /* -- Check if folder exists;*/
    %LET CHECKDIR = %chk_dir(dir=&dir.);
    %put &CHECKDIR.;
    %IF &CHECKDIR. EQ 0 %THEN %DO;
        %LET exitCode = 1;
        %put  NOTE: The directory "&dir." already exists.;
    %END;
    %ELSE %DO;
        /* -- Create directory trought system call;*/
        %sysexec cmd /C mkdir "&dir.";
        %IF &sysrc. EQ 0 %THEN %DO;
            %put NOTE: The directory &dir. has been created.;
            %LET exitCode = 0;
        %END;
        %ELSE %DO;
            %put NOTE: There was a problem while creating the directory &dir;
            %LET exitCode = 2;
        %END;
     %END;
     &exitCode.
%MEND createDir;
/*****************************************************************************\
| tempDir: Return a path to a temporary directory which is build the          |
|          following way:                                                     |
|                                                                             |
|     C:\Users\<USERNAME>\AppData\Local\Temp\ModSpaceCheckout_<randomElement>;|
|                                                                             |
|    @examples :                                                              |
|        %LET tmp = %tempDir();                                               |
|        %put &tmp.;                                                          |
\*****************************************************************************/
%MACRO tempDir()/store source;
    /* -- Base temp directory */
    %LET baseName = C:\Users\&sysuserid.\AppData\Local\Temp\;
    /* -- Generate a random element based on current time */
    %LET rdmSuf = %SYSFUNC(time());
/*    %LET rdmSuf = %substr(&rdmSuf.,%eval(%length(&rdmSuf.)- 2));*/
    /* -- Concatenate the elements */
    %LET tempPath = &baseName.ModSpaceCheckout_&rdmSuf.;
    &tempPath
%MEND tempDir;

/*****************************************************************************\
| login:                                                                      |
|    prompts the user for their username and password. This information       |
|    is stored in the session and utilised by the other functions             |
|                                                                             |
|                                                                             |
\*****************************************************************************/
%MACRO login /store source;
%window info         
  #5 @5 'Please enter userid:'       
  #5 @26 user 20 attr=underline                
  #7 @5 'Please enter password:'            
  #7 @28 pass 20 attr=underline display=no;           
       
%display info;        
%LET tmpDir = %tempDir();
** Create tmpdir;
%LET CREATEDIR = %createDir(dir=&tmpDir.);
%MEND;  

/*****************************************************************************\
| libname:                                                                    |
|    Checkout a modspace entry and assign the libname to the local checkout.  |
|    The macro also register those information into the work._mod table       |
|                                                                             |
|    @param libname                                                           |
|    @param Identifier                                                        |
|                                                                             |
|    @examples:                                                               |
|                                                                             |
|        %libname(libname=mylib2, Identifier=1101/Structured/Data)            |
|                                                                             |
|                                                                             |
|                                                                             |
\*****************************************************************************/
%MACRO libname(libname=, Identifier=) /store source;
    ** Options - TODO: revert them back at the end of the macro;
    options xsync xmin noxwait;
    /*%login*/
    ** Build the URL;
    %LET url = http://%sysget(modserver)/svn/modspace/trunk/&Identifier./; 
    %PUT NOTE: Set &libname. to &url.;
    %PUT NOTE: Using &tmpDir.\&libname. as local library;

    ** Create tmpdir\libname directory;
        %LET CREATEDIR = %createDir(dir=&tmpDir.\&libname.);
        %IF &CREATEDIR. EQ 0 %THEN %DO;
             ** Checkout modspace entry;
            %SYSEXEC cmd /C svn checkout &url. &tmpDir.\&libname. --username &user. --password &pass. --no-auth-cache;
            %IF &sysrc. EQ 0  %THEN %DO;
                ** Assign libname to tmp;
                LIBNAME  &libname. "&tmpDir.\&libname.";
            %END;
            %ELSE %DO; /* -- %IF &sysrc. EQ 0  %THEN %DO;*/
                 %PUT ERROR: Something went wrong during checking out;
            %END;
        %END;
        %ELSE %DO; /* -- %IF &CREATEDIR. EQ 0 %THEN %DO;*/
            %PUT ERROR: Temp directory could not be created;
        %END;
%MEND libname;

/*****************************************************************************\
| checkIn:                                                                    |
|    Commit back to svn any new files and modification found in the local     |
|    checkout associated with the libname. The local folder is also removed,  |
|    the libname deassigned and work._mod is updated                          |
|                                                                             |
|    @note : TODO: Modspace sync                                              |
|                                                                             |
|    @param libname: Name of an assigned libname                              |
|    @param msg: Commit message                                               |
|                                                                             |
|    @examples                                                                |
|        **http://w2008r2-mod/modspace/;                                      |
|        %libname(libname=Basic, Identifier=1022/Basic);                      |
|                                                                             |
|        * SAS CODE HERE;                                                     |
|        data MyLib.iris;                                                     |
|            set Sashelp.iris;                                                |
|         RUN;                                                                |
|        data MyLib.iris;                                                     |
|            set  MyLib.iris;                                                 |
|            PetalLength = 0;                                                 |
|        RUN;                                                                 |
|                                                                             |
|        %checkIn(libname=MyLib, msg="Set Petal length to 0");                |
\*****************************************************************************/
%MACRO checkIn(libname=, msg="No message") /store source;
    ** Add new files; 
    %sysexec cmd /C svn add --force  %cmpres(%SYSFUNC(PATHNAME(&libname.)))/\* --depth infinity --auto-props --parents --username &user. --password &pass. --no-auth-cache;
    %IF &sysrc. EQ 0 %THEN %DO;
        %PUT NOTE: New files added sucessfully;
        ** Commit modifications;
        %sysexec cmd /C svn commit %cmpres(%SYSFUNC(PATHNAME(&libname.))) -m &msg. --username &user. --password &pass. --no-auth-cache;
        %IF &sysrc. EQ 0 %THEN %DO;
            %PUT NOTE: Successful commit;   
            ** Remove library directory;
            %LET REMOVEDIR = %removeDir(dir=%cmpres(%SYSFUNC(PATHNAME(&libname.))));  
            ** Remove library reference;
            libname &libname.; 
        %END;
        %ELSE %DO;
            %PUT ERROR: There was a problem while committing (error code &sysrc.);
        %END;
    %END;
    %ELSE %DO;
        %PUT ERROR: Something went wrong when adding new files;
    %END;
%MEND checkIn;
  
/*****************************************************************************\
| finalize:                                                                   |
|    Tell ModSpace that the session is complete and that it should            |
|    synchronize its contents with the underlying repository.                 |
|                                                                             |
|    @param Identifier: Entry ID                                              |
|                                                                             |
|    @examples:                                                               |
|                                                                             |
|        %finalize(Identifier = 1022);                                        |
|                                                                             |
|                                                                             |
|                                                                             |
\*****************************************************************************/
%MACRO finalize(Identifier = ) /store source;     
    ** Sync with ModSpace;
    x "curl http://modspacewsuser:navmodws01@%sysget(MODSERVER)/modspace/cxf/services/entryLookup/synchronizeEntry/&Identifier./for/&user./with/&pass.";
    %IF &sysrc. EQ 0 %THEN %DO;
        %PUT NOTE: Successful sync with ModSpace;
        ** Remove tmp directory;
/*		libname _all_ clear;*/
		%LET REMOVEDIR = %removeDir(dir=%cmpres(&tmpDir.));
/*		%sysexec cmd /C del /S /F "&tmpDir.";*/
/*       %sysexec cmd /C rmdir /Q/S "&tmpDir.";*/
/*		%sysexec cmd /C rm -r "&tmpDir.";*/
    %END;
    %ELSE %DO;
        %PUT ERROR: Sync with ModSpace failed;
    %END;
%MEND finalize;

/*****************************************************************************\
| complete:                                                                   |
|    Finish checkin and sync at the same time.                                |
|                                                                             | 
|    @param Identifier: Entry ID                                              |
|    @param libname: Name of an assigned libname                              |
|    @param msg: Commit message                                               |
|                                                                             | 
|    @examples                                                                |
|        **http://w2008r2-mod/modspace/;                                      |
|        %libname(libname=MyLib,Identifier=1022/Basic);                       |
|                                                                             |
|        * SAS CODE HERE;                                                     |
|        data MyLib.bmt;                                                      |
|            set Sashelp.bmt;                                                 |
|        RUN;                                                                 |
|                                                                             |
|        %complete(Identifier=1022, libname=MyLib, msg="Add dataset bmt");    |
|                                                                             |
\*****************************************************************************/
%MACRO complete(Identifer=, libname=, msg="No message") /store source;
    %checkIn(libname=&libname., msg="No message");
    %finalize(Identifer=&Identifier.);
%MEND complete;