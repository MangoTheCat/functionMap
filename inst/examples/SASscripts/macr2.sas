%MACRO macr2(libname=, Identifier=, folder=);
	/* Create the work._mod table to keep track of libraries */
    %IF not %SYSFUNC(exist(work._mod)) %THEN %DO;
        %PUT NOTE: Creating work._mod to keep track of libraries checkout;
        PROC SQL;
            create table work._mod
            (LibName char(20), Identifier char(100), Url char(100), tempDir char(200));
        QUIT;
    %END;

    /* Build the URL */
    %LET url = http://%sysget(modserver)/svn/modspace/trunk/&Identifier./&folder.; 
    %PUT NOTE: Set &libname. to &url.;

    /* Create tmpdir */
    %LET tmpDir = %util4();
	%PUT NOTE: Using &tmpDir. as local library;
    %LET CREATEDIR = %util3(dir=&tmpDir.);
    %IF &CREATEDIR. EQ 0 %THEN %DO;

        /* Check if credentials exist */
	    %IF (%length(&user)=0 or %length(&pass)=0) %THEN %DO; 
		    %macr1;
		%END;

        /* Checkout */
        %SYSEXEC cmd /C svn checkout "%cmpres(&url.)" &tmpDir. --username &user. --password &pass. --no-auth-cache --non-interactive;

        %IF &sysrc. EQ 0  %THEN %DO;
            /* Assign libname to tmp */
            LIBNAME  &libname. "&tmpDir.";
            /* Register the library in the work._mod table */
            PROC SQL;
                insert into work._mod
                values("&libname.", "&Identifier.", "&url.", "&tmpDir.");
            QUIT;
        %END;
        %ELSE %DO; 
            %PUT ERROR: Error checking out. Please check the entry details are correct, that you have write permissions and that your login details are valid.;
            /* Remove directory */
            %LET REMOVEDIR = %util2(dir=%cmpres(&tmpDir.)); /*	%sysexec cmd /C rm -r "%cmpres(&tmpDir.)";*/
        %END;
      
    %END;
    %ELSE %DO;
        %PUT ERROR: Temp directory could not be created;
    %END;	
%MEND macr2;
