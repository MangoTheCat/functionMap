%MACRO macr4(Identifier = );     

    /* Create a temp file report.txt to hold output information from command window */
    %LET tmpfile = C:\Users\&sysuserid.\AppData\Local\Temp\report.txt;
    
    /* Create ModSpace URL */
    %LET modurl=http://modspacewsuser:navmodws01@%sysget(MODSERVER)/modspace/cxf/services/entryLookup/synchronizeEntry/&Identifier./for/&user./with/&pass.;
    
    /* Sync with ModSpace */
    X "curl &modurl -o &tmpfile";

    /* Check if report.txt exists */
    %LET CHECKDIR = %util1(dir=&tmpfile);
    %PUT &CHECKDIR.;
	%PUT NOTE: report.txt contains response from command window;
	%IF (&CHECKDIR. EQ 0 & &sysrc. EQ 0) %THEN %DO;
        %PUT NOTE: TRUE;
	    %PUT NOTE: Successful sync with ModSpace;

        /* Create variables to key each libraries that have been checked out but not checked in */
        proc sql noprint;
            select COUNT(*) into: num_values from work._mod where Identifier="&Identifier"; 
            %LET num_values=&num_values;
            %IF &num_values=0 %THEN %DO;
                %PUT NOTE: All libraries and temp directories have been removed;
            %END;
            %ELSE %DO;
                select libname into: libref1-:libref&num_values from work._mod where Identifier="&Identifier";
            %END;
        quit;
    
        /* Remove all libraries within the entry */
        %IF &num_values>0 %THEN %DO;
            %LOCAL j;
            %DO j=1 %TO &num_values;
                %util5(lib=&&libref&j);
            %END;
        %END;
    %END;

    %ELSE %DO;
        %PUT ERROR: Sync with ModSpace failed.;
		%PUT NOTE: If you want to change credential information, please login again.;
    %END;

    /* Delete the temp file */
    %sysexec cmd /C rm "&tmpfile.";
%MEND macr4;
