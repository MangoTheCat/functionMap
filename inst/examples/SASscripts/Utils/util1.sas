%MACRO util1(dir=);
    /* Init exit code; */
    %LOCAL exitCode;
    %LET exitCode = 1;

    %LOCAL rc fileref;
    %LET rc = %SYSFUNC(filename(fileref,&dir)) ;
    %IF %SYSFUNC(fexist(&fileref))  %THEN %DO;
       %PUT NOTE: The directory "&dir" exists ;
       %LET exitCode = 0;
    %END;
    %ELSE %DO;
       %PUT NOTE: The directory "&dir" does not exists ;
       %LET exitCode = 1;
    %END;
    &exitCode.
%MEND util1; 