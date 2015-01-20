%MACRO util3(dir=);
    /* Init exit code;*/
    %LOCAL exitCode;
    %LET exitCode = 1;
    /* Check if folder exists;*/
    %LET CHECKDIR = %util1(dir=&dir.);
    %put &CHECKDIR.;
    %IF &CHECKDIR. EQ 0 %THEN %DO;
        %LET exitCode = 1;
        %put  NOTE: The directory "&dir." already exists.;
    %END;
    %ELSE %DO;
        /* Create directory trought system call;*/
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
%MEND util3;
