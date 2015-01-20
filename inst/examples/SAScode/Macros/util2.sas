%MACRO util2(dir=) /store source;
    /* Init exit code; */
    %LOCAL exitCode;
    %LET exitCode = 1;
    /* Check if folder exists; */
    %LET CHECKDIR = %util1(dir=&dir.);
    %IF &CHECKDIR. EQ 1 %THEN %DO;
        %LET exitCode = 1;
    %END;
    %ELSE %DO;
        /* Remove directory and sub-directory throught system call; */
        %sysexec cmd /C rmdir /Q/S "&dir."; /*%sysexec cmd /C rm -r "&dir.";*/
        %IF &sysrc. EQ 0  %THEN %DO;
            %PUT NOTE: The directory &dir and its sub-directories have been deleted.;
            %LET exitCode = 0;
        %END;
        %ELSE %DO;
            %PUT NOTE: There was a problem while Removing the directory &dir.;
            %LET exitCode = 2;
        %END;
    %END;
    &exitCode
%MEND ;