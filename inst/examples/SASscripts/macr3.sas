%MACRO macr3(libname=, msg="No message");
    /* Add new files */
    %sysexec cmd /C svn add --force  %cmpres(%SYSFUNC(PATHNAME(&libname.)))/\* --depth infinity --auto-props --parents;

    %IF &sysrc. EQ 0 %THEN %DO;
        %PUT NOTE: New files added sucessfully;

        /* Check if credentials exist */
	    %IF (%length(&user)=0 or %length(&pass)=0) %THEN %DO; 
		    %macr1;
		%END;

        /* Commit modifications */
        %sysexec cmd /C svn commit %cmpres(%SYSFUNC(PATHNAME(&libname.))) -m &msg. --username &user. --password &pass. --no-auth-cache --non-interactive;

        %IF &sysrc. EQ 0 %THEN %DO;
            %PUT NOTE: Successful commit;
            /* Delete the library that has been checked in */
            %util5(lib=&libname.); 
        %END;
        %ELSE %DO;
            %PUT ERROR: There was a problem while committing (error code &sysrc.);
			%PUT NOTE: If you want to change credential information, please login again.;
        %END;
    %END;
    %ELSE %DO;
        %PUT ERROR: Something went wrong when adding new files.;
    %END;

%MEND macr3;

