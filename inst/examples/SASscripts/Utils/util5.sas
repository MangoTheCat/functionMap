%MACRO util5(lib=); 
  /* Delete a library */ 
  /* Remove row from registry */ 
  PROC SQL NOPRINT;
    delete from work._mod where LibName="&lib.";
  QUIT;
  /* Remove library directory */ 
  %LET REMOVEDIR = %util2(dir=%cmpres(%SYSFUNC(PATHNAME(&lib.))));  
  /* Remove library reference */ 
  libname &lib.; 
%MEND util5;
