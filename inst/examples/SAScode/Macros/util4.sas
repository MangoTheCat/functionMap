%MACRO util4();
    /* Base temp directory */
    %LET baseName = C:\Users\&sysuserid.\AppData\Local\Temp\;
    /* Generate a random element based on current time */
    %LET rdmSuf = %SYSFUNC(time());
    /* Concatenate the elements */
    %LET tempPath = &baseName.ModSpaceCheckout_&rdmSuf.;
    &tempPath
%MEND util4;

