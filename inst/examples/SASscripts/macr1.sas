%MACRO macr1;
  %WINDOW info      
    #1 @2 'MODSPACE LOGIN:' attr=highlight  
    #5 @5 'Please enter userid:'       
    #5 @26 user 20 attr=underline                
    #7 @5 'Please enter password:'            
    #7 @28 pass 20 attr=underline display=no;              
  %DISPLAY info;   
%MEND macr1; 
