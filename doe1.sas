PROC IMPORT OUT= WORK.doe 
            DATAFILE= "\\vmware-host\Shared Folders\Desktop\Counter.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC GLMPOWER DATA=work.doe;
	CLASS var1 price experience other;  /*NOTE THIS SYNTAX IS EXACTLY LIKE PROC GLM*/ 
	MODEL pr = var1 price experience other; 
	CONTRAST "Experience comparison" experience 1 -1 0;
	CONTRAST "Price comparison" price 1 -1 1 -1;
	CONTRAST "Location comparison" var1 1 -1 1 -1 0;
	CONTRAST "Other comparison" other 1 -1 1 -1;
	/*CONTRAST "Something"  Experience  1 -1 0 Experience*Price 0.5 -0.5 0 0.5 -0.5 0 0 0 0 0 0 0;*/
	POWER  
		STDDEV = 0.09949 /*Mean Square Error = MSE^0.5*/
		NTOTAL = .  /*TOTAL OBSERVATIONS IN THE STUDY*/ 
		POWER  = .8;
RUN;
