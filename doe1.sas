libname desktop '\\vmware-host\Shared Folders\Desktop';

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

/*Use 5040 or a different amount to sample from entire group, then assign everything randomly*/

data new;
	set desktop.rduch;
run;

proc surveyselect data=new
   method=srs n=5040 out=SampleSRS;
run;

data Unif;
set SampleSRS;
drop Var1 Long Lat u;
call streaminit(123);
u = rand("Uniform"); 
Location = ceil( 5*u );
Price = ceil( 4*u );
Experience = ceil( 3*u );
Other = ceil( 4*u );
output;
run;

proc export data=unif
   outfile="\\vmware-host\Shared Folders\Desktop\doe1.csv" 
   dbms=csv
   replace;
run;
