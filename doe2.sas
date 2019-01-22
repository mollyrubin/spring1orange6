libname desktop 'C:\Users\francisw\Documents\MSA_2019\course_work\courses_spring\spring1\design_of_experiments\spring1orange6-master\spring1orange6-master';

PROC IMPORT OUT= WORK.doe 
            DATAFILE= "C:\Users\francisw\Documents\MSA_2019\course_work\courses_spring\spring1\design_of_experiments\spring1orange6-master\spring1orange6-master\Counter.csv" 
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
drop Var1 Long Lat u_loc u_price u_exp u_other;
call streaminit(123);
u_loc = rand("Uniform");
u_price = rand("Uniform");
u_exp = rand("Uniform");
u_other = rand("Uniform"); 
Location = ceil( 5*u_loc );
Price = ceil( 4*u_price );
Experience = ceil( 3*u_exp );
Other = ceil( 4*u_other );
output;
run;

proc export data=unif
   outfile="C:\Users\francisw\Documents\MSA_2019\course_work\courses_spring\spring1\design_of_experiments\spring1orange6-master\spring1orange6-master\doe2.csv" 
   dbms=csv
   replace;
run;
