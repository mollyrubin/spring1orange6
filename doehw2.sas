PROC IMPORT OUT= WORK.doe2 
            DATAFILE= "\\vmware-host\Shared Folders\Desktop\orange team 6.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/*Distribution of yes/no's for categorical demographics*/
proc freq data=doe2;
	tables will_attend*race;
	tables will_attend*sex;
	tables will_attend*income;
run;

/*Distribution of age*/
proc univariate data=doe2;
	var ages;
	histogram;
run;

/*What park features people said they will/won't attend*/
proc freq data=doe2;
	tables will_attend*location;
	tables will_attend*price;
	tables will_attend*experience;
	tables will_attend*other;
run; 


/*General set up for final model, not sure exactly what this is supposed to look like
  or what the response variable is supposed to be, since we're trying to maximize profit*/
proc glm data = doe2; 
	class location price experience other; 
	model will_attend = location price experience other;
	lsmeans location / cl adjust=TUKEY;
	lsmeans price / cl adjust=TUKEY;
	lsmeans experience / cl adjust=TUKEY;
	lsmeans other / cl adjust=TUKEY;
run; 
quit; 
