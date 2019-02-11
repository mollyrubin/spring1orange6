

PROC IMPORT OUT= WORK.doe2 
            DATAFILE= "C:\Users\molly\OneDrive\Documents\_MSA 2018 COURSEWORK\DOE\orange team 6.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/*overall yes/no distribution*/
proc freq data=doe2;
	tables will_attend;
run;
/*only 8.6% will attend*/

/*Distribution of yes/no's for categorical demographics*/

proc freq data=doe2;
	tables will_attend*race;
	tables will_attend*sex;
	tables will_attend*income;
	tables will_attend*location;
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


proc genmod data=doe2; 
		class location price experience other; 
		model will_attend = location price experience other/dist   = bin link   = logit; 
	ESTIMATE "Experience comparison e1 vs e2" experience 1 -1 0;
	estimate 'Experience comparison e1 vs e3' experience 1 0 -1;
    estimate 'Experience comparison e2 vs e3' experience 0 1 -1;
	ESTIMATE "Other comparison o1 vs o2" other 1 -1 0 0;
    estimate 'Other comparison o1 vs o3' other 1 0 -1 0;
    estimate 'Other comparison o1 vs o4' other 1 0 0 -1;
    estimate 'Other comparison o2 vs o4' other 0 1 0 -1;
    estimate 'Other comparison o3 vs o4' other 0 0 1 -1;
    estimate 'Price comparison p1 vs p2' other 1 -1 0 0;
	estimate 'Price comparison p1 vs p3' other 1 0 -1 0;
	estimate 'Price comparison p1 vs p4' other 1 0 0 -1;
	estimate 'Price comparison p2 vs p4' other 0 1 0 -1;
	estimate 'Price comparison p3 vs p4' other 0 0 1 -1;
	estimate 'Location comparison loc1 vs loc2' location 1 -1 0 0 0;
    estimate 'Location comparison loc1 vs loc3' location 1 0 -1 0 0;
    estimate 'Location comparison loc1 vs loc4' location 1 0 0 -1 0;
    estimate 'Location comparison loc1 vs loc5' location 1 0 0 0 -1;
    estimate 'Location comparison loc2 vs loc3' location 0 1 -1 0 0;
    estimate 'Location comparison loc2 vs loc4' location 0 1 0 -1 0;
    estimate 'Location comparison loc2 vs loc4' location 0 1 0 0 -1;
    estimate 'Location comparison loc3 vs loc4' location 0 0 1 -1 0;
    estimate 'Location comparison loc3 vs loc5' location 0 0 1 0 -1;
    estimate 'Location comparison loc4 vs loc5' location 0 0 0 1 -1;
run; 
quit; 






