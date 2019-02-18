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

/*Tukey comparison for each prediction variable*/
proc glm data = doe2; 
	class location price experience other; 
	model will_attend = location price experience other;
	lsmeans location / cl adjust=TUKEY;
	lsmeans price / cl adjust=TUKEY;
	lsmeans experience / cl adjust=TUKEY;
	lsmeans other / cl adjust=TUKEY;
run; 
quit; 

/*Best combination probabilities based on highest tukey means that were not significantly different*/
/*We want to choose location 2, price 2, experience 3, other 4 even though it has a slightly 
lower probability because it will generate more revenue than price 1*/

proc genmod data = doe2 descending; 
	class location price experience other; 
	model will_attend = location price experience other / dist=bin link=logit;
	estimate "Probability Location 2, Price 1, Experience 2, Other 4 Respond Yes" intercept 1 location 0 1 0 0 0 price 1 0 0 0 experience 0 1 0 other 0 0 0 1;
	estimate "Probability Location 2, Price 2, Experience 2, Other 4 Respond Yes" intercept 1 location 0 1 0 0 0 price 0 1 0 0 experience 0 1 0 other 0 0 0 1;
	estimate "Probability Location 2, Price 1, Experience 3, Other 4 Respond Yes" intercept 1 location 0 1 0 0 0 price 1 0 0 0 experience 0 0 1 other 0 0 0 1;
	estimate "Probability Location 2, Price 2, Experience 3, Other 4 Respond Yes" intercept 1 location 0 1 0 0 0 price 0 1 0 0 experience 0 0 1 other 0 0 0 1;
	estimate "Probability Location 1, Price 1, Experience 2, Other 4 Respond Yes" intercept 1 location 1 0 0 0 0 price 1 0 0 0 experience 0 1 0 other 0 0 0 1;
	estimate "Probability Location 1, Price 2, Experience 2, Other 4 Respond Yes" intercept 1 location 1 0 0 0 0 price 0 1 0 0 experience 0 1 0 other 0 0 0 1;
	estimate "Probability Location 1, Price 1, Experience 3, Other 4 Respond Yes" intercept 1 location 1 0 0 0 0 price 1 0 0 0 experience 0 0 1 other 0 0 0 1;
	estimate "Probability Location 1, Price 2, Experience 3, Other 4 Respond Yes" intercept 1 location 1 0 0 0 0 price 0 1 0 0 experience 0 0 1 other 0 0 0 1;
run; 
quit;
