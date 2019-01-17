/*THIS IS BASED UPON THE RESULTS FROM THE STUDY*/ 
PROC GLMPOWER DATA=doe;
	CLASS location price experience other;  /*NOTE THIS SYNTAX IS EXACTLY LIKE PROC GLM*/ 
	MODEL pr = location price experience other price*experience; 
	CONTRAST "Experience1 vs. Experience2" experience 1 -1 0;
	CONTRAST "Experience2 vs. Experience3" experience 0 1 -1;
	CONTRAST "Price1 vs. Price2" price 1 -1 0 0;
	CONTRAST "Something"  Experience  1 -1 0 Experience*Price 0.5 -0.5 0 0.5 -0.5 0 0 0 0 0 0 0;
	POWER  /*THIS IS THE ONLY PART YOU NEED TO WORRY ABOUT*/ 
		STDDEV = 0.09949 /*Mean Square Error = MSE^0.5*/
		NTOTAL = .  /*TOTAL OBSERVATIONS IN THE STUDY*/ 
		POWER  = .8;
RUN; 

/*THIS IS BASED UPON THE RESULTS FROM THE STUDY*/ 
/*BECAUSE NOTHING IS GOING ON IN THE FIRST MODEL LOOK WHAT HAPPENS
  IF I ADD AN INTERACTION*/ 
