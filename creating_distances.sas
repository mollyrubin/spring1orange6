libname doe 'C:\Users\francisw\Documents\MSA_2019\course_work\courses_spring\spring1\design_of_experiments\project1';

data doe.distances;
	set doe.Rduch;
	array lats {5} Lat1 - Lat5 (35.89314 35.74628 35.7724 35.90535 35.86696);
	array longs {5} Long1 - Long5 (-78.878130 -78.875880 -78.676540 -79.054280 -78.575981);
	array locs {5} locs1 - locs5;
	min_dist = 1000000;
	closest_location = 0;
	do i = 1 to 5;
		locs(i) = sqrt( ( (lats(i) - LAT)**2 ) + ( ( longs(i) - LONG )**2 ) );
		if locs(i) < min_dist then
			do;
				min_dist = locs(i);
				closest_location = i;
			end;
	end;
	drop Lat1 - Lat5 Long1 - Long5 i VAR1;
run;
