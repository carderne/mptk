# Example 5: Scheduling with data section
set DAYS;
set SHIFTS;
param min_staff{d in DAYS, s in SHIFTS};
var staff{d in DAYS, s in SHIFTS} >= 0, integer;
minimize total_staff: sum{d in DAYS, s in SHIFTS} staff[d,s];
s.t. coverage{d in DAYS, s in SHIFTS}: staff[d,s] >= min_staff[d,s];

data;
set DAYS := Mon Tue Wed;
set SHIFTS := Morning Evening;
param min_staff: Morning Evening :=
Mon 5 3
Tue 4 2
Wed 6 4
;
end;
