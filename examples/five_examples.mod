# Example 1: Simple production planning
var x1 >= 0;
var x2 >= 0;
maximize profit: 40 * x1 + 30 * x2;
s.t. labor: 2 * x1 + x2 <= 100;
s.t. material: x1 + 2 * x2 <= 80;

# Example 2: Multi-product with sets
set PRODUCTS;
param profit{p in PRODUCTS};
var production{p in PRODUCTS} >= 0;
maximize total_profit: sum{p in PRODUCTS} profit[p] * production[p];
s.t. capacity: sum{p in PRODUCTS} production[p] <= 1000;

# Example 3: Transportation problem
set PLANTS;
set WAREHOUSES;
param supply{p in PLANTS};
param demand{w in WAREHOUSES};
param cost{p in PLANTS, w in WAREHOUSES};
var ship{p in PLANTS, w in WAREHOUSES} >= 0;
minimize total_cost: sum{i in PLANTS, j in WAREHOUSES} cost[i,j] * ship[i,j];
s.t. supply_limit{i in PLANTS}: sum{j in WAREHOUSES} ship[i,j] <= supply[i];
s.t. meet_demand{j in WAREHOUSES}: sum{i in PLANTS} ship[i,j] >= demand[j];

# Example 4: Investment portfolio
set ASSETS;
param returns{a in ASSETS};
param risk{a in ASSETS};
var invest{a in ASSETS} >= 0;
maximize return: sum{a in ASSETS} returns[a] * invest[a];
s.t. budget: sum{a in ASSETS} invest[a] = 1;
s.t. risk_limit: sum{a in ASSETS} risk[a] * invest[a] <= 0.3;

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
