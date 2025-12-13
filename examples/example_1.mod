# Example 1: Simple production planning
var x1 >= 0;
var x2 >= 0;
maximize profit: 40 * x1 + 30 * x2;
s.t. labor: 2 * x1 + x2 <= 100;
s.t. material: x1 + 2 * x2 <= 80;
end;
