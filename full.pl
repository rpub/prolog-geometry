% full.pl

% Tests for 3D point processing:
% query(isosceles3D(point3d(3,-2,-3), point3d(7,0,1), point3d(1,2,1))).
% query(isosceles3D(point3d(5,-1,2), point3d(-1,1,2), point3d(1,-3,-2))).
% query(equilateral3D(point3d(-1,1,3), point3d(-1,3,5), point3d(-3,3,3))).
% query(equilateral3D(point3d(-1,1,3), point3d(-1,3,5), point3d(-3,3,3))).
% query(right3D(point3d(0,0,0), point3d(6,0,6), point3d(6,0,0))).
% query(scalene3D(point3d(14,2,3), point3d(2,3,1), point3d(5, 7,2))).
% query(acute3D(point3d(0,0,3), point3d(3,30,2), point3d(3,3,4))).
% query(acute3D(point3d(12,12,3), point3d(12,12,1), point3d(0,23,5))).
% query(obtuse3D(point3d(0,0,3), point3d(3,30,2), point3d(3,3,4))).
% query(obtuse3D(point3d(12,12,3), point3d(12,12,1), point3d(0,23,5))).

% point2d() structure
point2d(X,Y):-number(X), number(Y).

% point3d() structure
point3d(X,Y,Z):-number(X), number(Y), number(Z).

	% helpers...
	isVertical(point2d(X1,_),point2d(X2,_),point2d(X3,_)) :- 
		X1 =:= X2, 
		X2 =:= X3.
	isHorizontal(point2d(_,Y1),point2d(_,Y2),point2d(X3,_)) :- 
		Y1 =:= Y2, 
		Y2 =:= X3.
	isDiagonal(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)) :- 
		(Y2-Y1)/(X2-X1) =:= (Y3-Y2)/(X3-X2).

	% pythagorean: a + b < c
	pythObtuse(A, B, C):- float(round((A)^2 + (B)^2)) < float(round((C)^2)).
	
	% generates side lengths for point2d()
	dimensions(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3), AB, BC, AC) :- 
		(AB is sqrt((X1-X2)**2 + (Y1-Y2)**2)),	% a to b
		(BC is sqrt((X3-X2)**2 + (Y3-Y2)**2)),	% b to c
		(AC is sqrt((X1-X3)**2 + (Y1-Y3)**2)).	% a to c

	% generates side lengths for point3d()
	dimensions3D(point3d(X1,Y1,Z1), point3d(X2,Y2,Z2), point3d(X3,Y3,Z3), AB, BC, AC) :- 
		(AB is sqrt((X1-X2)**2 + (Y1-Y2)**2 + (Z1-Z2)**2)),	% a to b
		(BC is sqrt((X3-X2)**2 + (Y3-Y2)**2 + (Z3-Z2)**2)),	% b to c
		(AC is sqrt((X1-X3)**2 + (Y1-Y3)**2 + (Z1-Z3)**2)).	% a to c

% vertical line, all Y-s share X-value
vertical(point2d(X1,_), point2d(X2,_)):- X1=:=X2.

% horizontal, X-s share Y-value
horizontal(point2d(_,Y1), point2d(_,Y2)):- Y1=:=Y2.

% line can be vertical, horizontal, or diagonal
line(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)) :- 
    isVertical(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3));
    isHorizontal(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3));
	isDiagonal(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)).

% triangle can be any 3 points that don't form a line 
triangle(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)) :- 
    not(line(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3))).

% Isosceles in 2D - true if any two sides are equal 
isosceles(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)) :- 
	triangle(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)),
	dimensions(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3), AB, BC, AC),
	(
		(AB =:= BC);
		(AB =:= AC); 
		(BC =:= AC)
	).

% Isosceles in 3D
isosceles3D(point3d(X1,Y1,Z1), point3d(X2,Y2,Z2), point3d(X3,Y3,Z3)) :- 
	dimensions3D(point3d(X1,Y1,Z1), point3d(X2,Y2,Z2), point3d(X3,Y3,Z3), AB, BC, AC),
	(
		(AB =:= BC);
		(AB =:= AC); 
		(BC =:= AC)
	).

% Equilateral in 2D - true if all three sides are equal
equilateral(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)) :- 
	triangle(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)),

	(AB is round(sqrt((X1-X2)**2 + (Y1-Y2)**2))),	% a to b
	(BC is round(sqrt((X3-X2)**2 + (Y3-Y2)**2))),	% b to c
	(AC is round(sqrt((X1-X3)**2 + (Y1-Y3)**2))),	% a to c
	(
		(AB =:= BC),
		(AB =:= AC),
		(BC =:= AC)
	).

% Equilaterial in 3D
equilateral3D(point3d(X1,Y1,Z1), point3d(X2,Y2,Z2), point3d(X3,Y3,Z3)):-
	(AB is round(sqrt((X1-X2)**2 + (Y1-Y2)**2 + (Z1-Z2)**2))),	% a to b
	(BC is round(sqrt((X3-X2)**2 + (Y3-Y2)**2 + (Z3-Z2)**2))),	% b to c
	(AC is round(sqrt((X1-X3)**2 + (Y1-Y3)**2 + (Z1-Z3)**2))),	% a to c
	(
		(AB =:= BC),
		(AB =:= AC),
		(BC =:= AC)
	).

% Right in 2D
right(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)) :- 
	triangle(point2d(X1,Y1), point2d(X2,Y2), point2d(X3,Y3)), 
	(
		((X2-X1)*(X3-X1)+(Y2-Y1)*(Y3-Y1)) =:= 0;
		((X3-X2)*(X3-X1)+(Y3-Y1)*(Y3-Y2)) =:= 0
	).

% Right in 3D
right3D(point3d(X1,Y1,Z1), point3d(X2,Y2,Z2), point3d(X3,Y3,Z3)):-
	dimensions3D(point3d(X1,Y1,Z1), point3d(X2,Y2,Z2), point3d(X3,Y3,Z3), AB, BC, AC), 
	(
		round((AB**2) + (AC**2)) =:= round(BC**2);
		round((AB**2) + (BC**2)) =:= round(AC**2);
		round((BC**2) + (AC**2)) =:= round(AB**2)
	).

% Scalene in 2D
scalene(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)) :-
	triangle(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)),
	dimensions(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3), AB, BC, AC),
	(
		(AB =\= BC),
		(AB =\= AC),
		(BC =\= AC)
	).

% Scalene in 3D
scalene3D(point3d(X1,Y1,Z1), point3d(X2,Y2,Z2), point3d(X3,Y3,Z3)) :-
	dimensions3D(point3d(X1,Y1,Z1), point3d(X2,Y2,Z2), point3d(X3,Y3,Z3), AB, BC, AC),	(
		(AB =\= BC),
		(AB =\= AC),
		(BC =\= AC)
	).

% Acute in 2D
acute(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)) :- 
    triangle(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)),
    not(obtuse(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3))),
    not(right(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3))).

% Acute in 3D
acute3D(point3d(X1,Y1,Z1), point3d(X2,Y2,Z2), point3d(X3,Y3,Z3)) :- 
    not(obtuse3D(point3d(X1,Y1,Z1),point3d(X2,Y2,Z2),point3d(X3,Y3,Z3))),
    not(right3D(point3d(X1,Y1,Z1),point3d(X2,Y2,Z2),point3d(X3,Y3,Z3))).

% Obtuse in 2D
obtuse(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)) :- 
	triangle(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3)),
	dimensions(point2d(X1,Y1),point2d(X2,Y2),point2d(X3,Y3), AB, BC, AC),
	(
		(((AB > BC),(AB > AC)), (pythObtuse(AC, BC, AB)));
		(((BC > AB),(BC > AC)), (pythObtuse(AC, AB, BC)));
		(((AC > AB),(AC > BC)), (pythObtuse(BC, AB, AC)))
	).

% Obtuse in 3D
obtuse3D(point3d(X1,Y1,Z1), point3d(X2,Y2,Z2), point3d(X3,Y3,Z3)) :- 
	dimensions3D(point3d(X1,Y1,Z1), point3d(X2,Y2,Z2), point3d(X3,Y3,Z3), AB, BC, AC),
	(
		(((AB > BC),(AB > AC)), (pythObtuse(AC, BC, AB)));
		(((BC > AB),(BC > AC)), (pythObtuse(AC, AB, BC)));
		(((AC > AB),(AC > BC)), (pythObtuse(BC, AB, AC)))
	).
