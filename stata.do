* create matrix
mat outmat = J(6,2,.)

* display matrix
mat list outmat 

* display matrix dimension
matrix A = J(1,2,42) 
local dim (`= rowsof(A)',`=colsof(A)') 
di "`dim'" 

* subsetting columns
mat diff = diff[2,1..1],diff[2,5..6],diff[2,4..4]

* rename column names of matrix
local markernames a b c
matrix rownames all = `markernames'
