select * 
from dbo.instr

where TSTNO = 485
;


--deduplicated by creating a new table and union everything 
Select * into dbo.instr FROM 
dbo.inst 
Union
select * 
from dbo.inst;


--just testing numbers below
select count (TSTNO) FROM Dbo.instr;

select * FROM Dbo.tst;

drop table dbo.test