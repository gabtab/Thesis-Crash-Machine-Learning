--These functions below give the table column names and detail on them
exec sp_columns instr ;
exec sp_columns tst ;
exec sp_columns veh ;


select 
--used this join to get the test and sensor table together it will need to get sensor readout too
a.TSTNO,
a.[ VERNO], 
a.[ TITLE],
a.[ TSTCFND],
b.[ VEHNO],
b.[ XUNITS],
b.[ XUNITSD],
b.[ YUNITS],
b.[ YUNITSD]



FROM CrashData.dbo.tst as a 

LEFT JOIN CrashData.dbo.instr as b
on a.TSTNO = b.TSTNO

where 
b.[ SENATTD] like('%VEHICLE CG%')
;

select COUNT(DISTINCT(a.TSTNO))
--this just selected the testno of all tests that have a vehicle cg sensor
FROM CrashData.dbo.tst as a 

LEFT JOIN CrashData.dbo.instr as b
on a.TSTNO = b.TSTNO

where 
b.[ SENATTD] like('%VEHICLE CG%')
;

select distinct(a.TSTNO),
--this was the same as above but included the impact angle and point
a.[ IMPANG], --impact angle
a.[ IMPPNT] --impact point

FROM CrashData.dbo.tst as a 


LEFT JOIN CrashData.dbo.instr as b
on a.TSTNO = b.TSTNO

where 
b.[ SENATTD] like('%VEHICLE CG%')

;
select a.TSTNO,
b.[ VEHNO],

--this was the same as above but included the file number (CURNO) of the sensor
b.[ CURNO],
a.[ IMPANG],
b.[ SENTYPD],
b.[ AXIS],
b.[ AXIS],
b.[ SENATTD],
c.[ MAKE],
c.[ MAKED],
c.[ ModelD],
c.[ YEAR],
c.[ VEHTWT],
c.[ VEHCG]


FROM CrashData.dbo.tst as a 


LEFT JOIN CrashData.dbo.instr as b
on a.TSTNO = b.TSTNO

LEFT JOIN CrashData.dbo.veh as c
on a.TSTNO = c.TSTNO

where a.TSTNO IN
(
		Select distinct(z.TSTNO)
				FROM CrashData.dbo.tst as z
				
				LEFT JOIN CrashData.dbo.instr as y
				on z.TSTNO = y.TSTNO
				where 
				
				y.[ SENATTD] like('%VEHICLE CG%')

)
order by a.TSTNO
;
