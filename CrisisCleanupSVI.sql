-- INCIDENT COUNT BY INCIDENT TYPE
select distinct
II.incident_type,
count (II.incident_type) as INCIDENT_COUNT,
min (WW.svi) as MINSVI,
avg (WW.svi)::numeric(10,5) as AVGSVI,
max (WW.svi) as MAXSVI
from incident_incidents II
inner join worksite_worksites WW on II.id = WW.incident_id
where II.start_at >= '2018-01-01'
group by II.incident_type
order by II.incident_type
;

-- INCIDENT COUNT BY ORGANIZATION
select distinct
OO.name as ORGANIZATION_NAME,
count (II.incident_type) as INCIDENT_COUNT,
min (WW.svi) as MINSVI,
avg (WW.svi)::numeric(10,5) as AVGSVI,
max (WW.svi) as MAXSVI
from incident_incidents II
inner join worksite_worksites WW on II.id = WW.incident_id
inner join organization_organizations_incidents OOI on II.id = OOI.incident_id
inner join organization_organizations OO on OOI.organization_id = OO.id
where II.start_at >= '2018-01-01'
group by ORGANIZATION_NAME
order by INCIDENT_COUNT desc
;

-- INCIDENT COUNT BY INCIDENT NAME
select distinct
II.incident_type,
II.name as INCIDENT_NAME,
to_char (II.start_at, 'YYYY-MM-DD') as INCIDENT_START,
count (II.incident_type) as INCIDENT_COUNT,
min (WW.svi) as MINSVI,
avg (WW.svi)::numeric(10,5) as AVGSVI,
max (WW.svi) as MAXSVI
from incident_incidents II
inner join worksite_worksites WW on II.id = WW.incident_id
where II.start_at >= '2018-01-01'
group by II.incident_type, II.name, II.start_at
order by INCIDENT_COUNT desc
;

select distinct * from worksite_worksites where svi_id = '25832';
select distinct * from location_svi;
select distinct * from incident_incidents II where II.start_at >= '2018-01-01';
select * from worksite_worksites WW where WW.svi_id = '25832';

-- INCIDENT BY SVI
select
LS.place_name,
to_char (II.start_at, 'YYYY-MM-DD') as INCIDENT_START,
count (WW.id) as WORKSITE_COUNT,
count (distinct II.case_label) as INCIDENT_COUNT,
WW.svi
from incident_incidents II
inner join worksite_worksites WW on II.id = WW.incident_id
inner join location_svi LS on WW.svi_id = LS.id
where II.start_at >= '2018-01-01'
group by LS.place_name, II.start_at, WW.svi
order by WORKSITE_COUNT desc, INCIDENT_COUNT desc
;