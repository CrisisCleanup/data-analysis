SELECT ww.id, wwwtsp.work_type_key,  ww.svi, ww.location, ww.created_at, ww.incident_id, ii.name, ii.incident_type
FROM worksite_worksites AS ww
LEFT JOIN worksite_worksites_work_types_statuses_phases AS wwwtsp
ON ww.id = wwwtsp.worksite_id
LEFT JOIN incident_incidents AS ii
ON ww.incident_id = ii.id
WHERE wwwtsp.invalidated_at IS NULL
AND ww.incident_id IN(
  WITH incident_query AS
  (SELECT ww.incident_id, COUNT(ww.incident_id) AS worksite_count
  FROM worksite_worksites AS ww
  GROUP BY ww.incident_id)
  SELECT incident_id
  FROM incident_query
  WHERE worksite_count >=1000
)
ORDER BY ww.id, ww.created_at;
