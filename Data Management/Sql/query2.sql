SELECT ris2.Nome_radio,  ris2.Nome_canzone, generi2.'Album.Name', generi2.'Artist.Name.s.'
FROM(SELECT Nome_radio, Nome_canzone
FROM(SELECT radio_name as Nome_radio, nome_canzone_listaok as Nome_canzone
FROM radio1genere
LEFT JOIN  match1_1 ON radio1genere.radio_url=match1_1.'X.1' ) as risquery1
WHERE risquery1.Nome_canzone IS NOT NULL
GROUP BY Nome_radio) as ris2
LEFT JOIN generi2 ON ris2.Nome_canzone = generi2.'Track.Name' 
GROUP BY Nome_radio
