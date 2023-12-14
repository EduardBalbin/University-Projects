SELECT nome_canzone_listaok, radio_name, genre,country_name
FROM match1_1
LEFT JOIN radio1genere ON match1_1.'X.1' = radio1genere.radio_url
GROUP BY nome_canzone_listaok;