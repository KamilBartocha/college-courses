-- zadanie nr 3. Do bazy wyk�adowcy napisa� zapytanie wypisuj�ce plan zaj�c
-- czyli kiedy jaki prowadz�cy ma zajecia i gdzie.

select distinct nazwisko as [Nazwisko], nazwa_prz as [Przedmiot], sala as [Sala], 
				nazwa_d as [Dzie�], data.od, data.do from prowadzacy

left join	przedmioty_prowadzacy on prowadzacy.id=id_pr
inner join	przedmioty on przedmioty.id = id_prz
left join	sale_przedmioty on prowadzacy.id = id_prow
inner join  sale on sale.id = id_sali
left join	dni on sale_przedmioty.id_dn=dni.id
left join	data on data.id_pr=prowadzacy.id

order by [Dzie�]