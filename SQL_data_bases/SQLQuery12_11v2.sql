--1)Zliczyc ilo�� prowadz�cych na uczelni (ile przedmiotow na uczelni analogia)
	select count(*) as [liczba prowadz�cych] from prowadzacy

	select count(*) as [liczba przedmiot�w] from przedmioty

--2)czy jest wykladowca kt�ry nie prowadzi zajec (analogia przedmiot kt�ry nie jest wyk�adany)
	select nazwisko from prowadzacy
	where not exists
	(
		select * from data where
		id_pr=prowadzacy.id
	)
--3)kto i co prowadzi � imie nazwisko i rzedmioty kt�re s� prowadzone
	select  nazwisko, nazwa_prz as [przedmiot]  from prowadzacy
	left join przedmioty_prowadzacy on prowadzacy.id=id_pr
	inner join przedmioty on przedmioty.id=id_prz

-- 4) liczba prowadzonych godzin w�r�d 
	select nazwisko as [Imi� i nazwisko],
	(
	select sum(liczba_g) from data
	where id_pr=prowadzacy.id
	) as [Liczba godzin] from prowadzacy 
	order by nazwisko

--5) lizba godzin jakie dany prowadz�cy po�wi�ci� na dany przedmiot 
	select nazwisko, nazwa_prz, 
		(
		select sum(liczba_g)from data where prowadzacy.id=id_pr and przedmioty.id=id_prz
		) 
		as [��cznie godzin] from prowadzacy inner join przedmioty_prowadzacy on prowadzacy.id=id_pr 
	inner join przedmioty on przedmioty.id=id_prz order by nazwisko

--6) liczba godzin dla danego przedmiotu
	select  nazwa_prz,  (select sum(liczba_g) from data
	where id_prz=przedmioty.id) as [ile] from przedmioty

--7) liczba godzin proawadzcego w danym tygodniu
	select nazwisko, nazwa_d, (select sum(liczba_g) from data 
	where prowadzacy.id=id_pr and dni.id=id_dn) as [��cznie godzin] from prowadzacy cross join dni 
	order by nazwisko, nazwa_d


