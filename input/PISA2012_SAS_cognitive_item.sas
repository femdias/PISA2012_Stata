﻿libname  M_DEC03 "C:\XXX"; 
filename Cogn "C:\XXX\INT_COG12_DEC03.txt"; 
options nofmterr;

PROC Format;
value $CNTF
		"ALB" = "Albania"
		"ARG" = "Argentina"
		"AUS" = "Australia"
		"AUT" = "Austria"
		"BEL" = "Belgium"
		"BRA" = "Brazil"
		"BGR" = "Bulgaria"
		"CAN" = "Canada"
		"CHL" = "Chile"
		"QCN" = "Shanghai-China"
		"TAP" = "Chinese Taipei"
		"COL" = "Colombia"
		"CRI" = "Costa Rica"
		"HRV" = "Croatia"
		"CZE" = "Czech Republic"
		"DNK" = "Denmark"
		"EST" = "Estonia"
		"FIN" = "Finland"
		"FRA" = "France"
		"DEU" = "Germany"
		"GRC" = "Greece"
		"HKG" = "Hong Kong-China"
		"HUN" = "Hungary"
		"ISL" = "Iceland"
		"IDN" = "Indonesia"
		"IRL" = "Ireland"
		"ISR" = "Israel"
		"ITA" = "Italy"
		"JPN" = "Japan"
		"JOR" = "Jordan"
		"KAZ" = "Kazakhstan"
		"KOR" = "Korea"
		"LVA" = "Latvia"
		"LIE" = "Liechtenstein"
		"LTU" = "Lithuania"
		"LUX" = "Luxembourg"
		"MAC" = "Macao-China"
		"MYS" = "Malaysia"
		"MEX" = "Mexico"
		"MNE" = "Montenegro"
		"NLD" = "Netherlands"
		"NZL" = "New Zealand"
		"NOR" = "Norway"
		"QRS" = "Perm(Russian Federation)"
		"PER" = "Peru"
		"POL" = "Poland"
		"PRT" = "Portugal"
		"QAT" = "Qatar"
		"ROU" = "Romania"
		"RUS" = "Russian Federation"
		"SRB" = "Serbia"
		"SGP" = "Singapore"
		"SVK" = "Slovak Republic"
		"SVN" = "Slovenia"
		"ESP" = "Spain"
		"SWE" = "Sweden"
		"CHE" = "Switzerland"
		"THA" = "Thailand"
		"TUN" = "Tunisia"
		"TUR" = "Turkey"
		"GBR" = "United Kingdom"
		"ARE" = "United Arab Emirates"
		"USA" = "United States of America"
		"URY" = "Uruguay"
		"VNM" = "Viet Nam"
		"QUA" = "Florida (USA)"
		"QUB" = "Connecticut (USA)"
		"QUC" = "Massachusetts (USA)"
		"N/A" = "Not applicable"
;
value $STRATUMF
		"ALB0001" = "ALB - stratum 01 : North, Urban, Public"
		"ALB0002" = "ALB - stratum 02 : North, Urban, Private"
		"ALB0003" = "ALB - stratum 03 : North, Rural, Public"
		"ALB0004" = "ALB - stratum 04 : North, Rural, Private"
		"ALB0005" = "ALB - stratum 05 : Center, Urban, Public"
		"ALB0006" = "ALB - stratum 06 : Center, Urban, Private"
		"ALB0007" = "ALB - stratum 07 : Center, Rural, Public"
		"ALB0008" = "ALB - stratum 08 : Center, Rural, Private"
		"ALB0009" = "ALB - stratum 09 : South, Urban, Public"
		"ALB0010" = "ALB - stratum 10 : South, Urban, Private"
		"ALB0011" = "ALB - stratum 11 : South, Rural, Public"
		"ALB0012" = "ALB - stratum 12 : South, Rural, Private"
		"ARE0101" = "ARE - stratum 01 : ABU DHABI,MOE,PUBLIC"
		"ARE0102" = "ARE - stratum 02 : ABU DHABI,MOE,PRIVATE"
		"ARE0104" = "ARE - stratum 04 : ABU DHABI,US,PRIVATE"
		"ARE0106" = "ARE - stratum 06 : ABU DHABI,UK,PRIVATE"
		"ARE0108" = "ARE - stratum 08 : ABU DHABI,INDIAN,PRIVATE"
		"ARE0109" = "ARE - stratum 09 : ABU DHABI,OTHER,PUBLIC"
		"ARE0110" = "ARE - stratum 10 : ABU DHABI,OTHER,PRIVATE"
		"ARE0211" = "ARE - stratum 11 : DUBAI,MOE,PUBLIC"
		"ARE0212" = "ARE - stratum 12 : DUBAI,MOE,PRIVATE"
		"ARE0214" = "ARE - stratum 14 : DUBAI,US,PRIVATE"
		"ARE0216" = "ARE - stratum 16 : DUBAI,UK,PRIVATE"
		"ARE0218" = "ARE - stratum 18 : DUBAI,INDIAN,PRIVATE"
		"ARE0219" = "ARE - stratum 19 : DUBAI,OTHER,PUBLIC"
		"ARE0220" = "ARE - stratum 20 : DUBAI,OTHER,PRIVATE"
		"ARE0321" = "ARE - stratum 21 : SHARJAH,MOE,PUBLIC"
		"ARE0322" = "ARE - stratum 22 : SHARJAH,MOE,PRIVATE"
		"ARE0324" = "ARE - stratum 24 : SHARJAH,US,PRIVATE"
		"ARE0326" = "ARE - stratum 26 : SHARJAH,UK,PRIVATE"
		"ARE0328" = "ARE - stratum 28 : SHARJAH,INDIAN,PRIVATE"
		"ARE0330" = "ARE - stratum 30 : SHARJAH,OTHER,PRIVATE"
		"ARE0431" = "ARE - stratum 31 : AJMAN,MOE,PUBLIC"
		"ARE0432" = "ARE - stratum 32 : AJMAN,MOE,PRIVATE"
		"ARE0434" = "ARE - stratum 34 : AJMAN,US,PRIVATE"
		"ARE0438" = "ARE - stratum 38 : AJMAN,INDIAN,PRIVATE"
		"ARE0440" = "ARE - stratum 40 : AJMAN,OTHER,PRIVATE"
		"ARE0541" = "ARE - stratum 41 : UMM AL QUWAIN,MOE,PUBLIC"
		"ARE0542" = "ARE - stratum 42 : UMM AL QUWAIN,MOE,PRIVATE"
		"ARE0548" = "ARE - stratum 48 : UMM AL QUWAIN,INDIAN,PRIVATE"
		"ARE0651" = "ARE - stratum 51 : RAS AL KHAIMAH,MOE,PUBLIC"
		"ARE0652" = "ARE - stratum 52 : RAS AL KHAIMAH,MOE,PRIVATE"
		"ARE0654" = "ARE - stratum 54 : RAS AL KHAIMAH,US,PRIVATE"
		"ARE0658" = "ARE - stratum 58 : RAS AL KHAIMAH,INDIAN,PRIVATE"
		"ARE0659" = "ARE - stratum 59 : RAS AL KHAIMAH,OTHER,PUBLIC"
		"ARE0660" = "ARE - stratum 60 : RAS AL KHAIMAH,OTHER,PRIVATE"
		"ARE0761" = "ARE - stratum 61 : FUJAIRAH,MOE,PUBLIC"
		"ARE0762" = "ARE - stratum 62 : FUJAIRAH,MOE,PRIVATE"
		"ARE0766" = "ARE - stratum 66 : FUJAIRAH,UK,PRIVATE"
		"ARE0768" = "ARE - stratum 68 : FUJAIRAH,INDIAN,PRIVATE"
		"ARE0769" = "ARE - stratum 69 : FUJAIRAH,OTHER,PUBLIC"
		"ARE0770" = "ARE - stratum 70 : FUJAIRAH,OTHER,PRIVATE"
		"ARG0001" = "ARG - stratum 01 : Centro"
		"ARG0102" = "ARG - stratum 02 : CABA"
		"ARG0003" = "ARG - stratum 03 : NEA"
		"ARG0004" = "ARG - stratum 04 : NOA"
		"ARG0005" = "ARG - stratum 05 : Cuyo"
		"ARG0006" = "ARG - stratum 06 : Patagonia"
		"AUS0101" = "AUS - stratum 01 : ACT_Catholic"
		"AUS0102" = "AUS - stratum 02 : ACT_Government"
		"AUS0103" = "AUS - stratum 03 : ACT_Independent"
		"AUS0204" = "AUS - stratum 04 : NSW_Catholic"
		"AUS0205" = "AUS - stratum 05 : NSW_Government"
		"AUS0206" = "AUS - stratum 06 : NSW_Independent"
		"AUS0307" = "AUS - stratum 07 : VIC_Catholic"
		"AUS0308" = "AUS - stratum 08 : VIC_Government"
		"AUS0309" = "AUS - stratum 09 : VIC_Independent"
		"AUS0410" = "AUS - stratum 10 : QLD_Catholic"
		"AUS0411" = "AUS - stratum 11 : QLD_Government"
		"AUS0412" = "AUS - stratum 12 : QLD_Independent"
		"AUS0513" = "AUS - stratum 13 : SA_Catholic"
		"AUS0514" = "AUS - stratum 14 : SA_Government"
		"AUS0515" = "AUS - stratum 15 : SA_Independent"
		"AUS0616" = "AUS - stratum 16 : WA_Catholic"
		"AUS0617" = "AUS - stratum 17 : WA_Government"
		"AUS0618" = "AUS - stratum 18 : WA_Independent"
		"AUS0719" = "AUS - stratum 19 : TAS_Catholic"
		"AUS0720" = "AUS - stratum 20 : TAS_Government"
		"AUS0721" = "AUS - stratum 21 : TAS_Independent"
		"AUS0822" = "AUS - stratum 22 : NT_Catholic"
		"AUS0823" = "AUS - stratum 23 : NT_Government"
		"AUS0824" = "AUS - stratum 24 : NT_Independent"
		"AUT0001" = "AUT - stratum 01 : TYPE 1  Pflichtschulen"
		"AUT0002" = "AUT - stratum 02 : TYPE 2  Polytechnische Schulen"
		"AUT0003" = "AUT - stratum 03 : TYPE 3  AHS-Langform"
		"AUT0004" = "AUT - stratum 04 : TYPE 4  AHS-Kurzform"
		"AUT0005" = "AUT - stratum 05 : TYPE 5  Schulen mit Statut - Waldorfschulen"
		"AUT0006" = "AUT - stratum 06 : TYPE 6  Schulen mit Statut - Sonstige"
		"AUT0007" = "AUT - stratum 07 : TYPE 7  Berufsschulen kaufmännisch  and gewerblich"
		"AUT0008" = "AUT - stratum 08 : TYPE 8  Berufsschulen landwirtschaftlich"
		"AUT0009" = "AUT - stratum 09 : TYPE 9  BMS gewerblich"
		"AUT0010" = "AUT - stratum 10 : TYPE 10  BMS kaufmännisch"
		"AUT0011" = "AUT - stratum 11 : TYPE 11  BMS wirtschaftlich"
		"AUT0012" = "AUT - stratum 12 : TYPE 12  BMS landwirtschaftlich"
		"AUT0013" = "AUT - stratum 13 : TYPE 13  BHS gewerblich"
		"AUT0014" = "AUT - stratum 14 : TYPE 14  BHS kaufmännisch"
		"AUT0015" = "AUT - stratum 15 : TYPE 15  BHS wirtschaftlich"
		"AUT0016" = "AUT - stratum 16 : TYPE 16  BHS landwirtschaftlich"
		"AUT0017" = "AUT - stratum 17 : TYPE 17  BAKIP"
		"BEL0101" = "BEL - stratum 01 : Flanders/Gen Ed only/private subsidized/ISCED2"
		"BEL0102" = "BEL - stratum 02 : Flanders/Gen Ed only/private subsidized/ISCED3"
		"BEL0103" = "BEL - stratum 03 : Flanders/Gen Ed only/private subsidized/Mixed ISCED"
		"BEL0104" = "BEL - stratum 04 : Flanders/Gen Ed only/network comm. Schls/ISCED2"
		"BEL0105" = "BEL - stratum 05 : Flanders/Gen Ed only/network comm. Schls/ISCED3"
		"BEL0106" = "BEL - stratum 06 : Flanders/Gen Ed only/network comm. Schls/Mixed ISCED"
		"BEL0107" = "BEL - stratum 07 : Flanders/Gen Ed and other type/private subsidized/ISCED2"
		"BEL0108" = "BEL - stratum 08 : Flanders/Gen Ed and other type/private subsidized /ISCED3"
		"BEL0109" = "BEL - stratum 09 : Flanders/Gen Ed and other type/private subsidized /Mixed ISCED"
		"BEL0110" = "BEL - stratum 10 : Flanders/Gen Ed and other type/network comm. Schls/ISCED2"
		"BEL0111" = "BEL - stratum 11 : Flanders/Gen Ed and other type/network comm. Schls/ISCED3"
		"BEL0112" = "BEL - stratum 12 : Flanders/Gen Ed and other type/network comm. Schls/Mixed ISCED"
		"BEL0113" = "BEL - stratum 13 : Flanders/Tech or Voc/private subsidized/ISCED2"
		"BEL0114" = "BEL - stratum 14 : Flanders/Tech or Voc/private subsidized/ISCED3"
		"BEL0115" = "BEL - stratum 15 : Flanders/Tech or Voc/private subsidized/Mixed ISCED"
		"BEL0116" = "BEL - stratum 16 : Flanders/Tech or Voc/network comm. Schls/ISCED2"
		"BEL0117" = "BEL - stratum 17 : Flanders/Tech or Voc/network comm. Schls/ISCED3"
		"BEL0118" = "BEL - stratum 18 : Flanders/Tech or Voc/network comm. Schls/Mixed ISCED"
		"BEL0119" = "BEL - stratum 19 : Flanders/Special Ed/ private subsidized"
		"BEL0120" = "BEL - stratum 20 : Flanders/Special Ed/network comm. schls"
		"BEL0121" = "BEL - stratum 21 : Flanders/Part-time Vocational Ed/community"
		"BEL0222" = "BEL - stratum 22 : French com/Regular/ISCED2"
		"BEL0223" = "BEL - stratum 23 : French com/Regular/ Mixed ISCED/Academic only"
		"BEL0224" = "BEL - stratum 24 : French com/Regular/ Mixed ISCED/Mixed"
		"BEL0225" = "BEL - stratum 25 : French com/Regular/ Mixed ISCED/Vocational only"
		"BEL0226" = "BEL - stratum 26 : French com/Special Ed"
		"BEL0227" = "BEL - stratum 27 : French com/Part-time Vocational Ed"
		"BEL0328" = "BEL - stratum 28 : German com/Regular"
		"BEL0329" = "BEL - stratum 29 : German com/Special Ed"
		"BGR0001" = "BGR - stratum 01 : Region 01"
		"BGR0002" = "BGR - stratum 02 : Region 02"
		"BGR0003" = "BGR - stratum 03 : Region 03"
		"BGR0004" = "BGR - stratum 04 : Region 04"
		"BGR0005" = "BGR - stratum 05 : Region 05"
		"BGR0006" = "BGR - stratum 06 : Region 06"
		"BGR0007" = "BGR - stratum 07 : Region 07"
		"BGR0008" = "BGR - stratum 08 : Region 08"
		"BGR0009" = "BGR - stratum 09 : Region 09"
		"BGR0010" = "BGR - stratum 10 : Region 10"
		"BGR0011" = "BGR - stratum 11 : Region 11"
		"BRA0101" = "BRA - stratum 01 : ACRE - FEDERAL/ESTADUAL "
		"BRA0102" = "BRA - stratum 02 : ACRE - MUNICIPAL "
		"BRA0103" = "BRA - stratum 03 : ACRE - PRIVADA "
		"BRA0204" = "BRA - stratum 04 : ALAGOAS - FEDERAL/ESTADUAL "
		"BRA0205" = "BRA - stratum 05 : ALAGOAS - MUNICIPAL "
		"BRA0206" = "BRA - stratum 06 : ALAGOAS - PRIVADA "
		"BRA0307" = "BRA - stratum 07 : AMAPÁ - FEDERAL/ESTADUAL "
		"BRA0308" = "BRA - stratum 08 : AMAPÁ - MUNICIPAL "
		"BRA0309" = "BRA - stratum 09 : AMAPÁ - PRIVADA "
		"BRA0410" = "BRA - stratum 10 : AMAZONAS - FEDERAL/ESTADUAL "
		"BRA0411" = "BRA - stratum 11 : AMAZONAS - MUNICIPAL"
		"BRA0412" = "BRA - stratum 12 : AMAZONAS - PRIVADA "
		"BRA0513" = "BRA - stratum 13 : BAHIA - FEDERAL/ESTADUAL "
		"BRA0514" = "BRA - stratum 14 : BAHIA - MUNICIPAL "
		"BRA0515" = "BRA - stratum 15 : BAHIA - PRIVADA "
		"BRA0616" = "BRA - stratum 16 : CEARÁ - FEDERAL/ESTADUAL"
		"BRA0617" = "BRA - stratum 17 : CEARÁ - MUNICIPAL "
		"BRA0618" = "BRA - stratum 18 : CEARÁ - PRIVADA "
		"BRA0719" = "BRA - stratum 19 : DISTRITO FEDERAL - FEDERAL/ESTADUAL"
		"BRA0720" = "BRA - stratum 20 : DISTRITO FEDERAL - PRIVADA "
		"BRA0821" = "BRA - stratum 21 : ESPÍRITO SANTO - FEDERAL/ESTADUAL"
		"BRA0822" = "BRA - stratum 22 : ESPÍRITO SANTO - MUNICIPAL "
		"BRA0823" = "BRA - stratum 23 : ESPÍRITO SANTO - PRIVADA "
		"BRA0924" = "BRA - stratum 24 : GOIÁS - FEDERAL/ESTADUAL "
		"BRA0925" = "BRA - stratum 25 : GOIÁS - MUNICIPAL"
		"BRA0926" = "BRA - stratum 26 : GOIÁS - PRIVADA"
		"BRA1027" = "BRA - stratum 27 : MARANHÃO - FEDERAL/ESTADUAL"
		"BRA1028" = "BRA - stratum 28 : MARANHÃO - MUNICIPAL"
		"BRA1029" = "BRA - stratum 29 : MARANHÃO - PRIVADA"
		"BRA1130" = "BRA - stratum 30 : MATO GROSSO - FEDERAL/ESTADUAL "
		"BRA1131" = "BRA - stratum 31 : MATO GROSSO - MUNICIPAL "
		"BRA1132" = "BRA - stratum 32 : MATO GROSSO - PRIVADA "
		"BRA1233" = "BRA - stratum 33 : MATO GROSSO DO SUL - FEDERAL/ESTADUAL "
		"BRA1234" = "BRA - stratum 34 : MATO GROSSO DO SUL - MUNICIPAL "
		"BRA1235" = "BRA - stratum 35 : MATO GROSSO DO SUL - PRIVADA "
		"BRA1336" = "BRA - stratum 36 : MINAS GERAIS - FEDERAL/ESTADUAL "
		"BRA1337" = "BRA - stratum 37 : MINAS GERAIS - MUNICIPAL "
		"BRA1338" = "BRA - stratum 38 : MINAS GERAIS - PRIVADA"
		"BRA1439" = "BRA - stratum 39 : PARÁ - FEDERAL/ESTADUAL "
		"BRA1440" = "BRA - stratum 40 : PARÁ - MUNICIPAL "
		"BRA1441" = "BRA - stratum 41 : PARÁ - PRIVADA"
		"BRA1542" = "BRA - stratum 42 : PARAÍBA - FEDERAL/ESTADUAL "
		"BRA1543" = "BRA - stratum 43 : PARAÍBA - MUNICIPAL"
		"BRA1544" = "BRA - stratum 44 : PARAÍBA - PRIVADA "
		"BRA1645" = "BRA - stratum 45 : PARANÁ - FEDERAL/ESTADUAL "
		"BRA1646" = "BRA - stratum 46 : PARANÁ - MUNICIPAL "
		"BRA1647" = "BRA - stratum 47 : PARANÁ - PRIVADA "
		"BRA1748" = "BRA - stratum 48 : PERNAMBUCO - FEDERAL/ESTADUAL "
		"BRA1749" = "BRA - stratum 49 : PERNAMBUCO - MUNICIPAL "
		"BRA1750" = "BRA - stratum 50 : PERNAMBUCO - PRIVADA "
		"BRA1851" = "BRA - stratum 51 : PIAUÍ - FEDERAL/ESTADUAL "
		"BRA1852" = "BRA - stratum 52 : PIAUÍ - MUNICIPAL"
		"BRA1853" = "BRA - stratum 53 : PIAUÍ - PRIVADA"
		"BRA1954" = "BRA - stratum 54 : RIO DE JANEIRO - FEDERAL/ESTADUAL"
		"BRA1955" = "BRA - stratum 55 : RIO DE JANEIRO - MUNICIPAL"
		"BRA1956" = "BRA - stratum 56 : RIO DE JANEIRO - PRIVADA"
		"BRA2057" = "BRA - stratum 57 : RIO GRANDE DO NORTE - FEDERAL/ESTADUAL"
		"BRA2058" = "BRA - stratum 58 : RIO GRANDE DO NORTE - MUNICIPAL"
		"BRA2059" = "BRA - stratum 59 : RIO GRANDE DO NORTE - PRIVADA"
		"BRA2160" = "BRA - stratum 60 : RIO GRANDE DO SUL - FEDERAL/ESTADUAL"
		"BRA2161" = "BRA - stratum 61 : RIO GRANDE DO SUL - MUNICIPAL"
		"BRA2162" = "BRA - stratum 62 : RIO GRANDE DO SUL - PRIVADA"
		"BRA2263" = "BRA - stratum 63 : RONDÔNIA - FEDERAL/ESTADUAL"
		"BRA2264" = "BRA - stratum 64 : RONDÔNIA - MUNICIPAL"
		"BRA2265" = "BRA - stratum 65 : RONDÔNIA - PRIVADA"
		"BRA2366" = "BRA - stratum 66 : RORAIMA - FEDERAL/ESTADUAL"
		"BRA2367" = "BRA - stratum 67 : RORAIMA - MUNICIPAL"
		"BRA2368" = "BRA - stratum 68 : RORAIMA - PRIVADA"
		"BRA2469" = "BRA - stratum 69 : SANTA CATARINA - FEDERAL/ESTADUAL"
		"BRA2470" = "BRA - stratum 70 : SANTA CATARINA - MUNICIPAL"
		"BRA2471" = "BRA - stratum 71 : SANTA CATARINA - PRIVADA"
		"BRA2572" = "BRA - stratum 72 : SÃO PAULO - FEDERAL/ESTADUAL"
		"BRA2573" = "BRA - stratum 73 : SÃO PAULO - MUNICIPAL"
		"BRA2574" = "BRA - stratum 74 : SÃO PAULO - PRIVADA"
		"BRA2675" = "BRA - stratum 75 : SERGIPE - FEDERAL/ESTADUAL"
		"BRA2676" = "BRA - stratum 76 : SERGIPE - MUNICIPAL"
		"BRA2677" = "BRA - stratum 77 : SERGIPE - PRIVADA "
		"BRA2778" = "BRA - stratum 78 : TOCANTINS - FEDERAL/ESTADUAL"
		"BRA2779" = "BRA - stratum 79 : TOCANTINS - MUNICIPAL"
		"BRA2780" = "BRA - stratum 80 : TOCANTINS - PRIVADA"
		"CAN0101" = "CAN - stratum 01 : prov 10 Fr.& Eng. 0 to 17"
		"CAN0102" = "CAN - stratum 02 : prov 10 Fr.& Eng. 18 to 34"
		"CAN0103" = "CAN - stratum 03 : prov 10 Fr.& Eng. 35 to 196"
		"CAN0104" = "CAN - stratum 04 : prov 10 Fr.& Eng. 197 to ..."
		"CAN0210" = "CAN - stratum 10 : prov 11 Fr.& Eng. 0 to ..."
		"CAN0320" = "CAN - stratum 20 : prov 12 Eng. 0 to 17"
		"CAN0321" = "CAN - stratum 21 : prov 12 Eng. 18 to 34"
		"CAN0322" = "CAN - stratum 22 : prov 12 Eng. 35 to ..."
		"CAN0325" = "CAN - stratum 25 : prov 12 Fr. 0 to 17"
		"CAN0326" = "CAN - stratum 26 : prov 12 Fr. 18 to ..."
		"CAN0430" = "CAN - stratum 30 : prov 13 Eng. 0 to 34"
		"CAN0431" = "CAN - stratum 31 : prov 13 Eng. 35 to ..."
		"CAN0435" = "CAN - stratum 35 : prov 13 Fr. 0 to 34"
		"CAN0436" = "CAN - stratum 36 : prov 13 Fr. 35 to 117"
		"CAN0437" = "CAN - stratum 37 : prov 13 Fr. 118 to ..."
		"CAN0540" = "CAN - stratum 40 : prov 24 Eng. 0 to 17"
		"CAN0541" = "CAN - stratum 41 : prov 24 Eng. 18 to 34"
		"CAN0542" = "CAN - stratum 42 : prov 24 Eng. 35 to 305"
		"CAN0543" = "CAN - stratum 43 : prov 24 Eng. 306 to ..."
		"CAN0545" = "CAN - stratum 45 : prov 24 Fr.  0 to 17"
		"CAN0546" = "CAN - stratum 46 : prov 24 Fr. 18 to 34"
		"CAN0547" = "CAN - stratum 47 : prov 24 Fr.  35 to ..."
		"CAN0650" = "CAN - stratum 50 : prov 35 Eng.  0 to 2"
		"CAN0651" = "CAN - stratum 51 : prov 35 Eng. 3 to 34"
		"CAN0652" = "CAN - stratum 52 : prov 35 Eng.  35 to ..."
		"CAN0655" = "CAN - stratum 55 : prov 35 Fr.  0 to 2"
		"CAN0656" = "CAN - stratum 56 : prov 35 Fr.  3 to 34"
		"CAN0657" = "CAN - stratum 57 : prov 35 Fr.  35 to ..."
		"CAN0760" = "CAN - stratum 60 : prov 46 Eng.  0 to 17"
		"CAN0761" = "CAN - stratum 61 : prov 46 Eng.  18 to 34"
		"CAN0762" = "CAN - stratum 62 : prov 46 Eng.  35 to ..."
		"CAN0765" = "CAN - stratum 65 : prov 46 Fr.  0 to 17"
		"CAN0766" = "CAN - stratum 66 : prov 46 Fr.  18 to ..."
		"CAN0870" = "CAN - stratum 70 : prov 47 Eng.  0 to 17"
		"CAN0871" = "CAN - stratum 71 : prov 47 Eng. 18 to 34"
		"CAN0872" = "CAN - stratum 72 : prov 47 Eng. 35 to ..."
		"CAN0875" = "CAN - stratum 75 : prov 47 Fr.  0 to 8"
		"CAN0876" = "CAN - stratum 76 : prov 47 Fr.  9 to ..."
		"CAN0980" = "CAN - stratum 80 : prov 48 Eng.  0 to 17"
		"CAN0981" = "CAN - stratum 81 : prov 48 Eng.  18 to 34"
		"CAN0982" = "CAN - stratum 82 : prov 48 Eng.  35 to ..."
		"CAN0985" = "CAN - stratum 85 : prov 48 Fr.  0 to 8"
		"CAN0986" = "CAN - stratum 86 : prov 48 Fr.  9 to ..."
		"CAN1090" = "CAN - stratum 90 : prov 59 Eng.  0 to 17"
		"CAN1091" = "CAN - stratum 91 : prov 59 Eng.  18 to 34"
		"CAN1092" = "CAN - stratum 92 : prov 59 Eng.  35 to ..."
		"CAN1095" = "CAN - stratum 95 : prov 59 Fr.  0 to 8"
		"CAN1096" = "CAN - stratum 96 : prov 59 Fr.  9 to ..."
		"CHE0101" = "CHE - stratum 01 : 1-AG.gr9.pub"
		"CHE1602" = "CHE - stratum 02 : 2-SO.gr9.pub"
		"CHE0303" = "CHE - stratum 03 : 3-BE(d).gr9.pub"
		"CHE0404" = "CHE - stratum 04 : 4-SG.gr9.pub"
		"CHE0706" = "CHE - stratum 06 : 6-VS(d).gr9.pub"
		"CHE0008" = "CHE - stratum 08 : 8-D-Rest.gr9.pub."
		"CHE0009" = "CHE - stratum 09 : 9-D-CH.other.v1.pub"
		"CHE0010" = "CHE - stratum 10 : 10-D-CH.other.v2.pub"
		"CHE0011" = "CHE - stratum 11 : 11-D-CH.other.v3.pub."
		"CHE0012" = "CHE - stratum 12 : 12-D-CH.other.b.pub."
		"CHE0013" = "CHE - stratum 13 : 13-D-CH.gr9.priv."
		"CHE0814" = "CHE - stratum 14 : 14-BE(f).gr9.pub."
		"CHE0915" = "CHE - stratum 15 : 15-FR(f).gr9.pub."
		"CHE1016" = "CHE - stratum 16 : 16-GE.gr9.pub."
		"CHE1117" = "CHE - stratum 17 : 17-JU.gr9.pub."
		"CHE1218" = "CHE - stratum 18 : 18-NE.gr9.pub."
		"CHE1319" = "CHE - stratum 19 : 19-VD.gr9.pub."
		"CHE1420" = "CHE - stratum 20 : 20-VS(f).gr9.pub."
		"CHE0021" = "CHE - stratum 21 : 21-F-CH.other.v1.pub."
		"CHE0022" = "CHE - stratum 22 : 22-F-CH.other.v2.pub."
		"CHE0023" = "CHE - stratum 23 : 23-F-CH.other.v3.pub."
		"CHE0024" = "CHE - stratum 24 : 24-F-CH.other.b.pub."
		"CHE0025" = "CHE - stratum 25 : 25-F-CH.gr9.priv."
		"CHE1526" = "CHE - stratum 26 : 26-I-CH.gr9.pub."
		"CHE0027" = "CHE - stratum 27 : 27-I-CH.other.v.pub."
		"CHE0028" = "CHE - stratum 28 : 28-I-CH.other.b.pub."
		"CHE0029" = "CHE - stratum 29 : 29-I-CH.gr9.priv."
		"CHE0661" = "CHE - stratum 61 : 61-ZH.gr9.pub.gym"
		"CHE0662" = "CHE - stratum 62 : 62-ZH.gr9.pub.rest"
		"CHL0101" = "CHL - stratum 01 : Municipal / Primary / Only Primary"
		"CHL0102" = "CHL - stratum 02 : Municipal / Primary andSecondary / HS"
		"CHL0103" = "CHL - stratum 03 : Municipal / Primary andSecondary / TP"
		"CHL0104" = "CHL - stratum 04 : Municipal / Primary andSecondary / HS-TP"
		"CHL0105" = "CHL - stratum 05 : Municipal / Secondary / HS"
		"CHL0106" = "CHL - stratum 06 : Municipal / Secondary / TP"
		"CHL0107" = "CHL - stratum 07 : Municipal / Secondary / HS-TP"
		"CHL0208" = "CHL - stratum 08 : Subsidized / Primary / Only Primary"
		"CHL0209" = "CHL - stratum 09 : Subsidized / Primary andSecondary / HS"
		"CHL0210" = "CHL - stratum 10 : Subsidized / Primary andSecondary / TP"
		"CHL0211" = "CHL - stratum 11 : Subsidized / Primary andSecondary / HS-TP"
		"CHL0212" = "CHL - stratum 12 : Subsidized / Secondary / HS"
		"CHL0213" = "CHL - stratum 13 : Subsidized / Secondary / TP"
		"CHL0214" = "CHL - stratum 14 : Subsidized / Secondary / HS-TP"
		"CHL0315" = "CHL - stratum 15 : Private / Primary / Only Primary"
		"CHL0316" = "CHL - stratum 16 : Private / Primary andSecondary / HS"
		"CHL0317" = "CHL - stratum 17 : Private / Secondary / HS "
		"CHL0318" = "CHL - stratum 18 : Private / Secondary / TP"
		"COL0101" = "COL - stratum 01 : BOGOTÁ"
		"COL0502" = "COL - stratum 02 : REST OF MANIZALES"
		"COL0303" = "COL - stratum 03 : MANIZALES-ESCUELA ACTIVA"
		"COL0404" = "COL - stratum 04 : MEDELLÍN "
		"COL0205" = "COL - stratum 05 : CALI"
		"COL0606" = "COL - stratum 06 : REST OF THE COUNTRY"
		"CRI0001" = "CRI - stratum 01 : Public/ academic/ urban"
		"CRI0002" = "CRI - stratum 02 : Public/ academic/ rural "
		"CRI0003" = "CRI - stratum 03 : Public/ technical/ urban"
		"CRI0004" = "CRI - stratum 04 : Public/ technical/ rural"
		"CRI0005" = "CRI - stratum 05 : private"
		"CZE0101" = "CZE - stratum 01 : Basic/Region1/Large"
		"CZE0102" = "CZE - stratum 02 : Basic/Region1/MSS"
		"CZE0103" = "CZE - stratum 03 : Basic/Region1/VSS"
		"CZE0204" = "CZE - stratum 04 : Basic/Region2/Large"
		"CZE0205" = "CZE - stratum 05 : Basic/Region2/MSS"
		"CZE0206" = "CZE - stratum 06 : Basic/Region2/VSS"
		"CZE0307" = "CZE - stratum 07 : Basic/Region3/Large"
		"CZE0308" = "CZE - stratum 08 : Basic/Region3/MSS"
		"CZE0309" = "CZE - stratum 09 : Basic/Region3/VSS"
		"CZE0411" = "CZE - stratum 11 : Basic/Region4/MSS"
		"CZE0412" = "CZE - stratum 12 : Basic/Region4/VSS"
		"CZE0513" = "CZE - stratum 13 : Basic/Region5/Large"
		"CZE0514" = "CZE - stratum 14 : Basic/Region5/MSS"
		"CZE0515" = "CZE - stratum 15 : Basic/Region5/VSS"
		"CZE0616" = "CZE - stratum 16 : Basic/Region6/Large"
		"CZE0617" = "CZE - stratum 17 : Basic/Region6/MSS"
		"CZE0618" = "CZE - stratum 18 : Basic/Region6/VSS"
		"CZE0719" = "CZE - stratum 19 : Basic/Region7/Large"
		"CZE0720" = "CZE - stratum 20 : Basic/Region7/MSS"
		"CZE0721" = "CZE - stratum 21 : Basic/Region7/VSS"
		"CZE0822" = "CZE - stratum 22 : Basic/Region8/Large"
		"CZE0823" = "CZE - stratum 23 : Basic/Region8/MSS"
		"CZE0824" = "CZE - stratum 24 : Basic/Region8/VSS"
		"CZE0925" = "CZE - stratum 25 : Basic/Region9/Large"
		"CZE0926" = "CZE - stratum 26 : Basic/Region9/MSS"
		"CZE0927" = "CZE - stratum 27 : Basic/Region9/VSS"
		"CZE1028" = "CZE - stratum 28 : Basic/Region10/Large"
		"CZE1029" = "CZE - stratum 29 : Basic/Region10/MSS"
		"CZE1030" = "CZE - stratum 30 : Basic/Region10/VSS"
		"CZE1131" = "CZE - stratum 31 : Basic/Region11/Large"
		"CZE1132" = "CZE - stratum 32 : Basic/Region11/MSS"
		"CZE1133" = "CZE - stratum 33 : Basic/Region11/VSS"
		"CZE1234" = "CZE - stratum 34 : Basic/Region12/Large"
		"CZE1235" = "CZE - stratum 35 : Basic/Region12/MSS"
		"CZE1236" = "CZE - stratum 36 : Basic/Region12/VSS"
		"CZE1337" = "CZE - stratum 37 : Basic/Region13/Large"
		"CZE1338" = "CZE - stratum 38 : Basic/Region13/MSS"
		"CZE1339" = "CZE - stratum 39 : Basic/Region13/VSS"
		"CZE1440" = "CZE - stratum 40 : Basic/Region14/Large"
		"CZE1441" = "CZE - stratum 41 : Basic/Region14/MSS"
		"CZE1442" = "CZE - stratum 42 : Basic/Region14/VSS"
		"CZE1543" = "CZE - stratum 43 : 6-8yrGym/Region1/Large"
		"CZE1544" = "CZE - stratum 44 : 6-8yrGym/Region1/MSS andVSS"
		"CZE1645" = "CZE - stratum 45 : 6-8yrGym/Region2/Large"
		"CZE1646" = "CZE - stratum 46 : 6-8yrGym/Region2/MSS andVSS"
		"CZE1747" = "CZE - stratum 47 : 6-8yrGym/Region3/Large"
		"CZE1748" = "CZE - stratum 48 : 6-8yrGym/Region3/MSS andVSS"
		"CZE1849" = "CZE - stratum 49 : 6-8yrGym/Region4/Large"
		"CZE1850" = "CZE - stratum 50 : 6-8yrGym/Region4/MSS andVSS"
		"CZE1951" = "CZE - stratum 51 : 6-8yrGym/Region5/Large"
		"CZE1952" = "CZE - stratum 52 : 6-8yrGym/Region5/MSS andVSS"
		"CZE2053" = "CZE - stratum 53 : 6-8yrGym/Region6/Large"
		"CZE2054" = "CZE - stratum 54 : 6-8yrGym/Region6/MSS andVSS"
		"CZE2155" = "CZE - stratum 55 : 6-8yrGym/Region7/Large"
		"CZE2156" = "CZE - stratum 56 : 6-8yrGym/Region7/MSS andVSS"
		"CZE2257" = "CZE - stratum 57 : 6-8yrGym/Region8/Large"
		"CZE2258" = "CZE - stratum 58 : 6-8yrGym/Region8/MSS andVSS"
		"CZE2359" = "CZE - stratum 59 : 6-8yrGym/Region9/Large"
		"CZE2360" = "CZE - stratum 60 : 6-8yrGym/Region9/MSS andVSS"
		"CZE2461" = "CZE - stratum 61 : 6-8yrGym/Region10/Large"
		"CZE2462" = "CZE - stratum 62 : 6-8yrGym/Region10/MSS andVSS"
		"CZE2563" = "CZE - stratum 63 : 6-8yrGym/Region11/Large"
		"CZE2564" = "CZE - stratum 64 : 6-8yrGym/Region11/MSS andVSS"
		"CZE2665" = "CZE - stratum 65 : 6-8yrGym/Region12/Large"
		"CZE2666" = "CZE - stratum 66 : 6-8yrGym/Region12/MSS andVSS"
		"CZE2767" = "CZE - stratum 67 : 6-8yrGym/Region13/Large"
		"CZE2768" = "CZE - stratum 68 : 6-8yrGym/Region13/MSS andVSS"
		"CZE2869" = "CZE - stratum 69 : 6-8yrGym/Region14/Large"
		"CZE2870" = "CZE - stratum 70 : 6-8yrGym/Region14/MSS andVSS"
		"CZE2971" = "CZE - stratum 71 : 4yr Gym/Large"
		"CZE2972" = "CZE - stratum 72 : 4yr Gym/MSS"
		"CZE2973" = "CZE - stratum 73 : 4yr Gym/VSS"
		"CZE3074" = "CZE - stratum 74 : Voc-Conservatory with maturate/Large"
		"CZE3075" = "CZE - stratum 75 : Voc-Conservatory with maturate/MSS"
		"CZE3076" = "CZE - stratum 76 : Voc-Conservatory with maturate/VSS"
		"CZE3177" = "CZE - stratum 77 : Voc-Conservatory without maturate/Large"
		"CZE3178" = "CZE - stratum 78 : Voc-Conservatory without maturate/MSS"
		"CZE3179" = "CZE - stratum 79 : Voc-Conservatory without maturate/VSS"
		"CZE3280" = "CZE - stratum 80 : Special/practical/Large"
		"CZE3281" = "CZE - stratum 81 : Special/practical/MSS"
		"CZE3282" = "CZE - stratum 82 : Special/practical/VSS"
		"DNK0001" = "DNK - stratum 01 : 1=high minority "
		"DNK0002" = "DNK - stratum 02 : 2=mid minority"
		"DNK0003" = "DNK - stratum 03 : 3= low minority"
		"DNK0004" = "DNK - stratum 04 : 4= no minority"
		"DNK0005" = "DNK - stratum 05 : 5=Faroe Island"
		"ESP0101" = "ESP - stratum 01 : Andalusia, public, NA"
		"ESP0102" = "ESP - stratum 02 : Andalusia, private, NA"
		"ESP0203" = "ESP - stratum 03 : Aragon, public, NA"
		"ESP0204" = "ESP - stratum 04 : Aragon, private, NA"
		"ESP0305" = "ESP - stratum 05 : Asturias, public, NA"
		"ESP0306" = "ESP - stratum 06 : Asturias, private, NA"
		"ESP0407" = "ESP - stratum 07 : BalearicIslands, public, NA"
		"ESP0408" = "ESP - stratum 08 : BalearicIslands, private, NA"
		"ESP0509" = "ESP - stratum 09 : CanaryIslands, public, NA"
		"ESP0510" = "ESP - stratum 10 : CanaryIslands, private, NA"
		"ESP0611" = "ESP - stratum 11 : Cantabria, public, NA"
		"ESP0612" = "ESP - stratum 12 : Cantabria, private, NA"
		"ESP0713" = "ESP - stratum 13 : CastileandLeon, public, NA"
		"ESP0714" = "ESP - stratum 14 : CastileandLeon, private, NA"
		"ESP0815" = "ESP - stratum 15 : Castile-LaMancha,public,NA"
		"ESP0816" = "ESP - stratum 16 : Castile-LaMancha,private,NA"
		"ESP0917" = "ESP - stratum 17 : Catalonia, public, NA"
		"ESP0918" = "ESP - stratum 18 : Catalonia, private, NA"
		"ESP1019" = "ESP - stratum 19 : Extremadura, public, NA"
		"ESP1020" = "ESP - stratum 20 : Extremadura, private, NA"
		"ESP1121" = "ESP - stratum 21 : Galicia, public, NA"
		"ESP1122" = "ESP - stratum 22 : Galicia, private, NA"
		"ESP1223" = "ESP - stratum 23 : LaRioja, public, NA"
		"ESP1224" = "ESP - stratum 24 : LaRioja, private, NA"
		"ESP1325" = "ESP - stratum 25 : Madrid, public, NA"
		"ESP1326" = "ESP - stratum 26 : Madrid, private, NA"
		"ESP1427" = "ESP - stratum 27 : Murcia, public, NA"
		"ESP1428" = "ESP - stratum 28 : Murcia, private, NA"
		"ESP1529" = "ESP - stratum 29 : Navarra, public, NA"
		"ESP1530" = "ESP - stratum 30 : Navarra, private, NA"
		"ESP1631" = "ESP - stratum 31 : BasqueCountry, public, A"
		"ESP1632" = "ESP - stratum 32 : BasqueCountry, public, B"
		"ESP1633" = "ESP - stratum 33 : BasqueCountry, public, D"
		"ESP1634" = "ESP - stratum 34 : BasqueCountry, private, A"
		"ESP1635" = "ESP - stratum 35 : BasqueCountry, private, B"
		"ESP1636" = "ESP - stratum 36 : BasqueCountry, private, D"
		"ESP1737" = "ESP - stratum 37 : Valencia, public, NA"
		"ESP1738" = "ESP - stratum 38 : Valencia, private, NA"
		"ESP1839" = "ESP - stratum 39 : CeutaandMelilla, public, NA"
		"ESP1840" = "ESP - stratum 40 : CeutaandMelilla, private, NA"
		"EST0001" = "EST - stratum 01 : Est"
		"EST0002" = "EST - stratum 02 : Rus"
		"EST0003" = "EST - stratum 03 : Mix"
		"FIN0001" = "FIN - stratum 01 : South-Urban-High"
		"FIN0002" = "FIN - stratum 02 : South-Urban-Low/Not 0"
		"FIN0003" = "FIN - stratum 03 : South-Urban-Low/Zero"
		"FIN0004" = "FIN - stratum 04 : South-Rural-Low"
		"FIN0005" = "FIN - stratum 05 : West-Urban-High"
		"FIN0006" = "FIN - stratum 06 : West-Urban-Low"
		"FIN0007" = "FIN - stratum 07 : West-Rural-Low"
		"FIN0008" = "FIN - stratum 08 : East-Urban-High"
		"FIN0009" = "FIN - stratum 09 : East-Urban-Low"
		"FIN0010" = "FIN - stratum 10 : East-Rural-Low"
		"FIN0011" = "FIN - stratum 11 : North-Urban-High"
		"FIN0012" = "FIN - stratum 12 : North-Urban-Low"
		"FIN0013" = "FIN - stratum 13 : North-Rural-Low"
		"FIN0014" = "FIN - stratum 14 : Swedish/Aland-Urban-Low"
		"FIN0015" = "FIN - stratum 15 : Swedish/Aland-Rural-Low"
		"FIN0016" = "FIN - stratum 16 : Swedish/Not Aland-Urban-Low"
		"FIN0017" = "FIN - stratum 17 : Swedish/Not Aland-Rural-Low"
		"FRA0001" = "FRA - stratum 01 : Lyc GT LS"
		"FRA0002" = "FRA - stratum 02 : Coll LS"
		"FRA0003" = "FRA - stratum 03 : Lyc PRO LS"
		"FRA0004" = "FRA - stratum 04 : Lyc Agr LS"
		"FRA0005" = "FRA - stratum 05 : MSS"
		"FRA0006" = "FRA - stratum 06 : VSS"
		"GRC0001" = "GRC - stratum 01 : Attica public"
		"GRC0002" = "GRC - stratum 02 : Central Greece public "
		"GRC0003" = "GRC - stratum 03 : Central Macedonia public"
		"GRC0004" = "GRC - stratum 04 : Crete public"
		"GRC0005" = "GRC - stratum 05 : East Macedonia public"
		"GRC0006" = "GRC - stratum 06 : Thrace public"
		"GRC0007" = "GRC - stratum 07 : Epirus public"
		"GRC0008" = "GRC - stratum 08 : Ionian islands public"
		"GRC0009" = "GRC - stratum 09 : North Aegean public"
		"GRC0010" = "GRC - stratum 10 : Peloponnese public"
		"GRC0011" = "GRC - stratum 11 : South Aegean public"
		"GRC0012" = "GRC - stratum 12 : Thessaly public"
		"GRC0013" = "GRC - stratum 13 : West Greece public"
		"GRC0014" = "GRC - stratum 14 : West Macedonia public"
		"GRC0015" = "GRC - stratum 15 : Attica private"
		"GRC0016" = "GRC - stratum 16 : Rest of Greece private"
		"HKG0001" = "HKG - stratum 01 : Government"
		"HKG0002" = "HKG - stratum 02 : Aided or Caput"
		"HKG0003" = "HKG - stratum 03 : Private or International"
		"HKG0004" = "HKG - stratum 04 : Direct Subsidy Scheme"
		"HRV0001" = "HRV - stratum 01 : Four year vocational prog."
		"HRV0002" = "HRV - stratum 02 : Vocational - industrial - crafts prog."
		"HRV0003" = "HRV - stratum 03 : Mixed type"
		"HRV0004" = "HRV - stratum 04 : Art - vocational prog."
		"HRV0005" = "HRV - stratum 05 : Gymnasium"
		"HRV0006" = "HRV - stratum 06 : Primary schools (lower secondary education)"
		"HUN0001" = "HUN - stratum 01 : (1) Prim"
		"HUN0002" = "HUN - stratum 02 : (2) Gram 1"
		"HUN0003" = "HUN - stratum 03 : (3) Gram 2"
		"HUN0004" = "HUN - stratum 04 : (4) Gram 3"
		"HUN0005" = "HUN - stratum 05 : (5) Voc Sec"
		"HUN0006" = "HUN - stratum 06 : (6) Voc"
		"IDN0001" = "IDN - stratum 01 : Indonesia"
		"IRL0001" = "IRL - stratum 01 : Size <=40 / Community/Comprehensive"
		"IRL0002" = "IRL - stratum 02 : Size 41-80 / Community/Comprehensive"
		"IRL0003" = "IRL - stratum 03 : Size > 80 / Community/Comprehensive"
		"IRL0004" = "IRL - stratum 04 : Size <=40 / Secondary"
		"IRL0005" = "IRL - stratum 05 : Size 41-80 / Secondary"
		"IRL0006" = "IRL - stratum 06 : Size > 80 / Secondary"
		"IRL0007" = "IRL - stratum 07 : Size <=40 / Vocational"
		"IRL0008" = "IRL - stratum 08 : Size 41-80 / Vocational"
		"IRL0009" = "IRL - stratum 09 : Size > 80 / Vocational"
		"IRL0010" = "IRL - stratum 10 : Non-Aided"
		"IRL0011" = "IRL - stratum 11 : Project Maths"
		"ISL0001" = "ISL - stratum 01 : Reykjavik / 1-8"
		"ISL0002" = "ISL - stratum 02 : Reykjavik / 9-16"
		"ISL0003" = "ISL - stratum 03 : Reykjavik / 17-35"
		"ISL0004" = "ISL - stratum 04 : Reykjavik / > 35"
		"ISL0005" = "ISL - stratum 05 : Nágrenni Reykjavíkur / 1-8"
		"ISL0006" = "ISL - stratum 06 : Nágrenni Reykjavíkur / 9-16"
		"ISL0007" = "ISL - stratum 07 : Nágrenni Reykjavíkur / 17-35"
		"ISL0008" = "ISL - stratum 08 : Nágrenni Reykjavíkur / > 35"
		"ISL0009" = "ISL - stratum 09 : Suðurnes / 1-8"
		"ISL0011" = "ISL - stratum 11 : Suðurnes / 17-35"
		"ISL0012" = "ISL - stratum 12 : Suðurnes / >35"
		"ISL0013" = "ISL - stratum 13 : Vesturland / 1-8"
		"ISL0014" = "ISL - stratum 14 : Vesturland / 9-16"
		"ISL0015" = "ISL - stratum 15 : Vesturland / 17-35"
		"ISL0016" = "ISL - stratum 16 : Vesturland / >35"
		"ISL0017" = "ISL - stratum 17 : Vestfirðir / 1-8"
		"ISL0018" = "ISL - stratum 18 : Vestfirðir / 9-16"
		"ISL0020" = "ISL - stratum 20 : Vestfirðir / > 35"
		"ISL0021" = "ISL - stratum 21 : Norðurland vestra / 1-8"
		"ISL0022" = "ISL - stratum 22 : Norðurland vestra / 9-16"
		"ISL0023" = "ISL - stratum 23 : Norðurland vestra /17-35"
		"ISL0024" = "ISL - stratum 24 : Norðurland vestra / >35"
		"ISL0025" = "ISL - stratum 25 : Norðurland eystra / 1-8"
		"ISL0027" = "ISL - stratum 27 : Norðurland eystra / 17-35"
		"ISL0028" = "ISL - stratum 28 : Norðurland eystra / > 35"
		"ISL0029" = "ISL - stratum 29 : Austurland / 1-8"
		"ISL0030" = "ISL - stratum 30 : Austurland  / 9-16"
		"ISL0031" = "ISL - stratum 31 : Austurland  / 17-35"
		"ISL0033" = "ISL - stratum 33 :  Suðurland / 1-8"
		"ISL0034" = "ISL - stratum 34 : Suðurland / 9-16"
		"ISL0035" = "ISL - stratum 35 : Suðurland /17-35"
		"ISL0036" = "ISL - stratum 36 : Suðurland / > 35"
		"ISR0001" = "ISR - stratum 01 "
		"ISR0002" = "ISR - stratum 02 "
		"ISR0003" = "ISR - stratum 03 "
		"ISR0004" = "ISR - stratum 04 "
		"ISR0005" = "ISR - stratum 05 "
		"ISR0006" = "ISR - stratum 06 "
		"ISR0007" = "ISR - stratum 07 "
		"ISR0008" = "ISR - stratum 08 "
		"ISR0009" = "ISR - stratum 09 "
		"ISR0010" = "ISR - stratum 10 "
		"ISR0011" = "ISR - stratum 11 "
		"ISR0012" = "ISR - stratum 12 "
		"ITA0101" = "ITA - REGION 01 stratum 01 : [Region 01] Bolzano Licei"
		"ITA0102" = "ITA - REGION 01 stratum 02 : [Region 01] Bolzano Tecnici"
		"ITA0103" = "ITA - REGION 01 stratum 03 : [Region 01] Bolzano Professionali"
		"ITA0104" = "ITA - REGION 01 stratum 04 : [Region 01] Bolzano Medie"
		"ITA0105" = "ITA - REGION 01 stratum 05 : [Region 01] Bolzano Formazione Professionale"
		"ITA0201" = "ITA - REGION 02 stratum 01 : [Region 02] Basilicata Licei"
		"ITA0202" = "ITA - REGION 02 stratum 02 : [Region 02] Basilicata Tecnici"
		"ITA0203" = "ITA - REGION 02 stratum 03 : [Region 02] Basilicata Professionali"
		"ITA0204" = "ITA - REGION 02 stratum 04 : [Region 02] Basilicata Medie"
		"ITA0301" = "ITA - REGION 03 stratum 01 : [Region 03] Campania Licei"
		"ITA0302" = "ITA - REGION 03 stratum 02 : [Region 03] Campania Tecnici"
		"ITA0303" = "ITA - REGION 03 stratum 03 : [Region 03] Campania Professionali"
		"ITA0304" = "ITA - REGION 03 stratum 04 : [Region 03] Campania Medie"
		"ITA0401" = "ITA - REGION 04 stratum 01 : [Region 04] Emilia Romagna Licei"
		"ITA0402" = "ITA - REGION 04 stratum 02 : [Region 04] Emilia Romagna Tecnici"
		"ITA0403" = "ITA - REGION 04 stratum 03 : [Region 04] Emilia Romagna Professionali"
		"ITA0404" = "ITA - REGION 04 stratum 04 : [Region 04] Emilia Romagna Medie"
		"ITA0405" = "ITA - REGION 04 stratum 05 : [Region 04] Emilia Romagna Formazione Professionale"
		"ITA0501" = "ITA - REGION 05 stratum 01 : [Region 05] Friuli Venezia Giulia Licei"
		"ITA0502" = "ITA - REGION 05 stratum 02 : [Region 05] Friuli Venezia Giulia Tecnici"
		"ITA0503" = "ITA - REGION 05 stratum 03 : [Region 05] Friuli Venezia Giulia Professionali"
		"ITA0504" = "ITA - REGION 05 stratum 04 : [Region 05] Friuli Venezia Giulia Medie"
		"ITA0505" = "ITA - REGION 05 stratum 05 : [Region 05] Friuli Venezia Giulia Formazione Professionale"
		"ITA0601" = "ITA - REGION 06 stratum 01 : [Region 06] Liguria Licei"
		"ITA0602" = "ITA - REGION 06 stratum 02 : [Region 06] Liguria Tecnici"
		"ITA0603" = "ITA - REGION 06 stratum 03 : [Region 06] Liguria Professionali"
		"ITA0604" = "ITA - REGION 06 stratum 04 : [Region 06] Liguria Medie"
		"ITA0605" = "ITA - REGION 06 stratum 05 : [Region 06] Liguria Formazione Professionale"
		"ITA0701" = "ITA - REGION 07 stratum 01 : [Region 07] Lombardia Licei"
		"ITA0702" = "ITA - REGION 07 stratum 02 : [Region 07] Lombardia Tecnici"
		"ITA0703" = "ITA - REGION 07 stratum 03 : [Region 07] Lombardia Professionali"
		"ITA0704" = "ITA - REGION 07 stratum 04 : [Region 07] Lombardia Medie"
		"ITA0705" = "ITA - REGION 07 stratum 05 : [Region 07] Lombardia Formazione Professionale"
		"ITA0801" = "ITA - REGION 08 stratum 01 : [Region 08] Piemonte Licei"
		"ITA0802" = "ITA - REGION 08 stratum 02 : [Region 08] Piemonte Tecnici"
		"ITA0803" = "ITA - REGION 08 stratum 03 : [Region 08] Piemonte Professionali"
		"ITA0804" = "ITA - REGION 08 stratum 04 : [Region 08] Piemonte Medie"
		"ITA0805" = "ITA - REGION 08 stratum 05 : [Region 08] Piemonte Formazione Professionale"
		"ITA0901" = "ITA - REGION 09 stratum 01 : [Region 09] Puglia Licei"
		"ITA0902" = "ITA - REGION 09 stratum 02 : [Region 09] Puglia Tecnici"
		"ITA0903" = "ITA - REGION 09 stratum 03 : [Region 09] Puglia Professionali"
		"ITA0904" = "ITA - REGION 09 stratum 04 : [Region 09] Puglia Medie"
		"ITA0905" = "ITA - REGION 09 stratum 05 : [Region 09] Puglia Formazione Professionale"
		"ITA1001" = "ITA - REGION 10 stratum 01 : [Region 10] Sardegna Licei"
		"ITA1002" = "ITA - REGION 10 stratum 02 : [Region 10] Sardegna Tecnici"
		"ITA1003" = "ITA - REGION 10 stratum 03 : [Region 10] Sardegna Professionali"
		"ITA1004" = "ITA - REGION 10 stratum 04 : [Region 10] Sardegna Medie"
		"ITA1101" = "ITA - REGION 11 stratum 01 : [Region 11] Sicilia Licei"
		"ITA1102" = "ITA - REGION 11 stratum 02 : [Region 11] Sicilia Tecnici"
		"ITA1103" = "ITA - REGION 11 stratum 03 : [Region 11] Sicilia Professionali"
		"ITA1104" = "ITA - REGION 11 stratum 04 : [Region 11] Sicilia Medie"
		"ITA1201" = "ITA - REGION 12 stratum 01 : [Region 12] Trento Licei"
		"ITA1202" = "ITA - REGION 12 stratum 02 : [Region 12] Trento Tecnici"
		"ITA1203" = "ITA - REGION 12 stratum 03 : [Region 12] Trento Professionali"
		"ITA1204" = "ITA - REGION 12 stratum 04 : [Region 12] Trento Medie"
		"ITA1205" = "ITA - REGION 12 stratum 05 : [Region 12] Trento Formazione Professionale"
		"ITA1301" = "ITA - REGION 13 stratum 01 : [Region 13] Veneto (Other) Licei"
		"ITA1302" = "ITA - REGION 13 stratum 02 : [Region 13] Veneto (Other) Tecnici"
		"ITA1303" = "ITA - REGION 13 stratum 03 : [Region 13] Veneto (Other) Professionali"
		"ITA1304" = "ITA - REGION 13 stratum 04 : [Region 13] Veneto (Other) Medie"
		"ITA1305" = "ITA - REGION 13 stratum 05 : [Region 13] Veneto (Other) Formazione Professionale"
		"ITA1306" = "ITA - REGION 13 stratum 06 : [Region 13] Veneto (Rovigo) Licei"
		"ITA1307" = "ITA - REGION 13 stratum 07 : [Region 13] Veneto (Rovigo) Tecnici"
		"ITA1308" = "ITA - REGION 13 stratum 08 : [Region 13] Veneto (Rovigo) Professionali"
		"ITA1309" = "ITA - REGION 13 stratum 09 : [Region 13] Veneto (Rovigo) Medie"
		"ITA1310" = "ITA - REGION 13 stratum 10 : [Region 13] Veneto (Rovigo) Formazione Professionale"
		"ITA1401" = "ITA - REGION 14 stratum 01 : [Region 14] Abruzzo Licei"
		"ITA1402" = "ITA - REGION 14 stratum 02 : [Region 14] Abruzzo Tecnici"
		"ITA1403" = "ITA - REGION 14 stratum 03 : [Region 14] Abruzzo Professionali"
		"ITA1404" = "ITA - REGION 14 stratum 04 : [Region 14] Abruzzo Medie"
		"ITA1405" = "ITA - REGION 14 stratum 05 : [Region 14] Abruzzo Formazione Professionale"
		"ITA1501" = "ITA - REGION 15 stratum 01 : [Region 15] Calabria Licei"
		"ITA1502" = "ITA - REGION 15 stratum 02 : [Region 15] Calabria Tecnici"
		"ITA1503" = "ITA - REGION 15 stratum 03 : [Region 15] Calabria Professionali"
		"ITA1504" = "ITA - REGION 15 stratum 04 : [Region 15] Calabria Medie"
		"ITA1505" = "ITA - REGION 15 stratum 05 : [Region 15] Calabria Formazione Professionale"
		"ITA1601" = "ITA - REGION 16 stratum 01 : [Region 16] Lazio Licei"
		"ITA1602" = "ITA - REGION 16 stratum 02 : [Region 16] Lazio Tecnici"
		"ITA1603" = "ITA - REGION 16 stratum 03 : [Region 16] Lazio Professionali"
		"ITA1604" = "ITA - REGION 16 stratum 04 : [Region 16] Lazio Medie"
		"ITA1701" = "ITA - REGION 17 stratum 01 : [Region 17] Marche Licei"
		"ITA1702" = "ITA - REGION 17 stratum 02 : [Region 17] Marche Tecnici"
		"ITA1703" = "ITA - REGION 17 stratum 03 : [Region 17] Marche Professionali"
		"ITA1704" = "ITA - REGION 17 stratum 04 : [Region 17] Marche Medie"
		"ITA1705" = "ITA - REGION 17 stratum 05 : [Region 17] Marche Formazione Professionale"
		"ITA1801" = "ITA - REGION 18 stratum 01 : [Region 18] Molise Licei"
		"ITA1802" = "ITA - REGION 18 stratum 02 : [Region 18] Molise Tecnici"
		"ITA1803" = "ITA - REGION 18 stratum 03 : [Region 18] Molise Professionali"
		"ITA1804" = "ITA - REGION 18 stratum 04 : [Region 18] Molise Medie"
		"ITA1901" = "ITA - REGION 19 stratum 01 : [Region 19] Toscana Licei"
		"ITA1902" = "ITA - REGION 19 stratum 02 : [Region 19] Toscana Tecnici"
		"ITA1903" = "ITA - REGION 19 stratum 03 : [Region 19] Toscana Professionali"
		"ITA1904" = "ITA - REGION 19 stratum 04 : [Region 19] Toscana Medie"
		"ITA1905" = "ITA - REGION 19 stratum 05 : [Region 19] Toscana Formazione Professionale"
		"ITA2001" = "ITA - REGION 20 stratum 01 : [Region 20] Umbria Licei"
		"ITA2002" = "ITA - REGION 20 stratum 02 : [Region 20] Umbria Tecnici"
		"ITA2003" = "ITA - REGION 20 stratum 03 : [Region 20] Umbria Professionali"
		"ITA2004" = "ITA - REGION 20 stratum 04 : [Region 20] Umbria Medie"
		"ITA2005" = "ITA - REGION 20 stratum 05 : [Region 20] Umbria Formazione Professionale"
		"ITA2101" = "ITA - REGION 21 stratum 01 : [Region 21] Valle dAosta Licei"
		"ITA2102" = "ITA - REGION 21 stratum 02 : [Region 21] Valle dAosta Tecnici"
		"ITA2103" = "ITA - REGION 21 stratum 03 : [Region 21] Valle dAosta Professionali"
		"ITA2104" = "ITA - REGION 21 stratum 04 : [Region 21] Valle dAosta Medie"
		"JOR0001" = "JOR - stratum 01 : Public_Discovery"
		"JOR0002" = "JOR - stratum 02 : Public_Madrasati"
		"JOR0003" = "JOR - stratum 03 : Public_ERSP"
		"JOR0004" = "JOR - stratum 04 : Public_CIDA"
		"JOR0005" = "JOR - stratum 05 : Public_other"
		"JOR0006" = "JOR - stratum 06 : UNRWA"
		"JOR0007" = "JOR - stratum 07 : Private"
		"JPN0101" = "JPN - stratum 01 : 01: Public and Academic"
		"JPN0202" = "JPN - stratum 02 : 02: Public and Practical"
		"JPN0203" = "JPN - stratum 03 : 03: Private and Academic"
		"JPN0204" = "JPN - stratum 04 : 04: Private and Practical"
		"KAZ0001" = "KAZ - stratum 01 : Akmolinskaya oblast, kazakh only"
		"KAZ0002" = "KAZ - stratum 02 : Akmolinskaya oblast, russian only"
		"KAZ0003" = "KAZ - stratum 03 : Akmolinskaya oblast, both"
		"KAZ0004" = "KAZ - stratum 04 : Aktubinskaya oblast, kazakh only"
		"KAZ0005" = "KAZ - stratum 05 : Aktubinskaya oblast, russian only"
		"KAZ0006" = "KAZ - stratum 06 : Aktubinskaya oblast, both"
		"KAZ0007" = "KAZ - stratum 07 : Almatinskaya oblast, kazakh only"
		"KAZ0008" = "KAZ - stratum 08 : Almatinskaya oblast, russian only"
		"KAZ0009" = "KAZ - stratum 09 : Almatinskaya oblast, both"
		"KAZ0010" = "KAZ - stratum 10 : Almatinskaya oblast, Kazakh and other or uighur"
		"KAZ0011" = "KAZ - stratum 11 : Almatinskaya oblast, russian and uighur"
		"KAZ0012" = "KAZ - stratum 12 : Almatinskaya oblast, russian and kazakh and uighur"
		"KAZ0013" = "KAZ - stratum 13 : Astana city, kazakh only"
		"KAZ0014" = "KAZ - stratum 14 : Astana city, russian"
		"KAZ0015" = "KAZ - stratum 15 : Astana city, both"
		"KAZ0016" = "KAZ - stratum 16 : Atyrauskaya oblast, kazakh only"
		"KAZ0017" = "KAZ - stratum 17 : Atyrauskaya oblast, russian only"
		"KAZ0018" = "KAZ - stratum 18 : Atyrauskaya oblast, both"
		"KAZ0019" = "KAZ - stratum 19 : East-Kazakhstan oblast, kazakh only"
		"KAZ0020" = "KAZ - stratum 20 : East-Kazakhstan oblast, russian only"
		"KAZ0021" = "KAZ - stratum 21 : East-Kazakhstan oblast, both"
		"KAZ0022" = "KAZ - stratum 22 : Almaty city, kazakh only"
		"KAZ0023" = "KAZ - stratum 23 : Almaty city, russian only"
		"KAZ0024" = "KAZ - stratum 24 : Almaty city, both"
		"KAZ0025" = "KAZ - stratum 25 : Almaty city, russian and uighur w/wo kazakh"
		"KAZ0026" = "KAZ - stratum 26 : Zhambylskaya oblast, kazakh only"
		"KAZ0027" = "KAZ - stratum 27 : Zhambylskaya oblast, russian only"
		"KAZ0028" = "KAZ - stratum 28 : Zhambylskaya oblast, both"
		"KAZ0029" = "KAZ - stratum 29 : West-Kazakhstan oblast, kazakh only"
		"KAZ0030" = "KAZ - stratum 30 : West-Kazakhstan oblast, russian only"
		"KAZ0031" = "KAZ - stratum 31 : West-Kazakhstan oblast, both"
		"KAZ0032" = "KAZ - stratum 32 : Karagandinskaya oblast, kazakh only"
		"KAZ0033" = "KAZ - stratum 33 : Karagandinskaya oblast, russian only"
		"KAZ0034" = "KAZ - stratum 34 : Karagandinskaya oblast, both"
		"KAZ0035" = "KAZ - stratum 35 : Kostanayskaya oblast, kazakh only"
		"KAZ0036" = "KAZ - stratum 36 : Kostanayskaya oblast, russian only"
		"KAZ0037" = "KAZ - stratum 37 : Kostanayskaya oblast, both"
		"KAZ0038" = "KAZ - stratum 38 : Kyzyl-Ordinskaya oblast, kazakh only"
		"KAZ0039" = "KAZ - stratum 39 : Kyzyl-Ordinskaya oblast, russian only"
		"KAZ0040" = "KAZ - stratum 40 : Kyzyl-Ordinskaya oblast, both"
		"KAZ0041" = "KAZ - stratum 41 : Mangystauskaya oblast, kazakh only"
		"KAZ0042" = "KAZ - stratum 42 : Mangystauskaya oblast, russian only"
		"KAZ0043" = "KAZ - stratum 43 : Mangystauskaya oblast, both"
		"KAZ0044" = "KAZ - stratum 44 : Pavlodarskaya oblast, kazakh only"
		"KAZ0045" = "KAZ - stratum 45 : Pavlodarskaya oblast, russian only"
		"KAZ0046" = "KAZ - stratum 46 : Pavlodarskaya oblast, both"
		"KAZ0047" = "KAZ - stratum 47 : North-Kazakhstan oblast, kazakh only"
		"KAZ0048" = "KAZ - stratum 48 : North-Kazakhstan oblast, russian only"
		"KAZ0049" = "KAZ - stratum 49 : North-Kazakhstan oblast, both"
		"KAZ0050" = "KAZ - stratum 50 : South-Kazakhstan oblast, kazakh only"
		"KAZ0051" = "KAZ - stratum 51 : South-Kazakhstan oblast, russian only"
		"KAZ0052" = "KAZ - stratum 52 : South-Kazakhstan oblast, both"
		"KAZ0053" = "KAZ - stratum 53 : South-Kazakhstan oblast, kazakh and other"
		"KAZ0054" = "KAZ - stratum 54 : South-Kazakhstan oblast, kazakh and uzbek"
		"KAZ0055" = "KAZ - stratum 55 : South-Kazakhstan oblast, kazakh and uzbek and other"
		"KAZ0056" = "KAZ - stratum 56 : South-Kazakhstan oblast, russian and uzbek "
		"KAZ0057" = "KAZ - stratum 57 : South-Kazakhstan oblast, russian and kazakh and uzbek"
		"KAZ0058" = "KAZ - stratum 58 : South-Kazakhstan oblast, russian and kazakh and other"
		"KOR0101" = "KOR - stratum 01 : Stratum 1:  Lower secondary/General"
		"KOR0202" = "KOR - stratum 02 : Stratum 2:  Upper secondary/General"
		"KOR0302" = "KOR - stratum 02 : Stratum 2:  Upper secondary/General"
		"KOR0403" = "KOR - stratum 03 : Stratum 3: Upper secondary/Vocational"
		"LIE9901" = "LIE - stratum 01 : Liechtenstein"
		"LTU0001" = "LTU - stratum 01 : Capital-Basic"
		"LTU0002" = "LTU - stratum 02 : Capital-Secondary"
		"LTU0003" = "LTU - stratum 03 : Capital-Gymnasium"
		"LTU0004" = "LTU - stratum 04 : Capital-Other"
		"LTU0005" = "LTU - stratum 05 : City-Basic"
		"LTU0006" = "LTU - stratum 06 : City-Secondary"
		"LTU0007" = "LTU - stratum 07 : City-Gymnasium"
		"LTU0008" = "LTU - stratum 08 : City-Other"
		"LTU0009" = "LTU - stratum 09 : Town-Basic"
		"LTU0010" = "LTU - stratum 10 : Town-Secondary"
		"LTU0011" = "LTU - stratum 11 : Town-Gymnasium"
		"LTU0012" = "LTU - stratum 12 : Town-Other"
		"LTU0013" = "LTU - stratum 13 : Village-Basic"
		"LTU0014" = "LTU - stratum 14 : Village-Secondary"
		"LTU0015" = "LTU - stratum 15 : Village-Gymnasium"
		"LTU0016" = "LTU - stratum 16 : Village-Other "
		"LUX0001" = "LUX - stratum 01 : ES government-public"
		"LUX0002" = "LUX - stratum 02 : EST government-public"
		"LUX0003" = "LUX - stratum 03 : ES+EST government-public"
		"LUX0004" = "LUX - stratum 04 : EST government-private"
		"LUX0005" = "LUX - stratum 05 : ES+EST government-private"
		"LUX0006" = "LUX - stratum 06 : Government independent"
		"LVA0001" = "LVA - stratum 01 : Riga"
		"LVA0002" = "LVA - stratum 02 : City"
		"LVA0003" = "LVA - stratum 03 : Town"
		"LVA0004" = "LVA - stratum 04 : Rural"
		"MAC0001" = "MAC - stratum 01 : Stratum 1:  Public, Grammar-International, Chinese and Portuguese"
		"MAC0002" = "MAC - stratum 02 : Stratum 2:  Public, Technical-vocational, Chinese"
		"MAC0003" = "MAC - stratum 03 : Stratum 3:  Private-in-Net, Grammar-International, Chinese"
		"MAC0004" = "MAC - stratum 04 : Stratum 4:  Private-in-Net, Grammar-International, English"
		"MAC0005" = "MAC - stratum 05 : Stratum 5:  Private-in-Net, Grammar-International, Chinese and English "
		"MAC0006" = "MAC - stratum 06 : Stratum 6:  Private-in-Net, Technical-vocational, Chinese"
		"MAC0007" = "MAC - stratum 07 : Stratum 7:  Private-not- in-Net, Grammar-International, Chinese"
		"MAC0008" = "MAC - stratum 08 : Stratum 8:  Private-not-in-Net, Grammar-International, English"
		"MAC0009" = "MAC - stratum 09 : Stratum 9: Private-not-in-Net, Grammar-International, Portuguese"
		"MAC0010" = "MAC - stratum 10 : Stratum 10: Private-not- in-Net, Grammar-International, Chinese and English"
		"MEX0101" = "MEX - REGION 01 stratum 01 : AGUASCALIENTES, large schools"
		"MEX0102" = "MEX - REGION 01 stratum 02 : AGUASCALIENTES, moderately small schools"
		"MEX0103" = "MEX - REGION 01 stratum 03 : AGUASCALIENTES, very small schools"
		"MEX0204" = "MEX - REGION 02 stratum 04 : BAJA CALIFORNIA, large schools"
		"MEX0205" = "MEX - REGION 02 stratum 05 : BAJA CALIFORNIA, moderately small schools"
		"MEX0206" = "MEX - REGION 02 stratum 06 : BAJA CALIFORNIA, very small schools"
		"MEX0307" = "MEX - REGION 03 stratum 07 : BAJA CALIFORNIA SUR, large schools"
		"MEX0308" = "MEX - REGION 03 stratum 08 : BAJA CALIFORNIA SUR, moderately small schools"
		"MEX0309" = "MEX - REGION 03 stratum 09 : BAJA CALIFORNIA SUR, very small schools"
		"MEX0410" = "MEX - REGION 04 stratum 10 : CAMPECHE, large schools"
		"MEX0411" = "MEX - REGION 04 stratum 11 : CAMPECHE, moderately small schools"
		"MEX0412" = "MEX - REGION 04 stratum 12 : CAMPECHE, very small schools"
		"MEX0513" = "MEX - REGION 05 stratum 13 : COAHUILA, large schools"
		"MEX0514" = "MEX - REGION 05 stratum 14 : COAHUILA, moderately small schools"
		"MEX0515" = "MEX - REGION 05 stratum 15 : COAHUILA, very small schools"
		"MEX0616" = "MEX - REGION 06 stratum 16 : COLIMA, large schools"
		"MEX0617" = "MEX - REGION 06 stratum 17 : COLIMA, moderately small schools"
		"MEX0618" = "MEX - REGION 06 stratum 18 : COLIMA, very small schools"
		"MEX0719" = "MEX - REGION 07 stratum 19 : CHIAPAS, large schools"
		"MEX0720" = "MEX - REGION 07 stratum 20 : CHIAPAS, moderately small schools"
		"MEX0721" = "MEX - REGION 07 stratum 21 : CHIAPAS, very small schools"
		"MEX0822" = "MEX - REGION 08 stratum 22 : CHIHUAHUA, large schools"
		"MEX0823" = "MEX - REGION 08 stratum 23 : CHIHUAHUA, moderately small schools"
		"MEX0824" = "MEX - REGION 08 stratum 24 : CHIHUAHUA, very small schools"
		"MEX0925" = "MEX - REGION 09 stratum 25 : DISTRITO FEDERAL, large schools"
		"MEX0926" = "MEX - REGION 09 stratum 26 : DISTRITO FEDERAL, moderately small schools"
		"MEX0927" = "MEX - REGION 09 stratum 27 : DISTRITO FEDERAL, very small schools"
		"MEX1028" = "MEX - REGION 10 stratum 28 : DURANGO, large schools"
		"MEX1029" = "MEX - REGION 10 stratum 29 : DURANGO, moderately small schools"
		"MEX1030" = "MEX - REGION 10 stratum 30 : DURANGO, very small schools"
		"MEX1131" = "MEX - REGION 11 stratum 31 : GUANAJUATO, large schools"
		"MEX1132" = "MEX - REGION 11 stratum 32 : GUANAJUATO, moderately small schools"
		"MEX1133" = "MEX - REGION 11 stratum 33 : GUANAJUATO, very small schools"
		"MEX1234" = "MEX - REGION 12 stratum 34 : GUERRERO, large schools"
		"MEX1235" = "MEX - REGION 12 stratum 35 : GUERRERO, moderately small schools"
		"MEX1236" = "MEX - REGION 12 stratum 36 : GUERRERO, very small schools"
		"MEX1337" = "MEX - REGION 13 stratum 37 : HIDALGO, large schools"
		"MEX1338" = "MEX - REGION 13 stratum 38 : HIDALGO, moderately small schools"
		"MEX1339" = "MEX - REGION 13 stratum 39 : HIDALGO, very small schools"
		"MEX1440" = "MEX - REGION 14 stratum 40 : JALISCO, large schools"
		"MEX1441" = "MEX - REGION 14 stratum 41 : JALISCO, moderately small schools"
		"MEX1442" = "MEX - REGION 14 stratum 42 : JALISCO, very small schools"
		"MEX1543" = "MEX - REGION 15 stratum 43 : MEXICO, large schools"
		"MEX1544" = "MEX - REGION 15 stratum 44 : MEXICO, moderately small schools"
		"MEX1545" = "MEX - REGION 15 stratum 45 : MEXICO, very small schools"
		"MEX1749" = "MEX - REGION 17 stratum 49 : MORELOS, large schools"
		"MEX1750" = "MEX - REGION 17 stratum 50 : MORELOS, moderately small schools"
		"MEX1751" = "MEX - REGION 17 stratum 51 : MORELOS, very small schools"
		"MEX1852" = "MEX - REGION 18 stratum 52 : NAYARIT, large schools"
		"MEX1853" = "MEX - REGION 18 stratum 53 : NAYARIT, moderately small schools"
		"MEX1854" = "MEX - REGION 18 stratum 54 : NAYARIT, very small schools"
		"MEX1955" = "MEX - REGION 19 stratum 55 : NUEVO LEON, large schools"
		"MEX1956" = "MEX - REGION 19 stratum 56 : NUEVO LEON, moderately small schools"
		"MEX1957" = "MEX - REGION 19 stratum 57 : NUEVO LEON, very small schools"
		"MEX2161" = "MEX - REGION 21 stratum 61 : PUEBLA, large schools"
		"MEX2162" = "MEX - REGION 21 stratum 62 : PUEBLA, moderately small schools"
		"MEX2163" = "MEX - REGION 21 stratum 63 : PUEBLA, very small schools"
		"MEX2264" = "MEX - REGION 22 stratum 64 : QUERETARO, large schools"
		"MEX2265" = "MEX - REGION 22 stratum 65 : QUERETARO, moderately small schools"
		"MEX2266" = "MEX - REGION 22 stratum 66 : QUERETARO, very small schools"
		"MEX2367" = "MEX - REGION 23 stratum 67 : QUINTANA ROO, large schools"
		"MEX2368" = "MEX - REGION 23 stratum 68 : QUINTANA ROO, moderately small schools"
		"MEX2369" = "MEX - REGION 23 stratum 69 : QUINTANA ROO, very small schools"
		"MEX2470" = "MEX - REGION 24 stratum 70 : SAN LUIS POTOSI, large schools"
		"MEX2471" = "MEX - REGION 24 stratum 71 : SAN LUIS POTOSI, moderately small schools"
		"MEX2472" = "MEX - REGION 24 stratum 72 : SAN LUIS POTOSI, very small schools"
		"MEX2573" = "MEX - REGION 25 stratum 73 : SINALOA, large schools"
		"MEX2574" = "MEX - REGION 25 stratum 74 : SINALOA, moderately small schools"
		"MEX2575" = "MEX - REGION 25 stratum 75 : SINALOA, very small schools"
		"MEX2779" = "MEX - REGION 27 stratum 79 : TABASCO, large schools"
		"MEX2780" = "MEX - REGION 27 stratum 80 : TABASCO, moderately small schools"
		"MEX2781" = "MEX - REGION 27 stratum 81 : TABASCO, very small schools"
		"MEX2882" = "MEX - REGION 28 stratum 82 : TAMAULIPAS, large schools"
		"MEX2883" = "MEX - REGION 28 stratum 83 : TAMAULIPAS, moderately small schools"
		"MEX2884" = "MEX - REGION 28 stratum 84 : TAMAULIPAS, very small schools"
		"MEX2985" = "MEX - REGION 29 stratum 85 : TLAXCALA, large schools"
		"MEX2986" = "MEX - REGION 29 stratum 86 : TLAXCALA, moderately small schools"
		"MEX2987" = "MEX - REGION 29 stratum 87 : TLAXCALA, very small schools"
		"MEX3088" = "MEX - REGION 30 stratum 88 : VERACRUZ, large schools"
		"MEX3089" = "MEX - REGION 30 stratum 89 : VERACRUZ, moderately small schools"
		"MEX3090" = "MEX - REGION 30 stratum 90 : VERACRUZ, very small schools"
		"MEX3191" = "MEX - REGION 31 stratum 91 : YUCATAN, large schools"
		"MEX3192" = "MEX - REGION 31 stratum 92 : YUCATAN, moderately small schools"
		"MEX3193" = "MEX - REGION 31 stratum 93 : YUCATAN, very small schools"
		"MEX3294" = "MEX - REGION 32 stratum 94 : ZACATECAS, large schools"
		"MEX3295" = "MEX - REGION 32 stratum 95 : ZACATECAS, moderately small schools"
		"MEX3296" = "MEX - REGION 32 stratum 96 : ZACATECAS, very small schools"
		"MEX9101" = "MEX - REGION 91 stratum 01 : [NO Region 99 and above] Prv. Upp. Sec. Large Schools"
		"MEX9102" = "MEX - REGION 91 stratum 02 : [NO Region 99 and above] Prv. Upp. Sec. MSS"
		"MEX9103" = "MEX - REGION 91 stratum 03 : [NO Region 99 and above] Prv. Upp. Sec. VSS"
		"MEX9104" = "MEX - REGION 91 stratum 04 : [NO Region 99 and above] Aut. Gen. Upp. Sec. Pub."
		"MEX9105" = "MEX - REGION 91 stratum 05 : [NO Region 99 and above] Cen. St. Gen. Upp. Sec. Pub. "
		"MEX9106" = "MEX - REGION 91 stratum 06 : [NO Region 99 and above] Dec. St. Gen. Upp. Sec. Pub."
		"MEX9107" = "MEX - REGION 91 stratum 07 : [NO Region 99 and above] Oth. Gen. Upp. Sec. Pub."
		"MEX9108" = "MEX - REGION 91 stratum 08 : [NO Region 99 and above] Cen. Fed. Tech. Upp. Sec. Pub."
		"MEX9109" = "MEX - REGION 91 stratum 09 : [NO Region 99 and above] Oth. Tech. Upp. Sec. Pub."
		"MEX9110" = "MEX - REGION 91 stratum 10 : [NO Region 99 and above] CONALEP-Prof. Tech. Pub. "
		"MEX9111" = "MEX - REGION 91 stratum 11 : [NO Region 99 and above] Oth. Prof. Tech. Pub."
		"MNE0001" = "MNE - stratum 01 : A-Primary-Central"
		"MNE0002" = "MNE - stratum 02 : A-Primary-North"
		"MNE0003" = "MNE - stratum 03 : A-Primary-South"
		"MNE0004" = "MNE - stratum 04 : B-Gimnazija-Central"
		"MNE0005" = "MNE - stratum 05 : B-Gimnazija -North"
		"MNE0006" = "MNE - stratum 06 : B-Gimnazija -South"
		"MNE0007" = "MNE - stratum 07 : C-Vocational-Central"
		"MNE0008" = "MNE - stratum 08 : C-Vocational-North"
		"MNE0009" = "MNE - stratum 09 : C-Vocational-South"
		"MNE0010" = "MNE - stratum 10 : D-Mixed-North"
		"MNE0011" = "MNE - stratum 11 : D-Mixed-South"
		"MYS0101" = "MYS - stratum 01 : MOE Fully Residential School"
		"MYS0102" = "MYS - stratum 02 : MOE National Secondary School"
		"MYS0103" = "MYS - stratum 03 : MOE Religious School"
		"MYS0104" = "MYS - stratum 04 : MOE Technical School"
		"MYS0106" = "MYS - stratum 06 : non-MOE Other Schools"
		"MYS0201" = "MYS - stratum 01 : MOE Fully Residential School"
		"MYS0202" = "MYS - stratum 02 : MOE National Secondary School"
		"MYS0203" = "MYS - stratum 03 : MOE Religious School"
		"MYS0204" = "MYS - stratum 04 : MOE Technical School"
		"MYS0205" = "MYS - stratum 05 : non-MOE MARA Junior Science College"
		"MYS0206" = "MYS - stratum 06 : non-MOE Other Schools"
		"NLD0001" = "NLD - stratum 01 : 01_PRO/VMBO"
		"NLD0002" = "NLD - stratum 02 : 02_HAVO/VWO"
		"NLD0003" = "NLD - stratum 03 : 03_General Education"
		"NLD0004" = "NLD - stratum 04 : 04_Private Education"
		"NOR0001" = "NOR - stratum 01 : Lower secondary schools"
		"NOR0002" = "NOR - stratum 02 : Upper secondary schools"
		"NOR0003" = "NOR - stratum 03 : Mixed/Both"
		"NZL0101" = "NZL - stratum 01 : Stratum 1:Large schools (250+)"
		"NZL0102" = "NZL - stratum 02 : Stratum 2:Medium large schools (> 29 and < 250)"
		"NZL0202" = "NZL - stratum 02 : Stratum 2:Medium large schools (> 29 and < 250)"
		"NZL0203" = "NZL - stratum 03 : Stratum 3:Small schools (<30)"
		"PER0001" = "PER - stratum 01 : Public/Urban "
		"PER0002" = "PER - stratum 02 : Public/Rural"
		"PER0003" = "PER - stratum 03 : Private/Urban "
		"PER0004" = "PER - stratum 04 : Private/Rural"
		"POL0001" = "POL - stratum 01 : 1 – Public Gymnasium"
		"POL0002" = "POL - stratum 02 : 2 – Private Gymnasium"
		"POL0003" = "POL - stratum 03 : 3 – Lycee"
		"POL0004" = "POL - stratum 04 : 4 – Vocational”"
		"PRI0072" = "PRI - stratum 72 : PUERTO RICO"
		"PRT0001" = "PRT - stratum 01 : Alentejo - Alentejo Central"
		"PRT0002" = "PRT - stratum 02 : Alentejo - Alentejo Litoral"
		"PRT0003" = "PRT - stratum 03 : Alentejo - Alto Alentejo"
		"PRT0004" = "PRT - stratum 04 : Alentejo - Baixo Alentejo"
		"PRT0005" = "PRT - stratum 05 : Alentejo - Lezíria do Tejo"
		"PRT0006" = "PRT - stratum 06 : Algarve - Algarve"
		"PRT0007" = "PRT - stratum 07 : Centro - Baixo Mondego"
		"PRT0008" = "PRT - stratum 08 : Centro - Baixo Vouga"
		"PRT0009" = "PRT - stratum 09 : Centro - Beira Interior Norte"
		"PRT0010" = "PRT - stratum 10 : Centro - Beira Interior Sul"
		"PRT0011" = "PRT - stratum 11 : Centro - Cova da Beira"
		"PRT0012" = "PRT - stratum 12 : Centro - Dão Lafões"
		"PRT0013" = "PRT - stratum 13 : Centro - Médio Tejo"
		"PRT0014" = "PRT - stratum 14 : Centro - Oeste"
		"PRT0015" = "PRT - stratum 15 : Centro - Pinhal Interior Norte"
		"PRT0016" = "PRT - stratum 16 : Centro - Pinhal Interior Sul"
		"PRT0017" = "PRT - stratum 17 : Centro - Pinhal Litoral"
		"PRT0018" = "PRT - stratum 18 : Centro - Serra da Estrela"
		"PRT0019" = "PRT - stratum 19 : Lisboa - Grande Lisboa"
		"PRT0020" = "PRT - stratum 20 : Lisboa - Península de Setúbal"
		"PRT0021" = "PRT - stratum 21 : Norte - Alto Trás-os-Montes"
		"PRT0022" = "PRT - stratum 22 : Norte - Ave"
		"PRT0023" = "PRT - stratum 23 : Norte - Cávado"
		"PRT0024" = "PRT - stratum 24 : Norte - Douro"
		"PRT0025" = "PRT - stratum 25 : Norte - Entre Douro e Vouga"
		"PRT0026" = "PRT - stratum 26 : Norte - Grande Porto"
		"PRT0027" = "PRT - stratum 27 : Norte - Minho Lima"
		"PRT0028" = "PRT - stratum 28 : Norte - Tâmega"
		"PRT0029" = "PRT - stratum 29 : Região Autónoma da Madeira    "
		"PRT0030" = "PRT - stratum 30 : Região Autónoma dos Açores"
		"QAT0001" = "QAT - stratum 01 : 1 – Independent"
		"QAT0002" = "QAT - stratum 02 : 2 – Independent complex"
		"QAT0003" = "QAT - stratum 03 : 3 – Community"
		"QAT0004" = "QAT - stratum 04 : 4 – International"
		"QAT0005" = "QAT - stratum 05 : 5 – Private"
		"QAT0006" = "QAT - stratum 06 : 6 – Private complex"
		"QCN0001" = "QCN - stratum 01 : ISCED2/General/no selectivity"
		"QCN0002" = "QCN - stratum 02 : Mixed ISCED/General/no selectivity"
		"QCN0003" = "QCN - stratum 03 : ISCED3/General/model"
		"QCN0004" = "QCN - stratum 04 : ISCED3/General/ordinary"
		"QCN0005" = "QCN - stratum 05 : Vocational/no selectivity"
		"QRS0059" = "QRS - stratum 59 : Perm Territory"
		"GBR2001" = "QSC - stratum 01 : Scotland 1-public/lowest band"
		"GBR2002" = "QSC - stratum 02 : Scotland 2-public/second lowest band"
		"GBR2003" = "QSC - stratum 03 : Scotland 3-public/middle band"
		"GBR2004" = "QSC - stratum 04 : Scotland 4-public/second highest band"
		"GBR2005" = "QSC - stratum 05 : Scotland 5-public/highest band"
		"GBR2006" = "QSC - stratum 06 : Scotland 6-public/missing"
		"GBR2007" = "QSC - stratum 07 : Scotland 7-independent/lowest band"
		"GBR2008" = "QSC - stratum 08 : Scotland 8-independent/second lowest band"
		"GBR2009" = "QSC - stratum 09 : Scotland 9-independent/middle band"
		"GBR2010" = "QSC - stratum 10 : Scotland 10-independent/second highest band"
		"GBR2011" = "QSC - stratum 11 : Scotland 11-independent/highest band"
		"GBR2012" = "QSC - stratum 12 : Scotland 12-independent/missing"
		"GBR1101" = "QUK - stratum 01 : England, Maintained selective, North"
		"GBR1102" = "QUK - stratum 02 : England, Maintained selective, Midlands"
		"GBR1103" = "QUK - stratum 03 : England, Maintained selective, South"
		"GBR1104" = "QUK - stratum 04 : England, Maintained selective, Greater London"
		"GBR1105" = "QUK - stratum 05 : England, Maintained non-selective, North"
		"GBR1106" = "QUK - stratum 06 : England, Maintained non-selective, Midlands"
		"GBR1107" = "QUK - stratum 07 : England, Maintained non-selective, South"
		"GBR1108" = "QUK - stratum 08 : England, Maintained non-selective, Greater London"
		"GBR1109" = "QUK - stratum 09 : England, Independent, North"
		"GBR1110" = "QUK - stratum 10 : England, Independent, Midlands"
		"GBR1111" = "QUK - stratum 11 : England, Independent, South"
		"GBR1112" = "QUK - stratum 12 : England, Independent, Greater London"
		"GBR1313" = "QUK - stratum 13 : Wales, Maintained, North"
		"GBR1314" = "QUK - stratum 14 : Wales, Maintained, Powys and South West"
		"GBR1315" = "QUK - stratum 15 : Wales, Maintained, South East"
		"GBR1316" = "QUK - stratum 16 : Wales, Independent, North"
		"GBR1317" = "QUK - stratum 17 : Wales, Independent, Powys and South West"
		"GBR1318" = "QUK - stratum 18 : Wales, Independent, South East"
		"GBR1219" = "QUK - stratum 19 : Northern Ireland, Maintained selective, Belfast"
		"GBR1220" = "QUK - stratum 20 : Northern Ireland, Maintained selective, Western"
		"GBR1221" = "QUK - stratum 21 : Northern Ireland, Maintained selective, North Eastern"
		"GBR1222" = "QUK - stratum 22 : Northern Ireland, Maintained selective, South Eastern"
		"GBR1223" = "QUK - stratum 23 : Northern Ireland, Maintained selective, Southern"
		"GBR1224" = "QUK - stratum 24 : Northern Ireland, Maintained non-selective, Belfast"
		"GBR1225" = "QUK - stratum 25 : Northern Ireland, Maintained non-selective, Western"
		"GBR1226" = "QUK - stratum 26 : Northern Ireland, Maintained non-selective, North Eastern"
		"GBR1227" = "QUK - stratum 27 : Northern Ireland, Maintained non-selective, South Eastern"
		"GBR1228" = "QUK - stratum 28 : Northern Ireland, Maintained non-selective, Southern"
		"GBR1229" = "QUK - stratum 29 : Northern Ireland, Independent"
		"ROU0001" = "ROU - stratum 01 : (1) GIM"
		"ROU0002" = "ROU - stratum 02 : (2) LIC "
		"RUS0003" = "RUS - stratum 03 : Republic of Bashkortostan"
		"RUS0004" = "RUS - stratum 04 : Republic of Buryatia"
		"RUS0005" = "RUS - stratum 05 : Republic of Dagestan"
		"RUS0007" = "RUS - stratum 07 : Kabardino-Balkarian Republic"
		"RUS0009" = "RUS - stratum 09 : Karachaevo-Chercessian Republic "
		"RUS0012" = "RUS - stratum 12 : Republic of Marij El"
		"RUS0016" = "RUS - stratum 16 : Republic of Tatarstan"
		"RUS0021" = "RUS - stratum 21 : Chuvashi Republic "
		"RUS0022" = "RUS - stratum 22 : Altai territory "
		"RUS0023" = "RUS - stratum 23 : Krasnodar territory "
		"RUS0024" = "RUS - stratum 24 : Krasnoyarsk territory "
		"RUS0025" = "RUS - stratum 25 : Primorsky territory "
		"RUS0026" = "RUS - stratum 26 : Stavropol territory "
		"RUS0027" = "RUS - stratum 27 : Khabarovsk territory "
		"RUS0029" = "RUS - stratum 29 : Arkhangelsk region "
		"RUS0032" = "RUS - stratum 32 : Bryansk region "
		"RUS0034" = "RUS - stratum 34 : Volgograd region "
		"RUS0036" = "RUS - stratum 36 : Voronezh region "
		"RUS0038" = "RUS - stratum 38 : Irkutsk region "
		"RUS0039" = "RUS - stratum 39 : Kaliningrad region "
		"RUS0040" = "RUS - stratum 40 : Kaluga region "
		"RUS0042" = "RUS - stratum 42 : Kemerovo region "
		"RUS0043" = "RUS - stratum 43 : Kirov region "
		"RUS0048" = "RUS - stratum 48 : Lipetzk region "
		"RUS0050" = "RUS - stratum 50 : Moscow region "
		"RUS0052" = "RUS - stratum 52 : Nizhni Novgorod region "
		"RUS0053" = "RUS - stratum 53 : Novgorod region "
		"RUS0054" = "RUS - stratum 54 : Novosibirsk region "
		"RUS0055" = "RUS - stratum 55 : Omsk region "
		"RUS0056" = "RUS - stratum 56 : Orenburg region "
		"RUS0059" = "RUS - stratum 59 : Perm territory "
		"RUS0061" = "RUS - stratum 61 : Rostov region "
		"RUS0062" = "RUS - stratum 62 : Ryazan region "
		"RUS0063" = "RUS - stratum 63 : Samara region "
		"RUS0064" = "RUS - stratum 64 : Saratov region "
		"RUS0066" = "RUS - stratum 66 : Sverdlovsk region "
		"RUS0071" = "RUS - stratum 71 : Tula region "
		"RUS0072" = "RUS - stratum 72 : Tyumen region "
		"RUS0074" = "RUS - stratum 74 : Chelyabinsk region "
		"RUS0077" = "RUS - stratum 77 : The City of Moscow"
		"RUS0078" = "RUS - stratum 78 : The City of Sankt-Petersburg"
		"RUS0079" = "RUS - stratum 79 : Jewish autonomous region"
		"SGP0101" = "SGP - stratum 01 : Public Secondary"
		"SGP0201" = "SGP - stratum 01 : Public Secondary"
		"SGP0203" = "SGP - stratum 03 : Private Secondary "
		"SRB0001" = "SRB - stratum 01 : Prim"
		"SRB0002" = "SRB - stratum 02 : Voj Gym"
		"SRB0003" = "SRB - stratum 03 : Voj Tech"
		"SRB0004" = "SRB - stratum 04 : Voj Oth"
		"SRB0005" = "SRB - stratum 05 : Bel Gym"
		"SRB0006" = "SRB - stratum 06 : Bel Tech"
		"SRB0007" = "SRB - stratum 07 : Bel Oth"
		"SRB0008" = "SRB - stratum 08 : West Gym"
		"SRB0009" = "SRB - stratum 09 : West Tech"
		"SRB0010" = "SRB - stratum 10 : West Oth"
		"SRB0011" = "SRB - stratum 11 : East Gym"
		"SRB0012" = "SRB - stratum 12 : East Tech"
		"SRB0013" = "SRB - stratum 13 : East Oth"
		"SRB0014" = "SRB - stratum 14 : South Gym"
		"SRB0015" = "SRB - stratum 15 : South Tech"
		"SRB0016" = "SRB - stratum 16 : South Oth"
		"SVK0001" = "SVK - stratum 01 : Region 1 / ZS"
		"SVK0002" = "SVK - stratum 02 : Region 1 / GYM"
		"SVK0003" = "SVK - stratum 03 : Region 1 / others"
		"SVK0004" = "SVK - stratum 04 : Region 2 / ZS"
		"SVK0005" = "SVK - stratum 05 : Region 2 / GYM"
		"SVK0006" = "SVK - stratum 06 : Region 2 / others"
		"SVK0007" = "SVK - stratum 07 : Region 3 / ZS"
		"SVK0008" = "SVK - stratum 08 : Region 3 / GYM"
		"SVK0009" = "SVK - stratum 09 : Region 3 / others"
		"SVK0010" = "SVK - stratum 10 : Region 4 / ZS"
		"SVK0011" = "SVK - stratum 11 : Region 4 / GYM"
		"SVK0012" = "SVK - stratum 12 : Region 4 / others"
		"SVK0013" = "SVK - stratum 13 : Region 5 / ZS"
		"SVK0014" = "SVK - stratum 14 : Region 5 / GYM"
		"SVK0015" = "SVK - stratum 15 : Region 5 / others"
		"SVK0016" = "SVK - stratum 16 : Region 6 / ZS"
		"SVK0017" = "SVK - stratum 17 : Region 6 / GYM"
		"SVK0018" = "SVK - stratum 18 : Region 6 / others"
		"SVK0019" = "SVK - stratum 19 : Region 7 / ZS"
		"SVK0020" = "SVK - stratum 20 : Region 7 / GYM"
		"SVK0021" = "SVK - stratum 21 : Region 7 / others"
		"SVK0022" = "SVK - stratum 22 : Region 8 / ZS"
		"SVK0023" = "SVK - stratum 23 : Region 8 / GYM"
		"SVK0024" = "SVK - stratum 24 : Region 8 / others"
		"SVN0006" = "SVN - stratum 00 : OS - elementary school"
		"SVN0007" = "SVN - stratum 00 : IO-adult educational programme"
		"SVN0101" = "SVN - stratum 01 : GIMg - gymnasia general"
		"SVN0102" = "SVN - stratum 02 : GIMs - gymnasia specialist"
		"SVN0103" = "SVN - stratum 03 : STSI - technical educational programmes"
		"SVN0104" = "SVN - stratum 04 : SPI - vocational of medium duration"
		"SVN0105" = "SVN - stratum 05 : NPI - vocational of short duration"
		"SVN0201" = "SVN - stratum 01 : GIMg - gymnasia general"
		"SVN0202" = "SVN - stratum 02 : GIMs - gymnasia specialist"
		"SVN0203" = "SVN - stratum 03 : STSI - technical educational programmes"
		"SVN0204" = "SVN - stratum 04 : SPI - vocational of medium duration"
		"SVN0205" = "SVN - stratum 05 : NPI - vocational of short duration"
		"SVN0301" = "SVN - stratum 01 : GIMg - gymnasia general"
		"SVN0302" = "SVN - stratum 02 : GIMs - gymnasia specialist"
		"SVN0303" = "SVN - stratum 03 : STSI - technical educational programmes"
		"SVN0304" = "SVN - stratum 04 : SPI - vocational of medium duration"
		"SVN0305" = "SVN - stratum 05 : NPI - vocational of short duration"
		"SVN0401" = "SVN - stratum 01 : GIMg - gymnasia general"
		"SVN0403" = "SVN - stratum 03 : STSI - technical educational programmes"
		"SVN0404" = "SVN - stratum 04 : SPI - vocational of medium duration"
		"SVN0405" = "SVN - stratum 05 : NPI - vocational of short duration"
		"SVN0501" = "SVN - stratum 01 : GIMg - gymnasia general"
		"SVN0503" = "SVN - stratum 03 : STSI - technical educational programmes"
		"SVN0504" = "SVN - stratum 04 : SPI - vocational of medium duration"
		"SVN0505" = "SVN - stratum 05 : NPI - vocational of short duration"
		"SVN0601" = "SVN - stratum 01 : GIMg - gymnasia general"
		"SVN0602" = "SVN - stratum 02 : GIMs - gymnasia specialist"
		"SVN0603" = "SVN - stratum 03 : STSI - technical educational programmes"
		"SVN0604" = "SVN - stratum 04 : SPI - vocational of medium duration"
		"SVN0605" = "SVN - stratum 05 : NPI - vocational of short duration"
		"SVN0701" = "SVN - stratum 01 : GIMg - gymnasia general"
		"SVN0702" = "SVN - stratum 02 : GIMs - gymnasia specialist"
		"SVN0703" = "SVN - stratum 03 : STSI - technical educational programmes"
		"SVN0704" = "SVN - stratum 04 : SPI - vocational of medium duration"
		"SVN0705" = "SVN - stratum 05 : NPI - vocational of short duration"
		"SVN0801" = "SVN - stratum 01 : GIMg - gymnasia general"
		"SVN0802" = "SVN - stratum 02 : GIMs - gymnasia specialist"
		"SVN0803" = "SVN - stratum 03 : STSI - technical educational programmes"
		"SVN0804" = "SVN - stratum 04 : SPI - vocational of medium duration"
		"SVN0805" = "SVN - stratum 05 : NPI - vocational of short duration"
		"SVN0901" = "SVN - stratum 01 : GIMg - gymnasia general"
		"SVN0902" = "SVN - stratum 02 : GIMs - gymnasia specialist"
		"SVN0903" = "SVN - stratum 03 : STSI - technical educational programmes"
		"SVN0904" = "SVN - stratum 04 : SPI - vocational of medium duration"
		"SVN0905" = "SVN - stratum 05 : NPI - vocational of short duration"
		"SVN1001" = "SVN - stratum 01 : GIMg - gymnasia general"
		"SVN1002" = "SVN - stratum 02 : GIMs - gymnasia specialist"
		"SVN1003" = "SVN - stratum 03 : STSI - technical educational programmes"
		"SVN1004" = "SVN - stratum 04 : SPI - vocational of medium duration"
		"SVN1005" = "SVN - stratum 05 : NPI - vocational of short duration"
		"SVN1101" = "SVN - stratum 01 : GIMg - gymnasia general"
		"SVN1102" = "SVN - stratum 02 : GIMs - gymnasia specialist"
		"SVN1103" = "SVN - stratum 03 : STSI - technical educational programmes"
		"SVN1104" = "SVN - stratum 04 : SPI - vocational of medium duration"
		"SVN1105" = "SVN - stratum 05 : NPI - vocational of short duration"
		"SVN1201" = "SVN - stratum 01 : GIMg - gymnasia general"
		"SVN1203" = "SVN - stratum 03 : STSI - technical educational programmes"
		"SVN1204" = "SVN - stratum 04 : SPI - vocational of medium duration"
		"SVN1205" = "SVN - stratum 05 : NPI - vocational of short duration"
		"SWE0001" = "SWE - stratum 01 : PUB/ LOWER SEC/ METRO"
		"SWE0002" = "SWE - stratum 02 : PUB/ LOWER SEC/ OTHER LARGE"
		"SWE0003" = "SWE - stratum 03 : PUB/ LOWER SEC/ AT LEAST 12500"
		"SWE0004" = "SWE - stratum 04 : PUB/ LOWER SEC/ MANUFACTURING"
		"SWE0005" = "SWE - stratum 05 : PUB/ LOWER SEC/ SPARSE POP"
		"SWE0006" = "SWE - stratum 06 : PUB/ UPPER SEC"
		"SWE0007" = "SWE - stratum 07 : INDEP/ LOWER SEC/ METRO"
		"SWE0008" = "SWE - stratum 08 : INDEP/ LOWER SEC/ OTHER LARGE"
		"SWE0009" = "SWE - stratum 09 : INDEP/ LOWER SEC/ AT LEAST 12500"
		"SWE0010" = "SWE - stratum 10 : INDEP/ LOWER SEC/ MANUFACTURING"
		"SWE0011" = "SWE - stratum 11 : INDEP/ LOWER SEC/ SPARSE POP"
		"SWE0012" = "SWE - stratum 12 : INDEP/ UPPER SEC"
		"TAP0101" = "TAP - stratum 01 : Gen. Jr. HS. Pub. Rur."
		"TAP0102" = "TAP - stratum 02 : Gen. Jr. HS. Pub. Urb."
		"TAP0103" = "TAP - stratum 03 : Gen. Jr. HS. Prv. Rur."
		"TAP0104" = "TAP - stratum 04 : Gen. Jr. HS. Prv. Urb."
		"TAP0105" = "TAP - stratum 05 : Gen. Sr. HS. Pub. Rur."
		"TAP0106" = "TAP - stratum 06 : Gen. Sr. HS. Pub. Urb."
		"TAP0107" = "TAP - stratum 07 : Gen. Sr. HS. Prv. Rur."
		"TAP0108" = "TAP - stratum 08 : Gen. Sr. HS. Prv. Urb."
		"TAP0109" = "TAP - stratum 09 : Voc. Sr. HS. Pub. Rur."
		"TAP0110" = "TAP - stratum 10 : Voc. Sr. HS. Pub. Urb."
		"TAP0111" = "TAP - stratum 11 : Voc. Sr. HS. Prv. Rur."
		"TAP0112" = "TAP - stratum 12 : Voc. Sr. HS. Prv. Urb."
		"TAP0213" = "TAP - stratum 13 : Mix. Sr. HS. Pub. Rur."
		"TAP0214" = "TAP - stratum 14 : Mix. Sr. HS. Pub. Urb."
		"TAP0215" = "TAP - stratum 15 : Mix. Sr. HS. Prv. Rur."
		"TAP0216" = "TAP - stratum 16 : Mix. Sr. HS. Prv. Urb."
		"TAP0317" = "TAP - stratum 17 : Comh. HS. Pub. Rur."
		"TAP0318" = "TAP - stratum 18 : Comh. HS. Pub. Urb."
		"TAP0319" = "TAP - stratum 19 : Comh. HS. Prv. Rur."
		"TAP0320" = "TAP - stratum 20 : Comh. HS. Prv. Urb."
		"TAP0321" = "TAP - stratum 21 : Cmpx. HS. Pub. Rur."
		"TAP0322" = "TAP - stratum 22 : Cmpx. HS. Pub. Urb."
		"TAP0323" = "TAP - stratum 23 : Cmpx. HS. Prv. Rur."
		"TAP0324" = "TAP - stratum 24 : Cmpx. HS. Prv. Urb."
		"TAP0325" = "TAP - stratum 25 : FY Coll. Pub. Rur."
		"TAP0326" = "TAP - stratum 26 : FY Coll. Pub. Urb."
		"TAP0327" = "TAP - stratum 27 : FY Coll. Prv. Rur."
		"TAP0328" = "TAP - stratum 28 : FY Coll. Prv. Urb."
		"THA0101" = "THA - stratum 01 : Office of the Basic Education Commission/Lower secondary"
		"THA0102" = "THA - stratum 02 : Office of the Basic Education Commission/Mixed"
		"THA0103" = "THA - stratum 03 : Office of the Basic Education Commission/Upper secondary"
		"THA0204" = "THA - stratum 04 : Office of the Private Education Commission/Lower Secondary"
		"THA0205" = "THA - stratum 05 : Office of the Private Education Commission/Mixed "
		"THA0206" = "THA - stratum 06 : Office of the Private Education Commission/Upper Secondary"
		"THA0207" = "THA - stratum 07 : Department of Education of Bangkok Metropolitan Administration/Lower secondary"
		"THA0208" = "THA - stratum 08 : Department of Education of Bangkok Metropolitan Administration/Mixed"
		"THA0209" = "THA - stratum 09 : Bureau of Local Educational Development and Co-ordination/ Lower Secondary"
		"THA0210" = "THA - stratum 10 : Bureau of Local Educational Development and Co-ordination/Mixed"
		"THA0211" = "THA - stratum 11 : Bureau of Local Educational Development and Co-ordination/Upper Secondary"
		"THA0212" = "THA - stratum 12 : Commission of Higher Education/Lower secondary"
		"THA0213" = "THA - stratum 13 : Commission of Higher Education/Mixed"
		"THA0214" = "THA - stratum 14 : Vocational Education Commission/Upper Secondary"
		"THA0215" = "THA - stratum 15 : Special schools with Math/Science focus/Mixed"	
		"TUN0001" = "TUN - stratum 01 : North-east/Urban"
		"TUN0002" = "TUN - stratum 02 : North-east/Periphery"
		"TUN0003" = "TUN - stratum 03 : North-east/Rural"
		"TUN0004" = "TUN - stratum 04 : North-west/Urban"
		"TUN0005" = "TUN - stratum 05 : North-west/Periphery"
		"TUN0006" = "TUN - stratum 06 : North-west/Rural"
		"TUN0007" = "TUN - stratum 07 : South-east/Urban"
		"TUN0008" = "TUN - stratum 08 : South-east/Periphery"
		"TUN0009" = "TUN - stratum 09 : South-east/Rural"
		"TUN0010" = "TUN - stratum 10 : South-west/Urban"
		"TUN0011" = "TUN - stratum 11 : South-west/Periphery"
		"TUN0012" = "TUN - stratum 12 : South-west/Rural"
		"TUN0013" = "TUN - stratum 13 : Center-east/Urban"
		"TUN0014" = "TUN - stratum 14 : Center-east/Periphery"
		"TUN0015" = "TUN - stratum 15 : Center-east/Rural"
		"TUN0016" = "TUN - stratum 16 : Center-west/Urban"
		"TUN0017" = "TUN - stratum 17 : Center-west/Periphery"
		"TUN0018" = "TUN - stratum 18 : Center-west/Rural"
		"TUR0001" = "TUR - stratum 01 : TR1 PRIMARY"
		"TUR0002" = "TUR - stratum 02 : TR1 GENERAL SECONDARY"
		"TUR0003" = "TUR - stratum 03 : TR1 VOCATIONAL AND TECHNICAL SECONDARY"
		"TUR0004" = "TUR - stratum 04 : TR2 PRIMARY"
		"TUR0005" = "TUR - stratum 05 : TR2 GENERAL SECONDARY"
		"TUR0006" = "TUR - stratum 06 : TR2 VOCATIONAL AND TECHNICAL SECONDARY"
		"TUR0007" = "TUR - stratum 07 : TR3 PRIMARY"
		"TUR0008" = "TUR - stratum 08 : TR3 GENERAL SECONDARY"
		"TUR0009" = "TUR - stratum 09 : TR3 VOCATIONAL AND TECHNICAL SECONDARY"
		"TUR0010" = "TUR - stratum 10 : TR4 PRIMARY"
		"TUR0011" = "TUR - stratum 11 : TR4 GENERAL SECONDARY"
		"TUR0012" = "TUR - stratum 12 : TR4 VOCATIONAL AND TECHNICAL SECONDARY"
		"TUR0013" = "TUR - stratum 13 : TR4 POLICE EDUCATION"
		"TUR0014" = "TUR - stratum 14 : TR5 PRIMARY"
		"TUR0015" = "TUR - stratum 15 : TR5 GENERAL SECONDARY"
		"TUR0016" = "TUR - stratum 16 : TR5 VOCATIONAL AND TECHNICAL SECONDARY"
		"TUR0017" = "TUR - stratum 17 : TR5 POLICE EDUCATION"
		"TUR0018" = "TUR - stratum 18 : TR6 PRIMARY"
		"TUR0019" = "TUR - stratum 19 : TR6 GENERAL SECONDARY"
		"TUR0020" = "TUR - stratum 20 : TR6 VOCATIONAL AND TECHNICAL SECONDARY"
		"TUR0021" = "TUR - stratum 21 : TR7 PRIMARY"
		"TUR0022" = "TUR - stratum 22 : TR7 GENERAL SECONDARY"
		"TUR0023" = "TUR - stratum 23 : TR7 VOCATIONAL AND TECHNICAL SECONDARY"
		"TUR0024" = "TUR - stratum 24 : TR8 PRIMARY"
		"TUR0025" = "TUR - stratum 25 : TR8 GENERAL SECONDARY"
		"TUR0026" = "TUR - stratum 26 : TR8 VOCATIONAL AND TECHNICAL SECONDARY"
		"TUR0027" = "TUR - stratum 27 : TR9 PRIMARY"
		"TUR0028" = "TUR - stratum 28 : TR9 GENERAL SECONDARY"
		"TUR0029" = "TUR - stratum 29 : TR9 VOCATIONAL AND TECHNICAL SECONDARY"
		"TUR0030" = "TUR - stratum 30 : TRA PRIMARY"
		"TUR0031" = "TUR - stratum 31 : TRA GENERAL SECONDARY"
		"TUR0032" = "TUR - stratum 32 : TRA VOCATIONAL AND TECHNICAL SECONDARY"
		"TUR0033" = "TUR - stratum 33 : TRB PRIMARY"
		"TUR0034" = "TUR - stratum 34 : TRB GENERAL SECONDARY"
		"TUR0035" = "TUR - stratum 35 : TRB VOCATIONAL AND TECHNICAL SECONDARY"
		"TUR0036" = "TUR - stratum 36 : TRC PRIMARY"
		"TUR0037" = "TUR - stratum 37 : TRC GENERAL SECONDARY"
		"TUR0038" = "TUR - stratum 38 : TRC VOCATIONAL AND TECHNICAL SECONDARY"
		"URY0001" = "URY - stratum 01 : Public General Secondary Schools  and Military School,Lower secondary only"
		"URY0002" = "URY - stratum 02 : Public General Secondary Schools  and Military School,Mixed"
		"URY0003" = "URY - stratum 03 : Public General Secondary Schools  and Military School,Upper secondary only"
		"URY0004" = "URY - stratum 04 : Private General Secondary Schools,Lower secondary only"
		"URY0005" = "URY - stratum 05 : Private General Secondary Schools,Mixed"
		"URY0006" = "URY - stratum 06 : Private General Secondary Schools,Upper secondary only"
		"URY0007" = "URY - stratum 07 : Public Technical Secondary Schools,Lower secondary only"
		"URY0008" = "URY - stratum 08 : Public Technical Secondary Schools,Mixed"
		"URY0009" = "URY - stratum 09 : Public Technical Secondary Schools,Upper secondary only"
		"URY0010" = "URY - stratum 10 : Rural Schools with Lower Secondary Degree,Lower secondary only"
		"USA1001" = "USA - stratum 01 : Northeast-Private"
		"USA1002" = "USA - stratum 02 : Northeast-Public"
		"USA1003" = "USA - stratum 03 : Midwest-Private"
		"USA1004" = "USA - stratum 04 : Midwest-Public"
		"USA1104" = "USA - stratum 04 : Midwest-Public"
		"USA1105" = "USA - stratum 05 : South-Private"
		"USA1106" = "USA - stratum 06 : South-Public"
		"USA1206" = "USA - stratum 06 : South-Public"
		"USA1207" = "USA - stratum 07 : West-Private"
		"USA1208" = "USA - stratum 08 : West-Public"
		"QUA0112" = "Florida (USA) - Stratum 12"
		"QUB0209" = "Connecticut (USA) - Stratum 09"
		"QUC0325" = "Massachusetts (USA) - Stratum 25"
		"QUD0472" = "Puerto Rico (USA) - Stratum 72"
		"VNM0101" = "VNM - stratum 01 : North Viet Nam / Public / Urban"
		"VNM0102" = "VNM - stratum 02 : North Viet Nam / Public / Rural"
		"VNM0103" = "VNM - stratum 03 : North Viet Nam / Public / Remote area"
		"VNM0104" = "VNM - stratum 04 : North Viet Nam / Private / Urban"
		"VNM0105" = "VNM - stratum 05 : North Viet Nam / Private / Rural"
		"VNM0106" = "VNM - stratum 06 : North Viet Nam / Private / Remote area"
		"VNM0207" = "VNM - stratum 07 : Central Viet Nam / Public / Urban"
		"VNM0208" = "VNM - stratum 08 : Central Viet Nam / Public / Rural"
		"VNM0209" = "VNM - stratum 09 : Central Viet Nam / Public / Remote area"
		"VNM0210" = "VNM - stratum 10 : Central Viet Nam / Private / Urban"
		"VNM0211" = "VNM - stratum 11 : Central Viet Nam / Private / Rural"
		"VNM0313" = "VNM - stratum 13 : Southern Viet Nam / Public / Urban"
		"VNM0314" = "VNM - stratum 14 : Southern Viet Nam / Public / Rural"
		"VNM0315" = "VNM - stratum 15 : Southern Viet Nam / Public / Remote area"
		"VNM0316" = "VNM - stratum 16 : Southern Viet Nam / Private / Urban"
		"VNM0317" = "VNM - stratum 17 : Southern Viet Nam / Private / Rural"
	  "ALB9797" = "Undisclosed STRATUM - Albania" 
		"ARG9797" = "Undisclosed STRATUM - Argentina" 
		"AUS9797" = "Undisclosed STRATUM - Australia" 
		"AUT9797" = "Undisclosed STRATUM - Austria" 
		"BEL9797" = "Undisclosed STRATUM - Belgium" 
		"BRA9797" = "Undisclosed STRATUM - Brazil" 
		"BGR9797" = "Undisclosed STRATUM - Bulgaria" 
		"CAN9797" = "Undisclosed STRATUM - Canada" 
		"CHL9797" = "Undisclosed STRATUM - Chile"  
		"QCN9797" = "Undisclosed STRATUM - Shanghai-China"  
		"TAP9797" = "Undisclosed STRATUM - Chinese Taipei"  
		"COL9797" = "Undisclosed STRATUM - Colombia"  
		"CRI9797" = "Undisclosed STRATUM - Costa Rica"
		"HRV9797" = "Undisclosed STRATUM - Croatia"
		"CZE9797" = "Undisclosed STRATUM - Czech Republic"  
		"DNK9797" = "Undisclosed STRATUM - Denmark"
		"EST9797" = "Undisclosed STRATUM - Estonia"
		"FIN9797" = "Undisclosed STRATUM - Finland"
		"FRA9797" = "Undisclosed STRATUM - France" 
		"DEU9797" = "Undisclosed STRATUM - Germany"
		"GRC9797" = "Undisclosed STRATUM - Greece" 
		"HKG9797" = "Undisclosed STRATUM - Hong Kong-China" 
		"HUN9797" = "Undisclosed STRATUM - Hungary"
		"ISL9797" = "Undisclosed STRATUM - Iceland"
		"IDN9797" = "Undisclosed STRATUM - Indonesia" 
		"IRL9797" = "Undisclosed STRATUM - Ireland"
		"ISR9797" = "Undisclosed STRATUM - Israel" 
		"ITA9797" = "Undisclosed STRATUM - Italy"  
		"JPN9797" = "Undisclosed STRATUM - Japan"  
		"JOR9797" = "Undisclosed STRATUM - Jordan" 
		"KAZ9797" = "Undisclosed STRATUM - Kazakhstan"
		"KOR9797" = "Undisclosed STRATUM - Korea"  
		"LVA9797" = "Undisclosed STRATUM - Latvia" 
		"LIE9797" = "Undisclosed STRATUM - Liechtenstein"
		"LTU9797" = "Undisclosed STRATUM - Lithuania" 
		"LUX9797" = "Undisclosed STRATUM - Luxembourg"
		"MAC9797" = "Undisclosed STRATUM - Macao-China"  
		"MYS9797" = "Undisclosed STRATUM - Malaysia"  
		"MEX9797" = "Undisclosed STRATUM - Mexico" 
		"MNE9797" = "Undisclosed STRATUM - Montenegro"
		"NLD9797" = "Undisclosed STRATUM - Netherlands"  
		"NZL9797" = "Undisclosed STRATUM - New Zealand"  
		"NOR9797" = "Undisclosed STRATUM - Norway" 
		"QRS9797" = "Undisclosed STRATUM - Perm(Russian Federation)" 
		"PER9797" = "Undisclosed STRATUM - Peru"
		"POL9797" = "Undisclosed STRATUM - Poland" 
		"PRT9797" = "Undisclosed STRATUM - Portugal"  
		"QAT9797" = "Undisclosed STRATUM - Qatar"  
		"ROU9797" = "Undisclosed STRATUM - Romania"
		"RUS9797" = "Undisclosed STRATUM - Russian Federation" 
		"SRB9797" = "Undisclosed STRATUM - Serbia" 
		"SGP9797" = "Undisclosed STRATUM - Singapore" 
		"SVK9797" = "Undisclosed STRATUM - Slovak Republic" 
		"SVN9797" = "Undisclosed STRATUM - Slovenia"  
		"ESP9797" = "Undisclosed STRATUM - Spain"  
		"SWE9797" = "Undisclosed STRATUM - Sweden" 
		"CHE9797" = "Undisclosed STRATUM - Switzerland"  
		"THA9797" = "Undisclosed STRATUM - Thailand"  
		"TUN9797" = "Undisclosed STRATUM - Tunisia"
		"TUR9797" = "Undisclosed STRATUM - Turkey" 
		"GBR9797" = "Undisclosed STRATUM - United Kingdom"  
		"ARE9797" = "Undisclosed STRATUM - United Arab Emirates"  
		"USA9797" = "Undisclosed STRATUM - United States of America" 
		"URY9797" = "Undisclosed STRATUM - Uruguay"
		"VNM9797" = "Undisclosed STRATUM - Viet Nam"
		"QUA9797" = "Undisclosed STRATUM - Florida (USA)"  
		"QUB9797" = "Undisclosed STRATUM - Connecticut (USA)" 
		"QUC9797" = "Undisclosed STRATUM - Massachusetts (USA)"  
		"QUD9797" = "Undisclosed STRATUM - Puerto Rico" 
;
value $SUBNATIOF
		"0080000" = "Albania"
		"7840000" = "UAE - Rest of the country (not adjudicated)"
		"7840100" = "UAE - Abu Dhabi"
		"7840200" = "UAE - Dubai"
		"0320000" = "Argentina - Rest of the country (not adjudicated)"
		"0320100" = "Argentina - CABA"
		"0360000" = "Australia"
		"0400000" = "Austria"
		"0560000" = "Belgium - French & German regions (not adjudicated)"
		"0560100" = "Belgium - Flemish Community"
		"1000000" = "Bulgaria"
		"0760000" = "Brazil"
		"1240000" = "Canada"
		"7560000" = "Switzerland"
		"1520000" = "Chile"
		"1700000" = "Colombia"
		"1880000" = "Costa Rica"
		"2030000" = "Czech Republic"
		"2760000" = "Germany"
		"2080000" = "Denmark"
		"7240000" = "Spain - Rest of the country (not adjudicated)"
		"7240100" = "Spain - Andalusia"
		"7241600" = "Spain - Basque Country"
		"7240600" = "Spain - Cantabria"
		"7241300" = "Spain - Madrid"
		"7241000" = "Spain - Extremadura"
		"7241100" = "Spain - Galicia"
		"7240400" = "Spain - Balearic Islands"
		"7241200" = "Spain - La Rioja"
		"7240700" = "Spain - Castile and Leon"
		"7241500" = "Spain - Navarre"
		"7240200" = "Spain - Aragon"
		"7240900" = "Spain - Catalonia"
		"7240300" = "Spain - Asturias"
		"7241400" = "Spain - Murcia"
		"2330000" = "Estonia"
		"2460000" = "Finland"
		"2500000" = "France"
		"8260000" = "United Kingdom - England, Wales & Northern Ireland"
		"8262000" = "United Kingdom - Scotland"
		"3000000" = "Greece"
		"3440000" = "Hong Kong-China"
		"1910000" = "Croatia"
		"3480000" = "Hungary"
		"3600000" = "Indonesia"
		"3720000" = "Ireland"
		"3520000" = "Iceland"
		"3760000" = "Israel"
		"3800000" = "Italy"
		"4000000" = "Jordan"
		"3920000" = "Japan"
		"3980000" = "Kazakhstan"
		"4100000" = "Korea"
		"4380000" = "Liechtenstein"
		"4400000" = "Lithuania"
		"4420000" = "Luxembourg"
		"4280000" = "Latvia"
		"4460000" = "Macao-China"
		"4840000" = "Mexico"
		"4990000" = "Montenegro"
		"4580000" = "Malaysia"
		"5280000" = "Netherlands"
		"5780000" = "Norway"
		"5540000" = "New Zealand"
		"6040000" = "Peru"
		"6160000" = "Poland"
		"6200000" = "Portugal"
		"6340000" = "Qatar"
		"1560000" = "Shanghai-China"
		"6430059" = "Russia - Perm Region"
		"6420000" = "Romania"
		"6430000" = "Russian Federation"
		"7020000" = "Singapore"
		"6880000" = "Serbia"
		"7030000" = "Slovak Republic"
		"7050000" = "Slovenia"
		"7520000" = "Sweden"
		"1580000" = "Chinese Taipei"
		"7640000" = "Thailand"
		"7880000" = "Tunisia"
		"7920000" = "Turkey"
		"8580000" = "Uruguay"
		"8400000" = "United States of America"
		"8400100" = "United States - Florida"
		"8400200" = "United States - Connecticut"
		"8400300" = "United States - Massachusetts"
		"7040000" = "Viet Nam"
;
value $NCF
		"000400" = "Afghanistan"
		"000800" = "Albania"
		"001200" = "Algeria"
		"001600" = "American Samoa "
		"002000" = "Andorra"
		"002400" = "Angola "
		"002800" = "Antigua and Barbuda"
		"002900" = "Caribbean"
		"003100" = "Azerbaijan "
		"003200" = "Argentina"
		"003600" = "Australia"
		"004000" = "Austria"
		"004400" = "Bahamas"
		"004800" = "Bahrain"
		"005000" = "Bangladesh "
		"005100" = "Armenia"
		"005200" = "Barbados "
		"005600" = "Belgium"
		"005601" = "Belgium (Flanders) "
		"005602" = "Belgium (French community) "
		"005603" = "Belgium (German community) "
		"006000" = "Bermuda"
		"006400" = "Bhutan "
		"006800" = "Bolivia (Plurinational State of) "
		"007000" = "Bosnia and Herzegovina "
		"007200" = "Botswana "
		"007600" = "Brazil "
		"008400" = "Belize "
		"009200" = "British Virgin Islands "
		"009600" = "Brunei Darussalam"
		"010000" = "Bulgaria "
		"010400" = "Myanmar"
		"010800" = "Burundi"
		"011200" = "Belarus"
		"011600" = "Cambodia "
		"012000" = "Cameroon "
		"012400" = "Canada "
		"013200" = "Cape Verde "
		"013600" = "Cayman Islands "
		"014000" = "Central African Republic "
		"014400" = "Sri Lanka"
		"014800" = "Chad "
		"015200" = "Chile"
		"015600" = "China"
		"015601" = "China (Shanghai) "
		"015604" = "China (Beijing)"
		"015610" = "China (incl. Hong Kong)"
		"015800" = "Chinese Taipei "
		"017000" = "Colombia "
		"017400" = "Comoros"
		"017500" = "Mayotte"
		"017800" = "Congo"
		"018000" = "Democratic Republic of the Congo "
		"018400" = "Cook Islands "
		"018800" = "Costa Rica "
		"019100" = "Croatia"
		"019200" = "Cuba "
		"020300" = "Czech Republic "
		"020400" = "Benin"
		"020800" = "Denmark"
		"021200" = "Dominica "
		"021400" = "Dominican Republic "
		"021800" = "Ecuador"
		"022200" = "El Salvador"
		"022600" = "Equatorial Guinea"
		"023100" = "Ethiopia "
		"023200" = "Eritrea"
		"023300" = "Estonia"
		"023400" = "Faeroe Islands "
		"023800" = "Falkland Islands (Malvinas)"
		"024200" = "Fiji "
		"024600" = "Finland"
		"024800" = "Åland Islands"
		"025000" = "France "
		"025400" = "French Guiana"
		"025800" = "French Polynesia "
		"026200" = "Djibouti "
		"026600" = "Gabon"
		"026800" = "Georgia"
		"027000" = "Gambia "
		"027500" = "Occupied Palestinian Territory "
		"027600" = "Germany"
		"028800" = "Ghana"
		"029200" = "Gibraltar"
		"029600" = "Kiribati "
		"030000" = "Greece "
		"030400" = "Greenland"
		"030800" = "Grenada"
		"031200" = "Guadeloupe "
		"031600" = "Guam "
		"032000" = "Guatemala"
		"032400" = "Guinea "
		"032800" = "Guyana "
		"033200" = "Haiti"
		"033600" = "Holy See "
		"034000" = "Honduras "
		"034400" = "Hong Kong-China"
		"034800" = "Hungary"
		"035200" = "Iceland"
		"035600" = "India"
		"035610" = "India (2009 States)"
		"036000" = "Indonesia"
		"036400" = "Iran, Islamic Republic of"
		"036800" = "Iraq "
		"037200" = "Ireland"
		"037600" = "Israel "
		"038000" = "Italy"
		"038001" = "Provincia Autonoma of Bolzano (ITA)"
		"038002" = "Provincia Basilicata (ITA) "
		"038003" = "Provincia Campania (ITA) "
		"038004" = "Provincia Emilia Romagna (ITA) "
		"038005" = "Provincia Friuli Venezia Giulia (ITA)"
		"038006" = "Provincia Liguria (ITA)"
		"038007" = "Provincia Lombardia (ITA)"
		"038008" = "Provincia Piemonte (ITA) "
		"038009" = "Provincia Puglia (ITA) "
		"038010" = "Provincia Sardegna (ITA) "
		"038011" = "Provincia Sicilia (ITA)"
		"038012" = "Provincia Trento (ITA) "
		"038013" = "Provincia Veneto (ITA) "
		"038014" = "The rest of Italy (ITA)"
		"038400" = "Côte d'Ivoire"
		"038800" = "Jamaica"
		"039200" = "Japan"
		"039800" = "Kazakhstan "
		"040000" = "Jordan "
		"040400" = "Kenya"
		"040800" = "Democratic People's Republic of Korea"
		"041000" = "Republic of Korea"
		"041400" = "Kuwait "
		"041700" = "Kyrgyzstan "
		"041800" = "Lao People's Democratic Republic "
		"042200" = "Lebanon"
		"042600" = "Lesotho"
		"042800" = "Latvia "
		"043000" = "Liberia"
		"043400" = "Libyan Arab Jamahiriya "
		"043800" = "Liechtenstein"
		"044000" = "Lithuania"
		"044200" = "Luxembourg "
		"044600" = "Macao-China"
		"045000" = "Madagascar "
		"045400" = "Malawi "
		"045800" = "Malaysia "
		"046200" = "Maldives "
		"046600" = "Mali "
		"047000" = "Malta"
		"047400" = "Martinique "
		"047800" = "Mauritania "
		"048000" = "Mauritius"
		"048400" = "Mexico "
		"049200" = "Monaco "
		"049600" = "Mongolia "
		"049800" = "Republic of Moldova"
		"049900" = "Montenegro "
		"050000" = "Montserrat "
		"050400" = "Morocco"
		"050800" = "Mozambique "
		"051200" = "Oman "
		"051600" = "Namibia"
		"052000" = "Nauru"
		"052400" = "Nepal"
		"052800" = "Netherlands"
		"053000" = "Netherlands Antilles "
		"053300" = "Aruba"
		"054000" = "New Caledonia"
		"054800" = "Vanuatu"
		"055400" = "New Zealand"
		"055800" = "Nicaragua"
		"056200" = "Niger"
		"056600" = "Nigeria"
		"057000" = "Niue "
		"057400" = "Norfolk Island "
		"057800" = "Norway "
		"058000" = "Northern Mariana Islands "
		"058300" = "Micronesia, Federated States of"
		"058400" = "Marshall Islands "
		"058500" = "Palau"
		"058600" = "Pakistan "
		"059100" = "Panama "
		"059800" = "Papua New Guinea "
		"060000" = "Paraguay "
		"060400" = "Peru "
		"060800" = "Philippines"
		"061200" = "Pitcairn "
		"061600" = "Poland "
		"062000" = "Portugal "
		"062400" = "Guinea-Bissau"
		"062600" = "Timor-Leste"
		"063000" = "Puerto Rico"
		"063400" = "Qatar"
		"063800" = "Réunion"
		"064200" = "Romania"
		"064300" = "Russian Federation "
		"064600" = "Rwanda "
		"065200" = "Saint-Barthélemy "
		"065400" = "Saint Helena "
		"065900" = "Saint Kitts and Nevis"
		"066000" = "Anguilla "
		"066200" = "Saint Lucia"
		"066300" = "Saint-Martin (French part) "
		"066600" = "Saint Pierre and Miquelon"
		"067000" = "Saint Vincent and the Grenadines "
		"067400" = "San Marino "
		"067800" = "Sao Tome and Principe"
		"068200" = "Saudi Arabia "
		"068600" = "Senegal"
		"068800" = "Serbia "
		"069000" = "Seychelles "
		"069400" = "Sierra Leone "
		"070200" = "Singapore"
		"070300" = "Slovak Republic"
		"070400" = "Viet Nam "
		"070500" = "Slovenia "
		"070600" = "Somalia"
		"071000" = "South Africa "
		"071600" = "Zimbabwe "
		"072400" = "Spain"
		"072401" = "Andalusia (ESP)"
		"072402" = "Aragon (ESP) "
		"072403" = "Asturias (ESP) "
		"072404" = "Balearic Islands (ESP) "
		"072405" = "Canary Islands (ESP) "
		"072406" = "Cantabria (ESP)"
		"072407" = "Castile-La Mancha (ESP)"
		"072408" = "Castile and Leon (ESP) "
		"072409" = "Catalonia (ESP)"
		"072410" = "Extremadura (ESP)"
		"072411" = "Galicia (ESP)"
		"072412" = "La Rioja (ESP) "
		"072413" = "Madrid (ESP) "
		"072414" = "Murcia (ESP) "
		"072415" = "Navarre (ESP)"
		"072416" = "Basque Country (ESP) "
		"072417" = "Valencian Community (ESP)"
		"072418" = "Ceuta and Melilla (ESP)"
		"072419" = "The rest of Spain (ESP)"
		"073200" = "Western Sahara "
		"073600" = "Sudan"
		"074000" = "Suriname "
		"074400" = "Svalbard and Jan Mayen Islands "
		"074800" = "Swaziland"
		"075200" = "Sweden "
		"075600" = "Switzerland"
		"075601" = "Switzerland (German-speaking community)"
		"075602" = "Switzerland (French-speaking community)"
		"075603" = "Switzerland (Italian-speaking community) "
		"076000" = "Syrian Arab Republic "
		"076200" = "Tajikistan "
		"076400" = "Thailand "
		"076800" = "Togo "
		"077200" = "Tokelau"
		"077600" = "Tonga"
		"078000" = "Trinidad and Tobago"
		"078400" = "United Arab Emirates "
		"078410" = "United Arab Emirates (Dubai) "
		"078411" = "Another Arabic country (ARE)"
		"078420" = "United Arab Emirates (ex.Dubai)"
		"078800" = "Tunisia"
		"079200" = "Turkey "
		"079500" = "Turkmenistan "
		"079600" = "Turks and Caicos Islands "
		"079800" = "Tuvalu "
		"080000" = "Uganda "
		"080400" = "Ukraine"
		"080700" = "FYR Macedonia"
		"081800" = "Egypt"
		"082600" = "United Kingdom "
		"082610" = "United Kingdom (excl.Scotland) "
		"082611" = "United Kingdom (England) "
		"082612" = "United Kingdom (Northern Ireland)"
		"082613" = "United Kingdom (Wales) "
		"082620" = "United Kingdom (Scotland)"
		"082640" = "United Kingdom (Great Britain) "
		"083000" = "Channel Islands"
		"083100" = "Guernsey "
		"083200" = "Jersey "
		"083300" = "Isle of Man"
		"083400" = "United Republic of Tanzania"
		"084000" = "United States of America "
		"085000" = "United States Virgin Islands "
		"085400" = "Burkina Faso "
		"085800" = "Uruguay"
		"086000" = "Uzbekistan "
		"086200" = "Venezuela (Bolivarian Republic of) "
		"087600" = "Wallis and Futuna Islands"
		"088200" = "Samoa"
		"088700" = "Yemen"
		"089400" = "Zambia "
		"009000" = "Solomon Islands"
		"100200" = "Africa "
		"100201" = "African country with Portuguese as the official language "
		"100202" = "A Sub-Saharan country (Africa excl. Maghreb) "
		"100204" = "North African country (Maghreb)"
		"114500" = "Middle Eastern country "
		"115000" = "A European country "
		"115001" = "Other Western European country (BEL) "
		"115009" = "Another country within the European Union (MLT)"
		"115002" = "Another country within the European Union (ITA)"
		"115003" = "Other European Union Country (LUX) "
		"115004" = "A country from European Union (PRT)"
		"115005" = "A European country that is not a member of the European Union"
		"115007" = "Other European country (NLD) "
		"115008" = "Other European country (QSC) "
		"115100" = "An Eastern European country"
		"115106" = "An Eastern European country outside the EU "
		"181000" = "One of the former USSR republics "
		"181001" = "In another former USSR republic (RUS)"
		"181002" = "In another former USSR republic (EST)"
		"189000" = "One of the former Yugoslav republics "
		"189001" = "One of the other former Yugoslav republics (HRV) "
		"189002" = "One of the other former Yugoslav republics (SRB) "
		"189003" = "Serbia and Montenegro"
		"900800" = "Another country (ALB)"
		"903100" = "Another country (AZE)"
		"903200" = "Another country (ARG)"
		"903600" = "Another country (AUS)"
		"904000" = "Another country (AUT)"
		"905600" = "Another country (BEL)"
		"907600" = "Another country (BRA)"
		"910000" = "Another country (BGR)"
		"912400" = "Another country (CAN)"
		"915200" = "Another country (CHL)"
		"915601" = "Another country (QCN )"
		"915604" = "Another country (QCH )"
		"915800" = "Another country (TAP)"
		"917000" = "Another country (COL)"
		"919100" = "Another country (HRV)"
		"920300" = "Another country (CZE)"
		"920800" = "Another country (DNK)"
		"921400" = "Another country (DOM)"
		"923300" = "Another country (EST)"
		"924600" = "Another country (FIN)"
		"925000" = "Another country (FRA)"
		"927600" = "Another country (DEU)"
		"930000" = "Another country (GRC)"
		"934400" = "Another country (HKG)"
		"934800" = "Another country (HUN)"
		"935200" = "Another country (ISL)"
		"936000" = "Another country (IDN)"
		"937200" = "Another country (IRL)"
		"937600" = "Another country (ISR)"
		"938000" = "Another country (ITA)"
		"939200" = "Another country (JPN)"
		"939800" = "Another country (KAZ)"
		"940000" = "Another country (JOR)"
		"941000" = "Another country (KOR)"
		"941700" = "Another country (KGZ)"
		"942800" = "Another country (LVA)"
		"943800" = "Another country (LIE)"
		"944000" = "Another country (LTU)"
		"944200" = "Another country (LUX)"
		"944600" = "Another country (MAC)"
		"948400" = "Another country (MEX)"
		"949800" = "Another country (MDA)"
		"949900" = "Another country (MNE)"
		"952800" = "Another country (NLD)"
		"955400" = "Another country (NZL)"
		"957800" = "Another country (NOR)"
		"959100" = "Another country (PAN)"
		"960400" = "Another country (PER)"
		"961600" = "Another country (POL)"
		"962000" = "Another country (PRT)"
		"963400" = "Another country (QAT)"
		"964200" = "Another country (ROU)"
		"964300" = "Another country (RUS)"
		"968800" = "Another country (SRB)"
		"970200" = "Another country (SGP)"
		"970300" = "Another country (SVK)"
		"970500" = "Another country (SVN)"
		"972400" = "Another country (ESP)"
		"975200" = "Another country (SWE)"
		"975600" = "Another country (CHE)"
		"976400" = "Another country (THA)"
		"985800" = "Another country (URY)"
		"978000" = "Another country (TTO)"
		"978410" = "Another country (QAR)"
		"978800" = "Another country (TUN)"
		"979200" = "Another country (TUR)"
		"980700" = "Another country (MKD)"
		"982600" = "Another country (GBR)"
		"982610" = "Another country (QUK)"
		"982620" = "Another country (QSC)"
		"984000" = "Another country (USA)"
		"918800" = "Another country (CRI)"
		"926800" = "Another country (GEO)"
		"935610" = "Another country (QIN)"
		"945800" = "Another country (MYS)"
		"947000" = "Another country (MLT)"
		"948000" = "Another country (MUS)"
		"978420" = "Another country (QRE)"
		"015602" = "Another province in mainland China (QCN) "
		"015605" = "Another province in mainland China (QCH) "
		"015603" = "Hong Kong, Macau, Taiwan "
		"119001" = "Another American country [URY] "
		"178411" = "Another Arabic country (QAR) "
		"100501" = "South, Latin  and Central America "
		"078421" = "Abu Dhabi (QRE)"
		"078422" = "Al Ain (QRE) "
		"078423" = "Western Region (QRE) "
		"078424" = "Sharjah (QRE)"
		"078425" = "Ajman (QRE)"
		"078426" = "Umm Al Quain (QRE) "
		"078427" = "Fujaurah (QRE) "
		"078428" = "Ras Al Khaimah (QRE) "
		"086201" = "Venezuela (Miranda)"
		"986201" = "Another country (QVE ) "
		"086220" = "Venezuela (ex. Miranda)"
		"181003" = "In another former USSR republic (GEO)"
		"099000" = "ZedlandA "
		"099100" = "ZedlandB "
		"099200" = "ZedlandC "
		"099300" = "ZedlandD "
		"099400" = "ZedlandE "
		"099500" = "ZedlandF "
		"099600" = "ZedlandG "
		"099700" = "ZedlandH "
		"099800" = "ZedlandJ "
		"099900" = "ZedlandK "
		"999000" = "Another country (ZZA)"
		"999100" = "Another country (ZZB)"
		"999200" = "Another country (ZZC)"
		"999300" = "Another country (ZZD)"
		"999400" = "Another country (ZZE)"
		"999500" = "Another country (ZZF)"
		"999600" = "Another country (ZZG)"
		"999700" = "Another country (ZZH)"
		"999800" = "Another country (ZZJ)"
		"999900" = "Another country (ZZK)"
		"978400" = "Another Country (ARE)"
		"919600" = "Another Country (CYP)"
		"970400" = "Another Country (VNM)"
		"181004" = "In another former USSR republic (KAZ)"
		"115006" = "Other European country (ITA) "
		"178401" = "Another State of the Gulf Cooperation Council (ARE)"
		"189004" = "Serbia, Montenegro and Kosovo"
		"089005" = "Kosovo "
		"064301" = "Perm (Russian Federation)"
		"064302" = "Another region of the Russian Federation "
		"000000" = "Not selected"
		"777777" = "N/A"
		"888888" = "Invalid"
		"999997" = "N/A"
		"999998" = "Invalid"
		"999999" = "Missing"
;
value $TESTLANGF
		"104" = "Mauritian French Creole"
		"367" = "Afar "
		"124" = "Achinese "
		"346" = "Acoli"
		"491" = "Adangme"
		"150" = "Afrikaans"
		"285" = "Akan "
		"140" = "Albanian "
		"147" = "Amharic"
		"500" = "Arabic "
		"617" = "Arabic dialect (TUN) "
		"117" = "Aragonese Spanish"
		"547" = "Armenian "
		"462" = "Arawak "
		"335" = "Assamese "
		"463" = "Australian languages "
		"468" = "Awadhi "
		"527" = "Aymara "
		"369" = "Azerbaijani"
		"289" = "Banda"
		"163" = "Baluchi"
		"177" = "Bambara"
		"310" = "Balinese "
		"451" = "Basque "
		"533" = "Basa "
		"448" = "Beja "
		"428" = "Belarusian "
		"394" = "Bemba"
		"162" = "Bengali"
		"297" = "Berber (Other) "
		"283" = "Bhojpuri "
		"127" = "Bihari "
		"246" = "Bikol"
		"245" = "Bislama"
		"103" = "Siksika"
		"192" = "Bosnian"
		"136" = "Braj "
		"446" = "Breton "
		"282" = "Batak"
		"457" = "Bugis"
		"351" = "Bulgarian"
		"173" = "Burmese"
		"450" = "Caddo"
		"445" = "Carib"
		"160" = "Catalan"
		"186" = "Cebuano"
		"429" = "Chamorro "
		"316" = "Chinese"
		"603" = "Shanghai dialect "
		"625" = "Cantonese"
		"641" = "Mandarin "
		"221" = "Chuukese "
		"372" = "Choctaw"
		"485" = "Cheyenne "
		"352" = "Coptic "
		"556" = "Cornish"
		"142" = "Corsican "
		"244" = "Czech"
		"264" = "Danish "
		"484" = "Dayak"
		"251" = "Dinka"
		"341" = "Divehi "
		"417" = "Dogri"
		"377" = "Duala"
		"322" = "Dutch"
		"511" = "Dyula"
		"267" = "Dzongkha "
		"182" = "Efik "
		"321" = "Ekajuk "
		"313" = "English"
		"121" = "Estonian "
		"461" = "Ewe"
		"525" = "Ewondo "
		"161" = "Fang "
		"404" = "Faroese"
		"145" = "Fanti"
		"314" = "Fijian "
		"420" = "Finnish"
		"219" = "Fon"
		"493" = "French "
		"274" = "Fula "
		"452" = "Friulian "
		"159" = "Gã "
		"395" = "Gayo "
		"299" = "Gbaya"
		"130" = "Georgian "
		"148" = "German "
		"648" = "Swiss German "
		"649" = "German (LIE) "
		"171" = "Ethiopic "
		"128" = "Gilbertese "
		"382" = "Scottish Gaelic"
		"434" = "Irish"
		"474" = "Galician "
		"271" = "Gondi"
		"269" = "Gorontalo"
		"356" = "Grebo"
		"449" = "Greek"
		"517" = "Guarani"
		"432" = "Gujarati "
		"199" = "Gwich'in "
		"374" = "Haida"
		"385" = "Haitian French Creole"
		"426" = "Hausa"
		"174" = "Hawaiian "
		"422" = "Hebrew "
		"440" = "Herero "
		"338" = "Hiligaynon "
		"292" = "Himachali"
		"415" = "Hindi"
		"157" = "Hittite"
		"298" = "Hmong"
		"172" = "Hiri Motu"
		"496" = "Hungarian"
		"405" = "Hupa "
		"290" = "Iban "
		"255" = "Igbo "
		"467" = "Icelandic"
		"345" = "Ido"
		"510" = "Sichuan Yi "
		"334" = "Ijo"
		"371" = "Iloko"
		"113" = "Indonesian "
		"222" = "Inupiaq"
		"200" = "Italian"
		"370" = "Javanese "
		"286" = "Japanese "
		"438" = "Kabyle "
		"138" = "Kachin "
		"536" = "Greenlandic"
		"502" = "Kamba"
		"437" = "Kannada"
		"425" = "Karen"
		"470" = "Kashmiri "
		"498" = "Kanuri "
		"233" = "Kawi "
		"275" = "Kazakh "
		"302" = "Kazakh "
		"218" = "Khasi"
		"520" = "Khmer"
		"384" = "Khotanese"
		"291" = "Kikuyu "
		"183" = "Kinyarwanda"
		"363" = "Kyrgyz "
		"277" = "Kimbundu "
		"211" = "Konkani"
		"499" = "Kongo"
		"301" = "Korean "
		"424" = "Kusaie "
		"169" = "Kpelle "
		"198" = "Kru"
		"225" = "Kurukh "
		"508" = "Kuanyama "
		"167" = "Kumyk"
		"105" = "Kurdish"
		"333" = "Kutenai"
		"487" = "Ladino "
		"427" = "Lahnda "
		"469" = "Lamba"
		"414" = "Lao"
		"325" = "Latvian"
		"339" = "Limburgish "
		"212" = "Lingala"
		"375" = "Lithuanian "
		"242" = "Mongo'-Nkundu"
		"349" = "Lozi "
		"507" = "Luxembourgish"
		"328" = "Luba'-Lulua"
		"357" = "Luba'-Katanga"
		"512" = "Ganda"
		"323" = "Luiseño"
		"119" = "Lunda"
		"194" = "Luo (Kenya and Tanzania) "
		"396" = "Lushai "
		"492" = "Macedonian "
		"402" = "Madurese "
		"347" = "Magahi "
		"423" = "Marshallese"
		"400" = "Maithili "
		"220" = "Makasar"
		"151" = "Malayalam"
		"330" = "Mandingo "
		"465" = "Maori"
		"522" = "Marathi"
		"387" = "Masai"
		"204" = "Malay"
		"359" = "Mandar "
		"303" = "Mende"
		"526" = "Micmac "
		"207" = "Minangkabau"
		"504" = "Malagasy "
		"409" = "Maltese"
		"397" = "Manchu "
		"543" = "Manipuri "
		"260" = "Mohawk "
		"366" = "Moldavian"
		"202" = "Mongolian"
		"294" = "Mooré"
		"354" = "Creek"
		"189" = "Marwari"
		"528" = "Nahuatl"
		"178" = "Neapolitan Italian "
		"557" = "Nauru"
		"300" = "Ndebele (South Africa) "
		"489" = "Ndebele (Zimbabwe) "
		"378" = "Ndonga "
		"516" = "Low German "
		"553" = "Nepali "
		"392" = "Newari "
		"360" = "Nias "
		"217" = "Niuean "
		"513" = "Norwegian (Nynorsk)"
		"141" = "Norwegian (Bokmål) "
		"238" = "Nogai"
		"523" = "Norwegian"
		"312" = "Northern Sotho "
		"431" = "Nubian languages "
		"348" = "Nyanja "
		"307" = "Nyamwezi "
		"501" = "Nyankole "
		"143" = "Nyoro"
		"318" = "Nzima"
		"368" = "Ojibwa "
		"280" = "Oriya"
		"206" = "Oromo"
		"453" = "Osage"
		"558" = "Ossetic"
		"254" = "Pangasinan "
		"231" = "Pahlavi"
		"407" = "Pampanga "
		"412" = "Panjabi"
		"481" = "Papiamento "
		"293" = "Palauan"
		"106" = "Phoenician "
		"466" = "Pali "
		"273" = "Polish "
		"158" = "Ponape "
		"232" = "Portuguese "
		"340" = "Pushto "
		"650" = "Aboriginal dialect (TAP) "
		"631" = "Amazonic language (PER)"
		"629" = "Indigenouslanguage (ARG) "
		"602" = "Minority languages and Bulgarian dialects (BGR)"
		"654" = "Another local Chinese dialect (QCN)"
		"604" = "Swiss Italian"
		"640" = "German dialect (BEL) "
		"615" = "Eastern European languages "
		"623" = "Other European Languages (NLD) "
		"605" = "Other European languages (QSC) "
		"606" = "Western European languages "
		"621" = "Flemish dialect (BEL)"
		"607" = "Regional languages (FRA) "
		"661" = "Hakka dialect (TAP)"
		"609" = "Chinese dialects or languages (HKG)"
		"642" = "Local language in Indonesia"
		"611" = "A dialect (ITA)"
		"610" = "Another official language (ITA)"
		"624" = "Another EU language (ITA)"
		"613" = "Chinese dialects or languages (MAC)"
		"622" = "Serbian of a yekavian variant or Montenegrin "
		"628" = "Indian Languages (MEX) "
		"627" = "Other National dialects or languages (ROU) "
		"643" = "Rusyn (SRB)"
		"612" = "Shanghai dialect "
		"620" = "A dialect (SVK)"
		"614" = "Languages of the former USSR "
		"639" = "Other former Yugoslavian languages (SVN) "
		"616" = "National dialects or languages (THA) "
		"630" = "Taiwanese dialect"
		"358" = "Quechua"
		"626" = "Ulster Scots "
		"608" = "Valencian"
		"600" = "Yugoslavian - Serbian, Croatian, etc "
		"332" = "Rajasthani "
		"234" = "Rapanui"
		"226" = "Rarotongan "
		"133" = "Raeto'-Romance "
		"381" = "Romani "
		"118" = "Romanian "
		"355" = "Rundi"
		"495" = "Russian"
		"393" = "Sandawe"
		"287" = "Sango (Ubangi Creole)"
		"208" = "Salishan languages "
		"398" = "Sasak"
		"228" = "Santali"
		"317" = "Serbian"
		"195" = "Scots"
		"266" = "Croatian "
		"456" = "Selkup "
		"552" = "Shan "
		"223" = "Sidamo "
		"261" = "Sinhalese"
		"170" = "Slovak "
		"442" = "Slovenian"
		"135" = "Southern Sami"
		"326" = "Northern Sami"
		"540" = "Sami "
		"259" = "Lule Sami"
		"518" = "Inari Sami "
		"272" = "Samoan "
		"342" = "Skolt Sami "
		"430" = "Shona"
		"270" = "Sindhi "
		"149" = "Soninke"
		"560" = "Sogdian"
		"137" = "Somali "
		"521" = "Songhai"
		"268" = "Sotho"
		"156" = "Spanish"
		"408" = "Sardinian"
		"288" = "Sranan "
		"239" = "Serer"
		"380" = "Swazi"
		"296" = "Sukuma "
		"224" = "Sundanese"
		"281" = "Susu "
		"403" = "Sumerian "
		"530" = "Swahili"
		"494" = "Swedish"
		"210" = "Syriac "
		"236" = "Tahitian "
		"154" = "Tamil"
		"401" = "Telugu "
		"433" = "Temne"
		"249" = "Terena "
		"131" = "Tetum"
		"237" = "Tajik"
		"108" = "Tagalog"
		"555" = "Thai "
		"247" = "Tibetan"
		"129" = "Tigré"
		"153" = "Tigrinya "
		"416" = "Tiv"
		"304" = "Tokelauan"
		"311" = "Tlingit"
		"305" = "Tamashek "
		"123" = "Tonga (Nyasa)"
		"383" = "Tongan "
		"235" = "Tok Pisin"
		"180" = "Tsimshian"
		"188" = "Tswana "
		"241" = "Tsonga "
		"175" = "Turkmen"
		"248" = "Tumbuka"
		"344" = "Turkish"
		"601" = "Gagauz "
		"337" = "Twi"
		"324" = "Tuvinian "
		"114" = "Ugaritic "
		"515" = "Uighur "
		"514" = "Ukrainian"
		"559" = "Umbundu"
		"258" = "Urdu "
		"471" = "Uzbek"
		"139" = "Vai"
		"279" = "Venda"
		"329" = "Vietnamese "
		"253" = "Votic"
		"475" = "Walamo "
		"112" = "Waray"
		"122" = "Washo"
		"379" = "Welsh"
		"230" = "Walloon"
		"419" = "Wolof"
		"184" = "Xhosa"
		"193" = "Yapese "
		"350" = "Yiddish"
		"134" = "Yoruba "
		"110" = "Zapotec"
		"532" = "Zenaga "
		"418" = "Zhuang "
		"331" = "Zande"
		"455" = "Zulu "
		"176" = "Zuni "
		"800" = "Another language (ARG) "
		"801" = "Another language (AUS) "
		"802" = "Another language (AUT) "
		"803" = "Another language (AZE) "
		"804" = "Another language (BEL) "
		"805" = "Another language (BRA) "
		"806" = "Another language (BGR) "
		"864" = "Another language (QCN) "
		"854" = "Another language (CHE) "
		"808" = "Another language (CHL) "
		"807" = "Another language (CAN) "
		"810" = "Another language (COL) "
		"812" = "Another language (CZE) "
		"818" = "Another language (DEU) "
		"868" = "Another language (DOM) "
		"813" = "Another language (DNK) "
		"814" = "Another language (EST) "
		"815" = "Another language (FIN) "
		"816" = "Another language (FRA) "
		"829" = "Another language (GBR) "
		"819" = "Another language (GRC) "
		"820" = "Another language (HKG) "
		"821" = "Another language (HUN) "
		"811" = "Another language (HRV) "
		"823" = "Another language (IDN) "
		"824" = "Another language (IRL) "
		"822" = "Another language (ISL) "
		"826" = "Another language (ITA) "
		"827" = "Another language (JPN) "
		"828" = "Another language (JOR) "
		"830" = "Another language (KGZ) "
		"845" = "Another language (KOR) "
		"817" = "Another language (KAZ) "
		"865" = "Another language (ALB) "
		"849" = "Another language (LIE) "
		"833" = "Another language (LTU) "
		"834" = "Another language (LUX) "
		"831" = "Another language (LVA) "
		"835" = "Another language (MAC) "
		"866" = "Another language (MKD) "
		"869" = "Another language (MDA) "
		"836" = "Another language (MEX) "
		"837" = "Another language (MNE) "
		"838" = "Another language (NLD) "
		"840" = "Another language (NOR) "
		"839" = "Another language (NZL) "
		"863" = "Another language (PAN) "
		"841" = "Another language (PER) "
		"842" = "Another language (POL) "
		"843" = "Another language (PRT) "
		"844" = "Another language (QAT) "
		"871" = "Another language (QAR) "
		"846" = "Another language (ROU) "
		"847" = "Another language (RUS) "
		"861" = "Another language (SRB) "
		"848" = "Another language (QSC) "
		"867" = "Another language (SGP) "
		"850" = "Another language (SVK) "
		"851" = "Another language (SVN) "
		"852" = "Another language (ESP) "
		"825" = "Another language (ISR) "
		"853" = "Another language (SWE) "
		"855" = "Another language (THA) "
		"856" = "Another language (TUN) "
		"857" = "Another language (TUR) "
		"862" = "Another language (TTO) "
		"809" = "Another language (TAP) "
		"858" = "Another language (QUK) "
		"860" = "Another language (URY) "
		"859" = "Another language (USA) "
		"832" = "Another language (CRI) "
		"870" = "Another language (GEO) "
		"880" = "Another language (QIN) "
		"872" = "Another language (MYS) "
		"873" = "Another language (MLT) "
		"874" = "Another language (MUS) "
		"875" = "Another language (QRE) "
		"876" = "Another language (QVN )"
		"877" = "Another language (ARE )"
		"879" = "Another Language (VNM) "
		"144" = "Filipino "
		"618" = "Another Language (QCH) " 
		"662" = "Portugués del Uruguay"          
		"996" = "Hybrid - English + Arabic (QAT)"
		"997" = "N/A"
		"998" = "Invalid"
		"999" = "Missing" 
;
value OECDF
		0 = "Non-OECD" 
		1 = "OECD"
;
value FORMIDF
		31 = "Form 31" 
		32 = "Form 32" 
		33 = "Form 33" 
		34 = "Form 34" 
		35 = "Form 35" 
		36 = "Form 36" 
		37 = "Form 37" 
		38 = "Form 38" 
		41 = "Form 41" 
		42 = "Form 42" 
		43 = "Form 43" 
		44 = "Form 44" 
		45 = "Form 45" 
		46 = "Form 46" 
		47 = "Form 47" 
		48 = "Form 48" 
		49 = "Form 49" 
		50 = "Form 50" 
		51 = "Form 51" 
		52 = "Form 52" 
		53 = "Form 53" 
		54 = "Form 54" 
		55 = "Form 55" 
		56 = "Form 56" 
		57 = "Form 57" 
		58 = "Form 58" 
		59 = "Form 59" 
		60 = "Form 60" 
		61 = "Form 61" 
		62 = "Form 62" 
		63 = "Form 63" 
		64 = "Form 64" 
		97 = "N/A" 
		98 = "Invalid"
		99 = "Missing"
;
value BookidF
		1	 = "booklet 1"
		2	 = "booklet 2"
		3	 = "booklet 3"
		4	 = "booklet 4"
		5	 = "booklet 5"
		6	 = "booklet 6"
		7	 = "booklet 7"
		8	 = "booklet 8"
		9	 = "booklet 9"
		10 = "booklet 10"
		11 = "booklet 11"
		12 = "booklet 12"
		13 = "booklet 13"
		21 = "booklet 21"
		22 = "booklet 22"
		23 = "booklet 23"
		24 = "booklet 24"
		25 = "booklet 25"
		26 = "booklet 26"
		27 = "booklet 27"
		20 = "UH booklet"
		99 = "Missing" 
;
value $PM00FQ01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM00GQ01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM00KQ02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM033Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM034Q01TF		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM155Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM155Q02DF		
	"7" = "N/A"
	"9" = "Missing"
	"A" = "No credit"
	"G" = "Partial credit"
	"H" = "Partial credit"
	"I" = "Partial credit"
	"O" = "Full credit"
	"r" = "Not reached"
;
value $PM155Q03DF		
	"7" = "N/A"
	"9" = "Missing"
	"A" = "No credit"
	"G" = "Partial credit"
	"H" = "Partial credit"
	"I" = "Partial credit"
	"O" = "Full credit"
	"P" = "Full credit"
	"Q" = "Full credit"
	"r" = "Not reached"
;
value $PM155Q04TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM192Q01TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM273Q01TF		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM305Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM406Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM406Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM408Q01TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM411Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM411Q02F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM420Q01TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM423Q01F		
	"1" = "Full credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM442Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM446Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM446Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM447Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM462Q01DF		
	"7" = "N/A"
	"9" = "Missing"
	"B" = "No credit"
	"C" = "No credit"
	"G" = "Partial credit"
	"H" = "Partial credit"
	"O" = "Full credit"
	"r" = "Not reached"
;
value $PM464Q01TF		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM474Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM496Q01TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM496Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM559Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM564Q01F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM564Q02F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"5" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM571Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM603Q01TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM800Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM803Q01TF		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM828Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM828Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM828Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM903Q01F		
	"0" = "No credit"
	"1" = "Partial credit"
	"2" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM903Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM905Q01TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM905Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM906Q01F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"5" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM906Q02F		
	"0" = "No credit"
	"1" = "Partial credit"
	"2" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM909Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM909Q02F		
	"1" = "Full credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM909Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM915Q01F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM915Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM918Q01F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM918Q02F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM918Q05F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM919Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM919Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM923Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"5" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM923Q03F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM923Q04F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM924Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM934Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM934Q02F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM936Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM936Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM939Q01F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM939Q02F		
	"1" = "Full credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM942Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"5" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM942Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM942Q03F		
	"0" = "No credit"
	"1" = "Partial credit"
	"2" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM943Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM943Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM948Q01F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM948Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM948Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM949Q01TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM949Q02TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM949Q03F		
	"0" = "No credit"
	"1" = "Partial credit"
	"2" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM953Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM953Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM953Q04DF		
	"7" = "N/A"
	"9" = "Missing"
	"A" = "No credit"
	"G" = "Partial credit"
	"H" = "Partial credit"
	"O" = "Full credit"
	"r" = "Not reached"
;
value $PM954Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM954Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM954Q04F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM955Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM955Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM955Q03F		
	"0" = "No credit"
	"1" = "Partial credit"
	"2" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM957Q01F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM957Q02F		
	"1" = "Full credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM957Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM961Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM961Q03F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM961Q05F		
	"0" = "No credit"
	"1" = "Partial credit"
	"2" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM967Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM967Q03TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM982Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM982Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM982Q03TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM982Q04F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM985Q01F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM985Q02F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM985Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM991Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM991Q02DF		
	"7" = "N/A"
	"9" = "Missing"
	"A" = "No credit"
	"G" = "Partial credit"
	"H" = "Partial credit"
	"O" = "Full credit"
	"r" = "Not reached"
;
value $PM992Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM992Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM992Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM995Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM995Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM995Q03F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM998Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM998Q04TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR220Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR220Q02BF		
	"1" = "Full credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR220Q04F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR404Q03F		
	"1" = "Full credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR404Q06F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR404Q07TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR404Q10AF		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR404Q10BF		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR406Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR406Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR406Q05F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR412Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"5" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR412Q05F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR412Q06TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR412Q08F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR420Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR420Q06F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR420Q09F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR420Q10F		
	"0" = "No credit"
	"1" = "Partial credit"
	"2" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR424Q02TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR424Q03F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR424Q07F		
	"1" = "Full credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR432Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR432Q05F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR432Q06TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"5" = "No credit"
	"6" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"A" = "No credit"
	"B" = "No credit"
	"C" = "Full credit"
	"r" = "Not reached"
;
value $PR437Q01F		
	"1" = "Full credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR437Q06F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR437Q07F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR446Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR446Q06F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR453Q01F		
	"1" = "Full credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR453Q04F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR453Q05TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR453Q06F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR455Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR455Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR455Q04F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR455Q05TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR456Q01F		
	"1" = "Full credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR456Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR456Q06F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR466Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR466Q03TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PR466Q06F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS131Q02DF		
	"7" = "N/A"
	"9" = "Missing"
	"B" = "No credit"
	"C" = "No credit"
	"D" = "No credit"
	"G" = "Full credit"
	"H" = "Full credit"
	"r" = "Not reached"
;
value $PS131Q04DF		
	"7" = "N/A"
	"9" = "Missing"
	"B" = "No credit"
	"C" = "No credit"
	"D" = "No credit"
	"E" = "No credit"
	"G" = "Full credit"
	"H" = "Full credit"
	"r" = "Not reached"
;
value $PS256Q01F		
	"1" = "Full credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS269Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS269Q03DF		
	"7" = "N/A"
	"9" = "Missing"
	"B" = "No credit"
	"C" = "No credit"
	"G" = "Full credit"
	"H" = "Full credit"
	"r" = "Not reached"
;
value $PS269Q04TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS326Q01F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS326Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS326Q03F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS326Q04TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS408Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS408Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS408Q04TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS408Q05F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS413Q04TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS413Q05F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS413Q06F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS415Q02F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS415Q07TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS415Q08TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS425Q02F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS425Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS425Q04F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS425Q05F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS428Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS428Q03F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS428Q05F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS438Q01TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS438Q02F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS438Q03DF		
	"7" = "N/A"
	"9" = "Missing"
	"B" = "No credit"
	"G" = "Full credit"
	"H" = "Full credit"
	"r" = "Not reached"
;
value $PS465Q01F		
	"0" = "No credit"
	"1" = "Partial credit"
	"2" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS465Q02F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS465Q04F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS466Q01TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS466Q05F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS466Q07TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS478Q01F		
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS478Q02TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS478Q03TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS498Q02TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS498Q03F		
	"1" = "Full credit"
	"2" = "No credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS498Q04F		
	"0" = "No credit"
	"1" = "Partial credit"
	"2" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS514Q02F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS514Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS514Q04F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS519Q01F		
	"0" = "No credit"
	"1" = "Partial credit"
	"2" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS519Q02TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS519Q03F		
	"0" = "No credit"
	"1" = "Full credit"
	"7" = "N/A"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS521Q02F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS521Q06F		
	"1" = "No credit"
	"2" = "Full credit"
	"3" = "No credit"
	"4" = "No credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS527Q01TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS527Q03TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PS527Q04TF		
	"0" = "No credit"
	"1" = "No credit"
	"2" = "No credit"
	"3" = "Full credit"
	"7" = "N/A"
	"8" = "Invalid"
	"9" = "Missing"
	"r" = "Not reached"
;
value $PM155Q02RF		
	"00" = "No credit"
	"11" = "Partial credit"
	"12" = "Partial credit"
	"13" = "Partial credit"
	"21" = "Full credit"
	"97" = "N/A"
	"99" = "Missing"
;
value $PM155Q03RF		
	"00" = "No credit"
	"11" = "Partial credit"
	"12" = "Partial credit"
	"13" = "Partial credit"
	"21" = "Full credit"
	"22" = "Full credit"
	"23" = "Full credit"
	"97" = "N/A"
	"99" = "Missing"
;
value $PM462Q01RF		
	"01" = "No credit"
	"02" = "No credit"
	"11" = "Partial credit"
	"12" = "Partial credit"
	"21" = "Full credit"
	"97" = "N/A"
	"99" = "Missing"
;
value $PM953Q04RF		
	"00" = "No credit"
	"11" = "Partial credit"
	"12" = "Partial credit"
	"21" = "Full credit"
	"97" = "N/A"
	"99" = "Missing"
;
value $PM991Q02RF		
	"00" = "No credit"
	"11" = "Partial credit"
	"12" = "Partial credit"
	"21" = "Full credit"
	"97" = "N/A"
	"99" = "Missing"
;
value $PS131Q02RF		
	"01" = "No credit"
	"02" = "No credit"
	"03" = "No credit"
	"11" = "Full credit"
	"12" = "Full credit"
	"97" = "N/A"
	"99" = "Missing"
;
value $PS131Q04RF		
	"01" = "No credit"
	"02" = "No credit"
	"03" = "No credit"
	"04" = "No credit"
	"11" = "Full credit"
	"12" = "Full credit"
	"97" = "N/A"
	"99" = "Missing"
;
value $PS269Q03RF		
	"01" = "No credit"
	"02" = "No credit"
	"11" = "Full credit"
	"12" = "Full credit"
	"97" = "N/A"
	"99" = "Missing"
;
value $PS438Q03RF		
	"01" = "No credit"
	"11" = "Full credit"
	"12" = "Full credit"
	"97" = "N/A"
	"99" = "Missing"
;
value PM034Q01RF		
	21 = "Full credit"
	9997 = "N/A"
	9998 = "Invalid"
	9999 = "Missing"
;
value $PM155Q04RF		
	"2111" = "Full credit"
	"7777" = "N/A"
	"8888" = "Invalid"
	"9999" = "Missing"
;
value $PM192Q01RF		
	"112" = "Full credit"
	"122" = "Full credit"
	"132" = "Full credit"
	"142" = "Full credit"
	"152" = "Full credit"
	"161" = "Full credit"
	"162" = "Full credit"
	"163" = "Full credit"
	"164" = "Full credit"
	"165" = "Full credit"
	"166" = "Full credit"
	"168" = "Full credit"
	"169" = "Full credit"
	"172" = "Full credit"
	"182" = "Full credit"
	"192" = "Full credit"
	"262" = "Full credit"
	"362" = "Full credit"
	"462" = "Full credit"
	"562" = "Full credit"
	"662" = "Full credit"
	"777" = "N/A"
	"862" = "Full credit"
	"888" = "Invalid"
	"962" = "Full credit"
	"999" = "Missing"
;
value $PM273Q01RF		
	"4213" = "Full credit"
	"9997" = "N/A"
	"9998" = "Invalid"
	"9999" = "Missing"
;
value $PM408Q01RF		
	"1222" = "Full credit"
	"7777" = "N/A"
	"8888" = "Invalid"
	"9999" = "Missing"
;
value $PM420Q01RF		
	"1112" = "Full credit"
	"7777" = "N/A"
	"8888" = "Invalid"
	"9999" = "Missing"
;
value PM464Q01RF		
	144 = "Full credit"
	9997 = "N/A"
	9998 = "Invalid"
	9999 = "Missing"
;
value $PM496Q01RF		
	"2111" = "Full credit"
	"7777" = "N/A"
	"8888" = "Invalid"
	"9999" = "Missing"
;
value $PM603Q01RF		
	"112" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value PM803Q01RF		
	12 = "Full credit"
	9997 = "N/A"
	9998 = "Invalid"
	9999 = "Missing"
;
value $PM905Q01RF		
	"12" = "Full credit"
	"77" = "N/A"
	"88" = "Invalid"
	"99" = "Missing"
;
value $PM949Q01RF		
	"211" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PM949Q02RF		
	"112" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PM967Q03RF		
	"22" = "Full credit"
	"77" = "N/A"
	"88" = "Invalid"
	"99" = "Missing"
;
value $PM982Q03RF		
	"11" = "Full credit"
	"77" = "N/A"
	"88" = "Invalid"
	"99" = "Missing"
;
value $PM998Q04RF		
	"12" = "Full credit"
	"77" = "N/A"
	"88" = "Invalid"
	"99" = "Missing"
;
value $PR404Q07RF		
	"122" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PR412Q06RF		
	"12" = "Full credit"
	"77" = "N/A"
	"88" = "Invalid"
	"99" = "Missing"
;
value $PR424Q02RF		
	"121" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PR432Q06RF		
	"112121221" = "Full credit"
	"777777777" = "N/A"
	"888888888" = "Invalid"
	"999999999" = "Missing"
;
value $PR453Q05RF		
	"2112" = "Full credit"
	"7777" = "N/A"
	"8888" = "Invalid"
	"9999" = "Missing"
;
value $PR455Q05RF		
	"2122" = "Full credit"
	"7777" = "N/A"
	"8888" = "Invalid"
	"9999" = "Missing"
;
value $PR466Q03RF		
	"122" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PS269Q04RF		
	"2112" = "Full credit"
	"7777" = "N/A"
	"8888" = "Invalid"
	"9999" = "Missing"
;
value $PS326Q04RF		
	"122" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PS408Q04RF		
	"211" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PS413Q04RF		
	"112" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PS415Q07RF		
	"21" = "Full credit"
	"77" = "N/A"
	"88" = "Invalid"
	"99" = "Missing"
;
value $PS415Q08RF		
	"112" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PS438Q01RF		
	"112" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PS466Q01RF		
	"121" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PS466Q07RF		
	"21" = "Full credit"
	"77" = "N/A"
	"88" = "Invalid"
	"99" = "Missing"
;
value $PS478Q02RF		
	"212" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PS478Q03RF		
	"12" = "Full credit"
	"77" = "N/A"
	"88" = "Invalid"
	"99" = "Missing"
;
value $PS498Q02RF		
	"121" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PS519Q02RF		
	"12" = "Full credit"
	"77" = "N/A"
	"88" = "Invalid"
	"99" = "Missing"
;
value $PS527Q01RF		
	"133" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PS527Q03RF		
	"112" = "Full credit"
	"122" = "Full credit"
	"182" = "Full credit"
	"192" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
value $PS527Q04RF		
	"121" = "Full credit"
	"777" = "N/A"
	"888" = "Invalid"
	"999" = "Missing"
;
run;

data M_DEC03.Cogn;
length
CNT $3
SUBNATIO $7
STRATUM $7
OECD 3
NC $6
SCHOOLID $7
StIDStd $5
BOOKID 3
PM00FQ01 $1
PM00GQ01 $1
PM00KQ02 $1
PM033Q01 $1
PM034Q01T $1
PM155Q01 $1
PM155Q02D $1
PM155Q03D $1
PM155Q04T $1
PM192Q01T $1
PM273Q01T $1
PM305Q01 $1
PM406Q01 $1
PM406Q02 $1
PM408Q01T $1
PM411Q01 $1
PM411Q02 $1
PM420Q01T $1
PM423Q01 $1
PM442Q02 $1
PM446Q01 $1
PM446Q02 $1
PM447Q01 $1
PM462Q01D $1
PM464Q01T $1
PM474Q01 $1
PM496Q01T $1
PM496Q02 $1
PM559Q01 $1
PM564Q01 $1
PM564Q02 $1
PM571Q01 $1
PM603Q01T $1
PM800Q01 $1
PM803Q01T $1
PM828Q01 $1
PM828Q02 $1
PM828Q03 $1
PM903Q01 $1
PM903Q03 $1
PM905Q01T $1
PM905Q02 $1
PM906Q01 $1
PM906Q02 $1
PM909Q01 $1
PM909Q02 $1
PM909Q03 $1
PM915Q01 $1
PM915Q02 $1
PM918Q01 $1
PM918Q02 $1
PM918Q05 $1
PM919Q01 $1
PM919Q02 $1
PM923Q01 $1
PM923Q03 $1
PM923Q04 $1
PM924Q02 $1
PM934Q01 $1
PM934Q02 $1
PM936Q01 $1
PM936Q02 $1
PM939Q01 $1
PM939Q02 $1
PM942Q01 $1
PM942Q02 $1
PM942Q03 $1
PM943Q01 $1
PM943Q02 $1
PM948Q01 $1
PM948Q02 $1
PM948Q03 $1
PM949Q01T $1
PM949Q02T $1
PM949Q03 $1
PM953Q02 $1
PM953Q03 $1
PM953Q04D $1
PM954Q01 $1
PM954Q02 $1
PM954Q04 $1
PM955Q01 $1
PM955Q02 $1
PM955Q03 $1
PM957Q01 $1
PM957Q02 $1
PM957Q03 $1
PM961Q02 $1
PM961Q03 $1
PM961Q05 $1
PM967Q01 $1
PM967Q03T $1
PM982Q01 $1
PM982Q02 $1
PM982Q03T $1
PM982Q04 $1
PM985Q01 $1
PM985Q02 $1
PM985Q03 $1
PM991Q01 $1
PM991Q02D $1
PM992Q01 $1
PM992Q02 $1
PM992Q03 $1
PM995Q01 $1
PM995Q02 $1
PM995Q03 $1
PM998Q02 $1
PM998Q04T $1
PR220Q01 $1
PR220Q02B $1
PR220Q04 $1
PR404Q03 $1
PR404Q06 $1
PR404Q07T $1
PR404Q10A $1
PR404Q10B $1
PR406Q01 $1
PR406Q02 $1
PR406Q05 $1
PR412Q01 $1
PR412Q05 $1
PR412Q06T $1
PR412Q08 $1
PR420Q02 $1
PR420Q06 $1
PR420Q09 $1
PR420Q10 $1
PR424Q02T $1
PR424Q03 $1
PR424Q07 $1
PR432Q01 $1
PR432Q05 $1
PR432Q06T $1
PR437Q01 $1
PR437Q06 $1
PR437Q07 $1
PR446Q03 $1
PR446Q06 $1
PR453Q01 $1
PR453Q04 $1
PR453Q05T $1
PR453Q06 $1
PR455Q02 $1
PR455Q03 $1
PR455Q04 $1
PR455Q05T $1
PR456Q01 $1
PR456Q02 $1
PR456Q06 $1
PR466Q02 $1
PR466Q03T $1
PR466Q06 $1
PS131Q02D $1
PS131Q04D $1
PS256Q01 $1
PS269Q01 $1
PS269Q03D $1
PS269Q04T $1
PS326Q01 $1
PS326Q02 $1
PS326Q03 $1
PS326Q04T $1
PS408Q01 $1
PS408Q03 $1
PS408Q04T $1
PS408Q05 $1
PS413Q04T $1
PS413Q05 $1
PS413Q06 $1
PS415Q02 $1
PS415Q07T $1
PS415Q08T $1
PS425Q02 $1
PS425Q03 $1
PS425Q04 $1
PS425Q05 $1
PS428Q01 $1
PS428Q03 $1
PS428Q05 $1
PS438Q01T $1
PS438Q02 $1
PS438Q03D $1
PS465Q01 $1
PS465Q02 $1
PS465Q04 $1
PS466Q01T $1
PS466Q05 $1
PS466Q07T $1
PS478Q01 $1
PS478Q02T $1
PS478Q03T $1
PS498Q02T $1
PS498Q03 $1
PS498Q04 $1
PS514Q02 $1
PS514Q03 $1
PS514Q04 $1
PS519Q01 $1
PS519Q02T $1
PS519Q03 $1
PS521Q02 $1
PS521Q06 $1
PS527Q01T $1
PS527Q03T $1
PS527Q04T $1
PM155Q02R $2
PM155Q03R $2
PM462Q01R $2
PM953Q04R $2
PM991Q02R $2
PS131Q02R $2
PS131Q04R $2
PS269Q03R $2
PS438Q03R $2
PM034Q01R 4
PM155Q04R $4
PM192Q01R $3
PM273Q01R $4
PM408Q01R $4
PM420Q01R $4
PM464Q01R 4
PM496Q01R $4
PM603Q01R $3
PM803Q01R 4
PM905Q01R $2
PM949Q01R $3
PM949Q02R $3
PM967Q03R $2
PM982Q03R $2
PM998Q04R $2
PR404Q07R $3
PR412Q06R $2
PR424Q02R $3
PR432Q06R $9
PR453Q05R $4
PR455Q05R $4
PR466Q03R $3
PS269Q04R $4
PS326Q04R $3
PS408Q04R $3
PS413Q04R $3
PS415Q07R $2
PS415Q08R $3
PS438Q01R $3
PS466Q01R $3
PS466Q07R $2
PS478Q02R $3
PS478Q03R $2
PS498Q02R $3
PS519Q02R $2
PS527Q01R $3
PS527Q03R $3
PS527Q04R $3
TESTLANG $3
VER_COGN $7
;
format
CNT $3.
SUBNATIO $7.
STRATUM $7.
OECD 1.
NC $6.
SCHOOLID $7.
StIDStd $5.
BOOKID 2.
PM00FQ01 $1.
PM00GQ01 $1.
PM00KQ02 $1.
PM033Q01 $1.
PM034Q01T $1.
PM155Q01 $1.
PM155Q02D $1.
PM155Q03D $1.
PM155Q04T $1.
PM192Q01T $1.
PM273Q01T $1.
PM305Q01 $1.
PM406Q01 $1.
PM406Q02 $1.
PM408Q01T $1.
PM411Q01 $1.
PM411Q02 $1.
PM420Q01T $1.
PM423Q01 $1.
PM442Q02 $1.
PM446Q01 $1.
PM446Q02 $1.
PM447Q01 $1.
PM462Q01D $1.
PM464Q01T $1.
PM474Q01 $1.
PM496Q01T $1.
PM496Q02 $1.
PM559Q01 $1.
PM564Q01 $1.
PM564Q02 $1.
PM571Q01 $1.
PM603Q01T $1.
PM800Q01 $1.
PM803Q01T $1.
PM828Q01 $1.
PM828Q02 $1.
PM828Q03 $1.
PM903Q01 $1.
PM903Q03 $1.
PM905Q01T $1.
PM905Q02 $1.
PM906Q01 $1.
PM906Q02 $1.
PM909Q01 $1.
PM909Q02 $1.
PM909Q03 $1.
PM915Q01 $1.
PM915Q02 $1.
PM918Q01 $1.
PM918Q02 $1.
PM918Q05 $1.
PM919Q01 $1.
PM919Q02 $1.
PM923Q01 $1.
PM923Q03 $1.
PM923Q04 $1.
PM924Q02 $1.
PM934Q01 $1.
PM934Q02 $1.
PM936Q01 $1.
PM936Q02 $1.
PM939Q01 $1.
PM939Q02 $1.
PM942Q01 $1.
PM942Q02 $1.
PM942Q03 $1.
PM943Q01 $1.
PM943Q02 $1.
PM948Q01 $1.
PM948Q02 $1.
PM948Q03 $1.
PM949Q01T $1.
PM949Q02T $1.
PM949Q03 $1.
PM953Q02 $1.
PM953Q03 $1.
PM953Q04D $1.
PM954Q01 $1.
PM954Q02 $1.
PM954Q04 $1.
PM955Q01 $1.
PM955Q02 $1.
PM955Q03 $1.
PM957Q01 $1.
PM957Q02 $1.
PM957Q03 $1.
PM961Q02 $1.
PM961Q03 $1.
PM961Q05 $1.
PM967Q01 $1.
PM967Q03T $1.
PM982Q01 $1.
PM982Q02 $1.
PM982Q03T $1.
PM982Q04 $1.
PM985Q01 $1.
PM985Q02 $1.
PM985Q03 $1.
PM991Q01 $1.
PM991Q02D $1.
PM992Q01 $1.
PM992Q02 $1.
PM992Q03 $1.
PM995Q01 $1.
PM995Q02 $1.
PM995Q03 $1.
PM998Q02 $1.
PM998Q04T $1.
PR220Q01 $1.
PR220Q02B $1.
PR220Q04 $1.
PR404Q03 $1.
PR404Q06 $1.
PR404Q07T $1.
PR404Q10A $1.
PR404Q10B $1.
PR406Q01 $1.
PR406Q02 $1.
PR406Q05 $1.
PR412Q01 $1.
PR412Q05 $1.
PR412Q06T $1.
PR412Q08 $1.
PR420Q02 $1.
PR420Q06 $1.
PR420Q09 $1.
PR420Q10 $1.
PR424Q02T $1.
PR424Q03 $1.
PR424Q07 $1.
PR432Q01 $1.
PR432Q05 $1.
PR432Q06T $1.
PR437Q01 $1.
PR437Q06 $1.
PR437Q07 $1.
PR446Q03 $1.
PR446Q06 $1.
PR453Q01 $1.
PR453Q04 $1.
PR453Q05T $1.
PR453Q06 $1.
PR455Q02 $1.
PR455Q03 $1.
PR455Q04 $1.
PR455Q05T $1.
PR456Q01 $1.
PR456Q02 $1.
PR456Q06 $1.
PR466Q02 $1.
PR466Q03T $1.
PR466Q06 $1.
PS131Q02D $1.
PS131Q04D $1.
PS256Q01 $1.
PS269Q01 $1.
PS269Q03D $1.
PS269Q04T $1.
PS326Q01 $1.
PS326Q02 $1.
PS326Q03 $1.
PS326Q04T $1.
PS408Q01 $1.
PS408Q03 $1.
PS408Q04T $1.
PS408Q05 $1.
PS413Q04T $1.
PS413Q05 $1.
PS413Q06 $1.
PS415Q02 $1.
PS415Q07T $1.
PS415Q08T $1.
PS425Q02 $1.
PS425Q03 $1.
PS425Q04 $1.
PS425Q05 $1.
PS428Q01 $1.
PS428Q03 $1.
PS428Q05 $1.
PS438Q01T $1.
PS438Q02 $1.
PS438Q03D $1.
PS465Q01 $1.
PS465Q02 $1.
PS465Q04 $1.
PS466Q01T $1.
PS466Q05 $1.
PS466Q07T $1.
PS478Q01 $1.
PS478Q02T $1.
PS478Q03T $1.
PS498Q02T $1.
PS498Q03 $1.
PS498Q04 $1.
PS514Q02 $1.
PS514Q03 $1.
PS514Q04 $1.
PS519Q01 $1.
PS519Q02T $1.
PS519Q03 $1.
PS521Q02 $1.
PS521Q06 $1.
PS527Q01T $1.
PS527Q03T $1.
PS527Q04T $1.
PM155Q02R $2.
PM155Q03R $2.
PM462Q01R $2.
PM953Q04R $2.
PM991Q02R $2.
PS131Q02R $2.
PS131Q04R $2.
PS269Q03R $2.
PS438Q03R $2.
PM034Q01R 4.
PM155Q04R $4.
PM192Q01R $3.
PM273Q01R $4.
PM408Q01R $4.
PM420Q01R $4.
PM464Q01R 4.
PM496Q01R $4.
PM603Q01R $3.
PM803Q01R 4.
PM905Q01R $2.
PM949Q01R $3.
PM949Q02R $3.
PM967Q03R $2.
PM982Q03R $2.
PM998Q04R $2.
PR404Q07R $3.
PR412Q06R $2.
PR424Q02R $3.
PR432Q06R $9.
PR453Q05R $4.
PR455Q05R $4.
PR466Q03R $3.
PS269Q04R $4.
PS326Q04R $3.
PS408Q04R $3.
PS413Q04R $3.
PS415Q07R $2.
PS415Q08R $3.
PS438Q01R $3.
PS466Q01R $3.
PS466Q07R $2.
PS478Q02R $3.
PS478Q03R $2.
PS498Q02R $3.
PS519Q02R $2.
PS527Q01R $3.
PS527Q03R $3.
PS527Q04R $3.
TESTLANG $3.
VER_COGN $7.
;
infile COGN missover linesize = 397 ;
input
CNT 1 - 3
SUBNATIO 4 - 10
STRATUM 11 - 17
OECD 18 - 18
NC 19 - 24
SCHOOLID 25 - 31
StIDStd 32 - 36
BOOKID 37 - 38
PM00FQ01 39 - 39
PM00GQ01 40 - 40
PM00KQ02 41 - 41
PM033Q01 42 - 42
PM034Q01T 43 - 43
PM155Q01 44 - 44
PM155Q02D 45 - 45
PM155Q03D 46 - 46
PM155Q04T 47 - 47
PM192Q01T 48 - 48
PM273Q01T 49 - 49
PM305Q01 50 - 50
PM406Q01 51 - 51
PM406Q02 52 - 52
PM408Q01T 53 - 53
PM411Q01 54 - 54
PM411Q02 55 - 55
PM420Q01T 56 - 56
PM423Q01 57 - 57
PM442Q02 58 - 58
PM446Q01 59 - 59
PM446Q02 60 - 60
PM447Q01 61 - 61
PM462Q01D 62 - 62
PM464Q01T 63 - 63
PM474Q01 64 - 64
PM496Q01T 65 - 65
PM496Q02 66 - 66
PM559Q01 67 - 67
PM564Q01 68 - 68
PM564Q02 69 - 69
PM571Q01 70 - 70
PM603Q01T 71 - 71
PM800Q01 72 - 72
PM803Q01T 73 - 73
PM828Q01 74 - 74
PM828Q02 75 - 75
PM828Q03 76 - 76
PM903Q01 77 - 77
PM903Q03 78 - 78
PM905Q01T 79 - 79
PM905Q02 80 - 80
PM906Q01 81 - 81
PM906Q02 82 - 82
PM909Q01 83 - 83
PM909Q02 84 - 84
PM909Q03 85 - 85
PM915Q01 86 - 86
PM915Q02 87 - 87
PM918Q01 88 - 88
PM918Q02 89 - 89
PM918Q05 90 - 90
PM919Q01 91 - 91
PM919Q02 92 - 92
PM923Q01 93 - 93
PM923Q03 94 - 94
PM923Q04 95 - 95
PM924Q02 96 - 96
PM934Q01 97 - 97
PM934Q02 98 - 98
PM936Q01 99 - 99
PM936Q02 100 - 100
PM939Q01 101 - 101
PM939Q02 102 - 102
PM942Q01 103 - 103
PM942Q02 104 - 104
PM942Q03 105 - 105
PM943Q01 106 - 106
PM943Q02 107 - 107
PM948Q01 108 - 108
PM948Q02 109 - 109
PM948Q03 110 - 110
PM949Q01T 111 - 111
PM949Q02T 112 - 112
PM949Q03 113 - 113
PM953Q02 114 - 114
PM953Q03 115 - 115
PM953Q04D 116 - 116
PM954Q01 117 - 117
PM954Q02 118 - 118
PM954Q04 119 - 119
PM955Q01 120 - 120
PM955Q02 121 - 121
PM955Q03 122 - 122
PM957Q01 123 - 123
PM957Q02 124 - 124
PM957Q03 125 - 125
PM961Q02 126 - 126
PM961Q03 127 - 127
PM961Q05 128 - 128
PM967Q01 129 - 129
PM967Q03T 130 - 130
PM982Q01 131 - 131
PM982Q02 132 - 132
PM982Q03T 133 - 133
PM982Q04 134 - 134
PM985Q01 135 - 135
PM985Q02 136 - 136
PM985Q03 137 - 137
PM991Q01 138 - 138
PM991Q02D 139 - 139
PM992Q01 140 - 140
PM992Q02 141 - 141
PM992Q03 142 - 142
PM995Q01 143 - 143
PM995Q02 144 - 144
PM995Q03 145 - 145
PM998Q02 146 - 146
PM998Q04T 147 - 147
PR220Q01 148 - 148
PR220Q02B 149 - 149
PR220Q04 150 - 150
PR404Q03 151 - 151
PR404Q06 152 - 152
PR404Q07T 153 - 153
PR404Q10A 154 - 154
PR404Q10B 155 - 155
PR406Q01 156 - 156
PR406Q02 157 - 157
PR406Q05 158 - 158
PR412Q01 159 - 159
PR412Q05 160 - 160
PR412Q06T 161 - 161
PR412Q08 162 - 162
PR420Q02 163 - 163
PR420Q06 164 - 164
PR420Q09 165 - 165
PR420Q10 166 - 166
PR424Q02T 167 - 167
PR424Q03 168 - 168
PR424Q07 169 - 169
PR432Q01 170 - 170
PR432Q05 171 - 171
PR432Q06T 172 - 172
PR437Q01 173 - 173
PR437Q06 174 - 174
PR437Q07 175 - 175
PR446Q03 176 - 176
PR446Q06 177 - 177
PR453Q01 178 - 178
PR453Q04 179 - 179
PR453Q05T 180 - 180
PR453Q06 181 - 181
PR455Q02 182 - 182
PR455Q03 183 - 183
PR455Q04 184 - 184
PR455Q05T 185 - 185
PR456Q01 186 - 186
PR456Q02 187 - 187
PR456Q06 188 - 188
PR466Q02 189 - 189
PR466Q03T 190 - 190
PR466Q06 191 - 191
PS131Q02D 192 - 192
PS131Q04D 193 - 193
PS256Q01 194 - 194
PS269Q01 195 - 195
PS269Q03D 196 - 196
PS269Q04T 197 - 197
PS326Q01 198 - 198
PS326Q02 199 - 199
PS326Q03 200 - 200
PS326Q04T 201 - 201
PS408Q01 202 - 202
PS408Q03 203 - 203
PS408Q04T 204 - 204
PS408Q05 205 - 205
PS413Q04T 206 - 206
PS413Q05 207 - 207
PS413Q06 208 - 208
PS415Q02 209 - 209
PS415Q07T 210 - 210
PS415Q08T 211 - 211
PS425Q02 212 - 212
PS425Q03 213 - 213
PS425Q04 214 - 214
PS425Q05 215 - 215
PS428Q01 216 - 216
PS428Q03 217 - 217
PS428Q05 218 - 218
PS438Q01T 219 - 219
PS438Q02 220 - 220
PS438Q03D 221 - 221
PS465Q01 222 - 222
PS465Q02 223 - 223
PS465Q04 224 - 224
PS466Q01T 225 - 225
PS466Q05 226 - 226
PS466Q07T 227 - 227
PS478Q01 228 - 228
PS478Q02T 229 - 229
PS478Q03T 230 - 230
PS498Q02T 231 - 231
PS498Q03 232 - 232
PS498Q04 233 - 233
PS514Q02 234 - 234
PS514Q03 235 - 235
PS514Q04 236 - 236
PS519Q01 237 - 237
PS519Q02T 238 - 238
PS519Q03 239 - 239
PS521Q02 240 - 240
PS521Q06 241 - 241
PS527Q01T 242 - 242
PS527Q03T 243 - 243
PS527Q04T 244 - 244
PM155Q02R 245 - 246
PM155Q03R 247 - 248
PM462Q01R 249 - 250
PM953Q04R 251 - 252
PM991Q02R 253 - 254
PS131Q02R 255 - 256
PS131Q04R 257 - 258
PS269Q03R 259 - 260
PS438Q03R 261 - 262
PM034Q01R 263 - 266
PM155Q04R 267 - 270
PM192Q01R 271 - 273
PM273Q01R 274 - 277
PM408Q01R 278 - 281
PM420Q01R 282 - 285
PM464Q01R 286 - 289
PM496Q01R 290 - 293
PM603Q01R 294 - 296
PM803Q01R 297 - 300
PM905Q01R 301 - 302
PM949Q01R 303 - 305
PM949Q02R 306 - 308
PM967Q03R 309 - 310
PM982Q03R 311 - 312
PM998Q04R 313 - 314
PR404Q07R 315 - 317
PR412Q06R 318 - 319
PR424Q02R 320 - 322
PR432Q06R 323 - 331
PR453Q05R 332 - 335
PR455Q05R 336 - 339
PR466Q03R 340 - 342
PS269Q04R 343 - 346
PS326Q04R 347 - 349
PS408Q04R 350 - 352
PS413Q04R 353 - 355
PS415Q07R 356 - 357
PS415Q08R 358 - 360
PS438Q01R 361 - 363
PS466Q01R 364 - 366
PS466Q07R 367 - 368
PS478Q02R 369 - 371
PS478Q03R 372 - 373
PS498Q02R 374 - 376
PS519Q02R 377 - 378
PS527Q01R 379 - 381
PS527Q03R 382 - 384
PS527Q04R 385 - 387
TESTLANG 388 - 390
VER_COGN 391 - 397
;
label
NC         = "National Centre 6-digit Code"
CNT        = "Country code 3-character" 
OECD       = "OECD country"
SUBNATIO    = "Adjudicated sub-region code 7-digit code (3-digit country code + region ID + stratum ID)"
STRATUM     = "Stratum ID 7-character (cnt + region ID + original stratum ID)"
SCHOOLID    = "School ID 7-digit (region ID + stratum ID + 3-digit school ID)"
STIDSTD    = "Student ID"
BOOKID     = "Booklet"
PM00FQ01   = "MATH - P2012 Apartment Purchase Q1"
PM00GQ01   = "MATH - P2012 An Advertising Column Q1"
PM00KQ02   = "MATH - P2012 Wheelchair Basketball Q2"
PM033Q01   = "MATH - P2000 A View with a Room Q1"
PM034Q01T  = "MATH - P2000 Bricks Q1"
PM155Q01   = "MATH - P2000 Pop Pyramids Q1"
PM155Q02D  = "MATH - P2000 Pop Pyramids Q2"
PM155Q03D  = "MATH - P2000 Pop Pyramids Q3"
PM155Q04T  = "MATH - P2000 Pop Pyramids Q4"
PM192Q01T  = "MATH - P2000 Containers Q1"
PM273Q01T  = "MATH - P2000 Pipelines Q1"
PM305Q01   = "MATH - P2000 Map Q1"
PM406Q01   = "MATH - P2003 Running Tracks Q1"
PM406Q02   = "MATH - P2003 Running Tracks Q2"
PM408Q01T  = "MATH - P2003 Lotteries Q1"
PM411Q01   = "MATH - P2003 Diving Q1"
PM411Q02   = "MATH - P2003 Diving Q2"
PM420Q01T  = "MATH - P2003 Transport Q1"
PM423Q01   = "MATH - P2003 Tossing Coins Q1"
PM442Q02   = "MATH - P2003 Braille Q2"
PM446Q01   = "MATH - P2003 The Thermometer Cricket Q1"
PM446Q02   = "MATH - P2003 The Thermometer Cricket Q2"
PM447Q01   = "MATH - P2003 Tile Arrangement Q1"
PM462Q01D  = "MATH - P2003 The Third Side Q1"
PM464Q01T  = "MATH - P2003 The Fence Q1"
PM474Q01   = "MATH - P2003 Running Time Q1"
PM496Q01T  = "MATH - P2003 Cash Withdrawal Q1"
PM496Q02   = "MATH - P2003 Cash Withdrawal Q2"
PM559Q01   = "MATH - P2003 Telephone Rates Q1"
PM564Q01   = "MATH - P2003 Chair Lift Q1"
PM564Q02   = "MATH - P2003 Chair Lift Q2"
PM571Q01   = "MATH - P2003 Stop the Car Q1"
PM603Q01T  = "MATH - P2003 Number Check Q1"
PM800Q01   = "MATH - P2003 Computer Game Q1"
PM803Q01T  = "MATH - P2003 Labels Q1"
PM828Q01   = "MATH - P2003 Carbon Dioxide Q1"
PM828Q02   = "MATH - P2003 Carbon Dioxide Q2"
PM828Q03   = "MATH - P2003 Carbon Dioxide Q3"
PM903Q01   = "MATH - P2012 Drip Rate Q1"
PM903Q03   = "MATH - P2012 Drip Rate Q3"
PM905Q01T  = "MATH - P2012 Tennis Balls Q1"
PM905Q02   = "MATH - P2012 Tennis Balls Q2"
PM906Q01   = "MATH - P2012 Crazy Ants Q1"
PM906Q02   = "MATH - P2012 Crazy Ants Q2"
PM909Q01   = "MATH - P2012 Speeding Fines Q1"
PM909Q02   = "MATH - P2012 Speeding Fines Q2"
PM909Q03   = "MATH - P2012 Speeding Fines Q3"
PM915Q01   = "MATH - P2012 Carbon Tax Q1"
PM915Q02   = "MATH - P2012 Carbon Tax Q2"
PM918Q01   = "MATH - P2012 Charts Q1"
PM918Q02   = "MATH - P2012 Charts Q2"
PM918Q05   = "MATH - P2012 Charts Q5"
PM919Q01   = "MATH - P2012 Zs Fan Merchandise Q1"
PM919Q02   = "MATH - P2012 Zs Fan Merchandise Q2"
PM923Q01   = "MATH - P2012 Sailing Ships Q1"
PM923Q03   = "MATH - P2012 Sailing Ships Q3"
PM923Q04   = "MATH - P2012 Sailing Ships Q4"
PM924Q02   = "MATH - P2012 Sauce Q2"
PM934Q01   = "MATH - P2012 London Eye Q1"
PM934Q02   = "MATH - P2012 London Eye Q2"
PM936Q01   = "MATH - P2012 Seats In A Theatre Q1"
PM936Q02   = "MATH - P2012 Seats In A Theatre Q2"
PM939Q01   = "MATH - P2012 Racing Q1"
PM939Q02   = "MATH - P2012 Racing Q2"
PM942Q01   = "MATH - P2012 Climbing Mount Fuji Q1"
PM942Q02   = "MATH - P2012 Climbing Mount Fuji Q2"
PM942Q03   = "MATH - P2012 Climbing Mount Fuji Q3"
PM943Q01   = "MATH - P2012 Arches Q1"
PM943Q02   = "MATH - P2012 Arches Q2"
PM948Q01   = "MATH - P2012 Part-Time Work Q1"
PM948Q02   = "MATH - P2012 Part-Time Work Q2"
PM948Q03   = "MATH - P2012 Part-Time Work Q3"
PM949Q01T  = "MATH - P2012 Roof Truss Design Q1"
PM949Q02T  = "MATH - P2012 Roof Truss Design Q2"
PM949Q03   = "MATH - P2012 Roof Truss Design Q3"
PM953Q02   = "MATH - P2012 Flu Test Q2"
PM953Q03   = "MATH - P2012 Flu Test Q3"
PM953Q04D  = "MATH - P2012 Flu Test Q4"
PM954Q01   = "MATH - P2012 Medicine Doses Q1"
PM954Q02   = "MATH - P2012 Medicine Doses Q2"
PM954Q04   = "MATH - P2012 Medicine Doses Q4"
PM955Q01   = "MATH - P2012 Migration Q1"
PM955Q02   = "MATH - P2012 Migration Q2"
PM955Q03   = "MATH - P2012 Migration Q3"
PM957Q01   = "MATH - P2012 Helen The Cyclist Q1"
PM957Q02   = "MATH - P2012 Helen The Cyclist Q2"
PM957Q03   = "MATH - P2012 Helen The Cyclist Q3"
PM961Q02   = "MATH - P2012 Chocolate Q2"
PM961Q03   = "MATH - P2012 Chocolate Q3"
PM961Q05   = "MATH - P2012 Chocolate Q5"
PM967Q01   = "MATH - P2012 Wooden Train Set Q1"
PM967Q03T  = "MATH - P2012 Wooden Train Set Q3"
PM982Q01   = "MATH - P2012 Employment Data Q1"
PM982Q02   = "MATH - P2012 Employment Data Q2"
PM982Q03T  = "MATH - P2012 Employment Data Q3"
PM982Q04   = "MATH - P2012 Employment Data Q4"
PM985Q01   = "MATH - P2012 Which Car Q1"
PM985Q02   = "MATH - P2012 Which Car Q2"
PM985Q03   = "MATH - P2012 Which Car Q3"
PM991Q01   = "MATH - P2012 Garage Q1"
PM991Q02D  = "MATH - P2012 Garage Q2"
PM992Q01   = "MATH - P2012 Spacers Q1"
PM992Q02   = "MATH - P2012 Spacers Q2"
PM992Q03   = "MATH - P2012 Spacers Q3"
PM995Q01   = "MATH - P2012 Revolving Door Q1"
PM995Q02   = "MATH - P2012 Revolving Door Q2"
PM995Q03   = "MATH - P2012 Revolving Door Q3"
PM998Q02   = "MATH - P2012 Bike Rental Q2"
PM998Q04T  = "MATH - P2012 Bike Rental Q4"
PR220Q01   = "READ - P2000 South Pole Q1"
PR220Q02B  = "READ - P2000 South Pole Q2B"
PR220Q04   = "READ - P2000 South Pole Q4"
PR404Q03   = "READ - P2009 Sleep Q3"
PR404Q06   = "READ - P2009 Sleep Q6"
PR404Q07T  = "READ - P2009 Sleep Q7"
PR404Q10A  = "READ - P2009 Sleep Q10A"
PR404Q10B  = "READ - P2009 Sleep Q10B"
PR406Q01   = "READ - P2009 Kokeshi Dolls Q1"
PR406Q02   = "READ - P2009 Kokeshi Dolls Q2"
PR406Q05   = "READ - P2009 Kokeshi Dolls Q5"
PR412Q01   = "READ - P2009 World Languages Q1"
PR412Q05   = "READ - P2009 World Languages Q5"
PR412Q06T  = "READ - P2009 World Languages Q6"
PR412Q08   = "READ - P2009 World Languages Q8"
PR420Q02   = "READ - P2009 Childrens Futures Q2"
PR420Q06   = "READ - P2009 Childrens Futures Q6"
PR420Q09   = "READ - P2009 Childrens Futures Q9"
PR420Q10   = "READ - P2009 Childrens Futures Q10"
PR424Q02T  = "READ - P2009 Fair Trade Q2"
PR424Q03   = "READ - P2009 Fair Trade Q3"
PR424Q07   = "READ - P2009 Fair Trade Q7"
PR432Q01   = "READ - P2009 About a book Q1"
PR432Q05   = "READ - P2009 About a book Q5"
PR432Q06T  = "READ - P2009 About a book Q6"
PR437Q01   = "READ - P2009 Narcissus Q1"
PR437Q06   = "READ - P2009 Narcissus Q6"
PR437Q07   = "READ - P2009 Narcissus Q7"
PR446Q03   = "READ - P2009 Job Vacancy Q3"
PR446Q06   = "READ - P2009 Job Vacancy Q6"
PR453Q01   = "READ - P2009 Find Summer Job Q1"
PR453Q04   = "READ - P2009 Find Summer Job Q4"
PR453Q05T  = "READ - P2009 Find Summer Job Q5"
PR453Q06   = "READ - P2009 Find Summer Job Q6"
PR455Q02   = "READ - P2009 Chocolate and Health Q2"
PR455Q03   = "READ - P2009 Chocolate and Health Q3"
PR455Q04   = "READ - P2009 Chocolate and Health Q4"
PR455Q05T  = "READ - P2009 Chocolate and Health Q5"
PR456Q01   = "READ - P2009 Biscuits Q1"
PR456Q02   = "READ - P2009 Biscuits Q2"
PR456Q06   = "READ - P2009 Biscuits Q6"
PR466Q02   = "READ - P2009 Work Right Q2"
PR466Q03T  = "READ - P2009 Work Right Q3"
PR466Q06   = "READ - P2009 Work Right Q6"
PS131Q02D  = "SCIE - P2000 Good Vibrations Q2"
PS131Q04D  = "SCIE - P2006 Good Vibrations Q4"
PS256Q01   = "SCIE - P2000 Spoons Q1"
PS269Q01   = "SCIE - P2000 Earths Temperature Q1"
PS269Q03D  = "SCIE - P2000 Earths Temperature Q3"
PS269Q04T  = "SCIE - P2000 Earths Temperature Q4"
PS326Q01   = "SCIE - P2003 Milk Q1"
PS326Q02   = "SCIE - P2003 Milk Q2"
PS326Q03   = "SCIE - P2003 Milk Q3"
PS326Q04T  = "SCIE - P2003 Milk Q4"
PS408Q01   = "SCIE - P2006 Wild Oat Grass Q1"
PS408Q03   = "SCIE - P2006 Wild Oat Grass Q3"
PS408Q04T  = "SCIE - P2006 Wild Oat Grass Q4"
PS408Q05   = "SCIE - P2006 Wild Oat Grass Q5"
PS413Q04T  = "SCIE - P2006 Plastic Age Q4"
PS413Q05   = "SCIE - P2006 Plastic Age Q5"
PS413Q06   = "SCIE - P2006 Plastic Age Q6"
PS415Q02   = "SCIE - P2006 Solar Panels Q2"
PS415Q07T  = "SCIE - P2006 Solar Panels Q7"
PS415Q08T  = "SCIE - P2006 Solar Panels Q8"
PS425Q02   = "SCIE - P2006 Penguin Island Q2"
PS425Q03   = "SCIE - P2006 Penguin Island Q3"
PS425Q04   = "SCIE - P2006 Penguin Island Q4"
PS425Q05   = "SCIE - P2006 Penguin Island Q5"
PS428Q01   = "SCIE - P2006 Bacteria in Milk Q1"
PS428Q03   = "SCIE - P2006 Bacteria in Milk Q3"
PS428Q05   = "SCIE - P2006 Bacteria in Milk Q5"
PS438Q01T  = "SCIE - P2006 Green Parks Q1"
PS438Q02   = "SCIE - P2006 Green Parks Q2"
PS438Q03D  = "SCIE - P2006 Green Parks Q3"
PS465Q01   = "SCIE - P2006 Different Climates Q1"
PS465Q02   = "SCIE - P2006 Different Climates Q2"
PS465Q04   = "SCIE - P2006 Different Climates Q4"
PS466Q01T  = "SCIE - P2006 Forest Fires Q1"
PS466Q05   = "SCIE - P2006 Forest Fires Q5"
PS466Q07T  = "SCIE - P2006 Forest Fires Q7"
PS478Q01   = "SCIE - P2006 Antibiotics Q1"
PS478Q02T  = "SCIE - P2006 Antibiotics Q2"
PS478Q03T  = "SCIE - P2006 Antibiotics Q3"
PS498Q02T  = "SCIE - P2006 Experimental Digestion Q2"
PS498Q03   = "SCIE - P2006 Experimental Digestion Q3"
PS498Q04   = "SCIE - P2006 Experimental Digestion Q4"
PS514Q02   = "SCIE - P2006 Development and Disaster Q2"
PS514Q03   = "SCIE - P2006 Development and Disaster Q3"
PS514Q04   = "SCIE - P2006 Development and Disaster Q4"
PS519Q01   = "SCIE - P2006 Airbags Q1"
PS519Q02T  = "SCIE - P2006 Airbags Q2"
PS519Q03   = "SCIE - P2006 Airbags Q3"
PS521Q02   = "SCIE - P2006 Cooking Outdoors Q2"
PS521Q06   = "SCIE - P2006 Cooking Outdoors Q6"
PS527Q01T  = "SCIE - P2006 Extinction of the Dinosaurs Q1"
PS527Q03T  = "SCIE - P2006 Extinction of the Dinosaurs Q3"
PS527Q04T  = "SCIE - P2006 Extinction of the Dinosaurs Q4"
PM155Q02R  = "MATH - P2000 Pop Pyramids Q2 - original responses"
PM155Q03R  = "MATH - P2000 Pop Pyramids Q3 - original responses"
PM462Q01R  = "MATH - P2003 The Third Side Q1 - original responses"
PM953Q04R  = "MATH - P2012 Flu Test Q4 - original responses"
PM991Q02R  = "MATH - P2012 Garage Q2 - original responses"
PS131Q02R  = "SCIE - P2000 Good Vibrations Q2 - original responses"
PS131Q04R  = "SCIE - P2006 Good Vibrations Q4 - original responses"
PS269Q03R  = "SCIE - P2000 Earths Temperature Q3 - original responses"
PS438Q03R  = "SCIE - P2006 Green Parks Q3 - original responses"
PM034Q01R  = "MATH - P2000 Bricks Q1 - original responses"
PM155Q04R  = "MATH - P2000 Pop Pyramids Q4 - original responses"
PM192Q01R  = "MATH - P2000 Containers Q1 - original responses"
PM273Q01R  = "MATH - P2000 Pipelines Q1 - original responses"
PM408Q01R  = "MATH - P2003 Lotteries Q1 - original responses"
PM420Q01R  = "MATH - P2003 Transport Q1 - original responses"
PM464Q01R  = "MATH - P2003 The Fence Q1 - original responses"
PM496Q01R  = "MATH - P2003 Cash Withdrawal Q1 - original responses"
PM603Q01R  = "MATH - P2003 Number Check Q1 - original responses"
PM803Q01R  = "MATH - P2003 Labels Q1 - original responses"
PM905Q01R  = "MATH - P2012 Tennis Balls Q1 - original responses"
PM949Q01R  = "MATH - P2012 Roof Truss Design Q1 - original responses"
PM949Q02R  = "MATH - P2012 Roof Truss Design Q2 - original responses"
PM967Q03R  = "MATH - P2012 Wooden Train Set Q3 - original responses"
PM982Q03R  = "MATH - P2012 Employment Data Q3 - original responses"
PM998Q04R  = "MATH - P2012 Bike Rental Q4 - original responses"
PR404Q07R  = "READ - P2009 Sleep Q7 - original responses"
PR412Q06R  = "READ - P2009 World Languages Q6 - original responses"
PR424Q02R  = "READ - P2009 Fair Trade Q2 - original responses"
PR432Q06R  = "READ - P2009 About a book Q6 - original responses"
PR453Q05R  = "READ - P2009 Find Summer Job Q5 - original responses"
PR455Q05R  = "READ - P2009 Chocolate and Health Q5 - original responses"
PR466Q03R  = "READ - P2009 Work Right Q3 - original responses"
PS269Q04R  = "SCIE - P2000 Earths Temperature Q4 - original responses"
PS326Q04R  = "SCIE - P2003 Milk Q4 - original responses"
PS408Q04R  = "SCIE - P2006 Wild Oat Grass Q4 - original responses"
PS413Q04R  = "SCIE - P2006 Plastic Age Q4 - original responses"
PS415Q07R  = "SCIE - P2006 Solar Panels Q7 - original responses"
PS415Q08R  = "SCIE - P2006 Solar Panels Q8 - original responses"
PS438Q01R  = "SCIE - P2006 Green Parks Q1 - original responses"
PS466Q01R  = "SCIE - P2006 Forest Fires Q1 - original responses"
PS466Q07R  = "SCIE - P2006 Forest Fires Q7 - original responses"
PS478Q02R  = "SCIE - P2006 Antibiotics Q2 - original responses"
PS478Q03R  = "SCIE - P2006 Antibiotics Q3 - original responses"
PS498Q02R  = "SCIE - P2006 Experimental Digestion Q2 - original responses"
PS519Q02R  = "SCIE - P2006 Airbags Q2 - original responses"
PS527Q01R  = "SCIE - P2006 Extinction of the Dinosaurs Q1 - original responses"
PS527Q03R  = "SCIE - P2006 Extinction of the Dinosaurs Q3 - original responses"
PS527Q04R  = "SCIE - P2006 Extinction of the Dinosaurs Q4 - original responses"
TESTLANG   = "Language of Test (3-character)"
VER_COGN   = "Date of cognitive database release"
;

format
CNT $CNTF.
SUBNATIO $SUBNATIOF.
STRATUM $STRATUMF.
OECD OECDF.
NC $NCF.
BOOKID BOOKIDF.
PM00FQ01 $PM00FQ01F.
PM00GQ01 $PM00GQ01F.
PM00KQ02 $PM00KQ02F.
PM033Q01 $PM033Q01F.
PM034Q01T $PM034Q01TF.
PM155Q01 $PM155Q01F.
PM155Q02D $PM155Q02DF.
PM155Q03D $PM155Q03DF.
PM155Q04T $PM155Q04TF.
PM192Q01T $PM192Q01TF.
PM273Q01T $PM273Q01TF.
PM305Q01 $PM305Q01F.
PM406Q01 $PM406Q01F.
PM406Q02 $PM406Q02F.
PM408Q01T $PM408Q01TF.
PM411Q01 $PM411Q01F.
PM411Q02 $PM411Q02F.
PM420Q01T $PM420Q01TF.
PM423Q01 $PM423Q01F.
PM442Q02 $PM442Q02F.
PM446Q01 $PM446Q01F.
PM446Q02 $PM446Q02F.
PM447Q01 $PM447Q01F.
PM462Q01D $PM462Q01DF.
PM464Q01T $PM464Q01TF.
PM474Q01 $PM474Q01F.
PM496Q01T $PM496Q01TF.
PM496Q02 $PM496Q02F.
PM559Q01 $PM559Q01F.
PM564Q01 $PM564Q01F.
PM564Q02 $PM564Q02F.
PM571Q01 $PM571Q01F.
PM603Q01T $PM603Q01TF.
PM800Q01 $PM800Q01F.
PM803Q01T $PM803Q01TF.
PM828Q01 $PM828Q01F.
PM828Q02 $PM828Q02F.
PM828Q03 $PM828Q03F.
PM903Q01 $PM903Q01F.
PM903Q03 $PM903Q03F.
PM905Q01T $PM905Q01TF.
PM905Q02 $PM905Q02F.
PM906Q01 $PM906Q01F.
PM906Q02 $PM906Q02F.
PM909Q01 $PM909Q01F.
PM909Q02 $PM909Q02F.
PM909Q03 $PM909Q03F.
PM915Q01 $PM915Q01F.
PM915Q02 $PM915Q02F.
PM918Q01 $PM918Q01F.
PM918Q02 $PM918Q02F.
PM918Q05 $PM918Q05F.
PM919Q01 $PM919Q01F.
PM919Q02 $PM919Q02F.
PM923Q01 $PM923Q01F.
PM923Q03 $PM923Q03F.
PM923Q04 $PM923Q04F.
PM924Q02 $PM924Q02F.
PM934Q01 $PM934Q01F.
PM934Q02 $PM934Q02F.
PM936Q01 $PM936Q01F.
PM936Q02 $PM936Q02F.
PM939Q01 $PM939Q01F.
PM939Q02 $PM939Q02F.
PM942Q01 $PM942Q01F.
PM942Q02 $PM942Q02F.
PM942Q03 $PM942Q03F.
PM943Q01 $PM943Q01F.
PM943Q02 $PM943Q02F.
PM948Q01 $PM948Q01F.
PM948Q02 $PM948Q02F.
PM948Q03 $PM948Q03F.
PM949Q01T $PM949Q01TF.
PM949Q02T $PM949Q02TF.
PM949Q03 $PM949Q03F.
PM953Q02 $PM953Q02F.
PM953Q03 $PM953Q03F.
PM953Q04D $PM953Q04DF.
PM954Q01 $PM954Q01F.
PM954Q02 $PM954Q02F.
PM954Q04 $PM954Q04F.
PM955Q01 $PM955Q01F.
PM955Q02 $PM955Q02F.
PM955Q03 $PM955Q03F.
PM957Q01 $PM957Q01F.
PM957Q02 $PM957Q02F.
PM957Q03 $PM957Q03F.
PM961Q02 $PM961Q02F.
PM961Q03 $PM961Q03F.
PM961Q05 $PM961Q05F.
PM967Q01 $PM967Q01F.
PM967Q03T $PM967Q03TF.
PM982Q01 $PM982Q01F.
PM982Q02 $PM982Q02F.
PM982Q03T $PM982Q03TF.
PM982Q04 $PM982Q04F.
PM985Q01 $PM985Q01F.
PM985Q02 $PM985Q02F.
PM985Q03 $PM985Q03F.
PM991Q01 $PM991Q01F.
PM991Q02D $PM991Q02DF.
PM992Q01 $PM992Q01F.
PM992Q02 $PM992Q02F.
PM992Q03 $PM992Q03F.
PM995Q01 $PM995Q01F.
PM995Q02 $PM995Q02F.
PM995Q03 $PM995Q03F.
PM998Q02 $PM998Q02F.
PM998Q04T $PM998Q04TF.
PR220Q01 $PR220Q01F.
PR220Q02B $PR220Q02BF.
PR220Q04 $PR220Q04F.
PR404Q03 $PR404Q03F.
PR404Q06 $PR404Q06F.
PR404Q07T $PR404Q07TF.
PR404Q10A $PR404Q10AF.
PR404Q10B $PR404Q10BF.
PR406Q01 $PR406Q01F.
PR406Q02 $PR406Q02F.
PR406Q05 $PR406Q05F.
PR412Q01 $PR412Q01F.
PR412Q05 $PR412Q05F.
PR412Q06T $PR412Q06TF.
PR412Q08 $PR412Q08F.
PR420Q02 $PR420Q02F.
PR420Q06 $PR420Q06F.
PR420Q09 $PR420Q09F.
PR420Q10 $PR420Q10F.
PR424Q02T $PR424Q02TF.
PR424Q03 $PR424Q03F.
PR424Q07 $PR424Q07F.
PR432Q01 $PR432Q01F.
PR432Q05 $PR432Q05F.
PR432Q06T $PR432Q06TF.
PR437Q01 $PR437Q01F.
PR437Q06 $PR437Q06F.
PR437Q07 $PR437Q07F.
PR446Q03 $PR446Q03F.
PR446Q06 $PR446Q06F.
PR453Q01 $PR453Q01F.
PR453Q04 $PR453Q04F.
PR453Q05T $PR453Q05TF.
PR453Q06 $PR453Q06F.
PR455Q02 $PR455Q02F.
PR455Q03 $PR455Q03F.
PR455Q04 $PR455Q04F.
PR455Q05T $PR455Q05TF.
PR456Q01 $PR456Q01F.
PR456Q02 $PR456Q02F.
PR456Q06 $PR456Q06F.
PR466Q02 $PR466Q02F.
PR466Q03T $PR466Q03TF.
PR466Q06 $PR466Q06F.
PS131Q02D $PS131Q02DF.
PS131Q04D $PS131Q04DF.
PS256Q01 $PS256Q01F.
PS269Q01 $PS269Q01F.
PS269Q03D $PS269Q03DF.
PS269Q04T $PS269Q04TF.
PS326Q01 $PS326Q01F.
PS326Q02 $PS326Q02F.
PS326Q03 $PS326Q03F.
PS326Q04T $PS326Q04TF.
PS408Q01 $PS408Q01F.
PS408Q03 $PS408Q03F.
PS408Q04T $PS408Q04TF.
PS408Q05 $PS408Q05F.
PS413Q04T $PS413Q04TF.
PS413Q05 $PS413Q05F.
PS413Q06 $PS413Q06F.
PS415Q02 $PS415Q02F.
PS415Q07T $PS415Q07TF.
PS415Q08T $PS415Q08TF.
PS425Q02 $PS425Q02F.
PS425Q03 $PS425Q03F.
PS425Q04 $PS425Q04F.
PS425Q05 $PS425Q05F.
PS428Q01 $PS428Q01F.
PS428Q03 $PS428Q03F.
PS428Q05 $PS428Q05F.
PS438Q01T $PS438Q01TF.
PS438Q02 $PS438Q02F.
PS438Q03D $PS438Q03DF.
PS465Q01 $PS465Q01F.
PS465Q02 $PS465Q02F.
PS465Q04 $PS465Q04F.
PS466Q01T $PS466Q01TF.
PS466Q05 $PS466Q05F.
PS466Q07T $PS466Q07TF.
PS478Q01 $PS478Q01F.
PS478Q02T $PS478Q02TF.
PS478Q03T $PS478Q03TF.
PS498Q02T $PS498Q02TF.
PS498Q03 $PS498Q03F.
PS498Q04 $PS498Q04F.
PS514Q02 $PS514Q02F.
PS514Q03 $PS514Q03F.
PS514Q04 $PS514Q04F.
PS519Q01 $PS519Q01F.
PS519Q02T $PS519Q02TF.
PS519Q03 $PS519Q03F.
PS521Q02 $PS521Q02F.
PS521Q06 $PS521Q06F.
PS527Q01T $PS527Q01TF.
PS527Q03T $PS527Q03TF.
PS527Q04T $PS527Q04TF.
PM155Q02R $PM155Q02RF.
PM155Q03R $PM155Q03RF.
PM462Q01R $PM462Q01RF.
PM953Q04R $PM953Q04RF.
PM991Q02R $PM991Q02RF.
PS131Q02R $PS131Q02RF.
PS131Q04R $PS131Q04RF.
PS269Q03R $PS269Q03RF.
PS438Q03R $PS438Q03RF.
PM034Q01R PM034Q01RF.
PM155Q04R $PM155Q04RF.
PM192Q01R $PM192Q01RF.
PM273Q01R $PM273Q01RF.
PM408Q01R $PM408Q01RF.
PM420Q01R $PM420Q01RF.
PM464Q01R PM464Q01RF.
PM496Q01R $PM496Q01RF.
PM603Q01R $PM603Q01RF.
PM803Q01R PM803Q01RF.
PM905Q01R $PM905Q01RF.
PM949Q01R $PM949Q01RF.
PM949Q02R $PM949Q02RF.
PM967Q03R $PM967Q03RF.
PM982Q03R $PM982Q03RF.
PM998Q04R $PM998Q04RF.
PR404Q07R $PR404Q07RF.
PR412Q06R $PR412Q06RF.
PR424Q02R $PR424Q02RF.
PR432Q06R $PR432Q06RF.
PR453Q05R $PR453Q05RF.
PR455Q05R $PR455Q05RF.
PR466Q03R $PR466Q03RF.
PS269Q04R $PS269Q04RF.
PS326Q04R $PS326Q04RF.
PS408Q04R $PS408Q04RF.
PS413Q04R $PS413Q04RF.
PS415Q07R $PS415Q07RF.
PS415Q08R $PS415Q08RF.
PS438Q01R $PS438Q01RF.
PS466Q01R $PS466Q01RF.
PS466Q07R $PS466Q07RF.
PS478Q02R $PS478Q02RF.
PS478Q03R $PS478Q03RF.
PS498Q02R $PS498Q02RF.
PS519Q02R $PS519Q02RF.
PS527Q01R $PS527Q01RF.
PS527Q03R $PS527Q03RF.
PS527Q04R $PS527Q04RF.
TESTLANG $TESTLANGF.
;
if BOOKID*1=97 or BOOKID*1=. then BOOKID=.N;if BOOKID*1=98 then BOOKID=.I;if BOOKID*1=99 then BOOKID=.M;
if PM034Q01R*1=9997 or PM034Q01R*1=. then PM034Q01R=.N;if PM034Q01R*1=9998 then PM034Q01R=.I;if PM034Q01R*1=9999 then PM034Q01R=.M;
if PM464Q01R*1=9997 or PM464Q01R*1=. then PM464Q01R=.N;if PM464Q01R*1=9998 then PM464Q01R=.I;if PM464Q01R*1=9999 then PM464Q01R=.M;
if PM803Q01R*1=9997 or PM803Q01R*1=. then PM803Q01R=.N;if PM803Q01R*1=9998 then PM803Q01R=.I;if PM803Q01R*1=9999 then PM803Q01R=.M;
run; 

