BGP7KXGH ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 10, 2006 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"352,55175-2575-01 ",.01)
 ;;55175-2575-01
 ;;9002226.02101,"352,55175-2575-01 ",.02)
 ;;55175-2575-01
 ;;9002226.02101,"352,55175-2753-01 ",.01)
 ;;55175-2753-01
 ;;9002226.02101,"352,55175-2753-01 ",.02)
 ;;55175-2753-01
 ;;9002226.02101,"352,55175-4410-01 ",.01)
 ;;55175-4410-01
 ;;9002226.02101,"352,55175-4410-01 ",.02)
 ;;55175-4410-01
 ;;9002226.02101,"352,55175-4417-01 ",.01)
 ;;55175-4417-01
 ;;9002226.02101,"352,55175-4417-01 ",.02)
 ;;55175-4417-01
 ;;9002226.02101,"352,55175-4435-01 ",.01)
 ;;55175-4435-01
 ;;9002226.02101,"352,55175-4435-01 ",.02)
 ;;55175-4435-01
 ;;9002226.02101,"352,55175-4460-01 ",.01)
 ;;55175-4460-01
 ;;9002226.02101,"352,55175-4460-01 ",.02)
 ;;55175-4460-01
 ;;9002226.02101,"352,55175-4461-01 ",.01)
 ;;55175-4461-01
 ;;9002226.02101,"352,55175-4461-01 ",.02)
 ;;55175-4461-01
 ;;9002226.02101,"352,55175-4465-01 ",.01)
 ;;55175-4465-01
 ;;9002226.02101,"352,55175-4465-01 ",.02)
 ;;55175-4465-01
 ;;9002226.02101,"352,55175-4466-01 ",.01)
 ;;55175-4466-01
 ;;9002226.02101,"352,55175-4466-01 ",.02)
 ;;55175-4466-01
 ;;9002226.02101,"352,55175-5205-01 ",.01)
 ;;55175-5205-01
 ;;9002226.02101,"352,55175-5205-01 ",.02)
 ;;55175-5205-01
 ;;9002226.02101,"352,55175-5245-01 ",.01)
 ;;55175-5245-01
 ;;9002226.02101,"352,55175-5245-01 ",.02)
 ;;55175-5245-01
 ;;9002226.02101,"352,55175-5247-01 ",.01)
 ;;55175-5247-01
 ;;9002226.02101,"352,55175-5247-01 ",.02)
 ;;55175-5247-01
 ;;9002226.02101,"352,55887-0726-20 ",.01)
 ;;55887-0726-20
 ;;9002226.02101,"352,55887-0726-20 ",.02)
 ;;55887-0726-20
 ;;9002226.02101,"352,55947-0206-15 ",.01)
 ;;55947-0206-15
 ;;9002226.02101,"352,55947-0206-15 ",.02)
 ;;55947-0206-15
 ;;9002226.02101,"352,55953-0051-01 ",.01)
 ;;55953-0051-01
 ;;9002226.02101,"352,55953-0051-01 ",.02)
 ;;55953-0051-01
 ;;9002226.02101,"352,55953-0051-53 ",.01)
 ;;55953-0051-53
 ;;9002226.02101,"352,55953-0051-53 ",.02)
 ;;55953-0051-53
 ;;9002226.02101,"352,55953-0209-25 ",.01)
 ;;55953-0209-25
 ;;9002226.02101,"352,55953-0209-25 ",.02)
 ;;55953-0209-25
 ;;9002226.02101,"352,55953-0212-20 ",.01)
 ;;55953-0212-20
 ;;9002226.02101,"352,55953-0212-20 ",.02)
 ;;55953-0212-20
 ;;9002226.02101,"352,57362-0105-01 ",.01)
 ;;57362-0105-01
 ;;9002226.02101,"352,57362-0105-01 ",.02)
 ;;57362-0105-01
 ;;9002226.02101,"352,57362-0116-01 ",.01)
 ;;57362-0116-01
 ;;9002226.02101,"352,57362-0116-01 ",.02)
 ;;57362-0116-01
 ;;9002226.02101,"352,57362-0326-55 ",.01)
 ;;57362-0326-55
 ;;9002226.02101,"352,57362-0326-55 ",.02)
 ;;57362-0326-55
 ;;9002226.02101,"352,57866-0051-01 ",.01)
 ;;57866-0051-01
 ;;9002226.02101,"352,57866-0051-01 ",.02)
 ;;57866-0051-01
 ;;9002226.02101,"352,58016-4604-01 ",.01)
 ;;58016-4604-01
 ;;9002226.02101,"352,58016-4604-01 ",.02)
 ;;58016-4604-01
 ;;9002226.02101,"352,58016-5018-01 ",.01)
 ;;58016-5018-01
 ;;9002226.02101,"352,58016-5018-01 ",.02)
 ;;58016-5018-01
 ;;9002226.02101,"352,58016-5018-20 ",.01)
 ;;58016-5018-20
 ;;9002226.02101,"352,58016-5018-20 ",.02)
 ;;58016-5018-20
 ;;9002226.02101,"352,58016-6002-01 ",.01)
 ;;58016-6002-01
 ;;9002226.02101,"352,58016-6002-01 ",.02)
 ;;58016-6002-01
 ;;9002226.02101,"352,58016-6059-01 ",.01)
 ;;58016-6059-01
 ;;9002226.02101,"352,58016-6059-01 ",.02)
 ;;58016-6059-01
 ;;9002226.02101,"352,58016-6059-17 ",.01)
 ;;58016-6059-17
 ;;9002226.02101,"352,58016-6059-17 ",.02)
 ;;58016-6059-17
 ;;9002226.02101,"352,58016-6099-01 ",.01)
 ;;58016-6099-01
 ;;9002226.02101,"352,58016-6099-01 ",.02)
 ;;58016-6099-01
 ;;9002226.02101,"352,58016-6109-01 ",.01)
 ;;58016-6109-01
 ;;9002226.02101,"352,58016-6109-01 ",.02)
 ;;58016-6109-01
 ;;9002226.02101,"352,58016-6205-01 ",.01)
 ;;58016-6205-01
 ;;9002226.02101,"352,58016-6205-01 ",.02)
 ;;58016-6205-01
 ;;9002226.02101,"352,58016-6207-00 ",.01)
 ;;58016-6207-00
 ;;9002226.02101,"352,58016-6207-00 ",.02)
 ;;58016-6207-00
 ;;9002226.02101,"352,58016-6207-01 ",.01)
 ;;58016-6207-01
 ;;9002226.02101,"352,58016-6207-01 ",.02)
 ;;58016-6207-01
 ;;9002226.02101,"352,58016-6237-01 ",.01)
 ;;58016-6237-01
 ;;9002226.02101,"352,58016-6237-01 ",.02)
 ;;58016-6237-01
 ;;9002226.02101,"352,58016-6315-01 ",.01)
 ;;58016-6315-01
 ;;9002226.02101,"352,58016-6315-01 ",.02)
 ;;58016-6315-01
 ;;9002226.02101,"352,58016-6316-17 ",.01)
 ;;58016-6316-17
 ;;9002226.02101,"352,58016-6316-17 ",.02)
 ;;58016-6316-17
 ;;9002226.02101,"352,58016-6318-01 ",.01)
 ;;58016-6318-01
 ;;9002226.02101,"352,58016-6318-01 ",.02)
 ;;58016-6318-01
 ;;9002226.02101,"352,58016-6318-10 ",.01)
 ;;58016-6318-10
 ;;9002226.02101,"352,58016-6318-10 ",.02)
 ;;58016-6318-10
 ;;9002226.02101,"352,58016-6404-01 ",.01)
 ;;58016-6404-01
 ;;9002226.02101,"352,58016-6404-01 ",.02)
 ;;58016-6404-01
 ;;9002226.02101,"352,58016-6404-20 ",.01)
 ;;58016-6404-20
 ;;9002226.02101,"352,58016-6404-20 ",.02)
 ;;58016-6404-20
 ;;9002226.02101,"352,58016-6523-60 ",.01)
 ;;58016-6523-60
 ;;9002226.02101,"352,58016-6523-60 ",.02)
 ;;58016-6523-60
 ;;9002226.02101,"352,58016-6537-00 ",.01)
 ;;58016-6537-00
 ;;9002226.02101,"352,58016-6537-00 ",.02)
 ;;58016-6537-00
 ;;9002226.02101,"352,58016-6537-01 ",.01)
 ;;58016-6537-01
 ;;9002226.02101,"352,58016-6537-01 ",.02)
 ;;58016-6537-01
 ;;9002226.02101,"352,58016-6537-14 ",.01)
 ;;58016-6537-14
 ;;9002226.02101,"352,58016-6537-14 ",.02)
 ;;58016-6537-14
 ;;9002226.02101,"352,58016-6569-01 ",.01)
 ;;58016-6569-01
 ;;9002226.02101,"352,58016-6569-01 ",.02)
 ;;58016-6569-01
 ;;9002226.02101,"352,58087-0317-20 ",.01)
 ;;58087-0317-20
 ;;9002226.02101,"352,58087-0317-20 ",.02)
 ;;58087-0317-20
 ;;9002226.02101,"352,58979-0050-10 ",.01)
 ;;58979-0050-10
 ;;9002226.02101,"352,58979-0050-10 ",.02)
 ;;58979-0050-10
 ;;9002226.02101,"352,58980-0108-17 ",.01)
 ;;58980-0108-17
 ;;9002226.02101,"352,58980-0108-17 ",.02)
 ;;58980-0108-17
 ;;9002226.02101,"352,59198-0356-04 ",.01)
 ;;59198-0356-04
 ;;9002226.02101,"352,59198-0356-04 ",.02)
 ;;59198-0356-04
 ;;9002226.02101,"352,59310-0175-40 ",.01)
 ;;59310-0175-40
 ;;9002226.02101,"352,59310-0175-40 ",.02)
 ;;59310-0175-40
 ;;9002226.02101,"352,59310-0175-41 ",.01)
 ;;59310-0175-41
 ;;9002226.02101,"352,59310-0175-41 ",.02)
 ;;59310-0175-41
 ;;9002226.02101,"352,59310-0177-80 ",.01)
 ;;59310-0177-80
 ;;9002226.02101,"352,59310-0177-80 ",.02)
 ;;59310-0177-80
 ;;9002226.02101,"352,59310-0177-81 ",.01)
 ;;59310-0177-81
 ;;9002226.02101,"352,59310-0177-81 ",.02)
 ;;59310-0177-81
 ;;9002226.02101,"352,59772-6175-01 ",.01)
 ;;59772-6175-01
 ;;9002226.02101,"352,59772-6175-01 ",.02)
 ;;59772-6175-01
 ;;9002226.02101,"352,59772-6175-02 ",.01)
 ;;59772-6175-02
 ;;9002226.02101,"352,59772-6175-02 ",.02)
 ;;59772-6175-02
 ;;9002226.02101,"352,59930-1500-06 ",.01)
 ;;59930-1500-06
 ;;9002226.02101,"352,59930-1500-06 ",.02)
 ;;59930-1500-06
 ;;9002226.02101,"352,59930-1500-08 ",.01)
 ;;59930-1500-08
 ;;9002226.02101,"352,59930-1500-08 ",.02)
 ;;59930-1500-08
 ;;9002226.02101,"352,59930-1509-01 ",.01)
 ;;59930-1509-01
 ;;9002226.02101,"352,59930-1509-01 ",.02)
 ;;59930-1509-01
 ;;9002226.02101,"352,59930-1509-02 ",.01)
 ;;59930-1509-02
 ;;9002226.02101,"352,59930-1509-02 ",.02)
 ;;59930-1509-02
 ;;9002226.02101,"352,59930-1515-04 ",.01)
 ;;59930-1515-04
 ;;9002226.02101,"352,59930-1515-04 ",.02)
 ;;59930-1515-04
 ;;9002226.02101,"352,59930-1517-01 ",.01)
 ;;59930-1517-01
 ;;9002226.02101,"352,59930-1517-01 ",.02)
 ;;59930-1517-01
 ;;9002226.02101,"352,59930-1517-02 ",.01)
 ;;59930-1517-02
 ;;9002226.02101,"352,59930-1517-02 ",.02)
 ;;59930-1517-02
 ;;9002226.02101,"352,59930-1560-01 ",.01)
 ;;59930-1560-01
 ;;9002226.02101,"352,59930-1560-01 ",.02)
 ;;59930-1560-01
 ;;9002226.02101,"352,59930-1560-02 ",.01)
 ;;59930-1560-02
 ;;9002226.02101,"352,59930-1560-02 ",.02)
 ;;59930-1560-02
 ;;9002226.02101,"352,59930-1647-02 ",.01)
 ;;59930-1647-02
 ;;9002226.02101,"352,59930-1647-02 ",.02)
 ;;59930-1647-02
 ;;9002226.02101,"352,60346-0168-64 ",.01)
 ;;60346-0168-64
 ;;9002226.02101,"352,60346-0168-64 ",.02)
 ;;60346-0168-64
 ;;9002226.02101,"352,60346-0180-46 ",.01)
 ;;60346-0180-46
 ;;9002226.02101,"352,60346-0180-46 ",.02)
 ;;60346-0180-46
 ;;9002226.02101,"352,60346-0207-97 ",.01)
 ;;60346-0207-97
 ;;9002226.02101,"352,60346-0207-97 ",.02)
 ;;60346-0207-97
 ;;9002226.02101,"352,60346-0226-76 ",.01)
 ;;60346-0226-76
 ;;9002226.02101,"352,60346-0226-76 ",.02)
 ;;60346-0226-76
 ;;9002226.02101,"352,60346-0282-74 ",.01)
 ;;60346-0282-74
 ;;9002226.02101,"352,60346-0282-74 ",.02)
 ;;60346-0282-74
 ;;9002226.02101,"352,60346-0331-76 ",.01)
 ;;60346-0331-76
 ;;9002226.02101,"352,60346-0331-76 ",.02)
 ;;60346-0331-76
 ;;9002226.02101,"352,60346-0384-76 ",.01)
 ;;60346-0384-76
 ;;9002226.02101,"352,60346-0384-76 ",.02)
 ;;60346-0384-76
 ;;9002226.02101,"352,60346-0394-76 ",.01)
 ;;60346-0394-76
 ;;9002226.02101,"352,60346-0394-76 ",.02)
 ;;60346-0394-76
 ;;9002226.02101,"352,60346-0408-76 ",.01)
 ;;60346-0408-76
 ;;9002226.02101,"352,60346-0408-76 ",.02)
 ;;60346-0408-76
 ;;9002226.02101,"352,60346-0420-84 ",.01)
 ;;60346-0420-84
 ;;9002226.02101,"352,60346-0420-84 ",.02)
 ;;60346-0420-84
 ;;9002226.02101,"352,60346-0427-47 ",.01)
 ;;60346-0427-47
 ;;9002226.02101,"352,60346-0427-47 ",.02)
 ;;60346-0427-47
 ;;9002226.02101,"352,60432-0094-06 ",.01)
 ;;60432-0094-06
 ;;9002226.02101,"352,60432-0094-06 ",.02)
 ;;60432-0094-06
 ;;9002226.02101,"352,60432-0157-06 ",.01)
 ;;60432-0157-06
 ;;9002226.02101,"352,60432-0157-06 ",.02)
 ;;60432-0157-06
 ;;9002226.02101,"352,60432-0157-21 ",.01)
 ;;60432-0157-21
 ;;9002226.02101,"352,60432-0157-21 ",.02)
 ;;60432-0157-21
 ;;9002226.02101,"352,60432-0676-01 ",.01)
 ;;60432-0676-01
 ;;9002226.02101,"352,60432-0676-01 ",.02)
 ;;60432-0676-01
 ;;9002226.02101,"352,60432-0676-30 ",.01)
 ;;60432-0676-30
 ;;9002226.02101,"352,60432-0676-30 ",.02)
 ;;60432-0676-30
 ;;9002226.02101,"352,60505-0802-01 ",.01)
 ;;60505-0802-01
 ;;9002226.02101,"352,60505-0802-01 ",.02)
 ;;60505-0802-01
 ;;9002226.02101,"352,60505-0802-02 ",.01)
 ;;60505-0802-02
 ;;9002226.02101,"352,60505-0802-02 ",.02)
 ;;60505-0802-02
 ;;9002226.02101,"352,60505-0806-01 ",.01)
 ;;60505-0806-01
 ;;9002226.02101,"352,60505-0806-01 ",.02)
 ;;60505-0806-01
 ;;9002226.02101,"352,60505-0807-01 ",.01)
 ;;60505-0807-01
 ;;9002226.02101,"352,60505-0807-01 ",.02)
 ;;60505-0807-01
 ;;9002226.02101,"352,60505-0808-01 ",.01)
 ;;60505-0808-01
 ;;9002226.02101,"352,60505-0808-01 ",.02)
 ;;60505-0808-01
 ;;9002226.02101,"352,60598-0061-60 ",.01)
 ;;60598-0061-60
 ;;9002226.02101,"352,60598-0061-60 ",.02)
 ;;60598-0061-60
 ;;9002226.02101,"352,60793-0011-08 ",.01)
 ;;60793-0011-08
 ;;9002226.02101,"352,60793-0011-08 ",.02)
 ;;60793-0011-08
 ;;9002226.02101,"352,60793-0011-14 ",.01)
 ;;60793-0011-14
 ;;9002226.02101,"352,60793-0011-14 ",.02)
 ;;60793-0011-14
 ;;9002226.02101,"352,60793-0120-01 ",.01)
 ;;60793-0120-01
 ;;9002226.02101,"352,60793-0120-01 ",.02)
 ;;60793-0120-01
 ;;9002226.02101,"352,60937-0504-00 ",.01)
 ;;60937-0504-00
 ;;9002226.02101,"352,60937-0504-00 ",.02)
 ;;60937-0504-00
 ;;9002226.02101,"352,60937-0509-01 ",.01)
 ;;60937-0509-01
 ;;9002226.02101,"352,60937-0509-01 ",.02)
 ;;60937-0509-01
 ;;9002226.02101,"352,60937-0510-00 ",.01)
 ;;60937-0510-00
 ;;9002226.02101,"352,60937-0510-00 ",.02)
 ;;60937-0510-00
 ;;9002226.02101,"352,60937-0511-00 ",.01)
 ;;60937-0511-00
 ;;9002226.02101,"352,60937-0511-00 ",.02)
 ;;60937-0511-00
 ;;9002226.02101,"352,62037-0794-44 ",.01)
 ;;62037-0794-44
 ;;9002226.02101,"352,62037-0794-44 ",.02)
 ;;62037-0794-44
 ;;9002226.02101,"352,62301-0045-45 ",.01)
 ;;62301-0045-45
 ;;9002226.02101,"352,62301-0045-45 ",.02)
 ;;62301-0045-45
 ;;9002226.02101,"352,62947-6406-05 ",.01)
 ;;62947-6406-05
 ;;9002226.02101,"352,62947-6406-05 ",.02)
 ;;62947-6406-05
 ;;9002226.02101,"352,62947-6406-09 ",.01)
 ;;62947-6406-09
 ;;9002226.02101,"352,62947-6406-09 ",.02)
 ;;62947-6406-09
 ;;9002226.02101,"352,63402-0511-24 ",.01)
 ;;63402-0511-24
 ;;9002226.02101,"352,63402-0511-24 ",.02)
 ;;63402-0511-24
 ;;9002226.02101,"352,63402-0512-24 ",.01)
 ;;63402-0512-24
 ;;9002226.02101,"352,63402-0512-24 ",.02)
 ;;63402-0512-24
 ;;9002226.02101,"352,63402-0513-24 ",.01)
 ;;63402-0513-24
 ;;9002226.02101,"352,63402-0513-24 ",.02)
 ;;63402-0513-24
 ;;9002226.02101,"352,63402-0513-96 ",.01)
 ;;63402-0513-96
 ;;9002226.02101,"352,63402-0513-96 ",.02)
 ;;63402-0513-96
 ;;9002226.02101,"352,63874-0708-20 ",.01)
 ;;63874-0708-20
 ;;9002226.02101,"352,63874-0708-20 ",.02)
 ;;63874-0708-20
 ;;9002226.02101,"352,63874-0749-17 ",.01)
 ;;63874-0749-17
 ;;9002226.02101,"352,63874-0749-17 ",.02)
 ;;63874-0749-17
 ;;9002226.02101,"352,64681-0157-06 ",.01)
 ;;64681-0157-06
 ;;9002226.02101,"352,64681-0157-06 ",.02)
 ;;64681-0157-06
 ;;9002226.02101,"352,64681-0157-21 ",.01)
 ;;64681-0157-21
 ;;9002226.02101,"352,64681-0157-21 ",.02)
 ;;64681-0157-21
 ;;9002226.02101,"352,65271-0001-12 ",.01)
 ;;65271-0001-12
 ;;9002226.02101,"352,65271-0001-12 ",.02)
 ;;65271-0001-12
 ;;9002226.02101,"352,65271-0001-25 ",.01)
 ;;65271-0001-25
 ;;9002226.02101,"352,65271-0001-25 ",.02)
 ;;65271-0001-25
 ;;9002226.02101,"352,65271-0001-30 ",.01)
 ;;65271-0001-30
 ;;9002226.02101,"352,65271-0001-30 ",.02)
 ;;65271-0001-30
 ;;9002226.02101,"352,65271-0001-60 ",.01)
 ;;65271-0001-60
 ;;9002226.02101,"352,65271-0001-60 ",.02)
 ;;65271-0001-60
 ;;9002226.02101,"352,65271-0002-05 ",.01)
 ;;65271-0002-05
 ;;9002226.02101,"352,65271-0002-05 ",.02)
 ;;65271-0002-05
