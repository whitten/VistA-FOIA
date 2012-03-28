BGPUXG ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON JUL 05, 2004 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;BGP URINE PROTEIN LOINC CODES
 ;
 ; This routine loads Taxonomy BGP URINE PROTEIN LOINC CODES
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 D OTHER
 I $O(^TMP("ATX",$J,3.6,0)) D BULL^ATXSTX2
 I $O(^TMP("ATX",$J,9002226,0)) D TAX^ATXSTX2
 D KILL^ATXSTX2
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"12777-9 ")
 ;;1
 ;;21,"12778-7 ")
 ;;2
 ;;21,"12842-1 ")
 ;;3
 ;;21,"13438-7 ")
 ;;4
 ;;21,"13705-9 ")
 ;;5
 ;;21,"13718-2 ")
 ;;6
 ;;21,"13801-6 ")
 ;;7
 ;;21,"13984-0 ")
 ;;8
 ;;21,"13985-7 ")
 ;;9
 ;;21,"13986-5 ")
 ;;10
 ;;21,"13987-3 ")
 ;;11
 ;;21,"13988-1 ")
 ;;12
 ;;21,"13989-9 ")
 ;;13
 ;;21,"13990-7 ")
 ;;14
 ;;21,"13991-5 ")
 ;;15
 ;;21,"13992-3 ")
 ;;16
 ;;21,"13993-1 ")
 ;;17
 ;;21,"13994-9 ")
 ;;18
 ;;21,"13995-6 ")
 ;;19
 ;;21,"14341-2 ")
 ;;20
 ;;21,"14585-4 ")
 ;;21
 ;;21,"14795-9 ")
 ;;22
 ;;21,"14896-5 ")
 ;;23
 ;;21,"14956-7 ")
 ;;24
 ;;21,"14957-5 ")
 ;;25
 ;;21,"14958-3 ")
 ;;26
 ;;21,"14959-1 ")
 ;;27
 ;;21,"14977-3 ")
 ;;28
 ;;21,"15187-8 ")
 ;;29
 ;;21,"15188-6 ")
 ;;30
 ;;21,"16285-9 ")
 ;;31
 ;;21,"1753-3 ")
 ;;32
 ;;21,"1754-1 ")
 ;;33
 ;;21,"1755-8 ")
 ;;34
 ;;21,"1757-4 ")
 ;;35
 ;;21,"17793-1 ")
 ;;36
 ;;21,"17811-1 ")
 ;;37
 ;;21,"17813-7 ")
 ;;38
 ;;21,"17815-2 ")
 ;;39
 ;;21,"17817-8 ")
 ;;40
 ;;21,"17819-4 ")
 ;;41
 ;;21,"17867-3 ")
 ;;42
 ;;21,"18362-4 ")
 ;;43
 ;;21,"18373-1 ")
 ;;44
 ;;21,"1928-1 ")
 ;;45
 ;;21,"1929-9 ")
 ;;46
 ;;21,"20454-5 ")
 ;;47
 ;;21,"20621-9 ")
 ;;48
 ;;21,"21028-6 ")
 ;;49
 ;;21,"21059-1 ")
 ;;50
 ;;21,"21482-5 ")
 ;;51
 ;;21,"22064-0 ")
 ;;52
 ;;21,"22704-1 ")
 ;;53
 ;;21,"25682-6 ")
 ;;54
 ;;21,"25684-2 ")
 ;;55
 ;;21,"26034-9 ")
 ;;56
 ;;21,"26801-1 ")
 ;;57
 ;;21,"27184-1 ")
 ;;58
 ;;21,"27298-9 ")
 ;;59
 ;;21,"27365-6 ")
 ;;60
 ;;21,"27394-6 ")
 ;;61
 ;;21,"2860-5 ")
 ;;62
 ;;21,"2887-8 ")
 ;;63
 ;;21,"2888-6 ")
 ;;64
 ;;21,"2889-4 ")
 ;;65
 ;;21,"2890-2 ")
 ;;66
 ;;21,"29899-2 ")
 ;;67
 ;;21,"29945-3 ")
 ;;68
 ;;21,"29946-1 ")
 ;;69
 ;;21,"29947-9 ")
 ;;70
 ;;21,"29951-1 ")
 ;;71
 ;;21,"30000-4 ")
 ;;72
 ;;21,"30001-2 ")
 ;;73
 ;;21,"30003-8 ")
 ;;74
 ;;21,"31134-0 ")
 ;;75
 ;;21,"31152-2 ")
 ;;76
 ;;21,"32209-9 ")
 ;;77
 ;;21,"32210-7 ")
 ;;78
 ;;21,"32294-1 ")
 ;;79
 ;;21,"32551-4 ")
 ;;80
 ;;21,"5804-0 ")
 ;;81
 ;;21,"6786-8 ")
 ;;82
 ;;21,"6790-0 ")
 ;;83
 ;;21,"6794-2 ")
 ;;84
 ;;21,"6795-9 ")
 ;;85
 ;;21,"6888-2 ")
 ;;86
 ;;21,"6889-0 ")
 ;;87
 ;;21,"6890-8 ")
 ;;88
 ;;21,"6941-9 ")
 ;;89
 ;;21,"6942-7 ")
 ;;90
 ;;21,"9318-7 ")
 ;;91
 ;;21,"9405-2 ")
 ;;92
 ;;21,"9406-0 ")
 ;;93
 ;;21,"9734-5 ")
 ;;94
 ;;21,"9744-4 ")
 ;;95
 ;;21,"9745-1 ")
 ;;96
 ;;21,"9829-3 ")
 ;;97
 ;;21,"9831-9 ")
 ;;98
 ;;9002226,808,.01)
 ;;BGP URINE PROTEIN LOINC CODES
 ;;9002226,808,.02)
 ;;@
 ;;9002226,808,.04)
 ;;@
 ;;9002226,808,.06)
 ;;@
 ;;9002226,808,.08)
 ;;@
 ;;9002226,808,.09)
 ;;@
 ;;9002226,808,.11)
 ;;@
 ;;9002226,808,.12)
 ;;@
 ;;9002226,808,.13)
 ;;1
 ;;9002226,808,.14)
 ;;FIHS
 ;;9002226,808,.15)
 ;;95.3
 ;;9002226,808,.16)
 ;;@
 ;;9002226,808,.17)
 ;;@
 ;;9002226,808,3101)
 ;;@
 ;;9002226.02101,"808,12777-9 ",.01)
 ;;12777-9
 ;;9002226.02101,"808,12777-9 ",.02)
 ;;12777-9
 ;;9002226.02101,"808,12778-7 ",.01)
 ;;12778-7
 ;;9002226.02101,"808,12778-7 ",.02)
 ;;12778-7
 ;;9002226.02101,"808,12842-1 ",.01)
 ;;12842-1
 ;;9002226.02101,"808,12842-1 ",.02)
 ;;12842-1
 ;;9002226.02101,"808,13438-7 ",.01)
 ;;13438-7
 ;;9002226.02101,"808,13438-7 ",.02)
 ;;13438-7
 ;;9002226.02101,"808,13705-9 ",.01)
 ;;13705-9
 ;;9002226.02101,"808,13705-9 ",.02)
 ;;13705-9
 ;;9002226.02101,"808,13718-2 ",.01)
 ;;13718-2
 ;;9002226.02101,"808,13718-2 ",.02)
 ;;13718-2
 ;;9002226.02101,"808,13801-6 ",.01)
 ;;13801-6
 ;;9002226.02101,"808,13801-6 ",.02)
 ;;13801-6
 ;;9002226.02101,"808,13984-0 ",.01)
 ;;13984-0
 ;;9002226.02101,"808,13984-0 ",.02)
 ;;13984-0
 ;;9002226.02101,"808,13985-7 ",.01)
 ;;13985-7
 ;;9002226.02101,"808,13985-7 ",.02)
 ;;13985-7
 ;;9002226.02101,"808,13986-5 ",.01)
 ;;13986-5
 ;;9002226.02101,"808,13986-5 ",.02)
 ;;13986-5
 ;;9002226.02101,"808,13987-3 ",.01)
 ;;13987-3
 ;;9002226.02101,"808,13987-3 ",.02)
 ;;13987-3
 ;;9002226.02101,"808,13988-1 ",.01)
 ;;13988-1
 ;;9002226.02101,"808,13988-1 ",.02)
 ;;13988-1
 ;;9002226.02101,"808,13989-9 ",.01)
 ;;13989-9
 ;;9002226.02101,"808,13989-9 ",.02)
 ;;13989-9
 ;;9002226.02101,"808,13990-7 ",.01)
 ;;13990-7
 ;;9002226.02101,"808,13990-7 ",.02)
 ;;13990-7
 ;;9002226.02101,"808,13991-5 ",.01)
 ;;13991-5
 ;;9002226.02101,"808,13991-5 ",.02)
 ;;13991-5
 ;;9002226.02101,"808,13992-3 ",.01)
 ;;13992-3
 ;;9002226.02101,"808,13992-3 ",.02)
 ;;13992-3
 ;;9002226.02101,"808,13993-1 ",.01)
 ;;13993-1
 ;;9002226.02101,"808,13993-1 ",.02)
 ;;13993-1
 ;;9002226.02101,"808,13994-9 ",.01)
 ;;13994-9
 ;;9002226.02101,"808,13994-9 ",.02)
 ;;13994-9
 ;;9002226.02101,"808,13995-6 ",.01)
 ;;13995-6
 ;;9002226.02101,"808,13995-6 ",.02)
 ;;13995-6
 ;
OTHER ; OTHER ROUTINES
 D ^BGPUXGB
 D ^BGPUXGC
 Q
