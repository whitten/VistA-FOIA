BGPLXH ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON NOV 18, 2003 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;BGP LIPID PROFILE LOINC CODES
 ;
 ; This routine loads Taxonomy BGP LIPID PROFILE LOINC CODES
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
 ;;21,"11054-4 ")
 ;;1
 ;;21,"12771-2 ")
 ;;2
 ;;21,"12772-0 ")
 ;;3
 ;;21,"12773-8 ")
 ;;4
 ;;21,"13457-7 ")
 ;;5
 ;;21,"13458-5 ")
 ;;6
 ;;21,"13459-3 ")
 ;;7
 ;;21,"14155-6 ")
 ;;8
 ;;21,"14647-2 ")
 ;;9
 ;;21,"16615-7 ")
 ;;10
 ;;21,"16616-5 ")
 ;;11
 ;;21,"2085-9 ")
 ;;12
 ;;21,"2087-5 ")
 ;;13
 ;;21,"2089-1 ")
 ;;14
 ;;21,"2091-7 ")
 ;;15
 ;;21,"2093-3 ")
 ;;16
 ;;21,"2095-8 ")
 ;;17
 ;;21,"2096-6 ")
 ;;18
 ;;21,"22748-8 ")
 ;;19
 ;;21,"24331-1 ")
 ;;20
 ;;21,"25371-6 ")
 ;;21
 ;;21,"2569-2 ")
 ;;22
 ;;21,"2570-0 ")
 ;;23
 ;;21,"26015-8 ")
 ;;24
 ;;21,"26017-4 ")
 ;;25
 ;;21,"27340-9 ")
 ;;26
 ;;21,"32308-9 ")
 ;;27
 ;;21,"32309-7 ")
 ;;28
 ;;21,"5932-9 ")
 ;;29
 ;;21,"9322-9 ")
 ;;30
 ;;21,"9342-7 ")
 ;;31
 ;;21,"9830-1 ")
 ;;32
 ;;21,"9832-7 ")
 ;;33
 ;;21,"9833-5 ")
 ;;34
 ;;9002226,792,.01)
 ;;BGP LIPID PROFILE LOINC CODES
 ;;9002226,792,.02)
 ;;@
 ;;9002226,792,.04)
 ;;@
 ;;9002226,792,.06)
 ;;@
 ;;9002226,792,.08)
 ;;@
 ;;9002226,792,.09)
 ;;@
 ;;9002226,792,.11)
 ;;@
 ;;9002226,792,.12)
 ;;@
 ;;9002226,792,.13)
 ;;1
 ;;9002226,792,.14)
 ;;FIHS
 ;;9002226,792,.15)
 ;;95.3
 ;;9002226,792,.16)
 ;;@
 ;;9002226,792,.17)
 ;;@
 ;;9002226,792,3101)
 ;;@
 ;;9002226.02101,"792,11054-4 ",.01)
 ;;11054-4
 ;;9002226.02101,"792,11054-4 ",.02)
 ;;11054-4
 ;;9002226.02101,"792,12771-2 ",.01)
 ;;12771-2
 ;;9002226.02101,"792,12771-2 ",.02)
 ;;12771-2
 ;;9002226.02101,"792,12772-0 ",.01)
 ;;12772-0
 ;;9002226.02101,"792,12772-0 ",.02)
 ;;12772-0
 ;;9002226.02101,"792,12773-8 ",.01)
 ;;12773-8
 ;;9002226.02101,"792,12773-8 ",.02)
 ;;12773-8
 ;;9002226.02101,"792,13457-7 ",.01)
 ;;13457-7
 ;;9002226.02101,"792,13457-7 ",.02)
 ;;13457-7
 ;;9002226.02101,"792,13458-5 ",.01)
 ;;13458-5
 ;;9002226.02101,"792,13458-5 ",.02)
 ;;13458-5
 ;;9002226.02101,"792,13459-3 ",.01)
 ;;13459-3
 ;;9002226.02101,"792,13459-3 ",.02)
 ;;13459-3
 ;;9002226.02101,"792,14155-6 ",.01)
 ;;14155-6
 ;;9002226.02101,"792,14155-6 ",.02)
 ;;14155-6
 ;;9002226.02101,"792,14647-2 ",.01)
 ;;14647-2
 ;;9002226.02101,"792,14647-2 ",.02)
 ;;14647-2
 ;;9002226.02101,"792,16615-7 ",.01)
 ;;16615-7
 ;;9002226.02101,"792,16615-7 ",.02)
 ;;16615-7
 ;;9002226.02101,"792,16616-5 ",.01)
 ;;16616-5
 ;;9002226.02101,"792,16616-5 ",.02)
 ;;16616-5
 ;;9002226.02101,"792,2085-9 ",.01)
 ;;2085-9
 ;;9002226.02101,"792,2085-9 ",.02)
 ;;2085-9
 ;;9002226.02101,"792,2087-5 ",.01)
 ;;2087-5
 ;;9002226.02101,"792,2087-5 ",.02)
 ;;2087-5
 ;;9002226.02101,"792,2089-1 ",.01)
 ;;2089-1
 ;;9002226.02101,"792,2089-1 ",.02)
 ;;2089-1
 ;;9002226.02101,"792,2091-7 ",.01)
 ;;2091-7
 ;;9002226.02101,"792,2091-7 ",.02)
 ;;2091-7
 ;;9002226.02101,"792,2093-3 ",.01)
 ;;2093-3
 ;;9002226.02101,"792,2093-3 ",.02)
 ;;2093-3
 ;;9002226.02101,"792,2095-8 ",.01)
 ;;2095-8
 ;;9002226.02101,"792,2095-8 ",.02)
 ;;2095-8
 ;;9002226.02101,"792,2096-6 ",.01)
 ;;2096-6
 ;;9002226.02101,"792,2096-6 ",.02)
 ;;2096-6
 ;;9002226.02101,"792,22748-8 ",.01)
 ;;22748-8
 ;;9002226.02101,"792,22748-8 ",.02)
 ;;22748-8
 ;;9002226.02101,"792,24331-1 ",.01)
 ;;24331-1
 ;;9002226.02101,"792,24331-1 ",.02)
 ;;24331-1
 ;;9002226.02101,"792,25371-6 ",.01)
 ;;25371-6
 ;;9002226.02101,"792,25371-6 ",.02)
 ;;25371-6
 ;;9002226.02101,"792,2569-2 ",.01)
 ;;2569-2
 ;;9002226.02101,"792,2569-2 ",.02)
 ;;2569-2
 ;;9002226.02101,"792,2570-0 ",.01)
 ;;2570-0
 ;;9002226.02101,"792,2570-0 ",.02)
 ;;2570-0
 ;;9002226.02101,"792,26015-8 ",.01)
 ;;26015-8
 ;;9002226.02101,"792,26015-8 ",.02)
 ;;26015-8
 ;;9002226.02101,"792,26017-4 ",.01)
 ;;26017-4
 ;;9002226.02101,"792,26017-4 ",.02)
 ;;26017-4
 ;;9002226.02101,"792,27340-9 ",.01)
 ;;27340-9
 ;;9002226.02101,"792,27340-9 ",.02)
 ;;27340-9
 ;;9002226.02101,"792,32308-9 ",.01)
 ;;32308-9
 ;;9002226.02101,"792,32308-9 ",.02)
 ;;32308-9
 ;;9002226.02101,"792,32309-7 ",.01)
 ;;32309-7
 ;;9002226.02101,"792,32309-7 ",.02)
 ;;32309-7
 ;;9002226.02101,"792,5932-9 ",.01)
 ;;5932-9
 ;;9002226.02101,"792,5932-9 ",.02)
 ;;5932-9
 ;;9002226.02101,"792,9322-9 ",.01)
 ;;9322-9
 ;;9002226.02101,"792,9322-9 ",.02)
 ;;9322-9
 ;;9002226.02101,"792,9342-7 ",.01)
 ;;9342-7
 ;;9002226.02101,"792,9342-7 ",.02)
 ;;9342-7
 ;;9002226.02101,"792,9830-1 ",.01)
 ;;9830-1
 ;;9002226.02101,"792,9830-1 ",.02)
 ;;9830-1
 ;;9002226.02101,"792,9832-7 ",.01)
 ;;9832-7
 ;;9002226.02101,"792,9832-7 ",.02)
 ;;9832-7
 ;;9002226.02101,"792,9833-5 ",.01)
 ;;9833-5
 ;;9002226.02101,"792,9833-5 ",.02)
 ;;9833-5
 ;
OTHER ; OTHER ROUTINES
 Q
