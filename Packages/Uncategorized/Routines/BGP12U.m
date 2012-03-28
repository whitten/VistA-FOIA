BGP12U ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;;BGP HEDIS ASTHMA LEUK NDC
 ;
 ; This routine loads Taxonomy BGP HEDIS ASTHMA LEUK NDC
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
 ;;21,"00006-0117-01 ")
 ;;23
 ;;21,"00006-0117-28 ")
 ;;24
 ;;21,"00006-0117-31 ")
 ;;25
 ;;21,"00006-0117-54 ")
 ;;26
 ;;21,"00006-0117-80 ")
 ;;27
 ;;21,"00006-0275-01 ")
 ;;28
 ;;21,"00006-0275-28 ")
 ;;29
 ;;21,"00006-0275-31 ")
 ;;30
 ;;21,"00006-0275-54 ")
 ;;31
 ;;21,"00006-0275-82 ")
 ;;32
 ;;21,"00006-0711-01 ")
 ;;33
 ;;21,"00006-0711-28 ")
 ;;34
 ;;21,"00006-0711-31 ")
 ;;35
 ;;21,"00006-0711-54 ")
 ;;36
 ;;21,"00006-3841-01 ")
 ;;37
 ;;21,"00006-3841-30 ")
 ;;38
 ;;21,"00074-8036-22 ")
 ;;83
 ;;21,"00247-1897-00 ")
 ;;1
 ;;21,"00247-1897-30 ")
 ;;2
 ;;21,"00247-1897-60 ")
 ;;3
 ;;21,"00247-1897-77 ")
 ;;4
 ;;21,"00247-1897-90 ")
 ;;5
 ;;21,"00247-1898-00 ")
 ;;6
 ;;21,"00247-1898-14 ")
 ;;7
 ;;21,"00247-1898-30 ")
 ;;8
 ;;21,"00247-1898-60 ")
 ;;9
 ;;21,"00247-1898-77 ")
 ;;10
 ;;21,"00247-1898-90 ")
 ;;11
 ;;21,"00247-1988-30 ")
 ;;39
 ;;21,"00310-0401-39 ")
 ;;12
 ;;21,"00310-0401-60 ")
 ;;13
 ;;21,"00310-0402-39 ")
 ;;14
 ;;21,"00310-0402-60 ")
 ;;15
 ;;21,"10122-0901-12 ")
 ;;84
 ;;21,"10122-0902-12 ")
 ;;86
 ;;21,"12280-0042-90 ")
 ;;40
 ;;21,"13411-0151-01 ")
 ;;41
 ;;21,"13411-0151-03 ")
 ;;42
 ;;21,"13411-0151-06 ")
 ;;43
 ;;21,"13411-0151-09 ")
 ;;44
 ;;21,"13411-0151-15 ")
 ;;45
 ;;21,"13411-0160-01 ")
 ;;46
 ;;21,"13411-0160-03 ")
 ;;47
 ;;21,"13411-0160-06 ")
 ;;48
 ;;21,"13411-0160-09 ")
 ;;49
 ;;21,"13411-0160-15 ")
 ;;50
 ;;21,"49999-0533-30 ")
 ;;51
 ;;21,"49999-0533-90 ")
 ;;52
 ;;21,"49999-0884-30 ")
 ;;53
 ;;21,"49999-0884-90 ")
 ;;54
 ;;21,"49999-0952-30 ")
 ;;55
 ;;21,"54569-4604-01 ")
 ;;16
 ;;21,"54569-4605-00 ")
 ;;56
 ;;21,"54569-4736-00 ")
 ;;57
 ;;21,"54868-3283-00 ")
 ;;58
 ;;21,"54868-3283-01 ")
 ;;59
 ;;21,"54868-3283-02 ")
 ;;60
 ;;21,"54868-4172-00 ")
 ;;17
 ;;21,"54868-4172-01 ")
 ;;18
 ;;21,"54868-4172-02 ")
 ;;19
 ;;21,"54868-4630-00 ")
 ;;61
 ;;21,"54868-4847-00 ")
 ;;62
 ;;21,"55289-0961-15 ")
 ;;63
 ;;21,"55289-0961-30 ")
 ;;64
 ;;21,"55289-0989-21 ")
 ;;65
 ;;21,"55289-0989-30 ")
 ;;66
 ;;21,"55289-0990-21 ")
 ;;67
 ;;21,"55289-0990-30 ")
 ;;68
 ;;21,"55887-0120-90 ")
 ;;69
 ;;21,"58864-0658-30 ")
 ;;70
 ;;21,"58864-0694-30 ")
 ;;71
 ;;21,"63629-1639-01 ")
 ;;72
 ;;21,"66105-0164-02 ")
 ;;73
 ;;21,"66105-0164-03 ")
 ;;74
 ;;21,"66105-0164-06 ")
 ;;75
 ;;21,"66105-0164-09 ")
 ;;76
 ;;21,"66105-0164-10 ")
 ;;77
 ;;21,"66105-0501-06 ")
 ;;20
 ;;21,"66105-0502-06 ")
 ;;21
 ;;21,"67801-0305-03 ")
 ;;78
 ;;21,"68115-0638-60 ")
 ;;22
 ;;21,"68115-0923-30 ")
 ;;79
 ;;21,"68115-0923-90 ")
 ;;80
 ;;21,"68258-3032-03 ")
 ;;81
 ;;21,"68258-3033-03 ")
 ;;82
 ;;21,"68734-0700-10 ")
 ;;85
 ;;21,"68734-0710-10 ")
 ;;87
 ;;9002226,532,.01)
 ;;BGP HEDIS ASTHMA LEUK NDC
 ;;9002226,532,.02)
 ;;@
 ;;9002226,532,.04)
 ;;n
 ;;9002226,532,.06)
 ;;@
 ;;9002226,532,.08)
 ;;@
 ;;9002226,532,.09)
 ;;3080528
 ;;9002226,532,.11)
 ;;@
 ;;9002226,532,.12)
 ;;@
 ;;9002226,532,.13)
 ;;1
 ;;9002226,532,.14)
 ;;@
 ;;9002226,532,.15)
 ;;@
 ;;9002226,532,.16)
 ;;@
 ;;9002226,532,.17)
 ;;@
 ;;9002226,532,3101)
 ;;@
 ;;9002226.02101,"532,00006-0117-01 ",.01)
 ;;00006-0117-01
 ;;9002226.02101,"532,00006-0117-01 ",.02)
 ;;00006-0117-01
 ;;9002226.02101,"532,00006-0117-28 ",.01)
 ;;00006-0117-28
 ;;9002226.02101,"532,00006-0117-28 ",.02)
 ;;00006-0117-28
 ;;9002226.02101,"532,00006-0117-31 ",.01)
 ;;00006-0117-31
 ;;9002226.02101,"532,00006-0117-31 ",.02)
 ;;00006-0117-31
 ;;9002226.02101,"532,00006-0117-54 ",.01)
 ;;00006-0117-54
 ;;9002226.02101,"532,00006-0117-54 ",.02)
 ;;00006-0117-54
 ;;9002226.02101,"532,00006-0117-80 ",.01)
 ;;00006-0117-80
 ;;9002226.02101,"532,00006-0117-80 ",.02)
 ;;00006-0117-80
 ;;9002226.02101,"532,00006-0275-01 ",.01)
 ;;00006-0275-01
 ;;9002226.02101,"532,00006-0275-01 ",.02)
 ;;00006-0275-01
 ;;9002226.02101,"532,00006-0275-28 ",.01)
 ;;00006-0275-28
 ;;9002226.02101,"532,00006-0275-28 ",.02)
 ;;00006-0275-28
 ;;9002226.02101,"532,00006-0275-31 ",.01)
 ;;00006-0275-31
 ;;9002226.02101,"532,00006-0275-31 ",.02)
 ;;00006-0275-31
 ;;9002226.02101,"532,00006-0275-54 ",.01)
 ;;00006-0275-54
 ;;9002226.02101,"532,00006-0275-54 ",.02)
 ;;00006-0275-54
 ;;9002226.02101,"532,00006-0275-82 ",.01)
 ;;00006-0275-82
 ;;9002226.02101,"532,00006-0275-82 ",.02)
 ;;00006-0275-82
 ;;9002226.02101,"532,00006-0711-01 ",.01)
 ;;00006-0711-01
 ;;9002226.02101,"532,00006-0711-01 ",.02)
 ;;00006-0711-01
 ;;9002226.02101,"532,00006-0711-28 ",.01)
 ;;00006-0711-28
 ;;9002226.02101,"532,00006-0711-28 ",.02)
 ;;00006-0711-28
 ;;9002226.02101,"532,00006-0711-31 ",.01)
 ;;00006-0711-31
 ;;9002226.02101,"532,00006-0711-31 ",.02)
 ;;00006-0711-31
 ;;9002226.02101,"532,00006-0711-54 ",.01)
 ;;00006-0711-54
 ;;9002226.02101,"532,00006-0711-54 ",.02)
 ;;00006-0711-54
 ;
OTHER ; OTHER ROUTINES
 D ^BGP12UB
 D ^BGP12UC
 Q
