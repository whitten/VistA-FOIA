BQIBTXF ;VNGT/HS/ALA-CREATED BY ^ATXSTX ON JUN 18, 2008;
 ;;2.1;ICARE MANAGEMENT SYSTEM;;Feb 07, 2011
 ;;BKMV MAC PROPH MED NDCS
 ;
 ; This routine loads Taxonomy BKMV MAC PROPH MED NDCS
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
 ;;21,"00013-5301-17 ")
 ;;1
 ;;21,"00069-3051-75 ")
 ;;2
 ;;21,"00069-3060-30 ")
 ;;3
 ;;21,"00069-3060-75 ")
 ;;4
 ;;21,"00069-3060-86 ")
 ;;5
 ;;21,"00069-3070-30 ")
 ;;6
 ;;21,"00069-3070-75 ")
 ;;7
 ;;21,"00069-3070-86 ")
 ;;8
 ;;21,"00069-3080-30 ")
 ;;9
 ;;21,"00069-3150-14 ")
 ;;10
 ;;21,"00069-3150-83 ")
 ;;11
 ;;21,"00074-2586-11 ")
 ;;12
 ;;21,"00074-2586-60 ")
 ;;13
 ;;21,"00074-3163-13 ")
 ;;14
 ;;21,"00074-3163-50 ")
 ;;15
 ;;21,"00074-3165-11 ")
 ;;16
 ;;21,"00074-3165-41 ")
 ;;17
 ;;21,"00074-3165-60 ")
 ;;18
 ;;21,"00074-3188-13 ")
 ;;19
 ;;21,"00074-3188-50 ")
 ;;20
 ;;21,"00074-3368-11 ")
 ;;21
 ;;21,"00074-3368-60 ")
 ;;22
 ;;9002226,872,.01)
 ;;BKMV MAC PROPH MED NDCS
 ;;9002226,872,.02)
 ;;MAC PROPH MED NDCS
 ;;9002226,872,.04)
 ;;@
 ;;9002226,872,.06)
 ;;@
 ;;9002226,872,.08)
 ;;0
 ;;9002226,872,.09)
 ;;3050209
 ;;9002226,872,.11)
 ;;@
 ;;9002226,872,.12)
 ;;145
 ;;9002226,872,.13)
 ;;1
 ;;9002226,872,.14)
 ;;@
 ;;9002226,872,.15)
 ;;2
 ;;9002226,872,.16)
 ;;@
 ;;9002226,872,.17)
 ;;@
 ;;9002226,872,3101)
 ;;@
 ;;9002226.02101,"872,00013-5301-17 ",.01)
 ;;00013-5301-17
 ;;9002226.02101,"872,00013-5301-17 ",.02)
 ;;00013-5301-17
 ;;9002226.02101,"872,00069-3051-75 ",.01)
 ;;00069-3051-75
 ;;9002226.02101,"872,00069-3051-75 ",.02)
 ;;00069-3051-75
 ;;9002226.02101,"872,00069-3060-30 ",.01)
 ;;00069-3060-30
 ;;9002226.02101,"872,00069-3060-30 ",.02)
 ;;00069-3060-30
 ;;9002226.02101,"872,00069-3060-75 ",.01)
 ;;00069-3060-75
 ;;9002226.02101,"872,00069-3060-75 ",.02)
 ;;00069-3060-75
 ;;9002226.02101,"872,00069-3060-86 ",.01)
 ;;00069-3060-86
 ;;9002226.02101,"872,00069-3060-86 ",.02)
 ;;00069-3060-86
 ;;9002226.02101,"872,00069-3070-30 ",.01)
 ;;00069-3070-30
 ;;9002226.02101,"872,00069-3070-30 ",.02)
 ;;00069-3070-30
 ;;9002226.02101,"872,00069-3070-75 ",.01)
 ;;00069-3070-75
 ;;9002226.02101,"872,00069-3070-75 ",.02)
 ;;00069-3070-75
 ;;9002226.02101,"872,00069-3070-86 ",.01)
 ;;00069-3070-86
 ;;9002226.02101,"872,00069-3070-86 ",.02)
 ;;00069-3070-86
 ;;9002226.02101,"872,00069-3080-30 ",.01)
 ;;00069-3080-30
 ;;9002226.02101,"872,00069-3080-30 ",.02)
 ;;00069-3080-30
 ;;9002226.02101,"872,00069-3150-14 ",.01)
 ;;00069-3150-14
 ;;9002226.02101,"872,00069-3150-14 ",.02)
 ;;00069-3150-14
 ;;9002226.02101,"872,00069-3150-83 ",.01)
 ;;00069-3150-83
 ;;9002226.02101,"872,00069-3150-83 ",.02)
 ;;00069-3150-83
 ;;9002226.02101,"872,00074-2586-11 ",.01)
 ;;00074-2586-11
 ;;9002226.02101,"872,00074-2586-11 ",.02)
 ;;00074-2586-11
 ;;9002226.02101,"872,00074-2586-60 ",.01)
 ;;00074-2586-60
 ;;9002226.02101,"872,00074-2586-60 ",.02)
 ;;00074-2586-60
 ;;9002226.02101,"872,00074-3163-13 ",.01)
 ;;00074-3163-13
 ;;9002226.02101,"872,00074-3163-13 ",.02)
 ;;00074-3163-13
 ;;9002226.02101,"872,00074-3163-50 ",.01)
 ;;00074-3163-50
 ;;9002226.02101,"872,00074-3163-50 ",.02)
 ;;00074-3163-50
 ;;9002226.02101,"872,00074-3165-11 ",.01)
 ;;00074-3165-11
 ;;9002226.02101,"872,00074-3165-11 ",.02)
 ;;00074-3165-11
 ;;9002226.02101,"872,00074-3165-41 ",.01)
 ;;00074-3165-41
 ;;9002226.02101,"872,00074-3165-41 ",.02)
 ;;00074-3165-41
 ;;9002226.02101,"872,00074-3165-60 ",.01)
 ;;00074-3165-60
 ;;9002226.02101,"872,00074-3165-60 ",.02)
 ;;00074-3165-60
 ;;9002226.02101,"872,00074-3188-13 ",.01)
 ;;00074-3188-13
 ;;9002226.02101,"872,00074-3188-13 ",.02)
 ;;00074-3188-13
 ;;9002226.02101,"872,00074-3188-50 ",.01)
 ;;00074-3188-50
 ;;9002226.02101,"872,00074-3188-50 ",.02)
 ;;00074-3188-50
 ;;9002226.02101,"872,00074-3368-11 ",.01)
 ;;00074-3368-11
 ;;9002226.02101,"872,00074-3368-11 ",.02)
 ;;00074-3368-11
 ;;9002226.02101,"872,00074-3368-60 ",.01)
 ;;00074-3368-60
 ;;9002226.02101,"872,00074-3368-60 ",.02)
 ;;00074-3368-60
 ;
OTHER ; OTHER ROUTINES
 Q
