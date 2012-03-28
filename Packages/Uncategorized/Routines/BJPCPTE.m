BJPCPTE ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON JUL 19, 2009;
 ;;2.0;IHS PCC SUITE;**2**;MAY 14, 2009
 ;;BJPC AC THRPY INDIC DXS
 ;
 ; This routine loads Taxonomy BJPC AC THRPY INDIC DXS
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
 ;;21,"289.81 ")
 ;;1
 ;;21,"415.11 ")
 ;;2
 ;;21,"415.12 ")
 ;;3
 ;;21,"415.19 ")
 ;;4
 ;;21,"425.4 ")
 ;;5
 ;;21,"427.31 ")
 ;;6
 ;;21,"451.0 ")
 ;;7
 ;;21,"451.11 ")
 ;;8
 ;;21,"451.19 ")
 ;;9
 ;;21,"451.2 ")
 ;;10
 ;;21,"451.81 ")
 ;;11
 ;;21,"451.82 ")
 ;;12
 ;;21,"451.83 ")
 ;;13
 ;;21,"451.84 ")
 ;;14
 ;;21,"451.89 ")
 ;;15
 ;;21,"451.9 ")
 ;;16
 ;;21,"453.0 ")
 ;;17
 ;;21,"453.1 ")
 ;;18
 ;;21,"453.2 ")
 ;;19
 ;;21,"453.3 ")
 ;;20
 ;;21,"453.40 ")
 ;;21
 ;;21,"453.41 ")
 ;;22
 ;;21,"453.42 ")
 ;;23
 ;;21,"453.8 ")
 ;;24
 ;;21,"453.9 ")
 ;;25
 ;;21,"V12.52 ")
 ;;26
 ;;21,"V43.3 ")
 ;;27
 ;;21,"V58.61 ")
 ;;28
 ;;9002226,1010,.01)
 ;;BJPC AC THRPY INDIC DXS
 ;;9002226,1010,.02)
 ;;ANTI-COAG INDIC DXS
 ;;9002226,1010,.04)
 ;;@
 ;;9002226,1010,.06)
 ;;@
 ;;9002226,1010,.08)
 ;;0
 ;;9002226,1010,.09)
 ;;3090719
 ;;9002226,1010,.11)
 ;;@
 ;;9002226,1010,.12)
 ;;31
 ;;9002226,1010,.13)
 ;;1
 ;;9002226,1010,.14)
 ;;@
 ;;9002226,1010,.15)
 ;;80
 ;;9002226,1010,.16)
 ;;@
 ;;9002226,1010,.17)
 ;;@
 ;;9002226,1010,3101)
 ;;@
 ;;9002226.02101,"1010,289.81 ",.01)
 ;;289.81 
 ;;9002226.02101,"1010,289.81 ",.02)
 ;;289.81 
 ;;9002226.02101,"1010,415.11 ",.01)
 ;;415.11 
 ;;9002226.02101,"1010,415.11 ",.02)
 ;;415.11 
 ;;9002226.02101,"1010,415.12 ",.01)
 ;;415.12 
 ;;9002226.02101,"1010,415.12 ",.02)
 ;;415.12 
 ;;9002226.02101,"1010,415.19 ",.01)
 ;;415.19 
 ;;9002226.02101,"1010,415.19 ",.02)
 ;;415.19 
 ;;9002226.02101,"1010,425.4 ",.01)
 ;;425.4 
 ;;9002226.02101,"1010,425.4 ",.02)
 ;;425.4 
 ;;9002226.02101,"1010,427.31 ",.01)
 ;;427.31 
 ;;9002226.02101,"1010,427.31 ",.02)
 ;;427.31 
 ;;9002226.02101,"1010,451.0 ",.01)
 ;;451.0 
 ;;9002226.02101,"1010,451.0 ",.02)
 ;;451.0 
 ;;9002226.02101,"1010,451.11 ",.01)
 ;;451.11 
 ;;9002226.02101,"1010,451.11 ",.02)
 ;;451.11 
 ;;9002226.02101,"1010,451.19 ",.01)
 ;;451.19 
 ;;9002226.02101,"1010,451.19 ",.02)
 ;;451.19 
 ;;9002226.02101,"1010,451.2 ",.01)
 ;;451.2 
 ;;9002226.02101,"1010,451.2 ",.02)
 ;;451.2 
 ;;9002226.02101,"1010,451.81 ",.01)
 ;;451.81 
 ;;9002226.02101,"1010,451.81 ",.02)
 ;;451.81 
 ;;9002226.02101,"1010,451.82 ",.01)
 ;;451.82 
 ;;9002226.02101,"1010,451.82 ",.02)
 ;;451.82 
 ;;9002226.02101,"1010,451.83 ",.01)
 ;;451.83 
 ;;9002226.02101,"1010,451.83 ",.02)
 ;;451.83 
 ;;9002226.02101,"1010,451.84 ",.01)
 ;;451.84 
 ;;9002226.02101,"1010,451.84 ",.02)
 ;;451.84 
 ;;9002226.02101,"1010,451.89 ",.01)
 ;;451.89 
 ;;9002226.02101,"1010,451.89 ",.02)
 ;;451.89 
 ;;9002226.02101,"1010,451.9 ",.01)
 ;;451.9 
 ;;9002226.02101,"1010,451.9 ",.02)
 ;;451.9 
 ;;9002226.02101,"1010,453.0 ",.01)
 ;;453.0 
 ;;9002226.02101,"1010,453.0 ",.02)
 ;;453.0 
 ;;9002226.02101,"1010,453.1 ",.01)
 ;;453.1 
 ;;9002226.02101,"1010,453.1 ",.02)
 ;;453.1 
 ;;9002226.02101,"1010,453.2 ",.01)
 ;;453.2 
 ;;9002226.02101,"1010,453.2 ",.02)
 ;;453.2 
 ;;9002226.02101,"1010,453.3 ",.01)
 ;;453.3 
 ;;9002226.02101,"1010,453.3 ",.02)
 ;;453.3 
 ;;9002226.02101,"1010,453.40 ",.01)
 ;;453.40 
 ;;9002226.02101,"1010,453.40 ",.02)
 ;;453.40 
 ;;9002226.02101,"1010,453.41 ",.01)
 ;;453.41 
 ;;9002226.02101,"1010,453.41 ",.02)
 ;;453.41 
 ;;9002226.02101,"1010,453.42 ",.01)
 ;;453.42 
 ;;9002226.02101,"1010,453.42 ",.02)
 ;;453.42 
 ;;9002226.02101,"1010,453.8 ",.01)
 ;;453.8 
 ;;9002226.02101,"1010,453.8 ",.02)
 ;;453.8 
 ;;9002226.02101,"1010,453.9 ",.01)
 ;;453.9 
 ;;9002226.02101,"1010,453.9 ",.02)
 ;;453.9 
 ;;9002226.02101,"1010,V12.52 ",.01)
 ;;V12.52 
 ;;9002226.02101,"1010,V12.52 ",.02)
 ;;V12.52 
 ;;9002226.02101,"1010,V43.3 ",.01)
 ;;V43.3 
 ;;9002226.02101,"1010,V43.3 ",.02)
 ;;V43.3 
 ;;9002226.02101,"1010,V58.61 ",.01)
 ;;V58.61 
 ;;9002226.02101,"1010,V58.61 ",.02)
 ;;V58.61 
 ;
OTHER ; OTHER ROUTINES
 Q
