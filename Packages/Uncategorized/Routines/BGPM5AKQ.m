BGPM5AKQ ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON JUL 15, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"67544068531 ")
 ;;1033
 ;;21,"67754097571 ")
 ;;1034
 ;;21,"68115043320 ")
 ;;1035
 ;;21,"68115049948 ")
 ;;1036
 ;;21,"68752000336 ")
 ;;1037
 ;;21,"68788018602 ")
 ;;1038
 ;;21,"68788178501 ")
 ;;1039
 ;;21,"68788178503 ")
 ;;1040
 ;;21,"68788178506 ")
 ;;1041
 ;;21,"70030041190 ")
 ;;1042
 ;;21,"70253021807 ")
 ;;1043
 ;;21,"74684000710 ")
 ;;1044
 ;;21,"75942000110 ")
 ;;1045
 ;;21,"75942000112 ")
 ;;1046
 ;;21,"75942000136 ")
 ;;1047
 ;;21,"75942000150 ")
 ;;1048
 ;;21,"75942000175 ")
 ;;1049
 ;;9002226,885,.01)
 ;;BGPMU ANTIPLATELET NDCS
 ;;9002226,885,.02)
 ;;Antiplatelet NDC codes
 ;;9002226,885,.04)
 ;;n
 ;;9002226,885,.06)
 ;;@
 ;;9002226,885,.08)
 ;;@
 ;;9002226,885,.09)
 ;;3110714
 ;;9002226,885,.11)
 ;;@
 ;;9002226,885,.12)
 ;;@
 ;;9002226,885,.13)
 ;;@
 ;;9002226,885,.14)
 ;;@
 ;;9002226,885,.15)
 ;;@
 ;;9002226,885,.16)
 ;;@
 ;;9002226,885,.17)
 ;;@
 ;;9002226,885,3101)
 ;;@
 ;;9002226.02101,"885,00056067570 ",.01)
 ;;00056067570
 ;;9002226.02101,"885,00056067570 ",.02)
 ;;00056067570
 ;;9002226.02101,"885,00056067585 ",.01)
 ;;00056067585
 ;;9002226.02101,"885,00056067585 ",.02)
 ;;00056067585
 ;;9002226.02101,"885,00067014182 ",.01)
 ;;00067014182
 ;;9002226.02101,"885,00067014182 ",.02)
 ;;00067014182
 ;;9002226.02101,"885,00067014350 ",.01)
 ;;00067014350
 ;;9002226.02101,"885,00067014350 ",.02)
 ;;00067014350
 ;;9002226.02101,"885,00067014385 ",.01)
 ;;00067014385
 ;;9002226.02101,"885,00067014385 ",.02)
 ;;00067014385
 ;;9002226.02101,"885,00067014468 ",.01)
 ;;00067014468
 ;;9002226.02101,"885,00067014468 ",.02)
 ;;00067014468
 ;;9002226.02101,"885,00067014502 ",.01)
 ;;00067014502
 ;;9002226.02101,"885,00067014502 ",.02)
 ;;00067014502
 ;;9002226.02101,"885,00067014560 ",.01)
 ;;00067014560
 ;;9002226.02101,"885,00067014560 ",.02)
 ;;00067014560
 ;;9002226.02101,"885,00067014568 ",.01)
 ;;00067014568
 ;;9002226.02101,"885,00067014568 ",.02)
 ;;00067014568
 ;;9002226.02101,"885,00067014574 ",.01)
 ;;00067014574
 ;;9002226.02101,"885,00067014574 ",.02)
 ;;00067014574
 ;;9002226.02101,"885,00067014577 ",.01)
 ;;00067014577
 ;;9002226.02101,"885,00067014577 ",.02)
 ;;00067014577
 ;;9002226.02101,"885,00067014650 ",.01)
 ;;00067014650
 ;;9002226.02101,"885,00067014650 ",.02)
 ;;00067014650
 ;;9002226.02101,"885,00067014663 ",.01)
 ;;00067014663
 ;;9002226.02101,"885,00067014663 ",.02)
 ;;00067014663
 ;;9002226.02101,"885,00067014685 ",.01)
 ;;00067014685
 ;;9002226.02101,"885,00067014685 ",.02)
 ;;00067014685
 ;;9002226.02101,"885,00067014760 ",.01)
 ;;00067014760
 ;;9002226.02101,"885,00067014760 ",.02)
 ;;00067014760
 ;;9002226.02101,"885,00067014768 ",.01)
 ;;00067014768
 ;;9002226.02101,"885,00067014768 ",.02)
 ;;00067014768
 ;;9002226.02101,"885,00067014774 ",.01)
 ;;00067014774
 ;;9002226.02101,"885,00067014774 ",.02)
 ;;00067014774
 ;;9002226.02101,"885,00067014777 ",.01)
 ;;00067014777
 ;;9002226.02101,"885,00067014777 ",.02)
 ;;00067014777
 ;;9002226.02101,"885,00067014810 ",.01)
 ;;00067014810
 ;;9002226.02101,"885,00067014810 ",.02)
 ;;00067014810
 ;;9002226.02101,"885,00067014822 ",.01)
 ;;00067014822
 ;;9002226.02101,"885,00067014822 ",.02)
 ;;00067014822
 ;;9002226.02101,"885,00067014850 ",.01)
 ;;00067014850
 ;;9002226.02101,"885,00067014850 ",.02)
 ;;00067014850
 ;;9002226.02101,"885,00067014860 ",.01)
 ;;00067014860
 ;;9002226.02101,"885,00067014860 ",.02)
 ;;00067014860
 ;;9002226.02101,"885,00067014910 ",.01)
 ;;00067014910
 ;;9002226.02101,"885,00067014910 ",.02)
 ;;00067014910
 ;;9002226.02101,"885,00067014922 ",.01)
 ;;00067014922
 ;;9002226.02101,"885,00067014922 ",.02)
 ;;00067014922
 ;;9002226.02101,"885,00067014950 ",.01)
 ;;00067014950
 ;;9002226.02101,"885,00067014950 ",.02)
 ;;00067014950
 ;;9002226.02101,"885,00067200007 ",.01)
 ;;00067200007
 ;;9002226.02101,"885,00067200007 ",.02)
 ;;00067200007
 ;;9002226.02101,"885,00067200024 ",.01)
 ;;00067200024
 ;;9002226.02101,"885,00067200024 ",.02)
 ;;00067200024
 ;;9002226.02101,"885,00067200050 ",.01)
 ;;00067200050
 ;;9002226.02101,"885,00067200050 ",.02)
 ;;00067200050
 ;;9002226.02101,"885,00067200055 ",.01)
 ;;00067200055
 ;;9002226.02101,"885,00067200055 ",.02)
 ;;00067200055
 ;;9002226.02101,"885,00067200077 ",.01)
 ;;00067200077
 ;;9002226.02101,"885,00067200077 ",.02)
 ;;00067200077
 ;;9002226.02101,"885,00067200083 ",.01)
 ;;00067200083
 ;;9002226.02101,"885,00067200083 ",.02)
 ;;00067200083
 ;;9002226.02101,"885,00067200086 ",.01)
 ;;00067200086
 ;;9002226.02101,"885,00067200086 ",.02)
 ;;00067200086
 ;;9002226.02101,"885,00067200091 ",.01)
 ;;00067200091
 ;;9002226.02101,"885,00067200091 ",.02)
 ;;00067200091
 ;;9002226.02101,"885,00067200094 ",.01)
 ;;00067200094
 ;;9002226.02101,"885,00067200094 ",.02)
 ;;00067200094
 ;;9002226.02101,"885,00067202124 ",.01)
 ;;00067202124
 ;;9002226.02101,"885,00067202124 ",.02)
 ;;00067202124
 ;;9002226.02101,"885,00067202150 ",.01)
 ;;00067202150
 ;;9002226.02101,"885,00067202150 ",.02)
 ;;00067202150
 ;;9002226.02101,"885,00067202191 ",.01)
 ;;00067202191
 ;;9002226.02101,"885,00067202191 ",.02)
 ;;00067202191
 ;;9002226.02101,"885,00067203002 ",.01)
 ;;00067203002
 ;;9002226.02101,"885,00067203002 ",.02)
 ;;00067203002
 ;;9002226.02101,"885,00067203006 ",.01)
 ;;00067203006
 ;;9002226.02101,"885,00067203006 ",.02)
 ;;00067203006
 ;;9002226.02101,"885,00067203007 ",.01)
 ;;00067203007
 ;;9002226.02101,"885,00067203007 ",.02)
 ;;00067203007
 ;;9002226.02101,"885,00067203008 ",.01)
 ;;00067203008
 ;;9002226.02101,"885,00067203008 ",.02)
 ;;00067203008
 ;;9002226.02101,"885,00067203010 ",.01)
 ;;00067203010
 ;;9002226.02101,"885,00067203010 ",.02)
 ;;00067203010
 ;;9002226.02101,"885,00067203024 ",.01)
 ;;00067203024
 ;;9002226.02101,"885,00067203024 ",.02)
 ;;00067203024
 ;;9002226.02101,"885,00067203030 ",.01)
 ;;00067203030
 ;;9002226.02101,"885,00067203030 ",.02)
 ;;00067203030
 ;;9002226.02101,"885,00067203033 ",.01)
 ;;00067203033
 ;;9002226.02101,"885,00067203033 ",.02)
 ;;00067203033
 ;;9002226.02101,"885,00067203050 ",.01)
 ;;00067203050
 ;;9002226.02101,"885,00067203050 ",.02)
 ;;00067203050
 ;;9002226.02101,"885,00067203051 ",.01)
 ;;00067203051
 ;;9002226.02101,"885,00067203051 ",.02)
 ;;00067203051
 ;;9002226.02101,"885,00067203057 ",.01)
 ;;00067203057
 ;;9002226.02101,"885,00067203057 ",.02)
 ;;00067203057
 ;;9002226.02101,"885,00067203077 ",.01)
 ;;00067203077
 ;;9002226.02101,"885,00067203077 ",.02)
 ;;00067203077
 ;;9002226.02101,"885,00067203083 ",.01)
 ;;00067203083
 ;;9002226.02101,"885,00067203083 ",.02)
 ;;00067203083
 ;;9002226.02101,"885,00067203086 ",.01)
 ;;00067203086
 ;;9002226.02101,"885,00067203086 ",.02)
 ;;00067203086
 ;;9002226.02101,"885,00067203091 ",.01)
 ;;00067203091
 ;;9002226.02101,"885,00067203091 ",.02)
 ;;00067203091
 ;;9002226.02101,"885,00067203092 ",.01)
 ;;00067203092
 ;;9002226.02101,"885,00067203092 ",.02)
 ;;00067203092
 ;;9002226.02101,"885,00067203094 ",.01)
 ;;00067203094
 ;;9002226.02101,"885,00067203094 ",.02)
 ;;00067203094
 ;;9002226.02101,"885,00067203516 ",.01)
 ;;00067203516
 ;;9002226.02101,"885,00067203516 ",.02)
 ;;00067203516
 ;;9002226.02101,"885,00067203524 ",.01)
 ;;00067203524
 ;;9002226.02101,"885,00067203524 ",.02)
 ;;00067203524
 ;;9002226.02101,"885,00067203550 ",.01)
 ;;00067203550
 ;;9002226.02101,"885,00067203550 ",.02)
 ;;00067203550
 ;;9002226.02101,"885,00067203591 ",.01)
 ;;00067203591
 ;;9002226.02101,"885,00067203591 ",.02)
 ;;00067203591
 ;;9002226.02101,"885,00067203702 ",.01)
 ;;00067203702
 ;;9002226.02101,"885,00067203702 ",.02)
 ;;00067203702
 ;;9002226.02101,"885,00067203706 ",.01)
 ;;00067203706
 ;;9002226.02101,"885,00067203706 ",.02)
 ;;00067203706
 ;;9002226.02101,"885,00067203710 ",.01)
 ;;00067203710
 ;;9002226.02101,"885,00067203710 ",.02)
 ;;00067203710
 ;;9002226.02101,"885,00067203724 ",.01)
 ;;00067203724
 ;;9002226.02101,"885,00067203724 ",.02)
 ;;00067203724
 ;;9002226.02101,"885,00067203733 ",.01)
 ;;00067203733
 ;;9002226.02101,"885,00067203733 ",.02)
 ;;00067203733
 ;;9002226.02101,"885,00067203750 ",.01)
 ;;00067203750
 ;;9002226.02101,"885,00067203750 ",.02)
 ;;00067203750
 ;;9002226.02101,"885,00067203777 ",.01)
 ;;00067203777
 ;;9002226.02101,"885,00067203777 ",.02)
 ;;00067203777
 ;;9002226.02101,"885,00067203783 ",.01)
 ;;00067203783
 ;;9002226.02101,"885,00067203783 ",.02)
 ;;00067203783
 ;;9002226.02101,"885,00067203784 ",.01)
 ;;00067203784
 ;;9002226.02101,"885,00067203784 ",.02)
 ;;00067203784
 ;;9002226.02101,"885,00067203791 ",.01)
 ;;00067203791
 ;;9002226.02101,"885,00067203791 ",.02)
 ;;00067203791
 ;;9002226.02101,"885,00067203907 ",.01)
 ;;00067203907
 ;;9002226.02101,"885,00067203907 ",.02)
 ;;00067203907
 ;;9002226.02101,"885,00067203908 ",.01)
 ;;00067203908
 ;;9002226.02101,"885,00067203908 ",.02)
 ;;00067203908
 ;;9002226.02101,"885,00067203924 ",.01)
 ;;00067203924
 ;;9002226.02101,"885,00067203924 ",.02)
 ;;00067203924
 ;;9002226.02101,"885,00067203933 ",.01)
 ;;00067203933
 ;;9002226.02101,"885,00067203933 ",.02)
 ;;00067203933
 ;;9002226.02101,"885,00067203950 ",.01)
 ;;00067203950
 ;;9002226.02101,"885,00067203950 ",.02)
 ;;00067203950
 ;;9002226.02101,"885,00067203977 ",.01)
 ;;00067203977
 ;;9002226.02101,"885,00067203977 ",.02)
 ;;00067203977
 ;;9002226.02101,"885,00067203983 ",.01)
 ;;00067203983
 ;;9002226.02101,"885,00067203983 ",.02)
 ;;00067203983
 ;;9002226.02101,"885,00067203984 ",.01)
 ;;00067203984
 ;;9002226.02101,"885,00067203984 ",.02)
 ;;00067203984
 ;;9002226.02101,"885,00067203986 ",.01)
 ;;00067203986
 ;;9002226.02101,"885,00067203986 ",.02)
 ;;00067203986
 ;;9002226.02101,"885,00067203991 ",.01)
 ;;00067203991
 ;;9002226.02101,"885,00067203991 ",.02)
 ;;00067203991
 ;;9002226.02101,"885,00067203992 ",.01)
 ;;00067203992
 ;;9002226.02101,"885,00067203992 ",.02)
 ;;00067203992
 ;;9002226.02101,"885,00067203994 ",.01)
 ;;00067203994
 ;;9002226.02101,"885,00067203994 ",.02)
 ;;00067203994
 ;;9002226.02101,"885,00067206313 ",.01)
 ;;00067206313
 ;;9002226.02101,"885,00067206313 ",.02)
 ;;00067206313
 ;;9002226.02101,"885,00067206339 ",.01)
 ;;00067206339
 ;;9002226.02101,"885,00067206339 ",.02)
 ;;00067206339
 ;;9002226.02101,"885,00067206365 ",.01)
 ;;00067206365
 ;;9002226.02101,"885,00067206365 ",.02)
 ;;00067206365
 ;;9002226.02101,"885,00067206513 ",.01)
 ;;00067206513
 ;;9002226.02101,"885,00067206513 ",.02)
 ;;00067206513
 ;;9002226.02101,"885,00067206539 ",.01)
 ;;00067206539
 ;;9002226.02101,"885,00067206539 ",.02)
 ;;00067206539
 ;;9002226.02101,"885,00067206565 ",.01)
 ;;00067206565
 ;;9002226.02101,"885,00067206565 ",.02)
 ;;00067206565
 ;;9002226.02101,"885,00067627016 ",.01)
 ;;00067627016
 ;;9002226.02101,"885,00067627016 ",.02)
 ;;00067627016
 ;;9002226.02101,"885,00067627020 ",.01)
 ;;00067627020
 ;;9002226.02101,"885,00067627020 ",.02)
 ;;00067627020
 ;;9002226.02101,"885,00067627040 ",.01)
 ;;00067627040
 ;;9002226.02101,"885,00067627040 ",.02)
 ;;00067627040
 ;;9002226.02101,"885,00067627080 ",.01)
 ;;00067627080
 ;;9002226.02101,"885,00067627080 ",.02)
 ;;00067627080
 ;;9002226.02101,"885,00067634120 ",.01)
 ;;00067634120
 ;;9002226.02101,"885,00067634120 ",.02)
 ;;00067634120
 ;;9002226.02101,"885,00067634140 ",.01)
 ;;00067634140
 ;;9002226.02101,"885,00067634140 ",.02)
 ;;00067634140
 ;;9002226.02101,"885,00078010305 ",.01)
 ;;00078010305
 ;;9002226.02101,"885,00078010305 ",.02)
 ;;00078010305
 ;;9002226.02101,"885,00078010308 ",.01)
 ;;00078010308
