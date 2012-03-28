BGP6AXI ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON AUG 21, 2005 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;;BGP6;;AUG 21, 2005
 ;;BGP ASTHMA INHALED STEROID NDC
 ;
 ; This routine loads Taxonomy BGP ASTHMA INHALED STEROID NDC
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
 ;;21,"00075-0060-37 ")
 ;;1
 ;;21,"00085-0649-02 ")
 ;;2
 ;;21,"00085-0736-04 ")
 ;;3
 ;;21,"00089-0175-40 ")
 ;;4
 ;;21,"00089-0177-80 ")
 ;;5
 ;;21,"00173-0491-00 ")
 ;;6
 ;;21,"00173-0494-00 ")
 ;;7
 ;;21,"00173-0495-00 ")
 ;;8
 ;;21,"00173-0497-00 ")
 ;;9
 ;;21,"00173-0498-00 ")
 ;;10
 ;;21,"00173-0499-00 ")
 ;;11
 ;;21,"00173-0504-00 ")
 ;;12
 ;;21,"00173-0509-00 ")
 ;;13
 ;;21,"00173-0511-00 ")
 ;;14
 ;;21,"00173-0695-00 ")
 ;;15
 ;;21,"00173-0695-02 ")
 ;;16
 ;;21,"00173-0696-00 ")
 ;;17
 ;;21,"00173-0696-02 ")
 ;;18
 ;;21,"00173-0697-00 ")
 ;;19
 ;;21,"00173-0697-02 ")
 ;;20
 ;;21,"00186-0915-42 ")
 ;;21
 ;;21,"00186-1988-04 ")
 ;;22
 ;;21,"00186-1989-04 ")
 ;;23
 ;;21,"51479-0011-03 ")
 ;;24
 ;;21,"52959-0286-03 ")
 ;;25
 ;;21,"52959-0585-00 ")
 ;;26
 ;;21,"52959-0596-01 ")
 ;;27
 ;;21,"52959-0598-01 ")
 ;;28
 ;;21,"54569-0053-00 ")
 ;;29
 ;;21,"54569-0067-00 ")
 ;;30
 ;;21,"54569-1004-00 ")
 ;;31
 ;;21,"54569-3656-00 ")
 ;;32
 ;;21,"54569-4540-00 ")
 ;;33
 ;;21,"54569-4602-00 ")
 ;;34
 ;;21,"54569-4603-00 ")
 ;;35
 ;;21,"54569-4863-00 ")
 ;;36
 ;;21,"54569-4896-00 ")
 ;;37
 ;;21,"54569-5162-00 ")
 ;;38
 ;;21,"54569-5163-00 ")
 ;;39
 ;;21,"54569-5241-00 ")
 ;;40
 ;;21,"54569-5242-00 ")
 ;;41
 ;;21,"54569-5243-00 ")
 ;;42
 ;;21,"54868-1268-01 ")
 ;;43
 ;;21,"54868-1269-01 ")
 ;;44
 ;;21,"54868-1841-01 ")
 ;;45
 ;;21,"54868-4182-00 ")
 ;;46
 ;;21,"54868-4264-00 ")
 ;;47
 ;;21,"54868-4295-00 ")
 ;;48
 ;;21,"54868-4392-00 ")
 ;;49
 ;;21,"54868-4516-00 ")
 ;;50
 ;;21,"54868-4517-00 ")
 ;;51
 ;;21,"54868-4518-00 ")
 ;;52
 ;;21,"54969-4741-00 ")
 ;;53
 ;;21,"58016-6075-01 ")
 ;;54
 ;;21,"58016-6207-01 ")
 ;;55
 ;;21,"59569-4822-00 ")
 ;;56
 ;;9002226,412,.01)
 ;;BGP ASTHMA INHALED STEROID NDC
 ;;9002226,412,.02)
 ;;@
 ;;9002226,412,.04)
 ;;n
 ;;9002226,412,.06)
 ;;@
 ;;9002226,412,.08)
 ;;@
 ;;9002226,412,.09)
 ;;@
 ;;9002226,412,.11)
 ;;@
 ;;9002226,412,.12)
 ;;@
 ;;9002226,412,.13)
 ;;1
 ;;9002226,412,.14)
 ;;@
 ;;9002226,412,.15)
 ;;@
 ;;9002226,412,.16)
 ;;@
 ;;9002226,412,.17)
 ;;@
 ;;9002226,412,3101)
 ;;@
 ;;9002226.02101,"412,00075-0060-37 ",.01)
 ;;00075-0060-37
 ;;9002226.02101,"412,00075-0060-37 ",.02)
 ;;00075-0060-37
 ;;9002226.02101,"412,00085-0649-02 ",.01)
 ;;00085-0649-02
 ;;9002226.02101,"412,00085-0649-02 ",.02)
 ;;00085-0649-02
 ;;9002226.02101,"412,00085-0736-04 ",.01)
 ;;00085-0736-04
 ;;9002226.02101,"412,00085-0736-04 ",.02)
 ;;00085-0736-04
 ;;9002226.02101,"412,00089-0175-40 ",.01)
 ;;00089-0175-40
 ;;9002226.02101,"412,00089-0175-40 ",.02)
 ;;00089-0175-40
 ;;9002226.02101,"412,00089-0177-80 ",.01)
 ;;00089-0177-80
 ;;9002226.02101,"412,00089-0177-80 ",.02)
 ;;00089-0177-80
 ;;9002226.02101,"412,00173-0491-00 ",.01)
 ;;00173-0491-00
 ;;9002226.02101,"412,00173-0491-00 ",.02)
 ;;00173-0491-00
 ;;9002226.02101,"412,00173-0494-00 ",.01)
 ;;00173-0494-00
 ;;9002226.02101,"412,00173-0494-00 ",.02)
 ;;00173-0494-00
 ;;9002226.02101,"412,00173-0495-00 ",.01)
 ;;00173-0495-00
 ;;9002226.02101,"412,00173-0495-00 ",.02)
 ;;00173-0495-00
 ;;9002226.02101,"412,00173-0497-00 ",.01)
 ;;00173-0497-00
 ;;9002226.02101,"412,00173-0497-00 ",.02)
 ;;00173-0497-00
 ;;9002226.02101,"412,00173-0498-00 ",.01)
 ;;00173-0498-00
 ;;9002226.02101,"412,00173-0498-00 ",.02)
 ;;00173-0498-00
 ;;9002226.02101,"412,00173-0499-00 ",.01)
 ;;00173-0499-00
 ;;9002226.02101,"412,00173-0499-00 ",.02)
 ;;00173-0499-00
 ;;9002226.02101,"412,00173-0504-00 ",.01)
 ;;00173-0504-00
 ;;9002226.02101,"412,00173-0504-00 ",.02)
 ;;00173-0504-00
 ;;9002226.02101,"412,00173-0509-00 ",.01)
 ;;00173-0509-00
 ;;9002226.02101,"412,00173-0509-00 ",.02)
 ;;00173-0509-00
 ;;9002226.02101,"412,00173-0511-00 ",.01)
 ;;00173-0511-00
 ;;9002226.02101,"412,00173-0511-00 ",.02)
 ;;00173-0511-00
 ;;9002226.02101,"412,00173-0695-00 ",.01)
 ;;00173-0695-00
 ;;9002226.02101,"412,00173-0695-00 ",.02)
 ;;00173-0695-00
 ;;9002226.02101,"412,00173-0695-02 ",.01)
 ;;00173-0695-02
 ;;9002226.02101,"412,00173-0695-02 ",.02)
 ;;00173-0695-02
 ;;9002226.02101,"412,00173-0696-00 ",.01)
 ;;00173-0696-00
 ;;9002226.02101,"412,00173-0696-00 ",.02)
 ;;00173-0696-00
 ;;9002226.02101,"412,00173-0696-02 ",.01)
 ;;00173-0696-02
 ;;9002226.02101,"412,00173-0696-02 ",.02)
 ;;00173-0696-02
 ;;9002226.02101,"412,00173-0697-00 ",.01)
 ;;00173-0697-00
 ;;9002226.02101,"412,00173-0697-00 ",.02)
 ;;00173-0697-00
 ;;9002226.02101,"412,00173-0697-02 ",.01)
 ;;00173-0697-02
 ;;9002226.02101,"412,00173-0697-02 ",.02)
 ;;00173-0697-02
 ;;9002226.02101,"412,00186-0915-42 ",.01)
 ;;00186-0915-42
 ;;9002226.02101,"412,00186-0915-42 ",.02)
 ;;00186-0915-42
 ;;9002226.02101,"412,00186-1988-04 ",.01)
 ;;00186-1988-04
 ;;9002226.02101,"412,00186-1988-04 ",.02)
 ;;00186-1988-04
 ;;9002226.02101,"412,00186-1989-04 ",.01)
 ;;00186-1989-04
 ;;9002226.02101,"412,00186-1989-04 ",.02)
 ;;00186-1989-04
 ;;9002226.02101,"412,51479-0011-03 ",.01)
 ;;51479-0011-03
 ;;9002226.02101,"412,51479-0011-03 ",.02)
 ;;51479-0011-03
 ;;9002226.02101,"412,52959-0286-03 ",.01)
 ;;52959-0286-03
 ;;9002226.02101,"412,52959-0286-03 ",.02)
 ;;52959-0286-03
 ;;9002226.02101,"412,52959-0585-00 ",.01)
 ;;52959-0585-00
 ;;9002226.02101,"412,52959-0585-00 ",.02)
 ;;52959-0585-00
 ;;9002226.02101,"412,52959-0596-01 ",.01)
 ;;52959-0596-01
 ;;9002226.02101,"412,52959-0596-01 ",.02)
 ;;52959-0596-01
 ;;9002226.02101,"412,52959-0598-01 ",.01)
 ;;52959-0598-01
 ;;9002226.02101,"412,52959-0598-01 ",.02)
 ;;52959-0598-01
 ;;9002226.02101,"412,54569-0053-00 ",.01)
 ;;54569-0053-00
 ;;9002226.02101,"412,54569-0053-00 ",.02)
 ;;54569-0053-00
 ;;9002226.02101,"412,54569-0067-00 ",.01)
 ;;54569-0067-00
 ;;9002226.02101,"412,54569-0067-00 ",.02)
 ;;54569-0067-00
 ;;9002226.02101,"412,54569-1004-00 ",.01)
 ;;54569-1004-00
 ;;9002226.02101,"412,54569-1004-00 ",.02)
 ;;54569-1004-00
 ;;9002226.02101,"412,54569-3656-00 ",.01)
 ;;54569-3656-00
 ;;9002226.02101,"412,54569-3656-00 ",.02)
 ;;54569-3656-00
 ;;9002226.02101,"412,54569-4540-00 ",.01)
 ;;54569-4540-00
 ;;9002226.02101,"412,54569-4540-00 ",.02)
 ;;54569-4540-00
 ;;9002226.02101,"412,54569-4602-00 ",.01)
 ;;54569-4602-00
 ;;9002226.02101,"412,54569-4602-00 ",.02)
 ;;54569-4602-00
 ;;9002226.02101,"412,54569-4603-00 ",.01)
 ;;54569-4603-00
 ;;9002226.02101,"412,54569-4603-00 ",.02)
 ;;54569-4603-00
 ;;9002226.02101,"412,54569-4863-00 ",.01)
 ;;54569-4863-00
 ;;9002226.02101,"412,54569-4863-00 ",.02)
 ;;54569-4863-00
 ;;9002226.02101,"412,54569-4896-00 ",.01)
 ;;54569-4896-00
 ;;9002226.02101,"412,54569-4896-00 ",.02)
 ;;54569-4896-00
 ;;9002226.02101,"412,54569-5162-00 ",.01)
 ;;54569-5162-00
 ;;9002226.02101,"412,54569-5162-00 ",.02)
 ;;54569-5162-00
 ;;9002226.02101,"412,54569-5163-00 ",.01)
 ;;54569-5163-00
 ;;9002226.02101,"412,54569-5163-00 ",.02)
 ;;54569-5163-00
 ;;9002226.02101,"412,54569-5241-00 ",.01)
 ;;54569-5241-00
 ;;9002226.02101,"412,54569-5241-00 ",.02)
 ;;54569-5241-00
 ;;9002226.02101,"412,54569-5242-00 ",.01)
 ;;54569-5242-00
 ;;9002226.02101,"412,54569-5242-00 ",.02)
 ;;54569-5242-00
 ;;9002226.02101,"412,54569-5243-00 ",.01)
 ;;54569-5243-00
 ;;9002226.02101,"412,54569-5243-00 ",.02)
 ;;54569-5243-00
 ;;9002226.02101,"412,54868-1268-01 ",.01)
 ;;54868-1268-01
 ;;9002226.02101,"412,54868-1268-01 ",.02)
 ;;54868-1268-01
 ;;9002226.02101,"412,54868-1269-01 ",.01)
 ;;54868-1269-01
 ;;9002226.02101,"412,54868-1269-01 ",.02)
 ;;54868-1269-01
 ;;9002226.02101,"412,54868-1841-01 ",.01)
 ;;54868-1841-01
 ;;9002226.02101,"412,54868-1841-01 ",.02)
 ;;54868-1841-01
 ;;9002226.02101,"412,54868-4182-00 ",.01)
 ;;54868-4182-00
 ;;9002226.02101,"412,54868-4182-00 ",.02)
 ;;54868-4182-00
 ;;9002226.02101,"412,54868-4264-00 ",.01)
 ;;54868-4264-00
 ;;9002226.02101,"412,54868-4264-00 ",.02)
 ;;54868-4264-00
 ;;9002226.02101,"412,54868-4295-00 ",.01)
 ;;54868-4295-00
 ;;9002226.02101,"412,54868-4295-00 ",.02)
 ;;54868-4295-00
 ;;9002226.02101,"412,54868-4392-00 ",.01)
 ;;54868-4392-00
 ;;9002226.02101,"412,54868-4392-00 ",.02)
 ;;54868-4392-00
 ;;9002226.02101,"412,54868-4516-00 ",.01)
 ;;54868-4516-00
 ;;9002226.02101,"412,54868-4516-00 ",.02)
 ;;54868-4516-00
 ;;9002226.02101,"412,54868-4517-00 ",.01)
 ;;54868-4517-00
 ;;9002226.02101,"412,54868-4517-00 ",.02)
 ;;54868-4517-00
 ;;9002226.02101,"412,54868-4518-00 ",.01)
 ;;54868-4518-00
 ;;9002226.02101,"412,54868-4518-00 ",.02)
 ;;54868-4518-00
 ;;9002226.02101,"412,54969-4741-00 ",.01)
 ;;54969-4741-00
 ;;9002226.02101,"412,54969-4741-00 ",.02)
 ;;54969-4741-00
 ;;9002226.02101,"412,58016-6075-01 ",.01)
 ;;58016-6075-01
 ;;9002226.02101,"412,58016-6075-01 ",.02)
 ;;58016-6075-01
 ;;9002226.02101,"412,58016-6207-01 ",.01)
 ;;58016-6207-01
 ;;9002226.02101,"412,58016-6207-01 ",.02)
 ;;58016-6207-01
 ;;9002226.02101,"412,59569-4822-00 ",.01)
 ;;59569-4822-00
 ;;9002226.02101,"412,59569-4822-00 ",.02)
 ;;59569-4822-00
 ;
OTHER ; OTHER ROUTINES
 Q
