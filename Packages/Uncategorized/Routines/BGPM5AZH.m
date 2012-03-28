BGPM5AZH ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON SEP 03, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;;BGPMU ALCOHOL DRUG DEP DX
 ;
 ; This routine loads Taxonomy BGPMU ALCOHOL DRUG DEP DX
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
 ;;21,"291 ")
 ;;1
 ;;21,"291.0 ")
 ;;2
 ;;21,"291.1 ")
 ;;3
 ;;21,"291.2 ")
 ;;4
 ;;21,"291.3 ")
 ;;5
 ;;21,"291.4 ")
 ;;6
 ;;21,"291.5 ")
 ;;7
 ;;21,"291.8 ")
 ;;8
 ;;21,"291.81 ")
 ;;9
 ;;21,"291.82 ")
 ;;10
 ;;21,"291.89 ")
 ;;11
 ;;21,"291.9 ")
 ;;12
 ;;21,"292 ")
 ;;13
 ;;21,"292.0 ")
 ;;14
 ;;21,"292.1 ")
 ;;15
 ;;21,"292.11 ")
 ;;16
 ;;21,"292.12 ")
 ;;17
 ;;21,"292.2 ")
 ;;18
 ;;21,"292.8 ")
 ;;19
 ;;21,"292.81 ")
 ;;20
 ;;21,"292.82 ")
 ;;21
 ;;21,"292.83 ")
 ;;22
 ;;21,"292.84 ")
 ;;23
 ;;21,"292.85 ")
 ;;24
 ;;21,"292.89 ")
 ;;25
 ;;21,"292.9 ")
 ;;26
 ;;21,"303.00 ")
 ;;27
 ;;21,"303.01 ")
 ;;28
 ;;21,"303.02 ")
 ;;29
 ;;21,"303.90 ")
 ;;30
 ;;21,"303.91 ")
 ;;31
 ;;21,"303.92 ")
 ;;32
 ;;21,"304.00 ")
 ;;33
 ;;21,"304.01 ")
 ;;34
 ;;21,"304.02 ")
 ;;35
 ;;21,"304.10 ")
 ;;36
 ;;21,"304.11 ")
 ;;37
 ;;21,"304.12 ")
 ;;38
 ;;21,"304.20 ")
 ;;39
 ;;21,"304.21 ")
 ;;40
 ;;21,"304.22 ")
 ;;41
 ;;21,"304.30 ")
 ;;42
 ;;21,"304.31 ")
 ;;43
 ;;21,"304.32 ")
 ;;44
 ;;21,"304.40 ")
 ;;45
 ;;21,"304.41 ")
 ;;46
 ;;21,"304.42 ")
 ;;47
 ;;21,"304.50 ")
 ;;48
 ;;21,"304.51 ")
 ;;49
 ;;21,"304.52 ")
 ;;50
 ;;21,"304.60 ")
 ;;51
 ;;21,"304.61 ")
 ;;52
 ;;21,"304.62 ")
 ;;53
 ;;21,"304.70 ")
 ;;54
 ;;21,"304.71 ")
 ;;55
 ;;21,"304.72 ")
 ;;56
 ;;21,"304.80 ")
 ;;57
 ;;21,"304.81 ")
 ;;58
 ;;21,"304.82 ")
 ;;59
 ;;21,"304.90 ")
 ;;60
 ;;21,"304.91 ")
 ;;61
 ;;21,"304.92 ")
 ;;62
 ;;21,"305.00 ")
 ;;63
 ;;21,"305.01 ")
 ;;64
 ;;21,"305.02 ")
 ;;65
 ;;21,"305.20 ")
 ;;66
 ;;21,"305.21 ")
 ;;67
 ;;21,"305.22 ")
 ;;68
 ;;21,"305.30 ")
 ;;69
 ;;21,"305.31 ")
 ;;70
 ;;21,"305.32 ")
 ;;71
 ;;21,"305.40 ")
 ;;72
 ;;21,"305.41 ")
 ;;73
 ;;21,"305.42 ")
 ;;74
 ;;21,"305.50 ")
 ;;75
 ;;21,"305.51 ")
 ;;76
 ;;21,"305.52 ")
 ;;77
 ;;21,"305.60 ")
 ;;78
 ;;21,"305.61 ")
 ;;79
 ;;21,"305.62 ")
 ;;80
 ;;21,"305.70 ")
 ;;81
 ;;21,"305.71 ")
 ;;82
 ;;21,"305.72 ")
 ;;83
 ;;21,"305.80 ")
 ;;84
 ;;21,"305.81 ")
 ;;85
 ;;21,"305.82 ")
 ;;86
 ;;21,"305.90 ")
 ;;87
 ;;21,"305.91 ")
 ;;88
 ;;21,"305.92 ")
 ;;89
 ;;21,"535.3 ")
 ;;90
 ;;21,"535.30 ")
 ;;91
 ;;21,"535.31 ")
 ;;92
 ;;21,"571.1 ")
 ;;93
 ;;9002226,1056,.01)
 ;;BGPMU ALCOHOL DRUG DEP DX
 ;;9002226,1056,.02)
 ;;Alcohol Drug Depend Dx
 ;;9002226,1056,.04)
 ;;@
 ;;9002226,1056,.06)
 ;;@
 ;;9002226,1056,.08)
 ;;@
 ;;9002226,1056,.09)
 ;;@
 ;;9002226,1056,.11)
 ;;@
 ;;9002226,1056,.12)
 ;;@
 ;;9002226,1056,.13)
 ;;@
 ;;9002226,1056,.14)
 ;;@
 ;;9002226,1056,.15)
 ;;80
 ;;9002226,1056,.16)
 ;;@
 ;;9002226,1056,.17)
 ;;@
 ;;9002226,1056,3101)
 ;;@
 ;;9002226.02101,"1056,291 ",.01)
 ;;291
 ;;9002226.02101,"1056,291 ",.02)
 ;;291
 ;;9002226.02101,"1056,291.0 ",.01)
 ;;291.0
 ;;9002226.02101,"1056,291.0 ",.02)
 ;;291.0
 ;;9002226.02101,"1056,291.1 ",.01)
 ;;291.1
 ;;9002226.02101,"1056,291.1 ",.02)
 ;;291.1
 ;;9002226.02101,"1056,291.2 ",.01)
 ;;291.2
 ;;9002226.02101,"1056,291.2 ",.02)
 ;;291.2
 ;;9002226.02101,"1056,291.3 ",.01)
 ;;291.3
 ;;9002226.02101,"1056,291.3 ",.02)
 ;;291.3
 ;;9002226.02101,"1056,291.4 ",.01)
 ;;291.4
 ;;9002226.02101,"1056,291.4 ",.02)
 ;;291.4
 ;;9002226.02101,"1056,291.5 ",.01)
 ;;291.5
 ;;9002226.02101,"1056,291.5 ",.02)
 ;;291.5
 ;;9002226.02101,"1056,291.8 ",.01)
 ;;291.8
 ;;9002226.02101,"1056,291.8 ",.02)
 ;;291.8
 ;;9002226.02101,"1056,291.81 ",.01)
 ;;291.81
 ;;9002226.02101,"1056,291.81 ",.02)
 ;;291.81
 ;;9002226.02101,"1056,291.82 ",.01)
 ;;291.82
 ;;9002226.02101,"1056,291.82 ",.02)
 ;;291.82
 ;;9002226.02101,"1056,291.89 ",.01)
 ;;291.89
 ;;9002226.02101,"1056,291.89 ",.02)
 ;;291.89
 ;;9002226.02101,"1056,291.9 ",.01)
 ;;291.9
 ;;9002226.02101,"1056,291.9 ",.02)
 ;;291.9
 ;;9002226.02101,"1056,292 ",.01)
 ;;292
 ;;9002226.02101,"1056,292 ",.02)
 ;;292
 ;;9002226.02101,"1056,292.0 ",.01)
 ;;292.0
 ;;9002226.02101,"1056,292.0 ",.02)
 ;;292.0
 ;;9002226.02101,"1056,292.1 ",.01)
 ;;292.1
 ;;9002226.02101,"1056,292.1 ",.02)
 ;;292.1
 ;;9002226.02101,"1056,292.11 ",.01)
 ;;292.11
 ;;9002226.02101,"1056,292.11 ",.02)
 ;;292.11
 ;;9002226.02101,"1056,292.12 ",.01)
 ;;292.12
 ;;9002226.02101,"1056,292.12 ",.02)
 ;;292.12
 ;;9002226.02101,"1056,292.2 ",.01)
 ;;292.2
 ;;9002226.02101,"1056,292.2 ",.02)
 ;;292.2
 ;;9002226.02101,"1056,292.8 ",.01)
 ;;292.8
 ;;9002226.02101,"1056,292.8 ",.02)
 ;;292.8
 ;;9002226.02101,"1056,292.81 ",.01)
 ;;292.81
 ;;9002226.02101,"1056,292.81 ",.02)
 ;;292.81
 ;;9002226.02101,"1056,292.82 ",.01)
 ;;292.82
 ;;9002226.02101,"1056,292.82 ",.02)
 ;;292.82
 ;;9002226.02101,"1056,292.83 ",.01)
 ;;292.83
 ;;9002226.02101,"1056,292.83 ",.02)
 ;;292.83
 ;;9002226.02101,"1056,292.84 ",.01)
 ;;292.84
 ;;9002226.02101,"1056,292.84 ",.02)
 ;;292.84
 ;;9002226.02101,"1056,292.85 ",.01)
 ;;292.85
 ;;9002226.02101,"1056,292.85 ",.02)
 ;;292.85
 ;;9002226.02101,"1056,292.89 ",.01)
 ;;292.89
 ;;9002226.02101,"1056,292.89 ",.02)
 ;;292.89
 ;;9002226.02101,"1056,292.9 ",.01)
 ;;292.9
 ;;9002226.02101,"1056,292.9 ",.02)
 ;;292.9
 ;;9002226.02101,"1056,303.00 ",.01)
 ;;303.00
 ;;9002226.02101,"1056,303.00 ",.02)
 ;;303.00
 ;;9002226.02101,"1056,303.01 ",.01)
 ;;303.01
 ;;9002226.02101,"1056,303.01 ",.02)
 ;;303.01
 ;;9002226.02101,"1056,303.02 ",.01)
 ;;303.02
 ;;9002226.02101,"1056,303.02 ",.02)
 ;;303.02
 ;;9002226.02101,"1056,303.90 ",.01)
 ;;303.90
 ;;9002226.02101,"1056,303.90 ",.02)
 ;;303.90
 ;;9002226.02101,"1056,303.91 ",.01)
 ;;303.91
 ;;9002226.02101,"1056,303.91 ",.02)
 ;;303.91
 ;;9002226.02101,"1056,303.92 ",.01)
 ;;303.92
 ;;9002226.02101,"1056,303.92 ",.02)
 ;;303.92
 ;;9002226.02101,"1056,304.00 ",.01)
 ;;304.00
 ;;9002226.02101,"1056,304.00 ",.02)
 ;;304.00
 ;;9002226.02101,"1056,304.01 ",.01)
 ;;304.01
 ;;9002226.02101,"1056,304.01 ",.02)
 ;;304.01
 ;;9002226.02101,"1056,304.02 ",.01)
 ;;304.02
 ;;9002226.02101,"1056,304.02 ",.02)
 ;;304.02
 ;;9002226.02101,"1056,304.10 ",.01)
 ;;304.10
 ;;9002226.02101,"1056,304.10 ",.02)
 ;;304.10
 ;;9002226.02101,"1056,304.11 ",.01)
 ;;304.11
 ;;9002226.02101,"1056,304.11 ",.02)
 ;;304.11
 ;;9002226.02101,"1056,304.12 ",.01)
 ;;304.12
 ;;9002226.02101,"1056,304.12 ",.02)
 ;;304.12
 ;;9002226.02101,"1056,304.20 ",.01)
 ;;304.20
 ;;9002226.02101,"1056,304.20 ",.02)
 ;;304.20
 ;;9002226.02101,"1056,304.21 ",.01)
 ;;304.21
 ;;9002226.02101,"1056,304.21 ",.02)
 ;;304.21
 ;;9002226.02101,"1056,304.22 ",.01)
 ;;304.22
 ;;9002226.02101,"1056,304.22 ",.02)
 ;;304.22
 ;;9002226.02101,"1056,304.30 ",.01)
 ;;304.30
 ;;9002226.02101,"1056,304.30 ",.02)
 ;;304.30
 ;;9002226.02101,"1056,304.31 ",.01)
 ;;304.31
 ;;9002226.02101,"1056,304.31 ",.02)
 ;;304.31
 ;;9002226.02101,"1056,304.32 ",.01)
 ;;304.32
 ;;9002226.02101,"1056,304.32 ",.02)
 ;;304.32
 ;;9002226.02101,"1056,304.40 ",.01)
 ;;304.40
 ;;9002226.02101,"1056,304.40 ",.02)
 ;;304.40
 ;;9002226.02101,"1056,304.41 ",.01)
 ;;304.41
 ;;9002226.02101,"1056,304.41 ",.02)
 ;;304.41
 ;;9002226.02101,"1056,304.42 ",.01)
 ;;304.42
 ;;9002226.02101,"1056,304.42 ",.02)
 ;;304.42
 ;;9002226.02101,"1056,304.50 ",.01)
 ;;304.50
 ;;9002226.02101,"1056,304.50 ",.02)
 ;;304.50
 ;;9002226.02101,"1056,304.51 ",.01)
 ;;304.51
 ;;9002226.02101,"1056,304.51 ",.02)
 ;;304.51
 ;;9002226.02101,"1056,304.52 ",.01)
 ;;304.52
 ;;9002226.02101,"1056,304.52 ",.02)
 ;;304.52
 ;;9002226.02101,"1056,304.60 ",.01)
 ;;304.60
 ;;9002226.02101,"1056,304.60 ",.02)
 ;;304.60
 ;;9002226.02101,"1056,304.61 ",.01)
 ;;304.61
 ;;9002226.02101,"1056,304.61 ",.02)
 ;;304.61
 ;;9002226.02101,"1056,304.62 ",.01)
 ;;304.62
 ;;9002226.02101,"1056,304.62 ",.02)
 ;;304.62
 ;;9002226.02101,"1056,304.70 ",.01)
 ;;304.70
 ;;9002226.02101,"1056,304.70 ",.02)
 ;;304.70
 ;;9002226.02101,"1056,304.71 ",.01)
 ;;304.71
 ;;9002226.02101,"1056,304.71 ",.02)
 ;;304.71
 ;;9002226.02101,"1056,304.72 ",.01)
 ;;304.72
 ;;9002226.02101,"1056,304.72 ",.02)
 ;;304.72
 ;;9002226.02101,"1056,304.80 ",.01)
 ;;304.80
 ;;9002226.02101,"1056,304.80 ",.02)
 ;;304.80
 ;;9002226.02101,"1056,304.81 ",.01)
 ;;304.81
 ;;9002226.02101,"1056,304.81 ",.02)
 ;;304.81
 ;;9002226.02101,"1056,304.82 ",.01)
 ;;304.82
 ;;9002226.02101,"1056,304.82 ",.02)
 ;;304.82
 ;;9002226.02101,"1056,304.90 ",.01)
 ;;304.90
 ;;9002226.02101,"1056,304.90 ",.02)
 ;;304.90
 ;;9002226.02101,"1056,304.91 ",.01)
 ;;304.91
 ;;9002226.02101,"1056,304.91 ",.02)
 ;;304.91
 ;;9002226.02101,"1056,304.92 ",.01)
 ;;304.92
 ;;9002226.02101,"1056,304.92 ",.02)
 ;;304.92
 ;;9002226.02101,"1056,305.00 ",.01)
 ;;305.00
 ;;9002226.02101,"1056,305.00 ",.02)
 ;;305.00
 ;;9002226.02101,"1056,305.01 ",.01)
 ;;305.01
 ;;9002226.02101,"1056,305.01 ",.02)
 ;;305.01
 ;;9002226.02101,"1056,305.02 ",.01)
 ;;305.02
 ;;9002226.02101,"1056,305.02 ",.02)
 ;;305.02
 ;;9002226.02101,"1056,305.20 ",.01)
 ;;305.20
 ;;9002226.02101,"1056,305.20 ",.02)
 ;;305.20
 ;;9002226.02101,"1056,305.21 ",.01)
 ;;305.21
 ;;9002226.02101,"1056,305.21 ",.02)
 ;;305.21
 ;;9002226.02101,"1056,305.22 ",.01)
 ;;305.22
 ;;9002226.02101,"1056,305.22 ",.02)
 ;;305.22
 ;;9002226.02101,"1056,305.30 ",.01)
 ;;305.30
 ;;9002226.02101,"1056,305.30 ",.02)
 ;;305.30
 ;;9002226.02101,"1056,305.31 ",.01)
 ;;305.31
 ;;9002226.02101,"1056,305.31 ",.02)
 ;;305.31
 ;;9002226.02101,"1056,305.32 ",.01)
 ;;305.32
 ;;9002226.02101,"1056,305.32 ",.02)
 ;;305.32
 ;;9002226.02101,"1056,305.40 ",.01)
 ;;305.40
 ;;9002226.02101,"1056,305.40 ",.02)
 ;;305.40
 ;;9002226.02101,"1056,305.41 ",.01)
 ;;305.41
 ;;9002226.02101,"1056,305.41 ",.02)
 ;;305.41
 ;;9002226.02101,"1056,305.42 ",.01)
 ;;305.42
 ;;9002226.02101,"1056,305.42 ",.02)
 ;;305.42
 ;;9002226.02101,"1056,305.50 ",.01)
 ;;305.50
 ;;9002226.02101,"1056,305.50 ",.02)
 ;;305.50
 ;;9002226.02101,"1056,305.51 ",.01)
 ;;305.51
 ;;9002226.02101,"1056,305.51 ",.02)
 ;;305.51
 ;;9002226.02101,"1056,305.52 ",.01)
 ;;305.52
 ;;9002226.02101,"1056,305.52 ",.02)
 ;;305.52
 ;;9002226.02101,"1056,305.60 ",.01)
 ;;305.60
 ;;9002226.02101,"1056,305.60 ",.02)
 ;;305.60
 ;;9002226.02101,"1056,305.61 ",.01)
 ;;305.61
 ;;9002226.02101,"1056,305.61 ",.02)
 ;;305.61
 ;;9002226.02101,"1056,305.62 ",.01)
 ;;305.62
 ;;9002226.02101,"1056,305.62 ",.02)
 ;;305.62
 ;;9002226.02101,"1056,305.70 ",.01)
 ;;305.70
 ;;9002226.02101,"1056,305.70 ",.02)
 ;;305.70
 ;;9002226.02101,"1056,305.71 ",.01)
 ;;305.71
 ;;9002226.02101,"1056,305.71 ",.02)
 ;;305.71
 ;;9002226.02101,"1056,305.72 ",.01)
 ;;305.72
 ;;9002226.02101,"1056,305.72 ",.02)
 ;;305.72
 ;;9002226.02101,"1056,305.80 ",.01)
 ;;305.80
 ;;9002226.02101,"1056,305.80 ",.02)
 ;;305.80
 ;;9002226.02101,"1056,305.81 ",.01)
 ;;305.81
 ;;9002226.02101,"1056,305.81 ",.02)
 ;;305.81
 ;;9002226.02101,"1056,305.82 ",.01)
 ;;305.82
 ;;9002226.02101,"1056,305.82 ",.02)
 ;;305.82
 ;;9002226.02101,"1056,305.90 ",.01)
 ;;305.90
 ;;9002226.02101,"1056,305.90 ",.02)
 ;;305.90
 ;;9002226.02101,"1056,305.91 ",.01)
 ;;305.91
 ;;9002226.02101,"1056,305.91 ",.02)
 ;;305.91
 ;;9002226.02101,"1056,305.92 ",.01)
 ;;305.92
 ;
OTHER ; OTHER ROUTINES
 D ^BGPM5AZI
 Q
