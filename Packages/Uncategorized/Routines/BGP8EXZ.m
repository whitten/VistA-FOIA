BGP8EXZ ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 28, 2007 ;
 ;;8.0;IHS CLINICAL REPORTING;;MAR 12, 2008
 ;;;BGP6;;SEP 28, 2007
 ;;BGP HEDIS ANTIANXIETY NDC
 ;
 ; This routine loads Taxonomy BGP HEDIS ANTIANXIETY NDC
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
 ;;21,"00008-0002-03 ")
 ;;4
 ;;21,"00008-0091-02 ")
 ;;1
 ;;21,"00037-1001-01 ")
 ;;24
 ;;21,"00037-1001-02 ")
 ;;25
 ;;21,"00037-1001-03 ")
 ;;26
 ;;21,"00037-1101-01 ")
 ;;5
 ;;21,"00115-3888-01 ")
 ;;6
 ;;21,"00115-3888-03 ")
 ;;7
 ;;21,"00115-3890-01 ")
 ;;27
 ;;21,"00115-3890-03 ")
 ;;28
 ;;21,"00182-0294-01 ")
 ;;29
 ;;21,"00182-0294-10 ")
 ;;30
 ;;21,"00185-0716-01 ")
 ;;8
 ;;21,"00185-0716-10 ")
 ;;9
 ;;21,"00185-0717-01 ")
 ;;31
 ;;21,"00185-0717-10 ")
 ;;32
 ;;21,"00247-1031-28 ")
 ;;33
 ;;21,"00364-0160-01 ")
 ;;10
 ;;21,"00364-0160-02 ")
 ;;11
 ;;21,"00364-0161-01 ")
 ;;34
 ;;21,"00364-0161-02 ")
 ;;35
 ;;21,"00405-0115-01 ")
 ;;12
 ;;21,"00405-0115-03 ")
 ;;13
 ;;21,"00405-0116-01 ")
 ;;36
 ;;21,"00405-0116-03 ")
 ;;37
 ;;21,"00536-4005-01 ")
 ;;14
 ;;21,"00536-4006-01 ")
 ;;38
 ;;21,"00591-5238-01 ")
 ;;39
 ;;21,"00591-5238-10 ")
 ;;40
 ;;21,"00591-5239-01 ")
 ;;15
 ;;21,"00591-5239-10 ")
 ;;16
 ;;21,"00615-0414-10 ")
 ;;17
 ;;21,"00615-0447-01 ")
 ;;41
 ;;21,"00839-5004-06 ")
 ;;42
 ;;21,"00839-5004-16 ")
 ;;43
 ;;21,"00839-5070-06 ")
 ;;18
 ;;21,"00904-0044-60 ")
 ;;19
 ;;21,"00904-0044-80 ")
 ;;20
 ;;21,"00904-0045-60 ")
 ;;44
 ;;21,"00904-0045-61 ")
 ;;45
 ;;21,"00904-0045-80 ")
 ;;46
 ;;21,"10551-0091-10 ")
 ;;2
 ;;21,"51641-0325-01 ")
 ;;47
 ;;21,"51641-0325-10 ")
 ;;48
 ;;21,"51641-0327-01 ")
 ;;21
 ;;21,"51641-0327-10 ")
 ;;22
 ;;21,"52544-0804-01 ")
 ;;23
 ;;21,"52544-0805-01 ")
 ;;49
 ;;21,"54569-0935-00 ")
 ;;50
 ;;21,"54569-0935-02 ")
 ;;51
 ;;21,"64248-0091-10 ")
 ;;3
 ;;9002226,445,.01)
 ;;BGP HEDIS ANTIANXIETY NDC
 ;;9002226,445,.02)
 ;;@
 ;;9002226,445,.04)
 ;;n
 ;;9002226,445,.06)
 ;;@
 ;;9002226,445,.08)
 ;;@
 ;;9002226,445,.09)
 ;;@
 ;;9002226,445,.11)
 ;;@
 ;;9002226,445,.12)
 ;;@
 ;;9002226,445,.13)
 ;;1
 ;;9002226,445,.14)
 ;;@
 ;;9002226,445,.15)
 ;;@
 ;;9002226,445,.16)
 ;;@
 ;;9002226,445,.17)
 ;;@
 ;;9002226,445,3101)
 ;;@
 ;;9002226.02101,"445,00008-0002-03 ",.01)
 ;;00008-0002-03
 ;;9002226.02101,"445,00008-0002-03 ",.02)
 ;;00008-0002-03
 ;;9002226.02101,"445,00008-0091-02 ",.01)
 ;;00008-0091-02
 ;;9002226.02101,"445,00008-0091-02 ",.02)
 ;;00008-0091-02
 ;;9002226.02101,"445,00037-1001-01 ",.01)
 ;;00037-1001-01
 ;;9002226.02101,"445,00037-1001-01 ",.02)
 ;;00037-1001-01
 ;;9002226.02101,"445,00037-1001-02 ",.01)
 ;;00037-1001-02
 ;;9002226.02101,"445,00037-1001-02 ",.02)
 ;;00037-1001-02
 ;;9002226.02101,"445,00037-1001-03 ",.01)
 ;;00037-1001-03
 ;;9002226.02101,"445,00037-1001-03 ",.02)
 ;;00037-1001-03
 ;;9002226.02101,"445,00037-1101-01 ",.01)
 ;;00037-1101-01
 ;;9002226.02101,"445,00037-1101-01 ",.02)
 ;;00037-1101-01
 ;;9002226.02101,"445,00115-3888-01 ",.01)
 ;;00115-3888-01
 ;;9002226.02101,"445,00115-3888-01 ",.02)
 ;;00115-3888-01
 ;;9002226.02101,"445,00115-3888-03 ",.01)
 ;;00115-3888-03
 ;;9002226.02101,"445,00115-3888-03 ",.02)
 ;;00115-3888-03
 ;;9002226.02101,"445,00115-3890-01 ",.01)
 ;;00115-3890-01
 ;;9002226.02101,"445,00115-3890-01 ",.02)
 ;;00115-3890-01
 ;;9002226.02101,"445,00115-3890-03 ",.01)
 ;;00115-3890-03
 ;;9002226.02101,"445,00115-3890-03 ",.02)
 ;;00115-3890-03
 ;;9002226.02101,"445,00182-0294-01 ",.01)
 ;;00182-0294-01
 ;;9002226.02101,"445,00182-0294-01 ",.02)
 ;;00182-0294-01
 ;;9002226.02101,"445,00182-0294-10 ",.01)
 ;;00182-0294-10
 ;;9002226.02101,"445,00182-0294-10 ",.02)
 ;;00182-0294-10
 ;;9002226.02101,"445,00185-0716-01 ",.01)
 ;;00185-0716-01
 ;;9002226.02101,"445,00185-0716-01 ",.02)
 ;;00185-0716-01
 ;;9002226.02101,"445,00185-0716-10 ",.01)
 ;;00185-0716-10
 ;;9002226.02101,"445,00185-0716-10 ",.02)
 ;;00185-0716-10
 ;;9002226.02101,"445,00185-0717-01 ",.01)
 ;;00185-0717-01
 ;;9002226.02101,"445,00185-0717-01 ",.02)
 ;;00185-0717-01
 ;;9002226.02101,"445,00185-0717-10 ",.01)
 ;;00185-0717-10
 ;;9002226.02101,"445,00185-0717-10 ",.02)
 ;;00185-0717-10
 ;;9002226.02101,"445,00247-1031-28 ",.01)
 ;;00247-1031-28
 ;;9002226.02101,"445,00247-1031-28 ",.02)
 ;;00247-1031-28
 ;;9002226.02101,"445,00364-0160-01 ",.01)
 ;;00364-0160-01
 ;;9002226.02101,"445,00364-0160-01 ",.02)
 ;;00364-0160-01
 ;;9002226.02101,"445,00364-0160-02 ",.01)
 ;;00364-0160-02
 ;;9002226.02101,"445,00364-0160-02 ",.02)
 ;;00364-0160-02
 ;;9002226.02101,"445,00364-0161-01 ",.01)
 ;;00364-0161-01
 ;;9002226.02101,"445,00364-0161-01 ",.02)
 ;;00364-0161-01
 ;;9002226.02101,"445,00364-0161-02 ",.01)
 ;;00364-0161-02
 ;;9002226.02101,"445,00364-0161-02 ",.02)
 ;;00364-0161-02
 ;;9002226.02101,"445,00405-0115-01 ",.01)
 ;;00405-0115-01
 ;;9002226.02101,"445,00405-0115-01 ",.02)
 ;;00405-0115-01
 ;;9002226.02101,"445,00405-0115-03 ",.01)
 ;;00405-0115-03
 ;;9002226.02101,"445,00405-0115-03 ",.02)
 ;;00405-0115-03
 ;;9002226.02101,"445,00405-0116-01 ",.01)
 ;;00405-0116-01
 ;;9002226.02101,"445,00405-0116-01 ",.02)
 ;;00405-0116-01
 ;;9002226.02101,"445,00405-0116-03 ",.01)
 ;;00405-0116-03
 ;;9002226.02101,"445,00405-0116-03 ",.02)
 ;;00405-0116-03
 ;;9002226.02101,"445,00536-4005-01 ",.01)
 ;;00536-4005-01
 ;;9002226.02101,"445,00536-4005-01 ",.02)
 ;;00536-4005-01
 ;;9002226.02101,"445,00536-4006-01 ",.01)
 ;;00536-4006-01
 ;;9002226.02101,"445,00536-4006-01 ",.02)
 ;;00536-4006-01
 ;;9002226.02101,"445,00591-5238-01 ",.01)
 ;;00591-5238-01
 ;;9002226.02101,"445,00591-5238-01 ",.02)
 ;;00591-5238-01
 ;;9002226.02101,"445,00591-5238-10 ",.01)
 ;;00591-5238-10
 ;;9002226.02101,"445,00591-5238-10 ",.02)
 ;;00591-5238-10
 ;;9002226.02101,"445,00591-5239-01 ",.01)
 ;;00591-5239-01
 ;;9002226.02101,"445,00591-5239-01 ",.02)
 ;;00591-5239-01
 ;;9002226.02101,"445,00591-5239-10 ",.01)
 ;;00591-5239-10
 ;;9002226.02101,"445,00591-5239-10 ",.02)
 ;;00591-5239-10
 ;;9002226.02101,"445,00615-0414-10 ",.01)
 ;;00615-0414-10
 ;;9002226.02101,"445,00615-0414-10 ",.02)
 ;;00615-0414-10
 ;;9002226.02101,"445,00615-0447-01 ",.01)
 ;;00615-0447-01
 ;;9002226.02101,"445,00615-0447-01 ",.02)
 ;;00615-0447-01
 ;;9002226.02101,"445,00839-5004-06 ",.01)
 ;;00839-5004-06
 ;;9002226.02101,"445,00839-5004-06 ",.02)
 ;;00839-5004-06
 ;;9002226.02101,"445,00839-5004-16 ",.01)
 ;;00839-5004-16
 ;;9002226.02101,"445,00839-5004-16 ",.02)
 ;;00839-5004-16
 ;;9002226.02101,"445,00839-5070-06 ",.01)
 ;;00839-5070-06
 ;;9002226.02101,"445,00839-5070-06 ",.02)
 ;;00839-5070-06
 ;;9002226.02101,"445,00904-0044-60 ",.01)
 ;;00904-0044-60
 ;;9002226.02101,"445,00904-0044-60 ",.02)
 ;;00904-0044-60
 ;;9002226.02101,"445,00904-0044-80 ",.01)
 ;;00904-0044-80
 ;;9002226.02101,"445,00904-0044-80 ",.02)
 ;;00904-0044-80
 ;;9002226.02101,"445,00904-0045-60 ",.01)
 ;;00904-0045-60
 ;;9002226.02101,"445,00904-0045-60 ",.02)
 ;;00904-0045-60
 ;;9002226.02101,"445,00904-0045-61 ",.01)
 ;;00904-0045-61
 ;;9002226.02101,"445,00904-0045-61 ",.02)
 ;;00904-0045-61
 ;;9002226.02101,"445,00904-0045-80 ",.01)
 ;;00904-0045-80
 ;;9002226.02101,"445,00904-0045-80 ",.02)
 ;;00904-0045-80
 ;;9002226.02101,"445,10551-0091-10 ",.01)
 ;;10551-0091-10
 ;;9002226.02101,"445,10551-0091-10 ",.02)
 ;;10551-0091-10
 ;;9002226.02101,"445,51641-0325-01 ",.01)
 ;;51641-0325-01
 ;;9002226.02101,"445,51641-0325-01 ",.02)
 ;;51641-0325-01
 ;;9002226.02101,"445,51641-0325-10 ",.01)
 ;;51641-0325-10
 ;;9002226.02101,"445,51641-0325-10 ",.02)
 ;;51641-0325-10
 ;;9002226.02101,"445,51641-0327-01 ",.01)
 ;;51641-0327-01
 ;;9002226.02101,"445,51641-0327-01 ",.02)
 ;;51641-0327-01
 ;;9002226.02101,"445,51641-0327-10 ",.01)
 ;;51641-0327-10
 ;;9002226.02101,"445,51641-0327-10 ",.02)
 ;;51641-0327-10
 ;;9002226.02101,"445,52544-0804-01 ",.01)
 ;;52544-0804-01
 ;;9002226.02101,"445,52544-0804-01 ",.02)
 ;;52544-0804-01
 ;;9002226.02101,"445,52544-0805-01 ",.01)
 ;;52544-0805-01
 ;;9002226.02101,"445,52544-0805-01 ",.02)
 ;;52544-0805-01
 ;;9002226.02101,"445,54569-0935-00 ",.01)
 ;;54569-0935-00
 ;;9002226.02101,"445,54569-0935-00 ",.02)
 ;;54569-0935-00
 ;;9002226.02101,"445,54569-0935-02 ",.01)
 ;;54569-0935-02
 ;;9002226.02101,"445,54569-0935-02 ",.02)
 ;;54569-0935-02
 ;;9002226.02101,"445,64248-0091-10 ",.01)
 ;;64248-0091-10
 ;;9002226.02101,"445,64248-0091-10 ",.02)
 ;;64248-0091-10
 ;
OTHER ; OTHER ROUTINES
 Q
