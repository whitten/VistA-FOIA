BGP9SXRM ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"493,54569-8577-00 ",.02)
 ;;54569-8577-00
 ;;9002226.02101,"493,54868-0365-00 ",.01)
 ;;54868-0365-00
 ;;9002226.02101,"493,54868-0365-00 ",.02)
 ;;54868-0365-00
 ;;9002226.02101,"493,54868-0365-02 ",.01)
 ;;54868-0365-02
 ;;9002226.02101,"493,54868-0365-02 ",.02)
 ;;54868-0365-02
 ;;9002226.02101,"493,54868-0451-00 ",.01)
 ;;54868-0451-00
 ;;9002226.02101,"493,54868-0451-00 ",.02)
 ;;54868-0451-00
 ;;9002226.02101,"493,54868-0451-01 ",.01)
 ;;54868-0451-01
 ;;9002226.02101,"493,54868-0451-01 ",.02)
 ;;54868-0451-01
 ;;9002226.02101,"493,54868-0451-02 ",.01)
 ;;54868-0451-02
 ;;9002226.02101,"493,54868-0451-02 ",.02)
 ;;54868-0451-02
 ;;9002226.02101,"493,54868-0451-03 ",.01)
 ;;54868-0451-03
 ;;9002226.02101,"493,54868-0451-03 ",.02)
 ;;54868-0451-03
 ;;9002226.02101,"493,54868-0452-03 ",.01)
 ;;54868-0452-03
 ;;9002226.02101,"493,54868-0452-03 ",.02)
 ;;54868-0452-03
 ;;9002226.02101,"493,54868-0453-00 ",.01)
 ;;54868-0453-00
 ;;9002226.02101,"493,54868-0453-00 ",.02)
 ;;54868-0453-00
 ;;9002226.02101,"493,54868-0453-01 ",.01)
 ;;54868-0453-01
 ;;9002226.02101,"493,54868-0453-01 ",.02)
 ;;54868-0453-01
 ;;9002226.02101,"493,54868-0453-02 ",.01)
 ;;54868-0453-02
 ;;9002226.02101,"493,54868-0453-02 ",.02)
 ;;54868-0453-02
 ;;9002226.02101,"493,54868-0453-04 ",.01)
 ;;54868-0453-04
 ;;9002226.02101,"493,54868-0453-04 ",.02)
 ;;54868-0453-04
 ;;9002226.02101,"493,54868-0453-05 ",.01)
 ;;54868-0453-05
 ;;9002226.02101,"493,54868-0453-05 ",.02)
 ;;54868-0453-05
 ;;9002226.02101,"493,54868-0494-00 ",.01)
 ;;54868-0494-00
 ;;9002226.02101,"493,54868-0494-00 ",.02)
 ;;54868-0494-00
 ;;9002226.02101,"493,54868-0494-01 ",.01)
 ;;54868-0494-01
 ;;9002226.02101,"493,54868-0494-01 ",.02)
 ;;54868-0494-01
 ;;9002226.02101,"493,54868-0494-02 ",.01)
 ;;54868-0494-02
 ;;9002226.02101,"493,54868-0494-02 ",.02)
 ;;54868-0494-02
 ;;9002226.02101,"493,54868-0495-00 ",.01)
 ;;54868-0495-00
 ;;9002226.02101,"493,54868-0495-00 ",.02)
 ;;54868-0495-00
 ;;9002226.02101,"493,54868-1261-00 ",.01)
 ;;54868-1261-00
 ;;9002226.02101,"493,54868-1261-00 ",.02)
 ;;54868-1261-00
 ;;9002226.02101,"493,54868-1262-00 ",.01)
 ;;54868-1262-00
 ;;9002226.02101,"493,54868-1262-00 ",.02)
 ;;54868-1262-00
 ;;9002226.02101,"493,54868-2702-00 ",.01)
 ;;54868-2702-00
 ;;9002226.02101,"493,54868-2702-00 ",.02)
 ;;54868-2702-00
 ;;9002226.02101,"493,54868-3653-00 ",.01)
 ;;54868-3653-00
 ;;9002226.02101,"493,54868-3653-00 ",.02)
 ;;54868-3653-00
 ;;9002226.02101,"493,54868-3795-00 ",.01)
 ;;54868-3795-00
 ;;9002226.02101,"493,54868-3795-00 ",.02)
 ;;54868-3795-00
 ;;9002226.02101,"493,54868-3796-00 ",.01)
 ;;54868-3796-00
 ;;9002226.02101,"493,54868-3796-00 ",.02)
 ;;54868-3796-00
 ;;9002226.02101,"493,54868-3797-00 ",.01)
 ;;54868-3797-00
 ;;9002226.02101,"493,54868-3797-00 ",.02)
 ;;54868-3797-00
 ;;9002226.02101,"493,54868-3799-00 ",.01)
 ;;54868-3799-00
 ;;9002226.02101,"493,54868-3799-00 ",.02)
 ;;54868-3799-00
 ;;9002226.02101,"493,54868-3800-00 ",.01)
 ;;54868-3800-00
 ;;9002226.02101,"493,54868-3800-00 ",.02)
 ;;54868-3800-00
 ;;9002226.02101,"493,54868-3857-00 ",.01)
 ;;54868-3857-00
 ;;9002226.02101,"493,54868-3857-00 ",.02)
 ;;54868-3857-00
 ;;9002226.02101,"493,54868-4030-00 ",.01)
 ;;54868-4030-00
 ;;9002226.02101,"493,54868-4030-00 ",.02)
 ;;54868-4030-00
 ;;9002226.02101,"493,54868-4030-01 ",.01)
 ;;54868-4030-01
 ;;9002226.02101,"493,54868-4030-01 ",.02)
 ;;54868-4030-01
 ;;9002226.02101,"493,54868-4089-00 ",.01)
 ;;54868-4089-00
 ;;9002226.02101,"493,54868-4089-00 ",.02)
 ;;54868-4089-00
 ;;9002226.02101,"493,54868-4170-00 ",.01)
 ;;54868-4170-00
 ;;9002226.02101,"493,54868-4170-00 ",.02)
 ;;54868-4170-00
 ;;9002226.02101,"493,54868-4384-00 ",.01)
 ;;54868-4384-00
 ;;9002226.02101,"493,54868-4384-00 ",.02)
 ;;54868-4384-00
 ;;9002226.02101,"493,54868-4386-00 ",.01)
 ;;54868-4386-00
 ;;9002226.02101,"493,54868-4386-00 ",.02)
 ;;54868-4386-00
 ;;9002226.02101,"493,54868-4462-00 ",.01)
 ;;54868-4462-00
 ;;9002226.02101,"493,54868-4462-00 ",.02)
 ;;54868-4462-00
 ;;9002226.02101,"493,54868-4463-00 ",.01)
 ;;54868-4463-00
 ;;9002226.02101,"493,54868-4463-00 ",.02)
 ;;54868-4463-00
 ;;9002226.02101,"493,54868-4671-00 ",.01)
 ;;54868-4671-00
 ;;9002226.02101,"493,54868-4671-00 ",.02)
 ;;54868-4671-00
 ;;9002226.02101,"493,54868-4679-00 ",.01)
 ;;54868-4679-00
 ;;9002226.02101,"493,54868-4679-00 ",.02)
 ;;54868-4679-00
 ;;9002226.02101,"493,54868-4830-00 ",.01)
 ;;54868-4830-00
 ;;9002226.02101,"493,54868-4830-00 ",.02)
 ;;54868-4830-00
 ;;9002226.02101,"493,54868-5480-00 ",.01)
 ;;54868-5480-00
 ;;9002226.02101,"493,54868-5480-00 ",.02)
 ;;54868-5480-00
 ;;9002226.02101,"493,55289-0047-25 ",.01)
 ;;55289-0047-25
 ;;9002226.02101,"493,55289-0047-25 ",.02)
 ;;55289-0047-25
 ;;9002226.02101,"493,55289-0047-30 ",.01)
 ;;55289-0047-30
 ;;9002226.02101,"493,55289-0047-30 ",.02)
 ;;55289-0047-30
 ;;9002226.02101,"493,55289-0047-42 ",.01)
 ;;55289-0047-42
 ;;9002226.02101,"493,55289-0047-42 ",.02)
 ;;55289-0047-42
 ;;9002226.02101,"493,55289-0047-90 ",.01)
 ;;55289-0047-90
 ;;9002226.02101,"493,55289-0047-90 ",.02)
 ;;55289-0047-90
 ;;9002226.02101,"493,55289-0101-30 ",.01)
 ;;55289-0101-30
