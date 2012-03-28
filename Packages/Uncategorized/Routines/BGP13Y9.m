BGP13Y9 ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"1101,00228-2475-10 ",.01)
 ;;00228-2475-10
 ;;9002226.02101,"1101,00228-2475-10 ",.02)
 ;;00228-2475-10
 ;;9002226.02101,"1101,00228-2475-50 ",.01)
 ;;00228-2475-50
 ;;9002226.02101,"1101,00228-2475-50 ",.02)
 ;;00228-2475-50
 ;;9002226.02101,"1101,00228-2475-96 ",.01)
 ;;00228-2475-96
 ;;9002226.02101,"1101,00228-2475-96 ",.02)
 ;;00228-2475-96
 ;;9002226.02101,"1101,00228-2577-03 ",.01)
 ;;00228-2577-03
 ;;9002226.02101,"1101,00228-2577-03 ",.02)
 ;;00228-2577-03
 ;;9002226.02101,"1101,00228-2577-09 ",.01)
 ;;00228-2577-09
 ;;9002226.02101,"1101,00228-2577-09 ",.02)
 ;;00228-2577-09
 ;;9002226.02101,"1101,00228-2577-50 ",.01)
 ;;00228-2577-50
 ;;9002226.02101,"1101,00228-2577-50 ",.02)
 ;;00228-2577-50
 ;;9002226.02101,"1101,00228-2577-73 ",.01)
 ;;00228-2577-73
 ;;9002226.02101,"1101,00228-2577-73 ",.02)
 ;;00228-2577-73
 ;;9002226.02101,"1101,00228-2578-03 ",.01)
 ;;00228-2578-03
 ;;9002226.02101,"1101,00228-2578-03 ",.02)
 ;;00228-2578-03
 ;;9002226.02101,"1101,00228-2578-09 ",.01)
 ;;00228-2578-09
 ;;9002226.02101,"1101,00228-2578-09 ",.02)
 ;;00228-2578-09
 ;;9002226.02101,"1101,00228-2578-50 ",.01)
 ;;00228-2578-50
 ;;9002226.02101,"1101,00228-2578-50 ",.02)
 ;;00228-2578-50
 ;;9002226.02101,"1101,00228-2578-73 ",.01)
 ;;00228-2578-73
 ;;9002226.02101,"1101,00228-2578-73 ",.02)
 ;;00228-2578-73
 ;;9002226.02101,"1101,00228-2579-03 ",.01)
 ;;00228-2579-03
 ;;9002226.02101,"1101,00228-2579-03 ",.02)
 ;;00228-2579-03
 ;;9002226.02101,"1101,00228-2579-09 ",.01)
 ;;00228-2579-09
 ;;9002226.02101,"1101,00228-2579-09 ",.02)
 ;;00228-2579-09
 ;;9002226.02101,"1101,00228-2579-50 ",.01)
 ;;00228-2579-50
 ;;9002226.02101,"1101,00228-2579-50 ",.02)
 ;;00228-2579-50
 ;;9002226.02101,"1101,00228-2588-03 ",.01)
 ;;00228-2588-03
 ;;9002226.02101,"1101,00228-2588-03 ",.02)
 ;;00228-2588-03
 ;;9002226.02101,"1101,00228-2588-09 ",.01)
 ;;00228-2588-09
 ;;9002226.02101,"1101,00228-2588-09 ",.02)
 ;;00228-2588-09
 ;;9002226.02101,"1101,00228-2588-50 ",.01)
 ;;00228-2588-50
 ;;9002226.02101,"1101,00228-2588-50 ",.02)
 ;;00228-2588-50
 ;;9002226.02101,"1101,00228-2588-73 ",.01)
 ;;00228-2588-73
 ;;9002226.02101,"1101,00228-2588-73 ",.02)
 ;;00228-2588-73
 ;;9002226.02101,"1101,00247-0451-01 ",.01)
 ;;00247-0451-01
 ;;9002226.02101,"1101,00247-0451-01 ",.02)
 ;;00247-0451-01
 ;;9002226.02101,"1101,00247-0451-02 ",.01)
 ;;00247-0451-02
 ;;9002226.02101,"1101,00247-0451-02 ",.02)
 ;;00247-0451-02
 ;;9002226.02101,"1101,00247-0451-03 ",.01)
 ;;00247-0451-03
 ;;9002226.02101,"1101,00247-0451-03 ",.02)
 ;;00247-0451-03
 ;;9002226.02101,"1101,00247-0451-04 ",.01)
 ;;00247-0451-04
 ;;9002226.02101,"1101,00247-0451-04 ",.02)
 ;;00247-0451-04
 ;;9002226.02101,"1101,00247-0451-10 ",.01)
 ;;00247-0451-10
 ;;9002226.02101,"1101,00247-0451-10 ",.02)
 ;;00247-0451-10
 ;;9002226.02101,"1101,00247-0451-20 ",.01)
 ;;00247-0451-20
 ;;9002226.02101,"1101,00247-0451-20 ",.02)
 ;;00247-0451-20
 ;;9002226.02101,"1101,00247-0451-30 ",.01)
 ;;00247-0451-30
 ;;9002226.02101,"1101,00247-0451-30 ",.02)
 ;;00247-0451-30
 ;;9002226.02101,"1101,00247-0451-60 ",.01)
 ;;00247-0451-60
 ;;9002226.02101,"1101,00247-0451-60 ",.02)
 ;;00247-0451-60
 ;;9002226.02101,"1101,00247-1099-60 ",.01)
 ;;00247-1099-60
 ;;9002226.02101,"1101,00247-1099-60 ",.02)
 ;;00247-1099-60
 ;;9002226.02101,"1101,00247-1100-30 ",.01)
 ;;00247-1100-30
 ;;9002226.02101,"1101,00247-1100-30 ",.02)
 ;;00247-1100-30
 ;;9002226.02101,"1101,00247-1100-60 ",.01)
 ;;00247-1100-60
 ;;9002226.02101,"1101,00247-1100-60 ",.02)
 ;;00247-1100-60
 ;;9002226.02101,"1101,00247-1101-30 ",.01)
 ;;00247-1101-30
 ;;9002226.02101,"1101,00247-1101-30 ",.02)
 ;;00247-1101-30
 ;;9002226.02101,"1101,00247-1101-60 ",.01)
 ;;00247-1101-60
 ;;9002226.02101,"1101,00247-1101-60 ",.02)
 ;;00247-1101-60
 ;;9002226.02101,"1101,00247-1102-30 ",.01)
 ;;00247-1102-30
 ;;9002226.02101,"1101,00247-1102-30 ",.02)
 ;;00247-1102-30
 ;;9002226.02101,"1101,00247-1102-60 ",.01)
 ;;00247-1102-60
 ;;9002226.02101,"1101,00247-1102-60 ",.02)
 ;;00247-1102-60
 ;;9002226.02101,"1101,00247-1114-30 ",.01)
 ;;00247-1114-30
 ;;9002226.02101,"1101,00247-1114-30 ",.02)
 ;;00247-1114-30
 ;;9002226.02101,"1101,00247-1114-60 ",.01)
 ;;00247-1114-60
 ;;9002226.02101,"1101,00247-1114-60 ",.02)
 ;;00247-1114-60
 ;;9002226.02101,"1101,00247-1115-30 ",.01)
 ;;00247-1115-30
 ;;9002226.02101,"1101,00247-1115-30 ",.02)
 ;;00247-1115-30
 ;;9002226.02101,"1101,00247-1115-60 ",.01)
 ;;00247-1115-60
 ;;9002226.02101,"1101,00247-1115-60 ",.02)
 ;;00247-1115-60
 ;;9002226.02101,"1101,00247-1117-03 ",.01)
 ;;00247-1117-03
 ;;9002226.02101,"1101,00247-1117-03 ",.02)
 ;;00247-1117-03
 ;;9002226.02101,"1101,00247-1117-30 ",.01)
 ;;00247-1117-30
 ;;9002226.02101,"1101,00247-1117-30 ",.02)
 ;;00247-1117-30
 ;;9002226.02101,"1101,00247-1117-60 ",.01)
 ;;00247-1117-60
 ;;9002226.02101,"1101,00247-1117-60 ",.02)
 ;;00247-1117-60
 ;;9002226.02101,"1101,00247-1118-03 ",.01)
 ;;00247-1118-03
 ;;9002226.02101,"1101,00247-1118-03 ",.02)
 ;;00247-1118-03
 ;;9002226.02101,"1101,00247-1118-30 ",.01)
 ;;00247-1118-30
 ;;9002226.02101,"1101,00247-1118-30 ",.02)
 ;;00247-1118-30
 ;;9002226.02101,"1101,00247-1135-30 ",.01)
 ;;00247-1135-30
 ;;9002226.02101,"1101,00247-1135-30 ",.02)
 ;;00247-1135-30
 ;;9002226.02101,"1101,00247-1135-60 ",.01)
 ;;00247-1135-60
 ;;9002226.02101,"1101,00247-1135-60 ",.02)
 ;;00247-1135-60
 ;;9002226.02101,"1101,00247-1137-03 ",.01)
 ;;00247-1137-03
 ;;9002226.02101,"1101,00247-1137-03 ",.02)
 ;;00247-1137-03
 ;;9002226.02101,"1101,00247-1221-00 ",.01)
 ;;00247-1221-00
 ;;9002226.02101,"1101,00247-1221-00 ",.02)
 ;;00247-1221-00
 ;;9002226.02101,"1101,00247-1221-50 ",.01)
 ;;00247-1221-50
 ;;9002226.02101,"1101,00247-1221-50 ",.02)
 ;;00247-1221-50
 ;;9002226.02101,"1101,00247-1221-60 ",.01)
 ;;00247-1221-60
 ;;9002226.02101,"1101,00247-1221-60 ",.02)
 ;;00247-1221-60
 ;;9002226.02101,"1101,00258-3687-90 ",.01)
 ;;00258-3687-90
 ;;9002226.02101,"1101,00258-3687-90 ",.02)
 ;;00258-3687-90
 ;;9002226.02101,"1101,00258-3688-90 ",.01)
 ;;00258-3688-90
 ;;9002226.02101,"1101,00258-3688-90 ",.02)
 ;;00258-3688-90
 ;;9002226.02101,"1101,00258-3689-90 ",.01)
 ;;00258-3689-90
 ;;9002226.02101,"1101,00258-3689-90 ",.02)
 ;;00258-3689-90
 ;;9002226.02101,"1101,00258-3690-90 ",.01)
 ;;00258-3690-90
 ;;9002226.02101,"1101,00258-3690-90 ",.02)
 ;;00258-3690-90
 ;;9002226.02101,"1101,00258-3691-90 ",.01)
 ;;00258-3691-90
 ;;9002226.02101,"1101,00258-3691-90 ",.02)
 ;;00258-3691-90
 ;;9002226.02101,"1101,00258-3692-90 ",.01)
 ;;00258-3692-90
 ;;9002226.02101,"1101,00258-3692-90 ",.02)
 ;;00258-3692-90
 ;;9002226.02101,"1101,00310-0891-10 ",.01)
 ;;00310-0891-10
 ;;9002226.02101,"1101,00310-0891-10 ",.02)
 ;;00310-0891-10
 ;;9002226.02101,"1101,00310-0891-39 ",.01)
 ;;00310-0891-39
 ;;9002226.02101,"1101,00310-0891-39 ",.02)
 ;;00310-0891-39
 ;;9002226.02101,"1101,00310-0892-10 ",.01)
 ;;00310-0892-10
 ;;9002226.02101,"1101,00310-0892-10 ",.02)
 ;;00310-0892-10
 ;;9002226.02101,"1101,00310-0892-39 ",.01)
 ;;00310-0892-39
 ;;9002226.02101,"1101,00310-0892-39 ",.02)
 ;;00310-0892-39
 ;;9002226.02101,"1101,00310-0893-10 ",.01)
 ;;00310-0893-10
 ;;9002226.02101,"1101,00310-0893-10 ",.02)
 ;;00310-0893-10
 ;;9002226.02101,"1101,00310-0893-39 ",.01)
 ;;00310-0893-39
 ;;9002226.02101,"1101,00310-0893-39 ",.02)
 ;;00310-0893-39
 ;;9002226.02101,"1101,00310-0894-10 ",.01)
 ;;00310-0894-10
 ;;9002226.02101,"1101,00310-0894-10 ",.02)
 ;;00310-0894-10
 ;;9002226.02101,"1101,00364-2880-01 ",.01)
 ;;00364-2880-01
 ;;9002226.02101,"1101,00364-2880-01 ",.02)
 ;;00364-2880-01
 ;;9002226.02101,"1101,00364-2884-01 ",.01)
 ;;00364-2884-01
 ;;9002226.02101,"1101,00364-2884-01 ",.02)
 ;;00364-2884-01
 ;;9002226.02101,"1101,00364-2886-01 ",.01)
 ;;00364-2886-01
 ;;9002226.02101,"1101,00364-2886-01 ",.02)
 ;;00364-2886-01
 ;;9002226.02101,"1101,00378-0023-01 ",.01)
 ;;00378-0023-01
 ;;9002226.02101,"1101,00378-0023-01 ",.02)
 ;;00378-0023-01
 ;;9002226.02101,"1101,00378-0023-05 ",.01)
 ;;00378-0023-05
 ;;9002226.02101,"1101,00378-0023-05 ",.02)
 ;;00378-0023-05
 ;;9002226.02101,"1101,00378-0045-01 ",.01)
 ;;00378-0045-01
 ;;9002226.02101,"1101,00378-0045-01 ",.02)
 ;;00378-0045-01
 ;;9002226.02101,"1101,00378-0045-05 ",.01)
 ;;00378-0045-05
 ;;9002226.02101,"1101,00378-0045-05 ",.02)
 ;;00378-0045-05
 ;;9002226.02101,"1101,00378-0135-01 ",.01)
 ;;00378-0135-01
 ;;9002226.02101,"1101,00378-0135-01 ",.02)
 ;;00378-0135-01
 ;;9002226.02101,"1101,00378-0135-05 ",.01)
 ;;00378-0135-05
 ;;9002226.02101,"1101,00378-0135-05 ",.02)
 ;;00378-0135-05
 ;;9002226.02101,"1101,00378-0411-01 ",.01)
 ;;00378-0411-01
 ;;9002226.02101,"1101,00378-0411-01 ",.02)
 ;;00378-0411-01
 ;;9002226.02101,"1101,00378-0411-05 ",.01)
 ;;00378-0411-05
 ;;9002226.02101,"1101,00378-0411-05 ",.02)
 ;;00378-0411-05
 ;;9002226.02101,"1101,00378-0480-01 ",.01)
 ;;00378-0480-01
 ;;9002226.02101,"1101,00378-0480-01 ",.02)
 ;;00378-0480-01
 ;;9002226.02101,"1101,00378-0480-30 ",.01)
 ;;00378-0480-30
 ;;9002226.02101,"1101,00378-0480-30 ",.02)
 ;;00378-0480-30
 ;;9002226.02101,"1101,00378-0481-01 ",.01)
 ;;00378-0481-01
 ;;9002226.02101,"1101,00378-0481-01 ",.02)
 ;;00378-0481-01
 ;;9002226.02101,"1101,00378-0481-30 ",.01)
 ;;00378-0481-30
 ;;9002226.02101,"1101,00378-0481-30 ",.02)
 ;;00378-0481-30
 ;;9002226.02101,"1101,00378-0494-01 ",.01)
 ;;00378-0494-01
 ;;9002226.02101,"1101,00378-0494-01 ",.02)
 ;;00378-0494-01
 ;;9002226.02101,"1101,00378-0512-01 ",.01)
 ;;00378-0512-01
 ;;9002226.02101,"1101,00378-0512-01 ",.02)
 ;;00378-0512-01
 ;;9002226.02101,"1101,00378-0512-10 ",.01)
 ;;00378-0512-10
 ;;9002226.02101,"1101,00378-0512-10 ",.02)
 ;;00378-0512-10
 ;;9002226.02101,"1101,00378-0525-01 ",.01)
 ;;00378-0525-01
 ;;9002226.02101,"1101,00378-0525-01 ",.02)
 ;;00378-0525-01
 ;;9002226.02101,"1101,00378-0772-01 ",.01)
 ;;00378-0772-01
 ;;9002226.02101,"1101,00378-0772-01 ",.02)
 ;;00378-0772-01
 ;;9002226.02101,"1101,00378-0772-05 ",.01)
 ;;00378-0772-05
 ;;9002226.02101,"1101,00378-0772-05 ",.02)
 ;;00378-0772-05
 ;;9002226.02101,"1101,00378-1020-05 ",.01)
 ;;00378-1020-05
 ;;9002226.02101,"1101,00378-1020-05 ",.02)
 ;;00378-1020-05
 ;;9002226.02101,"1101,00378-1020-77 ",.01)
 ;;00378-1020-77
 ;;9002226.02101,"1101,00378-1020-77 ",.02)
 ;;00378-1020-77
 ;;9002226.02101,"1101,00378-1120-01 ",.01)
 ;;00378-1120-01
 ;;9002226.02101,"1101,00378-1120-01 ",.02)
 ;;00378-1120-01
 ;;9002226.02101,"1101,00378-1120-93 ",.01)
 ;;00378-1120-93
 ;;9002226.02101,"1101,00378-1120-93 ",.02)
 ;;00378-1120-93
 ;;9002226.02101,"1101,00378-1180-01 ",.01)
 ;;00378-1180-01
 ;;9002226.02101,"1101,00378-1180-01 ",.02)
 ;;00378-1180-01
 ;;9002226.02101,"1101,00378-1180-05 ",.01)
 ;;00378-1180-05
 ;;9002226.02101,"1101,00378-1180-05 ",.02)
 ;;00378-1180-05
 ;;9002226.02101,"1101,00378-1411-01 ",.01)
 ;;00378-1411-01
 ;;9002226.02101,"1101,00378-1411-01 ",.02)
 ;;00378-1411-01
 ;;9002226.02101,"1101,00378-1411-05 ",.01)
 ;;00378-1411-05
 ;;9002226.02101,"1101,00378-1411-05 ",.02)
 ;;00378-1411-05
 ;;9002226.02101,"1101,00378-1411-77 ",.01)
 ;;00378-1411-77
 ;;9002226.02101,"1101,00378-1411-77 ",.02)
 ;;00378-1411-77
 ;;9002226.02101,"1101,00378-1430-05 ",.01)
 ;;00378-1430-05
 ;;9002226.02101,"1101,00378-1430-05 ",.02)
 ;;00378-1430-05
 ;;9002226.02101,"1101,00378-1430-77 ",.01)
 ;;00378-1430-77
 ;;9002226.02101,"1101,00378-1430-77 ",.02)
 ;;00378-1430-77
 ;;9002226.02101,"1101,00378-2120-01 ",.01)
 ;;00378-2120-01
 ;;9002226.02101,"1101,00378-2120-01 ",.02)
 ;;00378-2120-01
 ;;9002226.02101,"1101,00378-2120-93 ",.01)
 ;;00378-2120-93
 ;;9002226.02101,"1101,00378-2120-93 ",.02)
 ;;00378-2120-93
 ;;9002226.02101,"1101,00378-2180-01 ",.01)
 ;;00378-2180-01
 ;;9002226.02101,"1101,00378-2180-01 ",.02)
 ;;00378-2180-01
 ;;9002226.02101,"1101,00378-2180-05 ",.01)
 ;;00378-2180-05
 ;;9002226.02101,"1101,00378-2180-05 ",.02)
 ;;00378-2180-05
 ;;9002226.02101,"1101,00378-2222-01 ",.01)
 ;;00378-2222-01
 ;;9002226.02101,"1101,00378-2222-01 ",.02)
 ;;00378-2222-01
 ;;9002226.02101,"1101,00378-2223-01 ",.01)
 ;;00378-2223-01
 ;;9002226.02101,"1101,00378-2223-01 ",.02)
 ;;00378-2223-01
 ;;9002226.02101,"1101,00378-2224-01 ",.01)
 ;;00378-2224-01
 ;;9002226.02101,"1101,00378-2224-01 ",.02)
 ;;00378-2224-01
 ;;9002226.02101,"1101,00378-3475-01 ",.01)
 ;;00378-3475-01
 ;;9002226.02101,"1101,00378-3475-01 ",.02)
 ;;00378-3475-01
 ;;9002226.02101,"1101,00378-3475-30 ",.01)
 ;;00378-3475-30
 ;;9002226.02101,"1101,00378-3475-30 ",.02)
 ;;00378-3475-30
