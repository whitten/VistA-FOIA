BGP12XE ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"686,00228-2089-10 ",.02)
 ;;00228-2089-10
 ;;9002226.02101,"686,00228-2091-10 ",.01)
 ;;00228-2091-10
 ;;9002226.02101,"686,00228-2091-10 ",.02)
 ;;00228-2091-10
 ;;9002226.02101,"686,00228-2092-10 ",.01)
 ;;00228-2092-10
 ;;9002226.02101,"686,00228-2092-10 ",.02)
 ;;00228-2092-10
 ;;9002226.02101,"686,00228-3016-11 ",.01)
 ;;00228-3016-11
 ;;9002226.02101,"686,00228-3016-11 ",.02)
 ;;00228-3016-11
 ;;9002226.02101,"686,00228-3016-96 ",.01)
 ;;00228-3016-96
 ;;9002226.02101,"686,00228-3016-96 ",.02)
 ;;00228-3016-96
 ;;9002226.02101,"686,00247-0625-00 ",.01)
 ;;00247-0625-00
 ;;9002226.02101,"686,00247-0625-00 ",.02)
 ;;00247-0625-00
 ;;9002226.02101,"686,00247-0625-07 ",.01)
 ;;00247-0625-07
 ;;9002226.02101,"686,00247-0625-07 ",.02)
 ;;00247-0625-07
 ;;9002226.02101,"686,00247-0625-14 ",.01)
 ;;00247-0625-14
 ;;9002226.02101,"686,00247-0625-14 ",.02)
 ;;00247-0625-14
 ;;9002226.02101,"686,00247-0625-15 ",.01)
 ;;00247-0625-15
 ;;9002226.02101,"686,00247-0625-15 ",.02)
 ;;00247-0625-15
 ;;9002226.02101,"686,00247-0625-28 ",.01)
 ;;00247-0625-28
 ;;9002226.02101,"686,00247-0625-28 ",.02)
 ;;00247-0625-28
 ;;9002226.02101,"686,00247-0625-30 ",.01)
 ;;00247-0625-30
 ;;9002226.02101,"686,00247-0625-30 ",.02)
 ;;00247-0625-30
 ;;9002226.02101,"686,00247-0625-33 ",.01)
 ;;00247-0625-33
 ;;9002226.02101,"686,00247-0625-33 ",.02)
 ;;00247-0625-33
 ;;9002226.02101,"686,00247-0948-07 ",.01)
 ;;00247-0948-07
 ;;9002226.02101,"686,00247-0948-07 ",.02)
 ;;00247-0948-07
 ;;9002226.02101,"686,00247-0948-14 ",.01)
 ;;00247-0948-14
 ;;9002226.02101,"686,00247-0948-14 ",.02)
 ;;00247-0948-14
 ;;9002226.02101,"686,00247-0948-28 ",.01)
 ;;00247-0948-28
 ;;9002226.02101,"686,00247-0948-28 ",.02)
 ;;00247-0948-28
 ;;9002226.02101,"686,00247-0948-30 ",.01)
 ;;00247-0948-30
 ;;9002226.02101,"686,00247-0948-30 ",.02)
 ;;00247-0948-30
 ;;9002226.02101,"686,00247-1315-14 ",.01)
 ;;00247-1315-14
 ;;9002226.02101,"686,00247-1315-14 ",.02)
 ;;00247-1315-14
 ;;9002226.02101,"686,00247-1316-30 ",.01)
 ;;00247-1316-30
 ;;9002226.02101,"686,00247-1316-30 ",.02)
 ;;00247-1316-30
 ;;9002226.02101,"686,00247-1355-30 ",.01)
 ;;00247-1355-30
 ;;9002226.02101,"686,00247-1355-30 ",.02)
 ;;00247-1355-30
 ;;9002226.02101,"686,00247-1492-07 ",.01)
 ;;00247-1492-07
 ;;9002226.02101,"686,00247-1492-07 ",.02)
 ;;00247-1492-07
 ;;9002226.02101,"686,00247-1492-14 ",.01)
 ;;00247-1492-14
 ;;9002226.02101,"686,00247-1492-14 ",.02)
 ;;00247-1492-14
 ;;9002226.02101,"686,00247-1492-28 ",.01)
 ;;00247-1492-28
 ;;9002226.02101,"686,00247-1492-28 ",.02)
 ;;00247-1492-28
 ;;9002226.02101,"686,00247-1492-30 ",.01)
 ;;00247-1492-30
 ;;9002226.02101,"686,00247-1492-30 ",.02)
 ;;00247-1492-30
 ;;9002226.02101,"686,00247-1493-07 ",.01)
 ;;00247-1493-07
 ;;9002226.02101,"686,00247-1493-07 ",.02)
 ;;00247-1493-07
 ;;9002226.02101,"686,00247-1493-14 ",.01)
 ;;00247-1493-14
 ;;9002226.02101,"686,00247-1493-14 ",.02)
 ;;00247-1493-14
 ;;9002226.02101,"686,00247-1493-28 ",.01)
 ;;00247-1493-28
 ;;9002226.02101,"686,00247-1493-28 ",.02)
 ;;00247-1493-28
 ;;9002226.02101,"686,00247-1493-30 ",.01)
 ;;00247-1493-30
 ;;9002226.02101,"686,00247-1493-30 ",.02)
 ;;00247-1493-30
 ;;9002226.02101,"686,00247-1494-30 ",.01)
 ;;00247-1494-30
 ;;9002226.02101,"686,00247-1494-30 ",.02)
 ;;00247-1494-30
 ;;9002226.02101,"686,00247-1495-21 ",.01)
 ;;00247-1495-21
 ;;9002226.02101,"686,00247-1495-21 ",.02)
 ;;00247-1495-21
 ;;9002226.02101,"686,00247-1495-30 ",.01)
 ;;00247-1495-30
 ;;9002226.02101,"686,00247-1495-30 ",.02)
 ;;00247-1495-30
 ;;9002226.02101,"686,00247-1717-30 ",.01)
 ;;00247-1717-30
 ;;9002226.02101,"686,00247-1717-30 ",.02)
 ;;00247-1717-30
 ;;9002226.02101,"686,00247-1718-30 ",.01)
 ;;00247-1718-30
 ;;9002226.02101,"686,00247-1718-30 ",.02)
 ;;00247-1718-30
 ;;9002226.02101,"686,00247-1719-30 ",.01)
 ;;00247-1719-30
 ;;9002226.02101,"686,00247-1719-30 ",.02)
 ;;00247-1719-30
 ;;9002226.02101,"686,00247-1719-46 ",.01)
 ;;00247-1719-46
 ;;9002226.02101,"686,00247-1719-46 ",.02)
 ;;00247-1719-46
 ;;9002226.02101,"686,00247-1896-00 ",.01)
 ;;00247-1896-00
 ;;9002226.02101,"686,00247-1896-00 ",.02)
 ;;00247-1896-00
 ;;9002226.02101,"686,00247-1896-30 ",.01)
 ;;00247-1896-30
 ;;9002226.02101,"686,00247-1896-30 ",.02)
 ;;00247-1896-30
 ;;9002226.02101,"686,00247-1896-60 ",.01)
 ;;00247-1896-60
 ;;9002226.02101,"686,00247-1896-60 ",.02)
 ;;00247-1896-60
 ;;9002226.02101,"686,00247-1896-77 ",.01)
 ;;00247-1896-77
 ;;9002226.02101,"686,00247-1896-77 ",.02)
 ;;00247-1896-77
 ;;9002226.02101,"686,00247-1896-90 ",.01)
 ;;00247-1896-90
 ;;9002226.02101,"686,00247-1896-90 ",.02)
 ;;00247-1896-90
 ;;9002226.02101,"686,00349-8834-01 ",.01)
 ;;00349-8834-01
 ;;9002226.02101,"686,00349-8834-01 ",.02)
 ;;00349-8834-01
 ;;9002226.02101,"686,00378-8115-01 ",.01)
 ;;00378-8115-01
 ;;9002226.02101,"686,00378-8115-01 ",.02)
 ;;00378-8115-01
 ;;9002226.02101,"686,00406-1121-01 ",.01)
 ;;00406-1121-01
 ;;9002226.02101,"686,00406-1121-01 ",.02)
 ;;00406-1121-01
 ;;9002226.02101,"686,00406-1121-10 ",.01)
 ;;00406-1121-10
 ;;9002226.02101,"686,00406-1121-10 ",.02)
 ;;00406-1121-10
 ;;9002226.02101,"686,00406-1122-01 ",.01)
 ;;00406-1122-01
 ;;9002226.02101,"686,00406-1122-01 ",.02)
 ;;00406-1122-01
 ;;9002226.02101,"686,00406-1122-10 ",.01)
 ;;00406-1122-10
 ;;9002226.02101,"686,00406-1122-10 ",.02)
 ;;00406-1122-10
 ;;9002226.02101,"686,00406-1124-01 ",.01)
 ;;00406-1124-01
 ;;9002226.02101,"686,00406-1124-01 ",.02)
 ;;00406-1124-01
 ;;9002226.02101,"686,00406-1124-10 ",.01)
 ;;00406-1124-10
 ;;9002226.02101,"686,00406-1124-10 ",.02)
 ;;00406-1124-10
 ;;9002226.02101,"686,00406-1423-01 ",.01)
 ;;00406-1423-01
 ;;9002226.02101,"686,00406-1423-01 ",.02)
 ;;00406-1423-01
 ;;9002226.02101,"686,00406-1451-01 ",.01)
 ;;00406-1451-01
 ;;9002226.02101,"686,00406-1451-01 ",.02)
 ;;00406-1451-01
 ;;9002226.02101,"686,00406-8884-01 ",.01)
 ;;00406-8884-01
 ;;9002226.02101,"686,00406-8884-01 ",.02)
 ;;00406-8884-01
 ;;9002226.02101,"686,00406-8885-01 ",.01)
 ;;00406-8885-01
 ;;9002226.02101,"686,00406-8885-01 ",.02)
 ;;00406-8885-01
 ;;9002226.02101,"686,00406-8886-01 ",.01)
 ;;00406-8886-01
 ;;9002226.02101,"686,00406-8886-01 ",.02)
 ;;00406-8886-01
 ;;9002226.02101,"686,00406-8891-01 ",.01)
 ;;00406-8891-01
 ;;9002226.02101,"686,00406-8891-01 ",.02)
 ;;00406-8891-01
 ;;9002226.02101,"686,00406-8892-01 ",.01)
 ;;00406-8892-01
 ;;9002226.02101,"686,00406-8892-01 ",.02)
 ;;00406-8892-01
 ;;9002226.02101,"686,00406-8893-01 ",.01)
 ;;00406-8893-01
 ;;9002226.02101,"686,00406-8893-01 ",.02)
 ;;00406-8893-01
 ;;9002226.02101,"686,00406-8894-01 ",.01)
 ;;00406-8894-01
 ;;9002226.02101,"686,00406-8894-01 ",.02)
 ;;00406-8894-01
 ;;9002226.02101,"686,00406-8958-01 ",.01)
 ;;00406-8958-01
 ;;9002226.02101,"686,00406-8958-01 ",.02)
 ;;00406-8958-01
 ;;9002226.02101,"686,00406-8959-01 ",.01)
 ;;00406-8959-01
 ;;9002226.02101,"686,00406-8959-01 ",.02)
 ;;00406-8959-01
 ;;9002226.02101,"686,00406-8960-01 ",.01)
 ;;00406-8960-01
 ;;9002226.02101,"686,00406-8960-01 ",.02)
 ;;00406-8960-01
 ;;9002226.02101,"686,00406-8961-01 ",.01)
 ;;00406-8961-01
 ;;9002226.02101,"686,00406-8961-01 ",.02)
 ;;00406-8961-01
 ;;9002226.02101,"686,00406-8962-01 ",.01)
 ;;00406-8962-01
 ;;9002226.02101,"686,00406-8962-01 ",.02)
 ;;00406-8962-01
 ;;9002226.02101,"686,00463-2043-10 ",.01)
 ;;00463-2043-10
 ;;9002226.02101,"686,00463-2043-10 ",.02)
 ;;00463-2043-10
 ;;9002226.02101,"686,00463-3029-10 ",.01)
 ;;00463-3029-10
 ;;9002226.02101,"686,00463-3029-10 ",.02)
 ;;00463-3029-10
 ;;9002226.02101,"686,00463-3036-10 ",.01)
 ;;00463-3036-10
 ;;9002226.02101,"686,00463-3036-10 ",.02)
 ;;00463-3036-10
 ;;9002226.02101,"686,00463-7500-01 ",.01)
 ;;00463-7500-01
 ;;9002226.02101,"686,00463-7500-01 ",.02)
 ;;00463-7500-01
 ;;9002226.02101,"686,00463-7500-10 ",.01)
 ;;00463-7500-10
 ;;9002226.02101,"686,00463-7500-10 ",.02)
 ;;00463-7500-10
 ;;9002226.02101,"686,00463-8000-01 ",.01)
 ;;00463-8000-01
 ;;9002226.02101,"686,00463-8000-01 ",.02)
 ;;00463-8000-01
 ;;9002226.02101,"686,00463-8000-10 ",.01)
 ;;00463-8000-10
 ;;9002226.02101,"686,00463-8000-10 ",.02)
 ;;00463-8000-10
 ;;9002226.02101,"686,00527-0597-01 ",.01)
 ;;00527-0597-01
 ;;9002226.02101,"686,00527-0597-01 ",.02)
 ;;00527-0597-01
 ;;9002226.02101,"686,00527-0597-10 ",.01)
 ;;00527-0597-10
 ;;9002226.02101,"686,00527-0597-10 ",.02)
 ;;00527-0597-10
 ;;9002226.02101,"686,00527-1143-01 ",.01)
 ;;00527-1143-01
 ;;9002226.02101,"686,00527-1143-01 ",.02)
 ;;00527-1143-01
 ;;9002226.02101,"686,00527-1219-01 ",.01)
 ;;00527-1219-01
 ;;9002226.02101,"686,00527-1219-01 ",.02)
 ;;00527-1219-01
 ;;9002226.02101,"686,00527-1308-01 ",.01)
 ;;00527-1308-01
 ;;9002226.02101,"686,00527-1308-01 ",.02)
 ;;00527-1308-01
 ;;9002226.02101,"686,00527-1308-10 ",.01)
 ;;00527-1308-10
 ;;9002226.02101,"686,00527-1308-10 ",.02)
 ;;00527-1308-10
 ;;9002226.02101,"686,00527-1310-01 ",.01)
 ;;00527-1310-01
 ;;9002226.02101,"686,00527-1310-01 ",.02)
 ;;00527-1310-01
 ;;9002226.02101,"686,00527-1310-10 ",.01)
 ;;00527-1310-10
 ;;9002226.02101,"686,00527-1310-10 ",.02)
 ;;00527-1310-10
 ;;9002226.02101,"686,00527-1317-01 ",.01)
 ;;00527-1317-01
 ;;9002226.02101,"686,00527-1317-01 ",.02)
 ;;00527-1317-01
 ;;9002226.02101,"686,00527-1317-10 ",.01)
 ;;00527-1317-10
 ;;9002226.02101,"686,00527-1317-10 ",.02)
 ;;00527-1317-10
 ;;9002226.02101,"686,00527-1445-01 ",.01)
 ;;00527-1445-01
 ;;9002226.02101,"686,00527-1445-01 ",.02)
 ;;00527-1445-01
 ;;9002226.02101,"686,00527-1445-10 ",.01)
 ;;00527-1445-10
 ;;9002226.02101,"686,00527-1445-10 ",.02)
 ;;00527-1445-10
 ;;9002226.02101,"686,00536-5925-01 ",.01)
 ;;00536-5925-01
 ;;9002226.02101,"686,00536-5925-01 ",.02)
 ;;00536-5925-01
 ;;9002226.02101,"686,00555-0762-02 ",.01)
 ;;00555-0762-02
 ;;9002226.02101,"686,00555-0762-02 ",.02)
 ;;00555-0762-02
 ;;9002226.02101,"686,00555-0763-02 ",.01)
 ;;00555-0763-02
 ;;9002226.02101,"686,00555-0763-02 ",.02)
 ;;00555-0763-02
 ;;9002226.02101,"686,00555-0764-02 ",.01)
 ;;00555-0764-02
 ;;9002226.02101,"686,00555-0764-02 ",.02)
 ;;00555-0764-02
 ;;9002226.02101,"686,00555-0765-02 ",.01)
 ;;00555-0765-02
 ;;9002226.02101,"686,00555-0765-02 ",.02)
 ;;00555-0765-02
 ;;9002226.02101,"686,00555-0766-02 ",.01)
 ;;00555-0766-02
 ;;9002226.02101,"686,00555-0766-02 ",.02)
 ;;00555-0766-02
 ;;9002226.02101,"686,00555-0767-02 ",.01)
 ;;00555-0767-02
 ;;9002226.02101,"686,00555-0767-02 ",.02)
 ;;00555-0767-02
 ;;9002226.02101,"686,00555-0768-02 ",.01)
 ;;00555-0768-02
 ;;9002226.02101,"686,00555-0768-02 ",.02)
 ;;00555-0768-02
 ;;9002226.02101,"686,00555-0775-02 ",.01)
 ;;00555-0775-02
 ;;9002226.02101,"686,00555-0775-02 ",.02)
 ;;00555-0775-02
 ;;9002226.02101,"686,00555-0776-02 ",.01)
 ;;00555-0776-02
 ;;9002226.02101,"686,00555-0776-02 ",.02)
 ;;00555-0776-02
 ;;9002226.02101,"686,00555-0777-02 ",.01)
 ;;00555-0777-02
 ;;9002226.02101,"686,00555-0777-02 ",.02)
 ;;00555-0777-02
 ;;9002226.02101,"686,00555-0787-02 ",.01)
 ;;00555-0787-02
 ;;9002226.02101,"686,00555-0787-02 ",.02)
 ;;00555-0787-02
 ;;9002226.02101,"686,00555-0788-02 ",.01)
 ;;00555-0788-02
 ;;9002226.02101,"686,00555-0788-02 ",.02)
 ;;00555-0788-02
 ;;9002226.02101,"686,00555-0789-02 ",.01)
 ;;00555-0789-02
 ;;9002226.02101,"686,00555-0789-02 ",.02)
 ;;00555-0789-02
 ;;9002226.02101,"686,00555-0790-02 ",.01)
 ;;00555-0790-02
 ;;9002226.02101,"686,00555-0790-02 ",.02)
 ;;00555-0790-02
 ;;9002226.02101,"686,00555-0791-02 ",.01)
 ;;00555-0791-02
 ;;9002226.02101,"686,00555-0791-02 ",.02)
 ;;00555-0791-02
 ;;9002226.02101,"686,00555-0792-02 ",.01)
 ;;00555-0792-02
 ;;9002226.02101,"686,00555-0792-02 ",.02)
 ;;00555-0792-02
 ;;9002226.02101,"686,00555-0952-02 ",.01)
 ;;00555-0952-02
 ;;9002226.02101,"686,00555-0952-02 ",.02)
 ;;00555-0952-02
 ;;9002226.02101,"686,00555-0953-02 ",.01)
 ;;00555-0953-02
 ;;9002226.02101,"686,00555-0953-02 ",.02)
 ;;00555-0953-02
 ;;9002226.02101,"686,00555-0954-02 ",.01)
 ;;00555-0954-02
 ;;9002226.02101,"686,00555-0954-02 ",.02)
 ;;00555-0954-02
 ;;9002226.02101,"686,00555-0955-02 ",.01)
 ;;00555-0955-02
 ;;9002226.02101,"686,00555-0955-02 ",.02)
 ;;00555-0955-02
 ;;9002226.02101,"686,00555-0956-02 ",.01)
 ;;00555-0956-02
 ;;9002226.02101,"686,00555-0956-02 ",.02)
 ;;00555-0956-02
 ;;9002226.02101,"686,00555-0971-02 ",.01)
 ;;00555-0971-02
 ;;9002226.02101,"686,00555-0971-02 ",.02)
 ;;00555-0971-02
 ;;9002226.02101,"686,00555-0972-02 ",.01)
 ;;00555-0972-02
 ;;9002226.02101,"686,00555-0972-02 ",.02)
 ;;00555-0972-02
