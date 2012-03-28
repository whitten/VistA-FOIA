BGP9TXSF ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"710,1538-8 ",.01)
 ;;1538-8
 ;;9002226.02101,"710,1538-8 ",.02)
 ;;1538-8
 ;;9002226.02101,"710,1539-6 ",.01)
 ;;1539-6
 ;;9002226.02101,"710,1539-6 ",.02)
 ;;1539-6
 ;;9002226.02101,"710,1540-4 ",.01)
 ;;1540-4
 ;;9002226.02101,"710,1540-4 ",.02)
 ;;1540-4
 ;;9002226.02101,"710,1541-2 ",.01)
 ;;1541-2
 ;;9002226.02101,"710,1541-2 ",.02)
 ;;1541-2
 ;;9002226.02101,"710,1542-0 ",.01)
 ;;1542-0
 ;;9002226.02101,"710,1542-0 ",.02)
 ;;1542-0
 ;;9002226.02101,"710,1543-8 ",.01)
 ;;1543-8
 ;;9002226.02101,"710,1543-8 ",.02)
 ;;1543-8
 ;;9002226.02101,"710,1544-6 ",.01)
 ;;1544-6
 ;;9002226.02101,"710,1544-6 ",.02)
 ;;1544-6
 ;;9002226.02101,"710,1545-3 ",.01)
 ;;1545-3
 ;;9002226.02101,"710,1545-3 ",.02)
 ;;1545-3
 ;;9002226.02101,"710,1547-9 ",.01)
 ;;1547-9
 ;;9002226.02101,"710,1547-9 ",.02)
 ;;1547-9
 ;;9002226.02101,"710,1548-7 ",.01)
 ;;1548-7
 ;;9002226.02101,"710,1548-7 ",.02)
 ;;1548-7
 ;;9002226.02101,"710,1549-5 ",.01)
 ;;1549-5
 ;;9002226.02101,"710,1549-5 ",.02)
 ;;1549-5
 ;;9002226.02101,"710,1550-3 ",.01)
 ;;1550-3
 ;;9002226.02101,"710,1550-3 ",.02)
 ;;1550-3
 ;;9002226.02101,"710,1551-1 ",.01)
 ;;1551-1
 ;;9002226.02101,"710,1551-1 ",.02)
 ;;1551-1
 ;;9002226.02101,"710,1552-9 ",.01)
 ;;1552-9
 ;;9002226.02101,"710,1552-9 ",.02)
 ;;1552-9
 ;;9002226.02101,"710,1553-7 ",.01)
 ;;1553-7
 ;;9002226.02101,"710,1553-7 ",.02)
 ;;1553-7
 ;;9002226.02101,"710,1554-5 ",.01)
 ;;1554-5
 ;;9002226.02101,"710,1554-5 ",.02)
 ;;1554-5
 ;;9002226.02101,"710,1555-2 ",.01)
 ;;1555-2
 ;;9002226.02101,"710,1555-2 ",.02)
 ;;1555-2
 ;;9002226.02101,"710,1556-0 ",.01)
 ;;1556-0
 ;;9002226.02101,"710,1556-0 ",.02)
 ;;1556-0
 ;;9002226.02101,"710,1557-8 ",.01)
 ;;1557-8
 ;;9002226.02101,"710,1557-8 ",.02)
 ;;1557-8
 ;;9002226.02101,"710,1558-6 ",.01)
 ;;1558-6
 ;;9002226.02101,"710,1558-6 ",.02)
 ;;1558-6
 ;;9002226.02101,"710,16165-3 ",.01)
 ;;16165-3
 ;;9002226.02101,"710,16165-3 ",.02)
 ;;16165-3
 ;;9002226.02101,"710,16166-1 ",.01)
 ;;16166-1
 ;;9002226.02101,"710,16166-1 ",.02)
 ;;16166-1
 ;;9002226.02101,"710,16167-9 ",.01)
 ;;16167-9
 ;;9002226.02101,"710,16167-9 ",.02)
 ;;16167-9
 ;;9002226.02101,"710,16168-7 ",.01)
 ;;16168-7
 ;;9002226.02101,"710,16168-7 ",.02)
 ;;16168-7
 ;;9002226.02101,"710,16169-5 ",.01)
 ;;16169-5
 ;;9002226.02101,"710,16169-5 ",.02)
 ;;16169-5
 ;;9002226.02101,"710,16170-3 ",.01)
 ;;16170-3
 ;;9002226.02101,"710,16170-3 ",.02)
 ;;16170-3
 ;;9002226.02101,"710,16906-0 ",.01)
 ;;16906-0
 ;;9002226.02101,"710,16906-0 ",.02)
 ;;16906-0
 ;;9002226.02101,"710,16907-8 ",.01)
 ;;16907-8
 ;;9002226.02101,"710,16907-8 ",.02)
 ;;16907-8
 ;;9002226.02101,"710,16908-6 ",.01)
 ;;16908-6
 ;;9002226.02101,"710,16908-6 ",.02)
 ;;16908-6
 ;;9002226.02101,"710,16909-4 ",.01)
 ;;16909-4
 ;;9002226.02101,"710,16909-4 ",.02)
 ;;16909-4
 ;;9002226.02101,"710,16910-2 ",.01)
 ;;16910-2
 ;;9002226.02101,"710,16910-2 ",.02)
 ;;16910-2
 ;;9002226.02101,"710,16911-0 ",.01)
 ;;16911-0
 ;;9002226.02101,"710,16911-0 ",.02)
 ;;16911-0
 ;;9002226.02101,"710,16912-8 ",.01)
 ;;16912-8
 ;;9002226.02101,"710,16912-8 ",.02)
 ;;16912-8
 ;;9002226.02101,"710,16913-6 ",.01)
 ;;16913-6
 ;;9002226.02101,"710,16913-6 ",.02)
 ;;16913-6
 ;;9002226.02101,"710,16914-4 ",.01)
 ;;16914-4
 ;;9002226.02101,"710,16914-4 ",.02)
 ;;16914-4
 ;;9002226.02101,"710,16915-1 ",.01)
 ;;16915-1
 ;;9002226.02101,"710,16915-1 ",.02)
 ;;16915-1
 ;;9002226.02101,"710,17865-7 ",.01)
 ;;17865-7
 ;;9002226.02101,"710,17865-7 ",.02)
 ;;17865-7
 ;;9002226.02101,"710,18296-4 ",.01)
 ;;18296-4
 ;;9002226.02101,"710,18296-4 ",.02)
 ;;18296-4
 ;;9002226.02101,"710,18342-6 ",.01)
 ;;18342-6
 ;;9002226.02101,"710,18342-6 ",.02)
 ;;18342-6
 ;;9002226.02101,"710,18353-3 ",.01)
 ;;18353-3
 ;;9002226.02101,"710,18353-3 ",.02)
 ;;18353-3
 ;;9002226.02101,"710,18354-1 ",.01)
 ;;18354-1
 ;;9002226.02101,"710,18354-1 ",.02)
 ;;18354-1
 ;;9002226.02101,"710,19104-9 ",.01)
 ;;19104-9
 ;;9002226.02101,"710,19104-9 ",.02)
 ;;19104-9
 ;;9002226.02101,"710,19105-6 ",.01)
 ;;19105-6
 ;;9002226.02101,"710,19105-6 ",.02)
 ;;19105-6
 ;;9002226.02101,"710,20436-2 ",.01)
 ;;20436-2
 ;;9002226.02101,"710,20436-2 ",.02)
 ;;20436-2
 ;;9002226.02101,"710,20437-0 ",.01)
 ;;20437-0
 ;;9002226.02101,"710,20437-0 ",.02)
 ;;20437-0
 ;;9002226.02101,"710,20438-8 ",.01)
 ;;20438-8
 ;;9002226.02101,"710,20438-8 ",.02)
 ;;20438-8
 ;;9002226.02101,"710,20439-6 ",.01)
 ;;20439-6
 ;;9002226.02101,"710,20439-6 ",.02)
 ;;20439-6
 ;;9002226.02101,"710,20440-4 ",.01)
 ;;20440-4
 ;;9002226.02101,"710,20440-4 ",.02)
 ;;20440-4
 ;;9002226.02101,"710,20441-2 ",.01)
 ;;20441-2
 ;;9002226.02101,"710,20441-2 ",.02)
 ;;20441-2
 ;;9002226.02101,"710,21308-2 ",.01)
 ;;21308-2
 ;;9002226.02101,"710,21308-2 ",.02)
 ;;21308-2
 ;;9002226.02101,"710,21309-0 ",.01)
 ;;21309-0
 ;;9002226.02101,"710,21309-0 ",.02)
 ;;21309-0
 ;;9002226.02101,"710,21310-8 ",.01)
 ;;21310-8
 ;;9002226.02101,"710,21310-8 ",.02)
 ;;21310-8
 ;;9002226.02101,"710,2339-0 ",.01)
 ;;2339-0
 ;;9002226.02101,"710,2339-0 ",.02)
 ;;2339-0
 ;;9002226.02101,"710,2340-8 ",.01)
 ;;2340-8
