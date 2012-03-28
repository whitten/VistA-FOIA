BGP8EXPB ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 28, 2007 ;
 ;;8.0;IHS CLINICAL REPORTING;;MAR 12, 2008
 ;;;BGP6;;SEP 28, 2007
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226,549,.15)
 ;;95.3
 ;;9002226,549,.16)
 ;;@
 ;;9002226,549,.17)
 ;;@
 ;;9002226,549,3101)
 ;;@
 ;;9002226.02101,"549,10449-7 ",.01)
 ;;10449-7
 ;;9002226.02101,"549,10449-7 ",.02)
 ;;10449-7
 ;;9002226.02101,"549,10450-5 ",.01)
 ;;10450-5
 ;;9002226.02101,"549,10450-5 ",.02)
 ;;10450-5
 ;;9002226.02101,"549,10832-4 ",.01)
 ;;10832-4
 ;;9002226.02101,"549,10832-4 ",.02)
 ;;10832-4
 ;;9002226.02101,"549,10968-6 ",.01)
 ;;10968-6
 ;;9002226.02101,"549,10968-6 ",.02)
 ;;10968-6
 ;;9002226.02101,"549,11032-0 ",.01)
 ;;11032-0
 ;;9002226.02101,"549,11032-0 ",.02)
 ;;11032-0
 ;;9002226.02101,"549,11142-7 ",.01)
 ;;11142-7
 ;;9002226.02101,"549,11142-7 ",.02)
 ;;11142-7
 ;;9002226.02101,"549,11143-5 ",.01)
 ;;11143-5
 ;;9002226.02101,"549,11143-5 ",.02)
 ;;11143-5
 ;;9002226.02101,"549,12219-2 ",.01)
 ;;12219-2
 ;;9002226.02101,"549,12219-2 ",.02)
 ;;12219-2
 ;;9002226.02101,"549,12220-0 ",.01)
 ;;12220-0
 ;;9002226.02101,"549,12220-0 ",.02)
 ;;12220-0
 ;;9002226.02101,"549,12607-8 ",.01)
 ;;12607-8
 ;;9002226.02101,"549,12607-8 ",.02)
 ;;12607-8
 ;;9002226.02101,"549,12610-2 ",.01)
 ;;12610-2
 ;;9002226.02101,"549,12610-2 ",.02)
 ;;12610-2
 ;;9002226.02101,"549,12611-0 ",.01)
 ;;12611-0
 ;;9002226.02101,"549,12611-0 ",.02)
 ;;12611-0
 ;;9002226.02101,"549,12613-6 ",.01)
 ;;12613-6
 ;;9002226.02101,"549,12613-6 ",.02)
 ;;12613-6
 ;;9002226.02101,"549,12614-4 ",.01)
 ;;12614-4
 ;;9002226.02101,"549,12614-4 ",.02)
 ;;12614-4
 ;;9002226.02101,"549,12615-1 ",.01)
 ;;12615-1
 ;;9002226.02101,"549,12615-1 ",.02)
 ;;12615-1
 ;;9002226.02101,"549,12616-9 ",.01)
 ;;12616-9
 ;;9002226.02101,"549,12616-9 ",.02)
 ;;12616-9
 ;;9002226.02101,"549,12617-7 ",.01)
 ;;12617-7
 ;;9002226.02101,"549,12617-7 ",.02)
 ;;12617-7
 ;;9002226.02101,"549,12618-5 ",.01)
 ;;12618-5
 ;;9002226.02101,"549,12618-5 ",.02)
 ;;12618-5
 ;;9002226.02101,"549,12619-3 ",.01)
 ;;12619-3
 ;;9002226.02101,"549,12619-3 ",.02)
 ;;12619-3
 ;;9002226.02101,"549,12620-1 ",.01)
 ;;12620-1
 ;;9002226.02101,"549,12620-1 ",.02)
 ;;12620-1
 ;;9002226.02101,"549,12621-9 ",.01)
 ;;12621-9
 ;;9002226.02101,"549,12621-9 ",.02)
 ;;12621-9
 ;;9002226.02101,"549,12622-7 ",.01)
 ;;12622-7
 ;;9002226.02101,"549,12622-7 ",.02)
 ;;12622-7
 ;;9002226.02101,"549,12623-5 ",.01)
 ;;12623-5
 ;;9002226.02101,"549,12623-5 ",.02)
 ;;12623-5
 ;;9002226.02101,"549,12624-3 ",.01)
 ;;12624-3
 ;;9002226.02101,"549,12624-3 ",.02)
 ;;12624-3
 ;;9002226.02101,"549,12625-0 ",.01)
 ;;12625-0
 ;;9002226.02101,"549,12625-0 ",.02)
 ;;12625-0
 ;;9002226.02101,"549,12626-8 ",.01)
 ;;12626-8
 ;;9002226.02101,"549,12626-8 ",.02)
 ;;12626-8
 ;;9002226.02101,"549,12627-6 ",.01)
 ;;12627-6
 ;;9002226.02101,"549,12627-6 ",.02)
 ;;12627-6
 ;;9002226.02101,"549,12631-8 ",.01)
 ;;12631-8
 ;;9002226.02101,"549,12631-8 ",.02)
 ;;12631-8
 ;;9002226.02101,"549,12632-6 ",.01)
 ;;12632-6
 ;;9002226.02101,"549,12632-6 ",.02)
 ;;12632-6
 ;;9002226.02101,"549,12635-9 ",.01)
 ;;12635-9
 ;;9002226.02101,"549,12635-9 ",.02)
 ;;12635-9
 ;;9002226.02101,"549,12636-7 ",.01)
 ;;12636-7
 ;;9002226.02101,"549,12636-7 ",.02)
 ;;12636-7
 ;;9002226.02101,"549,12637-5 ",.01)
 ;;12637-5
 ;;9002226.02101,"549,12637-5 ",.02)
 ;;12637-5
 ;;9002226.02101,"549,12638-3 ",.01)
 ;;12638-3
 ;;9002226.02101,"549,12638-3 ",.02)
 ;;12638-3
 ;;9002226.02101,"549,12639-1 ",.01)
 ;;12639-1
 ;;9002226.02101,"549,12639-1 ",.02)
 ;;12639-1
 ;;9002226.02101,"549,12640-9 ",.01)
 ;;12640-9
 ;;9002226.02101,"549,12640-9 ",.02)
 ;;12640-9
 ;;9002226.02101,"549,12641-7 ",.01)
 ;;12641-7
 ;;9002226.02101,"549,12641-7 ",.02)
 ;;12641-7
 ;;9002226.02101,"549,12642-5 ",.01)
 ;;12642-5
 ;;9002226.02101,"549,12642-5 ",.02)
 ;;12642-5
 ;;9002226.02101,"549,12643-3 ",.01)
 ;;12643-3
 ;;9002226.02101,"549,12643-3 ",.02)
 ;;12643-3
 ;;9002226.02101,"549,12644-1 ",.01)
 ;;12644-1
 ;;9002226.02101,"549,12644-1 ",.02)
 ;;12644-1
 ;;9002226.02101,"549,12645-8 ",.01)
 ;;12645-8
 ;;9002226.02101,"549,12645-8 ",.02)
 ;;12645-8
 ;;9002226.02101,"549,12646-6 ",.01)
 ;;12646-6
 ;;9002226.02101,"549,12646-6 ",.02)
 ;;12646-6
 ;;9002226.02101,"549,12647-4 ",.01)
 ;;12647-4
 ;;9002226.02101,"549,12647-4 ",.02)
 ;;12647-4
 ;;9002226.02101,"549,12648-2 ",.01)
 ;;12648-2
 ;;9002226.02101,"549,12648-2 ",.02)
 ;;12648-2
 ;;9002226.02101,"549,12649-0 ",.01)
 ;;12649-0
 ;;9002226.02101,"549,12649-0 ",.02)
 ;;12649-0
 ;;9002226.02101,"549,12650-8 ",.01)
 ;;12650-8
 ;;9002226.02101,"549,12650-8 ",.02)
 ;;12650-8
 ;;9002226.02101,"549,12651-6 ",.01)
 ;;12651-6
 ;;9002226.02101,"549,12651-6 ",.02)
 ;;12651-6
 ;;9002226.02101,"549,12652-4 ",.01)
 ;;12652-4
 ;;9002226.02101,"549,12652-4 ",.02)
 ;;12652-4
 ;;9002226.02101,"549,12653-2 ",.01)
 ;;12653-2
 ;;9002226.02101,"549,12653-2 ",.02)
 ;;12653-2
 ;;9002226.02101,"549,12654-0 ",.01)
 ;;12654-0
 ;;9002226.02101,"549,12654-0 ",.02)
 ;;12654-0
 ;;9002226.02101,"549,12655-7 ",.01)
 ;;12655-7
 ;;9002226.02101,"549,12655-7 ",.02)
 ;;12655-7
 ;;9002226.02101,"549,12656-5 ",.01)
 ;;12656-5
 ;;9002226.02101,"549,12656-5 ",.02)
 ;;12656-5
 ;;9002226.02101,"549,12657-3 ",.01)
 ;;12657-3
 ;;9002226.02101,"549,12657-3 ",.02)
 ;;12657-3
 ;;9002226.02101,"549,12658-1 ",.01)
 ;;12658-1
 ;;9002226.02101,"549,12658-1 ",.02)
 ;;12658-1
 ;;9002226.02101,"549,12659-9 ",.01)
 ;;12659-9
 ;;9002226.02101,"549,12659-9 ",.02)
 ;;12659-9
 ;;9002226.02101,"549,13453-6 ",.01)
 ;;13453-6
 ;;9002226.02101,"549,13453-6 ",.02)
 ;;13453-6
 ;;9002226.02101,"549,13606-9 ",.01)
 ;;13606-9
 ;;9002226.02101,"549,13606-9 ",.02)
 ;;13606-9
 ;;9002226.02101,"549,13607-7 ",.01)
 ;;13607-7
 ;;9002226.02101,"549,13607-7 ",.02)
 ;;13607-7
 ;;9002226.02101,"549,13865-1 ",.01)
 ;;13865-1
 ;;9002226.02101,"549,13865-1 ",.02)
 ;;13865-1
 ;;9002226.02101,"549,13866-9 ",.01)
 ;;13866-9
 ;;9002226.02101,"549,13866-9 ",.02)
 ;;13866-9
 ;;9002226.02101,"549,14137-4 ",.01)
 ;;14137-4
 ;;9002226.02101,"549,14137-4 ",.02)
 ;;14137-4
 ;;9002226.02101,"549,14743-9 ",.01)
 ;;14743-9
 ;;9002226.02101,"549,14743-9 ",.02)
 ;;14743-9
 ;;9002226.02101,"549,14749-6 ",.01)
 ;;14749-6
 ;;9002226.02101,"549,14749-6 ",.02)
 ;;14749-6
 ;;9002226.02101,"549,14751-2 ",.01)
 ;;14751-2
 ;;9002226.02101,"549,14751-2 ",.02)
 ;;14751-2
 ;;9002226.02101,"549,14752-0 ",.01)
 ;;14752-0
 ;;9002226.02101,"549,14752-0 ",.02)
 ;;14752-0
 ;;9002226.02101,"549,14753-8 ",.01)
 ;;14753-8
 ;;9002226.02101,"549,14753-8 ",.02)
 ;;14753-8
 ;;9002226.02101,"549,14754-6 ",.01)
 ;;14754-6
 ;;9002226.02101,"549,14754-6 ",.02)
 ;;14754-6
 ;;9002226.02101,"549,14755-3 ",.01)
 ;;14755-3
 ;;9002226.02101,"549,14755-3 ",.02)
 ;;14755-3
 ;;9002226.02101,"549,14756-1 ",.01)
 ;;14756-1
 ;;9002226.02101,"549,14756-1 ",.02)
 ;;14756-1
 ;;9002226.02101,"549,14757-9 ",.01)
 ;;14757-9
 ;;9002226.02101,"549,14757-9 ",.02)
 ;;14757-9
 ;;9002226.02101,"549,14758-7 ",.01)
 ;;14758-7
 ;;9002226.02101,"549,14758-7 ",.02)
 ;;14758-7
 ;;9002226.02101,"549,14759-5 ",.01)
 ;;14759-5
 ;;9002226.02101,"549,14759-5 ",.02)
 ;;14759-5
 ;;9002226.02101,"549,14760-3 ",.01)
 ;;14760-3
 ;;9002226.02101,"549,14760-3 ",.02)
 ;;14760-3
 ;;9002226.02101,"549,14761-1 ",.01)
 ;;14761-1
 ;;9002226.02101,"549,14761-1 ",.02)
 ;;14761-1
 ;;9002226.02101,"549,14762-9 ",.01)
 ;;14762-9
 ;;9002226.02101,"549,14762-9 ",.02)
 ;;14762-9
 ;;9002226.02101,"549,14763-7 ",.01)
 ;;14763-7
 ;;9002226.02101,"549,14763-7 ",.02)
 ;;14763-7
 ;;9002226.02101,"549,14764-5 ",.01)
 ;;14764-5
 ;;9002226.02101,"549,14764-5 ",.02)
 ;;14764-5
 ;;9002226.02101,"549,14765-2 ",.01)
 ;;14765-2
 ;;9002226.02101,"549,14765-2 ",.02)
 ;;14765-2
 ;;9002226.02101,"549,14766-0 ",.01)
 ;;14766-0
 ;;9002226.02101,"549,14766-0 ",.02)
 ;;14766-0
 ;;9002226.02101,"549,14767-8 ",.01)
 ;;14767-8
 ;;9002226.02101,"549,14767-8 ",.02)
 ;;14767-8
 ;;9002226.02101,"549,14768-6 ",.01)
 ;;14768-6
 ;;9002226.02101,"549,14768-6 ",.02)
 ;;14768-6
 ;;9002226.02101,"549,14769-4 ",.01)
 ;;14769-4
 ;;9002226.02101,"549,14769-4 ",.02)
 ;;14769-4
 ;;9002226.02101,"549,14770-2 ",.01)
 ;;14770-2
 ;;9002226.02101,"549,14770-2 ",.02)
 ;;14770-2
 ;;9002226.02101,"549,14771-0 ",.01)
 ;;14771-0
 ;;9002226.02101,"549,14771-0 ",.02)
 ;;14771-0
 ;;9002226.02101,"549,1491-0 ",.01)
 ;;1491-0
 ;;9002226.02101,"549,1491-0 ",.02)
 ;;1491-0
 ;;9002226.02101,"549,1492-8 ",.01)
 ;;1492-8
 ;;9002226.02101,"549,1492-8 ",.02)
 ;;1492-8
 ;;9002226.02101,"549,1493-6 ",.01)
 ;;1493-6
 ;;9002226.02101,"549,1493-6 ",.02)
 ;;1493-6
 ;;9002226.02101,"549,1494-4 ",.01)
 ;;1494-4
 ;;9002226.02101,"549,1494-4 ",.02)
 ;;1494-4
 ;;9002226.02101,"549,1496-9 ",.01)
 ;;1496-9
 ;;9002226.02101,"549,1496-9 ",.02)
 ;;1496-9
 ;;9002226.02101,"549,1497-7 ",.01)
 ;;1497-7
 ;;9002226.02101,"549,1497-7 ",.02)
 ;;1497-7
 ;;9002226.02101,"549,1498-5 ",.01)
 ;;1498-5
 ;;9002226.02101,"549,1498-5 ",.02)
 ;;1498-5
 ;;9002226.02101,"549,1499-3 ",.01)
 ;;1499-3
 ;;9002226.02101,"549,1499-3 ",.02)
 ;;1499-3
 ;;9002226.02101,"549,14995-5 ",.01)
 ;;14995-5
 ;;9002226.02101,"549,14995-5 ",.02)
 ;;14995-5
 ;;9002226.02101,"549,14996-3 ",.01)
 ;;14996-3
 ;;9002226.02101,"549,14996-3 ",.02)
 ;;14996-3
 ;;9002226.02101,"549,1500-8 ",.01)
 ;;1500-8
 ;;9002226.02101,"549,1500-8 ",.02)
 ;;1500-8
 ;;9002226.02101,"549,1501-6 ",.01)
 ;;1501-6
 ;;9002226.02101,"549,1501-6 ",.02)
 ;;1501-6
 ;;9002226.02101,"549,1502-4 ",.01)
 ;;1502-4
 ;;9002226.02101,"549,1502-4 ",.02)
 ;;1502-4
 ;;9002226.02101,"549,1504-0 ",.01)
 ;;1504-0
 ;;9002226.02101,"549,1504-0 ",.02)
 ;;1504-0
 ;;9002226.02101,"549,1506-5 ",.01)
 ;;1506-5
 ;;9002226.02101,"549,1506-5 ",.02)
 ;;1506-5
 ;;9002226.02101,"549,1507-3 ",.01)
 ;;1507-3
 ;;9002226.02101,"549,1507-3 ",.02)
 ;;1507-3
 ;;9002226.02101,"549,15074-8 ",.01)
 ;;15074-8
 ;;9002226.02101,"549,15074-8 ",.02)
 ;;15074-8
 ;;9002226.02101,"549,1508-1 ",.01)
 ;;1508-1
 ;;9002226.02101,"549,1508-1 ",.02)
 ;;1508-1
 ;;9002226.02101,"549,1510-7 ",.01)
 ;;1510-7
 ;;9002226.02101,"549,1510-7 ",.02)
 ;;1510-7
 ;;9002226.02101,"549,1511-5 ",.01)
 ;;1511-5
 ;;9002226.02101,"549,1511-5 ",.02)
 ;;1511-5
 ;;9002226.02101,"549,1512-3 ",.01)
 ;;1512-3
 ;;9002226.02101,"549,1512-3 ",.02)
 ;;1512-3
 ;;9002226.02101,"549,1513-1 ",.01)
 ;;1513-1
 ;;9002226.02101,"549,1513-1 ",.02)
 ;;1513-1
 ;;9002226.02101,"549,1514-9 ",.01)
 ;;1514-9
 ;;9002226.02101,"549,1514-9 ",.02)
 ;;1514-9
 ;;9002226.02101,"549,1515-6 ",.01)
 ;;1515-6
 ;;9002226.02101,"549,1515-6 ",.02)
 ;;1515-6
 ;;9002226.02101,"549,1517-2 ",.01)
 ;;1517-2
 ;;9002226.02101,"549,1517-2 ",.02)
 ;;1517-2
 ;;9002226.02101,"549,1518-0 ",.01)
 ;;1518-0
 ;;9002226.02101,"549,1518-0 ",.02)
 ;;1518-0
 ;;9002226.02101,"549,1519-8 ",.01)
 ;;1519-8
 ;;9002226.02101,"549,1519-8 ",.02)
 ;;1519-8
 ;;9002226.02101,"549,1521-4 ",.01)
 ;;1521-4
 ;;9002226.02101,"549,1521-4 ",.02)
 ;;1521-4
 ;;9002226.02101,"549,1522-2 ",.01)
 ;;1522-2
 ;;9002226.02101,"549,1522-2 ",.02)
 ;;1522-2
 ;;9002226.02101,"549,1523-0 ",.01)
 ;;1523-0
 ;;9002226.02101,"549,1523-0 ",.02)
 ;;1523-0
 ;;9002226.02101,"549,1524-8 ",.01)
 ;;1524-8
 ;;9002226.02101,"549,1524-8 ",.02)
 ;;1524-8
 ;;9002226.02101,"549,1525-5 ",.01)
 ;;1525-5
 ;;9002226.02101,"549,1525-5 ",.02)
 ;;1525-5
 ;;9002226.02101,"549,1526-3 ",.01)
 ;;1526-3
 ;;9002226.02101,"549,1526-3 ",.02)
 ;;1526-3
 ;;9002226.02101,"549,1527-1 ",.01)
 ;;1527-1
 ;;9002226.02101,"549,1527-1 ",.02)
 ;;1527-1
 ;;9002226.02101,"549,1528-9 ",.01)
 ;;1528-9
 ;;9002226.02101,"549,1528-9 ",.02)
 ;;1528-9
 ;;9002226.02101,"549,1529-7 ",.01)
 ;;1529-7
 ;;9002226.02101,"549,1529-7 ",.02)
 ;;1529-7
 ;;9002226.02101,"549,1530-5 ",.01)
 ;;1530-5
 ;;9002226.02101,"549,1530-5 ",.02)
 ;;1530-5
 ;;9002226.02101,"549,1531-3 ",.01)
 ;;1531-3
 ;;9002226.02101,"549,1531-3 ",.02)
 ;;1531-3
 ;;9002226.02101,"549,1532-1 ",.01)
 ;;1532-1
 ;;9002226.02101,"549,1532-1 ",.02)
 ;;1532-1
 ;;9002226.02101,"549,1533-9 ",.01)
 ;;1533-9
 ;;9002226.02101,"549,1533-9 ",.02)
 ;;1533-9
 ;;9002226.02101,"549,1534-7 ",.01)
 ;;1534-7
 ;;9002226.02101,"549,1534-7 ",.02)
 ;;1534-7
 ;;9002226.02101,"549,1535-4 ",.01)
 ;;1535-4
 ;;9002226.02101,"549,1535-4 ",.02)
 ;;1535-4
 ;;9002226.02101,"549,1536-2 ",.01)
 ;;1536-2
 ;;9002226.02101,"549,1536-2 ",.02)
 ;;1536-2
 ;;9002226.02101,"549,1537-0 ",.01)
 ;;1537-0
 ;;9002226.02101,"549,1537-0 ",.02)
 ;;1537-0
 ;;9002226.02101,"549,1538-8 ",.01)
 ;;1538-8
 ;;9002226.02101,"549,1538-8 ",.02)
 ;;1538-8
 ;;9002226.02101,"549,1539-6 ",.01)
 ;;1539-6
 ;;9002226.02101,"549,1539-6 ",.02)
 ;;1539-6
 ;;9002226.02101,"549,1540-4 ",.01)
 ;;1540-4
 ;;9002226.02101,"549,1540-4 ",.02)
 ;;1540-4
 ;;9002226.02101,"549,1541-2 ",.01)
 ;;1541-2
 ;;9002226.02101,"549,1541-2 ",.02)
 ;;1541-2
 ;;9002226.02101,"549,1542-0 ",.01)
 ;;1542-0
 ;;9002226.02101,"549,1542-0 ",.02)
 ;;1542-0
 ;;9002226.02101,"549,1543-8 ",.01)
 ;;1543-8
 ;;9002226.02101,"549,1543-8 ",.02)
 ;;1543-8
 ;;9002226.02101,"549,1544-6 ",.01)
 ;;1544-6
 ;;9002226.02101,"549,1544-6 ",.02)
 ;;1544-6
 ;;9002226.02101,"549,1545-3 ",.01)
 ;;1545-3
 ;;9002226.02101,"549,1545-3 ",.02)
 ;;1545-3
 ;;9002226.02101,"549,1547-9 ",.01)
 ;;1547-9
 ;;9002226.02101,"549,1547-9 ",.02)
 ;;1547-9
 ;;9002226.02101,"549,1548-7 ",.01)
 ;;1548-7
