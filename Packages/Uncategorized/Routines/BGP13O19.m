BGP13O19 ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"998,60760-0418-30 ",.01)
 ;;60760-0418-30
 ;;9002226.02101,"998,60760-0418-30 ",.02)
 ;;60760-0418-30
 ;;9002226.02101,"998,60760-0418-60 ",.01)
 ;;60760-0418-60
 ;;9002226.02101,"998,60760-0418-60 ",.02)
 ;;60760-0418-60
 ;;9002226.02101,"998,60760-0810-30 ",.01)
 ;;60760-0810-30
 ;;9002226.02101,"998,60760-0810-30 ",.02)
 ;;60760-0810-30
 ;;9002226.02101,"998,60793-0136-01 ",.01)
 ;;60793-0136-01
 ;;9002226.02101,"998,60793-0136-01 ",.02)
 ;;60793-0136-01
 ;;9002226.02101,"998,60793-0136-05 ",.01)
 ;;60793-0136-05
 ;;9002226.02101,"998,60793-0136-05 ",.02)
 ;;60793-0136-05
 ;;9002226.02101,"998,60951-0767-70 ",.01)
 ;;60951-0767-70
 ;;9002226.02101,"998,60951-0767-70 ",.02)
 ;;60951-0767-70
 ;;9002226.02101,"998,60951-0767-90 ",.01)
 ;;60951-0767-90
 ;;9002226.02101,"998,60951-0767-90 ",.02)
 ;;60951-0767-90
 ;;9002226.02101,"998,61392-0711-30 ",.01)
 ;;61392-0711-30
 ;;9002226.02101,"998,61392-0711-30 ",.02)
 ;;61392-0711-30
 ;;9002226.02101,"998,61392-0711-31 ",.01)
 ;;61392-0711-31
 ;;9002226.02101,"998,61392-0711-31 ",.02)
 ;;61392-0711-31
 ;;9002226.02101,"998,61392-0711-32 ",.01)
 ;;61392-0711-32
 ;;9002226.02101,"998,61392-0711-32 ",.02)
 ;;61392-0711-32
 ;;9002226.02101,"998,61392-0711-39 ",.01)
 ;;61392-0711-39
 ;;9002226.02101,"998,61392-0711-39 ",.02)
 ;;61392-0711-39
 ;;9002226.02101,"998,61392-0711-45 ",.01)
 ;;61392-0711-45
 ;;9002226.02101,"998,61392-0711-45 ",.02)
 ;;61392-0711-45
 ;;9002226.02101,"998,61392-0711-51 ",.01)
 ;;61392-0711-51
 ;;9002226.02101,"998,61392-0711-51 ",.02)
 ;;61392-0711-51
 ;;9002226.02101,"998,61392-0711-54 ",.01)
 ;;61392-0711-54
 ;;9002226.02101,"998,61392-0711-54 ",.02)
 ;;61392-0711-54
 ;;9002226.02101,"998,61392-0711-60 ",.01)
 ;;61392-0711-60
 ;;9002226.02101,"998,61392-0711-60 ",.02)
 ;;61392-0711-60
 ;;9002226.02101,"998,61392-0711-90 ",.01)
 ;;61392-0711-90
 ;;9002226.02101,"998,61392-0711-90 ",.02)
 ;;61392-0711-90
 ;;9002226.02101,"998,61392-0711-91 ",.01)
 ;;61392-0711-91
 ;;9002226.02101,"998,61392-0711-91 ",.02)
 ;;61392-0711-91
 ;;9002226.02101,"998,61392-0716-30 ",.01)
 ;;61392-0716-30
 ;;9002226.02101,"998,61392-0716-30 ",.02)
 ;;61392-0716-30
 ;;9002226.02101,"998,61392-0716-31 ",.01)
 ;;61392-0716-31
 ;;9002226.02101,"998,61392-0716-31 ",.02)
 ;;61392-0716-31
 ;;9002226.02101,"998,61392-0716-32 ",.01)
 ;;61392-0716-32
 ;;9002226.02101,"998,61392-0716-32 ",.02)
 ;;61392-0716-32
 ;;9002226.02101,"998,61392-0716-39 ",.01)
 ;;61392-0716-39
 ;;9002226.02101,"998,61392-0716-39 ",.02)
 ;;61392-0716-39
 ;;9002226.02101,"998,61392-0716-45 ",.01)
 ;;61392-0716-45
 ;;9002226.02101,"998,61392-0716-45 ",.02)
 ;;61392-0716-45
 ;;9002226.02101,"998,61392-0716-51 ",.01)
 ;;61392-0716-51
 ;;9002226.02101,"998,61392-0716-51 ",.02)
 ;;61392-0716-51
 ;;9002226.02101,"998,61392-0716-54 ",.01)
 ;;61392-0716-54
 ;;9002226.02101,"998,61392-0716-54 ",.02)
 ;;61392-0716-54
 ;;9002226.02101,"998,61392-0716-60 ",.01)
 ;;61392-0716-60
 ;;9002226.02101,"998,61392-0716-60 ",.02)
 ;;61392-0716-60
 ;;9002226.02101,"998,61392-0716-90 ",.01)
 ;;61392-0716-90
 ;;9002226.02101,"998,61392-0716-90 ",.02)
 ;;61392-0716-90
 ;;9002226.02101,"998,61392-0716-91 ",.01)
 ;;61392-0716-91
 ;;9002226.02101,"998,61392-0716-91 ",.02)
 ;;61392-0716-91
 ;;9002226.02101,"998,61392-0739-30 ",.01)
 ;;61392-0739-30
 ;;9002226.02101,"998,61392-0739-30 ",.02)
 ;;61392-0739-30
 ;;9002226.02101,"998,61392-0739-31 ",.01)
 ;;61392-0739-31
 ;;9002226.02101,"998,61392-0739-31 ",.02)
 ;;61392-0739-31
 ;;9002226.02101,"998,61392-0739-32 ",.01)
 ;;61392-0739-32
 ;;9002226.02101,"998,61392-0739-32 ",.02)
 ;;61392-0739-32
 ;;9002226.02101,"998,61392-0739-39 ",.01)
 ;;61392-0739-39
 ;;9002226.02101,"998,61392-0739-39 ",.02)
 ;;61392-0739-39
 ;;9002226.02101,"998,61392-0739-45 ",.01)
 ;;61392-0739-45
 ;;9002226.02101,"998,61392-0739-45 ",.02)
 ;;61392-0739-45
 ;;9002226.02101,"998,61392-0739-51 ",.01)
 ;;61392-0739-51
 ;;9002226.02101,"998,61392-0739-51 ",.02)
 ;;61392-0739-51
 ;;9002226.02101,"998,61392-0739-54 ",.01)
 ;;61392-0739-54
 ;;9002226.02101,"998,61392-0739-54 ",.02)
 ;;61392-0739-54
 ;;9002226.02101,"998,61392-0739-60 ",.01)
 ;;61392-0739-60
 ;;9002226.02101,"998,61392-0739-60 ",.02)
 ;;61392-0739-60
 ;;9002226.02101,"998,61392-0739-90 ",.01)
 ;;61392-0739-90
 ;;9002226.02101,"998,61392-0739-90 ",.02)
 ;;61392-0739-90
 ;;9002226.02101,"998,61392-0739-91 ",.01)
 ;;61392-0739-91
 ;;9002226.02101,"998,61392-0739-91 ",.02)
 ;;61392-0739-91
 ;;9002226.02101,"998,61392-0740-30 ",.01)
 ;;61392-0740-30
 ;;9002226.02101,"998,61392-0740-30 ",.02)
 ;;61392-0740-30
 ;;9002226.02101,"998,61392-0740-31 ",.01)
 ;;61392-0740-31
 ;;9002226.02101,"998,61392-0740-31 ",.02)
 ;;61392-0740-31
 ;;9002226.02101,"998,61392-0740-32 ",.01)
 ;;61392-0740-32
 ;;9002226.02101,"998,61392-0740-32 ",.02)
 ;;61392-0740-32
 ;;9002226.02101,"998,61392-0740-39 ",.01)
 ;;61392-0740-39
 ;;9002226.02101,"998,61392-0740-39 ",.02)
 ;;61392-0740-39
 ;;9002226.02101,"998,61392-0740-45 ",.01)
 ;;61392-0740-45
 ;;9002226.02101,"998,61392-0740-45 ",.02)
 ;;61392-0740-45
 ;;9002226.02101,"998,61392-0740-51 ",.01)
 ;;61392-0740-51
 ;;9002226.02101,"998,61392-0740-51 ",.02)
 ;;61392-0740-51
 ;;9002226.02101,"998,61392-0740-54 ",.01)
 ;;61392-0740-54
 ;;9002226.02101,"998,61392-0740-54 ",.02)
 ;;61392-0740-54
 ;;9002226.02101,"998,61392-0740-60 ",.01)
 ;;61392-0740-60
 ;;9002226.02101,"998,61392-0740-60 ",.02)
 ;;61392-0740-60
 ;;9002226.02101,"998,61392-0740-90 ",.01)
 ;;61392-0740-90
 ;;9002226.02101,"998,61392-0740-90 ",.02)
 ;;61392-0740-90
 ;;9002226.02101,"998,61392-0740-91 ",.01)
 ;;61392-0740-91
 ;;9002226.02101,"998,61392-0740-91 ",.02)
 ;;61392-0740-91
 ;;9002226.02101,"998,62269-0266-29 ",.01)
 ;;62269-0266-29
 ;;9002226.02101,"998,62269-0266-29 ",.02)
 ;;62269-0266-29
 ;;9002226.02101,"998,62584-0354-01 ",.01)
 ;;62584-0354-01
 ;;9002226.02101,"998,62584-0354-01 ",.02)
 ;;62584-0354-01
 ;;9002226.02101,"998,62584-0354-11 ",.01)
 ;;62584-0354-11
 ;;9002226.02101,"998,62584-0354-11 ",.02)
 ;;62584-0354-11
 ;;9002226.02101,"998,62584-0780-01 ",.01)
 ;;62584-0780-01
 ;;9002226.02101,"998,62584-0780-01 ",.02)
 ;;62584-0780-01
 ;;9002226.02101,"998,62584-0780-11 ",.01)
 ;;62584-0780-11
 ;;9002226.02101,"998,62584-0780-11 ",.02)
 ;;62584-0780-11
 ;;9002226.02101,"998,62584-0781-01 ",.01)
 ;;62584-0781-01
 ;;9002226.02101,"998,62584-0781-01 ",.02)
 ;;62584-0781-01
 ;;9002226.02101,"998,62584-0781-11 ",.01)
 ;;62584-0781-11
 ;;9002226.02101,"998,62584-0781-11 ",.02)
 ;;62584-0781-11
 ;;9002226.02101,"998,62756-0446-02 ",.01)
 ;;62756-0446-02
 ;;9002226.02101,"998,62756-0446-02 ",.02)
 ;;62756-0446-02
 ;;9002226.02101,"998,62756-0446-04 ",.01)
 ;;62756-0446-04
 ;;9002226.02101,"998,62756-0446-04 ",.02)
 ;;62756-0446-04
 ;;9002226.02101,"998,62756-0446-05 ",.01)
 ;;62756-0446-05
 ;;9002226.02101,"998,62756-0446-05 ",.02)
 ;;62756-0446-05
 ;;9002226.02101,"998,63459-0700-60 ",.01)
 ;;63459-0700-60
 ;;9002226.02101,"998,63459-0700-60 ",.02)
 ;;63459-0700-60
 ;;9002226.02101,"998,63459-0701-60 ",.01)
 ;;63459-0701-60
 ;;9002226.02101,"998,63459-0701-60 ",.02)
 ;;63459-0701-60
 ;;9002226.02101,"998,63629-1308-00 ",.01)
 ;;63629-1308-00
 ;;9002226.02101,"998,63629-1308-00 ",.02)
 ;;63629-1308-00
 ;;9002226.02101,"998,63629-1339-01 ",.01)
 ;;63629-1339-01
 ;;9002226.02101,"998,63629-1339-01 ",.02)
 ;;63629-1339-01
 ;;9002226.02101,"998,63629-1339-02 ",.01)
 ;;63629-1339-02
 ;;9002226.02101,"998,63629-1339-02 ",.02)
 ;;63629-1339-02
 ;;9002226.02101,"998,63629-1339-03 ",.01)
 ;;63629-1339-03
 ;;9002226.02101,"998,63629-1339-03 ",.02)
 ;;63629-1339-03
 ;;9002226.02101,"998,63629-1339-04 ",.01)
 ;;63629-1339-04
 ;;9002226.02101,"998,63629-1339-04 ",.02)
 ;;63629-1339-04
 ;;9002226.02101,"998,63629-1339-05 ",.01)
 ;;63629-1339-05
 ;;9002226.02101,"998,63629-1339-05 ",.02)
 ;;63629-1339-05
 ;;9002226.02101,"998,63629-1339-06 ",.01)
 ;;63629-1339-06
 ;;9002226.02101,"998,63629-1339-06 ",.02)
 ;;63629-1339-06
 ;;9002226.02101,"998,63629-1339-07 ",.01)
 ;;63629-1339-07
 ;;9002226.02101,"998,63629-1339-07 ",.02)
 ;;63629-1339-07
 ;;9002226.02101,"998,63629-1564-01 ",.01)
 ;;63629-1564-01
 ;;9002226.02101,"998,63629-1564-01 ",.02)
 ;;63629-1564-01
 ;;9002226.02101,"998,63629-1564-02 ",.01)
 ;;63629-1564-02
 ;;9002226.02101,"998,63629-1564-02 ",.02)
 ;;63629-1564-02
 ;;9002226.02101,"998,63629-1586-01 ",.01)
 ;;63629-1586-01
 ;;9002226.02101,"998,63629-1586-01 ",.02)
 ;;63629-1586-01
 ;;9002226.02101,"998,63629-1622-01 ",.01)
 ;;63629-1622-01
 ;;9002226.02101,"998,63629-1622-01 ",.02)
 ;;63629-1622-01
 ;;9002226.02101,"998,63629-1622-02 ",.01)
 ;;63629-1622-02
 ;;9002226.02101,"998,63629-1622-02 ",.02)
 ;;63629-1622-02
 ;;9002226.02101,"998,63629-1622-03 ",.01)
 ;;63629-1622-03
 ;;9002226.02101,"998,63629-1622-03 ",.02)
 ;;63629-1622-03
 ;;9002226.02101,"998,63629-1622-04 ",.01)
 ;;63629-1622-04
 ;;9002226.02101,"998,63629-1622-04 ",.02)
 ;;63629-1622-04
 ;;9002226.02101,"998,63629-1623-01 ",.01)
 ;;63629-1623-01
 ;;9002226.02101,"998,63629-1623-01 ",.02)
 ;;63629-1623-01
 ;;9002226.02101,"998,63629-1623-02 ",.01)
 ;;63629-1623-02
 ;;9002226.02101,"998,63629-1623-02 ",.02)
 ;;63629-1623-02
 ;;9002226.02101,"998,63629-1623-03 ",.01)
 ;;63629-1623-03
 ;;9002226.02101,"998,63629-1623-03 ",.02)
 ;;63629-1623-03
 ;;9002226.02101,"998,63629-1623-04 ",.01)
 ;;63629-1623-04
 ;;9002226.02101,"998,63629-1623-04 ",.02)
 ;;63629-1623-04
 ;;9002226.02101,"998,63629-1623-05 ",.01)
 ;;63629-1623-05
 ;;9002226.02101,"998,63629-1623-05 ",.02)
 ;;63629-1623-05
 ;;9002226.02101,"998,63629-1623-06 ",.01)
 ;;63629-1623-06
 ;;9002226.02101,"998,63629-1623-06 ",.02)
 ;;63629-1623-06
 ;;9002226.02101,"998,63629-1623-07 ",.01)
 ;;63629-1623-07
 ;;9002226.02101,"998,63629-1623-07 ",.02)
 ;;63629-1623-07
 ;;9002226.02101,"998,63629-1640-01 ",.01)
 ;;63629-1640-01
 ;;9002226.02101,"998,63629-1640-01 ",.02)
 ;;63629-1640-01
 ;;9002226.02101,"998,63629-1640-02 ",.01)
 ;;63629-1640-02
 ;;9002226.02101,"998,63629-1640-02 ",.02)
 ;;63629-1640-02
 ;;9002226.02101,"998,63629-2768-01 ",.01)
 ;;63629-2768-01
 ;;9002226.02101,"998,63629-2768-01 ",.02)
 ;;63629-2768-01
 ;;9002226.02101,"998,63629-2768-02 ",.01)
 ;;63629-2768-02
 ;;9002226.02101,"998,63629-2768-02 ",.02)
 ;;63629-2768-02
 ;;9002226.02101,"998,63629-2768-03 ",.01)
 ;;63629-2768-03
 ;;9002226.02101,"998,63629-2768-03 ",.02)
 ;;63629-2768-03
 ;;9002226.02101,"998,63629-2768-04 ",.01)
 ;;63629-2768-04
 ;;9002226.02101,"998,63629-2768-04 ",.02)
 ;;63629-2768-04
 ;;9002226.02101,"998,63739-0049-10 ",.01)
 ;;63739-0049-10
 ;;9002226.02101,"998,63739-0049-10 ",.02)
 ;;63739-0049-10
 ;;9002226.02101,"998,63739-0049-15 ",.01)
 ;;63739-0049-15
 ;;9002226.02101,"998,63739-0049-15 ",.02)
 ;;63739-0049-15
 ;;9002226.02101,"998,63739-0066-10 ",.01)
 ;;63739-0066-10
 ;;9002226.02101,"998,63739-0066-10 ",.02)
 ;;63739-0066-10
 ;;9002226.02101,"998,63739-0066-15 ",.01)
 ;;63739-0066-15
 ;;9002226.02101,"998,63739-0066-15 ",.02)
 ;;63739-0066-15
 ;;9002226.02101,"998,63739-0166-10 ",.01)
 ;;63739-0166-10
 ;;9002226.02101,"998,63739-0166-10 ",.02)
 ;;63739-0166-10
 ;;9002226.02101,"998,63739-0166-15 ",.01)
 ;;63739-0166-15
 ;;9002226.02101,"998,63739-0166-15 ",.02)
 ;;63739-0166-15
 ;;9002226.02101,"998,63739-0167-10 ",.01)
 ;;63739-0167-10
 ;;9002226.02101,"998,63739-0167-10 ",.02)
 ;;63739-0167-10
 ;;9002226.02101,"998,63739-0167-15 ",.01)
 ;;63739-0167-15
 ;;9002226.02101,"998,63739-0167-15 ",.02)
 ;;63739-0167-15
 ;;9002226.02101,"998,63874-0315-01 ",.01)
 ;;63874-0315-01
 ;;9002226.02101,"998,63874-0315-01 ",.02)
 ;;63874-0315-01
 ;;9002226.02101,"998,63874-0315-02 ",.01)
 ;;63874-0315-02
 ;;9002226.02101,"998,63874-0315-02 ",.02)
 ;;63874-0315-02
 ;;9002226.02101,"998,63874-0315-04 ",.01)
 ;;63874-0315-04
 ;;9002226.02101,"998,63874-0315-04 ",.02)
 ;;63874-0315-04
 ;;9002226.02101,"998,63874-0315-05 ",.01)
 ;;63874-0315-05
 ;;9002226.02101,"998,63874-0315-05 ",.02)
 ;;63874-0315-05
 ;;9002226.02101,"998,63874-0315-07 ",.01)
 ;;63874-0315-07
 ;;9002226.02101,"998,63874-0315-07 ",.02)
 ;;63874-0315-07
 ;;9002226.02101,"998,63874-0315-10 ",.01)
 ;;63874-0315-10
 ;;9002226.02101,"998,63874-0315-10 ",.02)
 ;;63874-0315-10
 ;;9002226.02101,"998,63874-0315-12 ",.01)
 ;;63874-0315-12
 ;;9002226.02101,"998,63874-0315-12 ",.02)
 ;;63874-0315-12
 ;;9002226.02101,"998,63874-0315-14 ",.01)
 ;;63874-0315-14
 ;;9002226.02101,"998,63874-0315-14 ",.02)
 ;;63874-0315-14
 ;;9002226.02101,"998,63874-0315-15 ",.01)
 ;;63874-0315-15
