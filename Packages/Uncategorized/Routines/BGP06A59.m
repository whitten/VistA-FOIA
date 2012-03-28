BGP06A59 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON MAY 23, 2010;
 ;;10.0;IHS CLINICAL REPORTING;;JUN 18, 2010
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"974,58016-0154-40 ",.01)
 ;;58016-0154-40
 ;;9002226.02101,"974,58016-0154-40 ",.02)
 ;;58016-0154-40
 ;;9002226.02101,"974,58016-0155-00 ",.01)
 ;;58016-0155-00
 ;;9002226.02101,"974,58016-0155-00 ",.02)
 ;;58016-0155-00
 ;;9002226.02101,"974,58016-0155-10 ",.01)
 ;;58016-0155-10
 ;;9002226.02101,"974,58016-0155-10 ",.02)
 ;;58016-0155-10
 ;;9002226.02101,"974,58016-0155-12 ",.01)
 ;;58016-0155-12
 ;;9002226.02101,"974,58016-0155-12 ",.02)
 ;;58016-0155-12
 ;;9002226.02101,"974,58016-0155-15 ",.01)
 ;;58016-0155-15
 ;;9002226.02101,"974,58016-0155-15 ",.02)
 ;;58016-0155-15
 ;;9002226.02101,"974,58016-0155-16 ",.01)
 ;;58016-0155-16
 ;;9002226.02101,"974,58016-0155-16 ",.02)
 ;;58016-0155-16
 ;;9002226.02101,"974,58016-0155-18 ",.01)
 ;;58016-0155-18
 ;;9002226.02101,"974,58016-0155-18 ",.02)
 ;;58016-0155-18
 ;;9002226.02101,"974,58016-0155-20 ",.01)
 ;;58016-0155-20
 ;;9002226.02101,"974,58016-0155-20 ",.02)
 ;;58016-0155-20
 ;;9002226.02101,"974,58016-0155-24 ",.01)
 ;;58016-0155-24
 ;;9002226.02101,"974,58016-0155-24 ",.02)
 ;;58016-0155-24
 ;;9002226.02101,"974,58016-0155-28 ",.01)
 ;;58016-0155-28
 ;;9002226.02101,"974,58016-0155-28 ",.02)
 ;;58016-0155-28
 ;;9002226.02101,"974,58016-0155-30 ",.01)
 ;;58016-0155-30
 ;;9002226.02101,"974,58016-0155-30 ",.02)
 ;;58016-0155-30
 ;;9002226.02101,"974,58016-0155-36 ",.01)
 ;;58016-0155-36
 ;;9002226.02101,"974,58016-0155-36 ",.02)
 ;;58016-0155-36
 ;;9002226.02101,"974,58016-0155-40 ",.01)
 ;;58016-0155-40
 ;;9002226.02101,"974,58016-0155-40 ",.02)
 ;;58016-0155-40
 ;;9002226.02101,"974,58016-0156-00 ",.01)
 ;;58016-0156-00
 ;;9002226.02101,"974,58016-0156-00 ",.02)
 ;;58016-0156-00
 ;;9002226.02101,"974,58016-0156-02 ",.01)
 ;;58016-0156-02
 ;;9002226.02101,"974,58016-0156-02 ",.02)
 ;;58016-0156-02
 ;;9002226.02101,"974,58016-0156-03 ",.01)
 ;;58016-0156-03
 ;;9002226.02101,"974,58016-0156-03 ",.02)
 ;;58016-0156-03
 ;;9002226.02101,"974,58016-0156-06 ",.01)
 ;;58016-0156-06
 ;;9002226.02101,"974,58016-0156-06 ",.02)
 ;;58016-0156-06
 ;;9002226.02101,"974,58016-0156-07 ",.01)
 ;;58016-0156-07
 ;;9002226.02101,"974,58016-0156-07 ",.02)
 ;;58016-0156-07
 ;;9002226.02101,"974,58016-0156-10 ",.01)
 ;;58016-0156-10
 ;;9002226.02101,"974,58016-0156-10 ",.02)
 ;;58016-0156-10
 ;;9002226.02101,"974,58016-0156-12 ",.01)
 ;;58016-0156-12
 ;;9002226.02101,"974,58016-0156-12 ",.02)
 ;;58016-0156-12
 ;;9002226.02101,"974,58016-0156-14 ",.01)
 ;;58016-0156-14
 ;;9002226.02101,"974,58016-0156-14 ",.02)
 ;;58016-0156-14
 ;;9002226.02101,"974,58016-0156-15 ",.01)
 ;;58016-0156-15
 ;;9002226.02101,"974,58016-0156-15 ",.02)
 ;;58016-0156-15
 ;;9002226.02101,"974,58016-0156-16 ",.01)
 ;;58016-0156-16
 ;;9002226.02101,"974,58016-0156-16 ",.02)
 ;;58016-0156-16
 ;;9002226.02101,"974,58016-0156-20 ",.01)
 ;;58016-0156-20
 ;;9002226.02101,"974,58016-0156-20 ",.02)
 ;;58016-0156-20
 ;;9002226.02101,"974,58016-0156-21 ",.01)
 ;;58016-0156-21
 ;;9002226.02101,"974,58016-0156-21 ",.02)
 ;;58016-0156-21
 ;;9002226.02101,"974,58016-0156-24 ",.01)
 ;;58016-0156-24
 ;;9002226.02101,"974,58016-0156-24 ",.02)
 ;;58016-0156-24
 ;;9002226.02101,"974,58016-0156-28 ",.01)
 ;;58016-0156-28
 ;;9002226.02101,"974,58016-0156-28 ",.02)
 ;;58016-0156-28
 ;;9002226.02101,"974,58016-0156-30 ",.01)
 ;;58016-0156-30
 ;;9002226.02101,"974,58016-0156-30 ",.02)
 ;;58016-0156-30
 ;;9002226.02101,"974,58016-0156-40 ",.01)
 ;;58016-0156-40
 ;;9002226.02101,"974,58016-0156-40 ",.02)
 ;;58016-0156-40
 ;;9002226.02101,"974,58016-0156-60 ",.01)
 ;;58016-0156-60
 ;;9002226.02101,"974,58016-0156-60 ",.02)
 ;;58016-0156-60
 ;;9002226.02101,"974,58016-0156-73 ",.01)
 ;;58016-0156-73
 ;;9002226.02101,"974,58016-0156-73 ",.02)
 ;;58016-0156-73
 ;;9002226.02101,"974,58016-0156-89 ",.01)
 ;;58016-0156-89
 ;;9002226.02101,"974,58016-0156-89 ",.02)
 ;;58016-0156-89
 ;;9002226.02101,"974,58016-0161-06 ",.01)
 ;;58016-0161-06
 ;;9002226.02101,"974,58016-0161-06 ",.02)
 ;;58016-0161-06
 ;;9002226.02101,"974,58016-0161-10 ",.01)
 ;;58016-0161-10
 ;;9002226.02101,"974,58016-0161-10 ",.02)
 ;;58016-0161-10
 ;;9002226.02101,"974,58016-0161-12 ",.01)
 ;;58016-0161-12
 ;;9002226.02101,"974,58016-0161-12 ",.02)
 ;;58016-0161-12
 ;;9002226.02101,"974,58016-0161-14 ",.01)
 ;;58016-0161-14
 ;;9002226.02101,"974,58016-0161-14 ",.02)
 ;;58016-0161-14
 ;;9002226.02101,"974,58016-0161-15 ",.01)
 ;;58016-0161-15
 ;;9002226.02101,"974,58016-0161-15 ",.02)
 ;;58016-0161-15
 ;;9002226.02101,"974,58016-0161-16 ",.01)
 ;;58016-0161-16
 ;;9002226.02101,"974,58016-0161-16 ",.02)
 ;;58016-0161-16
 ;;9002226.02101,"974,58016-0161-18 ",.01)
 ;;58016-0161-18
 ;;9002226.02101,"974,58016-0161-18 ",.02)
 ;;58016-0161-18
 ;;9002226.02101,"974,58016-0161-20 ",.01)
 ;;58016-0161-20
 ;;9002226.02101,"974,58016-0161-20 ",.02)
 ;;58016-0161-20
 ;;9002226.02101,"974,58016-0161-21 ",.01)
 ;;58016-0161-21
 ;;9002226.02101,"974,58016-0161-21 ",.02)
 ;;58016-0161-21
 ;;9002226.02101,"974,58016-0161-24 ",.01)
 ;;58016-0161-24
 ;;9002226.02101,"974,58016-0161-24 ",.02)
 ;;58016-0161-24
 ;;9002226.02101,"974,58016-0161-28 ",.01)
 ;;58016-0161-28
 ;;9002226.02101,"974,58016-0161-28 ",.02)
 ;;58016-0161-28
 ;;9002226.02101,"974,58016-0161-30 ",.01)
 ;;58016-0161-30
 ;;9002226.02101,"974,58016-0161-30 ",.02)
 ;;58016-0161-30
 ;;9002226.02101,"974,58016-0161-40 ",.01)
 ;;58016-0161-40
 ;;9002226.02101,"974,58016-0161-40 ",.02)
 ;;58016-0161-40
 ;;9002226.02101,"974,58016-0161-50 ",.01)
 ;;58016-0161-50
 ;;9002226.02101,"974,58016-0161-50 ",.02)
 ;;58016-0161-50
 ;;9002226.02101,"974,58016-0161-60 ",.01)
 ;;58016-0161-60
 ;;9002226.02101,"974,58016-0161-60 ",.02)
 ;;58016-0161-60
 ;;9002226.02101,"974,58016-0162-28 ",.01)
 ;;58016-0162-28
 ;;9002226.02101,"974,58016-0162-28 ",.02)
 ;;58016-0162-28
 ;;9002226.02101,"974,58016-0162-30 ",.01)
 ;;58016-0162-30
 ;;9002226.02101,"974,58016-0162-30 ",.02)
 ;;58016-0162-30
 ;;9002226.02101,"974,58016-0162-40 ",.01)
 ;;58016-0162-40
 ;;9002226.02101,"974,58016-0162-40 ",.02)
 ;;58016-0162-40
 ;;9002226.02101,"974,58016-0164-10 ",.01)
 ;;58016-0164-10
 ;;9002226.02101,"974,58016-0164-10 ",.02)
 ;;58016-0164-10
 ;;9002226.02101,"974,58016-0164-20 ",.01)
 ;;58016-0164-20
 ;;9002226.02101,"974,58016-0164-20 ",.02)
 ;;58016-0164-20
 ;;9002226.02101,"974,58016-0164-24 ",.01)
 ;;58016-0164-24
 ;;9002226.02101,"974,58016-0164-24 ",.02)
 ;;58016-0164-24
 ;;9002226.02101,"974,58016-0164-30 ",.01)
 ;;58016-0164-30
 ;;9002226.02101,"974,58016-0164-30 ",.02)
 ;;58016-0164-30
 ;;9002226.02101,"974,58016-0164-36 ",.01)
 ;;58016-0164-36
 ;;9002226.02101,"974,58016-0164-36 ",.02)
 ;;58016-0164-36
 ;;9002226.02101,"974,58016-0164-40 ",.01)
 ;;58016-0164-40
 ;;9002226.02101,"974,58016-0164-40 ",.02)
 ;;58016-0164-40
 ;;9002226.02101,"974,58016-0164-48 ",.01)
 ;;58016-0164-48
 ;;9002226.02101,"974,58016-0164-48 ",.02)
 ;;58016-0164-48
 ;;9002226.02101,"974,58016-0167-00 ",.01)
 ;;58016-0167-00
 ;;9002226.02101,"974,58016-0167-00 ",.02)
 ;;58016-0167-00
 ;;9002226.02101,"974,58016-0167-12 ",.01)
 ;;58016-0167-12
 ;;9002226.02101,"974,58016-0167-12 ",.02)
 ;;58016-0167-12
 ;;9002226.02101,"974,58016-0167-15 ",.01)
 ;;58016-0167-15
 ;;9002226.02101,"974,58016-0167-15 ",.02)
 ;;58016-0167-15
 ;;9002226.02101,"974,58016-0167-20 ",.01)
 ;;58016-0167-20
 ;;9002226.02101,"974,58016-0167-20 ",.02)
 ;;58016-0167-20
 ;;9002226.02101,"974,58016-0167-30 ",.01)
 ;;58016-0167-30
 ;;9002226.02101,"974,58016-0167-30 ",.02)
 ;;58016-0167-30
 ;;9002226.02101,"974,58016-0167-40 ",.01)
 ;;58016-0167-40
 ;;9002226.02101,"974,58016-0167-40 ",.02)
 ;;58016-0167-40
 ;;9002226.02101,"974,58016-0171-20 ",.01)
 ;;58016-0171-20
 ;;9002226.02101,"974,58016-0171-20 ",.02)
 ;;58016-0171-20
 ;;9002226.02101,"974,58016-0171-30 ",.01)
 ;;58016-0171-30
 ;;9002226.02101,"974,58016-0171-30 ",.02)
 ;;58016-0171-30
 ;;9002226.02101,"974,58016-0175-30 ",.01)
 ;;58016-0175-30
 ;;9002226.02101,"974,58016-0175-30 ",.02)
 ;;58016-0175-30
 ;;9002226.02101,"974,58016-0175-40 ",.01)
 ;;58016-0175-40
 ;;9002226.02101,"974,58016-0175-40 ",.02)
 ;;58016-0175-40
 ;;9002226.02101,"974,58016-0176-40 ",.01)
 ;;58016-0176-40
 ;;9002226.02101,"974,58016-0176-40 ",.02)
 ;;58016-0176-40
 ;;9002226.02101,"974,58016-0184-00 ",.01)
 ;;58016-0184-00
 ;;9002226.02101,"974,58016-0184-00 ",.02)
 ;;58016-0184-00
 ;;9002226.02101,"974,58016-0184-12 ",.01)
 ;;58016-0184-12
 ;;9002226.02101,"974,58016-0184-12 ",.02)
 ;;58016-0184-12
 ;;9002226.02101,"974,58016-0184-15 ",.01)
 ;;58016-0184-15
 ;;9002226.02101,"974,58016-0184-15 ",.02)
 ;;58016-0184-15
 ;;9002226.02101,"974,58016-0184-18 ",.01)
 ;;58016-0184-18
 ;;9002226.02101,"974,58016-0184-18 ",.02)
 ;;58016-0184-18
 ;;9002226.02101,"974,58016-0184-24 ",.01)
 ;;58016-0184-24
 ;;9002226.02101,"974,58016-0184-24 ",.02)
 ;;58016-0184-24
 ;;9002226.02101,"974,58016-0184-30 ",.01)
 ;;58016-0184-30
 ;;9002226.02101,"974,58016-0184-30 ",.02)
 ;;58016-0184-30
 ;;9002226.02101,"974,58016-0184-40 ",.01)
 ;;58016-0184-40
 ;;9002226.02101,"974,58016-0184-40 ",.02)
 ;;58016-0184-40
 ;;9002226.02101,"974,58016-0284-15 ",.01)
 ;;58016-0284-15
 ;;9002226.02101,"974,58016-0284-15 ",.02)
 ;;58016-0284-15
 ;;9002226.02101,"974,58016-0284-20 ",.01)
 ;;58016-0284-20
 ;;9002226.02101,"974,58016-0284-20 ",.02)
 ;;58016-0284-20
 ;;9002226.02101,"974,58016-0284-30 ",.01)
 ;;58016-0284-30
 ;;9002226.02101,"974,58016-0284-30 ",.02)
 ;;58016-0284-30
 ;;9002226.02101,"974,58016-0284-50 ",.01)
 ;;58016-0284-50
 ;;9002226.02101,"974,58016-0284-50 ",.02)
 ;;58016-0284-50
 ;;9002226.02101,"974,58016-0339-12 ",.01)
 ;;58016-0339-12
 ;;9002226.02101,"974,58016-0339-12 ",.02)
 ;;58016-0339-12
 ;;9002226.02101,"974,58016-0339-15 ",.01)
 ;;58016-0339-15
 ;;9002226.02101,"974,58016-0339-15 ",.02)
 ;;58016-0339-15
 ;;9002226.02101,"974,58016-0339-18 ",.01)
 ;;58016-0339-18
 ;;9002226.02101,"974,58016-0339-18 ",.02)
 ;;58016-0339-18
 ;;9002226.02101,"974,58016-0339-20 ",.01)
 ;;58016-0339-20
 ;;9002226.02101,"974,58016-0339-20 ",.02)
 ;;58016-0339-20
 ;;9002226.02101,"974,58016-0339-30 ",.01)
 ;;58016-0339-30
 ;;9002226.02101,"974,58016-0339-30 ",.02)
 ;;58016-0339-30
 ;;9002226.02101,"974,58016-0356-00 ",.01)
 ;;58016-0356-00
 ;;9002226.02101,"974,58016-0356-00 ",.02)
 ;;58016-0356-00
 ;;9002226.02101,"974,58016-0356-21 ",.01)
 ;;58016-0356-21
 ;;9002226.02101,"974,58016-0356-21 ",.02)
 ;;58016-0356-21
 ;;9002226.02101,"974,58016-0356-30 ",.01)
 ;;58016-0356-30
 ;;9002226.02101,"974,58016-0356-30 ",.02)
 ;;58016-0356-30
 ;;9002226.02101,"974,58016-0356-60 ",.01)
 ;;58016-0356-60
 ;;9002226.02101,"974,58016-0356-60 ",.02)
 ;;58016-0356-60
 ;;9002226.02101,"974,58016-0356-90 ",.01)
 ;;58016-0356-90
 ;;9002226.02101,"974,58016-0356-90 ",.02)
 ;;58016-0356-90
 ;;9002226.02101,"974,58016-0362-40 ",.01)
 ;;58016-0362-40
 ;;9002226.02101,"974,58016-0362-40 ",.02)
 ;;58016-0362-40
 ;;9002226.02101,"974,58016-0373-20 ",.01)
 ;;58016-0373-20
 ;;9002226.02101,"974,58016-0373-20 ",.02)
 ;;58016-0373-20
 ;;9002226.02101,"974,58016-0373-40 ",.01)
 ;;58016-0373-40
 ;;9002226.02101,"974,58016-0373-40 ",.02)
 ;;58016-0373-40
 ;;9002226.02101,"974,58016-0391-00 ",.01)
 ;;58016-0391-00
 ;;9002226.02101,"974,58016-0391-00 ",.02)
 ;;58016-0391-00
 ;;9002226.02101,"974,58016-0391-01 ",.01)
 ;;58016-0391-01
 ;;9002226.02101,"974,58016-0391-01 ",.02)
 ;;58016-0391-01
 ;;9002226.02101,"974,58016-0391-06 ",.01)
 ;;58016-0391-06
 ;;9002226.02101,"974,58016-0391-06 ",.02)
 ;;58016-0391-06
 ;;9002226.02101,"974,58016-0391-10 ",.01)
 ;;58016-0391-10
 ;;9002226.02101,"974,58016-0391-10 ",.02)
 ;;58016-0391-10
 ;;9002226.02101,"974,58016-0391-15 ",.01)
 ;;58016-0391-15
 ;;9002226.02101,"974,58016-0391-15 ",.02)
 ;;58016-0391-15
 ;;9002226.02101,"974,58016-0391-18 ",.01)
 ;;58016-0391-18
 ;;9002226.02101,"974,58016-0391-18 ",.02)
 ;;58016-0391-18
 ;;9002226.02101,"974,58016-0391-20 ",.01)
 ;;58016-0391-20
 ;;9002226.02101,"974,58016-0391-20 ",.02)
 ;;58016-0391-20
 ;;9002226.02101,"974,58016-0391-28 ",.01)
 ;;58016-0391-28
 ;;9002226.02101,"974,58016-0391-28 ",.02)
 ;;58016-0391-28
 ;;9002226.02101,"974,58016-0391-30 ",.01)
 ;;58016-0391-30
 ;;9002226.02101,"974,58016-0391-30 ",.02)
 ;;58016-0391-30
 ;;9002226.02101,"974,58016-0391-60 ",.01)
 ;;58016-0391-60
 ;;9002226.02101,"974,58016-0391-60 ",.02)
 ;;58016-0391-60
 ;;9002226.02101,"974,58016-0391-90 ",.01)
 ;;58016-0391-90
 ;;9002226.02101,"974,58016-0391-90 ",.02)
 ;;58016-0391-90
 ;;9002226.02101,"974,58016-0453-00 ",.01)
 ;;58016-0453-00
 ;;9002226.02101,"974,58016-0453-00 ",.02)
 ;;58016-0453-00
 ;;9002226.02101,"974,58016-0453-12 ",.01)
 ;;58016-0453-12
 ;;9002226.02101,"974,58016-0453-12 ",.02)
 ;;58016-0453-12
 ;;9002226.02101,"974,58016-0453-15 ",.01)
 ;;58016-0453-15
 ;;9002226.02101,"974,58016-0453-15 ",.02)
 ;;58016-0453-15
 ;;9002226.02101,"974,58016-0453-20 ",.01)
 ;;58016-0453-20
 ;;9002226.02101,"974,58016-0453-20 ",.02)
 ;;58016-0453-20
 ;;9002226.02101,"974,58016-0453-21 ",.01)
 ;;58016-0453-21
 ;;9002226.02101,"974,58016-0453-21 ",.02)
 ;;58016-0453-21
 ;;9002226.02101,"974,58016-0453-30 ",.01)
 ;;58016-0453-30
 ;;9002226.02101,"974,58016-0453-30 ",.02)
 ;;58016-0453-30
 ;;9002226.02101,"974,58016-0453-40 ",.01)
 ;;58016-0453-40
 ;;9002226.02101,"974,58016-0453-40 ",.02)
 ;;58016-0453-40
 ;;9002226.02101,"974,58016-0512-20 ",.01)
 ;;58016-0512-20
 ;;9002226.02101,"974,58016-0512-20 ",.02)
 ;;58016-0512-20
