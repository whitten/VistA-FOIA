BGP8EXWN ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 28, 2007 ;
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
 ;;9002226.02101,"635,58016-0685-90 ",.01)
 ;;58016-0685-90
 ;;9002226.02101,"635,58016-0685-90 ",.02)
 ;;58016-0685-90
 ;;9002226.02101,"635,58016-0686-00 ",.01)
 ;;58016-0686-00
 ;;9002226.02101,"635,58016-0686-00 ",.02)
 ;;58016-0686-00
 ;;9002226.02101,"635,58016-0686-10 ",.01)
 ;;58016-0686-10
 ;;9002226.02101,"635,58016-0686-10 ",.02)
 ;;58016-0686-10
 ;;9002226.02101,"635,58016-0686-30 ",.01)
 ;;58016-0686-30
 ;;9002226.02101,"635,58016-0686-30 ",.02)
 ;;58016-0686-30
 ;;9002226.02101,"635,58016-0686-60 ",.01)
 ;;58016-0686-60
 ;;9002226.02101,"635,58016-0686-60 ",.02)
 ;;58016-0686-60
 ;;9002226.02101,"635,58016-0686-90 ",.01)
 ;;58016-0686-90
 ;;9002226.02101,"635,58016-0686-90 ",.02)
 ;;58016-0686-90
 ;;9002226.02101,"635,58016-0760-00 ",.01)
 ;;58016-0760-00
 ;;9002226.02101,"635,58016-0760-00 ",.02)
 ;;58016-0760-00
 ;;9002226.02101,"635,58016-0760-30 ",.01)
 ;;58016-0760-30
 ;;9002226.02101,"635,58016-0760-30 ",.02)
 ;;58016-0760-30
 ;;9002226.02101,"635,58016-0760-60 ",.01)
 ;;58016-0760-60
 ;;9002226.02101,"635,58016-0760-60 ",.02)
 ;;58016-0760-60
 ;;9002226.02101,"635,58016-0760-90 ",.01)
 ;;58016-0760-90
 ;;9002226.02101,"635,58016-0760-90 ",.02)
 ;;58016-0760-90
 ;;9002226.02101,"635,58016-0917-00 ",.01)
 ;;58016-0917-00
 ;;9002226.02101,"635,58016-0917-00 ",.02)
 ;;58016-0917-00
 ;;9002226.02101,"635,58016-0917-30 ",.01)
 ;;58016-0917-30
 ;;9002226.02101,"635,58016-0917-30 ",.02)
 ;;58016-0917-30
 ;;9002226.02101,"635,58016-0917-60 ",.01)
 ;;58016-0917-60
 ;;9002226.02101,"635,58016-0917-60 ",.02)
 ;;58016-0917-60
 ;;9002226.02101,"635,58016-0917-90 ",.01)
 ;;58016-0917-90
 ;;9002226.02101,"635,58016-0917-90 ",.02)
 ;;58016-0917-90
 ;;9002226.02101,"635,58016-0956-00 ",.01)
 ;;58016-0956-00
 ;;9002226.02101,"635,58016-0956-00 ",.02)
 ;;58016-0956-00
 ;;9002226.02101,"635,58016-0956-30 ",.01)
 ;;58016-0956-30
 ;;9002226.02101,"635,58016-0956-30 ",.02)
 ;;58016-0956-30
 ;;9002226.02101,"635,58016-0956-60 ",.01)
 ;;58016-0956-60
 ;;9002226.02101,"635,58016-0956-60 ",.02)
 ;;58016-0956-60
 ;;9002226.02101,"635,58016-0956-90 ",.01)
 ;;58016-0956-90
 ;;9002226.02101,"635,58016-0956-90 ",.02)
 ;;58016-0956-90
 ;;9002226.02101,"635,58016-0963-00 ",.01)
 ;;58016-0963-00
 ;;9002226.02101,"635,58016-0963-00 ",.02)
 ;;58016-0963-00
 ;;9002226.02101,"635,58016-0963-30 ",.01)
 ;;58016-0963-30
 ;;9002226.02101,"635,58016-0963-30 ",.02)
 ;;58016-0963-30
 ;;9002226.02101,"635,58016-0963-60 ",.01)
 ;;58016-0963-60
 ;;9002226.02101,"635,58016-0963-60 ",.02)
 ;;58016-0963-60
 ;;9002226.02101,"635,58016-0963-90 ",.01)
 ;;58016-0963-90
 ;;9002226.02101,"635,58016-0963-90 ",.02)
 ;;58016-0963-90
 ;;9002226.02101,"635,58016-0998-00 ",.01)
 ;;58016-0998-00
 ;;9002226.02101,"635,58016-0998-00 ",.02)
 ;;58016-0998-00
 ;;9002226.02101,"635,58016-0998-30 ",.01)
 ;;58016-0998-30
 ;;9002226.02101,"635,58016-0998-30 ",.02)
 ;;58016-0998-30
 ;;9002226.02101,"635,58016-0998-60 ",.01)
 ;;58016-0998-60
 ;;9002226.02101,"635,58016-0998-60 ",.02)
 ;;58016-0998-60
 ;;9002226.02101,"635,58016-0998-90 ",.01)
 ;;58016-0998-90
 ;;9002226.02101,"635,58016-0998-90 ",.02)
 ;;58016-0998-90
 ;;9002226.02101,"635,58016-2638-90 ",.01)
 ;;58016-2638-90
 ;;9002226.02101,"635,58016-2638-90 ",.02)
 ;;58016-2638-90
 ;;9002226.02101,"635,58177-0341-04 ",.01)
 ;;58177-0341-04
 ;;9002226.02101,"635,58177-0341-04 ",.02)
 ;;58177-0341-04
 ;;9002226.02101,"635,58177-0342-04 ",.01)
 ;;58177-0342-04
 ;;9002226.02101,"635,58177-0342-04 ",.02)
 ;;58177-0342-04
 ;;9002226.02101,"635,58177-0342-08 ",.01)
 ;;58177-0342-08
 ;;9002226.02101,"635,58177-0342-08 ",.02)
 ;;58177-0342-08
 ;;9002226.02101,"635,58177-0343-04 ",.01)
 ;;58177-0343-04
 ;;9002226.02101,"635,58177-0343-04 ",.02)
 ;;58177-0343-04
 ;;9002226.02101,"635,58177-0343-08 ",.01)
 ;;58177-0343-08
 ;;9002226.02101,"635,58177-0343-08 ",.02)
 ;;58177-0343-08
 ;;9002226.02101,"635,58177-0344-04 ",.01)
 ;;58177-0344-04
 ;;9002226.02101,"635,58177-0344-04 ",.02)
 ;;58177-0344-04
 ;;9002226.02101,"635,58177-0344-08 ",.01)
 ;;58177-0344-08
 ;;9002226.02101,"635,58177-0344-08 ",.02)
 ;;58177-0344-08
 ;;9002226.02101,"635,58864-0859-30 ",.01)
 ;;58864-0859-30
 ;;9002226.02101,"635,58864-0859-30 ",.02)
 ;;58864-0859-30
 ;;9002226.02101,"635,58864-0863-30 ",.01)
 ;;58864-0863-30
 ;;9002226.02101,"635,58864-0863-30 ",.02)
 ;;58864-0863-30
 ;;9002226.02101,"635,58864-0869-30 ",.01)
 ;;58864-0869-30
 ;;9002226.02101,"635,58864-0869-30 ",.02)
 ;;58864-0869-30
 ;;9002226.02101,"635,59762-0220-01 ",.01)
 ;;59762-0220-01
 ;;9002226.02101,"635,59762-0220-01 ",.02)
 ;;59762-0220-01
 ;;9002226.02101,"635,59762-0222-01 ",.01)
 ;;59762-0222-01
 ;;9002226.02101,"635,59762-0222-01 ",.02)
 ;;59762-0222-01
 ;;9002226.02101,"635,59762-0223-01 ",.01)
 ;;59762-0223-01
 ;;9002226.02101,"635,59762-0223-01 ",.02)
 ;;59762-0223-01
 ;;9002226.02101,"635,59762-5019-01 ",.01)
 ;;59762-5019-01
 ;;9002226.02101,"635,59762-5019-01 ",.02)
 ;;59762-5019-01
 ;;9002226.02101,"635,59762-5020-01 ",.01)
 ;;59762-5020-01
 ;;9002226.02101,"635,59762-5020-01 ",.02)
 ;;59762-5020-01
 ;;9002226.02101,"635,59762-5021-01 ",.01)
 ;;59762-5021-01
 ;;9002226.02101,"635,59762-5021-01 ",.02)
 ;;59762-5021-01
 ;;9002226.02101,"635,59762-5022-01 ",.01)
 ;;59762-5022-01
 ;;9002226.02101,"635,59762-5022-01 ",.02)
 ;;59762-5022-01
 ;;9002226.02101,"635,59772-5160-05 ",.01)
 ;;59772-5160-05
 ;;9002226.02101,"635,59772-5160-05 ",.02)
 ;;59772-5160-05
 ;;9002226.02101,"635,59772-5161-05 ",.01)
 ;;59772-5161-05
 ;;9002226.02101,"635,59772-5161-05 ",.02)
 ;;59772-5161-05
 ;;9002226.02101,"635,59772-5162-05 ",.01)
 ;;59772-5162-05
 ;;9002226.02101,"635,59772-5162-05 ",.02)
 ;;59772-5162-05
 ;;9002226.02101,"635,59772-5163-05 ",.01)
 ;;59772-5163-05
 ;;9002226.02101,"635,59772-5163-05 ",.02)
 ;;59772-5163-05
 ;;9002226.02101,"635,59772-5541-01 ",.01)
 ;;59772-5541-01
 ;;9002226.02101,"635,59772-5541-01 ",.02)
 ;;59772-5541-01
 ;;9002226.02101,"635,59772-5541-03 ",.01)
 ;;59772-5541-03
 ;;9002226.02101,"635,59772-5541-03 ",.02)
 ;;59772-5541-03
 ;;9002226.02101,"635,59772-5542-01 ",.01)
 ;;59772-5542-01
 ;;9002226.02101,"635,59772-5542-01 ",.02)
 ;;59772-5542-01
 ;;9002226.02101,"635,59772-5542-03 ",.01)
 ;;59772-5542-03
 ;;9002226.02101,"635,59772-5542-03 ",.02)
 ;;59772-5542-03
 ;;9002226.02101,"635,59772-5543-01 ",.01)
 ;;59772-5543-01
 ;;9002226.02101,"635,59772-5543-01 ",.02)
 ;;59772-5543-01
 ;;9002226.02101,"635,59772-5543-03 ",.01)
 ;;59772-5543-03
 ;;9002226.02101,"635,59772-5543-03 ",.02)
 ;;59772-5543-03
 ;;9002226.02101,"635,59772-5544-01 ",.01)
 ;;59772-5544-01
 ;;9002226.02101,"635,59772-5544-01 ",.02)
 ;;59772-5544-01
 ;;9002226.02101,"635,59772-5544-03 ",.01)
 ;;59772-5544-03
 ;;9002226.02101,"635,59772-5544-03 ",.02)
 ;;59772-5544-03
 ;;9002226.02101,"635,59772-7045-01 ",.01)
 ;;59772-7045-01
 ;;9002226.02101,"635,59772-7045-01 ",.02)
 ;;59772-7045-01
 ;;9002226.02101,"635,59772-7045-03 ",.01)
 ;;59772-7045-03
 ;;9002226.02101,"635,59772-7045-03 ",.02)
 ;;59772-7045-03
 ;;9002226.02101,"635,59772-7046-01 ",.01)
 ;;59772-7046-01
 ;;9002226.02101,"635,59772-7046-01 ",.02)
 ;;59772-7046-01
 ;;9002226.02101,"635,59772-7046-03 ",.01)
 ;;59772-7046-03
 ;;9002226.02101,"635,59772-7046-03 ",.02)
 ;;59772-7046-03
 ;;9002226.02101,"635,59772-7046-05 ",.01)
 ;;59772-7046-05
 ;;9002226.02101,"635,59772-7046-05 ",.02)
 ;;59772-7046-05
 ;;9002226.02101,"635,59772-7047-01 ",.01)
 ;;59772-7047-01
 ;;9002226.02101,"635,59772-7047-01 ",.02)
 ;;59772-7047-01
 ;;9002226.02101,"635,59772-7047-03 ",.01)
 ;;59772-7047-03
 ;;9002226.02101,"635,59772-7047-03 ",.02)
 ;;59772-7047-03
 ;;9002226.02101,"635,59772-7048-01 ",.01)
 ;;59772-7048-01
 ;;9002226.02101,"635,59772-7048-01 ",.02)
 ;;59772-7048-01
 ;;9002226.02101,"635,59930-1655-01 ",.01)
 ;;59930-1655-01
 ;;9002226.02101,"635,59930-1655-01 ",.02)
 ;;59930-1655-01
 ;;9002226.02101,"635,59930-1655-02 ",.01)
 ;;59930-1655-02
 ;;9002226.02101,"635,59930-1655-02 ",.02)
 ;;59930-1655-02
 ;;9002226.02101,"635,59930-1655-03 ",.01)
 ;;59930-1655-03
 ;;9002226.02101,"635,59930-1655-03 ",.02)
 ;;59930-1655-03
 ;;9002226.02101,"635,59930-1656-01 ",.01)
 ;;59930-1656-01
 ;;9002226.02101,"635,59930-1656-01 ",.02)
 ;;59930-1656-01
 ;;9002226.02101,"635,59930-1656-02 ",.01)
 ;;59930-1656-02
 ;;9002226.02101,"635,59930-1656-02 ",.02)
 ;;59930-1656-02
 ;;9002226.02101,"635,59930-1656-03 ",.01)
 ;;59930-1656-03
 ;;9002226.02101,"635,59930-1656-03 ",.02)
 ;;59930-1656-03
 ;;9002226.02101,"635,59930-1657-01 ",.01)
 ;;59930-1657-01
 ;;9002226.02101,"635,59930-1657-01 ",.02)
 ;;59930-1657-01
 ;;9002226.02101,"635,59930-1657-02 ",.01)
 ;;59930-1657-02
 ;;9002226.02101,"635,59930-1657-02 ",.02)
 ;;59930-1657-02
 ;;9002226.02101,"635,59930-1657-03 ",.01)
 ;;59930-1657-03
 ;;9002226.02101,"635,59930-1657-03 ",.02)
 ;;59930-1657-03
 ;;9002226.02101,"635,59930-1658-01 ",.01)
 ;;59930-1658-01
 ;;9002226.02101,"635,59930-1658-01 ",.02)
 ;;59930-1658-01
 ;;9002226.02101,"635,59930-1658-02 ",.01)
 ;;59930-1658-02
 ;;9002226.02101,"635,59930-1658-02 ",.02)
 ;;59930-1658-02
 ;;9002226.02101,"635,59930-1658-03 ",.01)
 ;;59930-1658-03
 ;;9002226.02101,"635,59930-1658-03 ",.02)
 ;;59930-1658-03
 ;;9002226.02101,"635,60346-0595-30 ",.01)
 ;;60346-0595-30
 ;;9002226.02101,"635,60346-0595-30 ",.02)
 ;;60346-0595-30
 ;;9002226.02101,"635,60346-0871-03 ",.01)
 ;;60346-0871-03
 ;;9002226.02101,"635,60346-0871-03 ",.02)
 ;;60346-0871-03
 ;;9002226.02101,"635,60346-0871-30 ",.01)
 ;;60346-0871-30
 ;;9002226.02101,"635,60346-0871-30 ",.02)
 ;;60346-0871-30
 ;;9002226.02101,"635,60429-0029-90 ",.01)
 ;;60429-0029-90
 ;;9002226.02101,"635,60429-0029-90 ",.02)
 ;;60429-0029-90
 ;;9002226.02101,"635,60429-0030-12 ",.01)
 ;;60429-0030-12
 ;;9002226.02101,"635,60429-0030-12 ",.02)
 ;;60429-0030-12
 ;;9002226.02101,"635,60429-0030-27 ",.01)
 ;;60429-0030-27
 ;;9002226.02101,"635,60429-0030-27 ",.02)
 ;;60429-0030-27
 ;;9002226.02101,"635,60429-0030-60 ",.01)
 ;;60429-0030-60
 ;;9002226.02101,"635,60429-0030-60 ",.02)
 ;;60429-0030-60
 ;;9002226.02101,"635,60429-0030-90 ",.01)
 ;;60429-0030-90
 ;;9002226.02101,"635,60429-0030-90 ",.02)
 ;;60429-0030-90
 ;;9002226.02101,"635,60429-0031-12 ",.01)
 ;;60429-0031-12
 ;;9002226.02101,"635,60429-0031-12 ",.02)
 ;;60429-0031-12
 ;;9002226.02101,"635,60429-0031-27 ",.01)
 ;;60429-0031-27
 ;;9002226.02101,"635,60429-0031-27 ",.02)
 ;;60429-0031-27
 ;;9002226.02101,"635,60429-0031-30 ",.01)
 ;;60429-0031-30
 ;;9002226.02101,"635,60429-0031-30 ",.02)
 ;;60429-0031-30
 ;;9002226.02101,"635,60429-0031-60 ",.01)
 ;;60429-0031-60
 ;;9002226.02101,"635,60429-0031-60 ",.02)
 ;;60429-0031-60
 ;;9002226.02101,"635,60429-0031-90 ",.01)
 ;;60429-0031-90
 ;;9002226.02101,"635,60429-0031-90 ",.02)
 ;;60429-0031-90
 ;;9002226.02101,"635,60505-0003-06 ",.01)
 ;;60505-0003-06
 ;;9002226.02101,"635,60505-0003-06 ",.02)
 ;;60505-0003-06
 ;;9002226.02101,"635,60505-0004-06 ",.01)
 ;;60505-0004-06
 ;;9002226.02101,"635,60505-0004-06 ",.02)
 ;;60505-0004-06
 ;;9002226.02101,"635,60505-0005-06 ",.01)
 ;;60505-0005-06
 ;;9002226.02101,"635,60505-0005-06 ",.02)
 ;;60505-0005-06
 ;;9002226.02101,"635,60505-0006-06 ",.01)
 ;;60505-0006-06
 ;;9002226.02101,"635,60505-0006-06 ",.02)
 ;;60505-0006-06
 ;;9002226.02101,"635,60505-0184-00 ",.01)
 ;;60505-0184-00
 ;;9002226.02101,"635,60505-0184-00 ",.02)
 ;;60505-0184-00
 ;;9002226.02101,"635,60505-0184-01 ",.01)
 ;;60505-0184-01
 ;;9002226.02101,"635,60505-0184-01 ",.02)
 ;;60505-0184-01
 ;;9002226.02101,"635,60505-0185-00 ",.01)
 ;;60505-0185-00
 ;;9002226.02101,"635,60505-0185-00 ",.02)
 ;;60505-0185-00
 ;;9002226.02101,"635,60505-0185-01 ",.01)
 ;;60505-0185-01
 ;;9002226.02101,"635,60505-0185-01 ",.02)
 ;;60505-0185-01
 ;;9002226.02101,"635,60505-0185-09 ",.01)
 ;;60505-0185-09
 ;;9002226.02101,"635,60505-0185-09 ",.02)
 ;;60505-0185-09
 ;;9002226.02101,"635,60505-0186-00 ",.01)
 ;;60505-0186-00
 ;;9002226.02101,"635,60505-0186-00 ",.02)
 ;;60505-0186-00
 ;;9002226.02101,"635,60505-0186-01 ",.01)
 ;;60505-0186-01
 ;;9002226.02101,"635,60505-0186-01 ",.02)
 ;;60505-0186-01
 ;;9002226.02101,"635,60505-0186-09 ",.01)
 ;;60505-0186-09
 ;;9002226.02101,"635,60505-0186-09 ",.02)
 ;;60505-0186-09
 ;;9002226.02101,"635,60505-0187-00 ",.01)
 ;;60505-0187-00
 ;;9002226.02101,"635,60505-0187-00 ",.02)
 ;;60505-0187-00
 ;;9002226.02101,"635,60505-0187-01 ",.01)
 ;;60505-0187-01
 ;;9002226.02101,"635,60505-0187-01 ",.02)
 ;;60505-0187-01
 ;;9002226.02101,"635,60505-0187-09 ",.01)
 ;;60505-0187-09
 ;;9002226.02101,"635,60505-0187-09 ",.02)
 ;;60505-0187-09
 ;;9002226.02101,"635,60505-0188-00 ",.01)
 ;;60505-0188-00
 ;;9002226.02101,"635,60505-0188-00 ",.02)
 ;;60505-0188-00
 ;;9002226.02101,"635,60505-0188-01 ",.01)
 ;;60505-0188-01
 ;;9002226.02101,"635,60505-0188-01 ",.02)
 ;;60505-0188-01
 ;;9002226.02101,"635,60505-0189-00 ",.01)
 ;;60505-0189-00
 ;;9002226.02101,"635,60505-0189-00 ",.02)
 ;;60505-0189-00
 ;;9002226.02101,"635,60505-0189-01 ",.01)
 ;;60505-0189-01
 ;;9002226.02101,"635,60505-0189-01 ",.02)
 ;;60505-0189-01
 ;;9002226.02101,"635,60505-0189-09 ",.01)
 ;;60505-0189-09
 ;;9002226.02101,"635,60505-0189-09 ",.02)
 ;;60505-0189-09
 ;;9002226.02101,"635,60505-0205-03 ",.01)
 ;;60505-0205-03
 ;;9002226.02101,"635,60505-0205-03 ",.02)
 ;;60505-0205-03
 ;;9002226.02101,"635,60505-0206-03 ",.01)
 ;;60505-0206-03
 ;;9002226.02101,"635,60505-0206-03 ",.02)
 ;;60505-0206-03
 ;;9002226.02101,"635,60505-0207-03 ",.01)
 ;;60505-0207-03
