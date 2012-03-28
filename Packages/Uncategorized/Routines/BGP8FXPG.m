BGP8FXPG ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 28, 2007 ;
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
 ;;9002226.02101,"657,60346-0282-74 ",.01)
 ;;60346-0282-74
 ;;9002226.02101,"657,60346-0282-74 ",.02)
 ;;60346-0282-74
 ;;9002226.02101,"657,60432-0019-16 ",.01)
 ;;60432-0019-16
 ;;9002226.02101,"657,60432-0019-16 ",.02)
 ;;60432-0019-16
 ;;9002226.02101,"657,60432-0157-06 ",.01)
 ;;60432-0157-06
 ;;9002226.02101,"657,60432-0157-06 ",.02)
 ;;60432-0157-06
 ;;9002226.02101,"657,60432-0157-21 ",.01)
 ;;60432-0157-21
 ;;9002226.02101,"657,60432-0157-21 ",.02)
 ;;60432-0157-21
 ;;9002226.02101,"657,60505-0802-01 ",.01)
 ;;60505-0802-01
 ;;9002226.02101,"657,60505-0802-01 ",.02)
 ;;60505-0802-01
 ;;9002226.02101,"657,60505-0802-02 ",.01)
 ;;60505-0802-02
 ;;9002226.02101,"657,60505-0802-02 ",.02)
 ;;60505-0802-02
 ;;9002226.02101,"657,60598-0061-60 ",.01)
 ;;60598-0061-60
 ;;9002226.02101,"657,60598-0061-60 ",.02)
 ;;60598-0061-60
 ;;9002226.02101,"657,60793-0010-12 ",.01)
 ;;60793-0010-12
 ;;9002226.02101,"657,60793-0010-12 ",.02)
 ;;60793-0010-12
 ;;9002226.02101,"657,60793-0011-08 ",.01)
 ;;60793-0011-08
 ;;9002226.02101,"657,60793-0011-08 ",.02)
 ;;60793-0011-08
 ;;9002226.02101,"657,60793-0011-14 ",.01)
 ;;60793-0011-14
 ;;9002226.02101,"657,60793-0011-14 ",.02)
 ;;60793-0011-14
 ;;9002226.02101,"657,60793-0120-01 ",.01)
 ;;60793-0120-01
 ;;9002226.02101,"657,60793-0120-01 ",.02)
 ;;60793-0120-01
 ;;9002226.02101,"657,61392-0016-30 ",.01)
 ;;61392-0016-30
 ;;9002226.02101,"657,61392-0016-30 ",.02)
 ;;61392-0016-30
 ;;9002226.02101,"657,61392-0016-45 ",.01)
 ;;61392-0016-45
 ;;9002226.02101,"657,61392-0016-45 ",.02)
 ;;61392-0016-45
 ;;9002226.02101,"657,61392-0016-51 ",.01)
 ;;61392-0016-51
 ;;9002226.02101,"657,61392-0016-51 ",.02)
 ;;61392-0016-51
 ;;9002226.02101,"657,61392-0016-54 ",.01)
 ;;61392-0016-54
 ;;9002226.02101,"657,61392-0016-54 ",.02)
 ;;61392-0016-54
 ;;9002226.02101,"657,61392-0016-56 ",.01)
 ;;61392-0016-56
 ;;9002226.02101,"657,61392-0016-56 ",.02)
 ;;61392-0016-56
 ;;9002226.02101,"657,61392-0016-60 ",.01)
 ;;61392-0016-60
 ;;9002226.02101,"657,61392-0016-60 ",.02)
 ;;61392-0016-60
 ;;9002226.02101,"657,61392-0016-90 ",.01)
 ;;61392-0016-90
 ;;9002226.02101,"657,61392-0016-90 ",.02)
 ;;61392-0016-90
 ;;9002226.02101,"657,61392-0016-91 ",.01)
 ;;61392-0016-91
 ;;9002226.02101,"657,61392-0016-91 ",.02)
 ;;61392-0016-91
 ;;9002226.02101,"657,61392-0017-30 ",.01)
 ;;61392-0017-30
 ;;9002226.02101,"657,61392-0017-30 ",.02)
 ;;61392-0017-30
 ;;9002226.02101,"657,61392-0017-51 ",.01)
 ;;61392-0017-51
 ;;9002226.02101,"657,61392-0017-51 ",.02)
 ;;61392-0017-51
 ;;9002226.02101,"657,61392-0017-54 ",.01)
 ;;61392-0017-54
 ;;9002226.02101,"657,61392-0017-54 ",.02)
 ;;61392-0017-54
 ;;9002226.02101,"657,61392-0017-56 ",.01)
 ;;61392-0017-56
 ;;9002226.02101,"657,61392-0017-56 ",.02)
 ;;61392-0017-56
 ;;9002226.02101,"657,61392-0017-60 ",.01)
 ;;61392-0017-60
 ;;9002226.02101,"657,61392-0017-60 ",.02)
 ;;61392-0017-60
 ;;9002226.02101,"657,61392-0017-90 ",.01)
 ;;61392-0017-90
 ;;9002226.02101,"657,61392-0017-90 ",.02)
 ;;61392-0017-90
 ;;9002226.02101,"657,61392-0017-91 ",.01)
 ;;61392-0017-91
 ;;9002226.02101,"657,61392-0017-91 ",.02)
 ;;61392-0017-91
 ;;9002226.02101,"657,61570-0019-01 ",.01)
 ;;61570-0019-01
 ;;9002226.02101,"657,61570-0019-01 ",.02)
 ;;61570-0019-01
 ;;9002226.02101,"657,61570-0019-05 ",.01)
 ;;61570-0019-05
 ;;9002226.02101,"657,61570-0019-05 ",.02)
 ;;61570-0019-05
 ;;9002226.02101,"657,61570-0020-01 ",.01)
 ;;61570-0020-01
 ;;9002226.02101,"657,61570-0020-01 ",.02)
 ;;61570-0020-01
 ;;9002226.02101,"657,61570-0022-01 ",.01)
 ;;61570-0022-01
 ;;9002226.02101,"657,61570-0022-01 ",.02)
 ;;61570-0022-01
 ;;9002226.02101,"657,63874-0443-01 ",.01)
 ;;63874-0443-01
 ;;9002226.02101,"657,63874-0443-01 ",.02)
 ;;63874-0443-01
 ;;9002226.02101,"657,63874-0443-15 ",.01)
 ;;63874-0443-15
 ;;9002226.02101,"657,63874-0443-15 ",.02)
 ;;63874-0443-15
 ;;9002226.02101,"657,63874-0443-20 ",.01)
 ;;63874-0443-20
 ;;9002226.02101,"657,63874-0443-20 ",.02)
 ;;63874-0443-20
 ;;9002226.02101,"657,63874-0443-30 ",.01)
 ;;63874-0443-30
 ;;9002226.02101,"657,63874-0443-30 ",.02)
 ;;63874-0443-30
 ;;9002226.02101,"657,63874-0447-01 ",.01)
 ;;63874-0447-01
 ;;9002226.02101,"657,63874-0447-01 ",.02)
 ;;63874-0447-01
 ;;9002226.02101,"657,63874-0447-15 ",.01)
 ;;63874-0447-15
 ;;9002226.02101,"657,63874-0447-15 ",.02)
 ;;63874-0447-15
 ;;9002226.02101,"657,63874-0447-20 ",.01)
 ;;63874-0447-20
 ;;9002226.02101,"657,63874-0447-20 ",.02)
 ;;63874-0447-20
 ;;9002226.02101,"657,63874-0447-30 ",.01)
 ;;63874-0447-30
 ;;9002226.02101,"657,63874-0447-30 ",.02)
 ;;63874-0447-30
 ;;9002226.02101,"657,63874-0447-60 ",.01)
 ;;63874-0447-60
 ;;9002226.02101,"657,63874-0447-60 ",.02)
 ;;63874-0447-60
 ;;9002226.02101,"657,63874-0675-01 ",.01)
 ;;63874-0675-01
 ;;9002226.02101,"657,63874-0675-01 ",.02)
 ;;63874-0675-01
 ;;9002226.02101,"657,63874-0675-15 ",.01)
 ;;63874-0675-15
 ;;9002226.02101,"657,63874-0675-15 ",.02)
 ;;63874-0675-15
 ;;9002226.02101,"657,63874-0675-20 ",.01)
 ;;63874-0675-20
 ;;9002226.02101,"657,63874-0675-20 ",.02)
 ;;63874-0675-20
 ;;9002226.02101,"657,63874-0675-30 ",.01)
 ;;63874-0675-30
 ;;9002226.02101,"657,63874-0675-30 ",.02)
 ;;63874-0675-30
 ;;9002226.02101,"657,63874-0714-20 ",.01)
 ;;63874-0714-20
 ;;9002226.02101,"657,63874-0714-20 ",.02)
 ;;63874-0714-20
 ;;9002226.02101,"657,63874-0744-12 ",.01)
 ;;63874-0744-12
 ;;9002226.02101,"657,63874-0744-12 ",.02)
 ;;63874-0744-12
 ;;9002226.02101,"657,63874-0744-24 ",.01)
 ;;63874-0744-24
 ;;9002226.02101,"657,63874-0744-24 ",.02)
 ;;63874-0744-24
 ;;9002226.02101,"657,65162-0324-10 ",.01)
 ;;65162-0324-10
 ;;9002226.02101,"657,65162-0324-10 ",.02)
 ;;65162-0324-10
 ;;9002226.02101,"657,65162-0324-11 ",.01)
 ;;65162-0324-11
 ;;9002226.02101,"657,65162-0324-11 ",.02)
 ;;65162-0324-11
 ;;9002226.02101,"657,65162-0325-10 ",.01)
 ;;65162-0325-10
 ;;9002226.02101,"657,65162-0325-10 ",.02)
 ;;65162-0325-10
 ;;9002226.02101,"657,65162-0325-11 ",.01)
 ;;65162-0325-11
 ;;9002226.02101,"657,65162-0325-11 ",.02)
 ;;65162-0325-11
 ;;9002226.02101,"657,65162-0335-10 ",.01)
 ;;65162-0335-10
 ;;9002226.02101,"657,65162-0335-10 ",.02)
 ;;65162-0335-10
 ;;9002226.02101,"657,66239-0111-16 ",.01)
 ;;66239-0111-16
 ;;9002226.02101,"657,66239-0111-16 ",.02)
 ;;66239-0111-16
 ;;9002226.02101,"657,67781-0251-01 ",.01)
 ;;67781-0251-01
 ;;9002226.02101,"657,67781-0251-01 ",.02)
 ;;67781-0251-01
 ;;9002226.02101,"657,67781-0251-05 ",.01)
 ;;67781-0251-05
 ;;9002226.02101,"657,67781-0251-05 ",.02)
 ;;67781-0251-05
 ;;9002226.02101,"657,67781-0252-01 ",.01)
 ;;67781-0252-01
 ;;9002226.02101,"657,67781-0252-01 ",.02)
 ;;67781-0252-01
 ;;9002226.02101,"657,68115-0328-60 ",.01)
 ;;68115-0328-60
 ;;9002226.02101,"657,68115-0328-60 ",.02)
 ;;68115-0328-60
 ;;9002226.02101,"657,68115-0547-20 ",.01)
 ;;68115-0547-20
 ;;9002226.02101,"657,68115-0547-20 ",.02)
 ;;68115-0547-20
 ;;9002226.02101,"657,68115-0637-13 ",.01)
 ;;68115-0637-13
 ;;9002226.02101,"657,68115-0637-13 ",.02)
 ;;68115-0637-13
 ;;9002226.02101,"657,68115-0638-60 ",.01)
 ;;68115-0638-60
 ;;9002226.02101,"657,68115-0638-60 ",.02)
 ;;68115-0638-60
 ;;9002226.02101,"657,68115-0652-01 ",.01)
 ;;68115-0652-01
 ;;9002226.02101,"657,68115-0652-01 ",.02)
 ;;68115-0652-01
 ;;9002226.02101,"657,68115-0653-01 ",.01)
 ;;68115-0653-01
 ;;9002226.02101,"657,68115-0653-01 ",.02)
 ;;68115-0653-01
 ;;9002226.02101,"657,68115-0657-01 ",.01)
 ;;68115-0657-01
 ;;9002226.02101,"657,68115-0657-01 ",.02)
 ;;68115-0657-01
 ;;9002226.02101,"657,68115-0760-01 ",.01)
 ;;68115-0760-01
 ;;9002226.02101,"657,68115-0760-01 ",.02)
 ;;68115-0760-01
 ;;9002226.02101,"657,68115-0775-07 ",.01)
 ;;68115-0775-07
 ;;9002226.02101,"657,68115-0775-07 ",.02)
 ;;68115-0775-07
 ;;9002226.02101,"657,68115-0923-90 ",.01)
 ;;68115-0923-90
 ;;9002226.02101,"657,68115-0923-90 ",.02)
 ;;68115-0923-90
 ;;9002226.02101,"657,68115-0924-60 ",.01)
 ;;68115-0924-60
 ;;9002226.02101,"657,68115-0924-60 ",.02)
 ;;68115-0924-60
 ;;9002226.02101,"657,68734-0700-10 ",.01)
 ;;68734-0700-10
 ;;9002226.02101,"657,68734-0700-10 ",.02)
 ;;68734-0700-10
