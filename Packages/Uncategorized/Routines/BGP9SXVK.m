BGP9SXVK ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON MAR 25, 2009 ;
 ;;9.0;IHS CLINICAL REPORTING;;JUL 1, 2009
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"747,00456-0672-99 ",.02)
 ;;00456-0672-99
 ;;9002226.02101,"747,00456-3581-01 ",.01)
 ;;00456-3581-01
 ;;9002226.02101,"747,00456-3581-01 ",.02)
 ;;00456-3581-01
 ;;9002226.02101,"747,00456-3581-05 ",.01)
 ;;00456-3581-05
 ;;9002226.02101,"747,00456-3581-05 ",.02)
 ;;00456-3581-05
 ;;9002226.02101,"747,00456-3581-10 ",.01)
 ;;00456-3581-10
 ;;9002226.02101,"747,00456-3581-10 ",.02)
 ;;00456-3581-10
 ;;9002226.02101,"747,00456-4301-01 ",.01)
 ;;00456-4301-01
 ;;9002226.02101,"747,00456-4301-01 ",.02)
 ;;00456-4301-01
 ;;9002226.02101,"747,00456-4302-01 ",.01)
 ;;00456-4302-01
 ;;9002226.02101,"747,00456-4302-01 ",.02)
 ;;00456-4302-01
 ;;9002226.02101,"747,00456-4303-01 ",.01)
 ;;00456-4303-01
 ;;9002226.02101,"747,00456-4303-01 ",.02)
 ;;00456-4303-01
 ;;9002226.02101,"747,00456-4310-01 ",.01)
 ;;00456-4310-01
 ;;9002226.02101,"747,00456-4310-01 ",.02)
 ;;00456-4310-01
 ;;9002226.02101,"747,00456-4310-02 ",.01)
 ;;00456-4310-02
 ;;9002226.02101,"747,00456-4310-02 ",.02)
 ;;00456-4310-02
 ;;9002226.02101,"747,00456-4320-00 ",.01)
 ;;00456-4320-00
 ;;9002226.02101,"747,00456-4320-00 ",.02)
 ;;00456-4320-00
 ;;9002226.02101,"747,00456-4320-01 ",.01)
 ;;00456-4320-01
 ;;9002226.02101,"747,00456-4320-01 ",.02)
 ;;00456-4320-01
 ;;9002226.02101,"747,00456-4320-02 ",.01)
 ;;00456-4320-02
 ;;9002226.02101,"747,00456-4320-02 ",.02)
 ;;00456-4320-02
 ;;9002226.02101,"747,00456-4330-00 ",.01)
 ;;00456-4330-00
 ;;9002226.02101,"747,00456-4330-00 ",.02)
 ;;00456-4330-00
 ;;9002226.02101,"747,00456-4330-01 ",.01)
 ;;00456-4330-01
 ;;9002226.02101,"747,00456-4330-01 ",.02)
 ;;00456-4330-01
 ;;9002226.02101,"747,00456-4330-02 ",.01)
 ;;00456-4330-02
 ;;9002226.02101,"747,00456-4330-02 ",.02)
 ;;00456-4330-02
 ;;9002226.02101,"747,00456-4345-01 ",.01)
 ;;00456-4345-01
 ;;9002226.02101,"747,00456-4345-01 ",.02)
 ;;00456-4345-01
 ;;9002226.02101,"747,00463-9031-16 ",.01)
 ;;00463-9031-16
 ;;9002226.02101,"747,00463-9031-16 ",.02)
 ;;00463-9031-16
 ;;9002226.02101,"747,00472-0750-21 ",.01)
 ;;00472-0750-21
 ;;9002226.02101,"747,00472-0750-21 ",.02)
 ;;00472-0750-21
 ;;9002226.02101,"747,00472-0750-60 ",.01)
 ;;00472-0750-60
 ;;9002226.02101,"747,00472-0750-60 ",.02)
 ;;00472-0750-60
 ;;9002226.02101,"747,00472-0752-21 ",.01)
 ;;00472-0752-21
 ;;9002226.02101,"747,00472-0752-21 ",.02)
 ;;00472-0752-21
 ;;9002226.02101,"747,00472-0752-60 ",.01)
 ;;00472-0752-60
 ;;9002226.02101,"747,00472-0752-60 ",.02)
 ;;00472-0752-60
 ;;9002226.02101,"747,00472-0873-08 ",.01)
 ;;00472-0873-08
 ;;9002226.02101,"747,00472-0873-08 ",.02)
 ;;00472-0873-08
 ;;9002226.02101,"747,00472-1238-16 ",.01)
 ;;00472-1238-16
 ;;9002226.02101,"747,00472-1238-16 ",.02)
 ;;00472-1238-16
 ;;9002226.02101,"747,00472-1444-28 ",.01)
 ;;00472-1444-28
 ;;9002226.02101,"747,00472-1444-28 ",.02)
 ;;00472-1444-28
 ;;9002226.02101,"747,00485-0059-16 ",.01)
 ;;00485-0059-16
 ;;9002226.02101,"747,00485-0059-16 ",.02)
 ;;00485-0059-16
 ;;9002226.02101,"747,00490-0080-00 ",.01)
 ;;00490-0080-00
 ;;9002226.02101,"747,00490-0080-00 ",.02)
 ;;00490-0080-00
 ;;9002226.02101,"747,00490-0080-30 ",.01)
 ;;00490-0080-30
 ;;9002226.02101,"747,00490-0080-30 ",.02)
 ;;00490-0080-30
 ;;9002226.02101,"747,00490-0080-60 ",.01)
 ;;00490-0080-60
 ;;9002226.02101,"747,00490-0080-60 ",.02)
 ;;00490-0080-60
 ;;9002226.02101,"747,00490-0080-90 ",.01)
 ;;00490-0080-90
 ;;9002226.02101,"747,00490-0080-90 ",.02)
 ;;00490-0080-90
 ;;9002226.02101,"747,00525-0305-01 ",.01)
 ;;00525-0305-01
 ;;9002226.02101,"747,00525-0305-01 ",.02)
 ;;00525-0305-01
 ;;9002226.02101,"747,00525-0376-16 ",.01)
 ;;00525-0376-16
 ;;9002226.02101,"747,00525-0376-16 ",.02)
 ;;00525-0376-16
 ;;9002226.02101,"747,00536-4652-05 ",.01)
 ;;00536-4652-05
 ;;9002226.02101,"747,00536-4652-05 ",.02)
 ;;00536-4652-05
 ;;9002226.02101,"747,00551-0123-01 ",.01)
 ;;00551-0123-01
 ;;9002226.02101,"747,00551-0123-01 ",.02)
 ;;00551-0123-01
 ;;9002226.02101,"747,00551-0123-02 ",.01)
 ;;00551-0123-02
 ;;9002226.02101,"747,00551-0123-02 ",.02)
 ;;00551-0123-02
 ;;9002226.02101,"747,00551-0124-01 ",.01)
 ;;00551-0124-01
 ;;9002226.02101,"747,00551-0124-01 ",.02)
 ;;00551-0124-01
 ;;9002226.02101,"747,00551-0124-02 ",.01)
 ;;00551-0124-02
 ;;9002226.02101,"747,00551-0124-02 ",.02)
 ;;00551-0124-02
 ;;9002226.02101,"747,00551-0205-01 ",.01)
 ;;00551-0205-01
 ;;9002226.02101,"747,00551-0205-01 ",.02)
 ;;00551-0205-01
 ;;9002226.02101,"747,00556-0149-16 ",.01)
 ;;00556-0149-16
 ;;9002226.02101,"747,00556-0149-16 ",.02)
 ;;00556-0149-16
 ;;9002226.02101,"747,00556-0149-28 ",.01)
 ;;00556-0149-28
 ;;9002226.02101,"747,00556-0149-28 ",.02)
 ;;00556-0149-28
 ;;9002226.02101,"747,00585-0673-02 ",.01)
 ;;00585-0673-02
 ;;9002226.02101,"747,00585-0673-02 ",.02)
 ;;00585-0673-02
 ;;9002226.02101,"747,00585-0673-03 ",.01)
 ;;00585-0673-03
 ;;9002226.02101,"747,00585-0673-03 ",.02)
 ;;00585-0673-03
 ;;9002226.02101,"747,00585-0675-01 ",.01)
 ;;00585-0675-01
 ;;9002226.02101,"747,00585-0675-01 ",.02)
 ;;00585-0675-01
 ;;9002226.02101,"747,00585-0675-02 ",.01)
 ;;00585-0675-02
 ;;9002226.02101,"747,00585-0675-02 ",.02)
 ;;00585-0675-02
 ;;9002226.02101,"747,00585-0685-02 ",.01)
 ;;00585-0685-02
