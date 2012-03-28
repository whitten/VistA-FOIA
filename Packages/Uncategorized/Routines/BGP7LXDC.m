BGP7LXDC ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON AUG 28, 2006 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"618,54569-1896-03 ",.02)
 ;;54569-1896-03
 ;;9002226.02101,"618,54569-1897-00 ",.01)
 ;;54569-1897-00
 ;;9002226.02101,"618,54569-1897-00 ",.02)
 ;;54569-1897-00
 ;;9002226.02101,"618,54569-1897-01 ",.01)
 ;;54569-1897-01
 ;;9002226.02101,"618,54569-1897-01 ",.02)
 ;;54569-1897-01
 ;;9002226.02101,"618,54569-1906-01 ",.01)
 ;;54569-1906-01
 ;;9002226.02101,"618,54569-1906-01 ",.02)
 ;;54569-1906-01
 ;;9002226.02101,"618,54569-3407-00 ",.01)
 ;;54569-3407-00
 ;;9002226.02101,"618,54569-3407-00 ",.02)
 ;;54569-3407-00
 ;;9002226.02101,"618,54569-5335-00 ",.01)
 ;;54569-5335-00
 ;;9002226.02101,"618,54569-5335-00 ",.02)
 ;;54569-5335-00
 ;;9002226.02101,"618,54868-0340-00 ",.01)
 ;;54868-0340-00
 ;;9002226.02101,"618,54868-0340-00 ",.02)
 ;;54868-0340-00
 ;;9002226.02101,"618,54868-0340-01 ",.01)
 ;;54868-0340-01
 ;;9002226.02101,"618,54868-0340-01 ",.02)
 ;;54868-0340-01
 ;;9002226.02101,"618,54868-0608-00 ",.01)
 ;;54868-0608-00
 ;;9002226.02101,"618,54868-0608-00 ",.02)
 ;;54868-0608-00
 ;;9002226.02101,"618,54868-0756-00 ",.01)
 ;;54868-0756-00
 ;;9002226.02101,"618,54868-0756-00 ",.02)
 ;;54868-0756-00
 ;;9002226.02101,"618,54868-1485-01 ",.01)
 ;;54868-1485-01
 ;;9002226.02101,"618,54868-1485-01 ",.02)
 ;;54868-1485-01
 ;;9002226.02101,"618,54868-2973-00 ",.01)
 ;;54868-2973-00
 ;;9002226.02101,"618,54868-2973-00 ",.02)
 ;;54868-2973-00
 ;;9002226.02101,"618,54868-2973-02 ",.01)
 ;;54868-2973-02
 ;;9002226.02101,"618,54868-2973-02 ",.02)
 ;;54868-2973-02
 ;;9002226.02101,"618,54868-2973-03 ",.01)
 ;;54868-2973-03
 ;;9002226.02101,"618,54868-2973-03 ",.02)
 ;;54868-2973-03
 ;;9002226.02101,"618,54868-3487-00 ",.01)
 ;;54868-3487-00
 ;;9002226.02101,"618,54868-3487-00 ",.02)
 ;;54868-3487-00
 ;;9002226.02101,"618,54868-3487-01 ",.01)
 ;;54868-3487-01
 ;;9002226.02101,"618,54868-3487-01 ",.02)
 ;;54868-3487-01
 ;;9002226.02101,"618,54868-3487-02 ",.01)
 ;;54868-3487-02
 ;;9002226.02101,"618,54868-3487-02 ",.02)
 ;;54868-3487-02
 ;;9002226.02101,"618,54868-4893-00 ",.01)
 ;;54868-4893-00
 ;;9002226.02101,"618,54868-4893-00 ",.02)
 ;;54868-4893-00
 ;;9002226.02101,"618,54868-4893-01 ",.01)
 ;;54868-4893-01
 ;;9002226.02101,"618,54868-4893-01 ",.02)
 ;;54868-4893-01
 ;;9002226.02101,"618,54868-5181-00 ",.01)
 ;;54868-5181-00
 ;;9002226.02101,"618,54868-5181-00 ",.02)
 ;;54868-5181-00
 ;;9002226.02101,"618,55053-0444-01 ",.01)
 ;;55053-0444-01
 ;;9002226.02101,"618,55053-0444-01 ",.02)
 ;;55053-0444-01
 ;;9002226.02101,"618,55289-0219-20 ",.01)
 ;;55289-0219-20
 ;;9002226.02101,"618,55289-0219-20 ",.02)
 ;;55289-0219-20
 ;;9002226.02101,"618,55289-0428-10 ",.01)
 ;;55289-0428-10
 ;;9002226.02101,"618,55289-0428-10 ",.02)
 ;;55289-0428-10
 ;;9002226.02101,"618,55289-0428-17 ",.01)
 ;;55289-0428-17
 ;;9002226.02101,"618,55289-0428-17 ",.02)
 ;;55289-0428-17
 ;;9002226.02101,"618,55289-0529-05 ",.01)
 ;;55289-0529-05
 ;;9002226.02101,"618,55289-0529-05 ",.02)
 ;;55289-0529-05
 ;;9002226.02101,"618,55887-0741-20 ",.01)
 ;;55887-0741-20
 ;;9002226.02101,"618,55887-0741-20 ",.02)
 ;;55887-0741-20
 ;;9002226.02101,"618,58016-0721-10 ",.01)
 ;;58016-0721-10
 ;;9002226.02101,"618,58016-0721-10 ",.02)
 ;;58016-0721-10
 ;;9002226.02101,"618,58016-0721-15 ",.01)
 ;;58016-0721-15
 ;;9002226.02101,"618,58016-0721-15 ",.02)
 ;;58016-0721-15
 ;;9002226.02101,"618,58016-0973-00 ",.01)
 ;;58016-0973-00
 ;;9002226.02101,"618,58016-0973-00 ",.02)
 ;;58016-0973-00
 ;;9002226.02101,"618,58016-0973-02 ",.01)
 ;;58016-0973-02
 ;;9002226.02101,"618,58016-0973-02 ",.02)
 ;;58016-0973-02
 ;;9002226.02101,"618,58016-0973-03 ",.01)
 ;;58016-0973-03
 ;;9002226.02101,"618,58016-0973-03 ",.02)
 ;;58016-0973-03
 ;;9002226.02101,"618,58016-0973-08 ",.01)
 ;;58016-0973-08
 ;;9002226.02101,"618,58016-0973-08 ",.02)
 ;;58016-0973-08
 ;;9002226.02101,"618,58016-0973-10 ",.01)
 ;;58016-0973-10
 ;;9002226.02101,"618,58016-0973-10 ",.02)
 ;;58016-0973-10
 ;;9002226.02101,"618,58016-0973-12 ",.01)
 ;;58016-0973-12
 ;;9002226.02101,"618,58016-0973-12 ",.02)
 ;;58016-0973-12
 ;;9002226.02101,"618,58016-0973-15 ",.01)
 ;;58016-0973-15
 ;;9002226.02101,"618,58016-0973-15 ",.02)
 ;;58016-0973-15
 ;;9002226.02101,"618,58016-0973-20 ",.01)
 ;;58016-0973-20
 ;;9002226.02101,"618,58016-0973-20 ",.02)
 ;;58016-0973-20
 ;;9002226.02101,"618,58016-0973-24 ",.01)
 ;;58016-0973-24
 ;;9002226.02101,"618,58016-0973-24 ",.02)
 ;;58016-0973-24
 ;;9002226.02101,"618,58016-0973-30 ",.01)
 ;;58016-0973-30
 ;;9002226.02101,"618,58016-0973-30 ",.02)
 ;;58016-0973-30
 ;;9002226.02101,"618,58016-0973-50 ",.01)
 ;;58016-0973-50
 ;;9002226.02101,"618,58016-0973-50 ",.02)
 ;;58016-0973-50
 ;;9002226.02101,"618,58016-0973-60 ",.01)
 ;;58016-0973-60
 ;;9002226.02101,"618,58016-0973-60 ",.02)
 ;;58016-0973-60
 ;;9002226.02101,"618,58016-0973-73 ",.01)
 ;;58016-0973-73
 ;;9002226.02101,"618,58016-0973-73 ",.02)
 ;;58016-0973-73
 ;;9002226.02101,"618,58016-0973-89 ",.01)
 ;;58016-0973-89
 ;;9002226.02101,"618,58016-0973-89 ",.02)
 ;;58016-0973-89
 ;;9002226.02101,"618,58016-0973-90 ",.01)
 ;;58016-0973-90
 ;;9002226.02101,"618,58016-0973-90 ",.02)
 ;;58016-0973-90
 ;;9002226.02101,"618,58016-3091-01 ",.01)
 ;;58016-3091-01
 ;;9002226.02101,"618,58016-3091-01 ",.02)
 ;;58016-3091-01
 ;;9002226.02101,"618,58016-3092-01 ",.01)
 ;;58016-3092-01
 ;;9002226.02101,"618,58016-3092-01 ",.02)
 ;;58016-3092-01
 ;;9002226.02101,"618,58016-3092-10 ",.01)
 ;;58016-3092-10
 ;;9002226.02101,"618,58016-3092-10 ",.02)
 ;;58016-3092-10
 ;;9002226.02101,"618,58177-0037-04 ",.01)
 ;;58177-0037-04
 ;;9002226.02101,"618,58177-0037-04 ",.02)
 ;;58177-0037-04
 ;;9002226.02101,"618,58298-0140-10 ",.01)
 ;;58298-0140-10
 ;;9002226.02101,"618,58298-0140-10 ",.02)
 ;;58298-0140-10
 ;;9002226.02101,"618,58298-0145-10 ",.01)
 ;;58298-0145-10
 ;;9002226.02101,"618,58298-0145-10 ",.02)
 ;;58298-0145-10
 ;;9002226.02101,"618,58298-0145-50 ",.01)
 ;;58298-0145-50
 ;;9002226.02101,"618,58298-0145-50 ",.02)
 ;;58298-0145-50
 ;;9002226.02101,"618,59010-0340-10 ",.01)
 ;;59010-0340-10
 ;;9002226.02101,"618,59010-0340-10 ",.02)
 ;;59010-0340-10
 ;;9002226.02101,"618,59010-0340-50 ",.01)
 ;;59010-0340-50
 ;;9002226.02101,"618,59010-0340-50 ",.02)
 ;;59010-0340-50
 ;;9002226.02101,"618,59741-0304-09 ",.01)
 ;;59741-0304-09
 ;;9002226.02101,"618,59741-0304-09 ",.02)
 ;;59741-0304-09
 ;;9002226.02101,"618,59741-0304-49 ",.01)
 ;;59741-0304-49
 ;;9002226.02101,"618,59741-0304-49 ",.02)
 ;;59741-0304-49
 ;;9002226.02101,"618,59741-0304-50 ",.01)
 ;;59741-0304-50
 ;;9002226.02101,"618,59741-0304-50 ",.02)
 ;;59741-0304-50
 ;;9002226.02101,"618,59741-0305-09 ",.01)
 ;;59741-0305-09
 ;;9002226.02101,"618,59741-0305-09 ",.02)
 ;;59741-0305-09
 ;;9002226.02101,"618,59741-0305-49 ",.01)
 ;;59741-0305-49
 ;;9002226.02101,"618,59741-0305-49 ",.02)
 ;;59741-0305-49
 ;;9002226.02101,"618,59741-0305-50 ",.01)
 ;;59741-0305-50
 ;;9002226.02101,"618,59741-0305-50 ",.02)
 ;;59741-0305-50
 ;;9002226.02101,"618,59879-0115-01 ",.01)
 ;;59879-0115-01
 ;;9002226.02101,"618,59879-0115-01 ",.02)
 ;;59879-0115-01
 ;;9002226.02101,"618,61570-0079-01 ",.01)
 ;;61570-0079-01
 ;;9002226.02101,"618,61570-0079-01 ",.02)
 ;;61570-0079-01
 ;;9002226.02101,"618,61570-0186-01 ",.01)
 ;;61570-0186-01
 ;;9002226.02101,"618,61570-0186-01 ",.02)
 ;;61570-0186-01
 ;;9002226.02101,"618,61570-0187-01 ",.01)
 ;;61570-0187-01
 ;;9002226.02101,"618,61570-0187-01 ",.02)
 ;;61570-0187-01
 ;;9002226.02101,"618,61570-0187-05 ",.01)
 ;;61570-0187-05
 ;;9002226.02101,"618,61570-0187-05 ",.02)
 ;;61570-0187-05
 ;;9002226.02101,"618,61570-0503-10 ",.01)
 ;;61570-0503-10
 ;;9002226.02101,"618,61570-0503-10 ",.02)
 ;;61570-0503-10
 ;;9002226.02101,"618,61570-0504-10 ",.01)
 ;;61570-0504-10
 ;;9002226.02101,"618,61570-0504-10 ",.02)
 ;;61570-0504-10
 ;;9002226.02101,"618,61570-0504-50 ",.01)
 ;;61570-0504-50
 ;;9002226.02101,"618,61570-0504-50 ",.02)
 ;;61570-0504-50
 ;;9002226.02101,"618,61570-0540-02 ",.01)
 ;;61570-0540-02
 ;;9002226.02101,"618,61570-0540-02 ",.02)
 ;;61570-0540-02
 ;;9002226.02101,"618,61570-0541-20 ",.01)
 ;;61570-0541-20
 ;;9002226.02101,"618,61570-0541-20 ",.02)
 ;;61570-0541-20
 ;;9002226.02101,"618,61646-0308-01 ",.01)
 ;;61646-0308-01
 ;;9002226.02101,"618,61646-0308-01 ",.02)
 ;;61646-0308-01
 ;;9002226.02101,"618,66267-0208-10 ",.01)
 ;;66267-0208-10
 ;;9002226.02101,"618,66267-0208-10 ",.02)
 ;;66267-0208-10
 ;;9002226.02101,"618,66267-0208-20 ",.01)
 ;;66267-0208-20
 ;;9002226.02101,"618,66267-0208-20 ",.02)
 ;;66267-0208-20
 ;;9002226.02101,"618,68115-0337-05 ",.01)
 ;;68115-0337-05
 ;;9002226.02101,"618,68115-0337-05 ",.02)
 ;;68115-0337-05
 ;;9002226.02101,"618,68115-0337-10 ",.01)
 ;;68115-0337-10
 ;;9002226.02101,"618,68115-0337-10 ",.02)
 ;;68115-0337-10
 ;;9002226.02101,"618,68115-0338-05 ",.01)
 ;;68115-0338-05
 ;;9002226.02101,"618,68115-0338-05 ",.02)
 ;;68115-0338-05
 ;;9002226.02101,"618,68115-0338-10 ",.01)
 ;;68115-0338-10
 ;;9002226.02101,"618,68115-0338-10 ",.02)
 ;;68115-0338-10
 ;;9002226.02101,"618,68115-0339-20 ",.01)
 ;;68115-0339-20
 ;;9002226.02101,"618,68115-0339-20 ",.02)
 ;;68115-0339-20
 ;;9002226.02101,"618,68115-0588-00 ",.01)
 ;;68115-0588-00
 ;;9002226.02101,"618,68115-0588-00 ",.02)
 ;;68115-0588-00
