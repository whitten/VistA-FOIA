BGPM5BCX ;IHS/MSC/SAT-CREATED BY ^ATXSTX ON SEP 12, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"1044,57315004402 ",.02)
 ;;57315004402
 ;;9002226.02101,"1044,57315004403 ",.01)
 ;;57315004403
 ;;9002226.02101,"1044,57315004403 ",.02)
 ;;57315004403
 ;;9002226.02101,"1044,57315004404 ",.01)
 ;;57315004404
 ;;9002226.02101,"1044,57315004404 ",.02)
 ;;57315004404
 ;;9002226.02101,"1044,57315004501 ",.01)
 ;;57315004501
 ;;9002226.02101,"1044,57315004501 ",.02)
 ;;57315004501
 ;;9002226.02101,"1044,57315004502 ",.01)
 ;;57315004502
 ;;9002226.02101,"1044,57315004502 ",.02)
 ;;57315004502
 ;;9002226.02101,"1044,57315004503 ",.01)
 ;;57315004503
 ;;9002226.02101,"1044,57315004503 ",.02)
 ;;57315004503
 ;;9002226.02101,"1044,57315004601 ",.01)
 ;;57315004601
 ;;9002226.02101,"1044,57315004601 ",.02)
 ;;57315004601
 ;;9002226.02101,"1044,57315004602 ",.01)
 ;;57315004602
 ;;9002226.02101,"1044,57315004602 ",.02)
 ;;57315004602
 ;;9002226.02101,"1044,57315004603 ",.01)
 ;;57315004603
 ;;9002226.02101,"1044,57315004603 ",.02)
 ;;57315004603
 ;;9002226.02101,"1044,57315005101 ",.01)
 ;;57315005101
 ;;9002226.02101,"1044,57315005101 ",.02)
 ;;57315005101
 ;;9002226.02101,"1044,57315005102 ",.01)
 ;;57315005102
 ;;9002226.02101,"1044,57315005102 ",.02)
 ;;57315005102
 ;;9002226.02101,"1044,57315005103 ",.01)
 ;;57315005103
 ;;9002226.02101,"1044,57315005103 ",.02)
 ;;57315005103
 ;;9002226.02101,"1044,57315005105 ",.01)
 ;;57315005105
 ;;9002226.02101,"1044,57315005105 ",.02)
 ;;57315005105
 ;;9002226.02101,"1044,57315005201 ",.01)
 ;;57315005201
 ;;9002226.02101,"1044,57315005201 ",.02)
 ;;57315005201
 ;;9002226.02101,"1044,57315005202 ",.01)
 ;;57315005202
 ;;9002226.02101,"1044,57315005202 ",.02)
 ;;57315005202
 ;;9002226.02101,"1044,57315005203 ",.01)
 ;;57315005203
 ;;9002226.02101,"1044,57315005203 ",.02)
 ;;57315005203
 ;;9002226.02101,"1044,57315005205 ",.01)
 ;;57315005205
 ;;9002226.02101,"1044,57315005205 ",.02)
 ;;57315005205
 ;;9002226.02101,"1044,57315006101 ",.01)
 ;;57315006101
 ;;9002226.02101,"1044,57315006101 ",.02)
 ;;57315006101
 ;;9002226.02101,"1044,57315006102 ",.01)
 ;;57315006102
 ;;9002226.02101,"1044,57315006102 ",.02)
 ;;57315006102
 ;;9002226.02101,"1044,57315006103 ",.01)
 ;;57315006103
 ;;9002226.02101,"1044,57315006103 ",.02)
 ;;57315006103
 ;;9002226.02101,"1044,57315006201 ",.01)
 ;;57315006201
 ;;9002226.02101,"1044,57315006201 ",.02)
 ;;57315006201
 ;;9002226.02101,"1044,57315006202 ",.01)
 ;;57315006202
 ;;9002226.02101,"1044,57315006202 ",.02)
 ;;57315006202
 ;;9002226.02101,"1044,57315006203 ",.01)
 ;;57315006203
 ;;9002226.02101,"1044,57315006203 ",.02)
 ;;57315006203
 ;;9002226.02101,"1044,57315006301 ",.01)
 ;;57315006301
 ;;9002226.02101,"1044,57315006301 ",.02)
 ;;57315006301
 ;;9002226.02101,"1044,57315006303 ",.01)
 ;;57315006303
 ;;9002226.02101,"1044,57315006303 ",.02)
 ;;57315006303
 ;;9002226.02101,"1044,57315007801 ",.01)
 ;;57315007801
 ;;9002226.02101,"1044,57315007801 ",.02)
 ;;57315007801
 ;;9002226.02101,"1044,57315007802 ",.01)
 ;;57315007802
 ;;9002226.02101,"1044,57315007802 ",.02)
 ;;57315007802
 ;;9002226.02101,"1044,57315007803 ",.01)
 ;;57315007803
 ;;9002226.02101,"1044,57315007803 ",.02)
 ;;57315007803
 ;;9002226.02101,"1044,57315007804 ",.01)
 ;;57315007804
 ;;9002226.02101,"1044,57315007804 ",.02)
 ;;57315007804
 ;;9002226.02101,"1044,57315007805 ",.01)
 ;;57315007805
 ;;9002226.02101,"1044,57315007805 ",.02)
 ;;57315007805
 ;;9002226.02101,"1044,57315007901 ",.01)
 ;;57315007901
 ;;9002226.02101,"1044,57315007901 ",.02)
 ;;57315007901
 ;;9002226.02101,"1044,57315007902 ",.01)
 ;;57315007902
 ;;9002226.02101,"1044,57315007902 ",.02)
 ;;57315007902
 ;;9002226.02101,"1044,57315007903 ",.01)
 ;;57315007903
 ;;9002226.02101,"1044,57315007903 ",.02)
 ;;57315007903
 ;;9002226.02101,"1044,57315007905 ",.01)
 ;;57315007905
 ;;9002226.02101,"1044,57315007905 ",.02)
 ;;57315007905
 ;;9002226.02101,"1044,57315008001 ",.01)
 ;;57315008001
 ;;9002226.02101,"1044,57315008001 ",.02)
 ;;57315008001
 ;;9002226.02101,"1044,57315008002 ",.01)
 ;;57315008002
 ;;9002226.02101,"1044,57315008002 ",.02)
 ;;57315008002
 ;;9002226.02101,"1044,57315008005 ",.01)
 ;;57315008005
 ;;9002226.02101,"1044,57315008005 ",.02)
 ;;57315008005
 ;;9002226.02101,"1044,57315008401 ",.01)
 ;;57315008401
 ;;9002226.02101,"1044,57315008401 ",.02)
 ;;57315008401
 ;;9002226.02101,"1044,57315008403 ",.01)
 ;;57315008403
 ;;9002226.02101,"1044,57315008403 ",.02)
 ;;57315008403
 ;;9002226.02101,"1044,57315008501 ",.01)
 ;;57315008501
 ;;9002226.02101,"1044,57315008501 ",.02)
 ;;57315008501
 ;;9002226.02101,"1044,57315008503 ",.01)
 ;;57315008503
 ;;9002226.02101,"1044,57315008503 ",.02)
 ;;57315008503
 ;;9002226.02101,"1044,57315008601 ",.01)
 ;;57315008601
 ;;9002226.02101,"1044,57315008601 ",.02)
 ;;57315008601
 ;;9002226.02101,"1044,57315008603 ",.01)
 ;;57315008603
 ;;9002226.02101,"1044,57315008603 ",.02)
 ;;57315008603
 ;;9002226.02101,"1044,57664028513 ",.01)
 ;;57664028513
 ;;9002226.02101,"1044,57664028513 ",.02)
 ;;57664028513
 ;;9002226.02101,"1044,57664028518 ",.01)
 ;;57664028518
 ;;9002226.02101,"1044,57664028518 ",.02)
 ;;57664028518
 ;;9002226.02101,"1044,57664028588 ",.01)
 ;;57664028588
 ;;9002226.02101,"1044,57664028588 ",.02)
 ;;57664028588
 ;;9002226.02101,"1044,57664028613 ",.01)
 ;;57664028613
 ;;9002226.02101,"1044,57664028613 ",.02)
 ;;57664028613
 ;;9002226.02101,"1044,57664028618 ",.01)
 ;;57664028618
 ;;9002226.02101,"1044,57664028618 ",.02)
 ;;57664028618
 ;;9002226.02101,"1044,57664028688 ",.01)
 ;;57664028688
 ;;9002226.02101,"1044,57664028688 ",.02)
 ;;57664028688
 ;;9002226.02101,"1044,57664028713 ",.01)
 ;;57664028713
 ;;9002226.02101,"1044,57664028713 ",.02)
 ;;57664028713
 ;;9002226.02101,"1044,57664028718 ",.01)
 ;;57664028718
 ;;9002226.02101,"1044,57664028718 ",.02)
 ;;57664028718
 ;;9002226.02101,"1044,57664028788 ",.01)
 ;;57664028788
 ;;9002226.02101,"1044,57664028788 ",.02)
 ;;57664028788
 ;;9002226.02101,"1044,57664028813 ",.01)
 ;;57664028813
 ;;9002226.02101,"1044,57664028813 ",.02)
 ;;57664028813
 ;;9002226.02101,"1044,57664028818 ",.01)
 ;;57664028818
 ;;9002226.02101,"1044,57664028818 ",.02)
 ;;57664028818
 ;;9002226.02101,"1044,57664028888 ",.01)
 ;;57664028888
 ;;9002226.02101,"1044,57664028888 ",.02)
 ;;57664028888
 ;;9002226.02101,"1044,57664028913 ",.01)
 ;;57664028913
 ;;9002226.02101,"1044,57664028913 ",.02)
 ;;57664028913
 ;;9002226.02101,"1044,57664028918 ",.01)
 ;;57664028918
 ;;9002226.02101,"1044,57664028918 ",.02)
 ;;57664028918
 ;;9002226.02101,"1044,57664028988 ",.01)
 ;;57664028988
 ;;9002226.02101,"1044,57664028988 ",.02)
 ;;57664028988
 ;;9002226.02101,"1044,57664029013 ",.01)
 ;;57664029013
 ;;9002226.02101,"1044,57664029013 ",.02)
 ;;57664029013
 ;;9002226.02101,"1044,57664029018 ",.01)
 ;;57664029018
 ;;9002226.02101,"1044,57664029018 ",.02)
 ;;57664029018
 ;;9002226.02101,"1044,57664029088 ",.01)
 ;;57664029088
 ;;9002226.02101,"1044,57664029088 ",.02)
 ;;57664029088
 ;;9002226.02101,"1044,57664035713 ",.01)
 ;;57664035713
 ;;9002226.02101,"1044,57664035713 ",.02)
 ;;57664035713
 ;;9002226.02101,"1044,57664035718 ",.01)
 ;;57664035718
 ;;9002226.02101,"1044,57664035718 ",.02)
 ;;57664035718
 ;;9002226.02101,"1044,57664035788 ",.01)
 ;;57664035788
 ;;9002226.02101,"1044,57664035788 ",.02)
 ;;57664035788
 ;;9002226.02101,"1044,57664036113 ",.01)
 ;;57664036113
 ;;9002226.02101,"1044,57664036113 ",.02)
 ;;57664036113
 ;;9002226.02101,"1044,57664036118 ",.01)
 ;;57664036118
 ;;9002226.02101,"1044,57664036118 ",.02)
 ;;57664036118
 ;;9002226.02101,"1044,57664036188 ",.01)
 ;;57664036188
 ;;9002226.02101,"1044,57664036188 ",.02)
 ;;57664036188
 ;;9002226.02101,"1044,57664036213 ",.01)
 ;;57664036213
 ;;9002226.02101,"1044,57664036213 ",.02)
 ;;57664036213
 ;;9002226.02101,"1044,57664036218 ",.01)
 ;;57664036218
 ;;9002226.02101,"1044,57664036218 ",.02)
 ;;57664036218
 ;;9002226.02101,"1044,57664036288 ",.01)
 ;;57664036288
 ;;9002226.02101,"1044,57664036288 ",.02)
 ;;57664036288
 ;;9002226.02101,"1044,57664039213 ",.01)
 ;;57664039213
 ;;9002226.02101,"1044,57664039213 ",.02)
 ;;57664039213
 ;;9002226.02101,"1044,57664039218 ",.01)
 ;;57664039218
 ;;9002226.02101,"1044,57664039218 ",.02)
 ;;57664039218
 ;;9002226.02101,"1044,57664039288 ",.01)
 ;;57664039288
 ;;9002226.02101,"1044,57664039288 ",.02)
 ;;57664039288
 ;;9002226.02101,"1044,57664039313 ",.01)
 ;;57664039313
 ;;9002226.02101,"1044,57664039313 ",.02)
 ;;57664039313
 ;;9002226.02101,"1044,57664039318 ",.01)
 ;;57664039318
 ;;9002226.02101,"1044,57664039318 ",.02)
 ;;57664039318
 ;;9002226.02101,"1044,57664039388 ",.01)
 ;;57664039388
 ;;9002226.02101,"1044,57664039388 ",.02)
 ;;57664039388
 ;;9002226.02101,"1044,57664039413 ",.01)
 ;;57664039413
 ;;9002226.02101,"1044,57664039413 ",.02)
 ;;57664039413
 ;;9002226.02101,"1044,57664039418 ",.01)
 ;;57664039418
 ;;9002226.02101,"1044,57664039418 ",.02)
 ;;57664039418
 ;;9002226.02101,"1044,57664039488 ",.01)
 ;;57664039488
 ;;9002226.02101,"1044,57664039488 ",.02)
 ;;57664039488
 ;;9002226.02101,"1044,57664039513 ",.01)
 ;;57664039513
 ;;9002226.02101,"1044,57664039513 ",.02)
 ;;57664039513
 ;;9002226.02101,"1044,57664039518 ",.01)
 ;;57664039518
 ;;9002226.02101,"1044,57664039518 ",.02)
 ;;57664039518
 ;;9002226.02101,"1044,57664039588 ",.01)
 ;;57664039588
 ;;9002226.02101,"1044,57664039588 ",.02)
 ;;57664039588
 ;;9002226.02101,"1044,57664039613 ",.01)
 ;;57664039613
 ;;9002226.02101,"1044,57664039613 ",.02)
 ;;57664039613
 ;;9002226.02101,"1044,57664039618 ",.01)
 ;;57664039618
 ;;9002226.02101,"1044,57664039618 ",.02)
 ;;57664039618
 ;;9002226.02101,"1044,57664039688 ",.01)
 ;;57664039688
 ;;9002226.02101,"1044,57664039688 ",.02)
 ;;57664039688
 ;;9002226.02101,"1044,57664042113 ",.01)
 ;;57664042113
 ;;9002226.02101,"1044,57664042113 ",.02)
 ;;57664042113
 ;;9002226.02101,"1044,57664042118 ",.01)
 ;;57664042118
 ;;9002226.02101,"1044,57664042118 ",.02)
 ;;57664042118
 ;;9002226.02101,"1044,57664042183 ",.01)
 ;;57664042183
 ;;9002226.02101,"1044,57664042183 ",.02)
 ;;57664042183
 ;;9002226.02101,"1044,57664042199 ",.01)
 ;;57664042199
 ;;9002226.02101,"1044,57664042199 ",.02)
 ;;57664042199
 ;;9002226.02101,"1044,57664042213 ",.01)
 ;;57664042213
 ;;9002226.02101,"1044,57664042213 ",.02)
 ;;57664042213
 ;;9002226.02101,"1044,57664042218 ",.01)
 ;;57664042218
 ;;9002226.02101,"1044,57664042218 ",.02)
 ;;57664042218
 ;;9002226.02101,"1044,57664042283 ",.01)
 ;;57664042283
 ;;9002226.02101,"1044,57664042283 ",.02)
 ;;57664042283
 ;;9002226.02101,"1044,57664042299 ",.01)
 ;;57664042299
 ;;9002226.02101,"1044,57664042299 ",.02)
 ;;57664042299
 ;;9002226.02101,"1044,57664042413 ",.01)
 ;;57664042413
 ;;9002226.02101,"1044,57664042413 ",.02)
 ;;57664042413
 ;;9002226.02101,"1044,57664042418 ",.01)
 ;;57664042418
 ;;9002226.02101,"1044,57664042418 ",.02)
 ;;57664042418
 ;;9002226.02101,"1044,57664042483 ",.01)
 ;;57664042483
 ;;9002226.02101,"1044,57664042483 ",.02)
 ;;57664042483
 ;;9002226.02101,"1044,57664042499 ",.01)
 ;;57664042499
 ;;9002226.02101,"1044,57664042499 ",.02)
 ;;57664042499
 ;;9002226.02101,"1044,57664042513 ",.01)
 ;;57664042513
 ;;9002226.02101,"1044,57664042513 ",.02)
 ;;57664042513
 ;;9002226.02101,"1044,57664042518 ",.01)
 ;;57664042518
 ;;9002226.02101,"1044,57664042518 ",.02)
 ;;57664042518
 ;;9002226.02101,"1044,57664042583 ",.01)
 ;;57664042583
 ;;9002226.02101,"1044,57664042583 ",.02)
 ;;57664042583
 ;;9002226.02101,"1044,57664042599 ",.01)
 ;;57664042599
 ;;9002226.02101,"1044,57664042599 ",.02)
 ;;57664042599
