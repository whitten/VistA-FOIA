BGPM5AR ;IHS/MSC/SAT-CREATED BY ^ATXSTX ON AUG 16, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"1005,49999076130 ",.01)
 ;;49999076130
 ;;9002226.02101,"1005,49999076130 ",.02)
 ;;49999076130
 ;;9002226.02101,"1005,49999081530 ",.01)
 ;;49999081530
 ;;9002226.02101,"1005,49999081530 ",.02)
 ;;49999081530
 ;;9002226.02101,"1005,49999081590 ",.01)
 ;;49999081590
 ;;9002226.02101,"1005,49999081590 ",.02)
 ;;49999081590
 ;;9002226.02101,"1005,49999082300 ",.01)
 ;;49999082300
 ;;9002226.02101,"1005,49999082300 ",.02)
 ;;49999082300
 ;;9002226.02101,"1005,49999087000 ",.01)
 ;;49999087000
 ;;9002226.02101,"1005,49999087000 ",.02)
 ;;49999087000
 ;;9002226.02101,"1005,49999087130 ",.01)
 ;;49999087130
 ;;9002226.02101,"1005,49999087130 ",.02)
 ;;49999087130
 ;;9002226.02101,"1005,49999087190 ",.01)
 ;;49999087190
 ;;9002226.02101,"1005,49999087190 ",.02)
 ;;49999087190
 ;;9002226.02101,"1005,49999087700 ",.01)
 ;;49999087700
 ;;9002226.02101,"1005,49999087700 ",.02)
 ;;49999087700
 ;;9002226.02101,"1005,49999087730 ",.01)
 ;;49999087730
 ;;9002226.02101,"1005,49999087730 ",.02)
 ;;49999087730
 ;;9002226.02101,"1005,49999087790 ",.01)
 ;;49999087790
 ;;9002226.02101,"1005,49999087790 ",.02)
 ;;49999087790
 ;;9002226.02101,"1005,49999087800 ",.01)
 ;;49999087800
 ;;9002226.02101,"1005,49999087800 ",.02)
 ;;49999087800
 ;;9002226.02101,"1005,49999087830 ",.01)
 ;;49999087830
 ;;9002226.02101,"1005,49999087830 ",.02)
 ;;49999087830
 ;;9002226.02101,"1005,49999087890 ",.01)
 ;;49999087890
 ;;9002226.02101,"1005,49999087890 ",.02)
 ;;49999087890
 ;;9002226.02101,"1005,49999087900 ",.01)
 ;;49999087900
 ;;9002226.02101,"1005,49999087900 ",.02)
 ;;49999087900
 ;;9002226.02101,"1005,49999087930 ",.01)
 ;;49999087930
 ;;9002226.02101,"1005,49999087930 ",.02)
 ;;49999087930
 ;;9002226.02101,"1005,49999087990 ",.01)
 ;;49999087990
 ;;9002226.02101,"1005,49999087990 ",.02)
 ;;49999087990
 ;;9002226.02101,"1005,49999088000 ",.01)
 ;;49999088000
 ;;9002226.02101,"1005,49999088000 ",.02)
 ;;49999088000
 ;;9002226.02101,"1005,49999088030 ",.01)
 ;;49999088030
 ;;9002226.02101,"1005,49999088030 ",.02)
 ;;49999088030
 ;;9002226.02101,"1005,49999088090 ",.01)
 ;;49999088090
 ;;9002226.02101,"1005,49999088090 ",.02)
 ;;49999088090
 ;;9002226.02101,"1005,49999092400 ",.01)
 ;;49999092400
 ;;9002226.02101,"1005,49999092400 ",.02)
 ;;49999092400
 ;;9002226.02101,"1005,49999092410 ",.01)
 ;;49999092410
 ;;9002226.02101,"1005,49999092410 ",.02)
 ;;49999092410
 ;;9002226.02101,"1005,49999092430 ",.01)
 ;;49999092430
 ;;9002226.02101,"1005,49999092430 ",.02)
 ;;49999092430
 ;;9002226.02101,"1005,49999092460 ",.01)
 ;;49999092460
 ;;9002226.02101,"1005,49999092460 ",.02)
 ;;49999092460
 ;;9002226.02101,"1005,49999093430 ",.01)
 ;;49999093430
 ;;9002226.02101,"1005,49999093430 ",.02)
 ;;49999093430
 ;;9002226.02101,"1005,49999094300 ",.01)
 ;;49999094300
 ;;9002226.02101,"1005,49999094300 ",.02)
 ;;49999094300
 ;;9002226.02101,"1005,49999094390 ",.01)
 ;;49999094390
 ;;9002226.02101,"1005,49999094390 ",.02)
 ;;49999094390
 ;;9002226.02101,"1005,49999098830 ",.01)
 ;;49999098830
 ;;9002226.02101,"1005,49999098830 ",.02)
 ;;49999098830
 ;;9002226.02101,"1005,51079025640 ",.01)
 ;;51079025640
 ;;9002226.02101,"1005,51079025640 ",.02)
 ;;51079025640
 ;;9002226.02101,"1005,51079026140 ",.01)
 ;;51079026140
 ;;9002226.02101,"1005,51079026140 ",.02)
 ;;51079026140
 ;;9002226.02101,"1005,51079069740 ",.01)
 ;;51079069740
 ;;9002226.02101,"1005,51079069740 ",.02)
 ;;51079069740
 ;;9002226.02101,"1005,51079069840 ",.01)
 ;;51079069840
 ;;9002226.02101,"1005,51079069840 ",.02)
 ;;51079069840
 ;;9002226.02101,"1005,51079069940 ",.01)
 ;;51079069940
 ;;9002226.02101,"1005,51079069940 ",.02)
 ;;51079069940
 ;;9002226.02101,"1005,51079086301 ",.01)
 ;;51079086301
 ;;9002226.02101,"1005,51079086301 ",.02)
 ;;51079086301
 ;;9002226.02101,"1005,51079086320 ",.01)
 ;;51079086320
 ;;9002226.02101,"1005,51079086320 ",.02)
 ;;51079086320
 ;;9002226.02101,"1005,51079086401 ",.01)
 ;;51079086401
 ;;9002226.02101,"1005,51079086401 ",.02)
 ;;51079086401
 ;;9002226.02101,"1005,51079086420 ",.01)
 ;;51079086420
 ;;9002226.02101,"1005,51079086420 ",.02)
 ;;51079086420
 ;;9002226.02101,"1005,51079095001 ",.01)
 ;;51079095001
 ;;9002226.02101,"1005,51079095001 ",.02)
 ;;51079095001
 ;;9002226.02101,"1005,51079095020 ",.01)
 ;;51079095020
 ;;9002226.02101,"1005,51079095020 ",.02)
 ;;51079095020
 ;;9002226.02101,"1005,51079095101 ",.01)
 ;;51079095101
 ;;9002226.02101,"1005,51079095101 ",.02)
 ;;51079095101
 ;;9002226.02101,"1005,51079095120 ",.01)
 ;;51079095120
 ;;9002226.02101,"1005,51079095120 ",.02)
 ;;51079095120
 ;;9002226.02101,"1005,51079095201 ",.01)
 ;;51079095201
 ;;9002226.02101,"1005,51079095201 ",.02)
 ;;51079095201
 ;;9002226.02101,"1005,51079095220 ",.01)
 ;;51079095220
 ;;9002226.02101,"1005,51079095220 ",.02)
 ;;51079095220
 ;;9002226.02101,"1005,51079095301 ",.01)
 ;;51079095301
 ;;9002226.02101,"1005,51079095301 ",.02)
 ;;51079095301
 ;;9002226.02101,"1005,51079095320 ",.01)
 ;;51079095320
 ;;9002226.02101,"1005,51079095320 ",.02)
 ;;51079095320
 ;;9002226.02101,"1005,51079098101 ",.01)
 ;;51079098101
 ;;9002226.02101,"1005,51079098101 ",.02)
 ;;51079098101
 ;;9002226.02101,"1005,51079098120 ",.01)
 ;;51079098120
 ;;9002226.02101,"1005,51079098120 ",.02)
 ;;51079098120
 ;;9002226.02101,"1005,51079098130 ",.01)
 ;;51079098130
 ;;9002226.02101,"1005,51079098130 ",.02)
 ;;51079098130
 ;;9002226.02101,"1005,51079098140 ",.01)
 ;;51079098140
 ;;9002226.02101,"1005,51079098140 ",.02)
 ;;51079098140
 ;;9002226.02101,"1005,51079098156 ",.01)
 ;;51079098156
 ;;9002226.02101,"1005,51079098156 ",.02)
 ;;51079098156
 ;;9002226.02101,"1005,51079098157 ",.01)
 ;;51079098157
 ;;9002226.02101,"1005,51079098157 ",.02)
 ;;51079098157
 ;;9002226.02101,"1005,51079098160 ",.01)
 ;;51079098160
 ;;9002226.02101,"1005,51079098160 ",.02)
 ;;51079098160
 ;;9002226.02101,"1005,51079098201 ",.01)
 ;;51079098201
 ;;9002226.02101,"1005,51079098201 ",.02)
 ;;51079098201
 ;;9002226.02101,"1005,51079098217 ",.01)
 ;;51079098217
 ;;9002226.02101,"1005,51079098217 ",.02)
 ;;51079098217
 ;;9002226.02101,"1005,51079098219 ",.01)
 ;;51079098219
 ;;9002226.02101,"1005,51079098219 ",.02)
 ;;51079098219
 ;;9002226.02101,"1005,51079098220 ",.01)
 ;;51079098220
 ;;9002226.02101,"1005,51079098220 ",.02)
 ;;51079098220
 ;;9002226.02101,"1005,51079098230 ",.01)
 ;;51079098230
 ;;9002226.02101,"1005,51079098230 ",.02)
 ;;51079098230
 ;;9002226.02101,"1005,51079098240 ",.01)
 ;;51079098240
 ;;9002226.02101,"1005,51079098240 ",.02)
 ;;51079098240
 ;;9002226.02101,"1005,51079098256 ",.01)
 ;;51079098256
 ;;9002226.02101,"1005,51079098256 ",.02)
 ;;51079098256
 ;;9002226.02101,"1005,51079098257 ",.01)
 ;;51079098257
 ;;9002226.02101,"1005,51079098257 ",.02)
 ;;51079098257
 ;;9002226.02101,"1005,51079098260 ",.01)
 ;;51079098260
 ;;9002226.02101,"1005,51079098260 ",.02)
 ;;51079098260
 ;;9002226.02101,"1005,51079098301 ",.01)
 ;;51079098301
 ;;9002226.02101,"1005,51079098301 ",.02)
 ;;51079098301
 ;;9002226.02101,"1005,51079098317 ",.01)
 ;;51079098317
 ;;9002226.02101,"1005,51079098317 ",.02)
 ;;51079098317
 ;;9002226.02101,"1005,51079098319 ",.01)
 ;;51079098319
 ;;9002226.02101,"1005,51079098319 ",.02)
 ;;51079098319
 ;;9002226.02101,"1005,51079098320 ",.01)
 ;;51079098320
 ;;9002226.02101,"1005,51079098320 ",.02)
 ;;51079098320
 ;;9002226.02101,"1005,51079098330 ",.01)
 ;;51079098330
 ;;9002226.02101,"1005,51079098330 ",.02)
 ;;51079098330
 ;;9002226.02101,"1005,51079098340 ",.01)
 ;;51079098340
 ;;9002226.02101,"1005,51079098340 ",.02)
 ;;51079098340
 ;;9002226.02101,"1005,51079098356 ",.01)
 ;;51079098356
 ;;9002226.02101,"1005,51079098356 ",.02)
 ;;51079098356
 ;;9002226.02101,"1005,51079098357 ",.01)
 ;;51079098357
 ;;9002226.02101,"1005,51079098357 ",.02)
 ;;51079098357
 ;;9002226.02101,"1005,51079098360 ",.01)
 ;;51079098360
 ;;9002226.02101,"1005,51079098360 ",.02)
 ;;51079098360
 ;;9002226.02101,"1005,51079098401 ",.01)
 ;;51079098401
 ;;9002226.02101,"1005,51079098401 ",.02)
 ;;51079098401
 ;;9002226.02101,"1005,51079098420 ",.01)
 ;;51079098420
 ;;9002226.02101,"1005,51079098420 ",.02)
 ;;51079098420
 ;;9002226.02101,"1005,51079098440 ",.01)
 ;;51079098440
 ;;9002226.02101,"1005,51079098440 ",.02)
 ;;51079098440
 ;;9002226.02101,"1005,51079098450 ",.01)
 ;;51079098450
 ;;9002226.02101,"1005,51079098450 ",.02)
 ;;51079098450
 ;;9002226.02101,"1005,51129045201 ",.01)
 ;;51129045201
 ;;9002226.02101,"1005,51129045201 ",.02)
 ;;51129045201
 ;;9002226.02101,"1005,51129110501 ",.01)
 ;;51129110501
 ;;9002226.02101,"1005,51129110501 ",.02)
 ;;51129110501
 ;;9002226.02101,"1005,51129130901 ",.01)
 ;;51129130901
 ;;9002226.02101,"1005,51129130901 ",.02)
 ;;51129130901
 ;;9002226.02101,"1005,51129133001 ",.01)
 ;;51129133001
 ;;9002226.02101,"1005,51129133001 ",.02)
 ;;51129133001
 ;;9002226.02101,"1005,51129133901 ",.01)
 ;;51129133901
 ;;9002226.02101,"1005,51129133901 ",.02)
 ;;51129133901
 ;;9002226.02101,"1005,51129139701 ",.01)
 ;;51129139701
 ;;9002226.02101,"1005,51129139701 ",.02)
 ;;51129139701
 ;;9002226.02101,"1005,51129140001 ",.01)
 ;;51129140001
 ;;9002226.02101,"1005,51129140001 ",.02)
 ;;51129140001
 ;;9002226.02101,"1005,51129157101 ",.01)
 ;;51129157101
 ;;9002226.02101,"1005,51129157101 ",.02)
 ;;51129157101
 ;;9002226.02101,"1005,51129157201 ",.01)
 ;;51129157201
 ;;9002226.02101,"1005,51129157201 ",.02)
 ;;51129157201
 ;;9002226.02101,"1005,51129161401 ",.01)
 ;;51129161401
 ;;9002226.02101,"1005,51129161401 ",.02)
 ;;51129161401
 ;;9002226.02101,"1005,51129163801 ",.01)
 ;;51129163801
 ;;9002226.02101,"1005,51129163801 ",.02)
 ;;51129163801
 ;;9002226.02101,"1005,51129171101 ",.01)
 ;;51129171101
 ;;9002226.02101,"1005,51129171101 ",.02)
 ;;51129171101
 ;;9002226.02101,"1005,51129171201 ",.01)
 ;;51129171201
 ;;9002226.02101,"1005,51129171201 ",.02)
 ;;51129171201
 ;;9002226.02101,"1005,51129175001 ",.01)
 ;;51129175001
 ;;9002226.02101,"1005,51129175001 ",.02)
 ;;51129175001
 ;;9002226.02101,"1005,51129178301 ",.01)
 ;;51129178301
 ;;9002226.02101,"1005,51129178301 ",.02)
 ;;51129178301
 ;;9002226.02101,"1005,51129178601 ",.01)
 ;;51129178601
 ;;9002226.02101,"1005,51129178601 ",.02)
 ;;51129178601
 ;;9002226.02101,"1005,51129181601 ",.01)
 ;;51129181601
 ;;9002226.02101,"1005,51129181601 ",.02)
 ;;51129181601
 ;;9002226.02101,"1005,51129188801 ",.01)
 ;;51129188801
 ;;9002226.02101,"1005,51129188801 ",.02)
 ;;51129188801
 ;;9002226.02101,"1005,51129188901 ",.01)
 ;;51129188901
 ;;9002226.02101,"1005,51129188901 ",.02)
 ;;51129188901
 ;;9002226.02101,"1005,51129191501 ",.01)
 ;;51129191501
 ;;9002226.02101,"1005,51129191501 ",.02)
 ;;51129191501
 ;;9002226.02101,"1005,51129191601 ",.01)
 ;;51129191601
 ;;9002226.02101,"1005,51129191601 ",.02)
 ;;51129191601
 ;;9002226.02101,"1005,51129194701 ",.01)
 ;;51129194701
 ;;9002226.02101,"1005,51129194701 ",.02)
 ;;51129194701
 ;;9002226.02101,"1005,51129196501 ",.01)
 ;;51129196501
 ;;9002226.02101,"1005,51129196501 ",.02)
 ;;51129196501
 ;;9002226.02101,"1005,51129199601 ",.01)
 ;;51129199601
 ;;9002226.02101,"1005,51129199601 ",.02)
 ;;51129199601
 ;;9002226.02101,"1005,51129209901 ",.01)
 ;;51129209901
 ;;9002226.02101,"1005,51129209901 ",.02)
 ;;51129209901
 ;;9002226.02101,"1005,51129213001 ",.01)
 ;;51129213001
 ;;9002226.02101,"1005,51129213001 ",.02)
 ;;51129213001
 ;;9002226.02101,"1005,51129224201 ",.01)
 ;;51129224201
 ;;9002226.02101,"1005,51129224201 ",.02)
 ;;51129224201
 ;;9002226.02101,"1005,51129227401 ",.01)
 ;;51129227401
