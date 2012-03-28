BGP05X3 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON MAY 23, 2010;
 ;;10.0;IHS CLINICAL REPORTING;;JUN 18, 2010
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"55289-0834-60 ")
 ;;790
 ;;21,"55289-0834-84 ")
 ;;791
 ;;21,"55289-0834-86 ")
 ;;792
 ;;21,"55289-0834-88 ")
 ;;793
 ;;21,"55289-0834-89 ")
 ;;794
 ;;21,"55289-0834-90 ")
 ;;795
 ;;21,"55289-0834-93 ")
 ;;796
 ;;21,"55289-0834-98 ")
 ;;797
 ;;21,"55289-0834-99 ")
 ;;798
 ;;21,"55289-0835-30 ")
 ;;799
 ;;21,"55289-0835-90 ")
 ;;800
 ;;21,"55289-0854-30 ")
 ;;801
 ;;21,"55289-0854-90 ")
 ;;802
 ;;21,"55289-0859-30 ")
 ;;803
 ;;21,"55289-0859-90 ")
 ;;804
 ;;21,"55289-0975-90 ")
 ;;805
 ;;21,"55499-1082-01 ")
 ;;806
 ;;21,"55887-0080-07 ")
 ;;807
 ;;21,"55887-0080-30 ")
 ;;808
 ;;21,"55887-0080-60 ")
 ;;809
 ;;21,"55887-0224-15 ")
 ;;810
 ;;21,"55887-0224-30 ")
 ;;811
 ;;21,"55887-0225-14 ")
 ;;812
 ;;21,"55887-0225-15 ")
 ;;813
 ;;21,"55887-0225-21 ")
 ;;814
 ;;21,"55887-0225-28 ")
 ;;815
 ;;21,"55887-0225-30 ")
 ;;816
 ;;21,"55887-0225-42 ")
 ;;817
 ;;21,"55887-0225-60 ")
 ;;818
 ;;21,"55887-0225-90 ")
 ;;819
 ;;21,"55887-0654-07 ")
 ;;820
 ;;21,"55887-0654-14 ")
 ;;821
 ;;21,"55887-0654-15 ")
 ;;822
 ;;21,"55887-0654-30 ")
 ;;823
 ;;21,"55887-0654-60 ")
 ;;824
 ;;21,"55887-0669-15 ")
 ;;825
 ;;21,"55887-0669-21 ")
 ;;826
 ;;21,"55887-0669-30 ")
 ;;827
 ;;21,"55887-0669-60 ")
 ;;828
 ;;21,"55887-0669-90 ")
 ;;829
 ;;21,"55887-0712-14 ")
 ;;830
 ;;21,"55887-0712-30 ")
 ;;831
 ;;21,"55887-0712-60 ")
 ;;832
 ;;21,"55887-0762-07 ")
 ;;833
 ;;21,"55887-0762-14 ")
 ;;834
 ;;21,"55887-0762-15 ")
 ;;835
 ;;21,"55887-0762-30 ")
 ;;836
 ;;21,"55887-0762-60 ")
 ;;837
 ;;21,"55887-0840-01 ")
 ;;838
 ;;21,"55887-0840-07 ")
 ;;839
 ;;21,"55887-0840-14 ")
 ;;840
 ;;21,"55887-0840-15 ")
 ;;841
 ;;21,"55887-0840-20 ")
 ;;842
 ;;21,"55887-0840-23 ")
 ;;843
 ;;21,"55887-0840-30 ")
 ;;844
 ;;21,"55887-0840-45 ")
 ;;845
 ;;21,"55887-0840-60 ")
 ;;846
 ;;21,"55887-0840-90 ")
 ;;847
 ;;21,"55887-0891-01 ")
 ;;848
 ;;21,"55887-0891-14 ")
 ;;849
 ;;21,"55887-0891-15 ")
 ;;850
 ;;21,"55887-0891-30 ")
 ;;851
 ;;21,"55887-0891-60 ")
 ;;852
 ;;21,"57664-0504-18 ")
 ;;853
 ;;21,"57664-0504-88 ")
 ;;854
 ;;21,"57844-0009-01 ")
 ;;855
 ;;21,"57844-0019-01 ")
 ;;856
 ;;21,"57866-9029-01 ")
 ;;857
 ;;21,"57866-9030-01 ")
 ;;858
 ;;21,"58016-0043-00 ")
 ;;859
 ;;21,"58016-0043-30 ")
 ;;860
 ;;21,"58016-0043-60 ")
 ;;861
 ;;21,"58016-0043-90 ")
 ;;862
 ;;21,"58016-0310-00 ")
 ;;863
 ;;21,"58016-0310-30 ")
 ;;864
 ;;21,"58016-0310-60 ")
 ;;865
 ;;21,"58016-0310-90 ")
 ;;866
 ;;21,"58016-0836-07 ")
 ;;867
 ;;21,"58016-0836-14 ")
 ;;868
 ;;21,"58016-0836-15 ")
 ;;869
 ;;21,"58016-0836-30 ")
 ;;870
 ;;21,"58016-0854-42 ")
 ;;871
 ;;21,"58016-0856-00 ")
 ;;872
 ;;21,"58016-0856-15 ")
 ;;873
 ;;21,"58016-0856-21 ")
 ;;874
 ;;21,"58016-0856-42 ")
 ;;875
 ;;21,"58016-0861-14 ")
 ;;876
 ;;21,"58016-0861-30 ")
 ;;877
 ;;21,"58016-0861-60 ")
 ;;878
 ;;21,"58177-0311-04 ")
 ;;879
 ;;21,"58177-0312-04 ")
 ;;880
 ;;21,"58521-0333-05 ")
 ;;881
 ;;21,"58605-0503-01 ")
 ;;882
 ;;21,"58605-0504-01 ")
 ;;883
 ;;21,"58605-0508-01 ")
 ;;884
 ;;21,"59630-0750-50 ")
 ;;885
 ;;21,"59630-0755-50 ")
 ;;886
 ;;21,"59630-0761-10 ")
 ;;887
 ;;21,"60793-0009-01 ")
 ;;888
 ;;21,"63304-0908-01 ")
 ;;889
 ;;21,"63304-0909-01 ")
 ;;890
 ;;21,"63304-0910-01 ")
 ;;891
 ;;21,"63304-0911-01 ")
 ;;892
 ;;21,"63629-1584-01 ")
 ;;893
 ;;21,"63629-1584-02 ")
 ;;894
 ;;21,"63629-1584-03 ")
 ;;895
 ;;21,"63629-1584-04 ")
 ;;896
 ;;21,"63629-1584-05 ")
 ;;897
 ;;21,"63629-1604-01 ")
 ;;898
 ;;21,"63629-1604-02 ")
 ;;899
 ;;21,"63629-1604-03 ")
 ;;900
 ;;21,"63629-1604-04 ")
 ;;901
 ;;21,"63629-2949-01 ")
 ;;902
 ;;21,"63629-2949-02 ")
 ;;903
 ;;21,"63629-2949-03 ")
 ;;904
 ;;21,"63629-2949-04 ")
 ;;905
 ;;21,"63629-2950-01 ")
 ;;906
 ;;21,"63629-2950-02 ")
 ;;907
 ;;21,"63629-2950-03 ")
 ;;908
 ;;21,"63629-2950-04 ")
 ;;909
 ;;21,"63629-3051-01 ")
 ;;910
 ;;21,"63629-3065-01 ")
 ;;911
 ;;21,"63629-3166-01 ")
 ;;912
 ;;21,"63629-3166-02 ")
 ;;913
 ;;21,"63874-0260-01 ")
 ;;914
 ;;21,"63874-0260-02 ")
 ;;915
 ;;21,"63874-0260-07 ")
 ;;916
 ;;21,"63874-0260-14 ")
 ;;917
 ;;21,"63874-0260-20 ")
 ;;918
 ;;21,"63874-0260-30 ")
 ;;919
 ;;21,"63874-0260-50 ")
 ;;920
 ;;21,"63874-0265-01 ")
 ;;921
 ;;21,"63874-0265-20 ")
 ;;922
 ;;21,"63874-0269-01 ")
 ;;923
 ;;21,"63874-0269-02 ")
 ;;924
 ;;21,"63874-0269-03 ")
 ;;925
 ;;21,"63874-0269-07 ")
 ;;926
 ;;21,"63874-0269-10 ")
 ;;927
 ;;21,"63874-0269-12 ")
 ;;928
 ;;21,"63874-0269-14 ")
 ;;929
 ;;21,"63874-0269-15 ")
 ;;930
 ;;21,"63874-0269-20 ")
 ;;931
 ;;21,"63874-0269-21 ")
 ;;932
 ;;21,"63874-0269-24 ")
 ;;933
 ;;21,"63874-0269-30 ")
 ;;934
 ;;21,"63874-0269-40 ")
 ;;935
 ;;21,"63874-0269-60 ")
 ;;936
 ;;21,"63874-0270-01 ")
 ;;937
 ;;21,"63874-0270-07 ")
 ;;938
 ;;21,"63874-0270-30 ")
 ;;939
 ;;21,"63874-0271-01 ")
 ;;940
 ;;21,"63874-0271-07 ")
 ;;941
 ;;21,"63874-0271-14 ")
 ;;942
 ;;21,"63874-0271-15 ")
 ;;943
 ;;21,"63874-0271-30 ")
 ;;944
 ;;21,"63874-0282-01 ")
 ;;945
 ;;21,"63874-0282-02 ")
 ;;946
 ;;21,"63874-0282-10 ")
 ;;947
 ;;21,"63874-0282-14 ")
 ;;948
 ;;21,"63874-0282-15 ")
 ;;949
 ;;21,"63874-0282-30 ")
 ;;950
 ;;21,"64376-0650-01 ")
 ;;951
 ;;21,"64376-0650-05 ")
 ;;952
 ;;21,"64376-0650-31 ")
 ;;953
 ;;21,"64376-0650-90 ")
 ;;954
 ;;21,"64720-0130-10 ")
 ;;955
 ;;21,"64720-0132-10 ")
 ;;956
 ;;21,"64720-0135-10 ")
 ;;957
 ;;21,"64720-0136-10 ")
 ;;958
 ;;21,"64720-0194-01 ")
 ;;959
 ;;21,"64720-0194-10 ")
 ;;960
 ;;21,"64720-0194-50 ")
 ;;961
 ;;21,"64720-0208-10 ")
 ;;962
 ;;21,"65234-0047-10 ")
 ;;963
 ;;21,"65234-0048-10 ")
 ;;964
 ;;21,"65234-0048-90 ")
 ;;965
 ;;21,"66336-0133-07 ")
 ;;966
 ;;21,"66336-0133-14 ")
 ;;967
 ;;21,"66336-0133-28 ")
 ;;968
 ;;21,"66336-0133-30 ")
 ;;969
 ;;21,"66336-0185-07 ")
 ;;970
 ;;21,"66336-0185-14 ")
 ;;971
 ;;21,"66336-0185-28 ")
 ;;972
 ;;21,"66336-0185-30 ")
 ;;973
 ;;21,"66336-0185-42 ")
 ;;974
 ;;21,"66336-0191-21 ")
 ;;975
 ;;21,"66336-0191-28 ")
 ;;976
 ;;21,"66336-0191-30 ")
 ;;977
 ;;21,"66336-0191-42 ")
 ;;978
 ;;21,"66336-0191-56 ")
 ;;979
 ;;21,"66336-0191-84 ")
 ;;980
 ;;21,"66336-0344-07 ")
 ;;981
 ;;21,"66336-0344-14 ")
 ;;982
 ;;21,"66336-0344-28 ")
 ;;983
 ;;21,"66336-0344-30 ")
 ;;984
 ;;21,"66336-0344-56 ")
 ;;985
 ;;21,"66336-0344-84 ")
 ;;986
 ;;21,"66336-0678-21 ")
 ;;987
 ;;21,"66336-0689-07 ")
 ;;988
 ;;21,"66336-0689-14 ")
 ;;989
 ;;21,"66336-0689-28 ")
 ;;990
 ;;21,"66336-0689-30 ")
 ;;991
 ;;21,"66336-0763-07 ")
 ;;992
 ;;21,"66336-0763-14 ")
 ;;993
 ;;21,"66336-0763-28 ")
 ;;994
 ;;21,"66336-0763-30 ")
 ;;995
 ;;21,"66336-0779-07 ")
 ;;996
 ;;21,"66336-0779-14 ")
 ;;997
 ;;21,"66336-0779-28 ")
 ;;998
 ;;21,"66336-0864-14 ")
 ;;999
 ;;21,"66336-0864-28 ")
 ;;1000
 ;;21,"66336-0864-30 ")
 ;;1001
 ;;21,"66336-0864-42 ")
 ;;1002
 ;;21,"66336-0864-60 ")
 ;;1003
 ;;21,"66336-0864-84 ")
 ;;1004
 ;;21,"67386-0102-01 ")
 ;;1005
 ;;21,"67767-0103-01 ")
 ;;1006
 ;;21,"67767-0106-01 ")
 ;;1007
 ;;21,"68115-0050-07 ")
 ;;1008
 ;;21,"68115-0050-14 ")
 ;;1009
 ;;21,"68115-0050-28 ")
 ;;1010
 ;;21,"68115-0050-90 ")
 ;;1011
 ;;21,"68115-0051-14 ")
 ;;1012
 ;;21,"68115-0051-21 ")
 ;;1013
 ;;21,"68115-0051-30 ")
 ;;1014
 ;;21,"68115-0108-21 ")
 ;;1015
 ;;21,"68115-0108-30 ")
 ;;1016
 ;;21,"68115-0109-14 ")
 ;;1017
 ;;21,"68115-0109-21 ")
 ;;1018
 ;;21,"68115-0109-30 ")
 ;;1019
 ;;21,"68115-0110-28 ")
 ;;1020
 ;;21,"68115-0273-14 ")
 ;;1021
 ;;21,"68115-0273-21 ")
 ;;1022
 ;;21,"68115-0273-60 ")
 ;;1023
 ;;21,"68115-0277-14 ")
 ;;1024
 ;;21,"68115-0277-30 ")
 ;;1025
 ;;21,"68115-0278-14 ")
 ;;1026
 ;;21,"68115-0278-30 ")
 ;;1027
 ;;21,"68115-0279-00 ")
 ;;1028
 ;;21,"68115-0279-30 ")
 ;;1029
 ;;21,"68115-0280-15 ")
 ;;1030
 ;;21,"68115-0280-30 ")
 ;;1031
 ;;21,"68115-0757-00 ")
 ;;1032
 ;;21,"68188-0132-01 ")
 ;;1033
 ;;21,"68188-0135-01 ")
 ;;1034
 ;;21,"68188-0137-01 ")
 ;;1035
 ;;21,"68188-0881-50 ")
 ;;1036
 ;;21,"68188-0882-50 ")
 ;;1037
 ;;21,"68387-0690-30 ")
 ;;1038
 ;;9002226,686,.01)
 ;;BGP HEDIS AMPHETAMINE NDC
 ;;9002226,686,.02)
 ;;@
 ;;9002226,686,.04)
 ;;n
 ;;9002226,686,.06)
 ;;@
 ;;9002226,686,.08)
 ;;@
 ;;9002226,686,.09)
 ;;@
 ;;9002226,686,.11)
 ;;@
 ;;9002226,686,.12)
 ;;@
 ;;9002226,686,.13)
 ;;1
 ;;9002226,686,.14)
 ;;@
 ;;9002226,686,.15)
 ;;@
 ;;9002226,686,.16)
 ;;@
 ;;9002226,686,.17)
 ;;@
 ;;9002226,686,3101)
 ;;@
 ;;9002226.02101,"686,00007-3512-20 ",.01)
 ;;00007-3512-20
 ;;9002226.02101,"686,00007-3512-20 ",.02)
 ;;00007-3512-20
 ;;9002226.02101,"686,00007-3512-59 ",.01)
 ;;00007-3512-59
 ;;9002226.02101,"686,00007-3512-59 ",.02)
 ;;00007-3512-59
 ;;9002226.02101,"686,00007-3513-20 ",.01)
 ;;00007-3513-20
 ;;9002226.02101,"686,00007-3513-20 ",.02)
 ;;00007-3513-20
 ;;9002226.02101,"686,00007-3513-59 ",.01)
 ;;00007-3513-59
 ;;9002226.02101,"686,00007-3513-59 ",.02)
 ;;00007-3513-59
 ;;9002226.02101,"686,00007-3514-20 ",.01)
 ;;00007-3514-20
 ;;9002226.02101,"686,00007-3514-20 ",.02)
 ;;00007-3514-20
 ;;9002226.02101,"686,00007-3514-59 ",.01)
 ;;00007-3514-59
 ;;9002226.02101,"686,00007-3514-59 ",.02)
 ;;00007-3514-59
 ;;9002226.02101,"686,00007-3519-20 ",.01)
 ;;00007-3519-20
 ;;9002226.02101,"686,00007-3519-20 ",.02)
 ;;00007-3519-20
 ;;9002226.02101,"686,00009-0024-01 ",.01)
 ;;00009-0024-01
 ;;9002226.02101,"686,00009-0024-01 ",.02)
 ;;00009-0024-01
 ;;9002226.02101,"686,00009-0024-02 ",.01)
 ;;00009-0024-02
 ;;9002226.02101,"686,00009-0024-02 ",.02)
 ;;00009-0024-02
 ;;9002226.02101,"686,00068-0697-61 ",.01)
 ;;00068-0697-61
 ;;9002226.02101,"686,00068-0697-61 ",.02)
 ;;00068-0697-61
 ;;9002226.02101,"686,00068-0698-61 ",.01)
 ;;00068-0698-61
 ;;9002226.02101,"686,00068-0698-61 ",.02)
 ;;00068-0698-61
 ;;9002226.02101,"686,00068-0698-62 ",.01)
 ;;00068-0698-62
 ;;9002226.02101,"686,00068-0698-62 ",.02)
 ;;00068-0698-62
 ;;9002226.02101,"686,00074-3377-04 ",.01)
 ;;00074-3377-04
 ;;9002226.02101,"686,00074-3377-04 ",.02)
 ;;00074-3377-04
 ;;9002226.02101,"686,00078-0370-05 ",.01)
 ;;00078-0370-05
 ;;9002226.02101,"686,00078-0370-05 ",.02)
 ;;00078-0370-05
 ;;9002226.02101,"686,00078-0371-05 ",.01)
 ;;00078-0371-05
 ;;9002226.02101,"686,00078-0371-05 ",.02)
 ;;00078-0371-05
 ;;9002226.02101,"686,00078-0372-05 ",.01)
 ;;00078-0372-05
 ;;9002226.02101,"686,00078-0372-05 ",.02)
 ;;00078-0372-05
 ;;9002226.02101,"686,00078-0380-05 ",.01)
 ;;00078-0380-05
 ;;9002226.02101,"686,00078-0380-05 ",.02)
 ;;00078-0380-05
 ;;9002226.02101,"686,00078-0381-05 ",.01)
 ;;00078-0381-05
 ;;9002226.02101,"686,00078-0381-05 ",.02)
 ;;00078-0381-05
 ;;9002226.02101,"686,00078-0382-05 ",.01)
 ;;00078-0382-05
 ;;9002226.02101,"686,00078-0382-05 ",.02)
 ;;00078-0382-05
 ;;9002226.02101,"686,00078-0424-05 ",.01)
 ;;00078-0424-05
 ;;9002226.02101,"686,00078-0424-05 ",.02)
 ;;00078-0424-05
 ;;9002226.02101,"686,00078-0430-05 ",.01)
 ;;00078-0430-05
 ;;9002226.02101,"686,00078-0430-05 ",.02)
 ;;00078-0430-05
 ;;9002226.02101,"686,00078-0431-05 ",.01)
 ;;00078-0431-05
 ;;9002226.02101,"686,00078-0431-05 ",.02)
 ;;00078-0431-05
 ;;9002226.02101,"686,00078-0432-05 ",.01)
 ;;00078-0432-05
 ;;9002226.02101,"686,00078-0432-05 ",.02)
 ;;00078-0432-05
 ;;9002226.02101,"686,00078-0439-05 ",.01)
 ;;00078-0439-05
 ;;9002226.02101,"686,00078-0439-05 ",.02)
 ;;00078-0439-05
 ;;9002226.02101,"686,00078-0440-05 ",.01)
 ;;00078-0440-05
 ;;9002226.02101,"686,00078-0440-05 ",.02)
 ;;00078-0440-05
 ;;9002226.02101,"686,00078-0441-05 ",.01)
 ;;00078-0441-05
 ;;9002226.02101,"686,00078-0441-05 ",.02)
 ;;00078-0441-05
 ;;9002226.02101,"686,00078-0442-05 ",.01)
 ;;00078-0442-05
 ;;9002226.02101,"686,00078-0442-05 ",.02)
 ;;00078-0442-05
 ;;9002226.02101,"686,00078-0493-05 ",.01)
 ;;00078-0493-05
 ;;9002226.02101,"686,00078-0493-05 ",.02)
 ;;00078-0493-05
 ;;9002226.02101,"686,00083-0003-30 ",.01)
 ;;00083-0003-30
 ;;9002226.02101,"686,00083-0003-30 ",.02)
 ;;00083-0003-30
 ;;9002226.02101,"686,00083-0007-30 ",.01)
 ;;00083-0007-30
 ;;9002226.02101,"686,00083-0007-30 ",.02)
 ;;00083-0007-30
 ;;9002226.02101,"686,00083-0016-30 ",.01)
 ;;00083-0016-30
 ;;9002226.02101,"686,00083-0016-30 ",.02)
 ;;00083-0016-30
 ;;9002226.02101,"686,00083-0034-30 ",.01)
 ;;00083-0034-30
 ;;9002226.02101,"686,00083-0034-30 ",.02)
 ;;00083-0034-30
 ;;9002226.02101,"686,00093-5275-01 ",.01)
 ;;00093-5275-01
 ;;9002226.02101,"686,00093-5275-01 ",.02)
 ;;00093-5275-01
 ;;9002226.02101,"686,00093-5276-01 ",.01)
 ;;00093-5276-01
 ;;9002226.02101,"686,00093-5276-01 ",.02)
 ;;00093-5276-01
 ;;9002226.02101,"686,00093-5277-01 ",.01)
 ;;00093-5277-01
 ;;9002226.02101,"686,00093-5277-01 ",.02)
 ;;00093-5277-01
 ;;9002226.02101,"686,00144-0740-01 ",.01)
 ;;00144-0740-01
 ;;9002226.02101,"686,00144-0740-01 ",.02)
 ;;00144-0740-01
 ;;9002226.02101,"686,00147-0102-20 ",.01)
 ;;00147-0102-20
 ;;9002226.02101,"686,00147-0102-20 ",.02)
 ;;00147-0102-20
 ;;9002226.02101,"686,00147-0109-10 ",.01)
 ;;00147-0109-10
