BGPM5ATQ ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON AUG 29, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"54868239104 ")
 ;;799
 ;;21,"54868239105 ")
 ;;800
 ;;21,"54868287500 ")
 ;;801
 ;;21,"54868287501 ")
 ;;802
 ;;21,"54868310500 ")
 ;;803
 ;;21,"54868310501 ")
 ;;804
 ;;21,"54868310502 ")
 ;;805
 ;;21,"54868310503 ")
 ;;806
 ;;21,"54868310700 ")
 ;;807
 ;;21,"54868310701 ")
 ;;808
 ;;21,"54868310702 ")
 ;;809
 ;;21,"54868310703 ")
 ;;810
 ;;21,"54868310706 ")
 ;;811
 ;;21,"54868310707 ")
 ;;812
 ;;21,"54868310708 ")
 ;;813
 ;;21,"54868310709 ")
 ;;814
 ;;21,"54868310900 ")
 ;;815
 ;;21,"54868310901 ")
 ;;816
 ;;21,"54868310902 ")
 ;;817
 ;;21,"54868310903 ")
 ;;818
 ;;21,"54868310904 ")
 ;;819
 ;;21,"54868310905 ")
 ;;820
 ;;21,"54868310906 ")
 ;;821
 ;;21,"54868310907 ")
 ;;822
 ;;21,"54868310908 ")
 ;;823
 ;;21,"54868310909 ")
 ;;824
 ;;21,"54868311100 ")
 ;;825
 ;;21,"54868311101 ")
 ;;826
 ;;21,"54868311103 ")
 ;;827
 ;;21,"54868311104 ")
 ;;828
 ;;21,"54868311105 ")
 ;;829
 ;;21,"54868311300 ")
 ;;830
 ;;21,"54868311303 ")
 ;;831
 ;;21,"54868311305 ")
 ;;832
 ;;21,"54868311307 ")
 ;;833
 ;;21,"54868311308 ")
 ;;834
 ;;21,"54868311309 ")
 ;;835
 ;;21,"54868316900 ")
 ;;836
 ;;21,"54868316901 ")
 ;;837
 ;;21,"54868316902 ")
 ;;838
 ;;21,"54868316903 ")
 ;;839
 ;;21,"54868316904 ")
 ;;840
 ;;21,"54868316905 ")
 ;;841
 ;;21,"54868316906 ")
 ;;842
 ;;21,"54868347200 ")
 ;;843
 ;;21,"54868347300 ")
 ;;844
 ;;21,"54868347800 ")
 ;;845
 ;;21,"54868347801 ")
 ;;846
 ;;21,"54868347802 ")
 ;;847
 ;;21,"54868347803 ")
 ;;848
 ;;21,"54868348000 ")
 ;;849
 ;;21,"54868348100 ")
 ;;850
 ;;21,"54868351100 ")
 ;;851
 ;;21,"54868351101 ")
 ;;852
 ;;21,"54868351102 ")
 ;;853
 ;;21,"54868351104 ")
 ;;854
 ;;21,"54868369500 ")
 ;;855
 ;;21,"54868374201 ")
 ;;856
 ;;21,"54868374202 ")
 ;;857
 ;;21,"54868374203 ")
 ;;858
 ;;21,"54868404700 ")
 ;;859
 ;;21,"54868412501 ")
 ;;860
 ;;21,"54868412502 ")
 ;;861
 ;;21,"54868412901 ")
 ;;862
 ;;21,"54868412902 ")
 ;;863
 ;;21,"54868413101 ")
 ;;864
 ;;21,"54868415000 ")
 ;;865
 ;;21,"54868415001 ")
 ;;866
 ;;21,"54868415002 ")
 ;;867
 ;;21,"54868415500 ")
 ;;868
 ;;21,"54868415501 ")
 ;;869
 ;;21,"54868415502 ")
 ;;870
 ;;21,"54868415503 ")
 ;;871
 ;;21,"54868415504 ")
 ;;872
 ;;21,"54868433500 ")
 ;;873
 ;;21,"54868446800 ")
 ;;874
 ;;21,"54868448800 ")
 ;;875
 ;;21,"54868454300 ")
 ;;876
 ;;21,"54868454301 ")
 ;;877
 ;;21,"54868454302 ")
 ;;878
 ;;21,"54868454303 ")
 ;;879
 ;;21,"54868461400 ")
 ;;880
 ;;21,"54868461401 ")
 ;;881
 ;;21,"54868461402 ")
 ;;882
 ;;21,"54868465100 ")
 ;;883
 ;;21,"54868465400 ")
 ;;884
 ;;21,"54868465401 ")
 ;;885
 ;;21,"54868471100 ")
 ;;886
 ;;21,"54868474300 ")
 ;;887
 ;;21,"54868474301 ")
 ;;888
 ;;21,"54868474302 ")
 ;;889
 ;;21,"54868480500 ")
 ;;890
 ;;21,"54868480600 ")
 ;;891
 ;;21,"54868485800 ")
 ;;892
 ;;21,"54868485801 ")
 ;;893
 ;;21,"54868485802 ")
 ;;894
 ;;21,"54868485803 ")
 ;;895
 ;;21,"54868485804 ")
 ;;896
 ;;21,"54868485805 ")
 ;;897
 ;;21,"54868485806 ")
 ;;898
 ;;21,"54868485807 ")
 ;;899
 ;;21,"54868485808 ")
 ;;900
 ;;21,"54868489800 ")
 ;;901
 ;;21,"54868489801 ")
 ;;902
 ;;21,"54868489802 ")
 ;;903
 ;;21,"54868489803 ")
 ;;904
 ;;21,"54868489804 ")
 ;;905
 ;;21,"54868495100 ")
 ;;906
 ;;21,"54868495101 ")
 ;;907
 ;;21,"54868495102 ")
 ;;908
 ;;21,"54868495103 ")
 ;;909
 ;;21,"54868495104 ")
 ;;910
 ;;21,"54868498700 ")
 ;;911
 ;;21,"54868498701 ")
 ;;912
 ;;21,"54868499000 ")
 ;;913
 ;;21,"54868502200 ")
 ;;914
 ;;21,"54868502201 ")
 ;;915
 ;;21,"54868502202 ")
 ;;916
 ;;21,"54868502203 ")
 ;;917
 ;;21,"54868502300 ")
 ;;918
 ;;21,"54868502301 ")
 ;;919
 ;;21,"54868502302 ")
 ;;920
 ;;21,"54868504000 ")
 ;;921
 ;;21,"54868504001 ")
 ;;922
 ;;21,"54868504002 ")
 ;;923
 ;;21,"54868510100 ")
 ;;924
 ;;21,"54868510101 ")
 ;;925
 ;;21,"54868510102 ")
 ;;926
 ;;21,"54868510103 ")
 ;;927
 ;;21,"54868516500 ")
 ;;928
 ;;21,"54868516501 ")
 ;;929
 ;;21,"54868517500 ")
 ;;930
 ;;21,"54868521100 ")
 ;;931
 ;;21,"54868521101 ")
 ;;932
 ;;21,"54868521102 ")
 ;;933
 ;;21,"54868521103 ")
 ;;934
 ;;21,"54868521104 ")
 ;;935
 ;;21,"54868521105 ")
 ;;936
 ;;21,"54868526900 ")
 ;;937
 ;;21,"54868530700 ")
 ;;938
 ;;21,"54868543000 ")
 ;;939
 ;;21,"54868543001 ")
 ;;940
 ;;21,"54868543002 ")
 ;;941
 ;;21,"54868543003 ")
 ;;942
 ;;21,"54868547100 ")
 ;;943
 ;;21,"54868547801 ")
 ;;944
 ;;21,"54868547802 ")
 ;;945
 ;;21,"54868553300 ")
 ;;946
 ;;21,"54868558900 ")
 ;;947
 ;;21,"54868564700 ")
 ;;948
 ;;21,"54868564701 ")
 ;;949
 ;;21,"54868564800 ")
 ;;950
 ;;21,"54868564801 ")
 ;;951
 ;;21,"54868564802 ")
 ;;952
 ;;21,"54868569300 ")
 ;;953
 ;;21,"54868569400 ")
 ;;954
 ;;21,"54868575600 ")
 ;;955
 ;;21,"54868575700 ")
 ;;956
 ;;21,"54868576700 ")
 ;;957
 ;;21,"54868576701 ")
 ;;958
 ;;21,"54868576800 ")
 ;;959
 ;;21,"54868576801 ")
 ;;960
 ;;21,"54868576900 ")
 ;;961
 ;;21,"54868576901 ")
 ;;962
 ;;21,"54868593800 ")
 ;;963
 ;;21,"54868598100 ")
 ;;964
 ;;21,"54868619000 ")
 ;;965
 ;;21,"54868627100 ")
 ;;966
 ;;21,"54868627900 ")
 ;;967
 ;;21,"54868627901 ")
 ;;968
 ;;21,"54868687501 ")
 ;;969
 ;;21,"54907014801 ")
 ;;970
 ;;21,"54907014805 ")
 ;;971
 ;;21,"54907014901 ")
 ;;972
 ;;21,"54907014904 ")
 ;;973
 ;;21,"54907014905 ")
 ;;974
 ;;21,"54907050902 ")
 ;;975
 ;;21,"54907051503 ")
 ;;976
 ;;21,"54907052001 ")
 ;;977
 ;;21,"54907052002 ")
 ;;978
 ;;21,"54907052101 ")
 ;;979
 ;;21,"54907052102 ")
 ;;980
 ;;21,"54907058200 ")
 ;;981
 ;;21,"54907058201 ")
 ;;982
 ;;21,"54907065701 ")
 ;;983
 ;;21,"54907065702 ")
 ;;984
 ;;21,"54907065705 ")
 ;;985
 ;;21,"54907070901 ")
 ;;986
 ;;21,"54907070903 ")
 ;;987
 ;;21,"54907071001 ")
 ;;988
 ;;21,"54907071002 ")
 ;;989
 ;;21,"54907071302 ")
 ;;990
 ;;21,"54907071504 ")
 ;;991
 ;;21,"54907071505 ")
 ;;992
 ;;21,"54907071603 ")
 ;;993
 ;;21,"54907071604 ")
 ;;994
 ;;21,"54907071701 ")
 ;;995
 ;;21,"54907071708 ")
 ;;996
 ;;21,"54907072501 ")
 ;;997
 ;;21,"54907072506 ")
 ;;998
 ;;21,"54907072601 ")
 ;;999
 ;;21,"54907072606 ")
 ;;1000
 ;;21,"54907075001 ")
 ;;1001
 ;;21,"54907075006 ")
 ;;1002
 ;;21,"54907075101 ")
 ;;1003
 ;;21,"54907075102 ")
 ;;1004
 ;;21,"54907075106 ")
 ;;1005
 ;;21,"54907075107 ")
 ;;1006
 ;;21,"54907075201 ")
 ;;1007
 ;;21,"54907075202 ")
 ;;1008
 ;;21,"54907075206 ")
 ;;1009
 ;;21,"54907075207 ")
 ;;1010
 ;;21,"54907075302 ")
 ;;1011
 ;;21,"54907075402 ")
 ;;1012
 ;;21,"54907076801 ")
 ;;1013
 ;;21,"54907076802 ")
 ;;1014
 ;;21,"54907076807 ")
 ;;1015
 ;;21,"54907082103 ")
 ;;1016
 ;;21,"54907082104 ")
 ;;1017
 ;;21,"54907082203 ")
 ;;1018
 ;;21,"54907082204 ")
 ;;1019
 ;;21,"54907096001 ")
 ;;1020
 ;;21,"54907096003 ")
 ;;1021
 ;;21,"54907096004 ")
 ;;1022
 ;;21,"54907096101 ")
 ;;1023
 ;;21,"54907096103 ")
 ;;1024
 ;;21,"54907096104 ")
 ;;1025
 ;;21,"54907096304 ")
 ;;1026
 ;;21,"54907096403 ")
 ;;1027
 ;;21,"54907096404 ")
 ;;1028
 ;;21,"54907096501 ")
 ;;1029
 ;;21,"54907096503 ")
 ;;1030
 ;;21,"54907096504 ")
 ;;1031
 ;;21,"54907096601 ")
 ;;1032
 ;;21,"54907096603 ")
 ;;1033
 ;;21,"54907096604 ")
 ;;1034
 ;;21,"54907096901 ")
 ;;1035
 ;;21,"54907096903 ")
 ;;1036
 ;;21,"54907096904 ")
 ;;1037
 ;;21,"54907097001 ")
 ;;1038
 ;;21,"54907097003 ")
 ;;1039
 ;;21,"54907097004 ")
 ;;1040
 ;;21,"54907097204 ")
 ;;1041
 ;;21,"54907097304 ")
 ;;1042
 ;;21,"54907097401 ")
 ;;1043
 ;;21,"54907097404 ")
 ;;1044
 ;;21,"54907097701 ")
 ;;1045
 ;;21,"54907097703 ")
 ;;1046
 ;;21,"54907097704 ")
 ;;1047
 ;;21,"54907097901 ")
 ;;1048
 ;;21,"54907097903 ")
 ;;1049
 ;;21,"54907097904 ")
 ;;1050
 ;;21,"54907512001 ")
 ;;1051
 ;;21,"54907512005 ")
 ;;1052
 ;;21,"54907514001 ")
 ;;1053
 ;;21,"54907515001 ")
 ;;1054
 ;;21,"54907515004 ")
 ;;1055
 ;;21,"54907654001 ")
 ;;1056
 ;;21,"54907654005 ")
 ;;1057
 ;;21,"54907655001 ")
 ;;1058
 ;;21,"54907655005 ")
 ;;1059
 ;;21,"54907656001 ")
 ;;1060
 ;;21,"54907656005 ")
 ;;1061
 ;;21,"54907657001 ")
 ;;1062
 ;;21,"54907657005 ")
 ;;1063
 ;;21,"54907658001 ")
 ;;1064
 ;;21,"54907659001 ")
 ;;1065
 ;;21,"54907760002 ")
 ;;1066
 ;;21,"54907761001 ")
 ;;1067
 ;;21,"54907761002 ")
 ;;1068
 ;;21,"54907762002 ")
 ;;1069
 ;;21,"54907763001 ")
 ;;1070
 ;;21,"54907763002 ")
 ;;1071
 ;;21,"54907763005 ")
 ;;1072
 ;;21,"54907954001 ")
 ;;1073
 ;;21,"54907954002 ")
 ;;1074
 ;;21,"54907955003 ")
 ;;1075
 ;;21,"54907955004 ")
 ;;1076
 ;;21,"54907956001 ")
 ;;1077
 ;;21,"54907956002 ")
 ;;1078
 ;;21,"54907957003 ")
 ;;1079
 ;;21,"54907957004 ")
 ;;1080
 ;;21,"54907958001 ")
 ;;1081
 ;;21,"54907958002 ")
 ;;1082
 ;;21,"54907959001 ")
 ;;1083
 ;;21,"54907959002 ")
 ;;1084
 ;;21,"55045111300 ")
 ;;1085
 ;;21,"55045111301 ")
 ;;1086
 ;;21,"55045111303 ")
 ;;1087
 ;;21,"55045111307 ")
 ;;1088
 ;;21,"55045111309 ")
 ;;1089
 ;;21,"55045111700 ")
 ;;1090
 ;;21,"55045111701 ")
 ;;1091
 ;;21,"55045111702 ")
 ;;1092
 ;;21,"55045111703 ")
 ;;1093
 ;;21,"55045111704 ")
 ;;1094
 ;;21,"55045111705 ")
 ;;1095
 ;;21,"55045111706 ")
 ;;1096
 ;;21,"55045111707 ")
 ;;1097
 ;;21,"55045111708 ")
 ;;1098
 ;;21,"55045111709 ")
 ;;1099
 ;;21,"55045112200 ")
 ;;1100
 ;;21,"55045112201 ")
 ;;1101
 ;;21,"55045112202 ")
 ;;1102
 ;;21,"55045112203 ")
 ;;1103
 ;;21,"55045112204 ")
 ;;1104
 ;;21,"55045112205 ")
 ;;1105
 ;;21,"55045112207 ")
 ;;1106
 ;;21,"55045112208 ")
 ;;1107
 ;;21,"55045112209 ")
 ;;1108
 ;;21,"55045116803 ")
 ;;1109
 ;;21,"55045116804 ")
 ;;1110
 ;;21,"55045116806 ")
 ;;1111
 ;;21,"55045116807 ")
 ;;1112
 ;;21,"55045116808 ")
 ;;1113
 ;;21,"55045117000 ")
 ;;1114
 ;;21,"55045117001 ")
 ;;1115
 ;;21,"55045117002 ")
 ;;1116
 ;;21,"55045117003 ")
 ;;1117
 ;;21,"55045117004 ")
 ;;1118
 ;;21,"55045117005 ")
 ;;1119
 ;;21,"55045117006 ")
 ;;1120
 ;;21,"55045117007 ")
 ;;1121
 ;;21,"55045117008 ")
 ;;1122
 ;;21,"55045117009 ")
 ;;1123
 ;;21,"55045117801 ")
 ;;1124
 ;;21,"55045117802 ")
 ;;1125
 ;;21,"55045117901 ")
 ;;1126
 ;;21,"55045117902 ")
 ;;1127
 ;;21,"55045118800 ")
 ;;1128
 ;;21,"55045118801 ")
 ;;1129
 ;;21,"55045118901 ")
 ;;1130
 ;;21,"55045119501 ")
 ;;1131
 ;;21,"55045119502 ")
 ;;1132
 ;;21,"55045119903 ")
 ;;1133
 ;;21,"55045120000 ")
 ;;1134
 ;;21,"55045120003 ")
 ;;1135
 ;;21,"55045120101 ")
 ;;1136
 ;;21,"55045120102 ")
 ;;1137
 ;;21,"55045120103 ")
 ;;1138
 ;;21,"55045120107 ")
 ;;1139
 ;;21,"55045120108 ")
 ;;1140
 ;;21,"55045120200 ")
 ;;1141
 ;;21,"55045120201 ")
 ;;1142
 ;;21,"55045120202 ")
 ;;1143
 ;;21,"55045120203 ")
 ;;1144
 ;;21,"55045120204 ")
 ;;1145
 ;;21,"55045120205 ")
 ;;1146
 ;;21,"55045120206 ")
 ;;1147
 ;;21,"55045120207 ")
 ;;1148
 ;;21,"55045120208 ")
 ;;1149
 ;;21,"55045120209 ")
 ;;1150
 ;;21,"55045120403 ")
 ;;1151
 ;;21,"55045120405 ")
 ;;1152
 ;;21,"55045120407 ")
 ;;1153
 ;;21,"55045120409 ")
 ;;1154
 ;;21,"55045120601 ")
 ;;1155
