BGPM5BB ;IHS/MSC/SAT-CREATED BY ^ATXSTX ON SEP 12, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;21,"58016070148 ")
 ;;3707
 ;;21,"58016070150 ")
 ;;3708
 ;;21,"58016070156 ")
 ;;3709
 ;;21,"58016070160 ")
 ;;3710
 ;;21,"58016070167 ")
 ;;3711
 ;;21,"58016070169 ")
 ;;3712
 ;;21,"58016070170 ")
 ;;3713
 ;;21,"58016070171 ")
 ;;3714
 ;;21,"58016070172 ")
 ;;3715
 ;;21,"58016070173 ")
 ;;3716
 ;;21,"58016070175 ")
 ;;3717
 ;;21,"58016070176 ")
 ;;3718
 ;;21,"58016070179 ")
 ;;3719
 ;;21,"58016070180 ")
 ;;3720
 ;;21,"58016070181 ")
 ;;3721
 ;;21,"58016070182 ")
 ;;3722
 ;;21,"58016070183 ")
 ;;3723
 ;;21,"58016070184 ")
 ;;3724
 ;;21,"58016070187 ")
 ;;3725
 ;;21,"58016070189 ")
 ;;3726
 ;;21,"58016070190 ")
 ;;3727
 ;;21,"58016070191 ")
 ;;3728
 ;;21,"58016070192 ")
 ;;3729
 ;;21,"58016070193 ")
 ;;3730
 ;;21,"58016070196 ")
 ;;3731
 ;;21,"58016070197 ")
 ;;3732
 ;;21,"58016070198 ")
 ;;3733
 ;;21,"58016070199 ")
 ;;3734
 ;;21,"58016070400 ")
 ;;3735
 ;;21,"58016070401 ")
 ;;3736
 ;;21,"58016070402 ")
 ;;3737
 ;;21,"58016070403 ")
 ;;3738
 ;;21,"58016070404 ")
 ;;3739
 ;;21,"58016070405 ")
 ;;3740
 ;;21,"58016070406 ")
 ;;3741
 ;;21,"58016070407 ")
 ;;3742
 ;;21,"58016070408 ")
 ;;3743
 ;;21,"58016070409 ")
 ;;3744
 ;;21,"58016070410 ")
 ;;3745
 ;;21,"58016070412 ")
 ;;3746
 ;;21,"58016070414 ")
 ;;3747
 ;;21,"58016070415 ")
 ;;3748
 ;;21,"58016070416 ")
 ;;3749
 ;;21,"58016070418 ")
 ;;3750
 ;;21,"58016070420 ")
 ;;3751
 ;;21,"58016070421 ")
 ;;3752
 ;;21,"58016070424 ")
 ;;3753
 ;;21,"58016070425 ")
 ;;3754
 ;;21,"58016070426 ")
 ;;3755
 ;;21,"58016070427 ")
 ;;3756
 ;;21,"58016070428 ")
 ;;3757
 ;;21,"58016070430 ")
 ;;3758
 ;;21,"58016070432 ")
 ;;3759
 ;;21,"58016070435 ")
 ;;3760
 ;;21,"58016070436 ")
 ;;3761
 ;;21,"58016070440 ")
 ;;3762
 ;;21,"58016070442 ")
 ;;3763
 ;;21,"58016070444 ")
 ;;3764
 ;;21,"58016070445 ")
 ;;3765
 ;;21,"58016070448 ")
 ;;3766
 ;;21,"58016070450 ")
 ;;3767
 ;;21,"58016070456 ")
 ;;3768
 ;;21,"58016070460 ")
 ;;3769
 ;;21,"58016070467 ")
 ;;3770
 ;;21,"58016070469 ")
 ;;3771
 ;;21,"58016070470 ")
 ;;3772
 ;;21,"58016070471 ")
 ;;3773
 ;;21,"58016070472 ")
 ;;3774
 ;;21,"58016070473 ")
 ;;3775
 ;;21,"58016070475 ")
 ;;3776
 ;;21,"58016070476 ")
 ;;3777
 ;;21,"58016070477 ")
 ;;3778
 ;;21,"58016070479 ")
 ;;3779
 ;;21,"58016070480 ")
 ;;3780
 ;;21,"58016070481 ")
 ;;3781
 ;;21,"58016070482 ")
 ;;3782
 ;;21,"58016070483 ")
 ;;3783
 ;;21,"58016070484 ")
 ;;3784
 ;;21,"58016070487 ")
 ;;3785
 ;;21,"58016070489 ")
 ;;3786
 ;;21,"58016070490 ")
 ;;3787
 ;;21,"58016070491 ")
 ;;3788
 ;;21,"58016070492 ")
 ;;3789
 ;;21,"58016070493 ")
 ;;3790
 ;;21,"58016070496 ")
 ;;3791
 ;;21,"58016070497 ")
 ;;3792
 ;;21,"58016070498 ")
 ;;3793
 ;;21,"58016070499 ")
 ;;3794
 ;;21,"58016071000 ")
 ;;3795
 ;;21,"58016071030 ")
 ;;3796
 ;;21,"58016071060 ")
 ;;3797
 ;;21,"58016071090 ")
 ;;3798
 ;;21,"58016071099 ")
 ;;3799
 ;;21,"58016074900 ")
 ;;3800
 ;;21,"58016074902 ")
 ;;3801
 ;;21,"58016074930 ")
 ;;3802
 ;;21,"58016074960 ")
 ;;3803
 ;;21,"58016074990 ")
 ;;3804
 ;;21,"58016080800 ")
 ;;3805
 ;;21,"58016080801 ")
 ;;3806
 ;;21,"58016080802 ")
 ;;3807
 ;;21,"58016080803 ")
 ;;3808
 ;;21,"58016080804 ")
 ;;3809
 ;;21,"58016080805 ")
 ;;3810
 ;;21,"58016080806 ")
 ;;3811
 ;;21,"58016080807 ")
 ;;3812
 ;;21,"58016080808 ")
 ;;3813
 ;;21,"58016080809 ")
 ;;3814
 ;;21,"58016080810 ")
 ;;3815
 ;;21,"58016080812 ")
 ;;3816
 ;;21,"58016080814 ")
 ;;3817
 ;;21,"58016080815 ")
 ;;3818
 ;;21,"58016080816 ")
 ;;3819
 ;;21,"58016080818 ")
 ;;3820
 ;;21,"58016080820 ")
 ;;3821
 ;;21,"58016080821 ")
 ;;3822
 ;;21,"58016080824 ")
 ;;3823
 ;;21,"58016080825 ")
 ;;3824
 ;;21,"58016080826 ")
 ;;3825
 ;;21,"58016080827 ")
 ;;3826
 ;;21,"58016080828 ")
 ;;3827
 ;;21,"58016080830 ")
 ;;3828
 ;;21,"58016080832 ")
 ;;3829
 ;;21,"58016080835 ")
 ;;3830
 ;;21,"58016080836 ")
 ;;3831
 ;;21,"58016080840 ")
 ;;3832
 ;;21,"58016080842 ")
 ;;3833
 ;;21,"58016080844 ")
 ;;3834
 ;;21,"58016080845 ")
 ;;3835
 ;;21,"58016080848 ")
 ;;3836
 ;;21,"58016080850 ")
 ;;3837
 ;;21,"58016080856 ")
 ;;3838
 ;;21,"58016080860 ")
 ;;3839
 ;;21,"58016080867 ")
 ;;3840
 ;;21,"58016080869 ")
 ;;3841
 ;;21,"58016080870 ")
 ;;3842
 ;;21,"58016080871 ")
 ;;3843
 ;;21,"58016080872 ")
 ;;3844
 ;;21,"58016080873 ")
 ;;3845
 ;;21,"58016080875 ")
 ;;3846
 ;;21,"58016080876 ")
 ;;3847
 ;;21,"58016080877 ")
 ;;3848
 ;;21,"58016080879 ")
 ;;3849
 ;;21,"58016080880 ")
 ;;3850
 ;;21,"58016080881 ")
 ;;3851
 ;;21,"58016080882 ")
 ;;3852
 ;;21,"58016080883 ")
 ;;3853
 ;;21,"58016080884 ")
 ;;3854
 ;;21,"58016080887 ")
 ;;3855
 ;;21,"58016080889 ")
 ;;3856
 ;;21,"58016080890 ")
 ;;3857
 ;;21,"58016080891 ")
 ;;3858
 ;;21,"58016080892 ")
 ;;3859
 ;;21,"58016080893 ")
 ;;3860
 ;;21,"58016080896 ")
 ;;3861
 ;;21,"58016080897 ")
 ;;3862
 ;;21,"58016080898 ")
 ;;3863
 ;;21,"58016080899 ")
 ;;3864
 ;;21,"58016081300 ")
 ;;3865
 ;;21,"58016081302 ")
 ;;3866
 ;;21,"58016081303 ")
 ;;3867
 ;;21,"58016081310 ")
 ;;3868
 ;;21,"58016081312 ")
 ;;3869
 ;;21,"58016081315 ")
 ;;3870
 ;;21,"58016081320 ")
 ;;3871
 ;;21,"58016081324 ")
 ;;3872
 ;;21,"58016081330 ")
 ;;3873
 ;;21,"58016081350 ")
 ;;3874
 ;;21,"58016081360 ")
 ;;3875
 ;;21,"58016081373 ")
 ;;3876
 ;;21,"58016081389 ")
 ;;3877
 ;;21,"58016081390 ")
 ;;3878
 ;;21,"58016081399 ")
 ;;3879
 ;;21,"58016081400 ")
 ;;3880
 ;;21,"58016081402 ")
 ;;3881
 ;;21,"58016081415 ")
 ;;3882
 ;;21,"58016081420 ")
 ;;3883
 ;;21,"58016081425 ")
 ;;3884
 ;;21,"58016081430 ")
 ;;3885
 ;;21,"58016081440 ")
 ;;3886
 ;;21,"58016081445 ")
 ;;3887
 ;;21,"58016081450 ")
 ;;3888
 ;;21,"58016081460 ")
 ;;3889
 ;;21,"58016081489 ")
 ;;3890
 ;;21,"58016081490 ")
 ;;3891
 ;;21,"58016081499 ")
 ;;3892
 ;;21,"58016081500 ")
 ;;3893
 ;;21,"58016081515 ")
 ;;3894
 ;;21,"58016081520 ")
 ;;3895
 ;;21,"58016081521 ")
 ;;3896
 ;;21,"58016081528 ")
 ;;3897
 ;;21,"58016081530 ")
 ;;3898
 ;;21,"58016081560 ")
 ;;3899
 ;;21,"58016081590 ")
 ;;3900
 ;;21,"58016081599 ")
 ;;3901
 ;;21,"58016081700 ")
 ;;3902
 ;;21,"58016081701 ")
 ;;3903
 ;;21,"58016081702 ")
 ;;3904
 ;;21,"58016081703 ")
 ;;3905
 ;;21,"58016081704 ")
 ;;3906
 ;;21,"58016081705 ")
 ;;3907
 ;;21,"58016081706 ")
 ;;3908
 ;;21,"58016081707 ")
 ;;3909
 ;;21,"58016081708 ")
 ;;3910
 ;;21,"58016081709 ")
 ;;3911
 ;;21,"58016081710 ")
 ;;3912
 ;;21,"58016081712 ")
 ;;3913
 ;;21,"58016081714 ")
 ;;3914
 ;;21,"58016081715 ")
 ;;3915
 ;;21,"58016081716 ")
 ;;3916
 ;;21,"58016081718 ")
 ;;3917
 ;;21,"58016081720 ")
 ;;3918
 ;;21,"58016081721 ")
 ;;3919
 ;;21,"58016081724 ")
 ;;3920
 ;;21,"58016081725 ")
 ;;3921
 ;;21,"58016081726 ")
 ;;3922
 ;;21,"58016081727 ")
 ;;3923
 ;;21,"58016081728 ")
 ;;3924
 ;;21,"58016081730 ")
 ;;3925
 ;;21,"58016081732 ")
 ;;3926
 ;;21,"58016081735 ")
 ;;3927
 ;;21,"58016081736 ")
 ;;3928
 ;;21,"58016081740 ")
 ;;3929
 ;;21,"58016081742 ")
 ;;3930
 ;;21,"58016081744 ")
 ;;3931
 ;;21,"58016081745 ")
 ;;3932
 ;;21,"58016081748 ")
 ;;3933
 ;;21,"58016081750 ")
 ;;3934
 ;;21,"58016081756 ")
 ;;3935
 ;;21,"58016081760 ")
 ;;3936
 ;;21,"58016081767 ")
 ;;3937
 ;;21,"58016081769 ")
 ;;3938
 ;;21,"58016081770 ")
 ;;3939
 ;;21,"58016081771 ")
 ;;3940
 ;;21,"58016081772 ")
 ;;3941
 ;;21,"58016081773 ")
 ;;3942
 ;;21,"58016081775 ")
 ;;3943
 ;;21,"58016081776 ")
 ;;3944
 ;;21,"58016081777 ")
 ;;3945
 ;;21,"58016081779 ")
 ;;3946
 ;;21,"58016081780 ")
 ;;3947
 ;;21,"58016081781 ")
 ;;3948
 ;;21,"58016081782 ")
 ;;3949
 ;;21,"58016081783 ")
 ;;3950
 ;;21,"58016081784 ")
 ;;3951
 ;;21,"58016081787 ")
 ;;3952
 ;;21,"58016081789 ")
 ;;3953
 ;;21,"58016081790 ")
 ;;3954
 ;;21,"58016081791 ")
 ;;3955
 ;;21,"58016081792 ")
 ;;3956
 ;;21,"58016081793 ")
 ;;3957
 ;;21,"58016081796 ")
 ;;3958
 ;;21,"58016081797 ")
 ;;3959
 ;;21,"58016081798 ")
 ;;3960
 ;;21,"58016081799 ")
 ;;3961
 ;;21,"58016081800 ")
 ;;3962
 ;;21,"58016081801 ")
 ;;3963
 ;;21,"58016081802 ")
 ;;3964
 ;;21,"58016081803 ")
 ;;3965
 ;;21,"58016081804 ")
 ;;3966
 ;;21,"58016081805 ")
 ;;3967
 ;;21,"58016081806 ")
 ;;3968
 ;;21,"58016081807 ")
 ;;3969
 ;;21,"58016081808 ")
 ;;3970
 ;;21,"58016081809 ")
 ;;3971
 ;;21,"58016081810 ")
 ;;3972
 ;;21,"58016081812 ")
 ;;3973
 ;;21,"58016081814 ")
 ;;3974
 ;;21,"58016081815 ")
 ;;3975
 ;;21,"58016081816 ")
 ;;3976
 ;;21,"58016081818 ")
 ;;3977
 ;;21,"58016081820 ")
 ;;3978
 ;;21,"58016081821 ")
 ;;3979
 ;;21,"58016081824 ")
 ;;3980
 ;;21,"58016081825 ")
 ;;3981
 ;;21,"58016081826 ")
 ;;3982
 ;;21,"58016081827 ")
 ;;3983
 ;;21,"58016081828 ")
 ;;3984
 ;;21,"58016081830 ")
 ;;3985
 ;;21,"58016081832 ")
 ;;3986
 ;;21,"58016081835 ")
 ;;3987
 ;;21,"58016081836 ")
 ;;3988
 ;;21,"58016081840 ")
 ;;3989
 ;;21,"58016081842 ")
 ;;3990
 ;;21,"58016081844 ")
 ;;3991
 ;;21,"58016081845 ")
 ;;3992
 ;;21,"58016081848 ")
 ;;3993
 ;;21,"58016081850 ")
 ;;3994
 ;;21,"58016081856 ")
 ;;3995
 ;;21,"58016081860 ")
 ;;3996
 ;;21,"58016081867 ")
 ;;3997
 ;;21,"58016081869 ")
 ;;3998
 ;;21,"58016081870 ")
 ;;3999
 ;;21,"58016081871 ")
 ;;4000
 ;;21,"58016081872 ")
 ;;4001
 ;;21,"58016081873 ")
 ;;4002
 ;;21,"58016081875 ")
 ;;4003
 ;;21,"58016081876 ")
 ;;4004
 ;;21,"58016081877 ")
 ;;4005
 ;;21,"58016081879 ")
 ;;4006
 ;;21,"58016081880 ")
 ;;4007
 ;;21,"58016081881 ")
 ;;4008
 ;;21,"58016081882 ")
 ;;4009
 ;;21,"58016081883 ")
 ;;4010
 ;;21,"58016081884 ")
 ;;4011
 ;;21,"58016081887 ")
 ;;4012
 ;;21,"58016081889 ")
 ;;4013
 ;;21,"58016081890 ")
 ;;4014
 ;;21,"58016081891 ")
 ;;4015
 ;;21,"58016081892 ")
 ;;4016
 ;;21,"58016081893 ")
 ;;4017
 ;;21,"58016081896 ")
 ;;4018
 ;;21,"58016081897 ")
 ;;4019
 ;;21,"58016081898 ")
 ;;4020
 ;;21,"58016081899 ")
 ;;4021
 ;;21,"58016083900 ")
 ;;4022
 ;;21,"58016083902 ")
 ;;4023
 ;;21,"58016083912 ")
 ;;4024
 ;;21,"58016083915 ")
 ;;4025
 ;;21,"58016083920 ")
 ;;4026
 ;;21,"58016083930 ")
 ;;4027
 ;;21,"58016083960 ")
 ;;4028
 ;;21,"58016083990 ")
 ;;4029
 ;;21,"58016084100 ")
 ;;4030
 ;;21,"58016084102 ")
 ;;4031
 ;;21,"58016084110 ")
 ;;4032
 ;;21,"58016084112 ")
 ;;4033
 ;;21,"58016084115 ")
 ;;4034
 ;;21,"58016084120 ")
 ;;4035
 ;;21,"58016084130 ")
 ;;4036
 ;;21,"58016084160 ")
 ;;4037
 ;;21,"58016084190 ")
 ;;4038
 ;;21,"58016085800 ")
 ;;4039
 ;;21,"58016085814 ")
 ;;4040
 ;;21,"58016085821 ")
 ;;4041
 ;;21,"58016085828 ")
 ;;4042
 ;;21,"58016085830 ")
 ;;4043
 ;;21,"58016085840 ")
 ;;4044
 ;;21,"58016085850 ")
 ;;4045
 ;;21,"58016085860 ")
 ;;4046
 ;;21,"58016085899 ")
 ;;4047
 ;;21,"58016086200 ")
 ;;4048
 ;;21,"58016086202 ")
 ;;4049
 ;;21,"58016086203 ")
 ;;4050
 ;;21,"58016086212 ")
 ;;4051
 ;;21,"58016086215 ")
 ;;4052
 ;;21,"58016086220 ")
 ;;4053
 ;;21,"58016086230 ")
 ;;4054
 ;;21,"58016086250 ")
 ;;4055
 ;;21,"58016086260 ")
 ;;4056
 ;;21,"58016086273 ")
 ;;4057
 ;;21,"58016086289 ")
 ;;4058
