AMQQPIR ; IHS/CMI/THL - Q-MAN PRE-INIT ROUTINE ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;-----
 N X,Y,N,%
LAB K ^UTILITY("AMQQ",$J,"SAVE")
 I $O(^AMQQ(5,1000)) W !!,"One moment please..."
 F X=1000:0 S X=$O(^AMQQ(5,X)) Q:'X  D SAVE W "."
 K ^AMQQ(1),^(5),^(4),^(7)
SET F %=9009071,9009074,9009075,9009077 S ^UTILITY("XBDSET",$J,%)="D^D"
 D EN2^XBKD
 K ^UTILITY("XBDSET",$J)
 Q
 ;
SAVE S Y="^AMQQ(5,"_X_")"
 F N=1:1 S Y=$Q(@Y) Q:$P(Y,",",2)'=X  S ^UTILITY("AMQQ",$J,"SAVE",X,N)=Y_"|"_@Y
 Q
