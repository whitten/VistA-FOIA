BGPM5ALY ;IHS/MSC/MMT-CREATED BY ^ATXSTX ON JUL 15, 2011;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;**1**;JUN 27, 2011;Build 106
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"900,58016005260 ",.01)
 ;;58016005260
 ;;9002226.02101,"900,58016005260 ",.02)
 ;;58016005260
 ;;9002226.02101,"900,58016005290 ",.01)
 ;;58016005290
 ;;9002226.02101,"900,58016005290 ",.02)
 ;;58016005290
 ;;9002226.02101,"900,58016007100 ",.01)
 ;;58016007100
 ;;9002226.02101,"900,58016007100 ",.02)
 ;;58016007100
 ;;9002226.02101,"900,58016007130 ",.01)
 ;;58016007130
 ;;9002226.02101,"900,58016007130 ",.02)
 ;;58016007130
 ;;9002226.02101,"900,58016007160 ",.01)
 ;;58016007160
 ;;9002226.02101,"900,58016007160 ",.02)
 ;;58016007160
 ;;9002226.02101,"900,58016007190 ",.01)
 ;;58016007190
 ;;9002226.02101,"900,58016007190 ",.02)
 ;;58016007190
 ;;9002226.02101,"900,58016024590 ",.01)
 ;;58016024590
 ;;9002226.02101,"900,58016024590 ",.02)
 ;;58016024590
 ;;9002226.02101,"900,58016025200 ",.01)
 ;;58016025200
 ;;9002226.02101,"900,58016025200 ",.02)
 ;;58016025200
 ;;9002226.02101,"900,58016025201 ",.01)
 ;;58016025201
 ;;9002226.02101,"900,58016025201 ",.02)
 ;;58016025201
 ;;9002226.02101,"900,58016025202 ",.01)
 ;;58016025202
 ;;9002226.02101,"900,58016025202 ",.02)
 ;;58016025202
 ;;9002226.02101,"900,58016025203 ",.01)
 ;;58016025203
 ;;9002226.02101,"900,58016025203 ",.02)
 ;;58016025203
 ;;9002226.02101,"900,58016025204 ",.01)
 ;;58016025204
 ;;9002226.02101,"900,58016025204 ",.02)
 ;;58016025204
 ;;9002226.02101,"900,58016025205 ",.01)
 ;;58016025205
 ;;9002226.02101,"900,58016025205 ",.02)
 ;;58016025205
 ;;9002226.02101,"900,58016025206 ",.01)
 ;;58016025206
 ;;9002226.02101,"900,58016025206 ",.02)
 ;;58016025206
 ;;9002226.02101,"900,58016025207 ",.01)
 ;;58016025207
 ;;9002226.02101,"900,58016025207 ",.02)
 ;;58016025207
 ;;9002226.02101,"900,58016025208 ",.01)
 ;;58016025208
 ;;9002226.02101,"900,58016025208 ",.02)
 ;;58016025208
 ;;9002226.02101,"900,58016025209 ",.01)
 ;;58016025209
 ;;9002226.02101,"900,58016025209 ",.02)
 ;;58016025209
 ;;9002226.02101,"900,58016025210 ",.01)
 ;;58016025210
 ;;9002226.02101,"900,58016025210 ",.02)
 ;;58016025210
 ;;9002226.02101,"900,58016025212 ",.01)
 ;;58016025212
 ;;9002226.02101,"900,58016025212 ",.02)
 ;;58016025212
 ;;9002226.02101,"900,58016025214 ",.01)
 ;;58016025214
 ;;9002226.02101,"900,58016025214 ",.02)
 ;;58016025214
 ;;9002226.02101,"900,58016025215 ",.01)
 ;;58016025215
 ;;9002226.02101,"900,58016025215 ",.02)
 ;;58016025215
 ;;9002226.02101,"900,58016025216 ",.01)
 ;;58016025216
 ;;9002226.02101,"900,58016025216 ",.02)
 ;;58016025216
 ;;9002226.02101,"900,58016025218 ",.01)
 ;;58016025218
 ;;9002226.02101,"900,58016025218 ",.02)
 ;;58016025218
 ;;9002226.02101,"900,58016025220 ",.01)
 ;;58016025220
 ;;9002226.02101,"900,58016025220 ",.02)
 ;;58016025220
 ;;9002226.02101,"900,58016025221 ",.01)
 ;;58016025221
 ;;9002226.02101,"900,58016025221 ",.02)
 ;;58016025221
 ;;9002226.02101,"900,58016025224 ",.01)
 ;;58016025224
 ;;9002226.02101,"900,58016025224 ",.02)
 ;;58016025224
 ;;9002226.02101,"900,58016025225 ",.01)
 ;;58016025225
 ;;9002226.02101,"900,58016025225 ",.02)
 ;;58016025225
 ;;9002226.02101,"900,58016025226 ",.01)
 ;;58016025226
 ;;9002226.02101,"900,58016025226 ",.02)
 ;;58016025226
 ;;9002226.02101,"900,58016025227 ",.01)
 ;;58016025227
 ;;9002226.02101,"900,58016025227 ",.02)
 ;;58016025227
 ;;9002226.02101,"900,58016025228 ",.01)
 ;;58016025228
 ;;9002226.02101,"900,58016025228 ",.02)
 ;;58016025228
 ;;9002226.02101,"900,58016025230 ",.01)
 ;;58016025230
 ;;9002226.02101,"900,58016025230 ",.02)
 ;;58016025230
 ;;9002226.02101,"900,58016025232 ",.01)
 ;;58016025232
 ;;9002226.02101,"900,58016025232 ",.02)
 ;;58016025232
 ;;9002226.02101,"900,58016025235 ",.01)
 ;;58016025235
 ;;9002226.02101,"900,58016025235 ",.02)
 ;;58016025235
 ;;9002226.02101,"900,58016025236 ",.01)
 ;;58016025236
 ;;9002226.02101,"900,58016025236 ",.02)
 ;;58016025236
 ;;9002226.02101,"900,58016025240 ",.01)
 ;;58016025240
 ;;9002226.02101,"900,58016025240 ",.02)
 ;;58016025240
 ;;9002226.02101,"900,58016025242 ",.01)
 ;;58016025242
 ;;9002226.02101,"900,58016025242 ",.02)
 ;;58016025242
 ;;9002226.02101,"900,58016025244 ",.01)
 ;;58016025244
 ;;9002226.02101,"900,58016025244 ",.02)
 ;;58016025244
 ;;9002226.02101,"900,58016025245 ",.01)
 ;;58016025245
 ;;9002226.02101,"900,58016025245 ",.02)
 ;;58016025245
 ;;9002226.02101,"900,58016025248 ",.01)
 ;;58016025248
 ;;9002226.02101,"900,58016025248 ",.02)
 ;;58016025248
 ;;9002226.02101,"900,58016025250 ",.01)
 ;;58016025250
 ;;9002226.02101,"900,58016025250 ",.02)
 ;;58016025250
 ;;9002226.02101,"900,58016025256 ",.01)
 ;;58016025256
 ;;9002226.02101,"900,58016025256 ",.02)
 ;;58016025256
 ;;9002226.02101,"900,58016025260 ",.01)
 ;;58016025260
 ;;9002226.02101,"900,58016025260 ",.02)
 ;;58016025260
 ;;9002226.02101,"900,58016025267 ",.01)
 ;;58016025267
 ;;9002226.02101,"900,58016025267 ",.02)
 ;;58016025267
 ;;9002226.02101,"900,58016025269 ",.01)
 ;;58016025269
 ;;9002226.02101,"900,58016025269 ",.02)
 ;;58016025269
 ;;9002226.02101,"900,58016025270 ",.01)
 ;;58016025270
 ;;9002226.02101,"900,58016025270 ",.02)
 ;;58016025270
 ;;9002226.02101,"900,58016025271 ",.01)
 ;;58016025271
 ;;9002226.02101,"900,58016025271 ",.02)
 ;;58016025271
 ;;9002226.02101,"900,58016025272 ",.01)
 ;;58016025272
 ;;9002226.02101,"900,58016025272 ",.02)
 ;;58016025272
 ;;9002226.02101,"900,58016025273 ",.01)
 ;;58016025273
 ;;9002226.02101,"900,58016025273 ",.02)
 ;;58016025273
 ;;9002226.02101,"900,58016025275 ",.01)
 ;;58016025275
 ;;9002226.02101,"900,58016025275 ",.02)
 ;;58016025275
 ;;9002226.02101,"900,58016025276 ",.01)
 ;;58016025276
 ;;9002226.02101,"900,58016025276 ",.02)
 ;;58016025276
 ;;9002226.02101,"900,58016025277 ",.01)
 ;;58016025277
 ;;9002226.02101,"900,58016025277 ",.02)
 ;;58016025277
 ;;9002226.02101,"900,58016025279 ",.01)
 ;;58016025279
 ;;9002226.02101,"900,58016025279 ",.02)
 ;;58016025279
 ;;9002226.02101,"900,58016025280 ",.01)
 ;;58016025280
 ;;9002226.02101,"900,58016025280 ",.02)
 ;;58016025280
 ;;9002226.02101,"900,58016025281 ",.01)
 ;;58016025281
 ;;9002226.02101,"900,58016025281 ",.02)
 ;;58016025281
 ;;9002226.02101,"900,58016025282 ",.01)
 ;;58016025282
 ;;9002226.02101,"900,58016025282 ",.02)
 ;;58016025282
 ;;9002226.02101,"900,58016025283 ",.01)
 ;;58016025283
 ;;9002226.02101,"900,58016025283 ",.02)
 ;;58016025283
 ;;9002226.02101,"900,58016025284 ",.01)
 ;;58016025284
 ;;9002226.02101,"900,58016025284 ",.02)
 ;;58016025284
 ;;9002226.02101,"900,58016025287 ",.01)
 ;;58016025287
 ;;9002226.02101,"900,58016025287 ",.02)
 ;;58016025287
 ;;9002226.02101,"900,58016025289 ",.01)
 ;;58016025289
 ;;9002226.02101,"900,58016025289 ",.02)
 ;;58016025289
 ;;9002226.02101,"900,58016025290 ",.01)
 ;;58016025290
 ;;9002226.02101,"900,58016025290 ",.02)
 ;;58016025290
 ;;9002226.02101,"900,58016025291 ",.01)
 ;;58016025291
 ;;9002226.02101,"900,58016025291 ",.02)
 ;;58016025291
 ;;9002226.02101,"900,58016025292 ",.01)
 ;;58016025292
 ;;9002226.02101,"900,58016025292 ",.02)
 ;;58016025292
 ;;9002226.02101,"900,58016025293 ",.01)
 ;;58016025293
 ;;9002226.02101,"900,58016025293 ",.02)
 ;;58016025293
 ;;9002226.02101,"900,58016025296 ",.01)
 ;;58016025296
 ;;9002226.02101,"900,58016025296 ",.02)
 ;;58016025296
 ;;9002226.02101,"900,58016025297 ",.01)
 ;;58016025297
 ;;9002226.02101,"900,58016025297 ",.02)
 ;;58016025297
 ;;9002226.02101,"900,58016025298 ",.01)
 ;;58016025298
 ;;9002226.02101,"900,58016025298 ",.02)
 ;;58016025298
 ;;9002226.02101,"900,58016025299 ",.01)
 ;;58016025299
 ;;9002226.02101,"900,58016025299 ",.02)
 ;;58016025299
 ;;9002226.02101,"900,58016025300 ",.01)
 ;;58016025300
 ;;9002226.02101,"900,58016025300 ",.02)
 ;;58016025300
 ;;9002226.02101,"900,58016025301 ",.01)
 ;;58016025301
 ;;9002226.02101,"900,58016025301 ",.02)
 ;;58016025301
 ;;9002226.02101,"900,58016025302 ",.01)
 ;;58016025302
 ;;9002226.02101,"900,58016025302 ",.02)
 ;;58016025302
 ;;9002226.02101,"900,58016025303 ",.01)
 ;;58016025303
 ;;9002226.02101,"900,58016025303 ",.02)
 ;;58016025303
 ;;9002226.02101,"900,58016025304 ",.01)
 ;;58016025304
 ;;9002226.02101,"900,58016025304 ",.02)
 ;;58016025304
 ;;9002226.02101,"900,58016025305 ",.01)
 ;;58016025305
 ;;9002226.02101,"900,58016025305 ",.02)
 ;;58016025305
 ;;9002226.02101,"900,58016025306 ",.01)
 ;;58016025306
 ;;9002226.02101,"900,58016025306 ",.02)
 ;;58016025306
 ;;9002226.02101,"900,58016025307 ",.01)
 ;;58016025307
 ;;9002226.02101,"900,58016025307 ",.02)
 ;;58016025307
 ;;9002226.02101,"900,58016025308 ",.01)
 ;;58016025308
 ;;9002226.02101,"900,58016025308 ",.02)
 ;;58016025308
 ;;9002226.02101,"900,58016025309 ",.01)
 ;;58016025309
 ;;9002226.02101,"900,58016025309 ",.02)
 ;;58016025309
 ;;9002226.02101,"900,58016025310 ",.01)
 ;;58016025310
 ;;9002226.02101,"900,58016025310 ",.02)
 ;;58016025310
 ;;9002226.02101,"900,58016025312 ",.01)
 ;;58016025312
 ;;9002226.02101,"900,58016025312 ",.02)
 ;;58016025312
 ;;9002226.02101,"900,58016025314 ",.01)
 ;;58016025314
 ;;9002226.02101,"900,58016025314 ",.02)
 ;;58016025314
 ;;9002226.02101,"900,58016025315 ",.01)
 ;;58016025315
 ;;9002226.02101,"900,58016025315 ",.02)
 ;;58016025315
 ;;9002226.02101,"900,58016025316 ",.01)
 ;;58016025316
 ;;9002226.02101,"900,58016025316 ",.02)
 ;;58016025316
 ;;9002226.02101,"900,58016025318 ",.01)
 ;;58016025318
 ;;9002226.02101,"900,58016025318 ",.02)
 ;;58016025318
 ;;9002226.02101,"900,58016025320 ",.01)
 ;;58016025320
 ;;9002226.02101,"900,58016025320 ",.02)
 ;;58016025320
 ;;9002226.02101,"900,58016025321 ",.01)
 ;;58016025321
 ;;9002226.02101,"900,58016025321 ",.02)
 ;;58016025321
 ;;9002226.02101,"900,58016025324 ",.01)
 ;;58016025324
 ;;9002226.02101,"900,58016025324 ",.02)
 ;;58016025324
 ;;9002226.02101,"900,58016025325 ",.01)
 ;;58016025325
 ;;9002226.02101,"900,58016025325 ",.02)
 ;;58016025325
 ;;9002226.02101,"900,58016025326 ",.01)
 ;;58016025326
 ;;9002226.02101,"900,58016025326 ",.02)
 ;;58016025326
 ;;9002226.02101,"900,58016025327 ",.01)
 ;;58016025327
 ;;9002226.02101,"900,58016025327 ",.02)
 ;;58016025327
 ;;9002226.02101,"900,58016025328 ",.01)
 ;;58016025328
 ;;9002226.02101,"900,58016025328 ",.02)
 ;;58016025328
 ;;9002226.02101,"900,58016025330 ",.01)
 ;;58016025330
 ;;9002226.02101,"900,58016025330 ",.02)
 ;;58016025330
 ;;9002226.02101,"900,58016025332 ",.01)
 ;;58016025332
 ;;9002226.02101,"900,58016025332 ",.02)
 ;;58016025332
 ;;9002226.02101,"900,58016025335 ",.01)
 ;;58016025335
 ;;9002226.02101,"900,58016025335 ",.02)
 ;;58016025335
 ;;9002226.02101,"900,58016025336 ",.01)
 ;;58016025336
 ;;9002226.02101,"900,58016025336 ",.02)
 ;;58016025336
 ;;9002226.02101,"900,58016025340 ",.01)
 ;;58016025340
 ;;9002226.02101,"900,58016025340 ",.02)
 ;;58016025340
 ;;9002226.02101,"900,58016025342 ",.01)
 ;;58016025342
 ;;9002226.02101,"900,58016025342 ",.02)
 ;;58016025342
 ;;9002226.02101,"900,58016025344 ",.01)
 ;;58016025344
 ;;9002226.02101,"900,58016025344 ",.02)
 ;;58016025344
 ;;9002226.02101,"900,58016025345 ",.01)
 ;;58016025345
 ;;9002226.02101,"900,58016025345 ",.02)
 ;;58016025345
 ;;9002226.02101,"900,58016025348 ",.01)
 ;;58016025348
 ;;9002226.02101,"900,58016025348 ",.02)
 ;;58016025348
 ;;9002226.02101,"900,58016025350 ",.01)
 ;;58016025350
 ;;9002226.02101,"900,58016025350 ",.02)
 ;;58016025350
 ;;9002226.02101,"900,58016025356 ",.01)
 ;;58016025356
 ;;9002226.02101,"900,58016025356 ",.02)
 ;;58016025356
 ;;9002226.02101,"900,58016025360 ",.01)
 ;;58016025360
 ;;9002226.02101,"900,58016025360 ",.02)
 ;;58016025360
