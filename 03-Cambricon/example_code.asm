/*
** Cambricon program fragments of MLP, pooling and BM.
*/


/* MLP code */
// $0: input size, $1: output size, $2: matrix size
// $3: input address, $4: weight address
// $5: bias address, $6: output address
// $7-$10: temp variable address

VLOAD $3, $0, #100      // load input vector from address (100)
MLOAD $4, $2, #300      // load weight matrix from address (300)
MMV $7, $1, $4, $3, $0  // Wx
VAV $8, $1, $7, $5      // tmp=Wx+b
VEXP $9, $1, $8         // exp(tmp)
VAS $10, $1, $9, #1     // 1+exp(tmp)
VDV $6, $1, $9, $10     // y=exp(tmp)/(1+exp(tmp))
VSTORE $6, $1, #200     // store output vector to address (200)


/* Pooling code */
// $0: feature map size, $1: input data size,
// $2: output data size, $3: pooling window size - 1
// $4: x-axis loop num, $5: y-axis loop num
// $6: input addr, $7: output addr
// $8: y-axis stride of input

    VLOAD $6, $1, #100      // load input neurons from address (100)
    SMOVE $5, $3            // init y
L0: SMOVE $4, $3            // init x
L1: VGTM $7, $0, $6, $7
    // feature map m, output[m]=(input[x][y][m]>output[m])?
    //                           input[x][y][m]:output[m]
    SADD $6, $6, $0         // update input address
    SADD $4, $4, #-1        // x--
    CB #L1, $4              // if(x>0) goto L1
    SADD $6, $6, $8         // update input address
    SADD $5, $5, #-1        // y--
    CB #L0, $5              // if(y>0) goto L0
    VSTORE $7, $2, #200     // stroe output neurons to address (200)


/* BM code */
// $0: visible vector size, $1: hidden vector size, $2: v-h matrix (W) size
// $3: h-h matrix (L) size, $4: visible vector address, $5: W address
// $6: L address, $7: bias address, $8: hidden vector address
// $9-$17: temp variable address

VLOAD $4, $0, #100          // load visible vector from address (100)
VLOAD $9, $1, #200          // load hidden vector from address (200)
MLOAD $5, $2, #300          // load W matrix from address (300)
MLOAD $6, $3, #400          // load L matrix from address (400)
MMV $10, $1, $5, $4, $0     // Wv
MMV $11, $1, $6, $9, $1     // Lh
VAV $12, $1, $10, $11       // Wv+Lh
VAV $13, $1, $12, $7        // tmp=Wv+Lh+b
VEXP $14, $1, $13           // exp(tmp)
VAS $15, $1, $14, #1        // 1+exp(tmp)
VDV $16, $1, $14, $15       // y=exp(tmp)/(1+exp(tmp))
RV $17, $1                  // i, r[i] = random(0,1)
VGT $8, $1, $17, $16        // i, h[i] = (r[i]>y[i])?1:0
VSTORE $8, $1, #500         // store hidden vector to address (500)