//JDEFVSAM JOB ALU0519,MSGCLASS=A,CLASS=C,NOTIFY=&SYSUID
//*--------------------------------------------------------------------*
//*                        GENERATE VSAM CLUSTER                       *
//*--------------------------------------------------------------------*
//DEDEF1   EXEC PGM=IDCAMS,REGION=0M
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
    DEFINE CLUSTER                                              -
           ( NAME( ALU0519.DESAFIO.DDS.CADCONTA )               -
             RECORDSIZE(470 470)                                -
             VOLUME(PR39C7)                                     -
             KEY(4 0)                                           -
             INDEXED                                            -
             CYL(01 01)                                         -
             FREESPACE(10 10)                                   -
             SHAREOPTIONS(2 3)                                  -
             CONTROLINTERVALSIZE(4096)                          -
             UNIQUE                                             -
             IMBED                                              -
             REPLICATE )                                        -
           DATA ( NAME( ALU0519.DESAFIO.DDS.CADCONTA.DATA ) )   -
           INDEX ( NAME( ALU0519.DESAFIO.DDS.CADCONTA.INDEX ) )

    DEFINE CLUSTER                                              -
           ( NAME( ALU0519.DESAFIO.DDS.CADTRANS )               -
             RECORDSIZE(80 80)                                  -
             VOLUME(PR39C7)                                     -
             KEY(7 0)                                           -
             INDEXED                                            -
             CYL(01 01)                                         -
             FREESPACE(10 10)                                   -
             SHAREOPTIONS(2 3)                                  -
             CONTROLINTERVALSIZE(4096)                          -
             UNIQUE                                             -
             IMBED                                              -
             REPLICATE )                                        -
           DATA ( NAME( ALU0519.DESAFIO.DDS.CADTRANS.DATA ) )   -
           INDEX ( NAME( ALU0519.DESAFIO.DDS.CADTRANS.INDEX ) )
/*
//*
