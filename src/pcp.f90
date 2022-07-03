SUBROUTINE pcp(ifile, ofile, experiment) ! # nocov start


CHARACTER*100 :: ifile
CHARACTER*100 :: ofile
CHARACTER*10 :: experiment
CHARACTER*5 :: variable
INTEGER, PARAMETER :: nfam=100000,ntime=201
INTEGER :: j,k,nn

INTEGER :: family(nfam)
INTEGER :: year(nfam)
INTEGER :: month(nfam)
INTEGER :: day(nfam)
INTEGER :: fmember(nfam)
REAL :: hour(nfam)
CHARACTER*3 :: classif(nfam)

INTEGER :: sys(nfam,ntime)
INTEGER :: size(nfam,ntime)
INTEGER :: sysant(nfam,ntime)
INTEGER :: dir(nfam,ntime)
INTEGER :: falha(nfam,ntime)
REAL :: xlat(nfam,ntime)
REAL :: xlon(nfam,ntime)
REAL :: elapsed_time(nfam,ntime)
REAL :: time(nfam,ntime)
REAL :: dsize(nfam,ntime)
REAL :: pmed(nfam,ntime)
REAL :: dpmed(nfam,ntime)
REAL :: pmax(nfam,ntime)
REAL :: dpmax(nfam,ntime)
REAL :: pmax9(nfam,ntime)
REAL :: dpmax9(nfam,ntime)
REAL :: frac(nfam,ntime)
REAL :: vel(nfam,ntime)
REAL :: tini(nfam,ntime)
REAL :: elapsed_time_begin(nfam,ntime)
CHARACTER*1 :: missing(nfam,ntime)
CHARACTER*1 :: cla(nfam,ntime)

variable="PCP"
OPEN(5,file=ifile,status='old')
OPEN(10,file=ofile,status='unknown')

PRINT *, "Attention!!! &
          This routine must not be used in the case of more than 200 lines in the table body"

WRITE(10,700)"FAMILY YEAR MONTH DAY HOUR FIRST_MEMBER CLASSIF VAR MISSING SYS YLAT &
   XLON TIME SIZE DSIZE PMED DPMED PMAX DPMAX PMAX9 DPMAX9 FRAC VEL DIR &
   TINI CLA SYSANT"

700 FORMAT(a151)  

DO k=1,nfam

 IF (experiment.eq.'SAAG') THEN
 21 READ(5,201,end=250)family(k),year(k),month(k),&
                       day(k),hour(k),fmember(k),classif(k)
 ELSE                        
 20 READ(5,200,end=250)family(k),year(k),month(k),&
                       day(k),hour(k),fmember(k),classif(k)
 END IF
 
 201 FORMAT(7x,I15,9x,i4,8x,i2,6x,i2,6x,f5.2,14x,i4,9x,a3)
 200 FORMAT(7x,I5,9x,i4,8x,i2,6x,i2,6x,f5.2,14x,i4,9x,a3)
 
     READ(5,1113)
       
 1113 FORMAT(a132)


    DO j=1,ntime
     
     READ(5,749,err=150)missing(k,j), & ! character
     sys(k,j),                          & ! integer
     xlat(k,j),                         & ! real
     xlon(k,j),                         & ! real
     elapsed_time(k,j),                 & ! real
     size(k,j),                         & ! integer
     dsize(k,j),                        & ! real
     pmed(k,j),                         & ! real
     dpmed(k,j),                        & ! real
     pmax(k,j),                         & ! real
     dpmax(k,j),                        & ! real
     pmax9(k,j),                        & ! real
     dpmax9(k,j),                       & ! real
     frac(k,j),                         & ! real
     vel(k,j),                          & ! real
     dir(k,j),                          & ! integer
     elapsed_time_begin(k,j),           & ! real
     cla(k,j),                          & ! character
     sysant(k,j)                          ! integer

   END DO
     
 150  CONTINUE

   nn=j-1
                
   DO j=1,nn

     IF (missing(k,j).eq.'*') THEN
           falha(k,j)=1
     ELSE 
           falha(k,j)=0
     END IF

     WRITE(10,800)family(k),            & ! integer
     year(k),                           & ! integer
     month(k),                          & ! integer
     day(k),                            & ! integer
     hour(k),                           & ! real
     fmember(k),                        & ! integer
     classif(k),                        & ! character
     variable,                          & ! character
     falha(k,j),                        & ! character
     sys(k,j),                          & ! integer
     xlat(k,j),                         & ! real
     xlon(k,j),                         & ! real
     elapsed_time(k,j),                 & ! real
     size(k,j),                         & ! integer
     dsize(k,j),                        & ! real
     pmed(k,j),                         & ! real
     dpmed(k,j),                        & ! real
     pmax(k,j),                         & ! real
     dpmax(k,j),                        & ! real
     pmax9(k,j),                        & ! real
     dpmax9(k,j),                       & ! real
     frac(k,j),                         & ! real
     vel(k,j),                          & ! real
     dir(k,j),                          & ! integer
     elapsed_time_begin(k,j),           & ! real
     cla(k,j),                          & ! character
     sysant(k,j)                          ! integer

   END DO
	

  749 FORMAT(a1,i4,2x,2(f7.2,1x),f6.2,1x,(i6,1x),(f8.1,1x),&
             6(f7.1,1x),f5.2,1x,f7.1,1x,i4,1x,f6.2,1x,1a,1x,20(i4,1x))

  800 FORMAT(I15,1x,i4,1x,i2,1x,i2,1x,f5.2,1x,i4,1x,a3,1x,a5,1x,&    ! header + variable
             i1,1x,i4,1x,2(f7.2,1x),f6.2,1x,(i6,1x),(f8.1,1x),&     ! ateh DSIZE
             6(f7.1,1x),f5.2,1x,f7.1,1x,i4,1x,f6.2,1x,1a,1x,20(i4,1x))

   ! Le as linhas em branco
   READ(5,*) 
   READ(5,*)

END DO	
	
250 CONTINUE

PRINT *, "Last family = ", k-1

CLOSE(5)
CLOSE(10)	


RETURN
END   ! # nocov end
