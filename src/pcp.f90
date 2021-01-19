SUBROUTINE pcp(filei, fileo) ! # nocov start

IMPLICIT none


CHARACTER*100 :: filei
CHARACTER*100 :: fileo
CHARACTER*5 :: variable
INTEGER, PARAMETER :: nfam=100000,ntime=200
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

! REAL :: ttime(nfam)
! REAL :: deltax(nfam)
! REAL :: deltay(nfam)
! CHARACTER*1 :: lastimage(nfam)
! CHARACTER*3 :: fim(nfam)

variable="PCP"
OPEN(5,file=filei,status='old')
OPEN(10,file=fileo,status='unknown')

DO k=1,nfam

 20 READ(5,200,end=250)family(k),year(k),month(k),&
                       day(k),hour(k),fmember(k),classif(k)
                           
    PRINT *, "Family = ",k

 200 FORMAT(7x,I10,9x,i4,8x,i2,6x,i2,6x,f5.2,14x,i4,9x,a3)
 
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
 
! tentativa de ler o rodape 
! 150 READ(5,900)ttime(k),deltax(k),deltay(k),&
!                lastimage(k),fim(k)
! 900 FORMAT(11x,f5.2,9x,f6.2,9x,f6.2,15x,1a,7x,3a)
!     PRINT *, ttime(k),deltax(k),deltay(k)

 
   nn=j-1
                
   DO j=1,nn

     WRITE(10,800)family(k),            & ! integer
     year(k),                           & ! integer
     month(k),                          & ! integer
     day(k),                            & ! integer
     hour(k),                           & ! real
     fmember(k),                        & ! integer
     classif(k),                        & ! character
     variable,                          & ! character
     missing(k,j),                      & ! character
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
           6(f7.1,1x),f5.2,1x,f7.1,1x,i4,f6.2,1x,1a,1x,20(i4,1x))

  800 FORMAT(I10,2x,i4,2x,i2,2x,i2,2x,f5.2,2x,i4,2x,a3,2x,a5,2x,&
            a1,i4,2x,2(f7.2,1x),f6.2,1x,(i6,1x),(f8.1,1x),&
            6(f7.1,1x),f5.2,1x,f7.1,1x,i4,f6.2,1x,1a,1x,20(i4,1x))

   ! Le as linhas em branco
   READ(5,*) 
   READ(5,*)

END DO	
	
250 CONTINUE


  CLOSE(5)
  CLOSE(10)	
	

RETURN
END   ! # nocov end
