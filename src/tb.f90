SUBROUTINE tb(ifile, ofile) ! # nocov start

IMPLICIT none


CHARACTER*100 :: ifile
CHARACTER*100 :: ofile
CHARACTER*5 :: variable
INTEGER, PARAMETER :: nfam=35000,ntime=200
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
INTEGER :: cbnum(nfam,ntime)        ! tb only
INTEGER :: cbmed(nfam,ntime)        ! tb only
INTEGER :: dir(nfam,ntime)
INTEGER :: sysant(nfam,ntime)
INTEGER :: falha(nfam,ntime)
REAL :: xlat(nfam,ntime)
REAL :: xlon(nfam,ntime)
REAL :: time(nfam,ntime)         
REAL :: dsize(nfam,ntime)
REAL :: tmed(nfam,ntime)
REAL :: dtmed(nfam,ntime)
REAL :: tmin(nfam,ntime)
REAL :: dtmin(nfam,ntime)
REAL :: tmin9(nfam,ntime)
REAL :: dtmin9(nfam,ntime)
REAL :: vel(nfam,ntime)
REAL :: incli(nfam,ntime)         ! tb only
REAL :: ecce(nfam,ntime)          ! tb only
REAL :: tini(nfam,ntime)          
REAL :: tfin(nfam,ntime)          ! tb only
CHARACTER*1 :: missing(nfam,ntime)
CHARACTER*1 :: cla(nfam,ntime)


variable="Tb"
OPEN(5,file=ifile,status='old')
OPEN(10,file=ofile,status='unknown')

 PRINT *, "Attention!!! &
          This routine must not be used in the case of more than 35000 families in a file"

WRITE(10,700)"FAMILY YEAR MONTH DAY HOUR FIRST_MEMBER CLASSIF VAR MISSING &
SYS YLAT XLON TIME SIZE DSIZE TMED DTMED TMIN DTMIN TMIN9 &
DTMIN9 CBNUM CBMED VEL DIR INCLI ECCE TINI TFIN CLA SYSANT"

700 FORMAT(a177)  

DO k=1,nfam

 20 READ(5,200,end=250)family(k),year(k),month(k),&
                       day(k),hour(k),fmember(k),&
                       classif(k)
                           

 200 FORMAT(7x,I5,9x,i4,8x,i2,6x,i2,6x,f5.2,14x,i4,9x,a3)
 
     READ(5,1113)
       
 1113 FORMAT(a160)


    DO j=1,ntime
     
     READ(5,749,err=150)missing(k,j),   & ! character
     sys(k,j),                          & ! integer
     xlat(k,j),                         & ! real
     xlon(k,j),                         & ! real
     time(k,j),                         & ! real
     size(k,j),                         & ! integer
     dsize(k,j),                        & ! real
     tmed(k,j),                         & ! real
     dtmed(k,j),                        & ! real
     tmin(k,j),                         & ! real
     dtmin(k,j),                        & ! real
     tmin9(k,j),                        & ! real
     dtmin9(k,j),                       & ! real
     cbnum(k,j),                        & ! integer
     cbmed(k,j),                        & ! integer
     vel(k,j),                          & ! real
     dir(k,j),                          & ! integer
     incli(k,j),                        & ! real
     ecce(k,j),                         & ! real
     tini(k,j),                         & ! real
     tfin(k,j),                         & ! real
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

     WRITE(10,800)family(k),            &
     year(k),                           &
     month(k),                          &
     day(k),                            &
     hour(k),                           &
     fmember(k),                        &
     classif(k),                        &
     variable,                          &
     falha(k,j),                        & ! character
     sys(k,j),                          & ! integer
     xlat(k,j),                         & ! real
     xlon(k,j),                         & ! real
     time(k,j),                         & ! real
     size(k,j),                         & ! integer
     dsize(k,j),                        & ! real
     tmed(k,j),                         & ! real
     dtmed(k,j),                        & ! real
     tmin(k,j),                         & ! real
     dtmin(k,j),                        & ! real
     tmin9(k,j),                        & ! real
     dtmin9(k,j),                       & ! real
     cbnum(k,j),                        & ! integer
     cbmed(k,j),                        & ! integer
     vel(k,j),                          & ! real
     dir(k,j),                          & ! integer
     incli(k,j),                        & ! real
     ecce(k,j),                         & ! real
     tini(k,j),                         & ! real
     tfin(k,j),                         & ! real
     cla(k,j),                          & ! character
     sysant(k,j)                          ! integer

   END DO
	

  749 FORMAT(a1,i4,2x,2(f7.2,1x),f6.2,1x,i6,1x,f8.1,1x,&  ! dsize
           6(f7.1,1x),i5,1x,i4,1x,f7.1,1x,i4,1x,f7.2,2x,& ! incli
           f4.2,1x,2(f5.1,1x),a2,1x,20(i4,1x))

  800 FORMAT(i5,1x,i4,1x,i2,1x,i2,1x,f5.2,1x,i4,1x,a3,1x,a5,1x,& ! header
            i1,i4,1x,2(f7.2,1x),f6.2,1x,i6,1x,f8.1,1x,&              ! dsize
            6(f7.1,1x),i5,1x,i4,1x,f7.1,1x,i4,1x,f7.2,1x,&           ! incli
            f4.2,1x,2(f5.1,1x),a2,1x,20(i4,1x))                      !  

   ! Le as linhas em branco
   READ(5,*) 
   READ(5,*)

END DO	
	
250 CONTINUE

PRINT *, ""
PRINT *, "Last family = ", k-1
PRINT *, ""
PRINT *, "FOR YOUR SECURITY: "
PRINT *, "PLEASE CERTIFY THAT YOUR INPUT FILE HAS LESS THAN 35000 FAMILIES."
PRINT *, "Type 'tail ifile' at terminal"
PRINT *, "if your input file has less than 35000 families, "
PRINT *, "is everything okay!"
PRINT *, ""

  CLOSE(5)
  CLOSE(10)	
	

RETURN
END   ! # nocov end
