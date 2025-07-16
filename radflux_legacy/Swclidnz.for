C     Last change: JA 9/28/2018 8:52:00 AM
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     PARAMETER STATEMENTS TO SET ARRAY SIZES:
C
C      PARAMETER (NMAX=100000)- NMAX is the total number of points that
C                              can be fit in the arrays for fitting.
C          Used in subroutines: swclreq1,medfit,rofunc,swclreq2,swclreq3
C
C     PARAMETER (NDAYMAX=18500) - NDAYMAX is the total number of days
C                             that can be handled in one run of the code.
C                             For 5 years max of data, set NDAYMAX=1850
C                             For 50 years max of data, set NDAYMAX=18500
C          Used in subroutines: clrcof1, clrcof2, swclrfcg
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PROGRAM SWCLRNZ
      INTEGER YR,MN,DY,HR,minit,DATE,i,n,t,IP5,IP6,IP7,IP8,dmin,s,k,l
      INTEGER stdate(60),sthr(60),stmin(60),j,maxtim,m,delnum,u,v,i2
      integer a1,a2,a3,a4,a5,a6,a7,a8,a9,b(30),c,tim,tmn,tdy,tmin,thr
      integer ttim,cent,cnt,zform,p,timflg,clrtim,numitr,itrinc,aczn
      INTEGER lsdate,rflg,longyr,dflg,tflg,a10,ckdate,sitflg,e,hrofst
      INTEGER month(12),zyr,zmn,zdy,zhr,zdate,ztim,y(0:100),doy,dn,adn
      integer lyr,lmn,ldy,lhr,ldate,ltim,cosdat,nummin,stamp,xi,lastim
      INTEGER man,aan,timadj,ydy
      REAL*8 TSW,DSW,COSZ,rerr,sncz,sncz1,avndfr,avnsw,x(0:100),swadd
      REAL*8 DIR,nsw(60),difrat(60),dr(60),dif(60),cz(60),maxdif,swrlim
      REAL*8 sw(60),pi,delS,rsum,avg,aau(60),tswlim,lsdfra,lsdfrb,lscswa
      REAL*8 ndifr(60),TOA(60),delT,lim,zd,ndstlm,delSlm,lscswb,swu
      REAL*8 nswmx,nswmn,gx(5),delfac,ncdxtim,czlim,timadd,rdmin,mfac
      REAL*8 time,Z,AZ,DEC,Au,hrang,slat,slon,xlong,nswb,longit,gt,x1
      REAL*8 dtminus,dtplus,dalb,alb(1500),dcz(1500),alba,albb,devalb
      REAL*8 aczmn,dalim,malb,aalb
      CHARACTER*12 DIRFIL,outfil,dayfil,cosfil,badfil
      CHARACTER*12 errfil,datfil,cosdir
      CHARACTER*12 cnffil,dir2
      CHARACTER*300 errlin
      CHARACTER*160 path,infil,usefil,line
      CHARACTER*5 h(30)
      CHARACTER*8 stem
      CHARACTER*1 ans
      logical ok,go,datlog,nrmdir,yrlog,snczgo,dyfil,cldflg,chkflg
      logical debug,cosok,start,chkday,ncdx,notgd,noclr,ngt,jdat,jdat2

      DATA MONTH /31,28,31,30,31,30,31,31,30,31,30,31/

      ok=.true.
      start=.true.
      datlog=.false.
      jdat=.false.
      jdat2=.false.
      nrmdir=.false.
      yrlog=.false.
      cldflg=.false.
      chkflg=.false.
      debug=.false.
      chkday=.false.
      ncdx=.false.
      ngt=.false.
      dmin=0
      cnt=0
      xi=0
      x(0)=-9999.0
      swadd=0.0

      timadj=-99999

      DIRFIL='inputfls.dir'
      dayfil='swclrid1.day'
      cnffil='swclidnz.cnf'
      dir2='swclidnz.dir'
      badfil='bad_data.cos'
      datfil='ldate.asc'
      cosdir='cosfils.dir'

      do 1 i=1,30
        h(i)='     '
        b(i)=0
 1    continue

      OPEN(UNIT=2,FILE='cnf_read.asc',STATUS='unknown')
      OPEN(UNIT=1,FILE=CNFFIL,STATUS='OLD',err=90)
	read(1,*)
	read(1,*)
	read(1,*) sitflg
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccc  sitflg: site ("data input includes") flag
ccccccccc
ccccccccc  0 = no additional fitting, input data includes only total,
ccccccccc      direct, and diffuse SW data
ccccccccc
ccccccccc  1 = input data includes upwelling SW, combine with SW dn to
ccccccccc      produce sfc albedo and clear sky fit
ccccccccc
ccccccccc  2 = input data includes another solar-driven value, such as
ccccccccc      UVB, produce clear sky fit for this value
ccccccccc
ccccccccc  3 = input data includes some non-solar-driven value, such as
ccccccccc      LW. Include in output, but do not produce clear sky fit
ccccccccc
ccccccccc  If set between 900 and 903, will activate debug flag for
ccccccccc    producing debug output, and halting the code between
ccccccccc    subroutines.
ccccccccc
ccccccccc  If set between 1000 and 1003, will activate the checkday
ccccccccc    flag (checks for input files that include more than one day of data),
ccccccccc    but not activate the debug flag
ccccccccc
ccccccccc  If set between 950 and 953, will activate BOTH the checkday
ccccccccc    AND the debug flags
ccccccccc  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(sitflg.ge.900.and.sitflg.lt.960) debug=.true.
        IF(sitflg.ge.950) chkday=.true.
        WRITE(2,*)'read(1,*) sitflg '
        WRITE(2,*)'sitflg= ',sitflg
        IF(sitflg.eq.901.or.sitflg.eq.1001.or.sitflg.eq.951) sitflg=1
        IF(sitflg.eq.902.or.sitflg.eq.1002.or.sitflg.eq.952) sitflg=2
        IF(sitflg.eq.903.or.sitflg.eq.1003.or.sitflg.eq.953) sitflg=3
        IF(sitflg.eq.900.or.sitflg.eq.1000.or.sitflg.eq.950) sitflg=0
        WRITE(2,*)' '
	read(1,*)
	read(1,*)
	read(1,*)
        read(1,*) clrtim,maxtim,delnum,n
        WRITE(2,*)'read(1,*) clrtim,maxtim,delnum,n '
        WRITE(2,*)'clrtim= ',clrtim
        WRITE(2,*)'maxtim= ',maxtim
        WRITE(2,*)'delnum= ',delnum
        WRITE(2,*)'daily coeff flg (1=yes,0=no)= ',n
        if(n.eq.0) then
          dyfil=.false.
        else
          dyfil=.true.
        endif
        read(1,*)
	read(1,*,ERR=94) slat,XLONG,ip5,timadj
        WRITE(2,*)' '
	write(2,*)'read(1,*) slat,XLONG,ip5'
        WRITE(2,*)'lat = ',slat
        WRITE(2,*)'long = ',xlong
        WRITE(2,*)'Time Flag (0/1 for LST/GMT) = ', ip5
        WRITE(2,*)'Time adjustment (in minutes) = ', timadj
        read(1,*)
        read(1,*)
        read(1,*) nswb,ndstlm,delSlm
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) nswb,ndstlm,delSlm'
        WRITE(2,*)'nswb= ',nswb
        WRITE(2,*)'ndstlm= ',ndstlm
        WRITE(2,*)'delSlm= ',delslm
        read(1,*)
        read(1,*)
        read(1,*) zform
        WRITE(2,*)' '
        WRITE(2,*)' read(1,*) zform '
        WRITE(2,*)'(0=calculate; 1=CosZ, 2=Zdegrees, 3=Zradians) ',zform
        read(1,*)
        read(1,*)
        read(1,*) s,k,l
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) s,k,l'
        WRITE(2,*)'hdr lines to skip, int in line, reals in line:',s,k,l
        read(1,*)
        read(1,*)
        read(1,*) n
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) n'
        WRITE(2,*)'date YYYYMMDD, separate integers, NCDX real, YYYYJJJ?
     %(0/1/2/3/4): ',n
        if(n.eq.0) datlog=.true.
        if(n.eq.2) ncdx=.true.
        if(n.eq.3) jdat=.true.
        if(n.eq.4) jdat2=.true.
        if(datlog.or.ncdx.or.jdat) then
          read(1,*)
          read(1,*) a1
          WRITE(2,*)' '
          WRITE(2,*)'read(1,*) a1'
          WRITE(2,*)'a1= ',a1
        elseif(jdat2) then
          read(1,*)
          read(1,*) a1,a2
          WRITE(2,*)' '
          WRITE(2,*)'read(1,*) a1,a2'
          WRITE(2,*)'a1= ',a1
          WRITE(2,*)'a2= ',a2
        else
          read(1,*)
          read(1,*) a1,a2,a3
          WRITE(2,*)' '
          WRITE(2,*)'read(1,*) a1,a2,a3'
          WRITE(2,*)'a1= ',a1
          WRITE(2,*)'a2= ',a2
          WRITE(2,*)'a3= ',a3
        endif
        read(1,*)
        read(1,*) n
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) n '
        WRITE(2,*)'4 or 2 digit year? (0=4 digit/ 1=2 digit) ',n
        if(n.eq.1) yrlog=.true.
        read(1,*)
        read(1,*) cent
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) cent '
        WRITE(2,*)'cent= ',cent
        read(1,*)
        read(1,*) timflg,nummin,stamp
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) timflg,nummin,stamp '
        WRITE(2,*)'time (hhmm), separate int, daymin, ncdx, (hhmmss)? (0
     %/1/2/3/4) ',timflg
        WRITE(2,*)'# minutes in average: ',nummin
        WRITE(2,*)'time stamp start/mid/end (1,0,-1): ',stamp
        if(timflg.eq.0.or.timflg.eq.2.or.timflg.eq.3.or.timflg.eq.4)then
          read(1,*)
          read(1,*) a4
          WRITE(2,*)' '
          WRITE(2,*)'read(1,*) a4 '
          WRITE(2,*)'a4= ',a4
        else
          read(1,*)
          read(1,*) a4,a5
          WRITE(2,*)' '
          WRITE(2,*)'read(1,*) a4,a5 '
          WRITE(2,*)'a4= ',a4
          WRITE(2,*)'a5= ',a5
        endif
        n=nummin/2
        timadd=n*1.0*stamp
          IF(timadd.lt.0.01.and.timadd.GT.-0.0000001) timadd=0.01
          WRITE(2,*)'n=nummin/2 ',n
          WRITE(2,*)'timadd=n*1.0*stamp ',timadd
        IF(nummin-(2*n).gt.0) then
          IF(stamp.lt.0) timadd=timadd-0.70
          IF(stamp.gt.0) timadd=timadd+0.30
        endif
          WRITE(2,*)'IF(nummin-(2*n).gt.0) ',timadd
          IF(nummin.gt.2) then
            delfac=1.0+(nummin/2.0)
          else
            delfac=nummin*1.0
          endif
          WRITE(2,*)'delfac= ',delfac
        read(1,*)
        read(1,*) n
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) n '
        WRITE(2,*)'direct normal (NIP) measurements? (0=yes/1=no) ',n
        if(n.eq.0) nrmdir=.true.
        read(1,*)
        if(zform.gt.0) then
          read(1,*)
          read(1,*) a9
          WRITE(2,*)' '
          WRITE(2,*)'zform>0, read(1,*) a9 '
          WRITE(2,*)'a9= ',a9
        else
          read(1,*)
          read(1,*)
          WRITE(2,*)' '
          WRITE(2,*)'zform<0, read(1,*) [skip reading a9]'
        endif
        read(1,*)
        IF(sitflg.gt.0) then
          read(1,*) a6,a7,a8,a10
        else
          read(1,*) a6,a7,a8
cc========  added to fix MatLab/UNIX crash per Chris Cox 8/26/2015  ======
cc========   Initialize SolX/SWup/etc a10 integer to zero  ===============
          a10=0
cc========================================================================
        endif
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) a6,a7,a8,a10'
        WRITE(2,*)'a6= ',a6
        WRITE(2,*)'a7= ',a7
        WRITE(2,*)'a8= ',a8
        IF(sitflg.gt.0) WRITE(2,*)'a10= ',a10
        read(1,*)
        read(1,*) tswlim
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) tswlim '
        WRITE(2,*)'tswlim= ',tswlim
        read(1,*)
        read(1,*) maxdif,czlim,mfac
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) maxdif, czlim, mfac '
        WRITE(2,*)'maxdif= ',maxdif
        WRITE(2,*)'czlim= ',czlim
        WRITE(2,*)'mfac= ',mfac
        read(1,*)
        read(1,*) swrlim
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) swrlim '
        WRITE(2,*)'swrlim= ',swrlim
        read(1,*)
        read(1,2) path
 2      format(a160)
        p=index(path,' ')
        path=path(1:p-1)
        WRITE(2,*)' '
        WRITE(2,*)'read(1,2) path '
        WRITE(2,*)'path= ',path(1:p-1),'<<<<<'
        read(1,*)
        read(1,*) numitr
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) numitr '
        WRITE(2,*)'numitr= ',numitr
        read(1,*)
        read(1,*) n,i
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) n,i '
        WRITE(2,*)'print  *.tst output files (0=n0, 1=yes): ',n
        WRITE(2,*)'output night data (0=n0, 1=yes): ',i
        IF(n.eq.1) cldflg=.true.
        IF(i.eq.1) ngt=.true.
        read(1,*)
        read(1,*) n
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) n '
        WRITE(2,*)'measurement phys limits testing (0=no, 1=test): ',n
        IF(n.eq.1) chkflg=.true.
        read(1,*)
        read(1,*) nswmx,nswmn
        IF(nswmx.gt.150.0) swadd=nswmx-150.0
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) nswmx,nswmn '
        WRITE(2,*)'NSWmax and NSWmin +/- limits: ',nswmx,nswmn
        IF(nswmx.lt.100.0.or.nswmn.lt.100.0) then
          WRITE(6,*)'%%%%%%%%%%%%%%%    WARNING!!!   %%%%%%%%%%%%%%%%%%'
          WRITE(6,*)'  NSWmax or NSWmin additive limits for clear '
          WRITE(6,*)'  detection in subroutine "SWCLRID2" are set '
          WRITE(6,*)'  less than 100.0 W/m^2.  '
          WRITE(6,*)'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
          WRITE(6,*)' '
          WRITE(6,*)' NSWmax & NSWmin =',nswmx,nswmn
          WRITE(6,*)' '
          WRITE(6,*)'   Do you wish to continue this run (y/n)'
          READ(*,106) ans
          IF(ans.eq.'n'.or.ans.eq.'N') GO TO 200
        endif
        read(1,*)
        read(1,*)
        read(1,*)
        read(1,*) c
        WRITE(2,*)' '
        WRITE(2,*)'read(1,*) c'
        WRITE(2,*)'# xtra variables printed out in *.cos: ',c
        IF(c.gt.30) then
          WRITE(6,*)' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          WRITE(6,*)'      Max number of Xtra Variables is 30'
          WRITE(6,*)' Max number has been limited to 30 for this run'
          WRITE(6,*)' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          c=30
          pause
        endif
        if(c.gt.0) then
          read(1,*)
          read(1,*)
          read(1,*)
          do 4 i=1,c
            read(1,2) line
 3          format(a5)
            n=INDEX(line,' ')
            READ(line(1:n),*)b(i)
            do v=n,154
              IF(line(v:v).ne.' ') GO TO 6
            end do
 6          read(line(v:v+5),3) h(i)
            WRITE(2,*)' '
            WRITE(2,*)'read(1,2) line'
            WRITE(2,*)'b(i)= ',b(i),'    h(i)= >',h(i),'<'
 4        continue
        endif
      close(unit=1)
      CLOSE(UNIT=2)

      OPEN(UNIT=77,FILE=BADFIL,STATUS='unknown')
      IF(sitflg.eq.2) then
      write(77,16)' zdate  ztim   ldate  ltim      CosZ     AU         S
     %Wdn       Dif       Dir     SolX    tflg dflg rflg'
      elseIF(sitflg.eq.3) then
      write(77,16)' zdate  ztim   ldate  ltim      CosZ     AU         S
     %Wdn       Dif       Dir      Aux    tflg dflg rflg'
      else
      write(77,16)' zdate  ztim   ldate  ltim      CosZ     AU         S
     %Wdn       Dif       Dir      SWu    tflg dflg rflg'
      endif
      OPEN(UNIT=1,FILE=DIRFIL,STATUS='OLD')
      OPEN(UNIT=89,FILE=dir2,STATUS='UNKNOWN')
      OPEN(UNIT=90,FILE=cosdir,STATUS='UNKNOWN')
 5    format(a114)
      OPEN(UNIT=8,FILE=dayfil,STATUS='UNKNOWN')
      write(8,*)' Date        AvNDR     AvNSW      sncz     N  TotN    D
     %ifAlb    dN  DayN   MornAlb    AftAlb  MrnN  AftN'
      rerr=-999.0d0
      pi=dacos(-1.d0)
      m=maxtim/2+1
      n=0
      i=0
      t=0
      IP7 = 0
      IF(debug) OPEN(UNIT=161,FILE='dbg_out.asc',STATUS='unknown')
      IF(debug) then
      WRITE(161,5)'   Date  Hr Mn    CosZ     AAU      TotSW   NSW
     %Dif    Dir    NDfr    DifRat   TOA      delT    delS    StDDR'
      endif

      IF(XLONG .GE. 0.) THEN
	 IF(XLONG .GT. 180.) THEN
	    SLON=  360.-XLONG
	 ELSE
	    SLON= -XLONG
	 ENDIF
      ELSE IF(XLONG .LT. -180.) THEN
	    SLON= -(360.+XLONG)
      ELSE
	    SLON= -XLONG
      ENDIF
      longit=-slon
      hrofst=INT((ABS(longit)+7.500)/15.000)
      IF(longit.lt.0.0) hrofst=-hrofst
        avndfr=0.d0
	avnsw=0.d0
        dalb=0.0
        malb=0.0
        aalb=0.0
        dn=0
        man=0
        aan=0
        adn=0

 10   READ(1,15,END=100) INFIL
 15   FORMAT(A160)
        e=index(infil,' ')
        infil=infil(1:e-1)
        WRITE(6,*)'===================================================='
        WRITE(6,*)'  PROCESSING FILE: ',inFIL(1:e-1)
cc        snczgo=.true.
        usefil=path(1:(p-1))//infil(1:e-1)
      OPEN(UNIT=2,FILE=useFIL,STATUS='OLD')
      do 18 i=1,s
	read(2,*,end=50)
 18   continue

 22   read(2,*,end=50,err=30) (y(u),u=1,k),(x(v),v=1,l)
      do xi=1,c
        IF(x(b(xi)).LT.-9999.0.or.x(b(xi)).GT.99999.0) x(b(xi))=-9999.0
      end do
      if(datlog) then
        date=y(a1)
        if(yrlog) then
          yr=(date/10000)
          longyr=(cent*100)+yr
          mn=(date-yr*10000)/100
          dy=(date-yr*10000-mn*100)
        else
          longyr=(date/10000)
          yr=longyr-(cent*100)
          mn=(date-longyr*10000)/100
          dy=(date-longyr*10000-mn*100)
        endif
        date=longyr*10000+mn*100+dy
      ELSEIF(ncdx) then
        date=INT(x(a1))
        if(yrlog) then
          yr=(date/10000)
          longyr=(cent*100)+yr
          mn=(date-yr*10000)/100
          dy=(date-yr*10000-mn*100)
        else
          longyr=(date/10000)
          yr=longyr-((longyr/100)*100)
          mn=(date-longyr*10000)/100
          dy=(date-longyr*10000-mn*100)
        endif
        date=longyr*10000+mn*100+dy
      ELSEIF(jdat) then
        date=y(a1)
        yr=date/1000
        doy=date-(yr*1000)
        call DOY2DATE(yr,doy,date)
        longyr=(date/10000)
        yr=longyr-(cent*100)
        mn=date/100 - longyr*100
        dy=date-longyr*10000-mn*100
      ELSEIF(jdat2) then
        yr=y(a1)
        doy=y(a2)
        call DOY2DATE(yr,doy,date)
        longyr=(date/10000)
        yr=longyr-(cent*100)
        mn=date/100 - longyr*100
        dy=date-longyr*10000-mn*100
      else
        yr=y(a1)
        mn=y(a2)
        dy=y(a3)
        if(yrlog) then
          longyr=(cent*100)+yr
          date=longyr*10000+mn*100+dy
        else
          longyr=yr
          yr=longyr-(cent*100)
          date=longyr*10000+mn*100+dy
        endif
      endif
      if(timflg.eq.0) then
        tim=y(a4)
        hr=tim/100
        minit=tim-(hr*100)
        dmin=hr*60+minit
      elseif(timflg.eq.2) then
        dmin=y(a4)
        hr=(dmin)/60
        minit=dmin-(hr*60)
        tim=(hr*100)+minit
      elseif(ncdx) then
        ncdxtim=x(a1)-date
        tim=NINT(ncdxtim*10000.0)
        hr=tim/100
        minit=tim-(hr*100)
        dmin=hr*60+minit
        hr=(dmin)/60
        minit=dmin-(hr*60)
        tim=(hr*100)+minit
        dmin=hr*60+minit
      elseIF(timflg.eq.4) then
        tim=y(a4)
        hr=tim/10000
        minit=(tim/100)-(hr*100)
        tim=hr*100+minit
        dmin=hr*60+minit
      else
        hr=y(a4)
        minit=y(a5)
        tim=(hr*100)+minit
        dmin=hr*60+minit
      endif

c     Check for leap year
      IF(MOD(longyr,4).ne.0) then
        month(2)=28
      ELSEIF(MOD(longyr,400).eq.0) then
        month(2)=29
      ELSEIF(MOD(longyr,100).eq.0) then
        month(2)=28
      ELSE
        month(2)=29
      endif

ccccccccccccccc  time = 2360 bug fix cccccccccccccc
      IF(tim.gt.2359.or.dmin.ge.1440) then
        dmin=dmin-1440
        hr=(dmin)/60
        minit=dmin-(hr*60)
        tim=(hr*100)+minit
        dy=dy+1
        IF(dy.gt.month(mn)) then
          dy=1
          mn=mn+1
          IF(mn.gt.12) then
            mn=1
            longyr=longyr+1
            yr=longyr
          endif
        endif
        date=longyr*10000+mn*100+dy
      endif
ccccccccccccccc END: time = 2360 bug fix cccccccccccccc

cccccccccccccccc apply time adjustment if needed  ccccccccccc
      IF(timadj.ne.0) then

cc      WRITE(6,*)' adjusting time:',date,tim,timadj

        call DATE2DOY(date,longyr,doy)
cc      WRITE(6,*)' date2doy sub:',date,longyr,doy

c         Check for leap year [for "yr" = 4-digit year]
          IF(MOD(longyr,4).ne.0) then
            ydy=365
          ELSEIF(MOD(longyr,400).eq.0) then
            ydy=366
          ELSEIF(MOD(longyr,100).eq.0) then
            ydy=365
          ELSE
            ydy=366
          endif


        dmin=dmin+timadj
        IF(dmin.lt.0) then
          dmin=1440+dmin
          doy=doy-1
          IF(doy.eq.0) then
            longyr=longyr-1
            doy=ydy
          endif
        ELSEIF(dmin.gt.2359) then
          dmin=dmin-1440
          doy=doy+1
          IF(doy.gt.ydy) then
            doy=1
            longyr=longyr+1
          endif
        endif

        hr=(dmin)/60
        minit=dmin-(hr*60)
        tim=(hr*100)+minit

        call DOY2DATE(longyr,doy,date)
         yr=date/10000
         mn=date/100 - yr*100
         dy=date-yr*10000-mn*100

cc      WRITE(6,*)'Time adjusted:',date,tim,longyr

      endif
cccccccccccccccc end apply time adjustment  ccccccccccc


      ckdate=date

      IF(ip5.gt.0) then
        zhr=hr
        zyr=longyr
        zmn=mn
        zdy=dy
        lyr=longyr
        lmn=mn
        ldy=dy
        lhr=hr+hrofst
        IF(lhr.lt.0) then
          lhr=lhr+24
          ldy=ldy-1
        ELSEIF(lhr.gt.23) then
          lhr=lhr-24
          ldy=ldy+1
        endif
        IF(ldy.eq.0) then
          lmn=lmn-1
          IF(lmn.gt.0) then
            ldy=month(lmn)
          else
            lmn=12
            ldy=month(lmn)
            longyr=longyr-1
            lyr=longyr
          endif
        elseIF(ldy.gt.month(lmn)) then
          ldy=1
          lmn=lmn+1
          IF(lmn.gt.12) then
            lmn=1
            longyr=longyr+1
            lyr=longyr
          endif
        endif
        zdate=zyr*10000+zmn*100+zdy
        ztim=zhr*100+minit
        ldate=lyr*10000+lmn*100+ldy
        ltim=lhr*100+minit
      else
        lhr=hr
        lyr=longyr
        lmn=mn
        ldy=dy
        zyr=longyr
        zmn=mn
        zdy=dy
        zhr=hr-hrofst
        IF(zhr.lt.0) then
          zhr=zhr+24
          zdy=zdy-1
        ELSEIF(zhr.gt.23) then
          zhr=zhr-24
          zdy=zdy+1
        endif
        IF(zdy.eq.0) then
          zmn=zmn-1
          IF(zmn.gt.0) then
            zdy=month(zmn)
          else
            zmn=12
            zdy=month(zmn)
            longyr=longyr-1
            zyr=longyr
          endif
        elseIF(zdy.gt.month(zmn)) then
          zdy=1
          zmn=zmn+1
          IF(zmn.gt.12) then
            zmn=1
            longyr=longyr+1
            zyr=longyr
          endif
        endif
        zdate=zyr*10000+zmn*100+zdy
        ztim=zhr*100+minit
        ldate=lyr*10000+lmn*100+ldy
        ltim=lhr*100+minit
      endif

      IF(start) then
        cosdat=ldate
        cosok=.true.
        lastim=ltim
      endif

      IF(ldate.eq.cosdat.and.ltim.lt.lastim) GO TO 22

      IF(ldate.ne.cosdat) cosok=.true.
      IF(cosok) then
	 if(n.gt.0) then
	   avndfr=avndfr/(n*1.d0)
	   avnsw=avnsw/(n*1.d0)
	 else
	   avndfr=1.d0
	   avnsw=-1.d0
	 endif
         IF(dn.gt.50/nummin.or.dn.ge.10) then
           dalb=dalb/(dn*1.0)
           IF(dalb.gt.1.0) dalb=-9.0
         else
           dalb=-9.0
         endif
         IF(man.gt.50/nummin.or.man.ge.10) then
           malb=malb/(man*1.0)
           IF(malb.gt.1.0) malb=1.0
         else
           malb=-9.0
         endif
         IF(aan.gt.50/nummin.or.aan.ge.10) then
           aalb=aalb/(aan*1.0)
           IF(aalb.gt.1.0) aalb=1.0
         else
           aalb=-9.0
         endif
         IF(.not.start) then
           write(8,51)cosdat,avndfr,avnsw,sncz,n,t,dalb,dn,adn,malb,
     %aalb,man,aan
           IF(debug) write(6,51)cosdat,avndfr,avnsw,sncz,n,t
           if(n.ge.delnum) write(89,54) outfil
         endif
        n=0
        t=0
        adn=0
        dalb=0.0
        malb=0.0
        aalb=0.0
        dn=0
        man=0
        aan=0
        avndfr=0.d0
	avnsw=0.d0
        start=.false.
        cosdat=ldate
        CLOSE(UNIT=99)
        CLOSE(UNIT=3)
        OPEN(UNIT=101,FILE=datfil,STATUS='unknown')
        WRITE(101,23) lyr*10000+lmn*100+ldy
 23     FORMAT(i8)
        CLOSE(UNIT=101)
        OPEN(UNIT=101,FILE=datfil,STATUS='old')
        READ(101,24) stem
 24     FORMAT(a8)
        CLOSE(UNIT=101)
        cosfil=stem//'.cos'
	open(unit=99,file=cosfil,status='unknown')
        WRITE(90,54) cosfil
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(sitflg.eq.2) then
      write(99,16)'   zdate  ztim     ldate  ltim      CosZ      SNCZ
     %  AU        SWdn       Dif        Dir     SolX    tflg dflg rflg',
     %(h(v),v=1,30)
 16     FORMAT(A117,40(6x,a5))
        elseIF(sitflg.eq.3) then
      write(99,16)'   zdate  ztim     ldate  ltim      CosZ      SNCZ
     %  AU        SWdn       Dif        Dir      aux    tflg dflg rflg',
     %(h(v),v=1,30)
        else
      write(99,16)'   zdate  ztim     ldate  ltim      CosZ      SNCZ
     %  AU        SWdn       Dif        Dir     SWup    tflg dflg rflg',
     %(h(v),v=1,30)
        endif
        OUTFIL=stem//'.clr'
	open(unit=3,file=outfil,status='unknown')
      WRITE(3,5)' Date  Hr Mn    CosZ     AAU      TotSW   NSW      Dif
     %   Dir    NDfr    DifRat   TOA      delT    delS    StDDR'
        cosok=.false.
        ok=.true.
ccccccccccccccccccccccccccccccccc
        snczgo=.true.
ccccccccccccccccccccccccccccccccc
        lastim=ltim
      endif

      time=tim*1.d0
      if(snczgo) then
cc         IP6 = 1
cc         IP8 = 0
         CALL EPHEMS(SLAT,SLON,dy,mn,longyr,TIME,IP5,1,IP7,0,
     +            AZ,AU,Z,HRANG,DEC)

        sncz=dCOS(Z*PI/180.d0)
        sncz1=sncz+0.1d0
        if(sncz1.gt.1.d0) sncz1=1.d0
        write(6,*)'  Solar Noon CosZ: ',sncz,'   time: ',time
        snczgo=.false.
      endif

      rdmin=dmin*1.0+timadd
      time=INT(rdmin/60)*100+(rdmin-INT(rdmin/60)*60)
      IP6 = 0
      IP8 = 0
      if(zform.eq.0) then
        IF(time.gt.59.590.and.time.lt.101.00d0.and.ip6.eq.0) then
          gt=101.01d0
          CALL EPHEMS(SLAT,SLON,dy,mn,longyr,GT,IP5,IP6,IP7,IP8,
     +            AZ,AU,Z,HRANG,DEC)
          Gx(1)=az
          Gx(2)=au
          Gx(3)=Z
          Gx(4)=dec
          gt=59.590
          CALL EPHEMS(SLAT,SLON,dy,mn,longyr,GT,IP5,IP6,IP7,IP8,
     +            AZ,AU,Z,HRANG,DEC)
          gt=(time-100.d0)+.01
          gt=gt/0.61d0
          az=((Gx(1)-az)*gt)+az
          au=((Gx(2)-au)*gt)+au
          Z=((Gx(3)-Z)*gt)+Z
          dec=((Gx(4)-dec)*gt)+dec
        else
          CALL EPHEMS(SLAT,SLON,dy,mn,longyr,TIME,IP5,IP6,IP7,IP8,
     +            AZ,AU,Z,HRANG,DEC)
        endif
        cosz=dCOS(Z*PI/180.d0)
      else
        if(zform.eq.1) cosz=(Zd)
        if(zform.eq.2) cosz=dCOS(Zd*pi/180.d0)
        if(zform.eq.3) cosz=dCOS(Zd)
      endif
      if(cosz.lt.0.d0.and..not.ngt) go to 22

      IF(x(a6).GT.-20.0) then
        tsw=x(a6)
        tflg=0
      else
        tflg=1
        tsw=x(a6)
        IF(cosZ.gt.0.0) write(77,26) zdate,ztim,ldate,ltim,cosz,au,tsw,
     %dsw,dir,swu,tflg,dflg,rflg
        tsw=-999.0
      endif
      IF(x(a7).GT.-20.0) then
        dsw=x(a7)
        dflg=0
      else
        dflg=1
        dsw=x(a7)
        IF(cosZ.gt.0.0) write(77,26) zdate,ztim,ldate,ltim,cosz,au,tsw,
     %dsw,dir,swu,tflg,dflg,rflg
        dsw=-999.0
      endif
      IF(x(a10).GT.-20.0.and.sitflg.gt.0) then
        swu=x(a10)
      else
        swu=-999.0
      endif
      if(zform.gt.0) zd=x(a9)

      IF(OK) THEN
        TMN=lmn
        TDY=ldy
        THR=lhr
        TMIN=minit
        ttim=thr*100+tmin
	OK=.FALSE.
      ENDIF

      rflg=0
      if(nrmdir.and.x(a8).GT.-20.0) then
        IF(cosz.ge.0.0) then
          dir=x(a8)*cosz
        else
          dir=x(a8)
        endif
      elseIF(x(a8).GT.-20.0) then
        dir=x(a8)
      ELSEIF(tsw.GT.-1.0.and.dsw.GT.-1.0) then
        rflg=1
        dir=x(a8)
        IF(cosZ.gt.0.0) write(77,26) zdate,ztim,ldate,ltim,cosz,au,tsw,
     %dsw,dir,swu,tflg,dflg,rflg
        dir=tsw-dsw
      else
        rflg=-9
        dir=x(a8)
        IF(cosZ.gt.0.0) write(77,26) zdate,ztim,ldate,ltim,cosz,au,tsw,
     %dsw,dir,swu,tflg,dflg,rflg
        dir=rerr
      endif
      IF(dir.LT.-20.0.or.dir.gt.9999.0) dir=-999.0
      IF(dsw.LT.-998.0.and.tsw.GT.-20.0.and.dir.gt.-20.0) then
        dsw=tsw-dir
        dflg=9
      endif
      IF(tsw.LT.-998.0.and.dsw.GT.-20.0.and.dir.gt.-20.0) then
        tsw=dsw+dir
        tflg=9
      endif
      IF(chkflg) then
        if(dabs(tsw-(dsw+dir)).gt.100.d0) then
          dflg=2
        IF(cosZ.gt.0.0) write(77,26) zdate,ztim,ldate,ltim,cosz,au,tsw,
     %dsw,dir,swu,tflg,dflg,rflg
          dsw=rerr
        endif
        IF(tsw.gt.1500.d0) then
          tflg=2
        IF(cosZ.gt.0.0) write(77,26) zdate,ztim,ldate,ltim,cosz,au,tsw,
     %dsw,dir,swu,tflg,dflg,rflg
          tsw=rerr
        endif
        IF(dsw.LT.-10.0.or.tsw.LT.-10.0) then
          if(c.eq.0) then
            write(99,25) zdate,ztim,ldate,ltim,cosz,sncz,au,tsw,dsw,dir
     %,swu,tflg,dflg,rflg
          elseif(c.gt.0) then
            write(99,25) zdate,ztim,ldate,ltim,cosz,sncz,au,tsw,dsw,dir
     %,swu,tflg,dflg,rflg,(x(b(v)),v=1,c)
          endif
cc      calc avg diffuse abledo if sitflg=1
          IF(sitflg.eq.1.and.tsw.gt.20.0.and.swu.gt.20.0) then
            adn=adn+1
            dalim=sncz*100.0
            IF(dalim.lt.50.0) dalim=50.0
            IF(tsw.gt.dalim.AND.ltim.gt.1100.and.ltim.lt.1300) then
              dalb=dalb+(swu/tsw)
              dn=dn+1
            ELSEIF(tsw.gt.dalim.AND.ltim.lt.1100)then
              malb=malb+(swu/tsw)
              man=man+1
            ELSEIF(tsw.gt.dalim.AND.ltim.gt.1300)then
              aalb=aalb+(swu/tsw)
              aan=aan+1
            endif
          endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          TMIN=TMIN+nummin
          IF(TMIN.ge.60) THEN
            TMIN=0
            THR=THR+1
          ENDIF
          IF(THR.EQ.24) THEN
            THR=0
            TDY=TDY+1
            IF(tdy.gt.month(tmn)) then
              tdy=1
              tmn=tmn+1
              IF(tmn.gt.12) then
                tmn=1
              endif
            endif
          ENDIF
          ttim=thr*100+tmin
          lastim=ltim
          go to 22
        endif
      endif
 25   format(i8,i6,2x,i8,i6,3f10.6,3f10.1,f12.5,3i5,50f11.4)
 26   format(i8,i6,2x,i8,i6,2f10.6,3f10.1,f12.5,3i5,50f11.4)

      if(c.eq.0) then
        write(99,25) zdate,ztim,ldate,ltim,cosz,sncz,au,tsw,dsw,dir,swu
     %,tflg,dflg,rflg
      elseif(c.gt.0) then
            write(99,25) zdate,ztim,ldate,ltim,cosz,sncz,au,tsw,dsw,dir
     %,swu,tflg,dflg,rflg,(x(b(v)),v=1,c)
      endif

      lastim=ltim

      do 29 i=1,maxtim
	toa(i)=0.d0
	nsw(i)=0.d0
	difrat(i)=0.d0
	dr(i)=0.d0
	ndifr(i)=0.d0
	dif(i)=0.d0
	cz(i)=1.d0
	aau(i)=0.d0
	sw(i)=0.d0
	stdate(i)=0
	sthr(i)=0
        stmin(i)=0
 29   continue
      i=1
      if(cosz.gt.0.1D0.and.dsw.gt.1.d0.and.tsw.gt.1.d0) then
	cz(i)=cosz
	toa(i)=1365.d0*cosz
        nsw(i)=tsw/(cosz**nswb)
cc      use sum SW for difrat if avail, else tsw
        IF(dir.gt.1.0) then
	  difrat(i)=dsw/(dsw+dir)
        else
	  difrat(i)=dsw/tsw
        endif
	dr(i)=dir
	ndifr(i)=(difrat(i))*cosz**0.8d0
	dif(i)=dsw
	sw(i)=tsw
	aau(i)=au
	stdate(i)=ldate
	sthr(i)=lhr
	stmin(i)=minit
	i=i+1
        t=t+1
        cnt=0
      endif

cc      calc avg diffuse abledo if sitflg=1
cc      IF(sitflg.eq.1.and.dir.gt.10.0.and.dsw.gt.10.0.and.swu.gt.20.0)
cc     %then
cc        adn=adn+1
cc        IF((dir+dsw).gt.200.0.AND.ltim.gt.1100.and.ltim.lt.1300) then
cc          dalb=dalb+(swu/(dir+dsw))
cc          dn=dn+1
cc        endif
cc      ELSEIF(sitflg.eq.1.and.tsw.gt.20.0.and.swu.gt.20.0) then
      IF(sitflg.eq.1.and.tsw.gt.20.0.and.swu.gt.20.0) then
        adn=adn+1
        dalim=sncz*100.0
        IF(dalim.lt.50.0) dalim=50.0
        IF(tsw.gt.dalim.AND.ltim.gt.1100.and.ltim.lt.1300) then
          dalb=dalb+(swu/tsw)
          dn=dn+1
        ELSEIF(tsw.gt.dalim.AND.ltim.lt.1100)then
          malb=malb+(swu/tsw)
          man=man+1
        ELSEIF(tsw.gt.dalim.AND.ltim.gt.1300)then
          aalb=aalb+(swu/tsw)
          aan=aan+1
        endif
      endif

      TMIN=TMIN+nummin
      IF(TMIN.ge.60) THEN
	TMIN=0
	THR=THR+1
      ENDIF
      IF(THR.EQ.24) THEN
	THR=0
        TDY=TDY+1
        IF(tdy.gt.month(tmn)) then
          tdy=1
          tmn=tmn+1
          IF(tmn.gt.12) then
            tmn=1
          endif
        endif
      ENDIF
      ttim=thr*100+tmin

 30   read(2,*,end=50,err=30) (y(u),u=1,k),(x(v),v=1,l)
      do xi=1,c
        IF(x(b(xi)).LT.-9999.0.or.x(b(xi)).GT.99999.0) x(b(xi))=-9999.0
      end do
      if(datlog) then
        date=y(a1)
        if(yrlog) then
          yr=(date/10000)
          longyr=(cent*100)+yr
          mn=(date-yr*10000)/100
          dy=(date-yr*10000-mn*100)
        else
          longyr=(date/10000)
          yr=longyr-(cent*100)
          mn=(date-longyr*10000)/100
          dy=(date-longyr*10000-mn*100)
        endif
        date=longyr*10000+mn*100+dy
      ELSEIF(ncdx) then
        date=INT(x(a1))
        if(yrlog) then
          yr=(date/10000)
          longyr=(cent*100)+yr
          mn=(date-yr*10000)/100
          dy=(date-yr*10000-mn*100)
        else
          longyr=(date/10000)
          yr=longyr-((longyr/100)*100)
          mn=(date-longyr*10000)/100
          dy=(date-longyr*10000-mn*100)
        endif
        date=longyr*10000+mn*100+dy
      ELSEIF(jdat) then
        date=y(a1)
        yr=date/1000
        doy=date-(yr*1000)
        call DOY2DATE(yr,doy,date)
        longyr=(date/10000)
        yr=longyr-(cent*100)
        mn=date/100 - longyr*100
        dy=date-longyr*10000-mn*100
      ELSEIF(jdat2) then
        yr=y(a1)
        doy=y(a2)
        call DOY2DATE(yr,doy,date)
        longyr=(date/10000)
        yr=longyr-(cent*100)
        mn=date/100 - longyr*100
        dy=date-longyr*10000-mn*100
      else
        yr=y(a1)
        mn=y(a2)
        dy=y(a3)
        if(yrlog) then
          longyr=(cent*100)+yr
          date=longyr*10000+mn*100+dy
        else
          longyr=yr
          yr=longyr-(cent*100)
          date=longyr*10000+mn*100+dy
        endif
      endif
      if(timflg.eq.0) then
        tim=y(a4)
        hr=tim/100
        minit=tim-(hr*100)
        dmin=hr*60+minit
      elseif(timflg.eq.2) then
        dmin=y(a4)
        hr=(dmin)/60
        minit=dmin-(hr*60)
        tim=(hr*100)+minit
      elseif(ncdx) then
        ncdxtim=x(a1)-date
        tim=NINT(ncdxtim*10000)
        hr=tim/100
        minit=tim-(hr*100)
        dmin=hr*60+minit
        hr=(dmin)/60
        minit=dmin-(hr*60)
        tim=(hr*100)+minit
        dmin=hr*60+minit
      elseIF(timflg.eq.4) then
        tim=y(a4)
        hr=tim/10000
        minit=(tim/100)-(hr*100)
        tim=hr*100+minit
        dmin=hr*60+minit
      else
        hr=y(a4)
        minit=y(a5)
        tim=(hr*100)+minit
        dmin=hr*60+minit
      endif

c     Check for leap year
      IF(MOD(longyr,4).ne.0) then
        month(2)=28
      ELSEIF(MOD(longyr,400).eq.0) then
        month(2)=29
      ELSEIF(MOD(longyr,100).eq.0) then
        month(2)=28
      ELSE
        month(2)=29
      endif

ccccccccccccccc  time = 2360 bug fix cccccccccccccc
      IF(tim.gt.2359.or.dmin.ge.1440) then
        dmin=dmin-1440
        hr=(dmin)/60
        minit=dmin-(hr*60)
        tim=(hr*100)+minit
        dy=dy+1
        IF(dy.gt.month(mn)) then
          dy=1
          mn=mn+1
          IF(mn.gt.12) then
            mn=1
            longyr=longyr+1
            yr=longyr
          endif
        endif
        date=longyr*10000+mn*100+dy
        ckdate=date
      endif
ccccccccccccccc END: time = 2360 bug fix cccccccccccccc

cccccccccccccccc apply time adjustment if needed  ccccccccccc
      IF(timadj.ne.0) then

cc      WRITE(6,*)' adjusting time:',date,tim,timadj,dmin

        call DATE2DOY(date,longyr,doy)
cc      WRITE(6,*)'date2doy:',date,longyr,doy

c         Check for leap year [for "yr" = 4-digit year]
          IF(MOD(longyr,4).ne.0) then
            ydy=365
          ELSEIF(MOD(longyr,400).eq.0) then
            ydy=366
          ELSEIF(MOD(longyr,100).eq.0) then
            ydy=365
          ELSE
            ydy=366
          endif
cc      WRITE(6,*)' ydy=',ydy

        dmin=dmin+timadj
        IF(dmin.lt.0) then
          dmin=1440+dmin
          doy=doy-1
          IF(doy.eq.0) then
            longyr=longyr-1
            doy=ydy
          endif
        ELSEIF(dmin.gt.2359) then
          dmin=dmin-1440
          doy=doy+1
          IF(doy.gt.ydy) then
            doy=1
            longyr=longyr+1
          endif
        endif

cc      WRITE(6,*)' adj dmin,doy,longyr:',dmin,doy,longyr

        hr=(dmin)/60
        minit=dmin-(hr*60)
        tim=(hr*100)+minit

        call DOY2DATE(longyr,doy,date)
         yr=date/10000
         mn=date/100 - yr*100
         dy=date-yr*10000-mn*100

cc      WRITE(6,*)'Time adjusted:',date,tim,longyr
cc        ckdate=date
      endif
cccccccccccccccc end apply time adjustment  ccccccccccc


      IF(date.ne.ckdate.and.chkday) then
        WRITE(6,*)' '
        WRITE(6,*)' '
        WRITE(6,*)' '
        WRITE(6,*)' '
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(6,*)'WARNING -- Date in input files has changed!'
        WRITE(6,*)'This will likely cause problems later in the code!'
        WRITE(6,*)'Start of file date: ',ckdate
        WRITE(6,*)'Currently read date: ',date
        WRITE(6,*)'Current input file: ',usefil
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(6,*)' '
        WRITE(6,*)' '
        WRITE(6,*)' '
        IF(debug) pause
        e=index(infil,'.')
        errfil=infil(1:e)//'xtr'
        OPEN(UNIT=111,FILE=errfil,STATUS='unknown',access='append')
  32    READ(2,33,END=34) errlin
  33    FORMAT(a300)
        WRITE(111,2) errlin
        GO TO 32
  34    CLOSE(UNIT=111)
        GO TO 50
      endif

      IF(ip5.gt.0) then
        zhr=hr
        zyr=longyr
        zmn=mn
        zdy=dy
        lyr=longyr
        lmn=mn
        ldy=dy
        lhr=hr+hrofst
        IF(lhr.lt.0) then
          lhr=lhr+24
          ldy=ldy-1
        ELSEIF(lhr.gt.23) then
          lhr=lhr-24
          ldy=ldy+1
        endif
        IF(ldy.eq.0) then
          lmn=lmn-1
          IF(lmn.gt.0) then
            ldy=month(lmn)
          else
            lmn=12
            ldy=month(lmn)
            longyr=longyr-1
            lyr=longyr
          endif
        elseIF(ldy.gt.month(lmn)) then
          ldy=1
          lmn=lmn+1
          IF(lmn.gt.12) then
            lmn=1
            longyr=longyr+1
            lyr=longyr
          endif
        endif
        zdate=zyr*10000+zmn*100+zdy
        ztim=zhr*100+minit
        ldate=lyr*10000+lmn*100+ldy
        ltim=lhr*100+minit
      else
        lhr=hr
        lyr=longyr
        lmn=mn
        ldy=dy
        zyr=longyr
        zmn=mn
        zdy=dy
        zhr=hr-hrofst
        IF(zhr.lt.0) then
          zhr=zhr+24
          zdy=zdy-1
        ELSEIF(zhr.gt.23) then
          zhr=zhr-24
          zdy=zdy+1
        endif
        IF(zdy.eq.0) then
          zmn=zmn-1
          IF(zmn.gt.0) then
            zdy=month(zmn)
          else
            zmn=12
            zdy=month(zmn)
            longyr=longyr-1
            zyr=longyr
          endif
        elseIF(zdy.gt.month(zmn)) then
          zdy=1
          zmn=zmn+1
          IF(zmn.gt.12) then
            zmn=1
            longyr=longyr+1
            zyr=longyr
          endif
        endif
        zdate=zyr*10000+zmn*100+zdy
        ztim=zhr*100+minit
        ldate=lyr*10000+lmn*100+ldy
        ltim=lhr*100+minit
      endif

      IF(ldate.eq.cosdat.and.ltim.le.lastim) GO TO 30

      IF(ldate.ne.cosdat) cosok=.true.
      IF(cosok) then
	 if(n.gt.0) then
	   avndfr=avndfr/(n*1.d0)
	   avnsw=avnsw/(n*1.d0)
	 else
	   avndfr=1.d0
	   avnsw=-1.d0
	 endif
         IF(dn.gt.50/nummin.or.dn.ge.10) then
           dalb=dalb/(dn*1.0)
           IF(dalb.gt.1.0) dalb=-9.0
         else
           dalb=-9.0
         endif
         IF(man.gt.50/nummin.or.man.ge.10) then
           malb=malb/(man*1.0)
           IF(malb.gt.1.0) malb=1.0
         else
           malb=-9.0
         endif
         IF(aan.gt.50/nummin.or.aan.ge.10) then
           aalb=aalb/(aan*1.0)
           IF(aalb.gt.1.0) aalb=1.0
         else
           aalb=-9.0
         endif
         write(8,51)cosdat,avndfr,avnsw,sncz,n,t,dalb,dn,adn,malb,
     %aalb,man,aan
         IF(debug) write(6,51)cosdat,avndfr,avnsw,sncz,n,t
         if(n.ge.delnum) write(89,54) outfil
        n=0
        t=0
        adn=0
        dalb=0.0
        malb=0.0
        aalb=0.0
        dn=0
        man=0
        aan=0
        avndfr=0.d0
	avnsw=0.d0
        cosdat=ldate
        CLOSE(UNIT=99)
        CLOSE(UNIT=3)
        OPEN(UNIT=101,FILE=datfil,STATUS='unknown')
        WRITE(101,23) lyr*10000+lmn*100+ldy
        CLOSE(UNIT=101)
        OPEN(UNIT=101,FILE=datfil,STATUS='old')
        READ(101,24) stem
        CLOSE(UNIT=101)
        cosfil=stem//'.cos'
	open(unit=99,file=cosfil,status='unknown')
        WRITE(90,54) cosfil
        IF(sitflg.eq.2) then
      write(99,16)'   zdate  ztim     ldate  ltim      CosZ      SNCZ
     %  AU        SWdn       Dif        Dir     SolX    tflg dflg rflg',
     %(h(v),v=1,30)
        elseIF(sitflg.eq.3) then
      write(99,16)'   zdate  ztim     ldate  ltim      CosZ      SNCZ
     %  AU        SWdn       Dif        Dir      aux    tflg dflg rflg',
     %(h(v),v=1,30)
        else
      write(99,16)'   zdate  ztim     ldate  ltim      CosZ      SNCZ
     %  AU        SWdn       Dif        Dir     SWup    tflg dflg rflg',
     %(h(v),v=1,30)
        endif
        OUTFIL=stem//'.clr'
	open(unit=3,file=outfil,status='unknown')
      WRITE(3,5)'   Date  Hr Mn    CosZ     AAU      TotSW   NSW      Di
     %f    Dir    NDfr    DifRat   TOA      delT    delS    StDDR'
        cosok=.false.
        ok=.true.
ccccccccccccccccccccccccccccccccc
        snczgo=.true.
ccccccccccccccccccccccccccccccccc
        lastim=ltim
      endif

      if(zform.gt.0) zd=x(a9)

      if(snczgo) then
cc         IP6 = 1
cc         IP8 = 0
         CALL EPHEMS(SLAT,SLON,dy,mn,longyr,TIME,IP5,1,IP7,0,
     +            AZ,AU,Z,HRANG,DEC)

        sncz=dCOS(Z*PI/180.d0)
        sncz1=sncz+0.1d0
        if(sncz1.gt.1.d0) sncz1=1.d0
        write(6,*)'  Solar Noon CosZ: ',sncz,'   time: ',time
        snczgo=.false.
      endif

      rdmin=dmin*1.d0+timadd
      time=INT(rdmin/60)*100+(rdmin-INT(rdmin/60)*60)

      if(zform.eq.0) then
        IF(time.gt.59.59d0.and.time.lt.101.00d0.and.ip6.eq.0) then
          gt=101.01d0
          CALL EPHEMS(SLAT,SLON,dy,mn,longyr,GT,IP5,IP6,IP7,IP8,
     +            AZ,AU,Z,HRANG,DEC)
          Gx(1)=az
          Gx(2)=au
          Gx(3)=Z
          Gx(4)=dec
          gt=59.59d0
          CALL EPHEMS(SLAT,SLON,dy,mn,longyr,GT,IP5,IP6,IP7,IP8,
     +            AZ,AU,Z,HRANG,DEC)
          gt=(time-100.d0)+.01
          gt=gt/0.61d0
          az=((Gx(1)-az)*gt)+az
          au=((Gx(2)-au)*gt)+au
          Z=((Gx(3)-Z)*gt)+Z
          dec=((Gx(4)-dec)*gt)+dec
        else
          CALL EPHEMS(SLAT,SLON,dy,mn,longyr,TIME,IP5,IP6,IP7,IP8,
     +            AZ,AU,Z,HRANG,DEC)
        endif
        cosz=dCOS(Z*PI/180.d0)
      else
        if(zform.eq.1) cosz=(Zd)
        if(zform.eq.2) cosz=dCOS(Zd*pi/180.d0)
        if(zform.eq.3) cosz=dCOS(Zd)
      endif
      if(cosz.lt.0.d0.and..not.ngt) go to 30
      IF(OK) THEN
        TMN=lmn
        TDY=ldy
        THR=lhr
        TMIN=minit
        ttim=thr*100+tmin
	OK=.FALSE.
      ENDIF
      IF(x(a6).GT.-20.0) then
        tsw=x(a6)
        tflg=0
      else
        tflg=1
        tsw=x(a6)
        IF(cosZ.gt.0.0) write(77,26) zdate,ztim,ldate,ltim,cosz,au,tsw,
     %dsw,dir,swu,tflg,dflg,rflg
        tsw=-999.0
      endif
      IF(x(a7).GT.-20.0) then
        dsw=x(a7)
        dflg=0
      else
        dflg=1
        dsw=x(a7)
        IF(cosZ.gt.0.0) write(77,26) zdate,ztim,ldate,ltim,cosz,au,tsw,
     %dsw,dir,swu,tflg,dflg,rflg
        dsw=-999.0
      endif
      IF(x(a10).GT.-20.0.and.sitflg.gt.0) then
        swu=x(a10)
      else
        swu=-999.0
      endif

      rflg=0
      if(nrmdir.and.x(a8).GT.-20.0) then
        IF(cosz.ge.0.0) then
          dir=x(a8)*cosz
        else
          dir=x(a8)
        endif
      elseIF(x(a8).GT.-20.0) then
        dir=x(a8)
      ELSEIF(tsw.GT.-1.0.and.dsw.GT.-1.0) then
        rflg=1
        dir=x(a8)
        IF(cosZ.gt.0.0) write(77,26) zdate,ztim,ldate,ltim,cosz,au,tsw,
     %dsw,dir,swu,tflg,dflg,rflg
        dir=tsw-dsw
      else
        rflg=-9
        dir=x(a8)
       write(77,26) zdate,ztim,ldate,ltim,cosz,au,tsw,dsw,dir,swu,tflg,
     %dflg,rflg
        dir=rerr
      endif
      IF(dir.LT.-20.0.or.dir.gt.9999.0) dir=-999.0

 35   IF(lTIM.gt.ttim) THEN
        cnt=cnt+1
cc        write(99,25)zdate,ztim,ldate,ttim,serr,serr,rerr,rerr,rerr,rerr
cc     %,-9,-9,-9,serr,serr,serr,serr,serr,serr,serr,serr,serr,serr,serr,
cc     %serr,serr,serr,serr,serr,serr,serr,serr,serr
        TMIN=TMIN+nummin
        IF(TMIN.ge.60) THEN
          TMIN=0
          THR=THR+1
        ENDIF
        IF(THR.EQ.24) THEN
          THR=0
          TDY=TDY+1
          IF(tdy.gt.month(tmn)) then
            tdy=1
            tmn=tmn+1
            IF(tmn.gt.12) then
              tmn=1
            endif
          endif
        ENDIF
        if(cnt.gt.14/nummin) then
          ttim=ltim
          thr=lhr
          tmin=minit
          cnt=0
        else
          ttim=thr*100+tmin
        endif
        GO TO 35
      endif
      cnt=0

      IF(dsw.LT.-998.0.and.tsw.GT.-20.0.and.dir.gt.-20.0) then
        dsw=tsw-dir
        dflg=9
      endif
      IF(tsw.LT.-998.0.and.dsw.GT.-20.0.and.dir.gt.-20.0) then
        tsw=dsw+dir
        tflg=9
      endif
      IF(chkflg) then
        if(dabs(tsw-(dsw+dir)).gt.100.d0)  then
          dflg=2
          IF(cosZ.gt.0.0) write(77,26) zdate,ztim,ldate,ltim,cosz,au,tsw
     %,dsw,dir,swu,tflg,dflg,rflg
          dsw=rerr
        endif
        IF(tsw.gt.1500.d0) then
          tflg=2
          IF(cosZ.gt.0.0) write(77,26) zdate,ztim,ldate,ltim,cosz,au,tsw
     %,dsw,dir,swu,tflg,dflg,rflg
          tsw=rerr
        endif
        IF(dsw.LT.-10.0.or.tsw.LT.-10.0) then
          if(c.eq.0) then
           write(99,25) zdate,ztim,ldate,ltim,cosz,sncz,au,tsw,dsw,dir
     %,swu,tflg,dflg,rflg
          elseif(c.gt.0) then
           write(99,25) zdate,ztim,ldate,ltim,cosz,sncz,au,tsw,dsw,dir
     %,swu,tflg,dflg,rflg,(x(b(v)),v=1,c)
          endif
cc      calc avg diffuse abledo if sitflg=1
          IF(sitflg.eq.1.and.tsw.gt.20.0.and.swu.gt.20.0) then
            adn=adn+1
            dalim=sncz*100.0
            IF(dalim.lt.50.0) dalim=50.0
            IF(tsw.gt.dalim.AND.ltim.gt.1100.and.ltim.lt.1300) then
              dalb=dalb+(swu/tsw)
              dn=dn+1
            ELSEIF(tsw.gt.dalim.AND.ltim.lt.1100)then
              malb=malb+(swu/tsw)
              man=man+1
            ELSEIF(tsw.gt.dalim.AND.ltim.gt.1300)then
              aalb=aalb+(swu/tsw)
              aan=aan+1
            endif
          endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          TMIN=TMIN+nummin
          IF(TMIN.ge.60) THEN
            TMIN=0
            THR=THR+1
          ENDIF
          IF(THR.EQ.24) THEN
            THR=0
            TDY=TDY+1
            IF(tdy.gt.month(tmn)) then
              tdy=1
              tmn=tmn+1
              IF(tmn.gt.12) then
                tmn=1
              endif
            endif
          ENDIF
          ttim=thr*100+tmin
          lastim=ltim
          go to 30
        endif
      endif

      if(c.eq.0) then
        write(99,25) zdate,ztim,ldate,ltim,cosz,sncz,au,tsw,dsw,dir,swu
     %,tflg,dflg,rflg
      elseif(c.gt.0) then
        write(99,25) zdate,ztim,ldate,ltim,cosz,sncz,au,tsw,dsw,dir,swu
     %,tflg,dflg,rflg,(x(b(v)),v=1,c)
      endif

      lastim=ltim

      if(cosz.gt.0.1D0.and.dsw.gt.1.0.and.tsw.gt.1.0) then
	cz(i)=cosz
	toa(i)=1365.d0*cosz
        nsw(i)=tsw/(cz(i)**nswb)
cc      use sum SW for difrat if avail, else tsw
        IF(dir.gt.1.0) then
	  difrat(i)=dsw/(dsw+dir)
        else
	  difrat(i)=dsw/tsw
        endif
	dr(i)=dir
	ndifr(i)=(difrat(i))*cosz**0.8d0
	dif(i)=dsw
	sw(i)=tsw
	aau(i)=au
	stdate(i)=ldate
	sthr(i)=lhr
	stmin(i)=minit
	i=i+1
	t=t+1
      endif

cc      calc avg diffuse abledo if sitflg=1
cc      IF(sitflg.eq.1.and.dir.gt.10.0.and.dsw.gt.10.0.and.swu.gt.20.0)
cc     %then
cc        adn=adn+1
cc        IF((dir+dsw).gt.200.0.AND.ltim.gt.1100.and.ltim.lt.1300) then
cc          dalb=dalb+(swu/(dir+dsw))
cc          dn=dn+1
cc        endif
cc      ELSEIF(sitflg.eq.1.and.tsw.gt.20.0.and.swu.gt.20.0) then
      IF(sitflg.eq.1.and.tsw.gt.20.0.and.swu.gt.20.0) then
        adn=adn+1
        dalim=sncz*100.0
        IF(dalim.lt.50.0) dalim=50.0
        IF(tsw.gt.dalim.AND.ltim.gt.1100.and.ltim.lt.1300) then
          dalb=dalb+(swu/tsw)
          dn=dn+1
        ELSEIF(tsw.gt.dalim.AND.ltim.lt.1100)then
          malb=malb+(swu/tsw)
          man=man+1
        ELSEIF(tsw.gt.dalim.AND.ltim.gt.1300)then
          aalb=aalb+(swu/tsw)
          aan=aan+1
        endif
      endif

      if(i.eq.(maxtim+1).and.cosz.gt.0.0) then
	delT=dabs(toa(m+1)-toa(m-1))
	delS=dabs(sw(m+1)-sw(m-1))
	lim=13.d0*sncz*nummin
        dtplus= (delT+delfac*delSlm*cz(m))
        IF(dtplus.lt.1.1) dtplus=1.1
        dtminus= (delT-delfac*sncz1/cz(m))
        IF(dtminus.LT.-0.1) dtminus=-0.1
        if(cz(m).gt.0.2d0.and.nsw(m).gt.1000.d0.and.nsw(m).lt.
     %1350.d0+swadd)then
          go=.true.
        elseif(cz(m).le.0.2d0.and.nsw(m).gt.900.d0.and.nsw(m).lt.
     %1250.d0+swadd)then
          go=.true.
        else
          go=.false.
        endif
        if(delT.lt.lim.and.go.and.dif(m).lt.maxdif) then
         if(dels.lt.dtplus.and.dels.gt.dtminus) then
          do 40 j=1,maxtim
	    avg=ndifr(j)+avg
 40       continue
	  avg=avg/(maxtim*1.d0)
	  do 41 j=1,maxtim
	    rsum=(ndifr(j)-avg)**2+rsum
 41       continue
	  rsum=sqrt(rsum/(maxtim*1.d0))
          if(rsum.lt.ndstlm)then
	 write(3,42)stdate(m),sthr(m),stmin(m),cz(m),aau(m),sw(m),
     %nsw(m),dif(m),dr(m),ndifr(m),difrat(m),toa(m),delt,dels,rsum
 42      format(i8,2i3,2f9.5,4f8.1,2f9.5,f8.2,2f8.3,f9.5)
	    avndfr=avndfr+ndifr(m)
	    avnsw=avnsw+nsw(m)
	    n=n+1
	  endif
	 endif
	endif
	do 43 j=1,(maxtim-1)
	  toa(j)=toa(j+1)
	  nsw(j)=nsw(j+1)
	  difrat(j)=difrat(j+1)
	  dr(j)=dr(j+1)
	  ndifr(j)=ndifr(j+1)
	  dif(j)=dif(j+1)
	  cz(j)=cz(j+1)
	  sw(j)=sw(j+1)
	  aau(j)=aau(j+1)
	  stdate(j)=stdate(j+1)
	  sthr(j)=sthr(j+1)
	  stmin(j)=stmin(j+1)
 43     continue
	i=maxtim
	avg=0.d0
	rsum=0.d0
      endif
      TMIN=TMIN+nummin
      IF(TMIN.ge.60) THEN
	TMIN=0
	THR=THR+1
      ENDIF
      IF(THR.EQ.24) THEN
	THR=0
        TDY=TDY+1
        IF(tdy.gt.month(tmn)) then
          tdy=1
          tmn=tmn+1
          IF(tmn.gt.12) then
            tmn=1
          endif
        endif
      ENDIF
      ttim=thr*100+tmin
      go to 30

 50   CONTINUE
      CLOSE(UNIT=2)
 51      format(i8,f10.6,f10.1,f10.6,2i6,f10.5,2i6,2f10.5,2i6)
 54      format(a12)
      GO TO 10

 90   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'   <<<<<< Error opening configuration file >>>>>>>'
      write(6,*)'The file "',cnffil,'" must be in the same directory'
      write(6,*)'as the executable file.'
      write(6,*)'?????????????????????????????????????????????????????'
      pause
      go to 200

 94   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'   <<<<<< Error reading configuration file >>>>>>>'
      write(6,*)' Did you forget to add the new minutes offset in '
      write(6,*)' setting number 2)?'
      write(6,*)'?????????????????????????????????????????????????????'
	write(6,*)'read(1,*) slat,XLONG,ip5'
        WRITE(6,*)'lat = ',slat
        WRITE(6,*)'long = ',xlong
        WRITE(6,*)'Time Flag (0/1 for LST/GMT) = ', ip5
        WRITE(6,*)'Time adjustment (in minutes) = ', timadj
      pause
      go to 200


 100  CONTINUE
	 if(n.gt.0) then
	   avndfr=avndfr/(n*1.d0)
	   avnsw=avnsw/(n*1.d0)
	 else
	   avndfr=1.d0
	   avnsw=-1.d0
	 endif
         IF(dn.gt.50/nummin.or.dn.ge.10) then
           dalb=dalb/(dn*1.0)
           IF(dalb.gt.1.0) dalb=-9.0
         else
           dalb=-9.0
         endif
         IF(man.gt.50/nummin.or.man.ge.10) then
           malb=malb/(man*1.0)
           IF(malb.gt.1.0) malb=1.0
         else
           malb=-9.0
         endif
         IF(aan.gt.50/nummin.or.aan.ge.10) then
           aalb=aalb/(aan*1.0)
           IF(aalb.gt.1.0) aalb=1.0
         else
           aalb=-9.0
         endif
         write(8,51)cosdat,avndfr,avnsw,sncz,n,t,dalb,dn,adn,malb,aalb,
     %man,aan
         IF(debug) write(6,51)cosdat,avndfr,avnsw,sncz,n,t
         if(n.ge.delnum) write(89,54) outfil
cc KLG looks like 1 was not closed....
      CLOSE(UNIT=1)
      CLOSE(UNIT=8)
      CLOSE(UNIT=89)
      CLOSE(UNIT=90)
      CLOSE(UNIT=77)
         close(unit=3)
         close(unit=99)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(swrlim.lt.0.02) then
          WRITE(6,*)' before interpolate diffuse albedo coefficient,
     %swrlim=',swrlim
          pause
        endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cc    interpolate diffuse albedo coefficient if sitflg=1
      IF(sitflg.eq.1) then
        call albcof1('swclrid1.day','alb_cof1.asc')

        OPEN(UNIT=8, FILE='alb_cof1.asc', STATUS='old')
        OPEN(UNIT=98,FILE='alb_dir.asc',STATUS='unknown')
        WRITE(98,*)'   Date        DifAlb        Alba        Albb      D
     %evAlb    DirN    DifN    onan   snczalb   MinCosz      sncz    Mrn
     %Alb    AftAlb'
 101    READ(8,*,END=104) stem,dalb,x1,i,man,malb,aalb
        x1=x1
        OPEN(UNIT=99,FILE=stem//'.cos',STATUS='old')
        WRITE(6,*) '  Opened albedo file: ',stem//'.cos'
        READ(99,*)
        dn=0
        aczn=0
        aczmn=9.0
 102    READ(99,*,END=103) zdate,ztim,ldate,ltim,cosz,sncz,au,tsw,Dsw,
     %Dir,SWu

        IF(cosz.lt.0.05) GO TO 102

cc        IF(dir.gt.10.0.and.dsw.gt.10.0.and.swu.gt.20.0.and.dalb.gt.0.0)
cc     %then
cc          IF((dir+dsw).gt.200.0.AND.(dir/(dir+dsw)).gt.0.25) then
cc            dn=dn+1
cc            alb(dn)=(swu/(dir+dsw))-dalb
cc            dcz(dn)=cosz
cc            IF(ltim.lt.1100.or.ltim.gt.1300) aczn=aczn+1
cc          endif
cc        ELSEIF(tsw.gt.20.0.and.dir.gt.10.0.and.swu.gt.20.0.and.
        IF(tsw.gt.20.0.and.dir.gt.10.0.and.swu.gt.20.0.and.
     %dalb.gt.0.0) then
          IF(tsw.gt.200.0.AND.(dir/tsw).gt.0.25) then
            dn=dn+1
            alb(dn)=(swu/tsw)-dalb
            dcz(dn)=cosz
            IF(cosz.lt.aczmn) aczmn=cosz
            IF(ltim.lt.1100.or.ltim.gt.1300) aczn=aczn+1
          endif
        endif

        GO TO 102

 103    CLOSE(UNIT=99)

        IF(dn.gt.100/nummin) then
          i2=dn/20
          IF(i2.LT.(11/nummin)+1) i2=(11/nummin)+1
          do xi=(dn+1),(dn+i2)
             alb(xi)=0.0
             dcz(xi)=sncz
          end do
          dn=xi

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(swrlim.lt.0.02) then
          WRITE(6,*)' before call sumsqfit,swrlim=',swrlim
          pause
        endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          call sumsqfit(dcz,alb,dn,albb,alba,devalb)

          IF(alba.gt.1.0.or.alba.LT.(-1.0)) THEN
            alba=-9.0
            albb=-9.0
          elseIF(albb.gt.1.0.or.albb.LT.(-1.0)) THEN
            alba=-9.0
            albb=-9.0
          elseIF(alba.LT.(-dalb)) THEN
            alba=-dalb
          elseIF((aczmn/sncz).gt.0.9) THEN
            alba=-9.0
            albb=-9.0
          endif
        else
           alba=-9.0
           albb=-9.0
           devalb=-9.0
        endif

        IF(alba.GT.-8.0) then
          alb(1)=dalb+alba+albb*sncz
        else
          alb(1)=-999.0
        endif

        WRITE(98,108) stem,dalb,alba,albb,devalb,dn,i,aczn,alb(1),
     %aczmn,sncz,malb,aalb
        GO TO 101

 104    CLOSE(UNIT=8)
        CLOSE(UNIT=98)


ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(swrlim.lt.0.02) then
          WRITE(6,*)' before call albcof2,swrlim=',swrlim
          pause
        endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        call albcof2('alb_dir.asc','alb_cof2.asc')

      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(swrlim.lt.0.02) then
          WRITE(6,*)' after albedo loop,swrlim=',swrlim
          pause
        endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF(debug) pause

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(swrlim.lt.0.02) then
          WRITE(6,*)' before CALL swclreq1,swrlim=',swrlim
          pause
        endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      CALL swclreq1(dyfil,delnum,dir2,noclr)
      IF(noclr) then
        WRITE(6,*)' No clear enough days detected in this run'
        pause
        GO TO 200
      endif
      IF(debug) then
       WRITE(6,*) 'End of swclrqe1..'
       pause
      endif
      call clrcof1(dyfil,delnum)
      IF(debug) then
       WRITE(6,*) 'End of clrcof1..'
       pause
      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(swrlim.lt.0.02) then
          WRITE(6,*)' before call swclrid2 loop,swrlim=',swrlim
          pause
        endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do itrinc=1,numitr
        call swclrid2(cosdir,maxtim,delnum,ndstlm,delSlm,tswlim,maxdif,
     %debug,delfac,nswmx,nswmn)
        IF(debug) then
         WRITE(6,*) 'End of swclrid2..'
         pause
        endif
        call swclreq2(dyfil,delnum)
        IF(debug) then
         WRITE(6,*) 'End of swclreq2...'
         pause
        endif
        call clrcof2(dyfil,delnum,lsdate,lsdfra,lsdfrb,lscswa,lscswb)
        IF(debug) then
         WRITE(6,*) 'End of clrcof2...'
         WRITE(6,*) 'lsdate = ', lsdate
         WRITE(6,110) 'lsdfra = ', lsdfra
         WRITE(6,110) 'lsdfrb = ', lsdfrb
         WRITE(6,110) 'lscswa = ', lscswa
         WRITE(6,110) 'lscswb = ', lscswb
         pause
        endif
      end do
      OPEN(UNIT=78,FILE='swlastdy.cfn',STATUS='unknown')
      WRITE(78,*)'  Last day with retrieved coefficients:'
      WRITE(78,105)lsdate,lsdfra,lsdfrb,lscswa,lscswb
 105  format(i8,2f10.6,f10.2,f10.6)
 106  format(a1)
 107  format(2i8,4f12.6)
 108  format(a8,2x,4f12.6,3i8,6f10.4)
 109  format(i8,5f12.6)
 110  format(a,f14.6)
      CLOSE(UNIT=78)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(swrlim.lt.0.02) then
          WRITE(6,*)' before call swclrid3,swrlim=',swrlim
          pause
        endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cc      call swclrid3(cosdir,maxtim,delnum,ndstlm,delSlm,tswlim,clrtim,
cc     %cldflg,maxdif,sitflg,delfac)
      call swclrid3(cosdir,maxtim,delnum,ndstlm,delSlm,tswlim,clrtim,
     %cldflg,maxdif,sitflg,delfac,nswmx,nswmn)
        IF(debug) then
         WRITE(6,*) 'End of swclrid3...'
         pause
        endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(swrlim.lt.0.02) then
          WRITE(6,*)' before call swclreq3,swrlim=',swrlim
          pause
        endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      call swclreq3(dyfil,delnum,sitflg,notgd)
        IF(debug) then
         WRITE(6,*) 'End of swclreq3...'
         pause
        endif
        IF(notgd) GO TO 200

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(swrlim.lt.0.02) then
          WRITE(6,*)' before call swclrfcg,swrlim=',swrlim
          pause
        endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      call swclrfcg(dyfil,delnum,cosdir,swrlim,sitflg,c,maxdif,czlim,
     %mfac,ngt,slat,xlong)

 200  STOP
      END

cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine swclreq1(dyfil,mlim,dirfil,noclr)
      PARAMETER (NMAX=199999)
      INTEGER N,totn,i,j,date,hr,imin,mlim,clrdyn
      REAL*8 cosz,armang,tsw,nsw,dif,dir,ndir,difrat,toa,delt,dels,rsum
      REAL*8 dfrb,dfra,cswa,cswb,devcsw,devdfr,avndr,sncz
      REAL*8 atsw(NMAX),acz(NMAX),adfr(NMAX),avnsw,rerr
      character*12 inpfil,infil,outfil,dirfil
      character*8 inpdat
      logical ok,dyfil,noclr

      inpfil='swclrid1.day'
      outfil='swclreq1.asc'
      ok=.true.
      noclr=.true.
      rerr=-9.0d0
      i=0
      clrdyn=0

      open(unit=1,file=inpfil,status='old')
      open(unit=89,file=dirfil,status='old')
      open(unit=3,file=outfil,status='unknown')
      write(3,*)'  for: clrDfR OR clrTSW = a*(CosZ)**b    >>>swclreq1'
      write(3,2)'   date      dfra     dfrb     devdfr     cswa     cswb
     %      devcsw    sncz      N'
      WRITE(6,*)' '
      WRITE(6,*)' '
      write(6,*)'  for: clrDfR OR clrTSW = a*(CosZ)**b    >>>swclreq1'
      write(6,2)'   date      dfra    dfrb    devdfr    cswa    cswb
     %devcsw   sncz    N'
 2    format(a82)
 3    format(a12)
      read(1,*)

 4    read(1,5,end=50,err=100)inpdat,avndr,avnsw,sncz,n,totn
 5    format(a8,f10.6,f10.1,f10.6,2i6)
      if(n.ge.mlim.and.dyfil) then
        read(89,3) infil
        noclr=.false.
        clrdyn=clrdyn+1
        i=0
      ELSEIF(.not.dyfil.and.n.ge.mlim) then
        read(89,3) infil
        noclr=.false.
      else
        if(dyfil) write(3,71)inpdat,rerr,rerr,rerr,rerr,rerr,rerr,sncz,n
        go to 4
      endif

      if(ok) then
      do 6 j=1,NMAX
	 atsw(J)=0.d0
         acz(J)=0.d0
         adfr(j)=0.d0
 6    continue
      ok=.false.
      endif

      open(unit=2,file=infil,status='old')
      read(2,*)
 9    read(2,10,end=20)date,hr,imin,cosz,armang,tsw,nsw,dif,dir,ndir,
     %difrat,toa,delt,dels,rsum
 10   format(i8,2i3,2f9.5,4f8.1,2f9.5,f8.2,2f8.3,f9.5)
      if(cosz.gt.0.1d0.and.difrat.gt.0.d0.and.tsw.gt.0.d0) then
        i=i+1
        IF(i.gt.NMAX) then
          WRITE(6,*)'**************************************************'
          WRITE(6,*)'Number of clear points greater than',NMAX,' limit'
          WRITE(6,*)'          in swclreq1 subroutine.'
          WRITE(6,*)' Processing up to ',date
          WRITE(6,*) hr,imin,' only!'
          WRITE(6,*)'    Click on OK to continue.'
          WRITE(6,*)'**************************************************'
          pause
          i=NMAX
          GO TO 50
        endif
        atsw(i)=log10(tsw)
	acz(i)=log10(cosz)
        adfr(i)=log10(difrat)
      endif
      go to 9

 20   continue
      close(unit=2)
      if(.not.dyfil) go to 4

 50   continue
      if(noclr.and.dyfil)then
        if(clrdyn.gt.0) noclr=.false.
        go to 100
      endif
      if(noclr) go to 100
      if(dyfil.and.i.lt.mlim) then
        write(3,71)inpdat,rerr,rerr,rerr,rerr,rerr,rerr,sncz,i
        write(6,72)' ',inpdat,rerr,rerr,rerr,rerr,rerr,rerr,sncz,i
        go to 80
      endif

      call medfit(acz,adfr,i,dfra,dfrb,devdfr)
      dfra=10.d0**dfra

      call medfit(acz,atsw,i,cswa,cswb,devcsw)
      cswa=10.d0**cswa

cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF(dyfil) then
        IF(dfra.gt.1.0.or.dfra.lt.0.0) then
          dfra=-9.0
          dfrb=-9.0
          i=-44
        endif
        IF(dfrb.gt.0.0.or.dfrb.lt.-2.0) then
          dfra=-9.0
          dfrb=-9.0
          i=-44
        endif
        IF(cswa.gt.2000.0.or.cswa.lt.0.0) then
          cswa=-9.0
          cswb=-9.0
          i=-44
        endif
        IF(cswb.gt.2.0.or.cswb.lt.0.9) then
          cswa=-9.0
          cswb=-9.0
          i=-44
        endif
      endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if(.not.dyfil) inpdat='000000'

cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      if(.not.dyfil) sncz=-9.0
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF(.not.dyfil.and.dfrb.GT.-0.4) dfrb=-0.4
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 70   write(3,71)inpdat,dfra,dfrb,devdfr,cswa,cswb,devcsw,sncz,i
      write(6,72)' ',inpdat,dfra,dfrb,devdfr,cswa,cswb,devcsw,sncz,i
 71   format(a8,f10.6,2f10.6,f8.1,2f10.6,f10.6,i10)
 72   format(a1,a8,3f9.5,f8.1,3f9.5,i8)
 80   if(dyfil) then
        noclr=.true.
        ok=.true.
        i=0
        go to 4
      else
        go to 100
      endif

 100  continue
      close(unit=1)
      close(unit=3)
cc KLG I added close 89
      close(unit=89)
      return
      end
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE MEDFIT(X,Y,NDATA,A,B,ABDEV)

C   Fits Y = a + b*X by the criterion of least ABSOLUTE deviations
c   (robust estimation, eliminating undue influence of outliers on
c   the linear fit). The arrays X and Y, of length NDATA, are the
c   input experimental points. The fitted parameters are output,
c   along with ABDEV which is the mean absolute deviation (in Y)
c   of the experimental points from the fitted line. This routine
c   uses the routine ROFUNC, with communication via a common block.

      real*8 x,y,a,b,abdev,sx,sy,sxy,sxx,xt,yt,arr,aa,abdevt,del,bb
      real*8 chisq,sigb,b1,f1,b2,f2
      INTEGER num
cccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER ln1,ln2
cccccccccccccccccccccccccccccccccccccccccccccc
      PARAMETER (NMAX=199999)
      EXTERNAL ROFUNC
      DIMENSION X(NDATA),Y(NDATA)
      COMMON /ARRAYS/ NDATAT,XT(NMAX),YT(NMAX),ARR(NMAX),AA,ABDEVT

      ln1=0
      ln2=0
      num=0
      SX=0.D0
      SY=0.D0
      SXY=0.D0
      SXX=0.D0
      DO 11 J=1,NDATA
	XT(J)=X(J)
	YT(J)=Y(J)
	SX=SX+X(J)
	SY=SY+Y(J)
	SXY=SXY+X(J)*Y(J)
	SXX=SXX+X(J)**2
 11   CONTINUE

      NDATAT=NDATA
      DEL=NDATA*SXX-SX**2
      AA=(SXX*SY-SX*SXY)/DEL
      BB=(NDATA*SXY-SX*SY)/DEL
      CHISQ=0.D0
      DO 12 J=1,NDATA
	CHISQ=CHISQ+(Y(J)-(AA+BB*X(J)))**2
 12   CONTINUE
      SIGB=SQRT(CHISQ/DEL)
      B1=BB
      F1=ROFUNC(B1)
      B2=BB+SIGN(2.D0*SIGB,F1)
      F2=ROFUNC(B2)

 20   IF(F1*F2.GT.0.D0) THEN
	BB=2.D0*B2-B1
	B1=B2
	F1=F2
	B2=BB
	F2=ROFUNC(B2)
        ln1=ln1+1
        IF(ln1.gt.20) GO TO 25
	GO TO 20
      ENDIF
 25   continue
      SIGB=0.01D0*SIGB
 30   IF(ABS(B2-B1).GT.SIGB) THEN
	BB=0.5D0*(B1+B2)
	IF(BB.EQ.B1.OR.BB.EQ.B2) GO TO 40
        num=num+1
        IF(num.ge.100) GO TO 40
	F=ROFUNC(BB)
	IF(F*F1.GE.0.D0) THEN
	  F1=F
	  B1=BB
	ELSE
	  F2=F
	  B2=BB
	ENDIF
        ln2=ln2+1
        IF(ln2.gt.20) GO TO 35
	GO TO 30
      ENDIF
 35   continue
 40   A=AA
      B=BB
      ABDEV=ABDEVT/(NDATA*1.D0)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION ROFUNC(B)

C   Evaluates the right-hand side of equation (14.6.16 of Numerical
c   Recipies ed. 1 for fortran) for a given value of B. Communication
c   with the subroutine MEDFIT is through a common block.

      real*8 b,x,y,arr,aa,abdev,rsum,d
      PARAMETER (NMAX=199999)
      COMMON /ARRAYS/ NDATA,X(NMAX),Y(NMAX),ARR(NMAX),AA,ABDEV
      N1=NDATA+1
      NML=N1/2
      NMH=N1-NML
      DO 11 J=1,NDATA
	ARR(J)=Y(J)-B*X(J)
 11   CONTINUE
      CALL SORT(NDATA,ARR)
      AA=0.5D0*(ARR(NML)+ARR(NMH))
      rsum=0.D0
      ABDEV=0.D0
      DO 12 J=1,NDATA
	D=Y(J)-(B*X(J)+AA)
	ABDEV=ABDEV+ABS(D)
        rsum=rsum+X(J)*SIGN(1.d0,D)
 12   CONTINUE
      ROFUNC=rsum
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE SORT(N,RA)

C   Sorts an array RA of length N into ascending numerical order using
c   the Heapsort algorithm. N is input, RA is replaced on output by
c   its sorted rearrangment
      REAL*8 RA,RRA
      DIMENSION RA(N)
      L=N/2+1
      IR=N

C   The index L will be decremented from its initial value down to 1
c   during the "heap creation" (hiring) phase. Once it reaches 1, the
c   index IR will be decremented from its initial value down to 1
c   during the "heap selection" (retirement and promotion) phase.

 10   CONTINUE
      IF(L.GT.1) THEN
	L=L-1
	RRA=RA(L)
      ELSE
	RRA=RA(IR)
	RA(IR)=RA(1)
	IR=IR-1
	IF(IR.EQ.1) THEN
	  RA(1)=RRA
	  RETURN
	ENDIF
      ENDIF
      I=L
      J=L+L
 20   IF(J.LE.IR) THEN
	IF(J.LT.IR) THEN
	  IF(RA(J).LT.RA(J+1)) J=J+1
	ENDIF
	IF(RRA.LT.RA(J)) THEN
	  RA(I)=RA(J)
	  I=J
	  J=J+J
	ELSE
	  J=IR+1
	ENDIF
	GO TO 20
      ENDIF
      RA(I)=RRA
      GO TO 10
      END
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine ALBCOF1(coffil,outfil)
      PARAMETER (NDAYMAX=12000)
      INTEGER n,cdate(0:NDAYMAX),c(0:NDAYMAX),date,i,k
      INTEGER l,j,m(5),d(0:NDAYMAX),nb
      REAL coeff(0:NDAYMAX),lc,ocoeff(0:NDAYMAX),x(10),ma(0:NDAYMAX)
      REAL fa(0:NDAYMAX)
      CHARACTER*12 COFFIL,lasfil,outfil
      LOGICAL ok

      lasfil='alb_coef.end'
      do n=0,NDAYMAX
        ocoeff(n)=0.0
        ma(n)=-9.0
        fa(n)=-9.0
        coeff(n)=-9.0
        cdate(n)=0
        c(n)=0
        d(n)=0
      end do
      OK=.TRUE.
      nb=0

      open(unit=1,file=coffil,status='old',err=91)
      open(unit=2,file=outfil,status='unknown')
      read(1,*)
      n=0
 2      n=n+1
        IF(n.gt.NDAYMAX) then
          WRITE(6,*)'*********************************************'
          WRITE(6,*)'Number of days greater than limit of',NDAYMAX,'!'
          WRITE(6,*)' Processing up to ',cdate(n-1),' only!'
          WRITE(6,*)'    Click on OK to continue.'
          WRITE(6,*)'*********************************************'
          pause
          GO TO 10
        endif
 3      read(1,*,end=10)cdate(n),(x(i),i=1,5),coeff(n),(m(i),i=1,2),
     %(x(i),i=6,7)
        c(n)=m(1)
        d(n)=m(2)
        ocoeff(n)=coeff(n)
        ma(n)=x(6)
        fa(n)=x(7)
        IF(coeff(n).GT.0.0) nb=nb+1
        go to 2

 5      format(i8,2f10.6,2i6,2f10.6)
 10   close(unit=1)

ccccccccccccccccc multiple day loop cccccccccccccccccccccccccc

        n=n-1
        WRITE(6,*)' ALBCOF1.sub Number of days = ',n
        open(unit=78,file=lasfil,status='old',ERR=11)
        read(78,*)
        read(78,*) date,lc
        close(unit=78)
        GO TO 12

 11     date=00000000
        lc=0.0

 12     k=0
        IF(nb.le.1) then
          lc=-9.0
        endif
        IF(coeff(1).lt.0.0) coeff(1)=lc
        do 14 i=1,n
         if(coeff(i).lt.0.0) then
           k=k+1
         ELSEIF(OK.and.i.eq.1) THEN
           write(2,5)cdate(i),coeff(i),ocoeff(i),c(i),d(i),ma(i),fa(i)
           lc=coeff(i)
           k=0
           OK=.FALSE.
         else
           dc=(coeff(i)-lc)/(k+1)
           j=i
           do 13 l=(i-k),(i)
             IF(ok) then
               coeff(l)=lc
               ok=.false.
             endif
             coeff(l)=coeff(l-1)+dc
             write(2,5)cdate(l),coeff(l),ocoeff(l),c(l),d(l),ma(l),fa(l)
 13        continue
           lc=coeff(i)
           k=0
         endif
 14     continue

        if(k.gt.0) then
          do 19 i=(n-(k-1)),n
            coeff(i)=lc
            write(2,5)cdate(i),coeff(i),ocoeff(i),c(i),d(i),ma(i),fa(i)
 19       continue
        endif
        close(unit=2)
        open(unit=78,file=lasfil,status='unknown')
        write(78,*)' ALBCOF1 Last day and coefficients:'
        IF(nb.eq.0) then
          write(78,5)-9999999,-9.0,-9.0,0,0
        else
          write(78,5)cdate(j),coeff(j),ocoeff(j),c(j),d(j),ma(j),fa(j)
        endif
        close(unit=78)
      go to 100
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 91   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'   <<<<<< Error opening coefficient file >>>>>>>'
      write(6,*)'The file ',coffil,' must be in the same directory'
      write(6,*)'as the executable.'
      write(6,*)'?????????????????????????????????????????????????????'
      go to 100

 100  return
      end
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine ALBCOF2(coffil,outfil)
      PARAMETER (NDAYMAX=12000)
      INTEGER n,cdate(0:NDAYMAX),c(0:NDAYMAX),date,i,k
      INTEGER l,j,d(0:NDAYMAX),nb
      REAL acoeff(0:NDAYMAX),dcoeff(0:NDAYMAX),x(10),ma(0:NDAYMAX)
      REAL bcoeff(0:NDAYMAX),ld,la,lb,da,db,devalb(0:NDAYMAX)
      real fa(0:NDAYMAX)
      CHARACTER*12 COFFIL,lasfil,outfil
      LOGICAL ok

      lasfil='alb_cof2.end'
      do n=0,NDAYMAX
        dcoeff(n)=0.0
        acoeff(n)=-9.0
        bcoeff(n)=-9.0
        devalb(n)=-9.0
        ma(n)=-9.0
        fa(n)=-9.0
        cdate(n)=0
        c(n)=0
        d(n)=0
      end do
      OK=.TRUE.
      nb=0

      open(unit=1,file=coffil,status='old',err=91)
      open(unit=2,file=outfil,status='unknown')
      read(1,*)
      n=0
 2      n=n+1
        IF(n.gt.NDAYMAX) then
          WRITE(6,*)'*********************************************'
          WRITE(6,*)'Number of days greater than limit of',NDAYMAX,'!'
          WRITE(6,*)' Processing up to ',cdate(n-1),' only!'
          WRITE(6,*)'    Click on OK to continue.'
          WRITE(6,*)'*********************************************'
          pause
          GO TO 10
        endif
 3      read(1,*,end=10)cdate(n),dcoeff(n),acoeff(n),bcoeff(n),devalb(n)
     %,c(n),d(n),(x(i),i=1,4),ma(n),fa(n)
        IF(dcoeff(n).gt.0.0) nb=nb+1
        go to 2

 5      format(i8,4f10.6,2i6,2f10.4)
 10   close(unit=1)

ccccccccccccccccc multiple day loop cccccccccccccccccccccccccc

        n=n-1
        WRITE(6,*)' ALBCOF2.sub Number of days = ',n
        open(unit=78,file=lasfil,status='old',ERR=11)
        read(78,*)
        read(78,*) date,ld,la,lb
        close(unit=78)
        GO TO 12

 11     date=00000000
        la=0.0
        lb=0.0

 12     k=0
        IF(nb.le.1) then
          la=-9.0
          lb=-9.0
        endif
        IF(acoeff(1).LT.-8.0) then
          acoeff(1)=la
          bcoeff(1)=lb
        endif
        do 14 i=1,n
         if(acoeff(i).LT.-8.0) then
           k=k+1
         ELSEIF(OK.and.i.eq.1) THEN
           write(2,5)cdate(i),dcoeff(i),acoeff(i),bcoeff(i),devalb(i),
     %c(i),d(i),ma(i),fa(i)
           la=acoeff(i)
           lb=bcoeff(i)
           k=0
           OK=.FALSE.
         else
           da=(acoeff(i)-la)/(k+1)
           db=(bcoeff(i)-lb)/(k+1)
           j=i
           do 13 l=(i-k),(i)
             IF(ok) then
               acoeff(l)=la
               bcoeff(l)=lb
               ok=.false.
             endif
             acoeff(l)=acoeff(l-1)+da
             bcoeff(l)=bcoeff(l-1)+db
             write(2,5)cdate(l),dcoeff(l),acoeff(l),bcoeff(l),devalb(l),
     %c(l),d(l),ma(l),fa(l)
 13        continue
           la=acoeff(i)
           lb=bcoeff(i)
           k=0
         endif
 14     continue

        if(k.gt.0) then
          do 19 i=(n-(k-1)),n
            acoeff(i)=la
            bcoeff(i)=lb
            write(2,5)cdate(i),dcoeff(i),acoeff(i),bcoeff(i),devalb(i),
     %c(i),d(i),ma(i),fa(i)
 19       continue
        endif
        close(unit=2)
        open(unit=78,file=lasfil,status='unknown')
        write(78,*)' ALBCOF1 Last day and coefficients:'
        IF(nb.eq.0) then
          write(78,5)-9999999,-9.0,-9.0,-9.0,-9.0,0,0
          write(6,5)-9999999,-9.0,-9.0,-9.0,-9.0,0,0
        else
          write(78,5)cdate(j),dcoeff(j),acoeff(j),bcoeff(j),devalb(j),
     %c(j),d(j),ma(j),fa(j)
          write(6,5)cdate(j),dcoeff(j),acoeff(j),bcoeff(j),devalb(j),
     %c(j),d(j),ma(j),fa(j)
        endif
        close(unit=78)
      go to 100
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 91   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'   <<<<<< Error opening coefficient file >>>>>>>'
      write(6,*)'The file ',coffil,' must be in the same directory'
      write(6,*)'as the executable.'
      write(6,*)'?????????????????????????????????????????????????????'
      go to 100

 100  return
      end
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
CCC       REGRESSION LINE COEFF SUB  (sumsq fit to Y=MX+B)
cc
cc    Chuck Long, March 4, 2006
cc
cc    This subroutine takes an array of X values (xary) and a corresponding
cc    array of Y values (yary), along with the number of values in the
cc    arrays (N), and fits a sum-of-squares regression linear fit to
cc    that data. Values returned are the multiplicative coefficient (M),
cc    the fit constant (B), and the standard deviation of the X,Y pairs
cc    from the fitted line (StDev).
cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine sumsqfit(xary,yary,n,m,b,stdev)

      INTEGER N,i
      REAL*8 SUMXY,SUMX,SUMY,SUMXSQ,SUMYSQ,M,B,VARY
      REAL*8 STDEV,SUMDSQ
      REAL*8 xary(5000),yary(5000)

      IF(n.gt.5000) then
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(6,*)'!!  In sumsqfit subroutine, N>max array of 5000   !!'
        WRITE(6,*)'!!   Setting n to 5000 for this fit...            !!'
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        pause
        n=5000
      endif

        SUMXY=0.0
        SUMX=0.0
        SUMY=0.0
        SUMXSQ=0.0
        SUMYSQ=0.0
        SUMDSQ=0.0


      do i=1,n
        X =xary(i)
        Y =yary(i)

        SUMXY=SUMXY+(X*Y)
        SUMX=SUMX+X
        SUMY=SUMY+Y
        SUMXSQ=SUMXSQ+(X**2)
        SUMYSQ=SUMYSQ+(Y**2)
        SUMDSQ=SUMDSQ+((Y-X)**2)
      end do

      M=((N*SUMXY)-(SUMX*SUMY))/((N*SUMXSQ)-(SUMX**2))
      B=(SUMY-(M*SUMX))/N
      VARY=((SUMYSQ-SUMY**2/N)-M*(SUMXY-SUMX*SUMY/N))/(N-2)
      STDEV=SQRT(VARY)

      return
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine CLRCOF1(dyfil,nlim)
      PARAMETER (NDAYMAX=12000)
      INTEGER n,cdate(0:NDAYMAX),c(0:NDAYMAX),date,i,k
      INTEGER l,nlim,pdate(0:NDAYMAX),j,o
      REAL dfrb(0:NDAYMAX),dfra(0:NDAYMAX),cswa(0:NDAYMAX)
      REAL sncz(0:NDAYMAX),cswb(0:NDAYMAX),x
      REAL*8 lda,ldb,lta,ltb,da,db,ta,tb,y
      CHARACTER*12 COFFIL,lasfil
      CHARACTER*1 ans
      logical dyfil,ok

      COFFIL='swclreq1.asc'
      lasfil='swlastdy.cfn'
      do n=0,NDAYMAX
        dfrb(n)=0.0
        dfra(n)=0.0
        cswb(n)=0.0
        cswa(n)=0.0
        sncz(n)=0.0
        cdate(n)=0
        pdate(n)=0
        c(n)=0
      end do

      open(unit=1,file=coffil,status='old',err=91)
      open(unit=2,file='clrcoef.asc',status='unknown')
      read(1,*)
      read(1,*)
      n=0
      j=1
cc KLG I added this initilizations
      k=0
      l=0
      if(dyfil) then
 6      n=n+1
        IF(n.gt.NDAYMAX) then
          WRITE(6,*)'*********************************************'
          WRITE(6,*)'Number of days greater than limit of',NDAYMAX,'!'
          WRITE(6,*)' Processing up to ',cdate(n-1),' only!'
          WRITE(6,*)'    Click on OK to continue.'
          WRITE(6,*)'*********************************************'
          pause
          GO TO 10
        endif
        read(1,*,end=10)cdate(n),dfra(n),dfrb(n),x,cswa(n),cswb(n),y,
     %sncz(n),c(n)
        if(dfrb(n).lt.-0.99d0.and.c(n).gt.nlim) then
           c(n)=-99
           pdate(n)=cdate(n)
           j=j+1
        endif
        go to 6
      else
        read(1,*)cdate(1),dfra(1),dfrb(1),x,cswa(1),cswb(1),y,sncz(1),
     %c(1)
        n=1
        write(2,3)cdate(1),dfra(1),dfrb(1),x,cswa(1),cswb(1),y,sncz(1),
     %c(1),1
        close(unit=2)
cc  KLG need to close 1 also
        close(unit=1)
        go to 100
      endif

 3      format(i8,f10.6,2f10.6,f10.2,2f10.6,f10.6,2i6)
 10   close(unit=1)

ccccccccccccccccc multiple day loop cccccccccccccccccccccccccc

      if(dyfil) then
        n=n-1
        i=1
        open(unit=78,file=lasfil,status='old',ERR=92)
        read(78,*)
        read(78,*) date,lda,ldb,lta,ltb
        WRITE(6,*)' '
        WRITE(6,*)' '
        write(6,*)'____________________________________________________'
        write(6,*)' Last day and coefficients:'
        write(6,21) ' ',date,lda,ldb,lta,ltb
        ok=.true.
        close(unit=78)
        do 14 i=1,n
         if(c(i).lt.nlim.and.cdate(i).ne.pdate(i)) then
           k=k+1
         elseif(cdate(i).eq.pdate(i)) then
           j=1
           o=i+1
 11        continue
           if(c(o).lt.nlim.and.o.lt.n) then
             o=o+1
             j=j+1
             go to 11
           elseif(o.eq.n) then
             dfrb(o)=ldb
           endif
           da=(dfra(i)-lda)/(k+1)
           db=(dfrb(o)-ldb)/(k+j+1)
           ta=(cswa(i)-lta)/(k+1)
           tb=(cswb(i)-ltb)/(k+1)
           if(ok) then
             dfra(i-k-1)=lda
             dfrb(i-k-1)=ldb
             cswa(i-k-1)=lta
             cswb(i-k-1)=ltb
             ok=.false.
           endif
           do 12 l=(i-k),(i)
             dfra(l)=dfra(l-1)+da
             dfrb(l)=dfrb(l-1)+db
             cswa(l)=cswa(l-1)+ta
             cswb(l)=cswb(l-1)+tb
        write(2,3)cdate(l),dfra(l),dfrb(l),x,cswa(l),cswb(l),y,sncz(l),
     %c(l),l
 12        continue
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           k=0
         elseif(k.gt.0) then
           da=(dfra(i)-lda)/(k+1)
           db=(dfrb(i)-ldb)/(k+1)
           ta=(cswa(i)-lta)/(k+1)
           tb=(cswb(i)-ltb)/(k+1)
           if(ok) then
             dfra(i-k-1)=lda
             dfrb(i-k-1)=ldb
             cswa(i-k-1)=lta
             cswb(i-k-1)=ltb
             ok=.false.
           endif
           do 13 l=(i-k),(i-1)
             dfra(l)=dfra(l-1)+da
             dfrb(l)=dfrb(l-1)+db
             cswa(l)=cswa(l-1)+ta
             cswb(l)=cswb(l-1)+tb
        write(2,3)cdate(l),dfra(l),dfrb(l),x,cswa(l),cswb(l),y,sncz(l),
     %c(l),l
 13        continue
        write(2,3)cdate(i),dfra(i),dfrb(i),x,cswa(i),cswb(i),y,sncz(i),
     %c(i),i
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           k=0
         else
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           ok=.false.
        write(2,3)cdate(i),dfra(i),dfrb(i),x,cswa(i),cswb(i),y,sncz(i),
     %c(i),i
         endif
 14     continue
        if(k.gt.0) then
          do 19 i=(n-(k-1)),n
           dfra(i)=lda
           dfrb(i)=ldb
           cswa(i)=lta
           cswb(i)=ltb
        write(2,3)cdate(i),dfra(i),dfrb(i),x,cswa(i),cswb(i),y,sncz(i),
     %c(i),i
 19       continue
 20     format(i8,2f10.6,f10.2,f10.6)
 21     format(a1,i8,2f10.6,f10.2,2f10.6)
        endif
        close(unit=2)
      endif
      go to 100
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 91   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'   <<<<<< Error opening coefficient file >>>>>>>'
      write(6,*)'The file "swclreq1.asc" must be in the same directory'
      write(6,*)'as the executable file.'
      write(6,*)'?????????????????????????????????????????????????????'
      go to 100

 92   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'<<<<<< Error opening last day coefficient file >>>>>>>'
      write(6,*)'The file "swlastdy.cfn" must be in the same directory'
      write(6,*)'as the executable file. This file contains the fit'
      write(6,*)'coefficients for the last "good" day of the previous'
      write(6,*)'run. '
      write(6,*)' '
      write(6,*)'NOTE: Without this file the current run results '
      write(6,*)'      are INVALID! End this run and try again.'
      write(6,*)' '
      write(6,*)'If this is the first run of the code for a site, do '
      write(6,*)'you want to have a generic "swlastdy.cfn" file'
      write(6,*)'generated for you so you can re-run the code? '
      write(6,*)'Answer y/n for yes or no: '
      write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'Answer: '
      READ(*,93) ans
 93   FORMAT(a1)
      IF(ans.eq.'y'.or.ans.eq.'Y') then
        OPEN(UNIT=59,FILE='swlastdy.cfn',STATUS='unknown')
        WRITE(59,*)'  Last day with retrieved coefficients:'
        WRITE(59,*)'20010101  0.126162 -0.737073   1086.80  1.190142'
        CLOSE(UNIT=59)
      endif

      write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'Do you want to have a generic "swlastdy.cfw" file,'
      write(6,*)'which is also needed later in the processing,'
      write(6,*)'also generated for you so the code will run? '
      write(6,*)'Answer y/n for yes or no: '
      write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'Answer: '

      READ(*,93) ans
      IF(ans.eq.'y'.or.ans.eq.'Y') then
        OPEN(UNIT=59,FILE='swlastdy.cfw',STATUS='unknown')
        WRITE(59,*)'  Last day with n>nlim retrieved coeffcients:
     % 702'
        WRITE(59,*)'20010101  0.127 -0.734   1084.5  1.187   1083.2  1.1
     %89   -0.315    1.99   -0.192    3.6    -6.0    -6.0'
        CLOSE(UNIT=59)
      endif

cc      CLOSE(UNIT=77)
      CLOSE(UNIT=2)
      go to 100

 100  return
      end
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine SWCLRID2(dirfil,maxtim,delnum,ndstlm,delSlm,tswlim,
     %maxdif,debug,delfac,nswmx,nswmn)
      INTEGER HR,imin,DATE,i,n,t,cfdate,ldate,e
      INTEGER stdate(60),sthr(60),stmin(60),j,maxtim,m,delnum,u,v,y(100)
      REAL*8 TSW,DSW,COSZ,sncz,sncz1,avndfr,avnsw,x(100),diflim,dtplus
      REAL*8 DIR,nsw(60),difrat(60),dr(60),dif(60),cz(60),maxdif,dtminus
      REAL*8 sw(60),delS,rsum,avg,aau(60),tswlim,ndstlm,au,delfac,nswmx
      REAL*8 ndifr(60),TOA(60),delT,lim,da,db,ds,sa,sb,ss,delSlm,nswmn
      CHARACTER*12 DIRFIL,cnffil,dayfil,cosfil,dir2,coffil,outfil,dbgfil
      logical go,cfok,debug,datlog

      cfok=.true.
      datlog=.true.
      do i=1,60
        stdate(i)=0
        sthr(i)=0
        stmin(i)=0
        nsw(i)=0.0
        difrat(i)=0.0
        dr(i)=0.0
        dif(i)=0.0
        cz(i)=0.0
        sw(i)=0.0
        aau(i)=0.0
        ndifr(i)=0.0
        toa(i)=0.0
      end do

      dayfil='swclrid2.day'
      coffil='clrcoef.asc'
      dir2='swclrid2.dir'
      dbgfil='swclrid2.clr'

      OPEN(UNIT=1,FILE=DIRFIL,STATUS='OLD')
      OPEN(UNIT=77,FILE=COFFIL,STATUS='OLD')
      OPEN(UNIT=89,FILE=dir2,STATUS='UNKNOWN')
 5    format(a114)
      OPEN(UNIT=8,FILE=dayfil,STATUS='UNKNOWN')
      write(8,*)' Date        AvNDR     AvNSW      sncz     N  TotN    D
     %ifAlb    dN   MornAlb    AftAlb  MrnN  AftN'
      m=maxtim/2+1
      i=0
      avg=0.0
      rsum=0.0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc     dummy=0
      IF(debug) then
        OPEN(UNIT=111,FILE='clrid2out.asc',STATUS='unknown')
        WRITE(111,*)'    date hr mn       cz      aau      sw     nsw
     % sa-      sa+     dif  diflim     dir    ndifr   difrat     toa
     % delt     lim    dels   delT-   delT+  delfac   sncz1 sncz1/cz   n
     %drstd   ndstlm'
        OPEN(UNIT=112,FILE=dbgfil,STATUS='unknown')
      WRITE(112,5)'   Date  Hr Mn    CosZ     AAU      TotSW   NSW
     %Dif    Dir    NDfr    DifRat   TOA      delT    delS    StDDR'
      endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 10   READ(1,15,END=100) cnfFIL
        READ(cnffil,11) ldate
 11     FORMAT(i8)
 15   FORMAT(A12)
        e=index(cnffil,'.')
        cosfil=cnffil(1:e)//'cos'
 16     FORMAT(A60,5(6x,a5))
        OUTFIL=cosFIL(1:e)//'clr'
	open(unit=3,file=outfil,status='unknown')
      WRITE(3,5)'   Date  Hr Mn    CosZ     AAU      TotSW   NSW      Di
     %f    Dir    NDfr    DifRat   TOA      delT    delS    StDDR'
        WRITE(6,*)'===================================================='
	WRITE(6,*)'  PROCESSING DAY: ',OUTFIL
      OPEN(UNIT=2,FILE=cosFIL,STATUS='OLD')
        avndfr=0.d0
	avnsw=0.d0
	n=0
	t=0
        read(2,*,end=50)

      if(cfok) read(77,*) cfdate,da,db,ds,sa,sb,ss
      if(cfdate.eq.0) cfok=.false.
 22   read(2,*,end=50,err=30) (y(u),u=1,4),(x(v),v=1,6)
        date=y(3)
      IF(datlog) then
        ldate=date
        datlog=.false.
      endif
      IF(debug.and.date.ne.ldate) then
        WRITE(6,*)'SWCLRID2 Problem with file dates: ',cosfil
        pause
      endif
      if(cfdate.eq.0) then
        cfok=.false.
        go to 25
      endif
      if(cfdate.gt.date) then
        write(6,*)'SWCLRID2   cfdate > date ',cfdate,date
        go to 22
      endif
 23   if(cfdate.lt.date) then
        write(6,*)'SWCLRID2   cfdate < date ',cfdate,date
        read(77,*,end=100) cfdate,da,db,ds,sa,sb,ss
        IF(debug) pause
        go to 23
      endif

 25   continue
        hr=y(4)/100
        imin=y(4)-hr*100
      sncz=x(2)
      tsw=x(4)
      dsw=x(5)
      dir=x(6)
      cosz=x(1)
      au=x(3)

        sncz1=sncz+0.1d0
        if(sncz1.gt.1.d0) sncz1=1.d0
      if(cosz.lt.0.d0.or.tsw.lt.tswlim) go to 22

      do 29 i=1,maxtim
	toa(i)=0.d0
	nsw(i)=0.d0
	difrat(i)=0.d0
	dr(i)=0.d0
	ndifr(i)=0.d0
	dif(i)=0.d0
	cz(i)=1.d0
	aau(i)=0.d0
	sw(i)=0.d0
	stdate(i)=0
	sthr(i)=0
        stmin(i)=0
 29   continue
      i=1
      if(cosz.gt.0.02.and.dsw.gt.1.d0.and.tsw.gt.1.d0) then
	cz(i)=cosz
	toa(i)=1365.d0*cosz
        nsw(i)=tsw/(cosz**sb)
cc      use sum SW for difrat if avail, else tsw
        IF(dir.gt.1.0) then
	  difrat(i)=dsw/(dsw+dir)
        else
	  difrat(i)=dsw/tsw
        endif
	dr(i)=dir
	ndifr(i)=(difrat(i))*cosz**0.8d0
	dif(i)=dsw
	sw(i)=tsw
	aau(i)=au
	stdate(i)=date
	sthr(i)=hr
	stmin(i)=imin
	i=i+1
        t=t+1
      endif

 30   read(2,*,end=50,err=30) (y(u),u=1,4),(x(v),v=1,6)
        date=y(3)
        hr=y(4)/100
        imin=y(4)-hr*100
      sncz=x(2)
      tsw=x(4)
      dsw=x(5)
      dir=x(6)
      cosz=x(1)
      au=x(3)
      if(cosz.lt.0.d0.or.tsw.lt.tswlim) go to 30
        sncz1=sncz+0.1d0
        if(sncz1.gt.1.d0) sncz1=1.d0

      if(cosz.gt.0.02.and.dsw.gt.1.0.and.tsw.gt.1.0) then
	cz(i)=cosz
	toa(i)=1365.d0*cosz
        nsw(i)=tsw/(cz(i)**sb)
cc      use sum SW for difrat if avail, else tsw
        IF(dir.gt.1.0) then
	  difrat(i)=dsw/(dsw+dir)
        else
	  difrat(i)=dsw/tsw
        endif
	dr(i)=dir
	ndifr(i)=(difrat(i))*cosz**0.8d0
	dif(i)=dsw
	sw(i)=tsw
	aau(i)=au
	stdate(i)=date
	sthr(i)=hr
	stmin(i)=imin
	i=i+1
	t=t+1
      endif
      if(i.eq.(maxtim+1)) then
	delT=dabs(toa(m+1)-toa(m-1))
	delS=dabs(sw(m+1)-sw(m-1))
        lim=20.d0*sncz*delfac
        dtplus= (delT+delfac*delSlm*cz(m))
        IF(dtplus.lt.1.1) dtplus=1.1
        dtminus= (delT-delfac*sncz1/cz(m))
        IF(dtminus.LT.-0.1) dtminus=-0.1
cc        if(cz(m).gt.0.02.and.nsw(m).gt.(sa-150.d0).and.nsw(m).lt.
cc     %(sa+150.d0))then
cc      dummy=0
        if(cz(m).gt.0.02.and.nsw(m).gt.(sa-nswmn).and.nsw(m).lt.
     %(sa+nswmx))then
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          go=.true.
        else
          go=.false.
        endif
        diflim=maxdif*(cz(m)**0.5)
        if(delT.lt.lim.and.go.and.dif(m).lt.diflim) then
         if(dels.lt.dtplus.and.dels.gt.dtminus) then
	  avg=0.d0
          rsum=0.d0
          do 40 j=1,maxtim
	    avg=ndifr(j)+avg
 40       continue
	  avg=avg/(maxtim*1.d0)
	  do 41 j=1,maxtim
	    rsum=(ndifr(j)-avg)**2+rsum
 41       continue
	  rsum=sqrt(rsum/(maxtim*1.d0))
          if(rsum.lt.ndstlm)then
	 write(3,42)stdate(m),sthr(m),stmin(m),cz(m),aau(m),sw(m),
     %nsw(m),dif(m),dr(m),ndifr(m),difrat(m),toa(m),delt,dels,rsum
 42      format(i8,2i3,2f9.5,4f8.1,2f9.5,f8.2,2f8.3,f9.5)
            IF(debug) then
	 write(112,42)stdate(m),sthr(m),stmin(m),cz(m),aau(m),sw(m),
     %nsw(m),dif(m),dr(m),ndifr(m),difrat(m),toa(m),delt,dels,rsum
            endif
	    avndfr=avndfr+ndifr(m)
	    avnsw=avnsw+nsw(m)
            n=n+1
          endif
         endif
        endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc      dummy=0
        IF(debug.and.go) then
	 write(111,142) stdate(m),sthr(m),stmin(m),cz(m),aau(m),sw(m),
     %nsw(m),(sa-nswmn),(sa+nswmx),dif(m),diflim,dr(m),ndifr(m),
     %difrat(m),toa(m),delt,lim,dels,dtminus,
     %dtplus,delfac,sncz1,sncz1/cz(m),rsum,ndstlm
 142     format(i8,2i3,2f9.5,7f8.1,2f9.5,f8.2,7f8.3,4f9.5)
        endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	do 43 j=1,(maxtim-1)
	  toa(j)=toa(j+1)
	  nsw(j)=nsw(j+1)
	  difrat(j)=difrat(j+1)
	  dr(j)=dr(j+1)
	  ndifr(j)=ndifr(j+1)
	  dif(j)=dif(j+1)
	  cz(j)=cz(j+1)
	  sw(j)=sw(j+1)
	  aau(j)=aau(j+1)
	  stdate(j)=stdate(j+1)
	  sthr(j)=sthr(j+1)
	  stmin(j)=stmin(j+1)
 43     continue
	i=maxtim
	avg=-9.d0
        rsum=-9.d0
      endif
      go to 30

 50   CONTINUE
      CLOSE(UNIT=2)
      datlog=.true.
	 if(n.gt.0) then
	   avndfr=avndfr/(n*1.d0)
	   avnsw=avnsw/(n*1.d0)
	 else
	   avndfr=-1.d0
	   avnsw=-1.d0
	 endif
         write(8,51)ldate,avndfr,avnsw,sncz,n,t
 51      format(i8,f10.6,f10.1,f10.6,2i6)
         if(n.ge.delnum) write(89,54) outfil
 54      format(a12)
         close(unit=3)
         close(unit=99)
      GO TO 10

 100  CONTINUE
      CLOSE(UNIT=89)
      CLOSE(UNIT=77)
      CLOSE(UNIT=1)
      CLOSE(UNIT=8)
      IF(debug) CLOSE(UNIT=111)
      IF(debug) CLOSE(UNIT=112)
      return
      END
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine swclreq2(dyfil,mlim)
      PARAMETER (NMAX=199999)
      INTEGER N,totn,i,j,date,hr,imin,mlim,clrdyn
      REAL*8 cosz,au,tsw,nsw,dif,dir,ndir,difrat,toa,delt,dels,rsum
      REAL*8 dfrb,dfra,cswa,cswb,devcsw,devdfr,avndr,sncz,maxcz,mincz
      REAL*8 atsw(NMAX),acz(NMAX),adfr(NMAX),avnsw,rerr
      character*12 inpfil,infil,outfil,dirfil
      character*8 inpdat
      logical ok,dyfil,noclr

      inpfil='swclrid2.day'
      outfil='swclreq2.asc'
      dirfil='swclrid2.dir'
      ok=.true.
      noclr=.true.
      rerr=-9.0d0
      i=0
      clrdyn=0

      open(unit=1,file=inpfil,status='old')
      open(unit=89,file=dirfil,status='old')
      open(unit=3,file=outfil,status='unknown')
      open(unit=90,file='czfil.asc',status='unknown',access='append')
      write(90,*)'    date     mincz    maxcz     sncz      n'

      write(3,*)'  for: clrDfR OR clrTSW = a*(CosZ)**b    >>>swclreq2'
      write(3,2)'   date      dfra     dfrb     devdfr     cswa     cswb
     %      devcsw    sncz      N'
      WRITE(6,*)' '
      WRITE(6,*)' '
      write(6,*)'  for: clrDfR OR clrTSW = a*(CosZ)**b    >>>swclreq2'
      write(6,2)'   date      dfra    dfrb    devdfr    cswa    cswb
     %devcsw   sncz    N'
 2    format(a81)
 3    format(a12)
      read(1,*)

 4    read(1,5,end=50,err=100)inpdat,avndr,avnsw,sncz,n,totn
 5    format(a8,f10.6,f10.1,f10.6,2i6)
      if(n.ge.mlim.and.dyfil) then
        read(89,3) infil
        noclr=.false.
        clrdyn=clrdyn+1
        i=0
      ELSEIF(.not.dyfil.and.n.ge.mlim) then
        read(89,3) infil
        noclr=.false.
      else
        if(dyfil)write(3,71) inpdat,rerr,rerr,rerr,rerr,rerr,rerr,sncz,n
        go to 4
      endif
      maxcz=0.d0
      mincz=1.d0

      if(ok) then
      do 6 j=1,NMAX
	 atsw(J)=0.d0
         acz(J)=0.d0
         adfr(j)=0.d0
 6    continue
      ok=.false.
      endif

      open(unit=2,file=infil,status='old')
      read(2,*)
 9    read(2,10,end=20)date,hr,imin,cosz,au,tsw,nsw,dif,dir,ndir,
     %difrat,toa,delt,dels,rsum
 10   format(i8,2i3,2f9.5,4f8.1,2f9.5,f8.2,2f8.3,f9.5)
      if(cosz.gt.0.02d0.and.difrat.gt.0.d0.and.tsw.gt.0.d0) then
        i=i+1
        IF(i.gt.NMAX) then
          WRITE(6,*)'**************************************************'
          WRITE(6,*)'Number of clear points greater than',NMAX,' limit'
          WRITE(6,*)'          in swclreq2 subroutine.'
          WRITE(6,*)' Processing up to ',date
          WRITE(6,*) hr,imin,' only!'
          WRITE(6,*)'    Click on OK to continue.'
          WRITE(6,*)'**************************************************'
          pause
          i=NMAX
          GO TO 50
        endif
        IF(dyfil) then
          atsw(i)=log10(tsw)
        ELSEIF(au.gt.0.0) then
          atsw(i)=LOG10(tsw*(au**2))
        else
          atsw(i)=log10(tsw)
        endif
	acz(i)=log10(cosz)
        adfr(i)=log10(difrat)
        if(cosz.gt.maxcz) maxcz=cosz
        if(cosz.lt.mincz) mincz=cosz
      endif
      go to 9

 20   continue
      close(unit=2)
      if(.not.dyfil) go to 4

 50   continue
      if(noclr.and.dyfil)then
        if(clrdyn.gt.0) noclr=.false.
        go to 100
      endif
      if(noclr) go to 100
      if(dyfil.and.i.lt.mlim) then
        write(3,71)inpdat,rerr,rerr,rerr,rerr,rerr,rerr,sncz,i
        write(6,72)' ',inpdat,rerr,rerr,rerr,rerr,rerr,rerr,sncz,i
        go to 80
      endif
      call medfit(acz,adfr,i,dfra,dfrb,devdfr)
      dfra=10.d0**dfra

      call medfit(acz,atsw,i,cswa,cswb,devcsw)
      cswa=10.d0**cswa

cccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    dummy=0
      IF(dyfil) then
        IF(dfra.gt.1.0.or.dfra.lt.0.0) dfra=-9.0
        IF(dfrb.gt.0.0.or.dfrb.lt.-2.0) dfrb=-9.0
        IF(cswa.gt.2000.0.or.cswa.lt.0.0) cswa=-9.0
        IF(cswb.gt.2.0.or.cswb.lt.0.9) cswb=-9.0
      endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if(.not.dyfil) inpdat='000000'
      if(.not.dyfil) sncz=-9.00000
      if(dyfil.and.maxcz.lt.(sncz*0.8d0)) then
        cswa=rerr
        cswb=rerr
        devcsw=rerr
        i=-44
      endif
      if(dyfil.and.mincz.gt.0.4d0) then
        dfrb=rerr
        devdfr=rerr
        IF(i.EQ.-44) then
          i=-11
        else
          i=-55
        endif
      endif

ccccccccccccc  limit changed due to "bug" discovered in Oregon Burns data  ccccc
cc      if(dyfil.and.dfrb.gt.-0.4) then
      if(dyfil.and.dfrb.gt.-0.25) then
        dfra=rerr
        dfrb=rerr
        devdfr=rerr
        IF(i.EQ.-44) then
          i=-11
        else
          i=-88
        endif
      endif
      if(dyfil.and.dfrb.lt.-1.00) then
        dfrb=rerr
        devdfr=rerr
        IF(i.EQ.-44.or.i.EQ.-11) then
          i=-11
        ELSEIF(i.EQ.-88) then
          i=-88
        else
          i=-99
        endif
      endif
      if(dyfil.and.i.EQ.-11) then
        write(3,71)inpdat,rerr,rerr,rerr,rerr,rerr,rerr,sncz,i
        write(6,72)' ',inpdat,rerr,rerr,rerr,rerr,rerr,rerr,sncz,i
        go to 80
      else
        IF(.not.dyfil.and.dfrb.GT.-0.25) dfrb=-0.25
 70     write(3,71)inpdat,dfra,dfrb,devdfr,cswa,cswb,devcsw,sncz,i
        write(6,72)' ',inpdat,dfra,dfrb,devdfr,cswa,cswb,devcsw,sncz,i
 71     format(a8,f10.6,2f10.6,f8.1,2f10.6,f10.6,i10)
 72     format(a1,a8,3f9.5,f8.1,3f9.5,i5)
      endif
 80   if(dyfil) then
        write(90,81) inpdat,mincz,maxcz,sncz,i
 81     format(a8,3f10.6,i6)
        noclr=.true.
        ok=.true.
        i=0
        go to 4
      else
        go to 100
      endif

 100  continue
      close(unit=1)
      close(unit=3)
      close(unit=89)
      close(unit=90)
      close(unit=2)
      RETURN
      end
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine CLRCOF2(dyfil,nlim,lsdate,lsdfra,lsdfrb,lscswa,lscswb)
      PARAMETER (NDAYMAX=12000)
      INTEGER n,cdate(0:NDAYMAX),c(0:NDAYMAX),date,i,k,z,lsdate
      INTEGER l,nlim,pdate(0:NDAYMAX),j,o,ddate(0:NDAYMAX)
      INTEGER edate(0:NDAYMAX)
      REAL dfrb(0:NDAYMAX),dfra(0:NDAYMAX),cswa(0:NDAYMAX)
      REAL sncz(0:NDAYMAX),cswb(0:NDAYMAX),x
      REAL*8 lda,ldb,lta,ltb,da,db,ta,tb,y,lsdfra,lsdfrb,lscswa,lscswb
      CHARACTER*12 COFFIL,lasfil
      logical dyfil,ok

      COFFIL='swclreq2.asc'
      lasfil='swlastdy.cfn'
      do n=0,NDAYMAX
        c(n)=0
        cdate(n)=0
        ddate(n)=0
        edate(n)=0
        pdate(n)=0
        dfrb(n)=0.0
        dfra(n)=0.0
        cswb(n)=0.0
        cswa(n)=0.0
        sncz(n)=0.0
      end do

      open(unit=1,file=coffil,status='old',err=91)
      open(unit=2,file='clrcoef.asc',status='unknown')
      read(1,*)
      read(1,*)
      n=0
      k=0
      j=1
      z=0
      if(dyfil) then
 2      n=n+1
        IF(n.gt.NDAYMAX) GO TO 10
        read(1,*,end=10)cdate(n),dfra(n),dfrb(n),x,cswa(n),cswb(n),y,
     %sncz(n),c(n)
        if(c(n).eq.-99) then
           pdate(n)=cdate(n)
        elseif(c(n).eq.-55) then
           pdate(n)=cdate(n)
        elseif(c(n).eq.-88) then
           edate(n)=cdate(n)
        elseif(c(n).eq.(-44)) then
          ddate(n)=cdate(n)
        else
          IF(c(n).ge.nlim) z=n
        endif
        go to 2
      else
        read(1,*)cdate(1),dfra(1),dfrb(1),x,cswa(1),cswb(1),y,sncz(1),
     %c(1)
        n=1
        z=1
        write(2,3)cdate(1),dfra(1),dfrb(1),x,cswa(1),cswb(1),y,sncz(1),
     %c(1),1
        close(unit=2)
        go to 100
      endif

 3      format(i8,f10.6,2f10.6,f10.2,2f10.6,f10.6,2i6)
 4      format(a1,i8,3f9.5,f10.2,3f9.5,2i6)
 10   close(unit=1)

ccccccccccccccccc multiple day loop cccccccccccccccccccccccccc

      if(dyfil) then
        n=n-1
        i=1
        open(unit=78,file=lasfil,status='old')
        read(78,*)
        read(78,*) date,lda,ldb,lta,ltb
        WRITE(6,*)' '
        WRITE(6,*)' '
        write(6,*)'____________________________________________________'
        write(6,*)' Last day and coefficients:'
        write(6,21) ' ',date,lda,ldb,lta,ltb
        ok=.true.
        close(unit=78)
        do 18 i=1,n
         if(c(i).lt.nlim.and.cdate(i).ne.pdate(i).and.cdate(i).ne.
     %ddate(i).and.cdate(i).ne.edate(i)) then
           k=k+1
         elseif(cdate(i).eq.pdate(i)) then
           j=1
           o=i+1
 11        continue
           if(c(o).lt.nlim.and.o.lt.n) then
             o=o+1
             j=j+1
             go to 11
           elseif(c(o).lt.nlim.and.o.eq.n) then
             dfrb(o)=ldb
           endif
           da=(dfra(i)-lda)/(k+1)
           db=(dfrb(o)-ldb)/(k+j+1)
           ta=(cswa(i)-lta)/(k+1)
           tb=(cswb(i)-ltb)/(k+1)
           if(ok) then
             dfra(i-k-1)=lda
             dfrb(i-k-1)=ldb
             cswa(i-k-1)=lta
             cswb(i-k-1)=ltb
             ok=.false.
           endif
           do 12 l=(i-k),(i)
             dfra(l)=dfra(l-1)+da
             dfrb(l)=dfrb(l-1)+db
             cswa(l)=cswa(l-1)+ta
             cswb(l)=cswb(l-1)+tb
        write(2,3)cdate(l),dfra(l),dfrb(l),x,cswa(l),cswb(l),y,sncz(l),
     %c(l),l
 12        continue
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           k=0
         elseif(cdate(i).eq.edate(i)) then
           j=1
           o=i+1
 13        continue
           if(c(o).lt.nlim.and.o.lt.n) then
             o=o+1
             j=j+1
             go to 13
           elseif(o.eq.n) then
             dfrb(o)=ldb
             dfra(o)=lda
           endif
           da=(dfra(o)-lda)/(k+j+1)
           db=(dfrb(o)-ldb)/(k+j+1)
           ta=(cswa(i)-lta)/(k+1)
           tb=(cswb(i)-ltb)/(k+1)
           if(ok) then
             dfra(i-k-1)=lda
             dfrb(i-k-1)=ldb
             cswa(i-k-1)=lta
             cswb(i-k-1)=ltb
             ok=.false.
           endif
           do 14 l=(i-k),(i)
             dfra(l)=dfra(l-1)+da
             dfrb(l)=dfrb(l-1)+db
             cswa(l)=cswa(l-1)+ta
             cswb(l)=cswb(l-1)+tb
        write(2,3)cdate(l),dfra(l),dfrb(l),x,cswa(l),cswb(l),y,sncz(l),
     %c(l),l
 14        continue
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           k=0
         elseif(cdate(i).eq.ddate(i)) then
           j=1
           o=i+1
 15        continue
           if(c(o).lt.nlim.and.o.lt.n) then
             o=o+1
             j=j+1
             go to 15
           elseif(c(o).lt.nlim.and.o.eq.n) then
             cswa(o)=lta
             cswb(o)=ltb
           endif
           da=(dfra(i)-lda)/(k+1)
           db=(dfrb(i)-ldb)/(k+1)
           ta=(cswa(o)-lta)/(k+j+1)
           tb=(cswb(o)-ltb)/(k+j+1)
           if(ok) then
             dfra(i-k-1)=lda
             dfrb(i-k-1)=ldb
             cswa(i-k-1)=lta
             cswb(i-k-1)=ltb
             ok=.false.
           endif
           do 16 l=(i-k),(i)
             dfra(l)=dfra(l-1)+da
             dfrb(l)=dfrb(l-1)+db
             cswa(l)=cswa(l-1)+ta
             cswb(l)=cswb(l-1)+tb
        write(2,3)cdate(l),dfra(l),dfrb(l),x,cswa(l),cswb(l),y,sncz(l),
     %c(l),l
 16        continue
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           k=0
         elseif(k.gt.0) then
           da=(dfra(i)-lda)/(k+1)
           db=(dfrb(i)-ldb)/(k+1)
           ta=(cswa(i)-lta)/(k+1)
           tb=(cswb(i)-ltb)/(k+1)
           if(ok) then
             dfra(i-k-1)=lda
             dfrb(i-k-1)=ldb
             cswa(i-k-1)=lta
             cswb(i-k-1)=ltb
             ok=.false.
           endif
           do 17 l=(i-k),(i-1)
             dfra(l)=dfra(l-1)+da
             dfrb(l)=dfrb(l-1)+db
             cswa(l)=cswa(l-1)+ta
             cswb(l)=cswb(l-1)+tb
        write(2,3)cdate(l),dfra(l),dfrb(l),x,cswa(l),cswb(l),y,sncz(l),
     %c(l),l
 17        continue
        write(2,3)cdate(i),dfra(i),dfrb(i),x,cswa(i),cswb(i),y,sncz(i),
     %c(i),i
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           k=0
         else
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           ok=.false.
        write(2,3)cdate(i),dfra(i),dfrb(i),x,cswa(i),cswb(i),y,sncz(i),
     %c(i),i
         endif
 18     continue
        if(k.gt.0) then
          do 19 i=(n-(k-1)),n
           dfra(i)=lda
           dfrb(i)=ldb
           cswa(i)=lta
           cswb(i)=ltb
        write(2,3)cdate(i),dfra(i),dfrb(i),x,cswa(i),cswb(i),y,sncz(i),
     %c(i),i
 19       continue
 20     format(i8,2f10.6,f10.2,f10.6)
 21     format(a1,i8,2f10.6,f10.2,f10.6)
        endif
        close(unit=2)
      endif
      go to 100

 91   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'   <<<<<< Error opening coefficient file.>>>>>>>'
      write(6,*)'The file "swclreq2.asc" must be in the same directory'
      write(6,*)'as the executable file "clrcof2.exe".'
      write(6,*)'?????????????????????????????????????????????????????'
      go to 100

 100  CLOSE(UNIT=1)
      lsdfra=dfra(z)
      lsdfrb=dfrb(z)
      lscswa=cswa(z)
      lscswb=cswb(z)
      lsdate=cdate(z)
      return
      end
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cc      subroutine SWCLRID3(dirfil,clrtim,delnum,ndstlm,delSlm,tswlim,
cc     %maxtim,cldflg,maxdif,sflg,delfac)
      subroutine SWCLRID3(dirfil,clrtim,delnum,ndstlm,delSlm,tswlim,
     %maxtim,cldflg,maxdif,sflg,delfac,nswmx,nswmn)
      INTEGER HR,imin,DATE,i,n,t,clr,reasn1,reasn2,reasn3,reasn4,e
      INTEGER stdate(60),sthr(60),stmin(60),j,maxtim,m,delnum,u,v,y(100)
      integer clrtim,cn,cfdate,cldnum,clrnum,cl2num,ovcnum,aun,sflg
      REAL*8 TSW,DSW,COSZ,avndfr,avnsw,x(100),sncz1,delT,delS,swu(60)
      REAL*8 DIR,nsw(60),difrat(60),dr(60),dif(60),cz(60),csw(60),w,avau
      REAL*8 sw(60),avg3,rsum,avg,aau(60),tswlim,cdr(60),lim,usw
      REAL*8 ndifr(60),TOA(60),avg2,sum2,sum3,da,db,ds,sa,sb,ss,drr(60)
      REAL*8 avg4,sum4,czmin,czmax,ndstlm,delSlm,sumsw,ssw(60),au,maxdif
      REAL*8 diflim,delfac,nswmx,nswmn
      CHARACTER*12 DIRFIL,dayfil,delfil,cosfil,dir2,coffil,out1,infil
      CHARACTER*12 out2,out4
      logical go,cfok,cldflg,nodyfl

      cfok=.true.
      nodyfl=.false.

      dayfil='swclrid3.day'
      delfil='swnulclr.bat'
      coffil='clrcoef.asc'
      dir2='swclrid3.dir'

      OPEN(UNIT=1,FILE=DIRFIL,STATUS='OLD')
      OPEN(UNIT=77,FILE=COFFIL,STATUS='OLD')
      OPEN(UNIT=88,FILE=delfil,STATUS='UNKNOWN')
      OPEN(UNIT=89,FILE=dir2,STATUS='UNKNOWN')
 5    format(a218)
      OPEN(UNIT=8,FILE=dayfil,STATUS='UNKNOWN')
      write(8,6)'   Date     AvNDR      AvNSW    sncz      N   TotN #clr
     %2  #clr #pcldy  #ovc   czmin    czmax   AvgAU'
 6    format(a100)
      m=maxtim/2
      cn=clrtim/2
      n=0
      i=0
cc KLG I added the initializations below
      j=0
      avg=0.0
      avg3=0.0
      avg4=0.0
      w=0.0
      sum3=0.0
      sum4=0.0


 10   READ(1,15,END=100) inFIL
        READ(infil,11) date
 11     FORMAT(i8)
 15   FORMAT(A12)
        n=index(infil,'.')
        cosfil=infil(1:n)//'cos'
 16     FORMAT(A60,5(6x,a5))
	out1=cosFIL(1:n)//'tst'
	out2=cosFIL(1:n)//'cl2'
	out4=cosFIL(1:n)//'clr'
	if(cldflg) open(unit=3,file=out1,status='unknown')
	open(unit=4,file=out2,status='unknown')
	open(unit=9,file=out4,status='unknown')
      IF(cldflg) then
      WRITE(3,17)'   Date  Hr Mn    CosZ     AAU     TotSW   NSW     CSW
     %      dif     Dir      NDfr     DifRat      CDr       Avg1     Std
     %1      Avg2      Std2     Avg3       Std3     Avg4       Std4  Cld
     % r1  r2  r3  r4    dT     dS    Max    MIN'
      endif
 17   format(a228)
      IF(sflg.eq.2) then
      WRITE(4,5)'   Date  Hr Mn    CosZ     AAU     TotSW   NSW     CSW
     %     dif     Dir      NDfr     DifRat      CDr       Avg1     Std1
     %      Avg2      Std2     Avg3       Std3     Avg4      Std4  Clf
     %  reason flgs     ssw     SolX'
      WRITE(9,5)'   Date  Hr Mn    CosZ     AAU     TotSW   NSW     CSW
     %     dif     Dir      NDfr     DifRat      CDr       Avg1     Std1
     %      Avg2      Std2     Avg3       Std3     Avg4      Std4  Clf
     %  reason flgs     ssw     SolX'
      elseIF(sflg.eq.3) then
      WRITE(4,5)'   Date  Hr Mn    CosZ     AAU     TotSW   NSW     CSW
     %     dif     Dir      NDfr     DifRat      CDr       Avg1     Std1
     %      Avg2      Std2     Avg3       Std3     Avg4      Std4  Clf
     %  reason flgs     ssw      Aux'
      WRITE(9,5)'   Date  Hr Mn    CosZ     AAU     TotSW   NSW     CSW
     %     dif     Dir      NDfr     DifRat      CDr       Avg1     Std1
     %      Avg2      Std2     Avg3       Std3     Avg4      Std4  Clf
     %  reason flgs     ssw      Aux'
      else
      WRITE(4,5)'   Date  Hr Mn    CosZ     AAU     TotSW   NSW     CSW
     %     dif     Dir      NDfr     DifRat      CDr       Avg1     Std1
     %      Avg2      Std2     Avg3       Std3     Avg4      Std4  Clf
     %  reason flgs     ssw      swu'
      WRITE(9,5)'   Date  Hr Mn    CosZ     AAU     TotSW   NSW     CSW
     %     dif     Dir      NDfr     DifRat      CDr       Avg1     Std1
     %      Avg2      Std2     Avg3       Std3     Avg4      Std4  Clf
     %  reason flgs     ssw      swu'
      endif
	WRITE(6,*)'===================================================='
	WRITE(6,*)'  PROCESSING DAY: ',out2
	avndfr=0.d0
	avnsw=0.d0
	n=0
	t=0
	cldnum=0
	clrnum=0
	cl2num=0
        ovcnum=0
        czmin=1.d0
        czmax=0.d0
        avau=0.0
        aun=0
      OPEN(UNIT=2,FILE=cosFIL,STATUS='OLD')
        read(2,*,end=60)

      if(cfok) read(77,*,end=100) cfdate,da,db,ds,sa,sb,ss
      if(cfdate.eq.0) cfok=.false.
      if(cfdate.eq.0) nodyfl=.true.
 22   read(2,*,end=60,err=30) (y(u),u=1,4),(x(v),v=1,7)
	date=y(3)
      if(cfdate.eq.0) then
        cfok=.false.
        nodyfl=.true.
        go to 25
      endif
      if(cfdate.gt.date) then
	write(6,*)'SWCLRID3  cfdate > date ',cfdate,date
	go to 22
      endif
 23   if(cfdate.lt.date) then
	read(77,*,end=100) cfdate,da,db,ds,sa,sb,ss
	write(6,*)'SWCLRID3  cfdate < date ',cfdate,date
	go to 23
      endif

 25   continue
	hr=y(4)/100
	imin=y(4)-hr*100
      sncz=x(2)
      tsw=x(4)
      dsw=x(5)
      dir=x(6)
      usw=x(7)
      IF(dir.GT.-20.0) then
        sumsw=dsw+dir
      else
        sumsw=-999.0
      endif
      cosz=x(1)
      au=x(3)
      IF(au.gt.0.0) THEN
        avau=au
        aun=1
      else
        avau=0.0
        aun=0
      endif
      sncz1=sncz+0.1
      if(sncz1.gt.1.d0) sncz1=1.d0

      if(cosz.lt.0.d0.or.tsw.lt.tswlim) go to 22

      do 29 i=1,60
	toa(i)=0.d0
	nsw(i)=0.d0
	difrat(i)=0.d0
	csw(i)=0.d0
	swu(i)=0.d0
	cdr(i)=0.d0
	dr(i)=0.d0
	drr(i)=0.d0
	ndifr(i)=0.d0
	dif(i)=0.d0
	cz(i)=1.d0
	aau(i)=0.d0
	sw(i)=0.d0
	ssw(i)=0.d0
	stdate(i)=0
	sthr(i)=0
	stmin(i)=0
 29   continue
      i=1
      if(cosz.gt.0.02.and.dsw.gt.1.d0.and.tsw.gt.1.d0) then
	cz(i)=cosz
        IF(nodyfl.and.au.gt.0.0000) then
	  toa(i)=(1365.d0/(au**2))*cosz
        else
	  toa(i)=1365.d0*cosz
        endif
	nsw(i)=tsw/(cosz**sb)
cc      use sum SW for difrat if avail, else tsw
        IF(dir.gt.1.0) then
	  difrat(i)=dsw/(dsw+dir)
        else
	  difrat(i)=dsw/tsw
        endif
	dr(i)=dir
	ndifr(i)=(difrat(i))*cosz**0.8d0
        IF(nodyfl.and.au.gt.0.0000) then
	  csw(i)=(sa/(aau(i)**2))*cosz**sb
        else
	  csw(i)=sa*cosz**sb
        endif
        IF(csw(i).gt.99999.0.or.csw(i).LT.-9999.0) csw(i)=-9999.0
	cdr(i)=da*cosz**db
	drr(i)=dabs((difrat(i)-cdr(i))/difrat(i))
	dif(i)=dsw
	sw(i)=tsw
	ssw(i)=sumsw
	swu(i)=usw
	aau(i)=au
	stdate(i)=date
	sthr(i)=hr
	stmin(i)=imin
	i=i+1
	t=t+1
      endif

 30   read(2,*,end=60,err=30) (y(u),u=1,4),(x(v),v=1,7)
	date=y(3)
	hr=y(4)/100
	imin=y(4)-hr*100
      sncz=x(2)
      tsw=x(4)
      dsw=x(5)
      dir=x(6)
      usw=x(7)
      IF(dir.GT.-20.0) then
        sumsw=dsw+dir
      else
        sumsw=-999.0
      endif
      cosz=x(1)
      au=x(3)
      IF(au.gt.0.0) then
        avau=avau+au
        aun=aun+1
      endif

      if(cosz.lt.0.d0.or.tsw.lt.tswlim) go to 30
      sncz1=sncz+0.1
      if(sncz1.gt.1.d0) sncz1=1.d0

      if(cosz.gt.0.02.and.dsw.gt.1.d0.and.tsw.gt.1.d0) then
	cz(i)=cosz
        IF(nodyfl.and.aau(i).gt.0.0000) then
	  toa(i)=(1365.d0/(au**2))*cosz
        else
	  toa(i)=1365.d0*cosz
        endif
	nsw(i)=tsw/(cz(i)**sb)
cc      use sum SW for difrat if avail, else tsw
        IF(dir.gt.1.0) then
	  difrat(i)=dsw/(dsw+dir)
        else
	  difrat(i)=dsw/tsw
        endif
	dr(i)=dir
	ndifr(i)=(difrat(i))*cosz**0.8d0
        IF(nodyfl.and.aau(i).gt.0.0000) then
	  csw(i)=(sa/(aau(i)**2))*cosz**sb
        else
	  csw(i)=sa*cosz**sb
        endif
        IF(csw(i).gt.99999.0.or.csw(i).LT.-9999.0) csw(i)=-9999.0
	cdr(i)=da*cosz**db
	drr(i)=dabs((difrat(i)-cdr(i))/difrat(i))
	dif(i)=dsw
	sw(i)=tsw
	ssw(i)=sumsw
	swu(i)=usw
	aau(i)=au
	stdate(i)=date
	sthr(i)=hr
	stmin(i)=imin
	i=i+1
	t=t+1
      endif
      if(i.eq.(maxtim+1)) then
          e=0
	  do 40 j=(m-cn),(m+cn)
	    avg=ndifr(j)+avg
            e=e+1
 40       continue
	  avg=avg/(e*1.d0)
	  do 41 j=(m-cn),(m+cn)
	    rsum=(ndifr(j)-avg)**2+rsum
 41       continue
	  rsum=sqrt(rsum/(e*1.d0))

	  do 42 j=1,maxtim
	    w=dabs(ndifr(j)-da)
	    avg2=w+avg2
 42       continue
	  avg2=avg2/(maxtim*1.d0)
	  do 43 j=1,maxtim
	    w=dabs(ndifr(j)-da)
	    sum2=(w-avg2)**2+sum2
 43       continue
	  sum2=sqrt(sum2/(maxtim*1.d0))
	  do 44 j=1,(m-cn),(m+cn)
	    w=dabs(ndifr(j)-da)
	    avg2=w+avg2
 44       continue
	  avg2=avg2/(clrtim*1.d0)

          do 45 j=1,maxtim
	    avg3=difrat(j)+avg3
 45       continue
	  avg3=avg3/(maxtim*1.d0)
	  do 46 j=1,maxtim
	    sum3=(difrat(j)-avg3)**2+sum3
 46       continue
	  sum3=sqrt(sum3/(maxtim*1.d0))
	  do 47 j=(m-cn),(m+cn)
	    avg3=difrat(j)+avg3
 47       continue
          avg3=avg3/(clrtim*1.d0)

          do 48 j=1,maxtim
            avg4=drr(j)+avg4
 48       continue
          avg4=avg4/(maxtim*1.d0)
          do 49 j=1,maxtim
            sum4=(drr(j)-avg4)**2+sum4
 49       continue
          sum4=sqrt(sum4/(maxtim*1.d0))
          do 50 j=(m-cn),(m+cn)
            avg4=drr(j)+avg4
 50       continue
          avg4=avg4/(clrtim*1.d0)

	delT=dabs(toa(m+1)-toa(m-1))
	delS=dabs(sw(m+1)-sw(m-1))
	lim=20.d0*sncz*delfac
cc	if(cz(m).gt.0.05d0.and.nsw(m).gt.(sa-100.d0).and.nsw(m).lt.
cc     %(sa+100.d0))then
	if(cz(m).gt.0.05d0.and.nsw(m).gt.(sa-nswmn).and.nsw(m).lt.
     %(sa+nswmx))then
	  go=.true.
	else
	  go=.false.
	endif
        clr=2
        reasn1=-9
        reasn4=-9
        reasn2=-9
        reasn3=-9
        diflim=maxdif*(cz(m)**0.5)
        if(delT.lt.lim.and.go.and.dif(m).lt.diflim) then
         if(dels.lt.(delT+delfac*delSlm*cz(m)).and.
     %dels.gt.(delT-delfac*sncz1/cz(m))) then
          if(rsum.lt.ndstlm)then
            clr=0
	  endif
	 endif
	endif

        IF(.not.GO) reasn1=1
        if(delT.gt.lim.or.dif(m).gt.diflim) reasn2=2
        if(dels.gt.(delT+delfac*delSlm*cz(m)).or.
     %dels.lt.(delT-delfac*sncz1/cz(m))) reasn3=3
        if(rsum.gt.ndstlm) reasn4=4

        if(clr.eq.2) then
          if(avg2.le.0.005d0.and.sum2.le.0.0015d0.and.abs(csw(m)-sw(m))
     %.lt.(0.02d0*csw(m)).and.avg4.le.0.15d0.and.sum4.le.0.03d0)then
             clr=1
          endif

          if(avg3.ge.0.95d0.and.sum3.le.(0.15d0*cz(m)**0.5d0).and.
     %avg4.ge.0.8d0.and.sum4.le.0.05d0) clr=3
        endif

        IF(cldflg) then
         write(3,57)stdate(m),sthr(m),stmin(m),cz(m),aau(m),sw(m),
     %nsw(m),csw(m),dif(m),dr(m),ndifr(m),difrat(m),cdr(m),avg,rsum,
     %avg2,sum2,avg3,sum3,avg4,sum4,clr,reasn1,reasn2,reasn3,reasn4,
     %delT,delS,(delT+(delfac*delSlm*cz(m))),(delT-(delfac*sncz1/cz(m)))
        endif
 57      format(i8,2i3,2f9.5,5f8.1,11f10.5,i3,4i4,4f7.2)
 58      format(i8,2i3,2f9.5,5f8.1,11f10.5,i3,4i4,f9.1,f12.5)
	if(clr.eq.2) then
	    cldnum=cldnum+1
	elseif(clr.eq.0) then
	    clrnum=clrnum+1
         write(4,58)stdate(m),sthr(m),stmin(m),cz(m),aau(m),sw(m),
     %nsw(m),csw(m),dif(m),dr(m),ndifr(m),difrat(m),cdr(m),avg,rsum,
     %avg2,sum2,avg3,sum3,avg4,sum4,clr,reasn1,reasn2,reasn3,reasn4,
     %ssw(m),swu(m)
         write(9,58)stdate(m),sthr(m),stmin(m),cz(m),aau(m),sw(m),
     %nsw(m),csw(m),dif(m),dr(m),ndifr(m),difrat(m),cdr(m),avg,rsum,
     %avg2,sum2,avg3,sum3,avg4,sum4,clr,reasn1,reasn2,reasn3,reasn4,
     %ssw(m),swu(m)
            cl2num=cl2num+1
            if(cz(m).lt.czmin) czmin=cz(m)
            if(cz(m).gt.czmax) czmax=cz(m)
	elseif(clr.eq.1) then
            IF(.not.nodyfl) then
         write(4,58)stdate(m),sthr(m),stmin(m),cz(m),aau(m),sw(m),
     %nsw(m),csw(m),dif(m),dr(m),ndifr(m),difrat(m),cdr(m),avg,rsum,
     %avg2,sum2,avg3,sum3,avg4,sum4,clr,reasn1,reasn2,reasn3,reasn4,
     %ssw(m),swu(m)
	      cl2num=cl2num+1
            endif
            if(cz(m).lt.czmin) czmin=cz(m)
            if(cz(m).gt.czmax) czmax=cz(m)
        elseif(clr.eq.3) then
	    ovcnum=ovcnum+1
	endif
	    avndfr=avndfr+ndifr(m)
	    avnsw=avnsw+nsw(m)
	    n=n+1
        do 59 j=1,(maxtim-1)
	  toa(j)=toa(j+1)
	  nsw(j)=nsw(j+1)
	  difrat(j)=difrat(j+1)
	  csw(j)=csw(j+1)
	  cdr(j)=cdr(j+1)
	  drr(j)=drr(j+1)
	  dr(j)=dr(j+1)
	  ndifr(j)=ndifr(j+1)
	  dif(j)=dif(j+1)
	  cz(j)=cz(j+1)
	  sw(j)=sw(j+1)
	  ssw(j)=ssw(j+1)
	  swu(j)=swu(j+1)
	  aau(j)=aau(j+1)
	  stdate(j)=stdate(j+1)
	  sthr(j)=sthr(j+1)
	  stmin(j)=stmin(j+1)
 59     continue
	i=maxtim
	avg=0.d0
	rsum=0.d0
	avg2=0.d0
	sum2=0.d0
        avg3=0.d0
	sum3=0.d0
        avg4=0.d0
        sum4=0.d0
      endif
      go to 30

 60   CONTINUE
      CLOSE(UNIT=2)
	 if(n.gt.0) then
	   avndfr=avndfr/(n*1.d0)
	   avnsw=avnsw/(n*1.d0)
	 else
	   avndfr=1.d0
	   avnsw=-1.d0
	 endif
         IF(aun.gt.0) then
           avau=avau/aun
         else
           avau=-9.0
         endif
         write(8,61)date,avndfr,avnsw,sncz,n,t,cl2num,clrnum,cldnum,
     %ovcnum,czmin,czmax,avau
 61      format(i8,f10.6,f10.1,f10.6,6i6,3f10.5)
         if(cl2num.ge.delnum) write(89,64) out2
 64      format(a12)
         if(cl2num.lt.delnum) write(88,65)'del ',out2
 65      format(a4,a12)
         if(clrnum.lt.delnum) write(88,65)'del ',out4
	 close(unit=3)
	 close(unit=4)
	 close(unit=7)
	 close(unit=9)
	 close(unit=71)
	 close(unit=99)
      GO TO 10


 100  CONTINUE
	 close(unit=1)
	 close(unit=88)
	 close(unit=89)
	 close(unit=8)
	 close(unit=77)
      RETURN
      END
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine swclreq3(dyfil,mlim,sflg,notgd)
      PARAMETER (NMAX=199999)
      INTEGER N,totn,i,j,date,hr,imin,mlim,clr,clrnum,cldnum,ovcnum
      INTEGER cl2num,clrdyn,sswn,n1,n2,n3,n4,maxczn,an,sflg
      REAL*8 cosz,armang,tsw,nsw,dif,dir,difrat,ndfr,maxcz,mincz,swu
      REAL*8 dfrb,dfra,cswa,cswb,devcsw,devdfr,avndr,sncz,devsdf,au
      REAL*8 atsw(NMAX),acz(NMAX),adfr(NMAX),avnsw,rerr,csw,cdr,aa
      REAL*8 avg1,sum1,avg2,sum2,avg3,sum3,avg4,sum4,mczrat
      REAL*8 ssw,assw(NMAX),sacz(NMAX),csswa,csswb,devssw,lswdd,x,y
      REAL*8 sdfmin,swdif(NMAX),swdifa,swdifb,lswda,lswdb,lswdc,sdfmax
      REAL*8 aalb(NMAX),aczan(NMAX),devalb,ab,mxsncz
      character*12 inpfil,infil,outfil,dirfil,allfil
      character*8 inpdat
      CHARACTER*1 ANS
      logical ok,dyfil,noclr,notgd

      inpfil='swclrid3.day'
      outfil='swclreq3.asc'
      dirfil='swclrid3.dir'
      allfil='all_clr.asc'
      ok=.true.
      noclr=.true.
      notgd=.false.
      rerr=-9.0d0
      i=0
      sswn=0
      an=0
      clrdyn=0
          swdifa=0.0
          swdifb=0.0
      sdfmin=1000000.0
      sdfmax=-1000000.0
        lswda=-90.0
        lswdb=-90.0
        lswdc=-90.0
        lswdd=-90.0
      mxsncz=-9.0
      maxcz=-1000.d0
      mincz=1000.d0
      maxczn=0

      IF(.not.dyfil) then
        open(unit=77,file=allfil,status='unknown')
        WRITE(77,*)'   date  hr mn     cosz      au     tsw     dif
     %dir    difrat      ssw        swu'
      endif
      open(unit=1,file=inpfil,status='old')
      open(unit=89,file=dirfil,status='old')
      open(unit=3,file=outfil,status='unknown')
      write(3,*)'  for: clrDfR OR clrTSW = a*(CosZ)**b    >>>swclreq3'
      write(3,1)'  date        dfra      dfrb    devdfr    cswa      csw
     %b     AvgAU      sncz         N   csswa     csswb      sswn    mcz
     %rat    swdifc    swdifa    swdifb    sdfmax        alba        alb
     %b'
      WRITE(6,*)' '
      WRITE(6,*)' '
      write(6,*)'  for: clrDfR OR clrTSW = a*(CosZ)**b    >>>swclreq3'
      write(6,2)'   date      dfra    dfrb    devdfr    cswa    cswb
     %devcsw   sncz    N'
 1    format(a188)
 2    format(a74)
 3    format(a12)
      read(1,*)

 4    read(1,5,end=50,err=100)inpdat,avndr,avnsw,sncz,n,totn,cl2num,
     %clrnum,cldnum,ovcnum,x,y,au
 5    format(a8,f10.6,f10.1,f10.6,6i6,3f10.5)
      if(cl2num.ge.mlim.and.dyfil) then
        read(89,3,end=100) infil
        noclr=.false.
        clrdyn=clrdyn+1
        i=0
        sswn=0
        an=0
        maxcz=-1000.d0
        mincz=1000.d0
        maxczn=0
      ELSEIF(.not.dyfil.and.clrnum.ge.mlim) then
        read(89,3) infil
        noclr=.false.
        IF(sncz.gt.mxsncz) mxsncz=sncz
      else
        if(dyfil) write(3,71)inpdat,rerr,rerr,rerr,rerr,rerr,au,sncz,
     %cl2num,rerr,rerr,clrnum,rerr,rerr,rerr,rerr,rerr,rerr,rerr
        go to 4
      endif

      if(ok) then
        do 6 j=1,NMAX
          atsw(J)=0.d0
          acz(J)=0.d0
          adfr(j)=0.d0
	  assw(J)=0.d0
          sacz(J)=0.d0
          swdif(j)=0.d0
	  aczan(J)=0.d0
	  aalb(j)=0.d0
 6      continue
        ok=.false.
      endif

      open(unit=2,file=infil,status='old')
      read(2,*)
 9    read(2,10,end=20,err=95)date,hr,imin,cosz,armang,tsw,nsw,csw,dif,
     %dir,ndfr,difrat,cdr,avg1,sum1,avg2,sum2,avg3,sum3,avg4,sum4,clr,
     %n1,n2,n3,n4,ssw,swu
 10   format(i8,2i3,2f9.5,5f8.1,11f10.5,i3,4i4,f9.1,f12.5)

      if(cosz.gt.0.1d0.and.difrat.gt.0.d0.and.tsw.gt.0.d0) then
        i=i+1
        IF(.not.dyfil) then
         WRITE(77,11)date,hr,imin,cosz,armang,tsw,dif,dir,difrat,ssw,swu
 11       FORMAT(i8,2i3,2f9.5,3f8.1,f10.5,f9.1,f12.5)
        endif

        IF(i.gt.NMAX) then
          WRITE(6,*)'**************************************************'
          WRITE(6,*)'Number of clear points greater than',NMAX,' limit'
          WRITE(6,*)'          in swclreq3 subroutine.'
          WRITE(6,*)' Processing up to ',date
          WRITE(6,*) hr,imin,' only!'
          WRITE(6,*)'    Click on OK to continue.'
          WRITE(6,*)'**************************************************'
          pause
          i=NMAX
          GO TO 50
        endif
        IF(dyfil) then
          atsw(i)=log10(tsw*cosz)
        ELSEIF(armang.gt.0.0) then
          atsw(i)=log10((tsw*(armang**2))*cosz)
        else
          atsw(i)=log10(tsw*cosz)
        endif
	acz(i)=log10(cosz)
        adfr(i)=log10(difrat/cosz)
        IF(ssw.gt.0.0) then
          sswn=sswn+1
          IF(dyfil) then
            assw(sswn)=LOG10(ssw*cosz)
          ELSEIF(armang.gt.0.0) then
            assw(sswn)=LOG10((ssw*(armang**2))*cosz)
          else
            assw(sswn)=LOG10(ssw*cosz)
          endif
          sacz(sswn)=LOG10(cosz)
          IF((tsw-ssw).lt.sdfmin.AND.(tsw-ssw).GT.-100.0) then
            sdfmin=(tsw-ssw)
          endif
          IF((tsw-ssw).gt.sdfmax.AND.(tsw-ssw).lT.100.0) then
            sdfmax=(tsw-ssw)
          endif
          swdif(sswn)=(tsw-ssw)
        endif
cc	if(swu.gt.0.d0.and.sflg.lt.3) then
	if(swu.gt.0.d0.and.sflg.eq.2) then
          an=an+1
cc          IF(sflg.eq.1.and.ssw.gt.0.0) then
cc	    aalb(an)=LOG10(swu/(ssw*cosz**2))
cc          elseIF(sflg.eq.1.and.tsw.gt.0.0) then
cc	    aalb(an)=LOG10(swu/(tsw*cosz**2))
cc          ELSEIF(sflg.eq.2) then
            IF(armang.gt.0.0) then
              aalb(an)=LOG10((swu*(armang**2))*cosz)
            else
              aalb(an)=LOG10(swu*cosz)
            endif
cc          endif
	  aczan(an)=log10(cosz)
        endif
        if(cosz.gt.maxcz) maxcz=cosz
        if(cosz.lt.mincz) mincz=cosz
        if(cosz.GT.(0.6*sncz)) maxczn=maxczn+1
      endif
      go to 9

 20   continue
      close(unit=2)
      if(.not.dyfil) go to 4

 50   continue
      mczrat=(maxczn*1.0)/i
      IF(mczrat.gt.99.0) mczrat=99.0
      IF(mczrat.LT.-99.0) mczrat=-99.0
      if(noclr.and.dyfil)then
        if(clrdyn.gt.0) noclr=.false.
        go to 100
      endif
      if(noclr) go to 100
      if(dyfil.and.i.lt.mlim) then
        write(3,71)inpdat,rerr,rerr,rerr,rerr,rerr,au,sncz,-66,rerr,rerr
     %,sswn,mczrat,rerr,rerr,rerr,rerr,rerr,rerr
        write(6,72)' ',inpdat,rerr,rerr,rerr,rerr,rerr,rerr,sncz,i
        go to 80
      endif
      call medfit(acz,adfr,i,dfra,dfrb,devdfr)
      dfra=10.d0**dfra
      dfrb=dfrb+1.000000

      call medfit(acz,atsw,i,cswa,cswb,devcsw)
      cswa=10.d0**cswa
      cswb=cswb-1.000000

ccccccccc Test for unrealistic values   ccccccccccccc
      IF(dfra.gt.1.0.or.dfra.lt.0.0) then
        i=-88
        IF(dyfil) then
          dfra=-9.0
        else
          notgd=.true.
        endif
      endif
      IF(dfrb.gt.0.0.or.dfrb.lt.-2.0) then
        i=-88
        IF(dyfil) then
          dfrb=-9.0
        else
          notgd=.true.
        endif
      endif
      IF(cswa.gt.1600.0.or.cswa.lt.0.0) then
        i=-44
        IF(dyfil) then
          cswa=-9.0
        else
          notgd=.true.
        endif
      endif
      IF(cswb.gt.2.0.or.cswb.lt.0.9) then
        i=-44
        IF(dyfil) then
          cswb=-9.0
        else
          notgd=.true.
        endif
      endif
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      call medfit(sacz,assw,sswn,csswa,csswb,devssw)
      csswa=10.d0**csswa
      csswb=csswb-1.000000

      IF(csswa.gt.1600.or.csswb.lt.0.9.or.csswb.gt.1.5) then
        csswa=cswa
        csswb=cswb
      endif

      IF(sdfmin.lt.0.0.and.sdfmin.GT.-100.0) then
        sdfmin=sdfmin*1.05
      ELSEIF(sdfmin.ge.0.0.and.sdfmin.lt.100.0) then
        sdfmin=sdfmin*0.95
      else
        sdfmin=0.0001
      endif
      if(sdfmax.gt.100.0.or.sdfmax.LT.-100.0) sdfmax=100.0

      IF(sswn.gt.10.and.(sdfmax-sdfmin).lt.0.1) then
          swdifa=lswda
          swdifb=lswdb
        GO TO 60
      ELSEIF(sswn.gt.10) then
        do j=1,sswn
          IF((swdif(j)-sdfmin).lt.0.0001) then
            swdif(j)=LOG10(0.0001)
          else
            swdif(j)=LOG10(swdif(j)-sdfmin)
          endif
        end do

        call medfit(sacz,swdif,sswn,swdifa,swdifb,devsdf)
        swdifa=10.d0**swdifa
        IF(swdifa.ge.100.0.or.swdifa.le.-100.0) then
          swdifa=lswda
          swdifb=lswdb
          sdfmin=lswdc
          sdfmax=lswdd
        elseIF(swdifb.ge.10.0.or.swdifb.le.-10.0) then
          swdifa=lswda
          swdifb=lswdb
          sdfmin=lswdc
          sdfmax=lswdd
        else
          lswda=swdifa
          lswdb=swdifb
          lswdc=sdfmin
          lswdd=sdfmax
        endif
      else
        swdifa=lswda
        swdifb=lswdb
        sdfmin=lswdc
        sdfmax=lswdd
      endif

 60   IF(an.gt.(mlim*0.75)) then
        call medfit(aczan,aalb,an,aa,ab,devalb)
        aa=10.d0**aa
cc        IF(sflg.eq.1) then
cc          ab=ab+2.000000
cc        ELSEIF(sflg.eq.2) then
          ab=ab-1.0
cc        endif
      else
        aa=-6.d0
        ab=-6.d0
      endif
cc      IF(sflg.eq.1) then
cc        IF(aa.gt.aalim.or.ab.gt.ablim.or.ab.LE.-1.0) then
cc          aa=-6.d0
cc          ab=-6.d0
cc        endif
cc      endif

      if(.not.dyfil) inpdat='000000'
      if(dyfil.and.mczrat.lt.0.45) then
        cswa=rerr
        cswb=rerr
        i=-44
        csswa=rerr
        csswb=rerr
      endif

      if(.not.dyfil.and.maxcz/mxsncz.lt.0.75) then
        i=-44
        notgd=.true.
      endif

      if(mincz.gt.0.4d0) then
        IF(i.EQ.-44) then
          i=-11
        else
          i=-55
        endif
        if(.not.dyfil) then
          notgd=.true.
        else
          dfrb=rerr
          devdfr=rerr
        endif
      endif

ccccccccccccc  limit changed due to "bug" discovered in Oregon Burns data  ccccc
ccc      if(dfrb.gt.-0.4) then
      if(dfrb.gt.-0.25) then
        IF(i.EQ.-44) then
          i=-11
        else
          i=-88
        endif
        if(.not.dyfil) then
          notgd=.true.
        else
          dfra=rerr
          dfrb=rerr
          devdfr=rerr
        endif
      endif

ccccccccccccc  limit changed due to now having IR loss corrected diffuse data  ccccc
cccc      if(dfrb.lt.-0.95) then
      if(dfrb.lt.-1.0) then
        IF(i.EQ.-44) then
          i=-11
        ELSEIF(i.EQ.-88) then
          i=-88
        ELSEIF(i.EQ.-11) then
          i=-11
        else
          i=-99
        endif
        if(.not.dyfil) then
          notgd=.true.
        else
          dfrb=rerr
          devdfr=rerr
        endif
      endif

      if(dyfil.and.i.EQ.-11) then
        write(3,71)inpdat,rerr,rerr,rerr,rerr,rerr,au,sncz,i,rerr,rerr,
     %sswn,mczrat,rerr,rerr,rerr,rerr,rerr,rerr
        write(6,72)' ',inpdat,rerr,rerr,rerr,rerr,rerr,rerr,sncz,i
        go to 80
      else
        IF(.not.dyfil) sncz=-9.0
        IF(notgd) then
          WRITE(6,*)'Last iteration, coefficient test(s) failed:'
          WRITE(6,*)' Num:',i
          WRITE(6,73)' maxcz/mxsncz.lt.0.75:',maxcz/mxsncz
          WRITE(6,73)' maxcz:',maxcz
          WRITE(6,73)' mxsncz:',mxsncz
          WRITE(6,73)' mincz (lim 0.4):',mincz
          WRITE(6,*)' -11 or -88:  dfrb.gt.-0.25'
          WRITE(6,*)' above or -99: dfrb.lt.-1.0'
          WRITE(6,61)' dfrb= ',dfrb
 61       format(a,f12.6)
          WRITE(6,*)' Do you wish to set the dfrb value to the default '
          WRITE(6,*)' limit (if needed) and proceed with this run of '
          WRITE(6,*)' the program? (enter y/n)'
cc          pause
          READ(*,73) ans
          IF(ans.eq.'y'.or.ans.eq.'Y') then
            notgd=.false.
cccccc  changed due to changed test limit of -0.25 from Oregon data fix   ccccccc
cc            IF(dfrb.GT.-0.4) dfrb=-0.4
            IF(dfrb.GT.-0.25) dfrb=-0.25
            IF(dfrb.lT.-1.0) dfrb=-1.0
          else
            cswa=rerr
            cswb=rerr
            csswa=rerr
            csswb=rerr
            dfra=rerr
            dfrb=rerr
            devdfr=rerr
            GO TO 100
          endif
 73       format(a,3f12.6)
        endif
 70     write(3,71)inpdat,dfra,dfrb,devdfr,cswa,cswb,au,sncz,i,csswa,
     %csswb,sswn,mczrat,sdfmin,swdifa,swdifb,sdfmax,aa,ab
        write(6,72)' ',inpdat,dfra,dfrb,devdfr,cswa,cswb,devcsw,sncz,i
 71     format(a8,f10.6,2f10.6,f8.1,2f10.6,f10.6,i10,f8.1,f10.6,i10,
     %f10.4,4f10.3,2f12.5)
 72     format(a1,a8,3f9.5,f8.1,3f9.5,i5)
      endif
 80   if(dyfil) then
        noclr=.true.
        ok=.true.
        i=0
        sdfmin=1000000.0
        sdfmax=-1000000.0
        go to 4
      else
        go to 100
      endif

 95   WRITE(6,*)'   Read error in swclreq3 sub from file: ',infil
      write(6,*) date,hr,imin,cosz,armang,tsw,nsw,csw,dif,
     %dir,ndfr,difrat,cdr,avg1,sum1,avg2,sum2,avg3,sum3,avg4,sum4,clr
      pause

 100  continue
      close(unit=1)
      close(unit=2)
      close(unit=3)
      close(unit=89)
      return
      end
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine swclrfcg(dyfil,nlim,dirfil,swrlim,site,h,maxdif,czlim,
     %mfac,ngt,lat,long)
      PARAMETER (NDAYMAX=12000)
      INTEGER n,m,cdate(0:NDAYMAX),c(0:NDAYMAX),date,tswn,difn,i,k,J,O
      INTEGER l,nlim,cswn,cdifn,ddate(0:NDAYMAX),pdate(0:NDAYMAX),dayn
      INTEGER edate(0:NDAYMAX),sn(0:NDAYMAX),sswn,z,cssn,sflg,tflg,dflg
      integer can,tdate,clrdat,clrtim(0:NDAYMAX),clrhr,clrmn,clrflg,nclr
      integer site,zdate,ztim,ldate,ltim,an,rflg,q,h,drdate,z2
      INTEGER nndr,albn,dyn
      REAL dfrb(0:NDAYMAX),dfra(0:NDAYMAX),cswa(0:NDAYMAX)
      REAL sncz(0:NDAYMAX),csswa(0:NDAYMAX),csswb(0:NDAYMAX)
      REAL cswb(0:NDAYMAX),xtr(30),devalb(0:NDAYMAX),ma(0:NDAYMAX)
      REAL alba(0:NDAYMAX),albb(0:NDAYMAX),albd(0:NDAYMAX),fa(0:NDAYMAX)
      real SumB(0:NDAYMAX),SumC(0:NDAYMAX),SumA(0:NDAYMAX)
      REAL ab(0:NDAYMAX),summax(0:NDAYMAX),aa(0:NDAYMAX),aau(0:NDAYMAX)
      REAL*8 tsw,dif,DIR,cosz,csw,cdif,difr,cdifr,rerr,serr,cdir
      REAL*8 tswfcg,diffcg,dtsw,dcsw,dswfcg,ddif,dcdif,ddiffc,ddifr,dcdr
      REAL*8 lda,ldb,lta,ltb,da,db,ta,tb,au,lcdifr,lcosz,cslope,lslope
      real*8 ssw,cssw,sswfcg,dssw,dcssw,dsswfc,swrlim,mczrat,scz
      REAL*8 lsmax,lsuma,lsumb,lsumc,tsuma,tsumb,tsumc,tsmax
      REAL*8 laa,lab,aaa,aab,swu,alb,calb,dcalb,dalb
      REAL*8 maxdif,czlim,newdra,orgdra,mfac,lat,long,lasdra,w,xx2,xx3
      REAL*8 uz,ucosz,incr1,incr2,adjz90,pi,sza,xxx,x,y,lsa,lsb,sa,sb
      CHARACTER*12 DIRFIL,INFIL,OUTFIL,COFFIL,tmpfil,dayfil,posfil
      CHARACTER*12 lasfil,badfil,clrfil
      character*250 headr1,headr2,headr3
      CHARACTER*5 xhdr(30)
      CHARACTER*107 tmplin
      CHARACTER*1 ans
      logical dyfil,ok,ngt

      IF(swrlim.lt.0.02) then
        WRITE(6,*)' in swclrfcg, swrlim=',swrlim
        pause
      endif

      
      rerr=-9999.0d0
      serr=-9.0d0
      drdate=99999999
      incr1= 0.571428571
      incr2= 0.276497696
      adjz90= 89.14285714
      pi=dacos(-1.d0)
      do n=0,NDAYMAX
        c(n)=0
        sn(n)=0
        clrtim(n)=0
        cdate(n)=0
        ddate(n)=0
        edate(n)=0
        pdate(n)=0
        dfrb(n)=0.0
        dfra(n)=0.0
        cswb(n)=0.0
        cswa(n)=0.0
        csswb(n)=0.0
        csswa(n)=0.0
        sumb(n)=0.0
        suma(n)=0.0
        sumc(n)=0.0
        summax(n)=0.0
        ab(n)=0.0
        aa(n)=0.0
        sncz(n)=0.0
        aau(n)=0.0
        alba(n)=0.0
        albb(n)=0.0
        albd(n)=0.0
        devalb(n)=0.0
        ma(n)=0.0
        fa(n)=0.0
      end do

      headr1='   Zdate  Ztim     Ldate  Ltim      CosZ       AU       ts
     %w      csw   tswfcg      dif     cdif   diffcg      dir     cdir
     %   difr    cdifr      ssw     cssw   sswfcg sflg tflg dflg rflg cl
     %rf        SWup       CSWup'
      headr2='   Zdate  Ztim     Ldate  Ltim      CosZ       AU       ts
     %w      csw   tswfcg      dif     cdif   diffcg      dir     cdir
     %   difr    cdifr      ssw     cssw   sswfcg sflg tflg dflg rflg cl
     %rf        SolX       CSolX'
      headr3='   Zdate  Ztim     Ldate  Ltim      CosZ       AU       ts
     %w      csw   tswfcg      dif     cdif   diffcg      dir     cdir
     %   difr    cdifr      ssw     cssw   sswfcg sflg tflg dflg rflg cl
     %rf         Aux        ****'

      lasfil='swlastdy.cfw'
      COFFIL='swclreq3.asc'
      badfil='bad_data.swf'
      open(unit=1,file=coffil,status='old',err=91)
      open(unit=2,file='clrcoef3.asc',status='unknown')
      read(1,*)
      read(1,*)
      n=0
      j=1
      z=0
      if(dyfil) then
 2      n=n+1
        IF(n.gt.NDAYMAX) GO TO 10
        read(1,*,end=10)cdate(n),dfra(n),dfrb(n),x,cswa(n),cswb(n),
     %aau(n),sncz(n),c(n),csswa(n),csswb(n),sn(n),mczrat,sumc(n)
     %,suma(n),sumb(n),summax(n),aa(n),ab(n)
        if(c(n).eq.-99) then
           pdate(n)=cdate(n)
        elseif(c(n).eq.-55) then
           pdate(n)=cdate(n)
        elseif(c(n).eq.-88) then
           edate(n)=cdate(n)
        elseif(c(n).eq.(-44)) then
          ddate(n)=cdate(n)
        endif
        IF(c(n).ge.nlim) z=n
ccccccccccccccccc  new "last day" cccccccccccccccccccc
        IF(c(n).ge.200) z2=n
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        go to 2
      else
        read(1,*)cdate(1),dfra(1),dfrb(1),x,cswa(1),cswb(1),y,sncz(1),
     %c(1),csswa(1),csswb(1),sn(1),mczrat,sumc(1)
     %,suma(1),sumb(1),summax(1),aa(1),ab(1)
 3      format(i8,f10.6,2f10.6,f10.2,2f10.6,f10.6,2i6,f10.2,f10.6,i6,
     %5f10.3,2f12.5)
        n=1
        z=1
        orgdra=dfra(1)
        write(2,3)cdate(1),dfra(1),dfrb(1),x,cswa(1),cswb(1),y,
     %sncz(1),c(1),1,csswa(1),csswb(1),sn(1),mczrat,sumc(1)
     %,suma(1),sumb(1),summax(1),aa(1),ab(1)
        close(unit=2)
        OPEN(UNIT=57,FILE='clrndr_b.asc',STATUS='unknown')
        WRITE(57,*)' infil            orgdra      newdra     flg     #<l
     %im'


ccccccccccc new one-fit DFRA interpolation loop cccccccccccc

        open(unit=22,file=dirfil,status='old')

 5      read(22,26,end=6) tmpfil
        m=index(tmpfil,'.')
        infil=tmpfil(1:m)//'cos'
        m=m-1
        n=n+1
        READ(tmpfil(1:m),31) cdate(n)
        nndr=0
        call clrndr(infil,orgdra,dfrb(1),csswa(1),csswb(1),maxdif,czlim,
     %mfac,newdra,nndr)
        IF(newdra.lt.0.2) then
          dfra(n)=newdra
          lasdra=newdra
          READ(tmpfil(1:m),*) drdate
          i=1
        else
          dfra(n)=9.0
          i=0
        endif
        WRITE(57,109) infil,orgdra,newdra,i,nndr
        GO TO 5

 6      CLOSE(UNIT=22)
        CLOSE(UNIT=57)

        OPEN(UNIT=57,FILE='clrndr_a.asc',STATUS='unknown')
        WRITE(57,*)'   Cdate      newdra     Origdra       DRb'
         WRITE(57,8) cdate(1), dfra(1), dfra(1) ,dfrb(1)

        dyn=n

        lda=dfra(1)
        do i=2,n
         newdra=dfra(i)
         k=1
         IF(dfra(i).GT.0.2) then
 7         continue
           IF((i+k).gt.n) then
             do o=i,n
               dfra(o)=dfra(1)
             end do
           ELSEIF(dfra(i+k).gt.0.2) then
             k=k+1
             GO TO 7
           ELSE
             da=(dfra(i+k)-lda)/(k+1)
             dfra(i)=lda+da
           endif
         endif
         lda=dfra(i)
         WRITE(57,8) cdate(i), dfra(i), newdra, dfrb(1)
 8       FORMAT(i8,6f12.6)
        end do

 9      n=1
ccccccccccc END  new one-fit DFRA interpolation loop cccccccccccc

      endif

 10   close(unit=1)
      x=-9.0
      y=-9.0
      mczrat=-0.90

ccccccccccccccccc multiple day loop cccccccccccccccccccccccccc

      if(dyfil) then
        n=n-1
        dyn=n
        i=1
        open(unit=78,file=lasfil,status='old',ERR=92)
        read(78,*)
        read(78,*) date,lda,ldb,lta,ltb,lsa,lsb,lsumc,lsuma,lsumb,lsmax,
     %laa,lab
        WRITE(6,*)' '
        WRITE(6,*)' '
        write(6,*)'____________________________________________________'
        write(6,*)' Last day and coefficients:'
        write(6,21) ' ',date,lda,ldb,lta,ltb,lsa,lsb
        ok=.true.
        k=0
        close(unit=78)
        do 18 i=1,n
         if(c(i).lt.nlim.and.cdate(i).ne.pdate(i).and.cdate(i).ne.
     %ddate(i).and.cdate(i).ne.edate(i)) then
           k=k+1
         elseif(cdate(i).eq.pdate(i)) then
           j=1
           o=i+1
 11        continue
           if(c(o).lt.nlim.and.o.lt.n) then
             o=o+1
             j=j+1
             go to 11
           elseif(c(o).lt.nlim.and.o.eq.n) then
             dfrb(o)=ldb
           endif
           da=(dfra(i)-lda)/(k+1)
           db=(dfrb(o)-ldb)/(k+j+1)
           ta=(cswa(i)-lta)/(k+1)
           tb=(cswb(i)-ltb)/(k+1)
           sa=(csswa(i)-lsa)/(k+1)
           sb=(csswb(i)-lsb)/(k+1)
           tsmax=(summax(i)-lsmax)/(k+1)
           tsuma=(suma(i)-lsuma)/(k+1)
           tsumb=(sumb(i)-lsumb)/(k+1)
           tsumc=(sumc(i)-lsumc)/(k+1)
           aaa=(aa(i)-laa)/(k+1)
           aab=(ab(i)-lab)/(k+1)
           if(ok) then
             dfra(i-k-1)=lda
             dfrb(i-k-1)=ldb
             cswa(i-k-1)=lta
             cswb(i-k-1)=ltb
             csswa(i-k-1)=lsa
             csswb(i-k-1)=lsb
             summax(i-k-1)=lsmax
             suma(i-k-1)=lsuma
             sumb(i-k-1)=lsumb
             sumc(i-k-1)=lsumc
             aa(i-k-1)=laa
             ab(i-k-1)=lab
             ok=.false.
           endif
           do 12 l=(i-k),(i)
             dfra(l)=dfra(l-1)+da
             dfrb(l)=dfrb(l-1)+db
             cswa(l)=cswa(l-1)+ta
             cswb(l)=cswb(l-1)+tb
             csswa(l)=csswa(l-1)+sa
             csswb(l)=csswb(l-1)+sb
             summax(l)=summax(l-1)+tsmax
             suma(l)=suma(l-1)+tsuma
             sumb(l)=sumb(l-1)+tsumb
             sumc(l)=sumc(l-1)+tsumc
             IF(aa(i).LT.-5.8.and.aa(i).GT.-6.2) then
               aa(l)=serr
               ab(l)=serr
             ELSEIF(laa.LT.-5.8.and.laa.GT.-6.2) then
               aa(l)=serr
               ab(l)=serr
             ELSEIF(aa(i).LT.-8.8.and.aa(i).GT.-9.2) then
               aa(l)=serr
               ab(l)=serr
             ELSEIF(laa.LT.-8.8.and.laa.GT.-9.2) then
               aa(l)=serr
               ab(l)=serr
             else
               aa(l)=aa(l-1)+aaa
               ab(l)=ab(l-1)+aab
             endif
        write(2,3)cdate(l),dfra(l),dfrb(l),x,cswa(l),cswb(l),aau(l),
     %sncz(l),c(l),l,csswa(l),csswb(l),sn(l),mczrat,sumc(l)
     %,suma(l),sumb(l),summax(l),aa(l),ab(l)
 12        continue
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           lsa=csswa(i)
           lsb=csswb(i)
           lsmax=summax(i)
           lsuma=suma(i)
           lsumb=sumb(i)
           lsumc=sumc(i)
           laa=aa(i)
           lab=ab(i)
           k=0
         elseif(cdate(i).eq.edate(i)) then
           j=1
           o=i+1
 13        continue
           if(c(o).lt.nlim.and.o.lt.n) then
             o=o+1
             j=j+1
             go to 13
           elseif(c(o).lt.nlim.and.o.eq.n) then
             dfrb(o)=ldb
             dfra(o)=lda
           endif
           da=(dfra(o)-lda)/(k+j+1)
           db=(dfrb(o)-ldb)/(k+j+1)
           ta=(cswa(i)-lta)/(k+1)
           tb=(cswb(i)-ltb)/(k+1)
           sa=(csswa(i)-lsa)/(k+1)
           sb=(csswb(i)-lsb)/(k+1)
           tsmax=(summax(i)-lsmax)/(k+1)
           tsuma=(suma(i)-lsuma)/(k+1)
           tsumb=(sumb(i)-lsumb)/(k+1)
           tsumc=(sumc(i)-lsumc)/(k+1)
           aaa=(aa(i)-laa)/(k+1)
           aab=(ab(i)-lab)/(k+1)
           if(ok) then
             dfra(i-k-1)=lda
             dfrb(i-k-1)=ldb
             cswa(i-k-1)=lta
             cswb(i-k-1)=ltb
             csswa(i-k-1)=lsa
             csswb(i-k-1)=lsb
             summax(i-k-1)=lsmax
             suma(i-k-1)=lsuma
             sumb(i-k-1)=lsumb
             sumc(i-k-1)=lsumc
             aa(i-k-1)=laa
             ab(i-k-1)=lab
             ok=.false.
           endif
           do 14 l=(i-k),(i)
             dfra(l)=dfra(l-1)+da
             dfrb(l)=dfrb(l-1)+db
             cswa(l)=cswa(l-1)+ta
             cswb(l)=cswb(l-1)+tb
             csswa(l)=csswa(l-1)+sa
             csswb(l)=csswb(l-1)+sb
             summax(l)=summax(l-1)+tsmax
             suma(l)=suma(l-1)+tsuma
             sumb(l)=sumb(l-1)+tsumb
             sumc(l)=sumc(l-1)+tsumc
             IF(aa(i).LT.-5.8.and.aa(i).GT.-6.2) then
               aa(l)=serr
               ab(l)=serr
             ELSEIF(laa.LT.-5.8.and.laa.GT.-6.2) then
               aa(l)=serr
               ab(l)=serr
             ELSEIF(aa(i).LT.-8.8.and.aa(i).GT.-9.2) then
               aa(l)=serr
               ab(l)=serr
             ELSEIF(laa.LT.-8.8.and.laa.GT.-9.2) then
               aa(l)=serr
               ab(l)=serr
             else
               aa(l)=aa(l-1)+aaa
               ab(l)=ab(l-1)+aab
             endif
        write(2,3)cdate(l),dfra(l),dfrb(l),x,cswa(l),cswb(l),aau(l),
     %sncz(l),c(l),l,csswa(l),csswb(l),sn(l),mczrat,sumc(l)
     %,suma(l),sumb(l),summax(l),aa(l),ab(l)
 14        continue
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           lsa=csswa(i)
           lsb=csswb(i)
           lsmax=summax(i)
           lsuma=suma(i)
           lsumb=sumb(i)
           lsumc=sumc(i)
           laa=aa(i)
           lab=ab(i)
           k=0
         elseif(cdate(i).eq.ddate(i)) then
           j=1
           o=i+1
 15        continue
           if(c(o).lt.nlim.and.o.lt.n) then
             o=o+1
             j=j+1
             go to 15
           elseif(c(o).lt.nlim.and.o.eq.n) then
             cswa(o)=lta
             cswb(o)=ltb
             csswa(o)=lsa
             csswb(o)=lsb
           endif
           da=(dfra(i)-lda)/(k+1)
           db=(dfrb(i)-ldb)/(k+1)
           ta=(cswa(o)-lta)/(k+j+1)
           tb=(cswb(o)-ltb)/(k+j+1)
           sa=(csswa(o)-lsa)/(k+j+1)
           sb=(csswb(o)-lsb)/(k+j+1)
           tsmax=(summax(i)-lsmax)/(k+1)
           tsuma=(suma(i)-lsuma)/(k+1)
           tsumb=(sumb(i)-lsumb)/(k+1)
           tsumc=(sumc(i)-lsumc)/(k+1)
           aaa=(aa(i)-laa)/(k+1)
           aab=(ab(i)-lab)/(k+1)
           if(ok) then
             dfra(i-k-1)=lda
             dfrb(i-k-1)=ldb
             cswa(i-k-1)=lta
             cswb(i-k-1)=ltb
             csswa(i-k-1)=lsa
             csswb(i-k-1)=lsb
             summax(i-k-1)=lsmax
             suma(i-k-1)=lsuma
             sumb(i-k-1)=lsumb
             sumc(i-k-1)=lsumc
             aa(i-k-1)=laa
             ab(i-k-1)=lab
             ok=.false.
           endif
           do 16 l=(i-k),(i)
             dfra(l)=dfra(l-1)+da
             dfrb(l)=dfrb(l-1)+db
             cswa(l)=cswa(l-1)+ta
             cswb(l)=cswb(l-1)+tb
             csswa(l)=csswa(l-1)+sa
             csswb(l)=csswb(l-1)+sb
             summax(l)=summax(l-1)+tsmax
             suma(l)=suma(l-1)+tsuma
             sumb(l)=sumb(l-1)+tsumb
             sumc(l)=sumc(l-1)+tsumc
             IF(aa(i).LT.-5.8.and.aa(i).GT.-6.2) then
               aa(l)=serr
               ab(l)=serr
             ELSEIF(laa.LT.-5.8.and.laa.GT.-6.2) then
               aa(l)=serr
               ab(l)=serr
             ELSEIF(aa(i).LT.-8.8.and.aa(i).GT.-9.2) then
               aa(l)=serr
               ab(l)=serr
             ELSEIF(laa.LT.-8.8.and.laa.GT.-9.2) then
               aa(l)=serr
               ab(l)=serr
             else
               aa(l)=aa(l-1)+aaa
               ab(l)=ab(l-1)+aab
             endif
        write(2,3)cdate(l),dfra(l),dfrb(l),x,cswa(l),cswb(l),aau(l),
     %sncz(l),c(l),l,csswa(l),csswb(l),sn(l),mczrat,sumc(l)
     %,suma(l),sumb(l),summax(l),aa(l),ab(l)
 16        continue
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           lsa=csswa(i)
           lsb=csswb(i)
           lsmax=summax(i)
           lsuma=suma(i)
           lsumb=sumb(i)
           lsumc=sumc(i)
           laa=aa(i)
           lab=ab(i)
           k=0
         elseif(k.gt.0) then
           da=(dfra(i)-lda)/(k+1)
           db=(dfrb(i)-ldb)/(k+1)
           ta=(cswa(i)-lta)/(k+1)
           tb=(cswb(i)-ltb)/(k+1)
           sa=(csswa(i)-lsa)/(k+1)
           sb=(csswb(i)-lsb)/(k+1)
           tsmax=(summax(i)-lsmax)/(k+1)
           tsuma=(suma(i)-lsuma)/(k+1)
           tsumb=(sumb(i)-lsumb)/(k+1)
           tsumc=(sumc(i)-lsumc)/(k+1)
           aaa=(aa(i)-laa)/(k+1)
           aab=(ab(i)-lab)/(k+1)
           if(ok) then
             dfra(i-k-1)=lda
             dfrb(i-k-1)=ldb
             cswa(i-k-1)=lta
             cswb(i-k-1)=ltb
             csswa(i-k-1)=lsa
             csswb(i-k-1)=lsb
             summax(i-k-1)=lsmax
             suma(i-k-1)=lsuma
             sumb(i-k-1)=lsumb
             sumc(i-k-1)=lsumc
             aa(i-k-1)=laa
             ab(i-k-1)=lab
             ok=.false.
           endif
           do 17 l=(i-k),(i-1)
             dfra(l)=dfra(l-1)+da
             dfrb(l)=dfrb(l-1)+db
             cswa(l)=cswa(l-1)+ta
             cswb(l)=cswb(l-1)+tb
             csswa(l)=csswa(l-1)+sa
             csswb(l)=csswb(l-1)+sb
             summax(l)=summax(l-1)+tsmax
             suma(l)=suma(l-1)+tsuma
             sumb(l)=sumb(l-1)+tsumb
             sumc(l)=sumc(l-1)+tsumc
             IF(aa(i).LT.-5.8.and.aa(i).GT.-6.2) then
               aa(l)=serr
               ab(l)=serr
             ELSEIF(laa.LT.-5.8.and.laa.GT.-6.2) then
               aa(l)=serr
               ab(l)=serr
             ELSEIF(aa(i).LT.-8.8.and.aa(i).GT.-9.2) then
               aa(l)=serr
               ab(l)=serr
             ELSEIF(laa.LT.-8.8.and.laa.GT.-9.2) then
               aa(l)=serr
               ab(l)=serr
             else
               aa(l)=aa(l-1)+aaa
               ab(l)=ab(l-1)+aab
             endif
        write(2,3)cdate(l),dfra(l),dfrb(l),x,cswa(l),cswb(l),aau(l),
     %sncz(l),c(l),l,csswa(l),csswb(l),sn(l),mczrat,sumc(l)
     %,suma(l),sumb(l),summax(l),aa(l),ab(l)
 17        continue
        write(2,3)cdate(i),dfra(i),dfrb(i),x,cswa(i),cswb(i),aau(i),
     %sncz(i),c(i),i,csswa(i),csswb(i),sn(i),mczrat,sumc(i)
     %,suma(i),sumb(i),summax(i),aa(i),ab(i)
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           lsa=csswa(i)
           lsb=csswb(i)
           lsmax=summax(i)
           lsuma=suma(i)
           lsumb=sumb(i)
           lsumc=sumc(i)
           laa=aa(i)
           lab=ab(i)
           k=0
         else
           lda=dfra(i)
           ldb=dfrb(i)
           lta=cswa(i)
           ltb=cswb(i)
           lsa=csswa(i)
           lsb=csswb(i)
           lsmax=summax(i)
           lsuma=suma(i)
           lsumb=sumb(i)
           lsumc=sumc(i)
           laa=aa(i)
           lab=ab(i)
           ok=.false.
        write(2,3)cdate(i),dfra(i),dfrb(i),x,cswa(i),cswb(i),aau(i),
     %sncz(i),c(i),i,csswa(i),csswb(i),sn(i),mczrat,sumc(i)
     %,suma(i),sumb(i),summax(i),aa(i),ab(i)
         endif
 18     continue
        if(k.gt.0) then
          do 19 i=(n-(k-1)),n
            cdate(i)=999999
 19       continue
        endif
        open(unit=78,file=lasfil,status='unknown')
cccccccccccccccccccc  New "last day"  cccccccccccccccccccccccccccccc
        write(78,*)'  Last day with n>200 retrieved coeffcients:',c(z2)
        write(78,20)cdate(z2),dfra(z2),dfrb(z2),cswa(z2),cswb(z2),
     %csswa(z2),csswb(z2),sumc(z2),suma(z2),sumb(z2),summax(z2),aa(z2),
     %ab(z2)
        write(78,*)'================================================='
        write(78,*)'  Last day with n>nlim retrieved coeffcients:',c(z)
        write(78,20)cdate(z),dfra(z),dfrb(z),cswa(z),cswb(z),
     %csswa(z),csswb(z),sumc(z),suma(z),sumb(z),summax(z),aa(z),ab(z)
cccccccccccccccccccc  New "last day"  cccccccccccccccccccccccccccccc
 20     format(i8,2f10.6,f10.2,f10.6,f10.2,f10.6,4f10.4,2f12.5)
 21     format(a1,i8,3f10.2,f10.6,f10.2,f10.6)
        close(unit=2)
        close(unit=78)
      else
        open(unit=78,file=lasfil,status='old',ERR=92)
        read(78,*)
        read(78,*) date,lda,ldb,lta,ltb,lsa,lsb,lsumc,lsuma,lsumb,lsmax,
     %laa,lab
        WRITE(6,*)' '
        WRITE(6,*)' '
        write(6,*)'____________________________________________________'
        write(6,*)' Last day and coefficients:'
        write(6,21) ' ',date,lda,ldb,lta,ltb,lsa,lsb
        close(unit=78)
        open(unit=78,file=lasfil,status='unknown')
        write(78,*)'  "One-fit" retrieved coeffcients:'
        write(78,20) 00000000,dfra(z),dfrb(z),cswa(z),cswb(z),
     %csswa(z),csswb(z),sumc(z),suma(z),sumb(z),summax(z),aa(z),ab(z)
        write(78,*)'================================================='
        write(78,*)'  Last NDR day with retrieved DRa coeffcient:'
        write(78,20)drdate,lasdra,dfrb(z),cswa(z),cswb(z),
     %csswa(z),csswb(z),sumc(z),suma(z),sumb(z),summax(z),aa(z),ab(z)
        close(unit=78)
      endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      open(unit=1,file=dirfil,status='old')
      dayfil='swfcgday.asc'
      open(unit=4,file=dayfil,status='unknown')
      posfil='posfcg.asc'
      open(unit=9,file=posfil,status='unknown')
      OPEN(UNIT=8,FILE=badfil,STATUS='unknown')
      IF(site.eq.2) then
      write(4,24)'   date      tsw      csw    tswfcg     dif     cdif
     % diffcg     difr     cdifr  tswn  cswn  difn cdifn  dayn     ssw
     %    cssw   sswfcg   sswn cssn      SolX        CSolX     n     cn'
      elseIF(site.eq.3) then
      write(4,24)'   date      tsw      csw    tswfcg     dif     cdif
     % diffcg     difr     cdifr  tswn  cswn  difn cdifn  dayn     ssw
     %    cssw   sswfcg   sswn cssn       Aux         ****     n     cn'
      else
      write(4,24)'   date      tsw      csw    tswfcg     dif     cdif
     % diffcg     difr     cdifr  tswn  cswn  difn cdifn  dayn     ssw
     %    cssw   sswfcg   sswn cssn      SWup        CSWup     n     cn'
      endif
 24   format(a185)
      IF(site.eq.2) then
      write(9,27) headr2
      write(8,27) headr2
      elseIF(site.eq.3) then
      write(9,27) headr3
      write(8,27) headr3
      else
      write(9,27) headr1
      write(8,27) headr1
      endif


cc    read in interpolated albedo coefficients if site=1

      IF(site.eq.1) then
        OPEN(UNIT=54,FILE='alb_cof2.asc',STATUS='old')
 330    READ(54,*,END=335) ldate,x,y,w,xxx,i,k,xx2,xx3
        do albn=1,dyn
          IF(ldate.eq.cdate(albn)) then
            alba(albn)=y
            albb(albn)=w
            albd(albn)=x
            devalb(albn)=xxx
            ma(albn)=xx2
            fa(albn)=xx3
            GO TO 330
          endif
        end do
      endif

 335  CLOSE(UNIT=54)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 25   read(1,26,end=100) tmpfil
 26   format(a12)
      m=index(tmpfil,'.')

cccccccccccccccccccccc one fit end on last good NDR day  ccccccccccccccc
cc      READ(tmpfil(1:m-1),*) ldrdate
cc      IF(ldrdate.gt.drdate) GO TO 100
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      infil=tmpfil(1:m)//'cos'

 109    FORMAT(a12,2f12.5,i8,2x,4i8)

      open(unit=2,file=infil,status='old')
      read(2,52,err=50) tmplin,(xhdr(i),i=1,h)
      outfil=infil(1:m)//'swf'
      open(unit=3,file=outfil,status='unknown')
      clrfil=tmpfil(1:m)//'clr'
 27   format(a217,5x,a5,50(6x,a5))
      write(6,*)' Infile= ',infil,'  Outfile= ',outfil
        dtsw=0.d0
        dcsw=0.d0
        dswfcg=0.d0
        dssw=0.d0
        dcssw=0.d0
        dsswfc=0.d0
        ddif=0.d0
        dcdif=0.d0
        ddiffc=0.d0
        ddifr=0.d0
        dcdr=0.d0
        dalb=0.d0
        dcalb=0.d0
        tswn=0
        sswn=0
        cssn=0
        difn=0
        cswn=0
        cdifn=0
        dayn=0
        an=0
        can=0
      do nclr=1,1450
        clrtim(nclr)=0
      end do
      OPEN(UNIT=55,FILE=clrfil,STATUS='old')
      nclr=1
      READ(55,*,ERR=28)
      do nclr=1,1450
        READ(55,*,END=28) clrdat,clrhr,clrmn
        clrtim(nclr)=clrhr*100+clrmn
      end do
 28   nclr=nclr-1
      CLOSE(UNIT=55)

 30   read(2,31,end=50) zDATE,ztim,ldate,ltim,cosz,scz,au,tsw,dif,DIR,
     %swu,tflg,dflg,rflg,(xtr(i),i=1,h)
 31   format(i8,i6,2x,i8,i6,3f10.6,3f10.1,f12.5,3i5,30f11.4)
      if(cosz.lt.0.d0.and..not.ngt) go to 30
      tdate=ldate
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc      Check cdate and date match here for daily coefficients
      n=1
cc      if(.not.dyfil) go to 33
 32   continue
      if(cdate(n).eq.ldate) then
        go to 33
      elseif(cdate(n).gt.ldate) then
        write(6,*)'swclrfcg Cannot match file & coeff dates: ',cdate(n)
     %,ldate
        open(unit=76,file='swnulclr.bat',access='APPEND')
        write(76,38)'del ',outfil
        close(unit=76)
        go to 50
      elseif(cdate(n).eq.0.and.dyfil) then
        go to 50
      else
        n=n+1
        go to 32
      endif
 33   continue

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc   calculate linear interpolation point for clear diffuse ratio

      if(.not.dyfil) then
         dfra(1)=dfra(n)
         alba(1)=alba(n)
         albb(1)=albb(n)
         albd(1)=albd(n)
         n=1
      endif

      lcosz=0.0001d0
      lcdifr=dfra(n)*(lcosz**dfrb(n))
 34   continue
       lcosz=lcosz+0.0001d0
       cdifr=dfra(n)*(lcosz**dfrb(n))
       cslope=(cdifr-lcdifr)/0.0001d0
       lslope=(cdifr-1.d0)/lcosz
       if(lslope.ge.cslope) then
         lcdifr=cdifr
         go to 34
       endif

      IF(site.eq.2) then
      write(3,*)'   Date      DFRa    DFRb     CSWa     CSWb     CSSWa
     %  CSSWb    SCORc    SCORa    SCORb      SolXa       SolXb     AvgA
     %U        Lat       Long'
      write(3,29)cdate(n),dfra(n),dfrb(n),cswa(n),cswb(n),csswa(n),
     %csswb(n),sumc(n),suma(n),sumb(n),aa(n),ab(n),aau(n),lat,long
      elseIF(site.eq.1) then
      write(3,*)'   Date      DFRa    DFRb     CSWa     CSWb     CSSWa
     %  CSSWb    SCORc    SCORa    SCORb       Alba        Albb     AvgA
     %U        Lat       Long'
        IF(alba(n).LT.-8.0.and.albd(n).gt.0.0) then
      write(3,29)cdate(n),dfra(n),dfrb(n),cswa(n),cswb(n),csswa(n),
     %csswb(n),sumc(n),suma(n),sumb(n),alba(n),albb(n),aau(n),
     %lat,long
        else
      write(3,29)cdate(n),dfra(n),dfrb(n),cswa(n),cswb(n),csswa(n),
     %csswb(n),sumc(n),suma(n),sumb(n),(alba(n)+albd(n)),albb(n),aau(n),
     %lat,long
        endif
      else
      write(3,*)'   Date      DFRa    DFRb     CSWa     CSWb     CSSWa
     %  CSSWb    SCORc    SCORa    SCORb       ****        ****     AvgA
     %U        Lat       Long'
      write(3,29)cdate(n),dfra(n),dfrb(n),cswa(n),cswb(n),csswa(n),
     %csswb(n),sumc(n),suma(n),sumb(n),aa(n),ab(n),aau(n),lat,long
      endif
 29    FORMAT(i8,2f9.4,2(f9.1,f9.4),2f9.2,f9.4,2f12.5,f10.5,2f11.4)
      WRITE(3,*)' '

      IF(site.eq.2) then
      write(3,27) headr2,(xhdr(i),i=1,h)
      elseIF(site.eq.3) then
      write(3,27) headr3,(xhdr(i),i=1,h)
      else
      write(3,27) headr1,(xhdr(i),i=1,h)
      endif
      dayn=dayn+1

cc      IF(swu.gt.0.0.and.site.ge.2) then
        alb=swu
cc      elseIF(tsw.gt.0.0.and.swu.gt.0.0.and.site.eq.1) then
cc        alb=swu/tsw
cc        IF(alb.gt.5.0) alb=serr
cc      else
cc        alb=serr
cc      endif
 35   continue

      ssw=rerr
cc
cc    for adjusted  csw, cssw, calb, set up "uz" and "ucosz" here
cc    be careful not to use "ucosz" for difrat
cc    for cdif, use cdifr*adjcsw for SZA<=90, cdif=adjcsw for sza>90
cc      NOTE: CosZ = -0.052336 is approximately a SZA of 93 degrees
cc
      sza=ACOS(cosz)
      sza=sza*180.0/pi
      IF(sza.ge.88.0.and.sza.le.90.0) then
        uz=88.0+(sza-88.0)*incr1
        ucosz=dCOS(uz*pi/180.0)
      ELSEIF(sza.gt.90.0.and.sza.le.93.0) then
        uz=adjz90+(sza-90.0)*incr2
        ucosz=dCOS(uz*pi/180.0)
      else
        ucosz=cosz
        uz=sza
      endif

      if(ucosz.ge.0.d0) then

cccccccccc  test for SSW  ccccccccccccccccccccccccccccccccc
        IF(dif.GT.-5.0.and.dir.GT.-5.0) then
          ssw=dif+dir
          sflg=0
        ELSEIF(tsw.gt.0.0.and.cosz.gt.0.100.and.sumb(n).GT.-89.0) then
          IF((sumc(n)+suma(n)*cosz**sumb(n)).le.summax(n)) then
            IF(tsw/csw.le.1.0000) then
              ssw=tsw-((sumc(n)+suma(n)*cosz**sumb(n))*tsw/csw)
            else
              ssw=tsw-((sumc(n)+suma(n)*cosz**sumb(n))*1.000)
            endif
          else
            IF(tsw/csw.le.1.0000) then
              ssw=tsw-summax(n)*tsw/csw
            else
              ssw=tsw-summax(n)*1.000
            endif
          endif
          sflg=1
      write(8,39)zdate,ztim,ldate,ltim,CosZ,Au,tsw,csw,tswfcg,dif,cdif,
     %diffcg,dir,cdir,difr,cdifr,ssw,cssw,sswfcg,sflg,tflg,dflg,rflg,
     %clrflg,alb,calb
        else
          ssw=-999.0
          sflg=-1
        endif

        IF(swrlim.lt.0.02) then
          WRITE(6,*)' in swclrfcg, before SSW test, swrlim=',swrlim
          pause
        endif

        IF(tsw.gt.0.0.and.ssw.gt.0.0) then
          IF(cosz.gt.0.25.and.ABS((tsw/csw)-(ssw/cssw)).gt.swrlim) then
            sflg=-2
      write(8,39)zdate,ztim,ldate,ltim,CosZ,Au,tsw,csw,tswfcg,dif,cdif,
     %diffcg,dir,cdir,difr,cdifr,ssw,cssw,sswfcg,sflg,tflg,dflg,rflg,
     %clrflg,alb,calb,swrlim
            ssw=-999.0
            dif=-999.0
            dir=-999.0
          elseIF(cosz.gt.0.09.and.ABS((tsw/csw)-(ssw/cssw)).gt.2*swrlim)
     % then
            sflg=-3
      write(8,39)zdate,ztim,ldate,ltim,CosZ,Au,tsw,csw,tswfcg,dif,cdif,
     %diffcg,dir,cdir,difr,cdifr,ssw,cssw,sswfcg,sflg,tflg,dflg,rflg,
     %clrflg,alb,calb,swrlim
            ssw=-999.0
            dif=-999.0
            dir=-999.0
          elseIF(cosz.gt.0.12.AND.dif.gt.0.d0) then
            IF((tsw/csw).gt.0.9.and.(dif/tsw).gt.0.9) then
              sflg=-4
      write(8,39)zdate,ztim,ldate,ltim,CosZ,Au,tsw,csw,tswfcg,dif,cdif,
     %diffcg,dir,cdir,difr,cdifr,ssw,cssw,sswfcg,sflg,tflg,dflg,rflg,
     %clrflg,alb,calb,tsw/csw
              ssw=-999.0
              dif=-999.0
              dir=-999.0
            endif
          endif
        endif
        if(ssw.gt.0.d0) then
          sswn=sswn+1
          dssw=dssw+ssw
          IF(cssw.gt.0.0) then
            sswfcg=ssw-cssw
            dsswfc=dsswfc+sswfcg
          endif
        else
          sswfcg=rerr
        endif
cccccccccc  END test for SSW  ccccccccccccccccccccccccccccccccc

        IF(dyfil) then
          csw=cswa(n)*(ucosz**cswb(n))
        ELSEIF(au.gt.0.0) then
          csw=(cswa(n)/(au**2))*(ucosz**cswb(n))
        else
          csw=cswa(n)*(ucosz**cswb(n))
        endif
        dcsw=dcsw+csw
        cswn=cswn+1
        IF(csswb(n).GT.-8.9) then
          IF(dyfil) then
            cssw=csswa(n)*(ucosz**csswb(n))
          ELSEIF(au.gt.0.0) then
            cssw=(csswa(n)/(au**2))*(ucosz**csswb(n))
          else
            cssw=csswa(n)*(ucosz**csswb(n))
          endif
          dcssw=dcssw+cssw
          cssn=cssn+1
        else
          cssw=-999.0
        endif

ccccccccccccccc Calculate clear SWup  or  Xvar  ccccccccccccccccccccccccc

cccccccc calculate absolute morning-afternoon allsky albedo difference  ccccccccccc
        IF(site.eq.1) then
          IF(ma(n).gt.0.0.and.fa(n).gt.0.0) then
            xx2=ABS(ma(n)-fa(n))
          else
            xx2=-999.0
          endif

cccccccc test data for snow melt/accumulation, use measured albedo if appropriate  cccccccccc
          IF(swu.gt.5.0.and.tsw.gt.10.0.and.devalb(n).gt.0.025.and.
     %xx2.GT.0.06) then
            xxx=swu/tsw
          elseIF(swu.gt.5.0.and.tsw.gt.10.0.and.devalb(n).LT.-1.0.and.
     %xx2.GT.0.1.and.albd(n).gt.0.35) then
            xxx=swu/tsw
          elseIF(swu.gt.5.0.and.tsw.gt.10.0.and.devalb(n).LT.-1.0.and.
     %xx2.GT.0.25) then
            xxx=swu/tsw
ccccccccccc  20140514 added ".and.xx2.le.0.2" to following test  ccccccccccccccc
cc          elseIF(ucosz.ge.0.0.and.alba(n).GT.-8.0)then
          elseIF(ucosz.ge.0.0.and.alba(n).GT.-8.0.and.xx2.le.0.2)then
            xxx=albd(n)+alba(n)+albb(n)*ucosz
          ELSEIF(swu.gt.5.0.and.tsw.gt.10.0) then
            xxx=swu/tsw
          else
            xxx=rerr
          endif
          IF(xxx.gt.1.0) xxx=1.0
          IF(csw.gt.0.0.and.xxx.gt.0.0) then
            calb=xxx*csw
          else
            calb=-999.0
          endif

ccccccc if no fitting (non-solar or no data), set clear variable to "-9999.0"  ccccccccccc
        elseIF(aa(n).LT.-5.8) then
          calb=rerr

ccccccc solar variable other than SWup ccccccccccccccccccccccccccccc
        ELSEIF(site.eq.2) then
          IF(dyfil) then
            calb=(aa(n))*(ucosz**ab(n))
          ELSEIF(au.gt.0.0) then
            calb=(aa(n)/(au**2))*(ucosz**ab(n))
          else
            calb=(aa(n))*(ucosz**ab(n))
          endif
        endif
ccccccccccccccc END Calculate clear SWup  or  Xvar  ccccccccccccccccccccccccc

        if(calb.gt.0.d0)then
cc          IF(site.eq.2) then
            can=can+1
            dcalb=dcalb+calb
cc          ELSEIF(cosz.gt.(0.2)) then
cc            can=can+1
cc            dcalb=dcalb+calb
cc          endif
        endif
        if(tsw.gt.0.d0) then
          tswfcg=tsw-csw
          tswn=tswn+1
          dtsw=dtsw+tsw
          dswfcg=dswfcg+tswfcg
        else
          tswfcg=rerr
        endif

        if(alb.gt.0.d0)then
cc          IF(site.eq.2) then
            an=an+1
            dalb=dalb+alb
cc          ELSEIF(cosz.gt.(0.2)) then
cc            an=an+1
cc            dalb=dalb+alb
cc          endif
        endif
      else
        csw=rerr
        tswfcg=rerr
        cssw=rerr
        sswfcg=rerr
        calb=-999.0
      endif

      if(cosz.ge.lcosz) then
        cdifr=dfra(n)*(cosz**dfrb(n))
        cdif=cdifr*csw
        IF(cssw.gt.0.0) then
          cdir=cssw-cdif
        else
          cdir=-999.0
        endif
        IF(cdir.lt.0.0) cdir=0.0
        dcdif=dcdif+cdif
        dcdr=dcdr+cdifr
        cdifn=cdifn+1
        diffcg=rerr
        if(dif.gt.0.d0.and.ssw.gt.0.d0.and.(dif/ssw).lt.10.0) then
          difr=dif/ssw
          diffcg=dif-cdif
          difn=difn+1
          ddif=ddif+dif
          ddiffc=ddiffc+diffcg
          ddifr=ddifr+difr
        elseif(dif.gt.0.d0.and.tsw.gt.0.d0.and.(dif/tsw).lt.10.0) then
          difr=dif/tsw
          diffcg=dif-cdif
          difn=difn+1
          ddif=ddif+dif
          ddiffc=ddiffc+diffcg
          ddifr=ddifr+difr
        else
          difr=serr
          diffcg=rerr
        endif
      elseif(cosz.lt.lcosz.and.cosz.gt.0.d0) then
        cdifr=(((dfra(n)*(lcosz**dfrb(n)))-1.0)*cosz/lcosz)+1.0
        cdif=cdifr*csw
        IF(cssw.gt.0.0) then
          cdir=cssw-cdif
        else
          cdir=-999.0
        endif
        IF(cdir.lt.0.0) cdir=0.0
        cdifn=cdifn+1
        dcdif=dcdif+cdif
        dcdr=dcdr+cdifr
        diffcg=rerr
        if(dif.gt.0.d0.and.ssw.gt.0.d0.and.(dif/ssw).lt.10.0) then
          difr=dif/ssw
          diffcg=dif-cdif
          difn=difn+1
          ddif=ddif+dif
          ddiffc=ddiffc+diffcg
          ddifr=ddifr+difr
        elseif(dif.gt.0.d0.and.tsw.gt.0.d0.and.(dif/tsw).lt.10.0) then
          difr=dif/tsw
          diffcg=dif-cdif
          difn=difn+1
          ddif=ddif+dif
          ddiffc=ddiffc+diffcg
          ddifr=ddifr+difr
        else
          difr=serr
          diffcg=rerr
        endif
      elseif(cosz.le.0.d0.and.ucosz.ge.0.d0) then
        cdifr=1.0
        cdif=csw
        IF(cssw.gt.0.0) then
          cdir=0.0
        else
          cdir=-999.0
        endif
        IF(cdir.lt.0.0) cdir=0.0
        cdifn=cdifn+1
        dcdif=dcdif+cdif
        dcdr=dcdr+cdifr
        diffcg=rerr
        if(dif.gt.0.d0.and.ssw.gt.0.d0.and.(dif/ssw).lt.10.0) then
          difr=dif/ssw
          diffcg=dif-cdif
          difn=difn+1
          ddif=ddif+dif
          ddiffc=ddiffc+diffcg
          ddifr=ddifr+difr
        elseif(dif.gt.0.d0.and.tsw.gt.0.d0.and.(dif/tsw).lt.10.0) then
          difr=dif/tsw
          diffcg=dif-cdif
          difn=difn+1
          ddif=ddif+dif
          ddiffc=ddiffc+diffcg
          ddifr=ddifr+difr
        else
          difr=serr
          diffcg=rerr
        endif
      else
        difr=serr
        cdifr=serr
        cdif=rerr
        diffcg=rerr
        cdir=rerr
      endif
      clrflg=0
      IF(nclr.gt.0) then
        do q=1,nclr
          IF(clrtim(q).eq.ltim) then
            clrflg=1
            GO TO 37
          endif
        end do
      endif
 37   continue
      write(3,39)zdate,ztim,ldate,ltim,CosZ,Au,tsw,csw,tswfcg,dif,cdif,
     %diffcg,dir,cdir,difr,cdifr,ssw,cssw,sswfcg,sflg,tflg,dflg,rflg,
     %clrflg,alb,calb,(xtr(i),i=1,h)
 38   format(a4,a12)
 39   format(i8,i6,2x,i8,i6,2f10.6,8f9.1,2f9.5,3f9.1,5i5,2f12.5,50f11.4)
      if(tswfcg.gt.30.d0) then
      write(9,39)zdate,ztim,ldate,ltim,CosZ,Au,tsw,csw,tswfcg,dif,cdif,
     %diffcg,dir,cdir,difr,cdifr,ssw,cssw,sswfcg,sflg,tflg,dflg,rflg,
     %clrflg,alb,calb
      endif
 40   read(2,31,end=50) zdate,ztim,ldate,ltim,cosz,scz,au,tsw,dif,DIR,
     %swu,tflg,dflg,rflg,(xtr(i),i=1,h)
      dayn=dayn+1
cc        IF(swu.gt.0.0.and.site.ge.2) then
          alb=swu
cc        elseIF(tsw.gt.0.0.and.swu.gt.0.0.and.site.eq.1) then
cc          alb=swu/tsw
cc          IF(alb.gt.5.0) alb=serr
cc        else
cc          alb=serr
cc        endif
      if(cosz.lt.0.d0.and..not.ngt) go to 40
      ssw=rerr
      go to 35

 50   continue
      close(unit=3)
      close(unit=2)
        if(tswn.gt.0) then
          dtsw=dtsw/tswn
          dswfcg=dswfcg/tswn
        endif
        if(sswn.gt.0) then
          dssw=dssw/sswn
        endif
        if(cssn.gt.0) then
          dsswfc=dsswfc/cssn
          dcssw=dcssw/cssn
        endif
        if(cswn.gt.0) then
          dcsw=dcsw/cswn
        endif
        if(difn.gt.0) then
          ddif=ddif/difn
          ddiffc=ddiffc/difn
          ddifr=ddifr/difn
        endif
        if(cdifn.gt.0) then
          dcdif=dcdif/cdifn
          dcdr=dcdr/cdifn
        endif
        if(an.gt.0) then
          dalb=dalb/an
        else
          dalb=serr
        endif
        if(can.gt.0) then
          dcalb=dcalb/can
        else
          dcalb=rerr
        endif
        if(dyfil) then
          write(4,51)cdate(n),dtsw,dcsw,dswfcg,ddif,dcdif,ddiffc,ddifr,
     %dcdr,tswn,cswn,difn,cdifn,dayn,dssw,dcssw,dsswfc,sswn,cssn,dalb,
     %dcalb,an,can
        else
          write(4,51)tdate,dtsw,dcsw,dswfcg,ddif,dcdif,ddiffc,ddifr,
     %dcdr,tswn,cswn,difn,cdifn,dayn,dssw,dcssw,dsswfc,sswn,cssn,dalb,
     %dcalb,an,can
        endif
 51     format(i8,6f9.1,2f9.5,5i6,3f9.1,2i6,2f12.5,2i6)
 52     FORMAT(A117,30(6x,a5))
      go to 25

 91   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'   <<<<<< Error opening coefficient file.>>>>>>>'
      write(6,*)'The file "swclreq3.asc" must be in the same directory'
      write(6,*)'as the executable file "swclrfcg.exe".'
      write(6,*)'?????????????????????????????????????????????????????'
      go to 100

 92   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'<<<<<< Error opening last day coefficient file >>>>>>>'
      write(6,*)'The file "swlastdy.cfw" must be in the same directory'
      write(6,*)'as the executable file. This file contains the fit'
      write(6,*)'coefficients for the last "good" day of the previous'
      write(6,*)'run. '
      write(6,*)' '
      write(6,*)'NOTE: Without this file the current run results '
      write(6,*)'      are INVALID! End this run and try again. '
      write(6,*)' '
      write(6,*)'If this is the first run of the code for a site, do '
      write(6,*)'you want to have a generic "swlastdy.cfw" file'
      write(6,*)'generated for you so the code will run? '
      write(6,*)'Answer y/n for yes or no: '
      write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'Answer: '
      READ(*,93) ans
 93   FORMAT(a1)
      IF(ans.eq.'y'.or.ans.eq.'Y') then
        OPEN(UNIT=59,FILE='swlastdy.cfw',STATUS='unknown')
        WRITE(59,*)'  Last day with n>nlim retrieved coeffcients:
     % 702'
        WRITE(59,*)'20010101  0.127 -0.734   1084.5  1.187   1083.2  1.1
     %89   -0.315    1.99   -0.192    3.6    -6.0    -6.0'
        CLOSE(UNIT=59)
      endif
      go to 100

 100  continue
      CLOSE(UNIT=1)
      CLOSE(UNIT=4)
      CLOSE(UNIT=8)
      CLOSE(UNIT=9)
      return
      end
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine clrndr(infil,adra,orgdrb,cswa,cswb,maxdif,czlim,mfac,
     %newdra,j)
      integer zDATE,ztim,ldate,ltim,n,i,k,j
      REAL*8 cosz,au,tsw,dif,csw,diflim,ndr,ndrlim,dir,ssw
      REAL*8 minndr,maxdif,czlim,newdra,drb,adra,mfac
      REAL*8 mdr(1450),avg,stdv,sncz
      REAL orgdrb,cswa,cswb
      CHARACTER*12 infil

      minndr=9.0
      do i=1,1450
       mdr(i)=9.0
      enddo
      drb=orgdrb+0.04
      n=0
      k=0

      OPEN(UNIT=11,FILE=infil,STATUS='old')
      READ(11,*)
 10   READ(11,12,END=50) zDATE,ztim,ldate,ltim,cosz,sncz,au,tsw,dif,dir
 12   format(i8,i6,2x,i8,i6,3f10.6,3f10.1,f12.5,3i5,30f11.4)

      IF(cosz.lt.czlim) GO TO 10
      csw=(cswa/au**2)*(cosz**cswb)
      diflim=maxdif*(cosz**0.5)
      ssw=dif+dir
      IF(dif.gt.1.0.and.ssw.gt.1.0) then
        ndr=(dif/ssw)/(cosz**drb)
      elseIF(dif.gt.1.0.and.tsw.gt.1.0) then
        ndr=(dif/tsw)/(cosz**drb)
      endif
      ndrlim=((diflim*mfac)/csw)/(cosz**drb)
      n=n+1

cc      IF(dif.le.diflim.and.ndr.le.ndrlim.and.ndr.ge.0.05) then
      IF(dif.le.diflim.and.ndr.le.ndrlim.and.ndr.ge.0.04) then
        k=k+1
        mdr(k)=ndr
      endif
      GO TO 10

 50   CLOSE(UNIT=11)

      IF(k.gt.4) then
        call SORT(k,mdr)
      else
        minndr=9.0
        stdv=9.0
        GO TO 60
      endif
      j=k
      avg=0.0
cc      IF(k.gt.10)  k=10
      IF(k.gt.5)  k=5
        do i=1,k
          avg=avg+mdr(i)
        end do

 55   avg=avg/(k*1.0)
      IF(avg.lt.1.0) then
        stdv=0.0
        do i=1,k
          stdv=stdv + (mdr(i)-avg)**2
        end do
        stdv=SQRT(stdv/k)
        minndr=avg
      else
        minndr=9.0
        stdv=9.0
      endif

 60     IF(stdv.lt.0.01) then
           newdra=minndr
c        ELSEIF(minndr.lt.0.2.and.minndr.gt.maxdra) then
c           newdra=maxdra
        else
           newdra=9.0
        endif
        rstdv=stdv
      return
      end

C======================================================================
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE DOY2DATE(year,doy,date)
C     subroutine for converting DOY to YYYYMMDD format
      integer year,doy,date,MONTHS(12),i,num

      DATA MONTHS /31,28,31,30,31,30,31,31,30,31,30,31/

      date=0

c     Check for leap year [for "yr" = 4-digit year]
      IF(MOD(year,4).ne.0) then
        months(2)=28
      ELSEIF(MOD(year,400).eq.0) then
        months(2)=29
      ELSEIF(MOD(year,100).eq.0) then
        months(2)=28
      ELSE
        months(2)=29
      endif

c Determine month and day from doy

      num=doy
      do i=1,12
        num=num-months(i)
        if(num.le.0) then
          num=num+months(i)
          go to 10
        endif
      enddo

 10   date=year*10000+i*100+num


      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C======================================================================
C======================================================================
      SUBROUTINE EPHEMS (LAT,LONG,DAY,MONTH,YEAR,TIME,P5,P6,P7,P8,
     *  A,R,Z,LHA,DEC)
C
C       SUBROUTINE TO COMPUTE SOLAR POSITION
C
C      WRITTEN BY WAYNE H WILSON, JR.
C      VISIBILITY LABORATORY
C      SCRIPPS INSTITUTION OF OCEANOGRAPHY
C      UNIVERSITY OF CALIFORNIA, SAN DIEGO
C      LA JOLLA, CALIFORNIA, 92093
C      PH. (714)-294-5534
C      25 FEB 1980
C
C
C
C  %%%%%%%%%%%%%%%%%  NOTE: KNOWN BUG PATCH %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      Wilson's code produces the incorrect output in the TIME is set
C      to between 100.00 and 100.59.  The following patch loop in the
C      MAIN program will avoid this problem. Declare REAL*8 gt,x(5)
C
CCCCCCCCCCCCCCCCCCCCCC  PATCH LOOP  CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      IF(time.gt.59.59d0.and.time.lt.101.00d0.and.ip6.eq.0) then
C        gt=101.01d0
C        CALL EPHEMS(SLAT,SLON,IDAY,IMONTH,IYEAR,gt,IP5,IP6,IP7,IP8,
C     +            AZANG,RADAU,SOLZA,HRANG,DECANG)
C        x(1)=azang
C        x(2)=radau
C        x(3)=solza
C        x(4)=decang
C        gt=59.59d0
C        CALL EPHEMS(SLAT,SLON,IDAY,IMONTH,IYEAR,gt,IP5,IP6,IP7,IP8,
C     +            AZANG,RADAU,SOLZA,HRANG,DECANG)
C        gt=(time-100.d0)+.01
C        gt=gt/0.61d0
C        azang=((x(1)-azang)*gt)+azang
C        radau=((x(2)-radau)*gt)+radau
C        solza=((x(3)-solza)*gt)+solza
C        decang=((x(4)-decang)*gt)+decang
C      else
C        CALL EPHEMS(SLAT,SLON,IDAY,IMONTH,IYEAR,TIME,IP5,IP6,IP7,IP8,
C     +            AZANG,RADAU,SOLZA,HRANG,DECANG)
C      endif
C
C
C
C
      IMPLICIT REAL*8(P)
      IMPLICIT INTEGER*4(I-N)
      REAL*8 LAT,LONG,TIME
      REAL*8 DARSIN,DARCOS,DTAN
      REAL*8 A, R, Z, HA, DEC, LHA
      REAL*8 DEGRD,PI,TOHMS,ONE
      INTEGER*4 P5,P6,P7,P8,P10
      INTEGER*4 DAY,MONTH,YEAR,g
C
C     LAT - LATITUDE (IN DEGREES AND FRACTION OF DEGREES), P19
C     LONG - LONGITUDE (IN DEGREES AND FRACTIONS OF DEGREES),P20
C     DAY - DAY OF THE MONTH,P16
C     MONTH - MONTH OF YEAR,P17
C     YEAR - YEAR,P18
C     TIME - TIME (HHMM.SS) LOCAL STANDARD LST OR GMT, P4
C     P5 - TIME TYPE (0 - LST, 1 - GMT)
C     P6 - APPARENT NOON CALCULATIONS (1-YES, 0-NO)
C     P7 - PRINT (0-NONE, 1-DATE, 2-DATA, 3 - NA COMPAR)
C     P8 - APPROXIMATION (0-FULL, 1-NO PLANETS, 2-NO LUNAR)
C
C      RETURNED VARIABLES
C
C      A - AZIMUTH ANGLE(IN DEGREES AND FRACTION OF DEGREE)
C     R - RADIUS VECTOR
C     Z - ZENITH ANGLE(IN DEGREES AND FRACTION OF DEGREE)
C     LHA - LOCAL HOUR ANGLE (IN DEGREES)
C     TIME - APPARENT NOON IF P6 = 1 (LST OR GMT SEE P5)
C
C     P21 - TIME (IN FRACTIONS OF DAY)
C     P51 - GMT IN DEGREES
C     P53 - ZONE LONGITUDE
C
C
	DATA ONE/1.0D0/
C
C
      DTAN(P)=DSIN(P)/DCOS(P)
      DARSIN(P)=DATAN2(P,DSQRT(1.0D0-P**2))
      DARCOS(P) = PID2 - DARSIN(P)
C
C
C-----------------------------------------------------------------------
      g=1
C-----------------------------------------------------------------------
C
C
	PID2 = DATAN(1.0D0)*2.0D0
        PI = acos(-1.d0)
      DEGRD = PI/180.D0
C
C++++++++++++++++++++++++++++++++++++++++++++
C      P57 - (ET-UT), DIFFERENCE OF EPHEMERIS & UNIVERSAL TIME
      P57 = 0
C
C
      P16 = DAY
      P17 = MONTH
      P18 = YEAR
      P19 = LAT
      P20 = LONG
      P54 = 10
      P55 = 14
      P10 = P7
    1 CONTINUE
      P53 = 15.*AINT((7.5+ABS(LONG))/15.0)*DSIGN(1.d0,LONG)
      P11 = P53
      IF(P5.EQ.1) P11=0D0
      IF(P6.NE.1) GO TO 10
      P56 = (P54+P55)/2D0
      P51 = P56/24D0*360D0+P53
      g=g+1
      if(g.ge.100) go to 620
      GO TO 20
   10 CONTINUE
      P21 = (DMOD(TIME,1.d0)*100/3600+DMOD(AINT(TIME)/100.d0,1.d0)
     *  *100./60.  + AINT(AINT(TIME)/100.0))/24.0
      P51 = P21*360D0+P11
   20 CONTINUE
      I = P18
      J = P17+1
      IF(J.GT.3) GO TO 40
      J = J + 12
      I = I - 1
   40 CONTINUE
      P13 = INT(I*365.25) + INT(J*30.6) + P16
      P22 = P13 - 694038.5D0
      P11 = P18 + P17/100.D0 + P16/1.0D4
      IF(P11.LT.1900.0228D0) P22 = P22 + 1
      IF(P11.LT.1800.0228D0) P22 = P22+1
      P23 = (P51/360D0+P22+P57)/36525D0
      P22 = P23*36525D0
C
C      MEAN LONGITUDE - P24
C
      P11 = 279.69668D0
      P12 = 0.9856473354D0
      P13 = 3.03D-4
      P24 = P11 + DMOD(P12*P22,360D0) + P13*P23**2
      P24 = DMOD(P24,360D0)
C
C     MEAN ANOMALY - P25
C
      P11 = 358.47583D0
      P12 = 0.985600267D0
      P13 = -1.5D-4
      P14 = -3.D-6
      P25 = P11 + DMOD(P12*P22,360D0) + P13*P23**2 + P14*P23**3
      P25 = P11 + DMOD(P12*P22,360D0) + P13*P23**2 + P14*P23**3
      P25 = DMOD(P25,360D0)
C
C     ECCENTRICITY - P26
C
      P11 = 0.01675104D0
      P12 = -4.18D-5
      P13 = -1.26D-7
      P26 = P11 + P12*P23 + P13*P23**2
      P11 = P25*DEGRD
      P12 = P11
C
C     ECCENTRIC ANOMALY - P13 (TEMP)
C
  100 CONTINUE
      P13 = P12
      P12 = P11 + P26*DSIN(P13)
      IF(DABS((P12-P13)/P12).GT.1.0D-8) GO TO 100
      P13 = P12/DEGRD
C
C     TRUE ANOMALY - P27
C
      P27 =2.0D0*DATAN(DSQRT((1.0D0+P26)/(1.0D0-P26))*DTAN(P13/2.0D0
     *   *DEGRD))/DEGRD
      IF(DSIGN(1.0D0,P27).NE.DSIGN(1.0D0,DSIN(P13*DEGRD))) P27 = P27 + 1
      IF(DSIGN(1.0D0,P27).NE.DSIGN(1.0D0,DSIN(P13*DEGRD)))
     *  P27 = P27 + 180.0D0
      IF(P27.LT. 0.0D0) P27 = P27 + 360D0
C
C     RADIUS VECTOR - R
C
      R = 1.0D0 - P26*DCOS(P13*DEGRD)
C
C     ABERRATION - P29
C
      P29 = -20.47/R/3600.0
C
C      MEAN OBLIQUITY - P43
C
      P11 = 23.452294D0
      P12 = -0.0130125D0
      P13 = -1.64D-6
      P14 = 5.03D-7
      P43 = P11 + P12*P23 + P13*P23**2 + P14*P23**3
C
C     MEAN ASCENSION - P45
C
      P11 = 279.6909832D0
      P12 = 0.98564734D0
      P13 = 3.8708D-4
      P45 = P11 + DMOD(P12*P22,360D0) + P13*P23**2
      P45 = DMOD(P45,360D0)
C
C-----------------------------------------------------------------------
C
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
C
C     NUTATION AND LONGITUDE PERT
C
      IF(P8.GT.1) GO TO 300
C-----------------------------------------------------------------------
C
C      MOON'S MEAN ANOMALY - P28
      P11 = 296.104608D0
      P12 = 1325D0*360D0
      P13 = 198.8491083D0
      P14 = .00919167D0
      P15 = 1.4388D-5
      P28 = P11 + DMOD(P12*P23,360D0) + P13*P23 + P14*P23**2 +
     *  P15*P23**3
      P28 = DMOD(P28,360D0)
C
C     MEAN ELONGATION OF MOON - P30:
C
      P11 = 350.737486
      P12 = 1236D0 * 360D0
      P13 = 307.1142167D0
      P14 = -1.436D-3
      P30 = P11 + DMOD(P12*P23,360D0) + P13*P23 + P14*P23**2
      P30 = DMOD(P30,360D0)
C
C     MOON LONG OF ASCENDING NODE - P31:
C
      P11 = 259.183275D0
      P12 = -5D0 * 360D0
      P13 = -134.142008D0
      P14 = 2.0778D-3
      P31 = P11 + DMOD(P12*P23,360D0) + P13*P23 + P14*P23**2
      P31 = DMOD(P31,360D0)
C
C     MEAN LONG OF MOON - P32:
C
      P11 = 270.434164D0
      P12 = 1336D0 * 360D0
      P13 = 307.8831417D0
      P14 = -1.1333D-3
      P32 = P11 + DMOD(P12*P23,360D0) + P13*P23 + P14*P23**2
      P32 = DMOD(P32,360D0)
C
C     MOON PERTURBATION OF SUN LONG - P74:
C
      P74 = 6.454D0*DSIN(P30*DEGRD) + .013D0*DSIN(3D0*P30*DEGRD) +
     *  .177D0*DSIN((P30+P28)*DEGRD) - .424D0*DSIN((P30-P28)*DEGRD)
       P74 = P74 + 0.039D0*DSIN((3D0*P30-P28)*DEGRD)
     * - 0.064D0*DSIN((P30+P25)*DEGRD) + 0.172D0*DSIN((P30-P25)*DEGRD)
      P74 = P74/3600D0
	P33 = P74
C
C     NUTATION OF LONG - P34
C
      P34 = -(17.2327D0-.017D0*P23) * DSIN(P31*DEGRD) +
     *  .2088D0*DSIN(2D0*P31*DEGRD) - .2037D0*DSIN(2D0*P32*DEGRD)
      P34 = P34 - 1.2729D0*DSIN(2D0*P24*DEGRD) + .1261D0*DSIN(P28*DEGRD)
      P34 = P34/3600D0
C
C     NUTATION IN OBLIQUITY - P34:
C
      P35 = 9.210D0*DCOS(P31*DEGRD) + .5522D0*DCOS(2D0*P24*DEGRD) -
     *  .09D0*DCOS(2D0*P31*DEGRD) + .0889D0*DCOS(2D0*P32*DEGRD)
      P35 = P35/3600D0
C
C     INEQUALITIES OF LONG PERIOD - P36:
C
      P36 = .266D0*DSIN((31.8D0+119D0*P23)*DEGRD) +
     *((1.882D0-.016D0*P23)*DEGRD) * DSIN((57.24D0+150.27D0*P23)*DEGRD)
      P36 = P36 + .202D0*DSIN((315.6D0+893.3D0*P23)*DEGRD) +
     * 1.089D0*P23**2 + 6.4D0*DSIN((231.19D0+20.2D0*P23)*DEGRD)
      P36 = P36/3600D0
C
C     MOON MEAN ARGUMENT OF LATITUDE - P63:
C
      P11 = 11.250889D0
      P12 = 1342D0*360D0
      P13 = 82.02515D0
      P14 = .003211D0
      P63 = P11 + DMOD(P12*P23,360D0) + P13*P23 + P14*P23**2
C
C-----------------------------------------------------------------------
C
CBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
C
C     PERTURBATIONS DUE TO PLANETS - P33:
C
      IF(P8.GT.0) GO TO 300
C
C     VENUS:
C
C     MEAN ANOMALY OF VENUS - P37:
C
      P11 = 212.603222D0
      P12 = 162D0*360D0
      P13 = 197.803875D0
      P14 = 1.286D-3
      P37 = P11 + DMOD(P12*P23,360D0) + P13*P23 + P14*P23**2
      P37 = DMOD(P37,360D0)
      P11 = 4.838D0*DCOS((299.171D0+P37-P25)*DEGRD) +
     *  5.526D0*DCOS((148.5259D0+2D0*P37-2D0*P25)*DEGRD)
      P11 = P11 + 2.497D0*DCOS((316.5759D0+2D0*P37-3D0*P25)*DEGRD) +
     *   .666D0*DCOS((177.71D0+3D0*P37-3D0*P25)*DEGRD)
      P11 = P11 + 1.559D0*DCOS((345.4559D0+3D0*P37-4D0*P25)*DEGRD) +
     * 1.024D0*DCOS((318.15D0+3D0*P37-5D0*P25)*DEGRD)
      P70 = P11
      P33 = P33+P11/3600D0
C
C     PERT. OF LATITUDE OF SUN BY VENUS - P61:
C
      P61 = .21D0*DCOS((151.8D0+3D0*P37-4D0*P25)*DEGRD) +
     * .092D0*DCOS((93.7D0+P37-2D0*P25)*DEGRD) +
     * .067D0*DCOS((123D0+2D0*P37-3D0*P25)*DEGRD)
C
C     MARS:
C
C     MEAN ANOMALY OF MARS - P38:
C
      P11 = 319.529022D0
      P12 = 53D0*360D0
      P13 = 59.8592194D0
      P14 = 1.8083D-4
      P38 = P11 + DMOD(P12*P23,360D0) + P13*P23 + P14*P23**2
      P38 = DMOD(P38,360D0)
      P11 = .273D0*DCOS((217.7D0-P38+P25)*DEGRD) +
     *  2.043D0*DCOS((343.888D0-2D0*P38+2D0*P25)*DEGRD)
      P11 = P11 + 1.77D0*DCOS((200.4017D0-2D0*P38+P25)*DEGRD)+
     * .425D0*DCOS((338.88D0-3D0*P38+2D0*P25)*DEGRD)
      P11 = P11 + .5D0*DCOS((105.18D0-4D0*P38+3D0*P25)*DEGRD) +
     *  .585D0*DCOS((334.06D0-4D0*P38+2D0*P25)*DEGRD)
      P71=P11
      P33 = P33+P11/3600D0
C
C     JUPITER:
C
C     MEAN ANOMALY OF JUPITER - P39:
C
      P11 = 225.3225D0
      P12 = 8D0*360D0
      P13 = 154.583D0
      P39 = P11 + DMOD(P12*P23,360D0) + P13*P23
      P39 = DMOD(P39,360D0)
      P11 = 7.208D0*DCOS((179.5317D0-P39+P25)*DEGRD) +
     *  2.6D0*DCOS((263.2167D0-P39)*DEGRD)
      P11 = P11 + 2.731*DCOS((87.1450D0-2D0*P39+2D0*P25)*DEGRD)
      P11 = P11 + 1.61D0*DCOS((109.4933D0-2D0*P39+P25)*DEGRD) +
     *  .556D0*DCOS((82.65D0-3D0*P39+2D0*P25)*DEGRD)
      P72=P11
      P33 = P33+P11/3600D0
C
C     PERT OF SOLAR LATITUDE BY JUPITER - P62:
C
      P62 = .166D0*DCOS((265.5D0-2D0*P39+P25)*DEGRD)
C
C     SATURN:
C
C     MEAN ANOMALY OF SATURN - P40:
C
      P11 = 175.613D0
      P12 = 3D0*360D0
      P13 = 141.794D0
      P40 = P11 + DMOD(P12*P23,360D0) + P13*P23
      P40 = DMOD(P40,360D0)
      P11 = .419*DCOS((100.58D0-P40+P25)*DEGRD) +
     *  .32D0*DCOS((269.46D0-P40)*DEGRD)
      P73=P11
      P33 = P33+P11/3600D0
C
C-----------------------------------------------------------------------
C
CBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
C
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
C
C     PARAMETERS
C
  300 CONTINUE
C
C     PRECESSION:
C
      P42 = (50.2564D0+.0222D0*(P18-1900D0)/100D0) *
     *  (P23-(P18-1900D0)/100D0) * 100D0/3600D0
C
C     APPARENT LONGITUDE - P41:
C
      P41 = P27-P25+P24+P29+P33+P36+P34
C
C     SOLAR LATITUDE - P60:
C
      P60 = (.576D0*DSIN(P63*DEGRD) + P61+P62)/3600D0
C
C      OBLIQUITY - P75
C
      P75 = P35 + P43
C
C     APPARENT RIGHT ASCENSION - P44:
C
      P44  =  DATAN(DTAN(P41*DEGRD)*DCOS(P75*DEGRD))/DEGRD
      IF(DSIGN(ONE,P44).NE.DSIGN(ONE,DSIN(P41*DEGRD))) P44 = P44+180D0
      IF(P44.LT.0.D0) P44 = 360D0+P44
C
C     EQUATION OF TIME - P46:
C
      P46 = P45-P44
      IF(P46.GT.180D0) P46 = P46-360D0
C
C     HOUR ANGLE - P48:
C
      P48 = P51+P46-180D0
      HA = P48
C
C     LOCAL HOUR ANGLE - P49
C
      P49  =  P48-P20
      P49 = DMOD(P49,360D0)
      IF(P49.LT.0) P49 = P49 + 360D0
      LHA=P49
C
C
C
C     DECLINATION - P47:
C
      P47 = DARSIN(DSIN(P41*DEGRD)*DSIN(P75*DEGRD)*DCOS(P60 *DEGRD) +
     * DSIN(P60*DEGRD) * DCOS(P75*DEGRD))/DEGRD
      DEC = P47
C
C     ZENITH ANGLE - Z:
C
      Z = DARCOS(DSIN(P19*DEGRD)*DSIN(P47*DEGRD) + DCOS(P19*DEGRD) *
     *  DCOS(P47*DEGRD) * DCOS(P49*DEGRD))/DEGRD
C
C     AZIMUTH ANGLE - A:
C
      P11 = (-DSIN(P19*DEGRD) * DCOS(P49*DEGRD) * DCOS(P47*DEGRD) +
     *  DSIN(P47*DEGRD) *DCOS(P19*DEGRD))/DSIN(Z*DEGRD)
      IF(DABS(P11).GT.1.0D0) P11 = DSIGN(1.0D0,P11)
      A= DARCOS(P11)/DEGRD
      IF(DSIGN(1.D0,-DCOS(P47*DEGRD)  * DSIN(P49*DEGRD)/DSIN(Z*DEGRD))
     * .NE.DSIGN(1D0,DSIN(A*DEGRD)))  A = 360-A
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IF(P10.EQ.0) GO TO 30
      CALL NUPAGE
      PRINT 2000,MONTH,DAY,YEAR
2000  FORMAT(' MONTH',I5,/,' DAY',I5,/,' YEAR',I6)
      IF(P8.EQ.0) PRINT 2001
      IF(P8.EQ.1) PRINT 2002
      IF(P8.EQ.2) PRINT 2003
2001  FORMAT(' FULL COMPUTATIONS')
2002  FORMAT(' NO PLANETS')
2003  FORMAT(' NO MOON')
      PRINT 2004,LAT,LONG
 2004  FORMAT(' LATITUDE ', F10.4,/,' LONGITUDE',F10.4)
	IF(P6.NE.1.AND.P5.EQ.0) PRINT 2050,TIME
 2050  FORMAT(' TIME (LST)',F10.2)
	IF(P6.NE.1.AND.P5.EQ.1) PRINT 2051,TIME
 2051  FORMAT(' TIME (GMT)',F10.2)
C
	IF(P10.LT.2) GO TO 30
C
CDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
C
      PRINT 2005,P23,P22
2005  FORMAT(' T',F15.6,' D',F15.6)
      P0 = TOHMS(P24)
      PRINT 2006,P0
2006  FORMAT(' MEAN LONG',F15.5)
      P0 = TOHMS(P25)
      PRINT 2007,P0
2007  FORMAT(' MEAN ANOMALY',F15.5)
      PRINT 2008,P26
2008  FORMAT(' ECCENTRICITY',F15.5)
      P0 = TOHMS(P27-P25)
      PRINT 2009,P0
2009  FORMAT(' EQUATION OF CENTER',F15.5)
      PRINT 2010,R
2010  FORMAT(' RADIUS VECTOR',F15.5)
      P0 = TOHMS(P29)
      PRINT 2011,P0
 2011 FORMAT (' ABERRATION ',F15.5)
      P0 = TOHMS(P43)
      PRINT 2012,P0
2012  FORMAT(' MEAN OB',F15.5)
      P0 =  TOHMS(P45/360D0*24D0)
      PRINT 2013,P0
2013  FORMAT(' MEAN ASCENSION',F15.5)
	IF(P10.GT.1) GO TO 35
      P0 = TOHMS(P74)
      PRINT 2014, P0
 2014 FORMAT (' P MOON',F15.5)
	IF(P10.GT.0) GO TO 35
      P0 = P70/3600D0
      P0 = TOHMS(P0)
      PRINT 2015, P0
 2015 FORMAT (' P VENUS',F15.5)
      P0 = P61/3600D0
      P0 = TOHMS(P0)
      PRINT 2016, P0
 2016 FORMAT (' P LAT BY VENUS ',F15.5)
      P0 = P71/3600D0
      P0 = TOHMS(P0)
      PRINT 2017, P0
 2017 FORMAT (' P MARS ',F15.5)
      P0 = P72/3600D0
      P0 = TOHMS(P0)
      PRINT 2018, P0
 2018 FORMAT (' P JUPITER ',F15.5)
      P0 = P62/3600D0
      P0 = TOHMS(P0)
      PRINT 2019, P0
 2019 FORMAT (' P OF LAT BY JUP ',F15.5)
      P0 = P73/3600D0
      P0 = TOHMS(P0)
      PRINT 2020, P0
 2020 FORMAT (' P SATURN ',F15.5)
C
   35  CONTINUE
C
      P0 = TOHMS(P33)
      PRINT 2021, P0
 2021 FORMAT (' PERTURBATIONS ',F15.5)
      P0 = TOHMS(P36)
      PRINT 2022, P0
 2022 FORMAT (' LONG PERIOD ', F15.5)
      P0 = TOHMS(P34)
      PRINT 2023, P0
 2023 FORMAT (' NUTATION OF LONG ', F15.5)
      P0 = TOHMS(P35)
      PRINT 2024, P0
 2024 FORMAT (' NUT OBLIQUITY ', F15.5)
      P0 = TOHMS(P42)
      PRINT 2025, P0
 2025 FORMAT (' PRECESSION ', F15.5)
      P0 = TOHMS(P41)
      PRINT 2026, P0
 2026 FORMAT (' APPAR. LONG. ',F15.5)
      P0 = TOHMS(P60)
      PRINT 2027, P0
 2027 FORMAT (' SOLAR LATITUDE ',F15.5)
      P0 = TOHMS(P75)
      PRINT 2028, P0
 2028 FORMAT (' OBLIQUITY ',F15.5)
      P0 = P44/360D0*24D0
      P0 = TOHMS(P0)
      PRINT 2029, P0
 2029 FORMAT (' APPAR. ASCENSION ',F15.5)
      P0 = P46/360D0*24D0
      P0 = TOHMS(P0)
      PRINT 2030, P0
 2030 FORMAT (' EQUATION OF TIME ', F15.5)
      P0 = TOHMS(P49)
      PRINT 2032, P0
 2032 FORMAT (' LHA = ',F15.5)
      P0 = TOHMS(P47)
      PRINT 2033, P0
 2033 FORMAT (' DECLIN. ',F15.5)
C
CDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
   30 CONTINUE
C
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
C
C
      IF(P6.EQ.0) GO TO 500
      P12 = P53/360D0*24D0
      IF(P5.EQ.0) P12=0D0
      P10 = 0
      IF(DMOD(A,180.d0).GT.0.01) GO TO 400
      P11 = P56+P12
      IF(P11.LT.0D0) P11 = P11+24D0
      P13 = DMOD(P11,1D0)*60.0D0
      TIME = DINT(P11)*100.0D0+DINT(P13)
     * + DMOD(P13,1.D0)*60.0D0/100.D0
      IF(P56+P12.LT.0D0) TIME = -TIME
      RETURN
C
C
  400 CONTINUE
      IF(A.LT.90.0.OR.A.GT.270.0) GO TO 410
      IF(A.LE.180.) GO TO 405
  402  CONTINUE
      P55 = P56
      GO TO 1
  405  CONTINUE
	P54 = P56
	GO TO 1
  410 CONTINUE
      IF(A.LT.90.0) GO TO 405
	GO TO 402
C
C
  500 CONTINUE
C
CEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
C
	IF(P10.EQ.0) GO TO 550
       P0 = A
      P0 = TOHMS(P0)
      PRINT 2034, P0
 2034 FORMAT (' A      DDD.MMSSS ',F15.5)
      P0 = Z
      P0 = TOHMS(P0)
      PRINT 2035, P0
 2035 FORMAT (' Z       DD.MMSSS ',F15.5)
C
  550  CONTINUE
      IF(P7.LT.3) RETURN
C
CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
C
      IF(P10.EQ.0) GO TO 600
      P52 = P46
      P0 = P41-(P29+P34)
      P0 = TOHMS(P0)
      PRINT 2036, P0
 2036 FORMAT (' LONGITUDE ',F15.6)
      P0 = (P29+P34)*10000
      P0 = TOHMS(P0)
      PRINT 2037, P0
 2037 FORMAT (' REDN TO LONG ',F15.6)
      P0 = P60*10000
      P0 = TOHMS(P0)
      PRINT 2038, P0
 2038 FORMAT (' LATITUDE ',F15.6)
      P0 = P42*10000
      P0 = TOHMS(P0)
      PRINT 2039, P0
 2039 FORMAT (' PRECESSION ',F15.6)
      P0 = P34*10000
      P0 = TOHMS(P0)
      PRINT 2040, P0
 2040 FORMAT (' NUT IN LONG ',F15.6)
      P0 = P35*10000
      P0 = TOHMS(P0)
      PRINT 2041, P0
 2041 FORMAT (' NUT IN OBLI ',F15.6)
      P0 = TOHMS(P43)
      PRINT 2042, P0
 2042 FORMAT (' OBLIQUITY ',F15.6)
      P0 = P44/360D0*24D0
      P0 = TOHMS(P0)
      PRINT 2043, P0
 2043 FORMAT (' APP RT. ASCEN ',F15.6)
      P0 = TOHMS(P47)
      PRINT 2044, P0
 2044 FORMAT (' APP DECLINTION ',F15.6)
C
  600 CONTINUE
      P16 = P16+1.D0
      IF(P10.EQ.0) GO TO 610
      P10 = 0D0
      GO TO 1
  610 CONTINUE
      P11 = 180.D0-P52-(P46-P52)*((-P52+180.D0)/360D0)
      P0 = TOHMS(P11/360.D0*24D0)
      PRINT 2045,P0
 2045 FORMAT (' ET ',F15.6)
C
C
 620  continue
      RETURN
C
CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
C
C              *** END SUBROUTINE EPHEMS ***
      END
C
C
      SUBROUTINE NUPAGE
C    DUMMY NEW PAGE ROUTINE
       PRINT 2000
 2000  FORMAT(1H1)
      RETURN
      END
C
C
      REAL*8 FUNCTION TOHMS(P1)
C     FUNCTION RETURNS P IN HH MM SS FORMAT
      IMPLICIT REAL*8(A-H,O-Z)
      DATA ONE/1.0D0/
      P3 = DABS(P1)
      B = DMOD(P3,ONE)*60.
      C = DINT(B)
      D = C/100.
      P2 = DINT(P3) + D + DMOD(B,ONE)*60./10000.
      P2 = P2*DSIGN(ONE,P1)
      TOHMS = P2
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE DATE2DOY(date,year,doy)
C     subroutine for converting YYYYMMDD to YYYY DDD format
      integer year,doy,date,MONTHS(12),i,mn,dy

      DATA MONTHs /31,28,31,30,31,30,31,31,30,31,30,31/

      year=date/10000
      mn=(date-year*10000)/100
      dy=date-year*10000-mn*100
      doy=0

c     Check for leap year [for "yr" = 4-digit year]
      IF(MOD(year,4).ne.0) then
        months(2)=28
      ELSEIF(MOD(year,400).eq.0) then
        months(2)=29
      ELSEIF(MOD(year,100).eq.0) then
        months(2)=28
      ELSE
        months(2)=29
      endif

c Determine doy from month and day

      do i=1,(mn-1)
        doy=doy+months(i)
      enddo

      doy=doy+dy

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

