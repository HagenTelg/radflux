C     Last change: JA 4/8/2015 4:05:18 PM
      PROGRAM swfcf

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     This program has been written and encompasses the ideas of:
c
c     Dr. Charles N. Long
c     Atmospheric Radiation Measurement Program
c     Pacific Northwest National Laboratory
c     P. O. 999, MSIN: K9-24
c     Richland, WA, USA  99352
c     Ofc.: 1 (509) 372-4917
c     FAX: 1 (509) 372-6247
c     e-mail: chuck.long@pnl.gov
c
c       Long, C. N., T. P. Ackerman, K. L. Gaustad, and J. N. S. Cole,
c      (2006): Estimation of fractional sky cover from broadband shortwave
c      radiometer measurements, JGR, 111, D11204, doi:10.1029/2005JD006475.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      REAL*8 x(1450,55),y(55),cf(1450),difcld,avgdr,dc(1450),stdev2
      REAL*8 avgcsw,dr(100),expa,expb,stdev,avgtsw,st(1450),clrsw
      REAL*8 a(55),er(55),dclim,drlim,stdlim,favdc,favcf,favcz,tmp(55)
      REAL*8 avdr(1450),rdn(1450),av,adv,narr(11)
      REAL*8 ofst,slope,ABDEV,carr(11),rnum7
      REAL*8 tstcf
      integer thr,tmin,tdate,lsttim,anum(55),swcflg,p,minit,ttime,tzhr
      INTEGER r,c,j,n,swftim(1450),k,drn,l,m,avgnum,hr,cflg(1450)
      INTEGER swfdat(1450),datnum,q,gmttim,s,e,tflg(1450),tmpflg
      INTEGER qc(6),totclr,clr(1450),zdat(1450),ztim(1450),zhr,tzdate
      INTEGER tztime,fitn,numc,num7,num0,num1,numo,aflg
      CHARACTER*12 dirfil,infil,outfil,cnffil,avgfil,fitfil
      CHARACTER*12 tstfil
      CHARACTER*700 line,path,lin1,lin2
      LOGICAL GO,fit,nxtday,start,tst

      GO=.true.
      fit=.false.
      tst=.false.
      do i=1,55
        er(i)=-999.0
        anum(i)=0
      end do
      do i=1,11
        narr(i)=i*1.0
      end do
 1    format(a,a,a,a,a,a,a,a)
      fitfil='armswzcf.fit'
      cnffil='armswzcf.cnf'
      OPEN(UNIT=1,FILE=cnffil,STATUS='old',ERR=98)
      READ(1,12) path
      READ(1,*) datnum
      READ(1,*) swcflg
      READ(1,*) i
      IF(i.eq.1.or.i.eq.3) fit=.true.
      IF(i.eq.2.or.i.eq.3) tst=.true.
      READ(1,*) expa
      READ(1,*) expb
      READ(1,*) dclim
      READ(1,*) drlim
      READ(1,*) avgnum
      READ(1,*) stdlim
      READ(1,*) czlim
      do i=1,datnum
        READ(1,*) er(i)
      end do
      IF(i.gt.datnum) i=datnum
 5    CLOSE(UNIT=1)
      er(i+1)=-9.0
      er(i+2)=-9.0
      er(i+3)=-9.0
      er(i+4)=-9.0
      i=INDEX(path,'/*')
      do p=(i-1),1,-1
        IF(path(p:p).eq.' ') then
          WRITE(6,*) path(1:p)//'<<'
        else
          GO TO 6
        endif
      end do

 6    WRITE(6,*)'  path= <',path(1:p),'>'
      avgnum=avgnum/2
      IF(fit) then
        OPEN(UNIT=88,FILE=fitfil,STATUS='unknown')
        WRITE(88,1)'    date  time      CosZ    difcld        cf  FitN
     %  ClrN'
      endif

      dirfil='swf.dir'
      OPEN(UNIT=9,FILE='swncferr.asc',STATUS='unknown')
      WRITE(9,*)'%%%%%%%% No Clear data for these days  %%%%%%%%%%%%%%'

      OPEN(UNIT=1,FILE=dirfil,STATUS='old',ERR=99)
 10   READ(1,11,END=100) infil
 11   FORMAT(a12)
        WRITE(6,*)'>>>  Processing file ',infil,' <<<'
      j=INDEX(infil,'.')
      outfil=infil(1:j)//'swc'
      avgfil=infil(1:j)//'c15'
      OPEN(UNIT=2,FILE=path(1:p)//infil,STATUS='old')
      WRITE(6,*)'  Opened Infile: ',path(1:p)//infil
      r=0
      start=.true.
      READ(2,12,ERR=40,END=40) lin1
      READ(2,12,ERR=40,END=40) lin2
      READ(2,*,ERR=40,END=40)
      READ(2,13,ERR=40,END=40) line
 12   FORMAT(a600)
 13   FORMAT(a600)
      do q=(700),1,-1
        IF(line(q:q).ne.' ') then
          GO TO 14
        endif
      end do
 14   continue
      IF(swcflg.gt.1) then
        OPEN(UNIT=3,FILE=outfil,STATUS='unknown')
        WRITE(3,12) lin1
        WRITE(3,12) lin2
        WRITE(3,*) ' '
        WRITE(3,1) line(1:30)//'     cf cflg  clr'//line(31:167)//
     %line(193:q)//'     DifCld    DRStDev      AvgDR       AvgN'
      endif
      IF(GO) then
        OPEN(UNIT=7,FILE=infil(1:6)//'cf.a15',STATUS='unknown')
        WRITE(7,1) line(1:30)//'   difcld  CFstdev    cf   Ncf  Ncsw  Ns
     %sw  Nclr'//line(31:167)//line(193:q)
        GO=.false.
      endif
      n=0
      r=0
      do l=1,1450
        clr(l)=0
        st(l)=-9.0
        dc(l)=-9.0
        cf(l)=-9.0
        avdr(l)=-9.0
        cflg(l)=-9
        tflg(l)=0
        rdn(l)=0.0
      end do

 30   r=r+1
      IF(r.gt.1441) then
        WRITE(6,*)'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
        WRITE(6,*)'% The input file: ',  infil ,' has more than     %'
        WRITE(6,*)'% 1440 minutes worth of data in it. Data ingest  %'
        WRITE(6,*)'% has stopped at LST time:',swftim(r-1),'          %'
        WRITE(6,*)'% You need to look at the original raw data file %'
        WRITE(6,*)'% that was used as input to the clear ID code    %'
        WRITE(6,*)'% for this date:', swfdat(r-1),'                   %'
        WRITE(6,*)'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
        pause
        GO TO 40
      endif
      READ(2,*,END=40,ERR=32) zdat(r),ztim(r),swfdat(r),swftim(r),
     %(y(c),c=1,15),(QC(j),j=1,5),(y(c),c=16,datnum)
      if (start) then
        lsttim=swftim(r)
        gmttim=9999
        tzdate=99999999
        tdate=99999999
        start=.false.
      end if
      IF(lsttim.gt.swftim(r)) then
        WRITE(6,*)'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
        WRITE(6,*)'% The input file: ',  infil ,' has a time        %'
        WRITE(6,*)'% problem in it. Current LST read time:          %'
        write(6,*)'% ',swftim(r),'                            %'
        WRITE(6,*)'% is greater than the last LST time read:        %'
        write(6,*)'% ',lsttim,'                            %'
        WRITE(6,*)'% You need to look at the original raw data file %'
        WRITE(6,*)'% that was used as input to the clear ID code    %'
        WRITE(6,*)'% for this date:', swfdat(r),'                     %'
        WRITE(6,*)'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
        pause
        GO TO 40
      endif
      lsttim=swftim(r)
      do j=1,datnum
        x(r,j)=y(j)
      end do
      IF(y(1).gt.0.19) clr(r)=qc(5)
      GO TO 30

 32   write(6,*)' read error, r= ',r
      write(6,*)' read error, date/time: ',swfdat(r),swftim(r)
      write(6,33)'data:',(y(j),j=1,datnum)
 33   format(a,50f10.3)
cc      pause

      do j=1,2
        x(r,j)=-9.0
      end do

      do j=3,10
        x(r,j)=-9999.0
      end do

      do j=11,12
        x(r,j)=-9.0
      end do

      do j=13,datnum
        x(r,j)=-9999.0
      end do
      GO TO 30

 40   CLOSE(UNIT=2)
      r=r-1
      IF(r.lt.10) then
        do j=1,r
          WRITE(3,60)zdat(j),ztim(j),swfdat(j),swftim(j),cf(j),cflg(j),
     %clr(j),(x(j,c),c=1,datnum),dc(j),st(j),avdr(j),rdn(j)
        end do
        GO TO 10
      endif
      n=1
      do j=1,r
        IF(clr(j).eq.1) then
          cf(j)=0.0
          cflg(j)=0
          n=n+1
        else
          cf(j)=2.0
          cflg(j)=-9
        endif
      end do

      do j=1,(avgnum)
        IF(cf(j).gt.1.5) cf(j)=-9.0
      end do

      do j=(r-avgnum),r
        IF(cf(j).gt.1.5) cf(j)=-9.0
      end do

      do m=(avgnum+1),(r-avgnum)
        difcld=0.0
        avgdr=0.0
        avgtsw=0.0
        avgcsw=0.0
        stdev=0.0
        drn=0
        do k=(m-avgnum),(m+avgnum)
          IF(x(k,3).ge.0.0.and.x(k,4).gt.0.0.and.x(k,6).ge.0.0.and.
     %x(k,7).ge.0.0.and.x(k,11).ge.0.0) then
             drn=drn+1
             clrsw=x(k,4)
             IF(x(k,14).gt.0.0) clrsw=x(k,14)
             difcld=difcld+(x(k,6)-x(k,7))/clrsw
             IF(x(k,11).le.1.0) then
               avgdr=avgdr+x(k,11)
               dr(drn)=x(k,11)
             else
               avgdr=avgdr+1.0
               dr(drn)=1.0
             endif
             avgtsw=avgtsw+x(k,3)
             avgcsw=avgcsw+x(k,4)
          endif
        end do
        IF(drn.gt.0) then
          difcld=difcld/(1.0*drn)
          avgdr=avgdr/(1.0*drn)
          avgtsw=avgtsw/(1.0*drn)
          avgcsw=avgcsw/(1.0*drn)
          do l=1,drn
            stdev=stdev+(dr(l)-avgdr)**2
          end do
          stdev=SQRT(stdev/(drn*1.0))
          IF(x(m,6).ge.0.0.and.x(m,7).ge.0.0) then
            IF(x(m,14).ge.0.0) then
               difcld=(x(m,6)-x(m,7))/x(m,14)
            elseif(x(m,4).ge.0.0) then
               difcld=(x(m,6)-x(m,7))/x(m,4)
            else
              difcld=-9.0
              cf(m)=-9.0
              cflg(m)=-9
            endif
            IF(difcld.gt.10.0) then
              IF(x(m,4).ge.0.0) then
               difcld=(x(m,6)-x(m,7))/x(m,4)
              else
                cf(m)=-9.0
                difcld=-9.0
                cflg(m)=-9
              endif
            endif
            IF(difcld.gt.10.0.or.difcld.LT.-10.0) then
              difcld=-9.0
              cf(m)=-9.0
              cflg(m)=-9
            endif
          else
            difcld=-9.0
            cf(m)=-9.0
            cflg(m)=-9
          endif

          avgtsw=-999.0
          IF(x(m,3).ge.0.0) avgtsw=x(m,3)

          avgcsw=-999.0
          IF(x(m,4).ge.0.0) avgcsw=x(m,4)

          avdr(m)=avgdr
          dc(m)=difcld
          st(m)=stdev
          rdn(m)=drn*1.0
          IF(cf(m).gt.1.5) then
            IF(x(m,1).lt.czlim) then
              cf(m)=-9.0
              cflg(m)=-1
            elseIF(difcld.lt.0.0.and.difcld.GT.-8.9) then
              IF(avgtsw/avgcsw.lt.0.4) then
                cf(m)=1.2
                cflg(m)=2
              ELSE
                cf(m)=0.00
                cflg(m)=1
              endif
            ELSEIF(difcld.lt.dclim.and.avgdr.gt.drlim.and.stdev.lt.
     %stdlim.and.difcld.GT.-8.9)then
              IF((avgtsw/avgcsw).lt.0.25) then
                cf(m)=1.2
                cflg(m)=4
              ELSEIF(avgtsw/avgcsw.lt.0.5) then
                cf(m)=1.1
                cflg(m)=5
              else
                cf(m)=1.0
                cflg(m)=6
              endif
            else
              cf(m)=expa*difcld**expb
                cflg(m)=7
              IF(cf(m).gt.1.0) cf(m)=1.0
              IF(cf(m).lt.0.0) cf(m)=0.0
            endif
          endif
        else
          cf(m)=-9.0
          cflg(m)=-1
          dc(m)=-9.0
          st(m)=-9.0
        endif

      end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc
cc          Put loop in here from 1 to r to test cf for anomalies
cc              using cf(j), cflg(j), and clr(j)

      tstfil=infil(1:8)//'.tst'
      IF(tst) then
        OPEN(UNIT=99,FILE=tstfil,STATUS='unknown')
        WRITE(99,1) line(1:30)//'     cf cflg  clr'//line(31:167)//
     %line(193:216)//'     DifCld    DRStDev      AvgDR       AvgN
     %   av        adv       ofst      slope      abdev      orgcf  #0
     %#1  #7  #c  #o  Aflg'
      endif

      do j=1,r
        s=j-5
        IF(s.lt.1) s=1
        e=j+5
        IF(e.gt.r) e=r
        av=0.0
        adv=0.0
        numc=0
        num7=0
        num0=0
        num1=0
        numo=0
        aflg=0
        do c=s,e
          IF(cf(c).GT.-1.0) then
            IF(cf(c).gt.1.0) cf(c)=1.0
            numc=numc+1
            carr(numc)=cf(c)
            av=cf(c)+av
            IF(cflg(c).eq.0) then
              num0=num0+1
            elseIF(cflg(c).eq.1) then
              num1=num1+1
            elseIF(cflg(c).eq.7) then
              num7=num7+1
            ELSEIF(cflg(c).gt.1) then
              numo=numo+1
            endif
          endif
        end do
        IF(numc.gt.3) then
          av=av/numc*1.0
          do c=1,numc
            adv=ABS(av-carr(c))+adv
          end do
          adv=adv/numc*1.0
          IF(adv.gt.0.1) adv=0.1
          IF(adv.gt.0.001) then
            call medfit(narr(1:numc),carr(1:numc),numc,ofst,slope,ABDEV)
          else
            ofst=-999.0
            slope=-999.0
            abdev=-999.0
          endif

          tstcf=cf(j)

cccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    Test for anomalous skipping between fit calc and OVC
cc    using average absolute deviation. If current data
cc    is more than 1 ADV from 11-point avg, then is
cc    replaced using offset and slope vs time fit
cc    from above.
cccccccccccccccccccccccccccccccccccccccccccccccccccc

          IF(ABS(cf(j)-av).gt.adv.and.ABS(cf(j)-av).ge.0.04.and.ofst
     %.GT.-1.0.and.cf(j).GT.-1.0.and.numo.gt.0) then
            cf(j)=ofst+slope*nint((numc*0.5)+0.2)
            IF(ABS(cf(j)-tstcf).gt.0.1) then
              aflg=2
            else
              aflg=1
            endif
            tflg(j)=1
          endif


cccccccccccccccccccccccccccccccccccccccccccccccccccc
cc    Test for anomalous skipping between CLR determined
cc    by other tests and using the NDCE fit eqn.
cc    This part basically weights the data of interest
cc    by the number of "num7" fitted values over the
cc    number of fit plus clear (num7+num0+num1).
cccccccccccccccccccccccccccccccccccccccccccccccccccc

          IF((num0+num7+num1)*1.0.GT.(numc*0.7).AND.(num0+num1).ge.1
     %.and.numo.lt.3.and.cf(j).GT.-1.0) then
            rnum7=(num7*1.0)/((num7+num0+num1)*1.0)
            cf(j)=cf(j)*rnum7
            IF(ABS(cf(j)-tstcf).gt.0.1) then
              aflg=5
            elseIF(ABS(cf(j)-tstcf).gt.0.01) then
              aflg=4
            else
              aflg=3
            endif
            tflg(j)=2
          else
            rnum7=-999.0
          endif

          IF(cf(j).GT.-1.0) then
            IF(cflg(j).lt.0.and.tflg(j).gt.0) then
              tmpflg=(-1*cflg(j))+(-10*tflg(j))
            ELSEIF(tflg(j).gt.0) then
              tmpflg=(10*Tflg(j))+cflg(j)
            else
              tmpflg=cflg(j)
            endif
          endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc     added 20141017: Noticed anomalous "skipping" between larger (>0.10)
cc       values of Scv and clear-detected zero values. This wasn't being
cc       caught and corrected right with the above "num7+num1+num0" scheme.
cc       After looking into it and analysis, the problem is well corrected
cc       just by substituting in the average Scv value (av) from above when
cc       "cflg" value is 20 or greater and noted by added 10 to ClrFlg.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccc smooth Cflg.ge.20 occurrences cccccccccccccccccccccccccccc
          if(tmpflg.ge.20.and.cflg(j).ge.0)then
            cf(j)=av
          endif
ccccccccccccccccc END smooth Cflg.ge.20 occurrences cccccccccccccccccccccccccccc

          IF(cf(j).GT.-1.0.and.tst) then
            WRITE(99,59)zdat(j),ztim(j),swfdat(j),swftim(j),cf(j),
     %tmpflg,clr(j),(x(j,c),c=1,17),dc(j),st(j),avdr(j),
     %rdn(j),av,adv,ofst,slope,abdev,tstcf,num0,num1,num7,numc,numo,aflg
          endif

        endif
      end do



 59      FORMAT(i8,i6,2x,i8,i6,f7.2,i5,i5,2f10.6,8f9.1,2f9.5,3f9.1,
     %2f12.1,10f11.4,5i4,i6)

       CLOSE(UNIT=99)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



      IF(swcflg.gt.1) then
        do j=1,r
            IF(cflg(j).lt.0.and.tflg(j).gt.0) then
              tmpflg=(-1*cflg(j))+(-10*tflg(j))
            ELSEIF(tflg(j).gt.0) then
              tmpflg=(10*Tflg(j))+cflg(j)
            else
              tmpflg=cflg(j)
            endif
          IF(cf(j).le.1.d0) then
            WRITE(3,60)zdat(j),ztim(j),swfdat(j),swftim(j),cf(j),tmpflg,
     %clr(j),(x(j,c),c=1,datnum),dc(j),st(j),avdr(j),rdn(j)
 60      FORMAT(i8,i6,2x,i8,i6,f7.2,i5,i5,2f10.6,8f9.1,2f9.5,3f9.1,
     %2f12.5,60f11.4)
          else
            WRITE(3,60)zdat(j),ztim(j),swfdat(j),swftim(j),1.0,tmpflg,
     %clr(j),(x(j,c),c=1,datnum),dc(j),st(j),avdr(j),rdn(j)
          endif
        end do
        CLOSE(UNIT=3)
      endif

      IF(swcflg.eq.1.or.swcflg.eq.3) then
        OPEN(UNIT=3,FILE=avgfil,STATUS='unknown')
        WRITE(3,12) lin1
        WRITE(3,12) lin2
        WRITE(3,*) ' '
        WRITE(3,1) line(1:30)//'   difcld  CFstdev    cf   Ncf  Ncsw  Ns
     %sw  Nclr'//line(31:167)//line(193:q)
      endif
      i=0
      n=0
      tzdate=zdat(1)
 70   i=i+1
      IF(i.gt.r) GO TO 85
      hr=swftim(i)/100
      zhr=ztim(i)/100
      minit=swftim(i)-(hr*100)
      IF(x(i,1).lt.czlim) GO TO 70
      IF(minit.eq.0.or.minit.eq.15.or.minit.eq.30.or.minit.eq.45) then
        WRITE(6,*)'day start: ',swfdat(i),swftim(i)
        do j=1,datnum
          IF(x(i,j).gt.er(j)) then
            a(j)=x(i,j)
            anum(j)=1
          else
            a(j)=0.0
            anum(j)=0
          endif
        end do
        IF(dc(i).GT.-8.1) then
          a(datnum+1)=dc(i)
          anum(datnum+1)=1
        else
          a(datnum+1)=0.0
          anum(datnum+1)=0
        endif
        IF(st(i).GT.-0.1) then
          a(datnum+2)=st(i)
          anum(datnum+2)=1
        else
          a(datnum+2)=0.0
          anum(datnum+2)=0
        endif
        IF(cf(i).GT.-0.1) then
          IF(cf(i).gt.1.d0) then
            a(datnum+3)=1.0
            tmp(1)=1.0
          else
            a(datnum+3)=cf(i)
            tmp(1)=cf(i)
          endif
          anum(datnum+3)=1
        else
          a(datnum+3)=0.0
          anum(datnum+3)=0
          tmp(1)=0.0
        endif
        IF(cflg(i).eq.7.and.dc(i).GT.-1.0) then
          favdc=dc(i)
          favcf=cf(i)
          favcz=x(i,1)
          fitn=1
        else
          favdc=0.0
          favcf=0.0
          favcz=0.0
          fitn=0
        endif
        n=1
        totclr=clr(i)
        lsttim=swftim(i)
        gmttim=ztim(i)
        thr=hr
        tzhr=zhr
        tmin=minit+15
        IF(tmin.gt.45) then
          thr=thr+1
          tzhr=tzhr+1
          tmin=0
          ttime=thr*100+tmin
          tztime=tzhr*100+tmin
        else
          thr=hr
          tzhr=zhr
          ttime=thr*100+tmin
          tztime=tzhr*100+tmin
        endif
        tdate=swfdat(i)
        tzdate=zdat(i)
        nxtday=.false.
      else
        GO TO 70
      endif

 80   i=i+1
      IF(i.GE.(r+1)) GO TO 85
      IF(x(i,1).lt.czlim) GO TO 80
      IF(swftim(i).lt.ttime) then
        do j=1,datnum
          IF(x(i,j).gt.er(j)) then
            a(j)=a(j)+x(i,j)
            anum(j)=anum(j)+1
          endif
        end do
        IF(dc(i).GT.-8.1) then
          a(datnum+1)=a(datnum+1)+dc(i)
          anum(datnum+1)=anum(datnum+1)+1
        endif
        IF(st(i).GT.-0.1) then
          a(datnum+2)=a(datnum+2)+st(i)
          anum(datnum+2)=anum(datnum+2)+1
        endif
        IF(cf(i).GT.-0.1) then
          IF(cf(i).gt.1.d0) then
            a(datnum+3)=a(datnum+3)+1.0
            tmp(anum(datnum+3)+1)=1.0
          else
            a(datnum+3)=a(datnum+3)+cf(i)
            tmp(anum(datnum+3)+1)=cf(i)
          endif
          anum(datnum+3)=anum(datnum+3)+1
        endif
        IF(cflg(i).eq.7.and.dc(i).GT.-1.0) then
          favdc=favdc+dc(i)
          favcf=favcf+cf(i)
          favcz=favcz+x(i,1)
          fitn=fitn+1
        endif
        n=n+1
        totclr=totclr+clr(i)
      else
        IF(n.gt.0) then
          do j=1,datnum+3
            IF(anum(j).gt.0) then
              a(j)=a(j)/(anum(j)*1.0)
            else
              a(j)=er(j)
            endif
          end do
          stdev2=0.0
          IF(anum(datnum+3).gt.1) then
            do j=1,anum(datnum+3)
              stdev2=stdev2+(tmp(j)-a(datnum+3))**2
            end do
            stdev2=SQRT(stdev2/anum(datnum+3))
          else
            stdev2=-0.99
          endif
          IF(stdev2.gt.1.0) stdev2=-0.99
          IF(swcflg.eq.1.or.swcflg.eq.3) then
            WRITE(3,81)tzdate,gmttim,tdate,lsttim,a(datnum+1),stdev2,
     %a(datnum+3),anum(datnum+3),anum(4),anum(13),totclr,
     %(a(c),c=1,datnum)
          endif
          WRITE(7,81)tzdate,gmttim,tdate,lsttim,a(datnum+1),stdev2,
     %a(datnum+3),anum(datnum+3),anum(4),anum(13),totclr,
     %(a(c),c=1,datnum)
 81     FORMAT(i8,i6,2x,i8,i6,2f9.5,f7.2,i5,3i6,2f10.6,8f9.1,2f9.5,3f9.1
     %,2f12.5,60f11.4)
        endif
        IF(fitn.gt.0) then
          favdc=favdc/fitn
          favcf=favcf/fitn
          favcz=favcz/fitn
        endif
        IF(fit.and.fitn.gt.0) then
          WRITE(88,83) tdate,lsttim,favcz,favdc,favcf,fitn,totclr
        endif
 83     FORMAT(i8,i6,3f10.6,i6,i8)
        do j=1,datnum
          IF(x(i,j).gt.er(j)) then
            a(j)=x(i,j)
            anum(j)=1
          else
            a(j)=0.0
            anum(j)=0
          endif
        end do
        IF(dc(i).GT.-8.1) then
          a(datnum+1)=dc(i)
          anum(datnum+1)=1
        else
          a(datnum+1)=0.0
          anum(datnum+1)=0
        endif
        IF(st(i).GT.-0.1) then
          a(datnum+2)=st(i)
          anum(datnum+2)=1
        else
          a(datnum+2)=0.0
          anum(datnum+2)=0
        endif
        IF(cf(i).GT.-0.1) then
          IF(cf(i).gt.1.0) then
            a(datnum+3)=1.0
            tmp(1)=1.0
          else
            a(datnum+3)=cf(i)
            tmp(1)=cf(i)
          endif
          anum(datnum+3)=1
        else
          a(datnum+3)=0.0
          anum(datnum+3)=0
          tmp(1)=0.0
        endif
        IF(cflg(i).eq.7.and.dc(i).GT.-1.0) then
          favdc=dc(i)
          favcf=cf(i)
          favcz=x(i,1)
          fitn=1
        else
          favdc=0.0
          favcf=0.0
          favcz=0.0
          fitn=0
        endif
        n=1
        totclr=clr(i)
 82     lsttim=ttime
        gmttim=tztime
        IF(nxtday) tzdate=zdat(r)
        tmin=ttime-((ttime/100)*100)+15
        IF(tmin.gt.45) then
          tmin=0
          ttime=(ttime/100+1)*100+tmin
          tztime=(tztime/100+1)*100+tmin
          IF(tztime.ge.2400) then
            tztime=tztime-2400
            nxtday=.true.
          else
            nxtday=.false.
          endif
        else
          ttime=(ttime/100)*100+tmin
          tztime=(tztime/100)*100+tmin
        endif
        IF(swftim(i).gt.ttime) GO TO 82
      endif
      GO TO 80

 85     IF(n.gt.0) then
          do j=1,datnum+3
            IF(anum(j).gt.0) then
              a(j)=a(j)/(anum(j)*1.0)
            else
              a(j)=er(j)
            endif
          end do
          stdev2=0.0
          IF(anum(datnum+3).gt.1) then
            do j=1,anum(datnum+3)
              stdev2=stdev2+(tmp(j)-a(datnum+3))**2
            end do
            stdev2=SQRT(stdev2/anum(datnum+3))
          else
            stdev2=-0.99
          endif
          IF(stdev2.gt.1.0) stdev2=-0.99
          IF(swcflg.eq.1.or.swcflg.eq.3) then
            WRITE(3,81)tzdate,gmttim,tdate,lsttim,a(datnum+1),stdev2,
     %a(datnum+3),anum(datnum+3),anum(4),anum(13),totclr,
     %(a(c),c=1,datnum)
          endif
          WRITE(7,81)tzdate,gmttim,tdate,lsttim,a(datnum+1),stdev2,
     %a(datnum+3),anum(datnum+3),anum(4),anum(13),totclr,
     %(a(c),c=1,datnum)
        endif
        do j=1,datnum+3
          a(j)=0.0
        end do
        IF(fitn.gt.0) then
          favdc=favdc/fitn
          favcf=favcf/fitn
          favcz=favcz/fitn
        endif
        IF(fit.and.fitn.gt.0) then
          WRITE(88,83) tdate,lsttim,favcz,favdc,favcf,fitn,totclr
        endif
        n=0
        r=0
        CLOSE(UNIT=3)
      GO TO 10

 98   WRITE(6,*)'======================================================'
      WRITE(6,*)'=======  Error opening "',cnffil,'"   =============='
      WRITE(6,*)'     >>>>>  hit ENTER to continue  <<<<<'
      READ(5,*)
      GO TO 100

 99   WRITE(6,*)'======================================================'
      WRITE(6,*)'=========  Error opening "swf.dir"   ================='
      WRITE(6,*)'     >>>>>  hit ENTER to continue  <<<<<'
      READ(5,*)

 100  CLOSE(UNIT=1)
      CLOSE(UNIT=9)
      CLOSE(UNIT=7)
      CLOSE(UNIT=88)
      stop
      END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
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
      PARAMETER (NMAX=1000000)
      EXTERNAL ROFUNC
      DIMENSION X(NDATA),Y(NDATA)
      COMMON /ARRAYS/ NDATAT,XT(NMAX),YT(NMAX),ARR(NMAX),AA,ABDEVT

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
	GO TO 20
      ENDIF
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
	GO TO 30
      ENDIF
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
      PARAMETER (NMAX=1000000)
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


