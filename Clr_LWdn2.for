C     Last change: JA 11/21/2016 10:38:55 AM
      program swccir
      PARAMETER (NMAX=140000)
      PARAMETER (NMAX2=140000)
      integer dayn,zdat,ztim,ldat,ltim,p,nx,i,clrn,j,h1,h2,idata(1500,8)
      INTEGER nTa,nRH,cflg,clr,n24,n,Nvp,typv,cdat(nmax),a,naav(75),ddt
      INTEGER nlw,cn,typT,typR,clim,nc,fdat,c,rhf,s,e,sflg,nlwu,nx2,xdc
      INTEGER tlhr,tzhr,tmin,h15,tclr,ttim,lastim,alim,dn(75),dclr,nls
      INTEGER dy,mn,yr,datres,drn,dmin,lhr,nn,dstrt,dend,ds(nmax),ne,ndc
      integer de(nmax),q,step,imid,n1num,n2num,gflg,dflg,rflg,nalb,ncalb
      INTEGER nscv,ntau,nlscv,nct,nch,ncz2,nWspd,ug,kk,hst(52),tott,ceql
      INTEGER ltn1,ltn2,ltd,cerr,conum,tflg
      REAL*8 x(0:55),es,sigma,errlm(55),av,vprs,av2,ocof(nmax),rtim
      REAL*8 RHa,RHb,Ta,RH,xarr(nmax),scv,ucof,Ec,LW,rhlim,cbh,aav(75)
      REAL*8 coeff(nmax),std,cof,RHfac,Clw,sarr(nmax),rdata(1500,64)
      REAL*8 Tco,scvlim,dalb,rerr,swu,cswu,lwu,parr(0:55),au,pi,ed,eavg
      REAL*8 tau,alb,cosz,dif,csw,ssw,r,stlim,slim,xvprs,swtr,tcof,uE
      REAL*8 f0,f1,f2,f3,za,zb,zc,lscv,tdif,tarr(2,25),Wspd,ussw,ueavg
      REAL*8 aadv(10),swcer(20),lwce(20),difce(20),ascv(20),alw(nmax2)
      REAL*8 cdif,dir,cdir,dra,drb,swa,swb,sxa,sxb,lat,xlong,long,Rflim
      REAL*8 az,z,hr,dec,maxLW,s15(20),l15(20),d15(20),g,gi,Tclim,Telim
      REAL*8 delcof,em,Te,n2arr(nmax),n2av,ncof,elim,lslim,tdlim,uz
      REAL*8 incr1,incr2,adjz90,lcosz,lcdifr,cdifr,cslope,lslope,ucosz
      REAL*8 ARH(1000000),Tdf(1000000),mnTdif,avTdif,lstd(nmax2),dav(75)
      REAL*8 rhmin,fcof(nmax),sncz,tstrhf,ostd,clwu(1500),slscv(1500)
      CHARACTER*14 dirfil,cnffil,infil,outfil,coffil,cof1,clrfil,dayfil
      CHARACTER*14 allfil,ngtfil,cof2
      CHARACTER*180 path
      CHARACTER*500 head1,head2,head15,headd
      LOGICAL OK,clrout,oneout,n1,n2,lwok1,lwok2

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      OPEN(UNIT=223,FILE='Clr_LWdn_tst.asc',STATUS='unknown')
      WRITE(223,*)'   ldat    ltim     rflim        rh    oldrhfac    ne
     %wrhfac'
ccc      WRITE(223,*)'reason     ldat    ltim         std       lslim
ccc     %       tdif       tdlim          lw        clw          cof'
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      outfil='Clr_LWdn.asc'
      dirfil='Clr_LWdn2.dir'
      cnffil='Clr_LWdn2.cnf'
      clrfil='Clr_LWdn.clr'
      allfil='ClrLWd15.a15'
      ngtfil='Clr_LWdn.ngt'
      DAYFIL='Clr_LWdn.day'
      COFFIL='Clr_LWdn.cof'
      COF1='Clr_LWdn.c1f'
      COF2='Clr_LWdn.c2f'
      ok=.true.
      clrout=.false.
      oneout=.false.
      lwok1=.false.
      lwok2=.false.
      conum=0
      n=0
      sigma=0.0000000567000
      x(0)=-9999.0
      rerr=-9999.0
      pi=dacos(-1.d0)
      g=0.867
      incr1= 0.571428571
      incr2= 0.276497696
      adjz90= 89.14285714

      do i=1,nmax
         coeff(i)=0.0
         ocof(i)=0.0
         xarr(i)=-9999.0
         sarr(i)=-9999.0
      end do

      do i=1,nmax2
         lstd(i)=0.0
         alw(i)=0.0
      end do

      nx=0
      tott=0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc      OPEN(UNIT=77,file='test.asc',STATUS='unknown')
cc      WRITE(77,*)' ltim,em,te,em/ec,std,ta-te  '
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      cerr=0

      OPEN(UNIT=1,FILE=cnffil,STATUS='old',ERR=96)
      READ(1,1,ERR=97) path
 1      format(a180)
        p=index(path,' ')
        p=p-1
        WRITE(6,*)'path= ',path(1:p),'<<<<<'
      cerr=cerr+1
      READ(1,*)
      cerr=cerr+1
      READ(1,7,ERR=97) head2
      cerr=cerr+1
      READ(1,*,ERR=97) i
      cerr=cerr+1
      IF(i.eq.1.or.i.eq.3) clrout=.true.
cc      IF(i.eq.1.or.i.eq.3) write(6,*)'  Clear Out set, clrout= ',clrout
cc      IF(i.eq.1.or.i.eq.3) pause
      IF(i.eq.1.or.i.eq.3) conum=1
      IF(i.eq.2.or.i.eq.3) oneout=.true.
      READ(1,*,ERR=97) rhf, rhlim, RFlim
      cerr=cerr+1
      READ(1,*,ERR=97) clim,alim,datres
      cerr=cerr+1
      READ(1,*,ERR=97) scvlim,slim
      cerr=cerr+1
      READ(1,*,ERR=97) tcof,g,gi
      cerr=cerr+1
      IF(g.lt.0.5.or.g.gt.0.98.or.gi.lt.0.5.or.gi.gt.0.98) then
        WRITE(6,*)'ggggggggggggggggggggggggggggggggggggggggggggggggggg'
        WRITE(6,*)'   assymetry parameter set outside normal bounds'
        WRITE(6,*)'   g= ',g
        WRITE(6,*)'   gi= ',gi
        WRITE(6,*)'ggggggggggggggggggggggggggggggggggggggggggggggggggg'
        pause
      endif
      READ(1,*,ERR=97) tclim,ug
      cerr=cerr+1
      IF(tclim.lt.150.0.or.ug.gt.1.or.ug.lt.0) then
        WRITE(6,*)'****************************************************'
        WRITE(6,*)'   Problem with Tcld liq/ice temperature limit or'
        WRITE(6,*)'   use Tcld and Te for which g (0/1 for n/y)'
        WRITE(6,*)'   tclim= ',tclim
        WRITE(6,*)'   ug= ',ug
        WRITE(6,*)'   NOTE: tclim must be in K!'
        WRITE(6,*)'****************************************************'
        pause
      endif
cccccccccccccccccccccccc which total SW to use   cccccccccccccccccccccc
      READ(1,*,ERR=97) dalb,stlim,sflg,tflg
      cerr=cerr+1
      READ(1,*,ERR=97) elim,lslim,tdlim,ceql
      cerr=cerr+1
      IF(ceql.gt.3) then
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(6,*)' Flag for setting CLW=LW if CLW>LW is'
        WRITE(6,*)'  set to a value >3:', ceql
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        pause
      endif

      READ(1,*,ERR=97) nx
      cerr=cerr+1
      IF(nx.gt.40) then
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(6,*)' Max allowable number of extra variables is 40!'
        WRITE(6,*)' nx (# of xtra vars) set to 40 for this run'
        WRITE(6,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        nx=40
        pause
      endif
      READ(1,*,ERR=97) nTa, typT
      cerr=cerr+1
      READ(1,*,ERR=94) nRH, typR, rhmin
      cerr=cerr+1
      READ(1,*,ERR=97) nVp, typV
      cerr=cerr+1
      READ(1,*,ERR=97) nlw
      cerr=cerr+1
      READ(1,*,ERR=97) nlwu
      cerr=cerr+1
      READ(1,*,ERR=97) nwspd
      cerr=cerr+1
      do i=1,nx
        READ(1,*,ERR=97) errlm(i)
        cerr=cerr+1
      end do
      CLOSE(UNIT=1)

ccccccc Added 20131216, flags for setting CLW=LW oif clr or LW<CLW ccccccccccccc

      if(ceql.eq.1.or.ceql.eq.3) lwok1=.true.
      if(ceql.eq.2.or.ceql.eq.3) lwok2=.true.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      step=11/datres
      IF(step.gt.10) step=10

      IF(sflg.gt.0) then
      head1 ='  Zdate  Ztim     Ldate  Ltim         CosZ           AU
     %  SWdn    CSWdn     LWdn    CLWdn     SWup    CSWup     LWup    CL
     %Wup    DifSW   CDifSW    DirSW   CDirSW  ClrF  TauF  TlmF     LWSc
     %v     SWScv    CldTau    CldTrn     TeLim      LWTe    CldTmp    C
     %ldHgt      Tair      VPrs        RH       RHfac          Ec
     % Wspd'

      head15='  Zdate  Ztim     Ldate  Ltim         CosZ           AU
     %  SWdn    CSWdn     LWdn    CLWdn     SWup    CSWup     LWup    Di
     %fSW   CDifSW    DirSW   CDirSW     N  NClr    AvTFlg     LWScv
     % SWScv    ScvADV    CldTau    CldTrn     TeLim      LWTe    CldTmp
     %    CldHgt      Tair      VPrs        RH       RHfac          Ec
     %     SWcer    SWcerADV        LWce     LWceADV       Difce    Difc
     %eADV     SWdnADV     LWdnADV      DifADV     MaxLWdn        Wspd
     %      Wdir'

      headd ='  Ldate    DayN    DltN     SWN    DifN     LWN    CLWN gf
     %lg dflg Rflg         CosZ           AU     SWdn    CSWdn     LWdn
     %   CLWdn     SWup    CSWup     LWup    DifSW   CDifSW    DirSW   C
     %DirSW  ClrN     LWScv     SWScv    CldTau    CldTrn    CldTmp    C
     %ldHgt      Tair      VPrs          RH       RHfac          Ec
     %   Wspd'
      else
      head1 ='  Zdate  Ztim     Ldate  Ltim         CosZ           AU
     %  SWdn    CSWdn     LWdn    CLWdn     SolX    CSolX     LWup    CL
     %Wup    DifSW   CDifSW    DirSW   CDirSW  ClrF  TauF  TlmF     LWSc
     %v     SWScv    CldTau    CldTrn     TeLim      LWTe    CldTmp    C
     %ldHgt      Tair      VPrs        RH       RHfac          Ec
     % Wspd'

      head15='  Zdate  Ztim     Ldate  Ltim         CosZ           AU
     %  SWdn    CSWdn     LWdn    CLWdn     SolX    CSolX     LWup    Di
     %fSW   CDifSW    DirSW   CDirSW     N  NClr    AvTFlg     LWScv
     % SWScv    ScvADV    CldTau    CldTrn     TeLim      LWTe    CldTmp
     %    CldHgt      Tair      VPrs        RH       RHfac          Ec
     %     SWcer    SWcerADV        LWce     LWceADV       Difce    Difc
     %eADV     SWdnADV     LWdnADV      DifADV     MaxLWdn        Wspd
     %      Wdir'

      headd ='  Ldate    DayN    DltN     SWN    DifN     LWN    CLWN gf
     %lg dflg Rflg         CosZ           AU     SWdn    CSWdn     LWdn
     %   CLWdn     SolX    CSolX     LWup    DifSW   CDifSW    DirSW   C
     %DirSW  ClrN     LWScv     SWScv    CldTau    CldTrn    CldTmp    C
     %ldHgt      Tair      VPrs          RH       RHfac          Ec
     %   Wspd'
      endif


      do h1=500,1,-1
        IF(head1(h1:h1).ne.' ') GO TO 2
      end do

 2    do h2=500,1,-1
        IF(head2(h2:h2).ne.' ') GO TO 3
      end do

 3    do h15=500,1,-1
        IF(head15(h15:h15).ne.' ') GO TO 4
      end do

 4    do i=500,1,-1
        IF(headd(i:i).ne.' ') GO TO 8
      end do
 8    OPEN(UNIT=33,FILE=dayfil,STATUS='unknown')
      WRITE(33,*) headd(1:i)//head2(1:h2)

      IF(oneout) then
        OPEN(UNIT=11,FILE=allfil,STATUS='unknown')
        OPEN(UNIT=22,FILE=ngtfil,STATUS='unknown')
        WRITE(11,*) head15(1:h15)//head2(1:h2)
        WRITE(22,*) head15(1:h15)//head2(1:h2)
      endif

cccccccccccccccccccccccccccccccccccccccccccc
cc      write(77,*) head1(1:h1)//head2(1:h2)
cccccccccccccccccccccccccccccccccccccccccccc

      IF(rhf.eq.1) then
cc        call calcrhfac(path,p,errlm,clim,nx,nta,typt,nrh,typr,
cc     %nvp,typv,nlw,rhlim,rhmin,rflim,clrout,rha,rhb)
        call calcrhfac(path,p,errlm,clim,nx,nta,typt,nrh,typr,
     %nvp,typv,nlw,rhlim,rhmin,rflim,conum,rha,rhb)
      elseif(rhf.eq.2) then
        RHa=0.00000000225400
        RHb=3.634
      elseif(rhf.eq.3) then
        OPEN(UNIT=55,FILE='Clr_LWdn.rhc',ERR=95,STATUS='old')
        read(55,*)
        read(55,*)
        read(55,*) RHa,RHb
        WRITE(6,*)'  Read RHa and RHb from file:',RHa,RHb
        pause
        CLOSE(UNIT=55)
      else
        RHa=0.0d0
        RHb=0.0d0
      endif

ccc   outfil='Clr_LWdn.asc'
      OPEN(UNIT=3,FILE=outfil,STATUS='unknown')
      OPEN(UNIT=8,FILE='Clr_LWdn1.asc',STATUS='unknown')

      nls=0

      OPEN(UNIT=1,FILE=dirfil,STATUS='old',ERR=99)
 5    READ(1,6,END=100) infil
 6    FORMAT(a12)
 7    format(a500)

      clrn=0
      dayn=0
      n24=0
      cn=0
      av=0.0
      av2=0.0
      std=0.0
      j=0
      i=0
      ddt=0

      IF(p.gt.0) then
        OPEN(UNIT=2,FILE=path(1:p)//infil,STATUS='old')
        WRITE(6,*)'Opened path//infil: ',path(1:p)//infil
      else
        OPEN(UNIT=2,FILE=infil,STATUS='old')
        WRITE(6,*)'Opened infil: ',infil
      endif
      READ(2,*,END=50)
      READ(2,*,END=50)
      READ(2,*,END=50)
      READ(2,*,END=50)
      n=n+1
      IF(ok) then
        WRITE(3,*)' ldate   n24   dayN   ClrN   CLWn    Coef  StDev'
        WRITE(8,*)' ldate   n24   dayN   ClrN   CLWn    Coef     StDev
     % OStd'
        ok=.false.
      endif

 10   READ(2,*,END=50,ERR=89) zdat,ztim,ldat,ltim,scv,cflg,clr,
     %(x(i),i=1,17),(x(i),i=18,17+nx)
       n24=n24+1

       IF(nTa.gt.0) then
         Ta=x(17+nTa)
         IF(typT.eq.1.and.Ta.gt.errlm(nta)) then
           Ta=Ta+273.15
         ELSEIF(typT.eq.3.and.Ta.gt.errlm(nta)) then
           Ta=Ta+40.0
           Ta=(Ta*5.0)/9.0
           Ta=Ta-40.0
           Ta=Ta+273.15
         ELSEIF(Ta.le.errlm(nta)) then
           Ta=rerr
         endif
       else
         Ta=rerr
       endif
       IF(Ta.gt.400.0.or.Ta.lt.100.0) Ta=rerr

       IF(nrh.gt.0) then
         RH=x(17+nRH)
         IF(typR.eq.2.and.RH.gt.errlm(nrh)) then
           rh=rh*100.0
         ELSEIF(rh.le.errlm(nrh)) then
           rh=rerr
         endif
       else
         rh=rerr
       endif
       IF(rh.gt.115.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       ELSEIF(rh.lt.rhmin) then
         rh=rerr
       endif

       IF(nvp.gt.0) then
         vprs=x(17+nvp)
         IF(typv.eq.2.and.vprs.gt.errlm(nvp)) then
           vprs=vprs*10.0
         ELSEIF(vprs.le.errlm(nvp)) then
           vprs=rerr
         endif
       ELSEIF(Ta.gt.0.0.and.RH.ge.2.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         vprs=es*rh/100.0
       else
         vprs=rerr
       endif
       IF(vprs.gt.60.0) vprs=rerr

       IF(rh.lt.0.0.and.vprs.ge.0.0.and.ta.gt.0.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         rh=(vprs/es)*100.0
       endif
       IF(rh.gt.115.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       endif

cccccccccccc check for RH and vprs bad data  ccccccccccccccccccc
         IF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.gt.0.and.Ta.gt.0.0) then
           es=6.1173*EXP(19.83923-(5419.289/(Ta)))
           xvprs=es*rh/100.0
           IF(abs(xvprs-vprs).lt.0.1) then
             vprs=rerr
             rh=rerr
           endif
         ELSEIF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.eq.0) then
             vprs=rerr
             rh=rerr
         endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       IF(nlw.gt.0) then
         clw=x(17+nlw)
         IF(clw.le.errlm(nlw)) clw=rerr
       else
         clw=rerr
       endif

       IF(clw.gt.550.0) clw=rerr

       cosz=x(1)
       IF(x(1).ge.0.0) dayn=dayn+1

       IF(clr.eq.1) then
         clrn=clrn+1
         IF(clw.gt.0.0.and.ddt.lt.nmax) then
           ddt=ddt+1
           alw(ddt)=clw
           lhr=ltim/100
           de(ddt)=(lhr*60)+(ltim-lhr*100)
         endif
         IF(vprs.gt.0.0.and.ta.gt.0.0.and.clw.gt.0.0) then
           cof=clw/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
           IF(rh.ge.0.0) then
             RHfac=RHa*(RH)**RHb
             tstrhf=(rhfac*((vprs/ta)**(1./7.)))*sigma*ta**4
             IF(tstrhf.gt.rflim) then
               RHfac=rflim/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
             endif
           else
             rhfac=0.0
           endif
           cof=cof-rhfac
           av=av+cof
           cn=cn+1
           xarr(cn)=cof
           Te=(clw/sigma)**0.25
           IF(rh.ge.0.0.AND.(Ta-Te).gt.0.0.and.tott.lt.1000000) then
             tott=tott+1
             tdf(tott)=Ta-Te
             arh(tott)=RH
             avTdif=avTdif+(Ta-Te)
           endif
         endif
       endif

       GO TO 10

 50   CLOSE(UNIT=2)

      IF(cn.ge.clim) then
        av=av/cn*1.0

        do i=1,cn
          std=std+(xarr(i)-av)**2
        end do
        std=SQRT(std/cn)

        j=0
        av2=0.0
cccccc    added Xarr>av-2*std on 20091230  ccccccccccccc
        do i=1,cn
          IF(ABS(xarr(i)-av).lt.std*2.0.and.xarr(i).Gt.(av-std*2.0))then
            av2=av2+xarr(i)
            j=j+1
            sarr(j)=xarr(i)
          endif
        end do
        av2=av2/j*1.0
        std=0.0
        do i=1,j
          std=std+(av2-sarr(i))**2
        end do
        std=SQRT(std/j)
        ostd=std

        av=0.0
        cn=0
        do i=1,j
         IF(ABS(sarr(i)-av2).lt.std*1.5.and.sarr(i).Gt.(av-std*1.5))then
            av=av+sarr(i)
            cn=cn+1
            xarr(cn)=sarr(i)
          endif
        end do
        av=av/cn*1.0

        std=0.0
        do i=1,cn
          std=std+(xarr(i)-av)**2
        end do
        std=SQRT(std/cn)

        IF(av.LT.-9999.0.or.av.gt.9999.0) av=-9.0
        IF(std.LT.-9999.0.or.std.gt.9999.0) std=-9.0

      else
        av=-9.0
        std=-9.0
      endif

      WRITE(3,55) ldat,n24,dayn,clrn,cn,av,std
      WRITE(8,55) ldat,n24,dayn,clrn,cn,av,std,ostd
 55   FORMAT(i8,4i6,8f10.4)
 56   FORMAT(i8,4i6,3f10.4,8I8)

      IF(ddt.gt.30/datres) then
        do i=(step+1),(ddt-step)
          av2=0.0
          kk=0
          e=de(i)+10
          s=de(i)-10
          do j=(i-step),(i+step)
            IF(de(j).le.e.and.de(j).ge.s) then
              av2=av2+alw(j)
              kk=kk+1
            endif
          end do

          IF(kk.ge.step) then
            av2=av2/kk
            nls=nls+1
            IF(nls.lt.nmax2) then
              do j=(i-step),(i+step)
                IF(de(j).le.e.and.de(j).ge.s) then
                  lstd(nls)=lstd(nls)+(alw(j)-av2)**2
                endif
              end do
              lstd(nls)=sqrt(lstd(nls)/kk)
            endif
          endif
        end do
      endif

      GO TO 5

 89   WRITE(6,*)'  last line read:'
      WRITE(6,*) zdat,ztim,ldat,ltim,scv,cflg,clr,
     %(x(i),i=1,17)
      pause
      GO TO 400

 94   write(6,*)'#####################################################'
      write(6,*)'   <<<<<< Error reading configuration file >>>>>>>'
      WRITE(6,*)' Did you include new minimum allowable RH setting? '
      write(6,*)'  Last good read line:',cerr
      pause
      go to 98

 95   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'   <<<<<< Error opening "Clr_LWdn.rhc" file >>>>>>>'
      write(6,*)'   This file must be in the same directory'
      write(6,*)'   as the executable file if RH factor flag.'
      write(6,*)'   is set to a value of "3"'
      pause
      go to 400


 96   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'   <<<<<< Error opening configuration file >>>>>>>'
      write(6,*)'The file "',cnffil,'" must be in the same directory'
      write(6,*)'as the executable file.'
      GO TO 98

 97   write(6,*)'#####################################################'
      write(6,*)'   <<<<<< Error reading configuration file >>>>>>>'
      write(6,*)'  Last good read line:',cerr
 98   write(6,*)'  A sample configuration file named: '
      write(6,*)' "Clr_LWdn2.cnf.sample" has been created '
      write(6,*)'#####################################################'

      OPEN(UNIT=57,FILE='Clr_LWdn2.cnf.sample',STATUS='unknown')
      WRITE(57,*)'c:\swc\     *path to input files (incl final "\")'
      WRITE(57,*)'   ****** the following line is the XtraVar header lin
     %e, 11 char spacing for each declared XtraVar, max 400 tot char ***
     %*****'
      WRITE(57,*)'        Aprs    Wdir     Precip '
      WRITE(57,*)'2                    * extra output file flag (0/1/2/3
     %) for (none/allclr/15-minute/both files)'
      WRITE(57,*)'1 75.0 35.0      * RH factor flag (0/1/2/3 for [none]/
     %[calc coeff]/[use std coeff]/[use prev coeff]), RHlim for separati
     %on of data (real, %), max RHFlux limit (set to -99 to bypass)'
      WRITE(57,*)'30 5 1         * min num to calc LW clear-sky avgs, mi
     %n num per 15-min for L15 files, # min data resolution'
      WRITE(57,*)'0.5 1.5                * min LW sky cover limit for ca
     %lc of CBT, max allowed n2d2n interp stdev multiplier (real)'
      WRITE(57,*)'1.16 0.87 0.8      * tau eq coefficient, assymetry par
     %ameters for liq and ice (first is default) (ex: 1.16 0.87 0.8)'
      WRITE(57,*)'248.0 1           * Tcld liq/ice temperature limit (K)
     %, use Tcld and Te for which g (0/1 for n/y) (ex: 248.0  1)'
      WRITE(57,*)'0.18 0.95 2 0        *  default albedo, min sky cover
     %limits for cloud tau calc, SWup flag (0/1/2=n/alb/swup), TSW flag
     %(0/1 to use Sum/Global as primary SW)'
      WRITE(57,*)'1.08 1.0 12.0 1    *limits for LW clear detection: e r
     %atio limit (le), LW StDEv limit (le), Ta-Te difference limit (gt),
     % if CLW>LW then set CLW=LW flag (0/1 for n/y)'
      WRITE(57,*)'6                    * nx - number of extra variables
     %in *.swc file to include (0-40 max)'
      WRITE(57,*)'3 2                  * nTa - of nx, which is the locat
     %ion of Ta (0 for none), T Type (1/2/3 for C/K/F)'
      WRITE(57,*)'4 1 10.0             * nRH - of nx, which is the locat
     %ion of RH (0 for none), RH type (1/2 for %/Ratio), min allowable R
     %H (%)'
      WRITE(57,*)'0 1                  * nVprs - of nx, which is the loc
     %ation of VPrs (0 for none), Vprs type (1/2 for mb/Kpsc)'
      WRITE(57,*)'1                    * nLWdn - of nx, which is the loc
     %ation of LWdn (0 for none)'
      WRITE(57,*)'2                    * nLWup - of nx, which is the loc
     %ation of LWup (0 for none)'
      WRITE(57,*)'6                    * nWspd - of nx, which is the loc
     %ation of Wspd (0 for none)'
      WRITE(57,*)'-98.0         * errlim - limit check for xtravar1 "bad
     %data", must be > actual '
      WRITE(57,*)'-98.0         * errlim - limit check for xtravar2 "bad
     %data", must be > actual '
      WRITE(57,*)'-98.0         * errlim - limit check for xtravar3 "bad
     %data", must be > actual '
      WRITE(57,*)'-98.0         * errlim - limit check for xtravar4 "bad
     %data", must be > actual '
      WRITE(57,*)'-98.0         * errlim - limit check for xtravar5 "bad
     %data", must be > actual '
      WRITE(57,*)'-98.0         * errlim - limit check for xtravar6 "bad
     %data", must be > actual '
      WRITE(57,*)'-98.0         * errlim - limit check for xtravar7 "bad
     %data", must be > actual '
      WRITE(57,*)'-98.0         * errlim - limit check for xtravar8 "bad
     %data", must be > actual '
      WRITE(57,*)'-98.0         * errlim - limit check for xtravar9 "bad
     %data", must be > actual '
      WRITE(57,*)'-98.0        * errlim - limit check for xtravar10 "bad
     % data", must be > actual '
      WRITE(57,*)'-98.0        * errlim - limit check for xtravar11 "bad
     % data", must be > actual '
      WRITE(57,*)'-98.0        * errlim - limit check for xtravar12 "bad
     % data", must be > actual '
      WRITE(57,*)'-98.0        * errlim - limit check for xtravar13 "bad
     % data", must be > actual '
      WRITE(57,*)'-998.0       * errlim - limit check for xtravar14 "bad
     % data", must be > actual '
      WRITE(57,*)'-998.0       * errlim - limit check for xtravar15 "bad
     % data", must be > actual '
      WRITE(57,*)'-998.0       * errlim - limit check for xtravar16 "bad
     % data", must be > actual '
      WRITE(57,*)'-998.0       * errlim - limit check for xtravar17 "bad
     % data", must be > actual '
      WRITE(57,*)'-998.0       * errlim - limit check for xtravar18 "bad
     % data", must be > actual '
      WRITE(57,*)'-998.0       * errlim - limit check for xtravar19 "bad
     % data", must be > actual '
      WRITE(57,*)'-998.0       * errlim - limit check for xtravar20 "bad
     % data", must be > actual '
      WRITE(57,*)' '
      WRITE(57,*)' '
      CLOSE(UNIT=57)
      pause
      go to 400

 99   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'   <<<<<< Error opening file list file >>>>>>>'
      write(6,*)'The file "',dirfil,'" must be in the same directory'
      write(6,*)'as the executable file.'
      write(6,*)'?????????????????????????????????????????????????????'
      pause
      go to 400

 100  CLOSE(UNIT=1)
      CLOSE(UNIT=3)
      CLOSE(UNIT=8)

cccccccccccccccc  avg/min/max Ta-Te, LW StDev, RH hist diagnostics  ccccccccccccccc

      IF(tott.gt.0) then
        avTdif=AvTdif/tott
        call sort(tott,Tdf)
        kk=0.1*tott
        mnTdif=0.0
        do i=1,kk
          mnTdif=mnTdif+Tdf(i)
        end do
        mnTdif=mnTdif/(kk)
        do i=1,52
          hst(i)=0
        end do
        do i=1,tott
          hst(INT(ARH(i)/2))=hst(INT(ARH(i)/2))+1
        end do

        OPEN(UNIT=222,FILE='Clr_LWdn_diagnostics.asc',STATUS='unknown')
        WRITE(222,*)' #data, #mindata:',tott,kk
        WRITE(222,*)'SWclr Ta-Te Avg & min10%:',avTdif, mnTdif

        IF(nls.ge.nmax2) nls=nmax2-1
        lstd(nmax2)= 0.0
        IF(nls.gt.0) then
          do i=1,nls
            lstd(nmax2)= lstd(nmax2)+lstd(i)
          end do
          lstd(nmax2)=lstd(nmax2)/nls

          call sort(nls,lstd)
          kk=0.1*nls
          mnTdif=0.0
          do i=nls,nls-kk,-1
            mnTdif=mnTdif+lstd(i)
          end do
          mnTdif=mnTdif/(kk)
        else
          kk=0
          lstd(nmax2)=-9999.0
          mnTdif=-9999.0
        endif

        WRITE(222,*)' '
        WRITE(222,*)' #data, #maxdata:',nls,kk
        WRITE(222,*)' SWclr Avg & max10% LW StDev:',lstd(nmax2),mnTdif
        WRITE(222,*)' '
        WRITE(222,*)' Daylight clear-sky RH Histogram:'
        WRITE(222,*)' BinLim        #'
        do i=1,50
          WRITE(222,*) (i+1)*2,hst(i)
        end do
        CLOSE(UNIT=222)
      endif
cccccccccccccccc end avg/min/max Ta-Te, LW StDev, RH hist diagnostics  ccccccccccccccc

ccc      COF1='Clr_LWdn.c1f'
ccc   outfil='Clr_LWdn.asc'  [saved for later reference as Clr_LWdn1.asc]
      call clrcof1(outfil,cof1)

cccccc  end 1st iteration of daylight coefficient fitting and interpolation

ccc    now re-read the data with the interpolated daylight coeffs,
ccc    look for LW less than calc clear LW
ccc      if found, include in re-calc of daylight lapse rate coeffs.


ccc      COF1='Clr_LWdn.c1f'
      OPEN(UNIT=1,FILE=cof1,STATUS='old')
      nc=1
 103  READ(1,*,END=104) cdat(nc),coeff(nc),ocof(nc)
      nc=nc+1
      GO TO 103

 104  CLOSE(UNIT=1)
      nc=nc-1
      ok=.true.

      OPEN(UNIT=8,FILE='Clr_LWdn2.asc',STATUS='unknown')
ccc   outfil='Clr_LWdn.asc'  [saved for later reference as Clr_LWdn2.asc]
      OPEN(UNIT=3,FILE=outfil,STATUS='unknown')
      OPEN(UNIT=1,FILE=dirfil,STATUS='old')
 105  READ(1,6,END=200) infil
      IF(p.gt.0) then
        OPEN(UNIT=2,FILE=path(1:p)//infil,STATUS='old')
        WRITE(6,*)'Opened path//infil: ',path(1:p)//infil
      else
        OPEN(UNIT=2,FILE=infil,STATUS='old')
        WRITE(6,*)'Opened infil: ',infil
      endif
      READ(2,*,END=150)
      READ(2,*,END=150)
      READ(2,*,END=150)
      READ(2,*,END=150)
      READ(infil(1:8),*) fdat
        do c=1,nc
          IF(cdat(c).eq.fdat) GO TO 107
        end do
        WRITE(6,*)'cdat.ne.fdat #1: ',cdat(c),fdat,c
        WRITE(6,*)'cdat(1),cdat(c-1): ',cdat(1),cdat(c-1)
        pause
        GO TO 200
 107  cn=0
      av=0.0
      std=0.0
      dayn=0
      n24=0
      ndc=0
      j=0
      imid=11/datres
      ltd=0
ccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc      subtract 1 from imid to counter array rotation
      imid=imid-1
      step=21/datres
      IF(step.gt.20) step=20

        DO i=1,25
          tarr(1,i)=-9.0
          tarr(2,i)=-9.0
        END DO

      IF(ok) then
        WRITE(3,*)'  ldate   n24  dltN  OrgN  FinN      Coef     StDev
     %  OrgCof     ltd'
        WRITE(8,*)'  ldate   n24  dltN  OrgN  FinN      Coef     StDev
     %  OrgCof    #L<C    #CLW     xdc       j'
        ok=.false.
      endif

 110  READ(2,*,END=150,ERR=89) zdat,ztim,ldat,ltim,scv,cflg,clr,
     %(x(i),i=1,17),(x(i),i=18,17+nx)

       n24=n24+1
       cosz=x(1)
       IF(x(1).ge.0.0) dayn=dayn+1

       IF(nTa.gt.0) then
         Ta=x(17+nTa)
         IF(typT.eq.1.and.Ta.gt.errlm(nta)) then
           Ta=Ta+273.15
         ELSEIF(typT.eq.3.and.Ta.gt.errlm(nta)) then
           Ta=Ta+40.0
           Ta=(Ta*5.0)/9.0
           Ta=Ta-40.0
           Ta=Ta+273.15
         ELSEIF(Ta.le.errlm(nta)) then
           Ta=rerr
         endif
       else
         Ta=rerr
       endif
       IF(Ta.gt.400.0.or.Ta.lt.100.0) Ta=rerr

       IF(nrh.gt.0) then
         RH=x(17+nRH)
         IF(typR.eq.2.and.RH.gt.errlm(nrh)) then
           rh=rh*100.0
         ELSEIF(rh.le.errlm(nrh)) then
           rh=rerr
         endif
       else
         rh=rerr
       endif
       IF(rh.gt.110.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       ELSEIF(rh.lt.rhmin) then
         rh=rerr
       endif

       IF(nvp.gt.0) then
         vprs=x(17+nvp)
         IF(typv.eq.2.and.vprs.gt.errlm(nvp)) then
           vprs=vprs*10.0
         ELSEIF(vprs.le.errlm(nvp)) then
           vprs=rerr
         endif
       ELSEIF(Ta.gt.0.0.and.RH.ge.2.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         vprs=es*rh/100.0
       else
         vprs=rerr
       endif
       IF(vprs.gt.60.0) vprs=rerr

       IF(rh.lt.0.0.and.vprs.ge.0.0.and.ta.gt.0.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         rh=(vprs/es)*100.0
       endif
       IF(rh.gt.110.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       endif

cccccccccccc check for RH and vprs bad data  ccccccccccccccccccc
         IF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.gt.0.and.Ta.gt.0.0) then
           es=6.1173*EXP(19.83923-(5419.289/(Ta)))
           xvprs=es*rh/100.0
           IF(abs(xvprs-vprs).lt.0.1) then
             vprs=rerr
             rh=rerr
           endif
         ELSEIF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.eq.0) then
             vprs=rerr
             rh=rerr
         endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       IF(nlw.gt.0) then
         lw=x(17+nlw)
         IF(lw.le.errlm(nlw)) lw=rerr
       else
         lw=rerr
       endif
       IF(lw.gt.600.0) lw=rerr

      IF(Ta.gt.0.0.and.RH.ge.0.0.and.vprs.ge.0.0) then
        rhfac=rha*(rh)**rhb
             tstrhf=(rhfac*((vprs/ta)**(1./7.)))*sigma*ta**4
             IF(tstrhf.gt.rflim) then
               RHfac=rflim/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
             endif
        Ec=(coeff(c)+rhfac)*((vprs/ta)**(1./7.))
        if(Ec.gt.1.d0) Ec=1.d0
        clw=Ec*sigma*ta**4
      elseIF(Ta.gt.0.0.and.vprs.ge.0.0) then
        rhfac=0.0
        Ec=(coeff(c)+rhfac)*((vprs/ta)**(1./7.))
        if(Ec.gt.1.d0) Ec=1.d0
        clw=Ec*sigma*ta**4
      else
        clw=rerr
        Ec=rerr
        rhfac=rerr
      endif

      IF(lw.GT.-1.0) then

        j=j+1
        sarr(j)=lw
        IF(ta.gt.0.0) then
          tdif=ta-((lw/sigma)**0.25)
        else
          tdif=-999.0
        endif
        tarr(1,j)=tdif
        IF(Ta.gt.0.0.and.vprs.gt.0.0.and.rhfac.GT.-1.0) then
          cof=lw/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
          cof=cof-rhfac
        else
          cof=-999.0
        endif
        IF(cof.lt.0.0.or.cof.gt.6.0) cof=-999.0
        tarr(2,j)=cof
cccccccccccccccccccccccccccccccccccccccccc
        IF(j.gt.step-1) then
          av2=0.0
          std=0.0
          do i=1,j
            av2=av2+sarr(i)
          end do
          av2=av2/(j*1.0)
          do i=1,j
            std=std+(av2-sarr(i))**2
          end do
          std=SQRT(std/(j*1.0))
        endif
        j=step-1
        DO i=1,j
          sarr(i)=sarr(i+1)
          tarr(1,i)=tarr(1,i+1)
          tarr(2,i)=tarr(2,i+1)
        END DO

        IF(vprs.gt.0.0.and.ta.gt.0.0.and.cosz.ge.0.0) then
cccccccccccccc  detect daylight clear LW cccccccccccccccccccccccccc
          IF(std.le.lslim.AND.(tarr(1,imid)).gt.tdlim.AND.
     %(tarr(2,imid)).gt.0.0) then
            av=av+tarr(2,imid)
            cn=cn+1
            xarr(cn)=tarr(2,imid)
            ndc=ndc+1
cccccccccccccc  detect daylight clear SW cccccccccccccccccccccccccc
          elseIF(clr.eq.1) then
            cof=lw/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
            cof=cof-rhfac
            av=av+cof
            cn=cn+1
            xarr(cn)=cof
            ndc=ndc+1
cccccccccccccc daylight measured less than calculated cccccccccccccccc
          elseIF(clw.GT.-1.0.and.lw.le.clw) then
            cof=lw/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
            cof=cof-rhfac
            av=av+cof
            cn=cn+1
            ltd=ltd+1
            xarr(cn)=cof
          endif
        endif
      endif

      GO TO 110


 150  CLOSE(UNIT=2)
      n=cn
      IF(ndc.ge.clim) then
        call sort(cn,xarr)
        xdc=2*ndc
        IF(xdc.gt.cn) xdc=cn
        IF((xdc*1.0)/cn.LT.(cn/2.0)) xdc=cn/2.0
        cn=xdc
        av=0.0
        do i=1,cn
          av=av+xarr(i)
        end do
      endif

      IF(cn.ge.clim) then
        av=av/cn*1.0
        std=0.0
        do i=1,cn
          std=std+(xarr(i)-av)**2
        end do
        std=SQRT(std/cn)
        j=0
        av2=0.0

        do i=1,cn
          IF(ltd.ge.clim) then
cccccc    changed from Xarr<avg  to Xarr<av+std on 20091119  ccccccccccccc
cccccc    added Xarr>av-1.5*std on 20091230  ccccccccccccc
            IF(xarr(i).LE.(av+std).and.xarr(i).Gt.(av-std*1.5)) then
              av2=av2+xarr(i)
              j=j+1
              sarr(j)=xarr(i)
            endif
          else
            IF(ABS(xarr(i)-av).lt.std*2.0.and.xarr(i).GE.(av-std*2.0))
     %then
              av2=av2+xarr(i)
              j=j+1
              sarr(j)=xarr(i)
            endif
          endif
        end do
        av2=av2/j*1.0
        std=0.0
        do i=1,j
          std=std+(av2-sarr(i))**2
        end do
        std=SQRT(std/j)

        av=0.0
        cn=0
cccccc    added Sarr>av-#*std on 20091230  ccccccccccccc
        do i=1,j
          IF(ltd.ge.clim) then
            IF(sarr(i).le.av2+std.and.sarr(i).Gt.av2-std*1.5) then
              av=av+sarr(i)
              cn=cn+1
              xarr(cn)=sarr(i)
            endif
          else
            IF(ABS(sarr(i)-av2).lt.std*1.5.and.sarr(i).Gt.av2-std*1.5)
     %then
              av=av+sarr(i)
              cn=cn+1
              xarr(cn)=sarr(i)
            endif
          endif
        end do
        av=av/cn*1.0
        std=0.0
        do i=1,cn
          std=std+(xarr(i)-av)**2
        end do
        std=SQRT(std/cn)

        IF(av.LT.-9999.0.or.av.gt.9999.0) av=-9.0
        IF(std.LT.-9999.0.or.std.gt.9999.0) std=-9.0

      else
        av=-9.0
        std=-9.0
      endif

      IF(av.GT.-8.0) then
        ucof=av
      ELSEIF(ocof(c).GT.-8.0) then
        ucof=ocof(c)
      else
        ucof=-9.0
      endif

      WRITE(3,56) cdat(c),n24,dayn,n,cn,ucof,std,ocof(c),ltd
      WRITE(8,56) cdat(c),n24,dayn,n,cn,ucof,std,ocof(c),ltd,ndc,xdc,j
      GO TO 105

 200  CLOSE(UNIT=1)
      CLOSE(UNIT=3)
      CLOSE(UNIT=8)

ccc      COF2='Clr_LWdn.c2f'
ccc   outfil='Clr_LWdn.asc'
      call clrcof1(outfil,cof2)


cccccc  end 2nd iteration of daylight coefficient fitting and interpolation
ccc    now re-read the data with the latest interpolated coeff,
ccc    look for night time clear sky


ccc      COF2='Clr_LWdn.c2f'
      OPEN(UNIT=1,FILE=cof2,STATUS='old')
      nc=1
 203  READ(1,*,END=204) cdat(nc),coeff(nc),ocof(nc),j
      nc=nc+1
      GO TO 203

 204  CLOSE(UNIT=1)
      nc=nc-1
      ok=.true.

      OPEN(UNIT=8,FILE='Clr_LWdn3.asc',STATUS='unknown')
ccc   outfil='Clr_LWdn.asc'  [saved for later reference as Clr_LWdn3.asc]
      OPEN(UNIT=3,FILE=outfil,STATUS='unknown')
      OPEN(UNIT=1,FILE=dirfil,STATUS='old')
ccccc 205  READ(1,6,END=300) infil
 205  READ(1,6,END=260) infil
      IF(p.gt.0) then
        OPEN(UNIT=2,FILE=path(1:p)//infil,STATUS='old')
        WRITE(6,*)'Opened path//infil: ',path(1:p)//infil
      else
        OPEN(UNIT=2,FILE=infil,STATUS='old')
        WRITE(6,*)'Opened infil: ',infil
      endif
      READ(2,*,END=250)
      READ(2,*,END=250)
      READ(2,*,END=250)
      READ(2,*,END=250)
      READ(infil(1:8),*) fdat
        do c=1,nc
          IF(cdat(c).eq.fdat) GO TO 207
        end do
        WRITE(6,*)'cdat.ne.fdat #2: ',cdat(c),fdat,c
        WRITE(6,*)'cdat(1),cdat(c-1): ',cdat(1),cdat(c-1)
        pause
        GO TO 300
 207  cn=0
      nn=0
      n=0
      av=0.0
      av2=0.0
      n2av=0.0
      std=0.0
      dayn=0
      n24=0
      n1=.true.
      n2=.false.
      j=0
      dstrt=-999
      dend=-999
      n1num=0
      n2num=0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF(ok) then
        WRITE(3,*)'   ldate   N1N   N2N    Ds    De     N1Cof      Dcof
     %    N2Cof  TotNgt   TotN1   TotN2'
        WRITE(8,*)'   ldate   N1N   N2N    Ds    De     N1Cof      Dcof
     %    N2Cof  TotNgt   TotN1   TotN2'
        ok=.false.
      endif

 210  READ(2,*,END=250,ERR=89) zdat,ztim,ldat,ltim,scv,cflg,clr,
     %(x(i),i=1,17),(x(i),i=18,17+nx)

      IF(n1) n1num=n1num+1
      IF(.not.n1.and..not.n2) n2num=n2num+1
      IF(x(1).gt.0.0.and.n1) then
        n1=.false.
        n2=.true.
        IF(n24.ge.1) then
          hr=ltim/100
          dstrt=hr*60+(ltim-(hr*100))
        else
          dstrt=-999
        endif
      elseIF(x(1).lt.0.0.and.n2) then
        n1=.false.
        n2=.false.
        IF(n24.ge.1) then
          hr=ltim/100
          dend=hr*60+(ltim-(hr*100))
        else
          dend=-999
        endif
      endif
       n24=n24+1
       IF(x(1).lt.0.0) dayn=dayn+1

       IF(nTa.gt.0) then
         Ta=x(17+nTa)
         IF(typT.eq.1.and.Ta.gt.errlm(nta)) then
           Ta=Ta+273.15
         ELSEIF(typT.eq.3.and.Ta.gt.errlm(nta)) then
           Ta=Ta+40.0
           Ta=(Ta*5.0)/9.0
           Ta=Ta-40.0
           Ta=Ta+273.15
         ELSEIF(Ta.le.errlm(nta)) then
           Ta=rerr
         endif
       else
         Ta=rerr
       endif
       IF(Ta.gt.400.0.or.Ta.lt.100.0) Ta=rerr

       IF(nrh.gt.0) then
         RH=x(17+nRH)
         IF(typR.eq.2.and.RH.gt.errlm(nrh)) then
           rh=rh*100.0
         ELSEIF(rh.le.errlm(nrh)) then
           rh=rerr
         endif
       else
         rh=rerr
       endif
       IF(rh.gt.110.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       ELSEIF(rh.lt.rhmin) then
         rh=rerr
       endif

       IF(nvp.gt.0) then
         vprs=x(17+nvp)
         IF(typv.eq.2.and.vprs.gt.errlm(nvp)) then
           vprs=vprs*10.0
         ELSEIF(vprs.le.errlm(nvp)) then
           vprs=rerr
         endif
       ELSEIF(Ta.gt.0.0.and.RH.gt.1.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         vprs=es*rh/100.0
       else
         vprs=rerr
       endif
       IF(vprs.gt.60.0) vprs=rerr

       IF(rh.lt.0.0.and.vprs.ge.0.0.and.ta.gt.0.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         rh=(vprs/es)*100.0
       endif
       IF(rh.gt.110.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       endif

cccccccccccc check for RH and vprs bad data  ccccccccccccccccccc
         IF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.gt.0.and.Ta.gt.0.0) then
           es=6.1173*EXP(19.83923-(5419.289/(Ta)))
           xvprs=es*rh/100.0
           IF(abs(xvprs-vprs).lt.0.1) then
             vprs=rerr
             rh=rerr
           endif
         ELSEIF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.eq.0) then
             vprs=rerr
             rh=rerr
         endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       IF(nlw.gt.0) then
         lw=x(17+nlw)
         IF(lw.le.errlm(nlw)) lw=rerr
       else
         lw=rerr
       endif
       IF(lw.gt.600.0) lw=rerr

CCC   NOTE: this uses the DAYLIGHT coefficient to calculate CLW for the
CCC         NIGHT LW<CLW testing!!!
      IF(Ta.gt.0.0.and.RH.ge.0.0.and.vprs.ge.0.0) then
        rhfac=rha*(rh)**rhb
             tstrhf=(rhfac*((vprs/ta)**(1./7.)))*sigma*ta**4
             IF(tstrhf.gt.rflim) then
               RHfac=rflim/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
             endif
        Ec=(coeff(c)+rhfac)*((vprs/ta)**(1./7.))
        if(Ec.gt.1.d0) Ec=1.d0
        clw=Ec*sigma*ta**4
      elseIF(Ta.gt.0.0.and.vprs.ge.0.0) then
        rhfac=0.0
        Ec=(coeff(c)+rhfac)*((vprs/ta)**(1./7.))
        if(Ec.gt.1.d0) Ec=1.d0
        clw=Ec*sigma*ta**4
      else
        clw=rerr
        Ec=rerr
        rhfac=rerr
      endif

      imid=11/datres
      step=21/datres

      IF(lw.GT.-1.0) then
        j=j+1
        sarr(j)=lw
        IF(ta.gt.0.0) then
          tdif=ta-((lw/sigma)**0.25)
        else
          tdif=-999.0
        endif
        tarr(1,j)=tdif
        IF(Ta.gt.0.0.and.vprs.gt.0.0.and.rhfac.GT.-1.0) then
          cof=lw/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
          cof=cof-rhfac
        else
          cof=-999.0
        endif
        IF(cof.lt.0.0.or.cof.gt.6.0) cof=-999.0
        tarr(2,j)=cof

cccccccccccccccccccccccccccccccccccccccccc
        IF(j.gt.step-1) then
          av2=0.0
          std=0.0
          do i=1,j
            av2=av2+sarr(i)
          end do
          av2=av2/(j*1.0)
          do i=1,j
            std=std+(av2-sarr(i))**2
          end do
          std=SQRT(std/(j*1.0))

          IF(tarr(1,imid).gt.0.0.and.x(1).lt.0.0)then
cccccccccccccc  night detect clear LW cccccccccccccccccccccccccc
            IF(std.le.lslim.AND.(tarr(1,imid)).gt.tdlim.AND.
     %(tarr(2,imid)).gt.0.0) then
              IF(n1) then
                av=av+tarr(2,imid)
                cn=cn+1
                xarr(cn)=tarr(2,imid)
              ELSE
                n2av=n2av+tarr(2,imid)
                nn=nn+1
                n2arr(nn)=tarr(2,imid)
              endif

cccccccccccccc night measured less than calculated cccccccccccccccc
CCC   NOTE: this uses the DAYLIGHT coefficient to calculate CLW for the
CCC         NIGHT LW<CLW testing!!!
cc            elseIF(clw.GT.-1.0.and.lw.le.clw) then
cc              IF(n1) then
cc                av=av+tarr(2,imid)
cc                cn=cn+1
cc                xarr(cn)=tarr(2,imid)
cc              ELSE
cc                n2av=n2av+tarr(2,imid)
cc                nn=nn+1
cc                n2arr(nn)=tarr(2,imid)
cc              endif
            endif
          endif

          j=step-1
          DO i=1,j
            sarr(i)=sarr(i+1)
            tarr(1,i)=tarr(1,i+1)
            tarr(2,i)=tarr(2,i+1)
          END DO

        endif
      endif

      GO TO 210


 250  CLOSE(UNIT=2)
      n=cn
      IF(cn.ge.clim) then
        av=av/cn*1.0
        std=0.0
        do i=1,cn
          std=std+(xarr(i)-av)**2
        end do
        std=SQRT(std/cn)
        j=0
        av2=0.0
        do i=1,cn
          IF(ABS(xarr(i)-av).lt.std*2.0) then
            av2=av2+xarr(i)
            j=j+1
            sarr(j)=xarr(i)
          endif
        end do
        av2=av2/j*1.0
        std=0.0
        do i=1,j
          std=std+(av2-sarr(i))**2
        end do
        std=SQRT(std/j)

        av=0.0
        cn=0
        do i=1,j
          IF(ABS(sarr(i)-av2).lt.std*1.5) then
            av=av+sarr(i)
            cn=cn+1
            xarr(cn)=sarr(i)
          endif
        end do
        av=av/cn*1.0
        std=0.0
        do i=1,cn
          std=std+(xarr(i)-av)**2
        end do
        std=SQRT(std/cn)

      else
        av=-9.0
        std=-9.0
      endif

        IF(av.LT.-9999.0.or.av.gt.9999.0) av=-9.0
        IF(std.LT.-9999.0.or.std.gt.9999.0) std=-9.0

ccccccc   This is the midnight to sunrise (n1) coefficient    ccccccccccccc
      IF(av.GT.-8.0) then
        ucof=av
      else
        ucof=-9.0
      endif

CCCCCCCC   Now do the sunset to midnight (n2) coefficient  CCCCCCCCCCCCCCCC
      std=0.0
      IF(nn.ge.clim) then
        n2av=n2av/nn*1.0

        do i=1,nn
          std=std+(n2arr(i)-n2av)**2
        end do
        std=SQRT(std/nn)
        j=0
        av2=0.0
        do i=1,nn
          IF(ABS(n2arr(i)-n2av).lt.std*2.0) then
            av2=av2+n2arr(i)
            j=j+1
            sarr(j)=n2arr(i)
          endif
        end do
        av2=av2/j*1.0
        std=0.0
        do i=1,j
          std=std+(av2-sarr(i))**2
        end do
        std=SQRT(std/j)

        av=0.0
        cn=0
        do i=1,j
          IF(ABS(sarr(i)-av2).lt.std*1.5) then
            av=av+sarr(i)
            cn=cn+1
            xarr(cn)=sarr(i)
          endif
        end do
        av=av/cn*1.0
        std=0.0
        do i=1,cn
          std=std+(xarr(i)-av)**2
        end do
        std=SQRT(std/cn)

        IF(av.LT.-9999.0.or.av.gt.9999.0) av=-9.0
        IF(std.LT.-9999.0.or.std.gt.9999.0) std=-9.0

      else
        av=-9.0
        std=-9.0
      endif

ccccccc   This is the sunset to midnight coefficient    ccccccccccccc
      IF(av.GT.-8.0) then
        ncof=av
      else
        ncof=-9.0
      endif

cc      IF(n1num.lt.30) dstrt=-999
      IF(n1num.lt.90/datres) dstrt=-999
      IF(n2num.lt.30/datres) dend=-999
      IF(dend.ge.1410) dend=-999
      IF(dstrt.lt.0.and.dend.lt.720) dend=-999
      IF(dstrt.gt.0.and.dend.gt.0.and.dend-dstrt.lt.90) then
        dstrt=-999
        dend=-999
      endif

cc      WRITE(3,56) cdat(c),n24,dayn,dstrt,dend,ucof,coeff(c),ncof
cc      WRITE(3,56) cdat(c),n,nn,dstrt,dend,ucof,coeff(c),ncof
      WRITE(3,56) cdat(c),n,nn,dstrt,dend,ucof,coeff(c),ncof,dayn,n1num
     %,n2num
      WRITE(8,56) cdat(c),n,nn,dstrt,dend,ucof,coeff(c),ncof,dayn,n1num
     %,n2num
      GO TO 205

cccccccccc   End first iteration night fitting using detected clr LWdn  ccccccc

 260  CLOSE(UNIT=1)
      CLOSE(UNIT=3)
      CLOSE(UNIT=8)



CCCCCC   Interpolate coefficients  CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
ccc   outfil='Clr_LWdn.asc'
ccc   COFFIL='Clr_LWdn.cof'
ccc      call clrcof2(clim,outfil,coffil,slim)
      call clrcof2(clim,outfil,'Clr_LWd1.cof',slim)


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCC second iteration of night detection now including CCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCC night LW<CLW data in fitting                      CCCCCCCCCCCCC



ccc   COFFIL='Clr_LWdn.cof'
ccc      OPEN(UNIT=1,FILE=coffil,STATUS='old')
      OPEN(UNIT=1,FILE='Clr_LWd1.cof',STATUS='old')
      nc=1
ccc    read in previous interpolated night coeffs, fcof(nc) is 1st part of local day,
CCC      coeff(nc) is day coeff,  and ocof(nc) is last part of local day
 263  READ(1,*,END=264) cdat(nc),fcof(nc)
      READ(1,*,END=264) cdat(nc),coeff(nc)
      READ(1,*,END=264) cdat(nc),ocof(nc)
      nc=nc+1
      GO TO 263

 264  CLOSE(UNIT=1)
      nc=nc-1
      ok=.true.

ccc      OPEN(UNIT=88,FILE='LW_lt_CLWn1.asc',STATUS='unknown')
ccc      OPEN(UNIT=89,FILE='LW_lt_CLWn2.asc',STATUS='unknown')
ccc      WRITE(88,*)'    Date     Ta-Te       Std   CalcCof    OldCof
ccc     %  LW       CLW        Ta      Vprs'
ccc      WRITE(89,*)'    Date     Ta-Te       Std   CalcCof    OldCof
ccc     %  LW       CLW        Ta      Vprs'
      OPEN(UNIT=8,FILE='Clr_LWdn4.asc',STATUS='unknown')
ccc   outfil='Clr_LWdn.asc'  [saved for later reference as Clr_LWdn4.asc]
      OPEN(UNIT=3,FILE=outfil,STATUS='unknown')
      OPEN(UNIT=1,FILE=dirfil,STATUS='old')
 265  READ(1,6,END=300) infil
      IF(p.gt.0) then
        OPEN(UNIT=2,FILE=path(1:p)//infil,STATUS='old')
        WRITE(6,*)'Opened path//infil: ',path(1:p)//infil
      else
        OPEN(UNIT=2,FILE=infil,STATUS='old')
        WRITE(6,*)'Opened infil: ',infil
      endif
      READ(2,*,END=280)
      READ(2,*,END=280) ddt,(x(i),i=1,14)

      lat= x(13)
      xlong= x(14)

      IF(XLONG .GE. 0.) THEN
	 IF(XLONG .GT. 180.) THEN
	    long=  360.-XLONG
	 ELSE
	    long= -XLONG
	 ENDIF
      ELSE IF(XLONG .LT. -180.) THEN
	    long= -(360.+XLONG)
      ELSE
	    long= -XLONG
      ENDIF

      READ(2,*,END=280)
      READ(2,*,END=280)
      READ(infil(1:8),*) fdat
        do c=1,nc
          IF(cdat(c).eq.fdat) GO TO 267
        end do
        WRITE(6,*)'cdat.ne.fdat #3: ',cdat(c),fdat,c
        WRITE(6,*)'cdat(1),cdat(c-1): ',cdat(1),cdat(c-1)
        pause
        GO TO 300
 267  cn=0
      nn=0
      n=0
      av=0.0
      av2=0.0
      n2av=0.0
      std=0.0
      dayn=0
      n24=0
      n1=.true.
      n2=.false.
      j=0
      dstrt=-999
      dend=-999
      n1num=0
      n2num=0

      ltn1=0
      ltn2=0

      IF(ok) then
        WRITE(3,*)'   ldate   N1N   N2N    Ds    De     N1Cof      Dcof
     %    N2Cof  TotNgt   TotN1   TotN2    ltn1    ltn2'
        WRITE(8,*)'   ldate   N1N   N2N    Ds    De     N1Cof      Dcof
     %    N2Cof  TotNgt   TotN1   TotN2    ltn1    ltn2'
        ok=.false.
      endif

 270  READ(2,*,END=280,ERR=89) zdat,ztim,ldat,ltim,scv,cflg,clr,
     %(x(i),i=1,17),(x(i),i=18,17+nx)

      IF(n1) n1num=n1num+1
      IF(.not.n1.and..not.n2) n2num=n2num+1
      IF(x(1).gt.0.0.and.n1) then
        n1=.false.
        n2=.true.
        IF(n24.ge.1) then
          hr=ltim/100
          dstrt=hr*60+(ltim-(hr*100))
        else
          dstrt=-999
        endif
      elseIF(x(1).le.0.0.and.n2) then
        n1=.false.
        n2=.false.
        IF(n24.ge.1) then
          hr=ltim/100
          dend=hr*60+(ltim-(hr*100))
        else
          dend=-999
        endif
      endif
       n24=n24+1
       IF(x(1).lt.0.0) dayn=dayn+1

       IF(nTa.gt.0) then
         Ta=x(17+nTa)
         IF(typT.eq.1.and.Ta.gt.errlm(nta)) then
           Ta=Ta+273.15
         ELSEIF(typT.eq.3.and.Ta.gt.errlm(nta)) then
           Ta=Ta+40.0
           Ta=(Ta*5.0)/9.0
           Ta=Ta-40.0
           Ta=Ta+273.15
         ELSEIF(Ta.le.errlm(nta)) then
           Ta=rerr
         endif
       else
         Ta=rerr
       endif
       IF(Ta.gt.400.0.or.Ta.lt.100.0) Ta=rerr

       IF(nrh.gt.0) then
         RH=x(17+nRH)
         IF(typR.eq.2.and.RH.gt.errlm(nrh)) then
           rh=rh*100.0
         ELSEIF(rh.le.errlm(nrh)) then
           rh=rerr
         endif
       else
         rh=rerr
       endif
       IF(rh.gt.110.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       ELSEIF(rh.lt.rhmin) then
         rh=rerr
       endif

       IF(nvp.gt.0) then
         vprs=x(17+nvp)
         IF(typv.eq.2.and.vprs.gt.errlm(nvp)) then
           vprs=vprs*10.0
         ELSEIF(vprs.le.errlm(nvp)) then
           vprs=rerr
         endif
       ELSEIF(Ta.gt.0.0.and.RH.gt.1.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         vprs=es*rh/100.0
       else
         vprs=rerr
       endif
       IF(vprs.gt.60.0) vprs=rerr

       IF(rh.lt.0.0.and.vprs.ge.0.0.and.ta.gt.0.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         rh=(vprs/es)*100.0
       endif
       IF(rh.gt.110.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       endif

cccccccccccc check for RH and vprs bad data  ccccccccccccccccccc
         IF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.gt.0.and.Ta.gt.0.0) then
           es=6.1173*EXP(19.83923-(5419.289/(Ta)))
           xvprs=es*rh/100.0
           IF(abs(xvprs-vprs).lt.0.1) then
             vprs=rerr
             rh=rerr
           endif
         ELSEIF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.eq.0) then
             vprs=rerr
             rh=rerr
         endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       IF(nlw.gt.0) then
         lw=x(17+nlw)
         IF(lw.le.errlm(nlw)) lw=rerr
       else
         lw=rerr
       endif
       IF(lw.gt.600.0) lw=rerr

CCC   set n1 or n2 coefficient to calculate CLW for the LW<CLW testing
      IF(Ta.gt.0.0.and.RH.ge.0.0.and.vprs.ge.0.0) then
        rhfac=rha*(rh)**rhb
             tstrhf=(rhfac*((vprs/ta)**(1./7.)))*sigma*ta**4
             IF(tstrhf.gt.rflim) then
               RHfac=rflim/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
             endif
        IF(n1) then
          Ec=(fcof(c)+rhfac)*((vprs/ta)**(1./7.))
          if(Ec.gt.1.d0) Ec=1.d0
          clw=Ec*sigma*ta**4
        else
          Ec=(ocof(c)+rhfac)*((vprs/ta)**(1./7.))
          if(Ec.gt.1.d0) Ec=1.d0
          clw=Ec*sigma*ta**4
        endif
      elseIF(Ta.gt.0.0.and.vprs.ge.0.0) then
        rhfac=0.0
        IF(n1) then
          Ec=(fcof(c)+rhfac)*((vprs/ta)**(1./7.))
          if(Ec.gt.1.d0) Ec=1.d0
          clw=Ec*sigma*ta**4
        else
          Ec=(ocof(c)+rhfac)*((vprs/ta)**(1./7.))
          if(Ec.gt.1.d0) Ec=1.d0
          clw=Ec*sigma*ta**4
        endif
      else
        clw=rerr
        Ec=rerr
        rhfac=rerr
      endif

      imid=11/datres
      step=21/datres

      IF(lw.GT.-1.0) then
        j=j+1
        sarr(j)=lw
        IF(ta.gt.0.0) then
          tdif=ta-((lw/sigma)**0.25)
        else
          tdif=-999.0
        endif
        tarr(1,j)=tdif
        IF(Ta.gt.0.0.and.vprs.gt.0.0.and.rhfac.GT.-1.0) then
          cof=lw/(sigma*Ta**4.0)
          cof=cof/((vprs/Ta)**(1.0/7.0))
          cof=cof-rhfac
        else
          cof=-999.0
        endif
        IF(cof.lt.0.0.or.cof.gt.6.0) cof=-999.0
        tarr(2,j)=cof

cccccccccccccccccccccccccccccccccccccccccc
        IF(j.gt.step-1) then
          av2=0.0
          std=0.0
          do i=1,j
            av2=av2+sarr(i)
          end do
          av2=av2/(j*1.0)
          do i=1,j
            std=std+(av2-sarr(i))**2
          end do
          std=SQRT(std/(j*1.0))

          IF(tarr(1,imid).gt.0.0.and.x(1).lt.0.0)then
cccccccccccccc  night detect clear LW cccccccccccccccccccccccccc
            IF(std.le.lslim.AND.(tarr(1,imid)).gt.tdlim.AND.
     %(tarr(2,imid)).gt.0.0) then
              IF(n1) then
                av=av+tarr(2,imid)
                cn=cn+1
                xarr(cn)=tarr(2,imid)
              ELSE
                n2av=n2av+tarr(2,imid)
                nn=nn+1
                n2arr(nn)=tarr(2,imid)
              endif

cccccccccccccc night measured less than calculated cccccccccccccccc
            elseIF(clw.GT.-1.0.and.lw.le.clw) then
              IF(n1) then
                av=av+tarr(2,j)
                cn=cn+1
                xarr(cn)=tarr(2,j)
                ltn1=ltn1+1
ccc                WRITE(88,275) ldat,Tarr(1,j),std,tarr(2,j),ocof(c)
ccc     %,lw,clw,Ta,vprs,j
ccc 275            FORMAT(i8,8f10.3,i8)
              ELSE
                n2av=n2av+tarr(2,j)
                nn=nn+1
                n2arr(nn)=tarr(2,j)
                ltn2=ltn2+1
ccc                WRITE(89,275) ldat,Tarr(1,j),std,tarr(2,j),ocof(c)
ccc     %,lw,clw,Ta,vprs,j
              endif
            endif
          endif

          j=step-1
          DO i=1,j
            sarr(i)=sarr(i+1)
            tarr(1,i)=tarr(1,i+1)
            tarr(2,i)=tarr(2,i+1)
          END DO

        endif
      endif

      GO TO 270


 280  CLOSE(UNIT=2)

ccc      CLOSE(UNIT=88)
ccc      CLOSE(UNIT=89)

      n=cn
      IF(cn.ge.clim) then
        av=av/cn*1.0
        std=0.0
        do i=1,cn
          std=std+(xarr(i)-av)**2
        end do
        std=SQRT(std/cn)
        j=0
        av2=0.0
        do i=1,cn
          IF(ltn1.ge.clim) then
cccccc    changed from Xarr<avg  to Xarr<av+std on 20091119  ccccccccccccc
            IF(xarr(i).le.av+std) then
              av2=av2+xarr(i)
              j=j+1
              sarr(j)=xarr(i)
            endif
          else
            IF(ABS(xarr(i)-av).lt.std*2.0) then
              av2=av2+xarr(i)
              j=j+1
              sarr(j)=xarr(i)
            endif
          endif
        end do
        av2=av2/j*1.0
        std=0.0
        do i=1,j
          std=std+(av2-sarr(i))**2
        end do
        std=SQRT(std/j)

        av=0.0
        cn=0
        do i=1,j
          IF(ltn1.ge.clim) then
            IF(sarr(i).le.av2+std) then
              av=av+sarr(i)
              cn=cn+1
              xarr(cn)=sarr(i)
            endif
          else
            IF(ABS(sarr(i)-av2).lt.std*1.5) then
              av=av+sarr(i)
              cn=cn+1
              xarr(cn)=sarr(i)
            endif
          endif
        end do
        av=av/cn*1.0
        std=0.0
        do i=1,cn
          std=std+(xarr(i)-av)**2
        end do
        std=SQRT(std/cn)

      else
        av=-9.0
        std=-9.0
      endif

        IF(av.LT.-9999.0.or.av.gt.9999.0) av=-9.0
        IF(std.LT.-9999.0.or.std.gt.9999.0) std=-9.0

ccccccc   This is the midnight to sunrise (n1) coefficient    ccccccccccccc
      IF(av.GT.-8.0) then
        ucof=av
      else
        ucof=-9.0
      endif

      std=0.0
      IF(nn.ge.clim) then
        n2av=n2av/nn*1.0

        do i=1,nn
          std=std+(n2arr(i)-n2av)**2
        end do
        std=SQRT(std/nn)
        j=0
        av2=0.0
        do i=1,nn
          IF(ltn2.ge.clim) then
cccccc    changed from n2arr<avg  to n2arr<av+std on 20091119  ccccccccccccc
            IF(n2arr(i).le.n2av+std) then
              av2=av2+n2arr(i)
              j=j+1
              sarr(j)=n2arr(i)
            endif
          else
            IF(ABS(n2arr(i)-n2av).lt.std*2.0) then
              av2=av2+n2arr(i)
              j=j+1
              sarr(j)=n2arr(i)
            endif
          endif
        end do
        av2=av2/j*1.0
        std=0.0
        do i=1,j
          std=std+(av2-sarr(i))**2
        end do
        std=SQRT(std/j)

        av=0.0
        cn=0
        do i=1,j
          IF(ltn2.ge.clim) then
            IF(sarr(i).le.av2+std) then
              av=av+sarr(i)
              cn=cn+1
              xarr(cn)=sarr(i)
            endif
          else
            IF(ABS(sarr(i)-av2).lt.std*1.5) then
              av=av+sarr(i)
              cn=cn+1
              xarr(cn)=sarr(i)
            endif
          endif
        end do
        av=av/cn*1.0
        std=0.0
        do i=1,cn
          std=std+(xarr(i)-av)**2
        end do
        std=SQRT(std/cn)

        IF(av.LT.-9999.0.or.av.gt.9999.0) av=-9.0
        IF(std.LT.-9999.0.or.std.gt.9999.0) std=-9.0

      else
        av=-9.0
        std=-9.0
      endif

ccccccc   This is the sunset to midnight (n2) coefficient    ccccccccccccc
      IF(av.GT.-8.0) then
        ncof=av
      else
        ncof=-9.0
      endif

CCCCCCCCC  Test dstrt and dend
          ldat=cdat(c)
          yr=ldat/10000
          mn=ldat/100-yr*100
          dy=ldat-yr*10000-mn*100
          rtim=1200.00
          CALL EPHEMS(LAT,LONG,dy,mn,yr,rtim,0,1,0,0,
     +            AZ,AU,Z,HR,DEC)
          sncz=dCOS(Z*PI/180.d0)

      IF(sncz.lt.0.01) then
        dstrt=-999
        dend=-999
      endif

      IF(n1num.lt.90/datres) dstrt=-999
      IF(n2num.lt.30/datres) dend=-999
      IF(dend.ge.1410) dend=-999
      IF(dstrt.lt.0.and.dend.lt.720) dend=-999
      IF(dstrt.gt.0.and.dend.gt.0.and.dend-dstrt.lt.90) then
        dstrt=-999
        dend=-999
      endif

cc      WRITE(3,56) cdat(c),n24,dayn,dstrt,dend,ucof,coeff(c),ncof
cc      WRITE(3,56) cdat(c),n,nn,dstrt,dend,ucof,coeff(c),ncof
      WRITE(3,56) cdat(c),n,nn,dstrt,dend,ucof,coeff(c),ncof,dayn,n1num
     %,n2num,ltn1,ltn2
      WRITE(8,57) cdat(c),n,nn,dstrt,dend,ucof,coeff(c),ncof,dayn,n1num
     %,n2num,ltn1,ltn2,sncz
 57   FORMAT(i8,4i6,3f10.4,5I8,f10.4)
      GO TO 265

cccccccccc   End 2nd iteration night fitting using detected clr LWdn & LW<CLW  ccccccc

 300  CLOSE(UNIT=1)
      CLOSE(UNIT=3)
      CLOSE(UNIT=8)

ccc   outfil='Clr_LWdn.asc'
ccc   COFFIL='Clr_LWdn.cof'
      call clrcof2(clim,outfil,coffil,slim)


cccccc  end 4th iteration of coefficient fitting and interpolation
ccc    now re-read the data with the latest interpolated coeff,
ccc    calculate the clear-sky LW,
ccc      if flag set, output all clear-sky comparison data to file.

ccc   COFFIL='Clr_LWdn.cof'
      OPEN(UNIT=1,FILE=coffil,STATUS='old')
      nc=1
 303  READ(1,*,END=304) cdat(nc),coeff(nc),ocof(nc),ds(nc),de(nc)
      if(nlw.eq.0) coeff(nc)=-9.0
      IF(cdat(nc).eq.0) GO TO 304
      nc=nc+1
      GO TO 303

 304  CLOSE(UNIT=1)
      nc=nc-1
      ok=.true.

      IF(clrout) THEN
        OPEN(UNIT=9,FILE=clrfil,STATUS='unknown')
        WRITE(9,*) head1(1:h1)//head2(1:h2)
      endif

      OPEN(UNIT=1,FILE=dirfil,STATUS='old')
 305  READ(1,6,END=400) infil
      IF(p.gt.0) then
        OPEN(UNIT=2,FILE=path(1:p)//infil,STATUS='old')
        WRITE(6,*)'Opened path//infil: ',path(1:p)//infil
      else
        OPEN(UNIT=2,FILE=infil,STATUS='old')
        WRITE(6,*)'Opened infil: ',infil
      endif
      READ(2,*,END=350)
      READ(2,*,END=350) ddt,(x(i),i=1,14)

      dra= x(1)
      drb= x(2)
cccccccccccccccccccccccc which total SW to use   cccccccccccccccccccccc
      if(tflg.eq.0) then
        swa= x(5)
        swb= x(6)
      else
        swa= x(3)
        swb= x(4)
      endif
      sxa= x(10)
      sxb= x(11)
      lat= x(13)
      xlong= x(14)

      IF(XLONG .GE. 0.) THEN
	 IF(XLONG .GT. 180.) THEN
	    long=  360.-XLONG
	 ELSE
	    long= -XLONG
	 ENDIF
      ELSE IF(XLONG .LT. -180.) THEN
	    long= -(360.+XLONG)
      ELSE
	    long= -XLONG
      ENDIF

      READ(2,*,END=350)
      READ(2,*,END=350)
      READ(infil(1:8),*) fdat
        do c=1,nc
          IF(cdat(c).eq.fdat) GO TO 307
        end do
        WRITE(6,*)'cdat.ne.fdat #4: ',cdat(c),fdat,c
        WRITE(6,*)'cdat(1),cdat(c-1): ',cdat(1),cdat(c-1)
        pause
        GO TO 400
 307  cn=0
      av=0.0
      av2=0.0
      std=0.0
      n24=0

      do s=1,nmax
        xarr(s)=-999.0
        sarr(s)=-999.0
      end do

      s=0
      e=0

 310  READ(2,*,END=340,ERR=89) zdat,ztim,ldat,ltim,scv,cflg,clr,
     %(x(i),i=1,17),(x(i),i=18,17+nx)

       IF(nTa.gt.0) then
         Ta=x(17+nTa)
         IF(typT.eq.1.and.Ta.gt.errlm(nta)) then
           Ta=Ta+273.15
         ELSEIF(typT.eq.3.and.Ta.gt.errlm(nta)) then
           Ta=Ta+40.0
           Ta=(Ta*5.0)/9.0
           Ta=Ta-40.0
           Ta=Ta+273.15
         ELSEIF(Ta.le.errlm(nta)) then
           Ta=rerr
         endif
       else
         Ta=rerr
       endif
       IF(Ta.gt.400.0.or.Ta.lt.100.0) Ta=rerr

       IF(nrh.gt.0) then
         RH=x(17+nRH)
         IF(typR.eq.2.and.RH.gt.errlm(nrh)) then
           rh=rh*100.0
         ELSEIF(rh.le.errlm(nrh)) then
           rh=rerr
         endif
       else
         rh=rerr
       endif
       IF(rh.gt.110.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       ELSEIF(rh.lt.rhmin) then
         rh=rerr
       endif

       IF(nvp.gt.0) then
         vprs=x(17+nvp)
         IF(typv.eq.2.and.vprs.gt.errlm(nvp)) then
           vprs=vprs*10.0
         ELSEIF(vprs.le.errlm(nvp)) then
           vprs=rerr
         endif
       ELSEIF(Ta.gt.0.0.and.RH.ge.2.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         vprs=es*rh/100.0
       else
         vprs=rerr
       endif
       IF(vprs.gt.60.0) vprs=rerr

       IF(rh.lt.0.0.and.vprs.ge.0.0.and.ta.gt.0.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         rh=(vprs/es)*100.0
       endif
       IF(rh.gt.110.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       endif

cccccccccccc check for RH and vprs bad data  ccccccccccccccccccc
         IF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.gt.0.and.Ta.gt.0.0) then
           es=6.1173*EXP(19.83923-(5419.289/(Ta)))
           xvprs=es*rh/100.0
           IF(abs(xvprs-vprs).lt.0.1) then
             vprs=rerr
             rh=rerr
           endif
         ELSEIF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.eq.0) then
             vprs=rerr
             rh=rerr
         endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       IF(nWspd.gt.0) then
         Wspd=x(17+nWspd)
         IF(wspd.le.errlm(nwspd)) wspd=rerr
       else
         wspd=rerr
       endif

       IF(nlw.gt.0) then
         lw=x(17+nlw)
         IF(lw.le.errlm(nlw)) lw=rerr
       else
         lw=rerr
       endif
       IF(lw.gt.600.0) lw=rerr

       IF(nlwu.gt.0) then
         lwu=x(17+nlwu)
         IF(lwu.le.errlm(nlwu)) lwu=rerr
       else
         lwu=rerr
       endif
       IF(lwu.gt.700.0) lwu=rerr


ccc    interpolate lapse rate coefficients through the day and night
ccc    based on times of sunrise and sunset
ccc    added ".and.nlw.gt.0" for data sets without LWdn measurements 20160805
      IF(Ta.gt.0.0.and.vprs.ge.0.0.and.nlw.gt.0) then
        IF(RH.ge.0.0)then
          rhfac=rha*(rh)**rhb
             tstrhf=(rhfac*((vprs/ta)**(1./7.)))*sigma*ta**4
             IF(tstrhf.gt.rflim) then
      WRITE(223,699)  ldat,ltim,rflim,rh,rhfac,
     %rflim/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
 699   FORMAT(2i8,2f10.2,10f12.6)
               RHfac=rflim/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
             endif
        else
          rhfac=0.0
        endif
        lhr=ltim/100
        dmin=lhr*60+(ltim-(lhr*100))
cccccccccccccccccccc   start of run   cccccccccccccccccccccccccccc
        IF(c.eq.1.and.dmin.le.ds(c)) then
          ucof=coeff(c)
cccccccccccccccccccc   end of run  cccccccccccccccccccccccccccc
        elseIF(c.EQ.nc-2.and.dmin.gt.de(c)) then
          ucof=coeff(c+2)
cccccccccccccccccccc  normal 2nd half of night   cccccccccccccccccccccccccccc
        elseIF(dmin.le.ds(c)) then
          av2=(coeff(c-1)+Coeff(c))/2.0
          delcof=(coeff(c)-av2)/ds(c)
          ucof=av2+delcof*(dmin)
cccccccccccccccccccc  normal transition to day value   cccccccccccccccccccccccccccc
        elseIF(dmin.le.ds(c)+90) then
          delcof=(coeff(c+1)-coeff(c))/90.0
          ucof=coeff(c)+delcof*(dmin-ds(c))
cccccccccccccccccccc   new, hold constant all day   cccccccccccccccccccccccccccc
        ELSEIF(dmin.le.de(c)-60) then
          ucof=coeff(c+1)
cccccccccccccccccccc  normal transition at sunset   cccccccccccccccccccccccccccc
        elseIF(dmin.le.de(c)+30) then
          delcof=(coeff(c+2)-coeff(c+1))/(90.0)
          ucof=coeff(c+1)+delcof*(dmin-(de(c)-60))
cccccccccccccccccccc  normal 1st half of night   cccccccccccccccccccccccccccc
        elseIF(dmin.gt.de(c)+30.and.de(c).GT.-1) then
          av2=(coeff(c+2)+Coeff(c+3))/2.0
          delcof=(av2-coeff(c+2))/(1439.0-(de(c)+30))
          ucof=coeff(c+2)+delcof*(dmin-(de(c)+30))
        ELSE
          yr=ldat/10000
          mn=ldat/100-yr*100
          dy=ldat-yr*10000-mn*100
          rtim=1200.00
          CALL EPHEMS(LAT,LONG,dy,mn,yr,rtim,0,1,0,0,
     +            AZ,AU,Z,HR,DEC)
          cosz=dCOS(Z*PI/180.d0)
cccccccccccccccccccc  all night data   cccccccccccccccccccccccccccc
          IF(cosz.le.0.0) then
            delcof=(coeff(c+2)-coeff(c))/(1440.0)
            ucof=coeff(c)+delcof*(dmin)
          else
cccccccccccccccccccc  all day data, 1st half   ccccccccccccccccc
            IF(dmin.lt.720) then
              delcof=(coeff(c+1)-coeff(c-2))/(1439.0)
              ucof=coeff(c-2)+delcof*(dmin+720)
cccccccccccccccccccc  all day data, 2nd half   ccccccccccccccccc
            else
              delcof=(coeff(c+4)-coeff(c+1))/(1439.0)
              ucof=coeff(c+1)+delcof*(dmin-720)
            endif
          endif
        endif
        Ec=(ucof+rhfac)*((vprs/ta)**(1./7.))
        if(Ec.gt.1.d0) Ec=1.d0
        clw=Ec*sigma*ta**4
      else
        clw=rerr
        Ec=rerr
        rhfac=rerr
      endif

      IF(scv.LT.-8.0) scv=rerr
      IF(x(1).LT.-8.0) x(1)=rerr
      IF(x(2).LT.-8.0) x(2)=rerr
      IF(x(3).LT.-98.0) x(3)=rerr
      IF(x(4).LT.-98.0) x(4)=rerr
      IF(x(5).LT.-9998.0) x(5)=rerr
      IF(x(6).LT.-98.0) x(6)=rerr
      IF(x(7).LT.-98.0) x(7)=rerr
      IF(x(8).LT.-998.0) x(8)=rerr
      IF(x(9).LT.-98.0) x(9)=rerr
      IF(x(10).LT.-98.0) x(10)=rerr
      IF(x(11).LT.-8.0) x(11)=rerr
      IF(x(12).LT.-8.0) x(12)=rerr
      IF(x(13).LT.-98.0) x(13)=rerr
      IF(x(14).LT.-98.0) x(14)=rerr
      IF(x(15).LT.-9998.0) x(15)=rerr
      IF(x(16).LT.-98.0) x(16)=rerr
      IF(x(17).LT.-98.0) x(17)=rerr

      do i=1,nx
        IF(x(17+i).lt.errlm(i)) x(17+i)=rerr
      end do

      cosz=x(1)
      au=x(2)
        dif=x(6)
        cdif=x(7)
        dir=x(9)
        cdir=x(10)
      IF(cosz.ge.0.0) then
cccccccccccccccccccccccc which total SW to use   cccccccccccccccccccccc
        if(tflg.eq.0) then
          csw=x(14)
          ssw=x(13)
        else
          csw=x(4)
          ssw=x(3)
        endif
        cswu=x(17)
        swu=x(16)
        IF(csw.lt.0.0) then
          IF(ddt.lt.10) then
            csw=(swa/au**2)*cosz**swb
          else
            csw=swa*cosz**swb
          endif
        endif
cccccccccccccccccccccccc which total SW to use   cccccccccccccccccccccc
        if(tflg.eq.0) then
          IF(ssw.lt.0.0.and.x(3).ge.0.0) ssw=x(3)
        else
          IF(ssw.lt.0.0.and.dif.ge.0.0.and.dir.ge.0.0) ssw=dif+dir
        endif
        IF(ssw.lt.0.0.and.ssw.GT.-900.0) ssw=0.0
        IF(dif.lt.0.0.and.ssw.ge.0.0.and.dir.ge.0.0) dif=ssw-dir
        IF(dif.lt.0.0.and.dif.GT.-900.0) dif=0.0
        IF(cdif.lt.0.0) cdif=(dra*cosz**drb)*csw
cccccccc   added 20161121  ccccccccccccccccccccccccccc
        IF(cdif.lt.0.0) cdif=0.0
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(dir.lt.0.0.and.ssw.ge.0.0.and.dif.ge.0.0) dir=ssw-dif
        IF(dir.lt.0.0.and.dir.GT.-900.0) dir=0.0
        IF(cdir.lt.0.0.and.csw.ge.0.0.and.cdif.ge.0.0) cdir=csw-cdif
        IF(cdir.lt.0.0) cdir=0.0
      elseIF(cosz.gE.-0.052336) then
cc      NOTE: CosZ = -0.052336 is approximately a SZA of 93 degrees
cccccccccccccccccccccccc which total SW to use   cccccccccccccccccccccc
        if(tflg.eq.0) then
          csw=x(14)
          if(csw.lt.0.0) csw=0.0
          ssw=x(13)
          IF(ssw.lt.0.0.and.x(3).ge.0.0) ssw=x(3)
          if(ssw.lt.0.0.and.ssw.GT.-900.0) ssw=0.0
        else
          csw=x(4)
          if(csw.lt.0.0) csw=0.0
          ssw=x(3)
          IF(ssw.lt.0.0.and.dif.ge.0.0.and.dir.ge.0.0) ssw=dif+dir
          if(ssw.lt.0.0.and.ssw.GT.-900.0) ssw=0.0
        endif
        IF(cdif.lt.0.0) cdif=0.0
        cdir=0.0
        dir=0.0
        IF(x(16).gt.0.0) then
          swu=x(16)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        ELSEIF(x(16).LT.-99.0) then
          swu=rerr
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        else
          swu=0.0
        endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(x(17).gt.0.0) then
          cswu=x(17)
        elseIF(x(17).LT.-99.0) then
          cswu=rerr
        else
          cswu=0.0
        endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        IF(dif.lt.0.0.and.ssw.GE.-0.1.and.x(9).GE.-0.1) dif=ssw-x(9)
        IF(dif.lt.0.0.and.dif.GT.-900.0) dif=0.0
      else
        x(6)=0.0
        x(7)=0.0
        x(9)=0.0
        x(10)=0.0
        IF(sflg.ne.0) then
          cswu=0.0
          swu=0.0
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        else
          cswu=rerr
          swu=rerr
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        endif
        dif=0.0
        cdif=0.0
        dir=0.0
        cdir=0.0
        ssw=0.0
        csw=0.0
      endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccc   put all data in the rdata(r,c) array so we can do averaging
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      n24=n24+1
      IF(n24.gt.1499) then
        WRITE(6,*)' Number of data this day > 1499! Ending this day on:'
        WRITE(6,*)' Zdate, Ztime, Ldate, Ltime: ',zdat,ztim,ldat,ltim
        pause
        GO TO 340
      endif
      idata(n24,1)= zdat
      idata(n24,2)= ztim
      idata(n24,3)= ldat
      idata(n24,4)= ltim
      idata(n24,5)= clr
      idata(n24,6)= 0
      idata(n24,7)= 0
      rdata(n24,1)= cosz
      rdata(n24,2)= au
      rdata(n24,3)= ssw
      rdata(n24,4)= csw
      rdata(n24,5)= lw
      rdata(n24,6)= clw
      rdata(n24,7)= swu
      rdata(n24,8)= cswu
      rdata(n24,9)= lwu
      rdata(n24,10)= dif
      rdata(n24,11)= cdif
      rdata(n24,12)= dir
      rdata(n24,13)= cdir
      rdata(n24,14)= -9999.0
      rdata(n24,15)= scv
      rdata(n24,16)= rerr
      rdata(n24,17)= rerr
      rdata(n24,18)= rerr
      rdata(n24,19)= ta
      rdata(n24,20)= vprs
      rdata(n24,21)= rh
      rdata(n24,22)= rhfac
      rdata(n24,23)= ec
      rdata(n24,24)= Wspd
      rdata(n24,61)= -9999.0
      rdata(n24,62)= -9999.0
      rdata(n24,63)= -9999.0
      clwu(n24)=-9999.0

      nx2=0
      do i=1,nx
        IF(i.ne.nta.and.i.ne.nrh.and.i.ne.nvp.and.i.ne.nlw.and.
     %i.ne.nlwu.and.i.ne.nwspd) then
          nx2=nx2+1
          parr(nx2)=x(17+i)
        endif
      end do

      do i=1,nx2
        rdata(n24,24+i)=parr(i)
      end do

      GO TO 310

 340  CLOSE(UNIT=2)

cc    open 1-minute outfile
        outfil=infil(1:9)//'lw1'
        OPEN(UNIT=3,FILE=outfil,STATUS='unknown')
        WRITE(3,*) head1(1:h1)//head2(1:h2)


cc    now do the day's processing
      eavg=0.0
      ne=0
      do a=1,n24

      rdata(a,14)=-9999.0
      lscv=-9999.0

ccccccccccccccccccc daylight avg ec cccccccccccccccccccccccc
          clr=idata(a,5)
          Ec=rdata(a,23)
          ta=rdata(a,19)
          lw=rdata(a,5)
          cosz=rdata(a,1)
          IF(clr.eq.1.and.nlw.gt.0) then
            Ed=lw/(sigma*Ta**4)
            clwu(a)=rdata(a,9)
          else
            Ed=-9999.0
          endif
          IF(cosz.gt.0.1) then
            IF(Ec.gt.0.0) then
              eavg=eavg+Ec
              ne=ne+1
            ELSEIF(Ed.gt.0.0) then
              eavg=eavg+Ed
              ne=ne+1
            endif
          endif

ccccccccccccccccccccc   LW clear sky detection for LWup cccccccccccccccccccccc
          clw=rdata(a,6)
          ta=rdata(a,19)

          av=0.0
          std=0.0
          q=0

          step=10/datres+1

          IF((a-step).lt.1) then
            s=1
          else
            s=a-step
          endif
          IF((a+step).gt.n24) then
            e=n24
          else
            e=a+step
          endif
          do i=s,e
            IF(rdata(i,5).GT.-1.0) then
              q=q+1
              av=av+rdata(i,5)
              xarr(q)= rdata(i,5)
            endif
          end do
          IF(q.gt.0) then
            av=av/q
            do i=1,q
              std=std+(av-xarr(i))**2
            end do
            std=SQRT(std/q)
          else
            av=-999.0
            std=-999.0
          endif

          IF(ta.gt.0.0.and.lw.gt.0.0.and.Ec.gt.0.0.and.std.GT.-1.0) then
            em=lw/(sigma*ta**4)
            Te=(lw/sigma)**0.25
            IF(em/Ec.le.elim.and.std.le.lslim.AND.(Ta-Te).gt.tdlim) then
              IF(idata(a,5).eq.0) idata(a,5)=2
              clwu(a)=rdata(a,9)
cccccccccccc    added 20091120 set flag to 3 and accept as LW clear if just std and Ta-Te diff OK  ccccccc
          ELSEIF(std.le.lslim.AND.(Ta-Te).gt.tdlim) then
              IF(idata(a,5).eq.0) idata(a,5)=3
              clwu(a)=rdata(a,9)
              endif
          else
            em=-9999.0
            Te=-9999.0
          endif
ccccccccccccccccccccc end LW up clear sky detection  cccccccccccccccccccccc

cccccccccccccc   set clw=lw if lw<clw  ccccccccccccccccccccccccccccccccccccc
        IF(clw.gt.0.0.and.lw.gt.0.0.and.lwok1) then
          IF(clw.gt.lw) then
            IF(idata(a,5).eq.0) idata(a,5)=9
            clw=lw
            clwu(a)=rdata(a,9)
          endif
          rdata(a,6)=clw
        endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccc   set clw=lw if ClrFlg= 1 or 2  cccccccccccccccccccccccccccccc
        IF(lw.gt.0.0.and.lwok2) then
          IF(idata(a,5).eq.1.or.idata(a,5).eq.2) then
            clw=lw
            clwu(a)=rdata(a,9)
          endif
          rdata(a,6)=clw
        endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccc   LW cloud fraction section  cccccccccccccccccccccc

cc   Durr and Philipona method

        lscv=-9999.0

        IF(Ec.gt.0.0.and.em.gt.0.0.and.std.GT.-1.0) then
          f0=1.01
          f1=0.12
          f2=0.21
          f3=0.38
          za=f0+f1*((1.0/Ec)-1.0)
          zb=f0+f2*((1.0/Ec)-1.0)
          zc=f0+f3*((1.0/Ec)-1.0)

          IF(idata(a,5).gt.0) then
            lscv=0.0
cccccccccccccccc  new 20051012: if LWCE=0, LWscv=0  cccccccccccccccccccc
          ELSEIF(lw-clw.GT.-0.001.and.lw-clw.lt.2.0) then
            lscv=0.0
cccccccccccccccc  new 20051012: if LWCE=0, LWscv=0  cccccccccccccccccccc
          ELSEIF(em/Ec.le.f0) then
            IF(std.le.1.0) then
              lscv=0.0
            elseIF(std.le.2.0) then
              lscv=0.125
            else
              lscv=0.25
            endif
          ELSEIF(em/Ec.gt.f0.and.em/Ec.le.za) then
            IF(std.le.1.0) then
              lscv=0.125
            elseIF(std.le.2.0) then
              lscv=0.25
            else
              lscv=0.375
            endif
          ELSEIF(em/Ec.gt.za.and.em/Ec.le.zb) then
            IF(std.le.1.0) then
              lscv=0.25
            else
              lscv=0.5
            endif
          ELSEIF(em/Ec.gt.zb.and.em/Ec.le.zc) then
            IF(std.le.4.0) then
              lscv=0.625
            else
              lscv=0.75
            endif
          ELSE
            IF(std.le.2.0) then
              lscv=1.0
            elseIF(std.le.8.0) then
              lscv=0.875
            else
              lscv=0.75
            endif
          endif
        else
          lscv=-9999.0
        endif

        rdata(a,14)=lscv

ccccccccccccccccccccc   end LW cloud fraction section  cccccccccccccccccccccc

      END do

ccccccccccccc calc daylight avg E if enough data ccccccccccccccccccccccc
      IF(ne.ge.clim) then
        Eavg=Eavg/ne
        uEavg=eavg
      endif

cc    now do 2nd part of day's processing
cc   set step from "(5/datres)+1" to "(3/datres)+1", and limit to 3, on 20091119
      step=(3/datres) +1
      IF(step.gt.3) step=3

      do a=1,n24
ccccccccccccccccccccc  smoothing of LW cloud fraction section  cccccccccccccccccccccc
          IF((a-step).lt.1) then
            s=1
          else
            s=a-step
          endif
          IF((a+step).gt.n24) then
            e=n24
          else
            e=a+step
          endif
          q=0
          lscv=0.0
          do i=s,e
            IF(rdata(i,14).GT.-1.0) then
              q=q+1
              lscv=lscv+rdata(i,14)
            endif
          end do
          IF(q.gt.0) then
            lscv=lscv/q
          else
            lscv=-9999.0
          endif
          slscv(a)=lscv

cccccccccccccccccccc  split do loop and added slscv array for smoothing LW Scv 20121030
      END do

      do a=1,n24
        rdata(a,14) = slscv(a)
      END do
cccccccccccccccccccc  split do loop and added slscv array for smoothing LW Scv 20121030

      do a=1,n24
ccccccccccccccccccccc   cloud base temp section  cccccccccccccccccccccc
cc      use LW scv when available

        lw=rdata(a,5)
        clw=rdata(a,6)
        ta=rdata(a,19)
        Ec=rdata(a,23)
        tco=rerr
        Te=(lw/sigma)**0.25

        IF(lscv.ge.scvlim.and.clw.gt.0.0.and.lw.gt.0.0) then
          IF((lw-clw).GT.0.0.and.Ec.gt.0.0) then
            tco=(lw-clw)/((1.0-Ec)*lscv*sigma)
            tco=tco**(0.25d0)
            IF(tco.lt.150.0) tco=150.0
          ELSEIF(ABS(lw-clw).lt.4.0) then
            Tco = (lw/sigma)**(0.25d0)
            IF(tco.lt.150.0) tco=150.0
          else
            Tco = rerr
          endif
        else
          Tco = rerr
        endif
        IF(tco.LT.rerr) tco=rerr
        IF(tco.gt.500.0) tco=rerr
        IF(tco.gt.Ta) tco=Ta
        IF(tco.gt.Te) tco=Te
        rdata(a,17)= tco

ccccccccccccccccccccc  end cloud base temp section  ccccccccccccccccccc

ccccccccccccccccccccc   cloud base height section  cccccccccccccccccccccc
cc      use simple lapse rate for now

        IF(Ta.gt.0.0.and.tco.gt.0.0) then
          cbh=(Ta-tco)/10.0
          IF(cbh.lt.0.0) cbh=0.0
cc          IF(cbh.le.0.02) then
cc            cbh=0.0
cc          IF(cbh.le.2.0) then
cc            cbh=1.0
cc          ELSEIF(cbh.le.5.0 )then
cc            cbh=2.0
cc          else
cc            cbh=3.0
cc          endif
        else
          cbh=rerr
        endif

        IF(cbh.gt.25.0) cbh=rerr
        rdata(a,18)=cbh

ccccccccccccccccccccc  end cloud base height section  ccccccccccccccccccc

ccccccccccccccccccccc   cloud tau section  cccccccccccccccccccccccccccc

        av=0.0
        av2=0.0
        r=0.0

        swu=rdata(a,7)
        ssw=rdata(a,3)

        IF(sflg.eq.2.and.swu.gt.0.0.and.ssw.gt.0.0) then
          alb=swu/ssw
        elseIF(sflg.eq.1.and.swu.gt.0.0.and.ssw.gt.0.0) then
          alb=swu
          swu=alb*ssw
          rdata(a,7)=swu
        else
          alb=rerr
        endif

        cswu=rdata(a,8)
        csw=rdata(a,4)
        cdir=rdata(a,13)

        IF(sflg.eq.1.and.cswu.gt.0.0.and.csw.gt.0.0) then
          cswu=cswu*csw
          rdata(a,8)=cswu
        ELSEIF(cosz.ge.0.0.and.sflg.eq.1.and.sxa.GT.-1.0) then
          cswu=(sxa*cosz**sxb)*(swa*cosz**swb)
          rdata(a,8)=cswu
        ELSEIF(cosz.lt.0.0.and.sflg.eq.1) then
          cswu=0.0
          rdata(a,8)=0.0
        ELSEIF(sflg.eq.0.and.cswu.LT.-90.0) then
          cswu=rerr
          rdata(a,8)=rerr
        endif

        IF(alb.gt.0.95) alb=0.95

        dif=rdata(a,10)
        scv=rdata(a,15)
        lscv=rdata(a,14)
        cosz=rdata(a,1)
        cdif=rdata(a,11)
        clw=rdata(a,6)
        ta=rdata(a,19)
        Ec=rdata(a,23)
        lw=rdata(a,5)
        telim=-9999.0
        idata(a,7)=0

        IF(lw.gt.0.0) then
          Te=(lw/sigma)**0.25
        else
          Te=-9999.0
        endif
        rdata(a,63)=Te

        IF(Ec.gt.0.0) then
          uE=Ec
        ELSEIF(ueavg.gt.0.0) then
          uE=ueavg
        else
          uE=-9999.0
        endif

        step=(7/datres) +1
        IF(step.gt.7) step=7

        IF(cosz.gt.0.0) then
          IF((a-step).lt.1) then
            s=1
          else
            s=a-step
          endif
          IF((a+step).gt.n24) then
            e=n24
          else
            e=a+step
          endif
          do i=s,e
            xarr(i)= rdata(i,12)
            sarr(i)= rdata(i,13)
          end do
        endif

        IF(cosz.gt.0.15.and.csw.gt.0.0) then
          IF(alb.le.0.0.or.alb.ge.1.0) then
            alb=dalb
          endif

          s=1
          IF(a.gt.1) s=a-1
          e=n24
          IF(a.lt.n24) e=a+1
          ussw=0.0
          kk=0
          do i=s,e
            IF(rdata(i,3).GT.-1.0) then
              ussw=ussw+rdata(i,3)
              kk=kk+1
            endif
          end do

          IF(kk.gt.0) then
            ussw=ussw/kk
          else
            ussw=-9999.0
          endif

cccccccccccccc use  Te to determine which g to use  ccccccccccc
          IF(scv.GT.-1.0.and.clw.gt.0.0.and.uE.gt.0.0) then
            telim=clw+scv*sigma*(1.0-uE)*tclim**4
            telim=((telim/sigma)**0.25)-2.0
            idata(a,7)=1
          ELSEIF(lscv.GT.-1.0.and.clw.gt.0.0.and.uE.gt.0.0) then
            telim=clw+lscv*sigma*(1.0-uE)*tclim**4
            telim=((telim/sigma)**0.25)+1.0
            idata(a,7)=2
          ELSEIF(clw.gt.0.0.and.uE.gt.0.0) then
            telim=clw+sigma*(1.0-uE)*tclim**4
            telim=((telim/sigma)**0.25)-2.0
            idata(a,7)=3
          ELSEIF(Ta.gt.0.0) then
            telim=0.965*Ta
            idata(a,7)=4
          else
            telim=tclim
            idata(a,7)=5
          endif
          rdata(a,62)=telim

          IF(scv.gt.0.95.and.ussw.gt.0.0) then
            IF(ug.eq.1.and.te.gt.0.0.and.te.le.telim) then
              r=ussw/(csw*cosz**0.25)
              tau=((tcof/r)-1.0)/((1.0-alb)*(1.0-gi))
              idata(a,6)=2
            ELSEIF(ug.eq.1.and.te.gt.telim) then
              r=ussw/(csw*cosz**0.25)
              tau=((tcof/r)-1.0)/((1.0-alb)*(1.0-g))
              idata(a,6)=1
            else
              r=ussw/(csw*cosz**0.25)
              tau=((tcof/r)-1.0)/((1.0-alb)*(1.0-g))
              idata(a,6)=1
            endif
            IF(tau.lt.0.0) tau=0.0
            IF(tau.gt.1000.0) tau=1000.0
cccccccccccccc if SW SCV missing, try LW SCV. Still use Te to determine which g to use  ccccccccccc
          ELSEIF(scv.LT.-1.0.and.Lscv.ge.0.90.and.ussw.gt.0.0.and.cosz
     %.gt.0.20) then
            IF(ug.eq.1.and.te.gt.0.0.and.te.le.telim) then
              r=ussw/(csw*cosz**0.25)
              tau=((tcof/r)-1.0)/((1.0-alb)*(1.0-gi))
              idata(a,6)=2
            ELSEIF(ug.eq.1.and.te.gt.telim) then
              r=ussw/(csw*cosz**0.25)
              tau=((tcof/r)-1.0)/((1.0-alb)*(1.0-g))
              idata(a,6)=1
            else
              r=ussw/(csw*cosz**0.25)
              tau=((tcof/r)-1.0)/((1.0-alb)*(1.0-g))
              idata(a,6)=1
            endif
            IF(tau.lt.0.0) tau=0.0
            IF(tau.gt.1000.0) tau=1000.0
cccccccccccccc Partly cloudy section THIS NEEDS WORK!!!!!  ccccccccccc
          elseif(scv.gt.stlim) then
            av=0.0
            av2=0.0
            j=0

            step=(7/datres) +1
            IF(step.gt.7) step=7
            IF((a-step).lt.1) then
              s=1
            else
              s=a-step
            endif
            IF((a+step).gt.n24) then
              e=n24
            else
              e=a+step
            endif

cccccccccc  calc 15-minute average of dir [av] and cdir [av2]
cccccccccc  calc 15-minute average ratio of dir/cdir [av/av2]
            do i=s,e
              IF(xarr(i).ge.0.0.and.sarr(i).ge.0.0) then
                av=av+xarr(i)
                av2=av2+sarr(i)
                j=j+1
              endif
            end do
            IF(j.gt.0) then
              av=av/(j*1.0)
              av2=av2/(j*1.0)
              av2=av/av2
              IF(av2.gt.1.0) av2=1.0
            else
              av=rerr
              av2=rerr
            endif

cccccccccc  use 15-minute average ratio of dir/cdir [av2] to "make" smoothed dir component
            if(av2.gt.0.0.and.cdir.gt.0.0.and.dif.gt.0.0) then
              av=av2*cdir+dif
cc              av2= (av-(1.0-scv)*csw)/scv
ccc              IF(av2.lt.0.0) av2=0.0
cc              r=dif/(av2*cosz**0.25)
              r=av/(csw*cosz**0.25)
              IF(ug.eq.1.and.te.gt.0.0.and.te.le.telim) then
                tau=((tcof/r)-1.0)/((1.0-alb)*(1.0-gi))
                idata(a,6)=2
              ELSEIF(ug.eq.1.and.te.gt.telim) then
                tau=((tcof/r)-1.0)/((1.0-alb)*(1.0-g))
                idata(a,6)=1
              else
                tau=((tcof/r)-1.0)/((1.0-alb)*(1.0-g))
                idata(a,6)=1
              endif
              IF(tau.lt.0.0) tau=0.0
              IF(tau.gt.1000.0) tau=1000.0
            else
              tau=-9998.0
            endif
          else
            tau=rerr
          endif
        else
          tau=rerr
        endif

        rdata(a,16)= tau

ccccccccccccccccccccc  end cloud tau section  ccccccccccccccccccccccccc

ccccccccccccccccccccc   cloud trans section  cccccccccccccccccccccccccccc

        ssw=rdata(a,3)
        csw=rdata(a,4)
        cosz=rdata(a,1)

        IF(cosz.gt.0.1.and.ssw.gt.0.0.and.csw.gt.20.0) then
          swtr=ssw/csw
        else
          swtr=-9999.0
        endif
        IF(swtr.gt.99999.0) swtr=-9999.0
        rdata(a,61)=swtr

ccccccccccccccccccccc   end cloud trans section  cccccccccccccccccccccccccccc


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

ccc   Add "CLWup" column filled with "-9999.0" so output format same as
ccc        "*.flx" files. Also change extension to "*.lw1"
ccc       "CLWup" after rdata(a,9)=LWup


cc    write out "clrout" data if flag set
        clr=idata(a,5)
        scv=rdata(a,15)
        IF(clrout.and.clr.gt.0.and.clw.gt.0.0.and.lw.gt.0.0.and.
     %scv.lt.0.04) then
          WRITE(9,348) (idata(a,i),i=1,4),(rdata(a,i),i=1,9),clwu(a),
     %(rdata(a,i),i=10,13),idata(a,5),idata(a,6),idata(a,7),
     %(rdata(a,i),i=14,16),(rdata(a,i),i=61,63),(rdata(a,i),i=17,24+nx2)
        endif

        IF(rdata(a,17).gt.500.0) rdata(a,17)=rerr
        IF(rdata(a,18).gt.25.0) rdata(a,18)=rerr
cc    write out 1-minute data
          WRITE(3,348)(idata(a,i),i=1,4),(rdata(a,i),i=1,9),clwu(a),
     %(rdata(a,i),i=10,13),idata(a,5),idata(a,6),idata(a,7),
     %(rdata(a,i),i=14,16),(rdata(a,i),i=61,63),(rdata(a,i),i=17,24+nx2)
cc        "lw1" format
 348   FORMAT(i8,i6,2x,i8,i6,2f13.6,4f9.1,2f9.2,6f9.1,3i6,11f10.2,
     %65f12.4)

      END do

 350  CLOSE(UNIT=3)

ccccccccc  15-minute averages section  ccccccccccccccccccccccc
      IF(.not.oneout) GO TO 360

      outfil=infil(1:9)//'l15'
      OPEN(UNIT=3,FILE=outfil,STATUS='unknown')
      WRITE(6,*)' Opened L15 file: ',outfil
      WRITE(3,*) head15(1:h15)//head2(1:h2)

      do a=1,74
        dav(a)=0.0
        aav(a)=0.0
        naav(a)=0
        dn(a)=0
      end do

      do a=1,10
        aadv(a)=0.0
      end do

      clr=0
      tclr=0
      dclr=0

      tmin=15
      ltim=idata(1,4)
      tlhr=ltim/100
      tzhr=idata(1,2)/100
      ldat=idata(1,3)
      zdat=idata(1,1)
      lastim=tlhr*100
      ztim=tzhr*100
      ttim=tlhr*100+tmin

 351  IF(ltim.gt.ttim) then
        lastim=tlhr*100+tmin
        ztim=tzhr*100+tmin
        tmin=tmin+15
        ttim=tlhr*100+tmin
        IF(tmin.gt.45) then
          tmin=0
          ttim=(tlhr+1)*100+tmin
        endif
        GO TO 351
      endif

      do a=1,n24
        ltim=idata(a,4)
        IF(ltim.lt.ttim) then
          do i=1,24+nx2
            IF(rdata(a,i).GT.-900.0) then
              aav(i)=aav(i)+rdata(a,i)
              naav(i)=naav(i)+1
              IF(i.eq.15) then
                ascv(naav(15))=rdata(a,15)
              endif
            endif
          end do
          IF(idata(a,5).ge.0) then
            tclr=tclr+1
            IF(idata(a,5).gt.0.and.idata(a,5).lt.4) clr=clr+1
            IF(idata(a,5).gt.0.and.idata(a,5).lt.4) dclr=dclr+1
          endif
          IF(idata(a,6).gt.0) then
            aav(72)=aav(72)+idata(a,6)
            naav(72)=naav(72)+1
          endif
          IF(rdata(a,3).GT.10.0.and.rdata(a,4).GT.10.0) then
            aav(62)=aav(62)+ rdata(a,3)/rdata(a,4)
            naav(62)=naav(62)+1
            swcer(naav(62))= rdata(a,3)/rdata(a,4)
          endif
          IF(rdata(a,5).GT.0.0.and.rdata(a,6).GT.0.0) then
            aav(64)=aav(64)+ rdata(a,5)-rdata(a,6)
            naav(64)=naav(64)+1
            lwce(naav(64))= rdata(a,5)-rdata(a,6)
          endif
          IF(rdata(a,10).GT.5.0.and.rdata(a,11).GT.5.0) then
            aav(66)=aav(66)+ rdata(a,10)-rdata(a,11)
            naav(66)=naav(66)+1
            difce(naav(66))= rdata(a,10)-rdata(a,11)
          endif
          IF(rdata(a,5).gt.maxLW) maxLW= rdata(a,5)
          IF(rdata(a,3).GT.0.0) then
            aav(68)=aav(68)+ rdata(a,3)
            naav(68)=naav(68)+1
            s15(naav(68))= rdata(a,3)
          endif
          IF(rdata(a,5).GT.0.0) then
            aav(69)=aav(69)+ rdata(a,5)
            naav(69)=naav(69)+1
            l15(naav(69))= rdata(a,5)
          endif
          IF(rdata(a,10).GT.0.0) then
            aav(70)=aav(70)+ rdata(a,10)
            naav(70)=naav(70)+1
            d15(naav(70))= rdata(a,10)
          endif
          IF(rdata(a,61).GT.0.0) then
            aav(71)=aav(71)+ rdata(a,61)
            naav(71)=naav(71)+1
          endif
          IF(rdata(a,62).GT.0.0) then
            aav(73)=aav(73)+ rdata(a,62)
            naav(73)=naav(73)+1
          endif
          IF(rdata(a,63).GT.0.0) then
            aav(74)=aav(74)+ rdata(a,63)
            naav(74)=naav(74)+1
          endif
          zdat=idata(a,1)

        else
          do i=1,24+nx2
            IF(naav(i).GT.alim) then
              aav(i)=aav(i)/naav(i)*1.0
            else
              aav(i)=rerr
            endif
          end do

          IF(naav(15).gt.alim) then
            do i=1,naav(15)
              aadv(1)=aadv(1) + ABS(aav(15)-ascv(i))
            end do
            aadv(1)=aadv(1)/naav(15)
          else
            aadv(1)=rerr
          endif

          IF(naav(62).gt.alim) then
            aav(62)=aav(62)/naav(62)*1.0
            do i=1,naav(62)
              aadv(2)=aadv(2) + ABS(aav(62)-swcer(i))
            end do
            aadv(2)=aadv(2)/naav(62)
            aav(63)=aadv(2)
          else
            aav(62)=rerr
            aav(63)=rerr
          endif

          IF(naav(64).gt.alim) then
            aav(64)=aav(64)/naav(64)*1.0
            do i=1,naav(64)
              aadv(3)=aadv(3) + ABS(aav(64)-lwce(i))
            end do
            aadv(3)=aadv(3)/naav(64)
            aav(65)=aadv(3)
          else
            aav(64)=rerr
            aav(65)=rerr
          endif

          IF(naav(66).gt.alim) then
            aav(66)=aav(66)/naav(66)*1.0
            do i=1,naav(66)
              aadv(4)=aadv(4) + ABS(aav(66)-difce(i))
            end do
            aadv(4)=aadv(4)/naav(66)
            aav(67)=aadv(4)
          else
            aav(66)=rerr
            aav(67)=rerr
          endif

          IF(naav(68).gt.alim) then
            aav(68)=aav(68)/naav(68)*1.0
            do i=1,naav(68)
              aadv(8)=aadv(8) + ABS(aav(68)-s15(i))
            end do
            aadv(8)=aadv(8)/naav(68)
            aav(68)=aadv(8)
          else
            aav(68)=rerr
          endif

          IF(naav(69).gt.alim) then
            aav(69)=aav(69)/naav(69)*1.0
            do i=1,naav(69)
              aadv(9)=aadv(9) + ABS(aav(69)-l15(i))
            end do
            aadv(9)=aadv(9)/naav(69)
            aav(69)=aadv(9)
          else
            aav(68)=rerr
          endif

          IF(naav(70).gt.alim) then
            aav(70)=aav(70)/naav(70)*1.0
            do i=1,naav(70)
              aadv(10)=aadv(10) + ABS(aav(70)-d15(i))
            end do
            aadv(10)=aadv(10)/naav(70)
            aav(70)=aadv(10)
          else
            aav(70)=rerr
          endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          IF(naav(71).gt.alim) then
            aav(71)=aav(71)/naav(71)*1.0
          else
            aav(71)=rerr
          endif
          IF(naav(72).gt.alim) then
            aav(72)=aav(72)/naav(72)*1.0
          else
            aav(72)=rerr
          endif
          IF(naav(73).gt.alim) then
            aav(73)=aav(73)/naav(73)*1.0
          else
            aav(73)=rerr
          endif
          IF(naav(74).gt.alim) then
            aav(74)=aav(74)/naav(74)*1.0
          else
            aav(74)=rerr
          endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc        "l15" format
 358  FORMAT(i8,i6,2x,i8,i6,2f13.6,4f9.1,2f9.2,5f9.1,2i6,13f10.2,
     %99f12.4)
          IF(nx2.gt.0) then
            WRITE(3,358) zdat,ztim,ldat,lastim,(aav(i),i=1,13),tclr,clr,
     %aav(72),aav(14),aav(15),aadv(1),aav(16),aav(71),(aav(i),i=73,74),
     %(aav(i),i=17,23),(aav(i),i=62,70),maxlw,(aav(i),i=24,(24+nx2))
            WRITE(11,358)zdat,ztim,ldat,lastim,(aav(i),i=1,13),tclr,clr,
     %aav(72),aav(14),aav(15),aadv(1),aav(16),aav(71),(aav(i),i=73,74),
     %(aav(i),i=17,23),(aav(i),i=62,70),maxlw,(aav(i),i=24,(24+nx2))
            IF(aav(1).lt.0.0) then
            WRITE(22,358)zdat,ztim,ldat,lastim,(aav(i),i=1,13),tclr,clr,
     %aav(72),aav(14),aav(15),aadv(1),aav(16),aav(71),(aav(i),i=73,74),
     %(aav(i),i=17,23),(aav(i),i=62,70),maxlw,(aav(i),i=24,(24+nx2))
            endif
          else
            WRITE(3,358)zdat,ztim,ldat,lastim,(aav(i),i=1,13),tclr,clr,
     %aav(72),aav(14),aav(15),aadv(1),aav(16),aav(71),(aav(i),i=73,74),
     %(aav(i),i=17,23),(aav(i),i=62,70),maxlw
            WRITE(11,358)zdat,ztim,ldat,lastim,(aav(i),i=1,13),tclr,clr,
     %aav(72),aav(14),aav(15),aadv(1),aav(16),aav(71),(aav(i),i=73,74),
     %(aav(i),i=17,23),(aav(i),i=62,70),maxlw
            IF(aav(1).lt.0.0) then
            WRITE(22,358)zdat,ztim,ldat,lastim,(aav(i),i=1,13),tclr,clr,
     %aav(72),aav(14),aav(15),aadv(1),aav(16),aav(71),(aav(i),i=73,74),
     %(aav(i),i=17,23),(aav(i),i=62,70),maxlw
            endif
          endif

          do i=1,74
            aav(i)=0.0
            naav(i)=0
          end do

          do i=1,10
            aadv(i)=0.0
          end do

          clr=0
          tclr=0

          zdat=idata(a,1)
          lastim=ttim
          tlhr=ttim/100
          tmin=ttim-(tlhr*100)
          tzhr=idata(a,2)/100
          ztim=tzhr*100+tmin
          tmin=tmin+15
          ttim=tlhr*100+tmin
          IF(tmin.gt.45) then
            tmin=0
            ttim=(tlhr+1)*100+tmin
          endif

          do i=1,24+nx2
            IF(rdata(a,i).GT.-900) then
              aav(i)=rdata(a,i)
              naav(i)=1
              IF(i.eq.15) then
                ascv(1)=rdata(a,15)
              endif
            endif
          end do

          maxLW= rdata(a,5)

          IF(idata(a,5).ge.0) then
            tclr=1
            IF(idata(a,5).gt.0.and.idata(a,5).lt.4) clr=1
          endif
          IF(idata(a,6).gt.0) then
            aav(72)=idata(a,6)
            naav(72)=1
          endif
          IF(rdata(a,3).GT.10.0.and.rdata(a,4).GT.10.0) then
            aav(62)=rdata(a,3)/rdata(a,4)
            naav(62)=1
            swcer(naav(62))= rdata(a,3)/rdata(a,4)
          endif
          IF(rdata(a,5).GT.0.0.and.rdata(a,6).GT.0.0) then
            aav(64)=rdata(a,5)-rdata(a,6)
            naav(64)=1
            lwce(naav(64))= rdata(a,5)-rdata(a,6)
          endif
          IF(rdata(a,10).GT.5.0.and.rdata(a,11).GT.5.0) then
            aav(66)=rdata(a,10)-rdata(a,11)
            naav(66)=1
            difce(naav(66))= rdata(a,10)-rdata(a,11)
          endif
          IF(rdata(a,3).GT.0.0) then
            aav(68)= rdata(a,3)
            naav(68)=1
            s15(naav(68))= rdata(a,3)
          endif
          IF(rdata(a,5).GT.0.0) then
            aav(69)= rdata(a,5)
            naav(69)=1
            l15(naav(69))= rdata(a,5)
          endif
          IF(rdata(a,10).GT.0.0) then
            aav(70)= rdata(a,10)
            naav(70)=1
            d15(naav(70))= rdata(a,10)
          endif
          IF(rdata(a,61).GT.0.0) then
            aav(71)=rdata(a,61)
            naav(71)=1
          endif
          IF(rdata(a,62).GT.0.0) then
            aav(73)=rdata(a,62)
            naav(73)=1
          endif
          IF(rdata(a,63).GT.0.0) then
            aav(74)=rdata(a,63)
            naav(74)=1
          endif
        endif
      end do

          do i=1,24+nx2
            IF(naav(i).GT.alim) then
              aav(i)=aav(i)/naav(i)*1.0
            else
              aav(i)=rerr
            endif
          end do

          IF(naav(15).gt.alim) then
            do i=1,naav(15)
              aadv(1)=aadv(1) + ABS(aav(15)-ascv(i))
            end do
            aadv(1)=aadv(1)/naav(15)
          else
            aadv(1)=rerr
          endif

          IF(naav(62).gt.alim) then
            aav(62)=aav(62)/naav(62)*1.0
            do i=1,naav(62)
              aadv(2)=aadv(2) + ABS(aav(62)-swcer(i))
            end do
            aadv(2)=aadv(2)/naav(62)
            aav(63)=aadv(2)
          else
            aav(62)=rerr
            aav(63)=rerr
          endif

          IF(naav(64).gt.alim) then
            aav(64)=aav(64)/naav(64)*1.0
            do i=1,naav(64)
              aadv(3)=aadv(3) + ABS(aav(64)-lwce(i))
            end do
            aadv(3)=aadv(3)/naav(64)
            aav(65)=aadv(3)
          else
            aav(64)=rerr
            aav(65)=rerr
          endif

          IF(naav(66).gt.alim) then
            aav(66)=aav(66)/naav(66)*1.0
            do i=1,naav(66)
              aadv(4)=aadv(4) + ABS(aav(66)-difce(i))
            end do
            aadv(4)=aadv(4)/naav(66)
            aav(67)=aadv(4)
          else
            aav(66)=rerr
            aav(67)=rerr
          endif

          IF(naav(68).gt.alim) then
            aav(68)=aav(68)/naav(68)*1.0
            do i=1,naav(68)
              aadv(8)=aadv(8) + ABS(aav(68)-s15(i))
            end do
            aadv(8)=aadv(8)/naav(68)
            aav(68)=aadv(8)
          else
            aav(68)=rerr
          endif

          IF(naav(69).gt.alim) then
            aav(69)=aav(69)/naav(69)*1.0
            do i=1,naav(69)
              aadv(9)=aadv(9) + ABS(aav(69)-l15(i))
            end do
            aadv(9)=aadv(9)/naav(69)
            aav(69)=aadv(9)
          else
            aav(69)=rerr
          endif

          IF(naav(70).gt.alim) then
            aav(70)=aav(70)/naav(70)*1.0
            do i=1,naav(70)
              aadv(10)=aadv(10) + ABS(aav(70)-d15(i))
            end do
            aadv(10)=aadv(10)/naav(70)
            aav(70)=aadv(10)
          else
            aav(70)=rerr
          endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          IF(naav(71).gt.alim) then
            aav(71)=aav(71)/naav(71)*1.0
          else
            aav(71)=rerr
          endif
          IF(naav(72).gt.alim) then
            aav(72)=aav(72)/naav(72)*1.0
          else
            aav(72)=rerr
          endif
          IF(naav(73).gt.alim) then
            aav(73)=aav(73)/naav(73)*1.0
          else
            aav(73)=rerr
          endif
          IF(naav(74).gt.alim) then
            aav(74)=aav(74)/naav(74)*1.0
          else
            aav(74)=rerr
          endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          IF(nx2.gt.0) then
            WRITE(3,358) zdat,ztim,ldat,lastim,(aav(i),i=1,13),tclr,clr,
     %aav(72),aav(14),aav(15),aadv(1),aav(16),aav(71),(aav(i),i=73,74),
     %(aav(i),i=17,23),(aav(i),i=62,70),maxlw,(aav(i),i=24,(24+nx2))
            WRITE(11,358)zdat,ztim,ldat,lastim,(aav(i),i=1,13),tclr,clr,
     %aav(72),aav(14),aav(15),aadv(1),aav(16),aav(71),(aav(i),i=73,74),
     %(aav(i),i=17,23),(aav(i),i=62,70),maxlw,(aav(i),i=24,(24+nx2))
            IF(aav(1).lt.0.0) then
            WRITE(22,358)zdat,ztim,ldat,lastim,(aav(i),i=1,13),tclr,clr,
     %aav(72),aav(14),aav(15),aadv(1),aav(16),aav(71),(aav(i),i=73,74),
     %(aav(i),i=17,23),(aav(i),i=62,70),maxlw,(aav(i),i=24,(24+nx2))
            endif
          else
            WRITE(3,358)zdat,ztim,ldat,lastim,(aav(i),i=1,13),tclr,clr,
     %aav(72),aav(14),aav(15),aadv(1),aav(16),aav(71),(aav(i),i=73,74),
     %(aav(i),i=17,23),(aav(i),i=62,70),maxlw
            WRITE(11,358)zdat,ztim,ldat,lastim,(aav(i),i=1,13),tclr,clr,
     %aav(72),aav(14),aav(15),aadv(1),aav(16),aav(71),(aav(i),i=73,74),
     %(aav(i),i=17,23),(aav(i),i=62,70),maxlw
            IF(aav(1).lt.0.0) then
            WRITE(22,358)zdat,ztim,ldat,lastim,(aav(i),i=1,13),tclr,clr,
     %aav(72),aav(14),aav(15),aadv(1),aav(16),aav(71),(aav(i),i=73,74),
     %(aav(i),i=17,23),(aav(i),i=62,70),maxlw
            endif
          endif


          do i=1,74
            aav(i)=0.0
            naav(i)=0
          end do

          do i=1,10
            aadv(i)=0.0
          end do

          clr=0
          tclr=0

      CLOSE(UNIT=3)

cccccccccccccccc Daily Averages ccccccccccccccccccccccccc
 360  nalb=0
      ncalb=0
      nscv=0
      nlscv=0
      ntau=0
      nct=0
      nch=0

      do a=1,74
        dav(a)=0.0
        aav(a)=0.0
        naav(a)=0
        dn(a)=0
      end do

      do a=1,10
        aadv(a)=0.0
      end do

      clr=0
      tclr=0
      dclr=0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do a=1,n24
        cosz=rdata(a,1)
        IF(idata(a,5).gt.0.and.idata(a,5).lt.4) dclr=dclr+1
        do i=1,24+nx2
          IF(rdata(a,i).GT.-900.0) then
            dn(i)=dn(i)+1
            dav(i)=dav(i)+rdata(a,i)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          IF(i.eq.7.and.rdata(a,i).gt.0.0.and.cosz.gt.0.0) nalb=nalb+1
          IF(i.eq.8.and.rdata(a,i).gt.0.0.and.cosz.gt.0.0) ncalb=ncalb+1
          IF(i.eq.14.and.rdata(a,i).GT.-1.0) nlscv=nlscv+1
          IF(i.eq.17.and.rdata(a,i).GT.-1.0) nct=nct+1
          IF(i.eq.18.and.rdata(a,i).GT.-1.0) nch=nch+1
          IF(i.eq.15.and.rdata(a,i).GE.-1.0.and.cosz.ge.0.2) nscv=nscv+1
          IF(i.eq.16.and.rdata(a,i).GT.-1.0.and.cosz.ge.0.2) ntau=ntau+1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccc  daylight (cosZ>0) possible data count: gsw 63, dif 65, dir 67  cccccccccc
            IF(i.eq.3.and.cosz.gt.0.0) dn(63)=dn(63)+1
            IF(i.eq.10.and.cosz.gt.0.0) dn(65)=dn(65)+1
            IF(i.eq.12.and.cosz.gt.0.0) dn(67)=dn(67)+1
cccccccccc  daylight (ClrVal>0) data count: GSW and CSW 62  cccccccccc
cc      NOTE: CosZ = -0.052336 is approximately a SZA of 93 degrees
            IF(i.eq.3.and.rdata(a,4).gt.0.0.and.cosz.gt.-0.052336) then
              dav(62)=dav(62)+rdata(a,3)
              dav(63)=dav(63)+rdata(a,4)
              dn(62)=dn(62)+1
            endif
cccccccccc  daylight (ClrVal>0) data count: dif and Cdif 64  cccccccccc
cc      NOTE: CosZ = -0.052336 is approximately a SZA of 93 degrees
           IF(i.eq.10.and.rdata(a,11).gt.0.0.and.cosz.gt.-0.052336) then
              dav(64)=dav(64)+rdata(a,10)
              dav(65)=dav(65)+rdata(a,11)
              dn(64)=dn(64)+1
            endif
cccccccccc  daylight (ClrVal>0) data count: dir and Cdir 66  cccccccccc
            IF(i.eq.12.and.rdata(a,13).gt.0.0.and.cosz.gt.0.0) then
              dav(66)=dav(66)+rdata(a,12)
              dav(67)=dav(67)+rdata(a,13)
              dn(66)=dn(66)+1
            endif
          endif
        end do
      END do

      drn=NINT(1440/datres*1.d0)
      do i=1,(24+nx2)
        IF(i.ge.14.and.i.le.18.and.dn(i).gt.1)then
ccccccc  bypass for cloud properties tested later
          dav(i)=dav(i)/dn(i)*1.0
        elseIF(dn(i).gt.(drn/2)) then
          dav(i)=dav(i)/dn(i)*1.0
        else
          dav(i)=rerr
        endif
      end do

      dayn=0
      ncz2=0

      IF(dn(4).lt.drn.or.dn(11).lt.drn.or.dn(13).lt.drn) THEN
        yr=ldat/10000
        mn=ldat/100-(yr*100)
        dy=ldat-yr*10000-mn*100
        dav(4)=0.d0
        dav(11)=0.d0
        dav(13)=0.d0

cc      calculate linear interpolation point for clear diffuse ratio
        lcosz=0.0001d0
        lcdifr=dra*(lcosz**drb)
 370    continue
        lcosz=lcosz+0.0001d0
        cdifr=dra*(lcosz**drb)
        cslope=(cdifr-lcdifr)/0.0001d0
        lslope=(cdifr-1.d0)/lcosz
        if(lslope.ge.cslope) then
          lcdifr=cdifr
          go to 370
        endif

        do a=0,1439
          ltim=((a/60)*100)+(a-(a/60)*60)
          rtim=ltim*1.d0
          CALL EPHEMS(LAT,LONG,dy,mn,yr,rtim,0,0,0,0,
     +            AZ,AU,Z,HR,DEC)
          cosz=dCOS(Z*PI/180.d0)
          IF(ltim.eq.100) then
            rtim=59.d0
            CALL EPHEMS(LAT,LONG,dy,mn,yr,rtim,0,0,0,0,
     +            AZ,AU,Z,HR,DEC)
            cosz=dCOS(Z*PI/180.d0)
            rtim=101.d0
            CALL EPHEMS(LAT,LONG,dy,mn,yr,rtim,0,0,0,0,
     +            AZ,AU,Z,HR,DEC)
            cosz=(COSZ+dCOS(Z*PI/180.d0))/2.D0
          ENDIF
cc
cc      NOTE: CosZ = -0.052336 is approximately a SZA of 93 degrees
cc
cc    for adjusted  csw, set up "uz" and "ucosz" here
cc    for cdif, use cdifr*adjcsw for SZA<=90, cdif=adjcsw for sza>90
cc
          IF(z.ge.88.0.and.z.le.90.0) then
            uz=88.0+(z-88.0)*incr1
            ucosz=dCOS(uz*pi/180.0)
          ELSEIF(z.gt.90.0.and.z.le.93.0) then
            uz=adjz90+(z-90.0)*incr2
            ucosz=dCOS(uz*pi/180.0)
          else
            ucosz=cosz
            uz=z
          endif

          IF(ucosz.gt.0.0) then
            IF(ddt.eq.0) then
              csw=(swa/au**2)*ucosz**swb
            else
              csw=swa*ucosz**swb
            endif
            dav(4)=dav(4)+csw
            dayn=dayn+1
            if(cosz.ge.lcosz) then
              cdif= (dra*cosz**drb)*csw
            elseif(cosz.lt.lcosz.and.cosz.gt.0.d0) then
              cdif=((((dra*(lcosz**drb))-1.0)*cosz/lcosz)+1.0)*csw
            elseif(cosz.le.0.d0.and.ucosz.ge.0.d0) then
              cdif=csw
            endif
            dav(11)=dav(11)+ cdif
          endif
          IF(cosz.ge.0.2) ncz2=ncz2+1
        end do
        dav(4)=dav(4)/1440.0
        dav(11)=dav(11)/1440.0
        dav(13)=dav(4)-dav(11)
      else
        yr=ldat/10000
        mn=ldat/100-(yr*100)
        dy=ldat-yr*10000-mn*100
        do a=0,1439
          ltim=((a/60)*100)+(a-(a/60)*60)
          rtim=ltim*1.d0
          CALL EPHEMS(LAT,LONG,dy,mn,yr,rtim,0,0,0,0,
     +            AZ,AU,Z,HR,DEC)
          cosz=dCOS(Z*PI/180.d0)
          IF(ltim.eq.100) then
            rtim=59.d0
            CALL EPHEMS(LAT,LONG,dy,mn,yr,rtim,0,0,0,0,
     +            AZ,AU,Z,HR,DEC)
            cosz=dCOS(Z*PI/180.d0)
            rtim=101.d0
            CALL EPHEMS(LAT,LONG,dy,mn,yr,rtim,0,0,0,0,
     +            AZ,AU,Z,HR,DEC)
            cosz=(COSZ+dCOS(Z*PI/180.d0))/2.D0
          ENDIF
          IF(cosz.gt.0.0) dayn=dayn+1
          IF(cosz.ge.0.2) ncz2=ncz2+1
        END do
      endif
      dayn=NINT(dayn/datres*1.d0)
      ncz2=NINT(ncz2/datres*1.d0)

cccc Check DayAvg SWu and CSWu  ccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          IF(nalb.lt.(dayn/2)) dav(7)=rerr
          IF(ncalb.lt.(dayn/2)) dav(8)=rerr
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccc Check DayAvg SCV and Tau  ccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          IF(nscv.lt.(ncz2/2)) dav(15)=rerr
          IF(dav(15).GT.-1.0.and.ntau.lt.(nscv/2)) dav(16)=rerr
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccc Check DayAvg LWscv, CBT, and CBH  ccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          IF(nlscv.lt.(drn/2)) dav(14)=rerr
          IF(dav(14).GT.-1.0.and.nct.lt.(nlscv/2)) dav(17)=rerr
          IF(dav(14).GT.-1.0.and.nch.lt.(nlscv/2)) dav(18)=rerr
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccc Check DayAvg GSW  ccccc
      gflg=0
      IF(dn(63).lt.dayn) then
        IF(dn(65).ge.dayn.and.dn(67).ge.dayn) then
cccc if Dif and Dir OK, GSW=dif+dir
          dav(3)=dav(10)+dav(12)
          gflg=1
        elseIF(dn(62).ge.dayn/2) then
cccc else if # data >50% use avail meas/clr ratio * dayavg clear
          dav(62)=dav(62)/dav(63)
          dav(3)= dav(62)*dav(4)
          gflg=2
        ELSEIF(dn(63).lt.dayn/2) then
cccc else if # data <50% set to bad
          dav(3)=rerr
          gflg=3
        endif
      endif

cccc Check DayAvg Dif  ccccc
      dflg=0
      IF(dn(65).lt.dayn) then
        IF(dn(63).ge.dayn.and.dn(67).ge.dayn) then
cccc if GSW and Dir OK, dif=gsw-dir
          dav(10)=dav(3)-dav(12)
          dflg=1
        elseIF(dn(64).ge.dayn/2) then
cccc else if # data >50% use avail meas/clr ratio * dayavg clear
          dav(64)=dav(64)/dav(65)
          dav(10)= dav(64)*dav(11)
          dflg=2
        ELSEIF(dn(65).lt.dayn/2) then
cccc else if # data <50% set to bad
          dav(10)=rerr
          dflg=3
        endif
      endif

cccc Check DayAvg Dir  ccccc
      rflg=0
      IF(dn(67).lt.dayn) then
        IF(dn(63).ge.dayn.and.dn(65).ge.dayn) then
cccc if GSW and Dif OK, dir=gsw-dif
          dav(12)=dav(3)-dav(10)
          rflg=1
        ELSEIF(dn(66).ge.dayn/2) then
cccc else if # data >50% use avail meas/clr ratio * dayavg clear
          dav(66)=dav(66)/dav(67)
          dav(12)= dav(66)*dav(13)
          rflg=2
        ELSEIF(dn(67).lt.dayn/2) then
cccc else if # data <50% set to bad
          dav(12)=rerr
          rflg=3
        endif
      endif

cccc ReCheck DayAvg Dif and Dir given above  ccccc
      IF(dn(65).lt.dayn.or.dn(67).lt.dayn) then
        IF(dn(64).ge.dn(66).and.dav(3).gt.0.0.and.dav(10).gt.0.0) then
          dav(12)=dav(3)-dav(10)
          IF(dav(12).lt.0.0) then
            dav(12)=0.0
            dav(10)=dav(3)
          endif
          rflg=5
       ELSEIF(dn(66).gt.dn(64).and.dav(3).gt.0.0.and.dav(12).gt.0.0)then
          dav(10)=dav(3)-dav(12)
          dflg=5
        endif
      endif

      IF(dn(5).LT.(drn/2)) dav(5)=rerr
      IF(dn(6).lt.(drn/2)) dav(6)=rerr

      IF(dav(3).gt.0.0.and.dav(4).gt.0.0) then
        swtr=dav(3)/dav(4)
      else
        swtr=-9999.0
      endif

      WRITE(33,398) LDAT,n24,dayn,dn(63),dn(65),dn(5),dn(6),
     %gflg,dflg,rflg,(dav(i),i=1,13),dclr,(dav(i),i=14,16),swtr,
     %(dav(i),i=17,24+nx2)
 398  FORMAT(7i8,3i5,2f13.6,4f9.1,2f9.2,5f9.1,i6,8f10.2,60f12.4)

      GO TO 305


 400  CLOSE(UNIT=1)
      IF(CLROUT) CLOSE(UNIT=9)
      CLOSE(UNIT=33)
      CLOSE(UNIT=11)
      CLOSE(UNIT=22)


      stop
      end program

cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine CLRCOF1(coffil,outfil)
      PARAMETER (NDAYMAX=140000)
      INTEGER n,cdate(0:NDAYMAX),c(0:NDAYMAX),date,i,k
      INTEGER l,j,m(5),d(0:NDAYMAX)
      REAL*8 coeff(0:NDAYMAX),lc,ocoeff(0:NDAYMAX)
      CHARACTER*12 COFFIL,lasfil,outfil
      LOGICAL ok

      lasfil='Clr_LWdn.end'
      do n=0,NDAYMAX
        ocoeff(n)=0.0
        coeff(n)=0.0
        cdate(n)=0
        c(n)=0
        d(n)=0
      end do
      OK=.TRUE.

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
 3      read(1,*,end=10)cdate(n),(m(i),i=1,4),coeff(n)
        c(n)=m(4)
        d(n)=m(2)
        ocoeff(n)=coeff(n)
        go to 2

 5      format(i8,2f10.6,4i6)
 10   close(unit=1)

ccccccccccccccccc multiple day loop cccccccccccccccccccccccccc

        n=n-1
        WRITE(6,*)' CLRCOF1.sub Number of days = ',n
        open(unit=78,file=lasfil,status='old',ERR=11)
        read(78,*)
        read(78,*,ERR=11) date,lc
        close(unit=78)
        GO TO 12

 11     date=00000000
        lc=0.0

 12     k=0
cc===== added initialize j=1 to fix UNIX initialization bug per Chris Cox 8/26/2015
        j=1
cc===========================================================================
        IF(coeff(1).lt.0.0) coeff(1)=lc
        do 14 i=1,n
         if(coeff(i).lt.0.0) then
           k=k+1
         ELSEIF(OK.and.i.eq.1) THEN
           write(2,5)cdate(i),coeff(i),ocoeff(i),c(i),d(i)
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
             write(2,5)cdate(l),coeff(l),ocoeff(l),c(l),d(l)
 13        continue
           lc=coeff(i)
           k=0
         endif
 14     continue

        if(k.gt.0) then
          do 19 i=(n-(k-1)),n
            coeff(i)=lc
            write(2,5)cdate(i),coeff(i),ocoeff(i),c(i),d(i)
 19       continue
        endif
        close(unit=2)
        open(unit=78,file=lasfil,status='unknown')
        write(78,*)' SWFCLRIR Last day and coefficients:'
        write(78,5)cdate(j),coeff(j),ocoeff(j),c(j),d(j)
        write(78,*)cdate(j),coeff(j),ocoeff(j),c(j),d(j)
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
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine CLRCOF2(nlim,coffil,outfil,slim)
      PARAMETER (NDAYMAX=140000)
      INTEGER n,cdate(0:NDAYMAX),c(0:NDAYMAX),date,i,k
      INTEGER l,nlim,j,m(5),d(0:NDAYMAX),num1,num2,dnum
      REAL*8 coeff(0:NDAYMAX),lc,ocoeff(0:NDAYMAX)
      REAL*8 n1(ndaymax),n2(ndaymax),av1,av2,std1,std2,slim
      REAL*8 d1(ndaymax),dav,dstd
      CHARACTER*12 COFFIL,lasfil,outfil
      LOGICAL ok

cc      slim=1.5

      lasfil='Clr_LWdn.end'
      do n=0,NDAYMAX
        ocoeff(n)=0.0
        coeff(n)=0.0
        cdate(n)=0
        c(n)=0
        d(n)=0
      end do
      OK=.TRUE.
      num1=0
      num2=0
      av1=0.0
      av2=0.0
      std1=0.0
      std2=0.0
      dnum=0
      dav=0.0
      dstd=0.0

      open(unit=1,file=coffil,status='old',err=91)
      open(unit=2,file=outfil,status='unknown')
      read(1,*)
      n=1
 2      IF(n.gt.NDAYMAX) then
          WRITE(6,*)'*********************************************'
          WRITE(6,*)'Number of days greater than limit of',NDAYMAX,'!'
          WRITE(6,*)' Processing up to ',cdate(n-1),' only!'
          WRITE(6,*)'    Click on OK to continue.'
          WRITE(6,*)'*********************************************'
          pause
          GO TO 10
        endif
 3      read(1,*,end=10)cdate(n),(m(i),i=1,4),coeff(n),coeff(n+1),
     %coeff(n+2)
        c(n)=m(3)
        c(n+1)=m(3)
        c(n+2)=m(3)
        d(n)=m(4)
        d(n+1)=m(4)
        d(n+2)=m(4)
        ocoeff(n)=coeff(n)
        ocoeff(n+1)=coeff(n+1)
        ocoeff(n+2)=coeff(n+2)
        cdate(n+1)=cdate(n)
        cdate(n+2)=cdate(n)
        IF(coeff(n).gt.0.0) then
          num1=num1+1
          n1(num1)=coeff(n)
          av1=av1+coeff(n)
        endif
        IF(coeff(n+2).gt.0.0) then
          num2=num2+1
          n2(num2)=coeff(n+2)
          av2=av2+coeff(n+2)
        endif
        IF(coeff(n+1).gt.0.0) then
          dnum=dnum+1
          d1(dnum)=coeff(n+1)
          dav=dav+coeff(n+1)
        endif
        n=n+3
        go to 2


 5      format(i8,2f10.6,4i6)
 10   close(unit=1)

      IF(num1.gt.1) then
        av1=av1/num1
        do i=1,num1
          std1=std1+(n1(i)-av1)**2
        end do
        std1=SQRT(std1/num1)
      ELSEIF(num1.eq.1) then
        av1=n1(1)
        std1=0.0
      ELSE
        av1=-9.0
        std1=-9.0
      endif

      IF(num2.gt.1) then
        av2=av2/num2
        do i=1,num2
          std2=std2+(n2(i)-av2)**2
        end do
        std2=SQRT(std2/num2)
      ELSEIF(num2.eq.1) then
        av2=n2(1)
        std2=0.0
      ELSE
        av2=-9.0
        std2=-9.0
      endif

      IF(dnum.gt.1) then
        dav=dav/dnum
        do i=1,dnum
          dstd=dstd+(d1(i)-dav)**2
        end do
        dstd=SQRT(dstd/dnum)
      ELSEIF(dnum.eq.1) then
        dav=d1(1)
        dstd=0.0
      ELSE
        dav=-9.0
        dstd=-9.0
      endif

ccccccccccccccccc multiple day loop cccccccccccccccccccccccccc

        n=n-1
        WRITE(6,*)' CLRCOF2.sub Number of coeffs = ',n
        open(unit=78,file=lasfil,status='old',ERR=11)
        read(78,*)
        read(78,*,ERR=11) date,lc
        close(unit=78)
        GO TO 12

 11     date=00000000
        lc=0.0

 12     k=0
cc===== added initialize j=1 to fix UNIX initialization bug per Chris Cox 8/26/2015
        j=1
cc===========================================================================
        IF(coeff(1).lt.0.0) coeff(1)=lc
        do 15 i=1,n
         if(coeff(i).lt.0.0) then
           k=k+1
         ELSEIF(OK.and.i.eq.1) THEN
           write(2,5)cdate(i),coeff(i),ocoeff(i),c(i),d(i)
           lc=coeff(i)
           k=0
           OK=.FALSE.
         ELSEIF(coeff(i).gt.0.0.and.k.eq.0) THEN
           write(2,5)cdate(i),coeff(i),ocoeff(i),c(i),d(i)
           lc=coeff(i)
         else
           dc=(coeff(i)-lc)/(k+1)
           j=i
           do 13 l=(i-k),(i)
             IF(ok) then
               coeff(l)=lc
               ok=.false.
             endif
             coeff(l)=coeff(l-1)+dc
 13        continue
           do 14 l=(i-k),(i)
             IF(MOD(l,3).eq.0) then
               IF(av2.gt.0.0.and.coeff(l).GT.(av2+slim*std2)) then
                 IF(l.lt.i) coeff(l)= av2+slim*std2
               elseif(av2.gt.0.0.and.coeff(l).lT.(av2-slim*std2)) then
                 IF(l.lt.i) coeff(l)= av2-slim*std2
               endif
             ELSEIF(MOD(l,3).eq.1) then
               IF(av1.gt.0.0.and.coeff(l).GT.(av1+slim*std1)) then
                 IF(l.lt.i) coeff(l)= av1+slim*std1
               elseif(av1.gt.0.0.and.coeff(l).lT.(av1-slim*std1)) then
                 IF(l.lt.i) coeff(l)= av1-slim*std1
               endif
             ELSE
               IF(dav.gt.0.0.and.coeff(l).GT.(dav+slim*dstd)) then
                 IF(l.lt.i) coeff(l)= dav+slim*dstd
               elseif(dav.gt.0.0.and.coeff(l).lT.(dav-slim*dstd)) then
                 IF(l.lt.i) coeff(l)= dav-slim*dstd
               endif
             endif
             write(2,5)cdate(l),coeff(l),ocoeff(l),c(l),d(l)
 14        continue
           lc=coeff(i)
           k=0
         endif
 15     continue


        if(k.gt.0) then
          do 19 i=(n-(k-1)),n
            coeff(i)=lc
            write(2,5)cdate(i),coeff(i),ocoeff(i),c(i),d(i)
 19       continue
        endif
        close(unit=2)
        open(unit=78,file=lasfil,status='unknown')
        write(78,*)' SWFCLRIR Last day and coefficients:'
        write(78,5)cdate(j),coeff(j),ocoeff(j),c(j),d(j)
        write(78,*)cdate(j),coeff(j),ocoeff(j),c(j),d(j)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        WRITE(78,*)' '
        WRITE(78,*)'==================================================='
        WRITE(78,*)'          avg   avg+std   avg-std  fitnum'
        write(78,25)'n1',av1,av1+slim*std1,av1-slim*std1,num1
        write(78,25)'dy',dav,dav+slim*dstd,dav-slim*dstd,dnum
        write(78,25)'n2',av2,av2+slim*std2,av2-slim*std2,num2
 25      format(a2,1x,3f10.4,i8)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

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
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine calcrhfac(path,p,errlm,clim,nx,nta,typt,nrh,typr,nvp,
     %typv,nlw,rhlim,rhmin,rflim,conum,rha,rhb)
      PARAMETER (NMAX=1000000)
      PARAMETER (NDAT=140000)
      INTEGER  nta,typt,nrh,typr,nvp,typv,nlw,i,cn,p,j,cdat(ndat),k,l
      INTEGER zdat,ztim,ldat,ltim,cflg,clr,ocn,clim,nc,fdat,c,nx,dn,nl
      integer conum
      REAL*8 xarr(nmax),yarr(nmax),rha,rhb,ta,vprs,rh,lw,clw,xx
      REAL*8 errlm(55),rhlim,cof(ndat),av2,x(55),av,std,rerr
      REAL*8 rflim,avgcof,avgxx,avgta,avgvp,avglw,mxrh(nmax),avgrh,amxrh
      REAL*8 avlwl,avrhl,mxlw(nmax),amxlw,mxad(nmax),amxad,avgad
      REAL*8 mxvp(nmax),amxvp,mxTa(nmax),amxTa,rhmin
      CHARACTER*14 dirfil,infil,outfil,cof1,clrfil,out2
      CHARACTER*180 path
      LOGICAL clrout

      outfil='Clr_LWdn.asc'
      out2='ClrLWdRH.asc'
      dirfil='Clr_LWdn2.dir'
      COF1='Clr_LWdn.c1f'
      sigma=0.0000000567000
      rerr=-9999.0
      avgxx=0.0
      avgcof=0.0
      avgTa=0.0
      avgvp=0.0
      avglw=0.0
      avgad=0.0
      avgcof=0.0
      avgRH=0.0
      amxRH=0.0
      amxLW=0.0
      amxad=0.0
      avlwl=0.0
      avrhl=0.0
      l=0

      if(conum.eq.1) then
        clrout=.true.
      else
        clrout=.false.
      endif

      IF(clrout.or.conum.eq.1) then
        clrfil='rhfacclr.asc'
        OPEN(UNIT=9,FILE=clrfil,STATUS='unknown')
      WRITE(9,*)'          RH          Ta        vprs          lw      c
     %of(c)          xx     Log10RH     Log10xx'

        open(unit=19,file='alldata.asc',status='unknown')
        open(unit=10,file='rhfacNclr.asc',status='unknown')
cc        pause
      endif

      WRITE(6,*)'  In  calcrhfac  sub...'

      OPEN(UNIT=3,FILE=outfil,STATUS='unknown')
        WRITE(3,*)'  ldate   n24   dayN   ClrN   CLWn    Coef  StDev'
      OPEN(UNIT=8,FILE=out2,STATUS='unknown')
        WRITE(8,*)'  ldate   n24   dayN   ClrN   CLWn    Coef  StDev'

      OPEN(UNIT=1,FILE=dirfil,STATUS='old',ERR=99)
 5    READ(1,6,END=100) infil
 6    FORMAT(a12)

      IF(p.gt.0) then
        OPEN(UNIT=2,FILE=path(1:p)//infil,STATUS='old')
        WRITE(6,*)'RHsub 1st Opened path//infil: ',path(1:p)//infil
      else
        OPEN(UNIT=2,FILE=infil,STATUS='old')
        WRITE(6,*)'RHsub 1st Opened infil: ',infil
      endif
      READ(2,*,END=50)
      READ(2,*,END=50)
      READ(2,*,END=50)
      READ(2,*,END=50)
      cn=0
      av=0.0
      std=0.0

 10   READ(2,*,END=50,ERR=89) zdat,ztim,ldat,ltim,scv,cflg,clr,
     %(x(i),i=1,17),(x(i),i=18,17+nx)

       IF(nTa.gt.0) then
         Ta=x(17+nTa)
         IF(typT.eq.1.and.Ta.gt.errlm(nta)) then
           Ta=Ta+273.15
         ELSEIF(typT.eq.3.and.Ta.gt.errlm(nta)) then
           Ta=Ta+40.0
           Ta=(Ta*5.0)/9.0
           Ta=Ta-40.0
           Ta=Ta+273.15
         ELSEIF(Ta.le.errlm(nta)) then
           Ta=rerr
         endif
       else
         Ta=rerr
       endif
       IF(Ta.gt.400.0.or.Ta.lt.100.0) Ta=rerr

       IF(nrh.gt.0) then
         RH=x(17+nRH)
         IF(typR.eq.2.and.RH.gt.errlm(nrh)) then
           rh=rh*100.0
         ELSEIF(rh.le.errlm(nrh)) then
           rh=rerr
         endif
       else
         rh=rerr
       endif
       IF(rh.gt.115.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       ELSEIF(rh.lt.rhmin) then
         rh=rerr
       endif

       IF(nvp.gt.0) then
         vprs=x(17+nvp)
         IF(typv.eq.2.and.vprs.gt.errlm(nvp)) then
           vprs=vprs*10.0
         ELSEIF(vprs.le.errlm(nvp)) then
           vprs=rerr
         endif
       ELSEIF(Ta.gt.0.0.and.RH.ge.2.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         vprs=es*rh/100.0
       else
         vprs=rerr
       endif
       IF(vprs.gt.60.0) vprs=rerr

       IF(rh.lt.0.0.and.vprs.ge.0.0.and.ta.gt.0.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         rh=(vprs/es)*100.0
       endif
       IF(rh.gt.115.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       endif

cccccccccccc check for RH and vprs bad data  ccccccccccccccccccc
         IF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.gt.0.and.Ta.gt.0.0) then
           es=6.1173*EXP(19.83923-(5419.289/(Ta)))
           xvprs=es*rh/100.0
           IF(abs(xvprs-vprs).lt.0.1) then
             vprs=rerr
             rh=rerr
           endif
         ELSEIF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.eq.0) then
             vprs=rerr
             rh=rerr
         endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       IF(nlw.gt.0) then
         clw=x(17+nlw)
         IF(clw.le.errlm(nlw)) clw=rerr
       else
         clw=rerr
       endif
       IF(clw.gt.550.0) clw=rerr

       IF(clr.eq.1.and.rh.lt.rhlim.and.rh.ge.0.0) then
         IF(vprs.gt.0.0.and.ta.gt.0.0.and.clw.gt.0.0) then
           xx=clw/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
           av=av+xx
           avlwl=avlwl+clw
           avrhl=avrhl+rh
           cn=cn+1
           l=l+1
           IF(cn.gt.nmax) then
             cn=nmax
             GO TO 50
           endif
           xarr(cn)=xx
         endif
       endif

       GO TO 10

 50   CLOSE(UNIT=2)
      ocn=cn
      IF(cn.ge.clim) then
        av=av/cn*1.0

        do i=1,cn
          std=std+(xarr(i)-av)**2
        end do
        std=SQRT(std/cn)

        j=0
        av2=0.0
        do i=1,cn
          IF(ABS(xarr(i)-av).lt.std*2.0) then
            av2=av2+xarr(i)
            j=j+1
            yarr(j)=xarr(i)
          endif
        end do
        av2=av2/j*1.0
        std=0.0
        do i=1,j
          std=std+(av2-yarr(i))**2
        end do
        std=SQRT(std/j)

        av=0.0
        cn=0
        do i=1,j
          IF(ABS(yarr(i)-av2).lt.std*1.5) then
            av=av+yarr(i)
            cn=cn+1
            xarr(cn)=yarr(i)
          endif
        end do
        av=av/cn*1.0
        std=0.0

        do i=1,cn
          std=std+(xarr(i)-av)**2
        end do
        std=SQRT(std/cn)

      else
        av=-9.0
        std=-9.0
      endif

      WRITE(3,55) ldat,ocn,ocn,j,cn,av,std
      WRITE(8,55) ldat,ocn,ocn,j,cn,av,std
 55   FORMAT(i8,4i6,8f10.4)
      GO TO 5


 89   WRITE(6,*)'  last line read:'
      WRITE(6,*) zdat,ztim,ldat,ltim,scv,cflg,clr,
     %(x(i),i=1,17)
      pause
      GO TO 200

 99   write(6,*)'?????????????????????????????????????????????????????'
      write(6,*)'   <<<<<< Error opening file list file >>>>>>>'
      write(6,*)'The file "',dirfil,'" must be in the same directory'
      write(6,*)'as the executable file.'
      write(6,*)'?????????????????????????????????????????????????????'
      pause
      go to 200

 100  CLOSE(UNIT=1)
      CLOSE(UNIT=3)
      CLOSE(UNIT=8)

ccc      COF1='Clr_LWdn.c1f'
      call clrcof1(outfil,cof1)

      nl=l
      IF(l.ge.clim) then
        avlwl=avlwl/l*1.0
        avrhl=avrhl/l*1.0
      else
        avlwl=-9.0
        avrhl=-9.0
      endif

cccccc  end 1st iteration of coefficient fitting and interpolation

ccc    now re-read the data with the interpolated coeff,
ccc     use  measured lw to calc the RHfac coeffs.


ccc      COF1='Clr_LWdn.c1f'
      OPEN(UNIT=1,FILE=cof1,STATUS='old')
      nc=1
 103  READ(1,*,END=104) cdat(nc),cof(nc)
      nc=nc+1
      GO TO 103

 104  CLOSE(UNIT=1)
      nc=nc-1
      cn=0
      dn=0

      OPEN(UNIT=1,FILE=dirfil,STATUS='old')
 105  READ(1,6,END=180) infil
      IF(p.gt.0) then
        OPEN(UNIT=2,FILE=path(1:p)//infil,STATUS='old')
        WRITE(6,*)'RHsub 2nd Opened path//infil: ',path(1:p)//infil
      else
        OPEN(UNIT=2,FILE=infil,STATUS='old')
        WRITE(6,*)'RHsub 2nd Opened infil: ',infil
      endif
      READ(2,*,END=150)
      READ(2,*,END=150)
      READ(2,*,END=150)
      READ(2,*,END=150)
      READ(infil(1:8),*) fdat
        do c=1,nc
          IF(cdat(c).eq.fdat) GO TO 107
        end do
        WRITE(6,*)'cdat.ne.fdat #5: ',cdat(c),fdat,c
        WRITE(6,*)'cdat(1),cdat(c-1): ',cdat(1),cdat(c-1)
        pause
        GO TO 200
 107  continue


 110  READ(2,*,END=150,ERR=89) zdat,ztim,ldat,ltim,scv,cflg,clr,
     %(x(i),i=1,17),(x(i),i=18,17+nx)

       IF(nTa.gt.0) then
         Ta=x(17+nTa)
         IF(typT.eq.1.and.Ta.gt.errlm(nta)) then
           Ta=Ta+273.15
         ELSEIF(typT.eq.3.and.Ta.gt.errlm(nta)) then
           Ta=Ta+40.0
           Ta=(Ta*5.0)/9.0
           Ta=Ta-40.0
           Ta=Ta+273.15
         ELSEIF(Ta.le.errlm(nta)) then
           Ta=rerr
         endif
       else
         Ta=rerr
       endif
       IF(Ta.gt.400.0.or.Ta.lt.100.0) Ta=rerr

       IF(nrh.gt.0) then
         RH=x(17+nRH)
         IF(typR.eq.2.and.RH.gt.errlm(nrh)) then
           rh=rh*100.0
         ELSEIF(rh.le.errlm(nrh)) then
           rh=rerr
         endif
       else
         rh=rerr
       endif
       IF(rh.gt.115.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       ELSEIF(rh.lt.rhmin) then
         rh=rerr
       endif

       IF(nvp.gt.0) then
         vprs=x(17+nvp)
         IF(typv.eq.2.and.vprs.gt.errlm(nvp)) then
           vprs=vprs*10.0
         ELSEIF(vprs.le.errlm(nvp)) then
           vprs=rerr
         endif
       ELSEIF(Ta.gt.0.0.and.RH.ge.2.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         vprs=es*rh/100.0
       else
         vprs=rerr
       endif

       IF(rh.lt.0.0.and.vprs.ge.0.0.and.ta.gt.0.0) then
         es=6.1173*EXP(19.83923-(5419.289/(Ta)))
         rh=(vprs/es)*100.0
       endif
       IF(rh.gt.115.0) then
         rh=rerr
       ELSEIF(rh.gt.100.0) then
         rh=100.0
       endif

cccccccccccc check for RH and vprs bad data  ccccccccccccccccccc
         IF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.gt.0.and.Ta.gt.0.0) then
           es=6.1173*EXP(19.83923-(5419.289/(Ta)))
           xvprs=es*rh/100.0
           IF(abs(xvprs-vprs).lt.0.1) then
             vprs=rerr
             rh=rerr
           endif
         ELSEIF(rh.GT.-1.0.and.rh.lt.5.0.and.nvp.eq.0) then
             vprs=rerr
             rh=rerr
         endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       IF(nlw.gt.0) then
         lw=x(17+nlw)
         IF(lw.le.errlm(nlw)) lw=rerr
       else
         lw=rerr
       endif
       IF(lw.gt.600.0) lw=rerr

      IF(lw.GT.-1.0.and.rh.ge.rhlim.and.vprs.gt.0.0.and.ta.gt.0.0) then
        xx=lw/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
        xx=xx-cof(c)
        IF(clr.eq.1) dn=dn+1
        IF(xx.gt.0.0.and.clr.eq.1) then
          cn=cn+1
          IF(cn.gt.nmax) then
            cn=nmax
            GO TO 180
          endif
          xarr(cn)=LOG10(rh)
          yarr(cn)=LOG10(xx)
          IF(clrout.or.conum.eq.1) WRITE(9,111) RH,Ta,vprs,lw,cof(c),xx
     %,xarr(cn),yarr(cn)
 111      FORMAT(4f12.4,4f12.8)
          IF(clrout.or.conum.eq.1) WRITE(6,111) RH,Ta,vprs,lw,cof(c),xx
     %,xarr(cn),yarr(cn)
          xx=(lw+rflim)/((sigma*Ta**4)*(vprs/Ta)**(1.0/7.0))
          xx=xx-cof(c)
          avgxx=avgxx+xx
          avgcof=avgcof+cof(c)
          avgTa=avgTa+Ta
          avgRH=avgRH+RH
          avgvp=avgvp+vprs
          avglw=avglw+lw
          xx=lw-(cof(c)*(vprs/Ta)**(1.0/7.0)*(sigma*Ta**4))
          avgad=avgad+xx
          mxad(cn)=xx
          mxrh(cn)=rh
          mxlw(cn)=lw
          mxvp(cn)=vprs
          mxta(cn)=ta
        else
          write(10,111)RH,Ta,vprs,lw,cof(c),xx
     %,xarr(cn),yarr(cn)
        
        endif
      else
        write(19,111)RH,Ta,vprs,lw,cof(c),xx
     %,xarr(cn),yarr(cn)
      endif

      GO TO 110

 150  CLOSE(UNIT=2)
      GO TO 105

 180  CLOSE(UNIT=1)
      WRITE(6,*)'  rhsub, end 2nd iteration, entering medfit, cn=',cn
      c=cn
      k=cn
      avgcof=avgcof/(c*1.d0)
      avgTa=avgTa/(c*1.d0)
      avgRH=avgRH/(c*1.d0)
      avgvp=avgvp/(c*1.d0)
      avglw=avglw/(c*1.d0)
      avgad=avgad/(c*1.d0)
      avgxx=avgxx/(c*1.d0)

      call sort(cn,mxrh)
      call sort(cn,mxlw)
      call sort(cn,mxad)
      call sort(cn,mxta)
      call sort(cn,mxvp)

      j=0.1*cn
      amxrh=0.0
      amxlw=0.0
      amxad=0.0
      amxta=0.0
      amxvp=0.0
      do k=cn,cn-j,-1
        amxrh=amxrh+mxrh(k)
        amxlw=amxlw+mxlw(k)
        amxad=amxad+mxad(k)
        amxta=amxta+mxta(k)
        amxvp=amxvp+mxvp(k)
      end do
      j=j+1
      amxrh=amxrh/(j*1.d0)
      amxad=amxad/(j*1.d0)
      amxlw=amxlw/(j*1.d0)
      amxta=amxta/(j*1.d0)
      amxvp=amxvp/(j*1.d0)
      l=110-int((amxrh/avgrh)*100.0)
      IF(l.lt.2) l=2

      j=0.011*cn
      IF(j.lt.10) j=10
      rh=rhlim*0.7
      xx=0.00001
      do i=k+1,k+j
          cn=cn+1
          IF(cn.gt.nmax) then
            cn=nmax
            GO TO 190
          endif
          xarr(cn)=LOG10(rh)
          yarr(cn)=LOG10(xx)
          IF(clrout.or.conum.eq.1) WRITE(9,111) RH,avgta,avgvp,avglw,
     %avgcof,xx,xarr(cn),yarr(cn)
      end do
      k=cn
ccccccccccccccccccccccccccccccccccccccccccccccccc
      IF(rflim.gt.0.0) then
        rh=105.0
        do i=k+1,k+(j*l)
            cn=cn+1
            IF(cn.gt.nmax) then
              cn=nmax
              GO TO 190
            endif
            xarr(cn)=LOG10(rh)
            yarr(cn)=LOG10(avgxx)
          IF(clrout.or.conum.eq.1) WRITE(9,111) RH,avgta,avgvp,avglw,
     %avgcof,avgxx,xarr(cn),yarr(cn)
        end do
      endif
ccccccccccccccccccccccccccccccccccccccccccccccccc

 190  IF(cn.ge.clim) then
        call medfit(xarr,yarr,cn,rha,rhb,std)
        rha=10.d0**rha
      else
        rha=-9.0
        rhb=-9.0
        std=-9.0
      endif

      rh=rhlim*0.7
      WRITE(6,*) 'returned from medfit, rha, rhb, std:'
      WRITE(6,*) rha,rhb,std
      OPEN(UNIT=98,FILE='Clr_LWdn.rhc',STATUS='unknown')
      WRITE(98,*) 'returned from medfit, rha, rhb, std:'
      WRITE(98,198) rha,rhb,std
 198  FORMAT(4f20.14)
      WRITE(98,*) rha,rhb,std
      WRITE(98,*) 'Nxx>0, tot padded cn, tot Nclr=1:', c,cn,dn
      WRITE(98,*) '#padded,rh:',j,rh
      WRITE(98,*) ' '
      xx=(rha*100.0**rhb)*(avgvp/avgta)**(1.d0/7.d0)*sigma*avgta**4
      WRITE(98,*) '100% RHFlux at avgVP & avgTa:',xx
      xx=(rha*100.0**rhb)*(amxvp/amxta)**(1.d0/7.d0)*sigma*amxta**4
      WRITE(98,*) '100% RHFlux at amxVP & amxTa:',xx
      WRITE(98,*) ' '
      WRITE(98,*) 'For Rh<RHlim:'
      WRITE(98,*) '# lowRH data:',nl
      WRITE(98,*) 'avgLW,agvRH:',avlwl,avRHl
      WRITE(98,*) ' '
      WRITE(98,*) 'For Rh>RHlim:'
      WRITE(98,*) 'avgLW,amxLW:',avgLW,amxLW
      WRITE(98,*) 'avgRHflux,amxRHflux:',avgad,amxad
      WRITE(98,*) 'avgRH,amxRH:',avgRH,amxRH
      WRITE(98,*) 'avgVP,amxVP:',avgVP,amxvp
      WRITE(98,*) 'avgTa,amxTa:',avgTa,amxTa
      WRITE(98,*) 'avg LOG10RHflux,rflim:',avgxx,rflim
      WRITE(98,*) '# padded RH=105%:',j*l
      CLOSE(UNIT=98)
      CLOSE(UNIT=9)
      CLOSE(UNIT=19)
      CLOSE(UNIT=10)

 200  return
      end
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

