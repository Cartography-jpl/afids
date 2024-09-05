c VICAR program MEDIAN
c
      INCLUDE 'VICMAIN_FOR'
      subroutine main44
      implicit none
      common/c1/iunit,ounit,sl,ss,mindn,maxdn
      integer*4 iunit,ounit,sl,ss,mindn,maxdn

      integer*4 nl,ns,nlin,nsin,nlw,nsw,nbuf
      integer*4 status,cnt
      integer*8 n1,n2
      character*10 fmt
      external mainmed

      call xvmessage('MEDIAN 12-Jul-2016 (64-bit) - RJB',' ')
      call xvunit(iunit,'inp',1,status,' ')
      call xvopen(iunit,status,'open_act','sa','io_act','sa',
     +            'u_format','half',' ')
      call xvsize(sl,ss,nl,ns,nlin,nsin)
      if (ns.lt.2 .or. nl.lt.2) 
     1	 call mabend('??-E median cannot handle a file with nl ' //
     +		 'or ns less than 2.')
      call xvget(iunit,status,'format',fmt,' ')
      call uprcase(fmt)
      if (fmt.eq.'BYTE') then
         mindn = 0
         maxdn = 255
      elseif (fmt.eq.'HALF' .or. fmt.eq.'WORD') then
         mindn = -32768
         maxdn = 32767
      else
         call mabend('??E- Only BYTE and HALF images allowed')
      endif

      call xvunit(ounit,'out',1,status,' ')
      call xvopen(ounit,status,'op','write','open_act','sa',
     +          'io_act','sa','u_format','half',' ')

      call xvp('nlw',nlw,cnt)
      call xvp('nsw',nsw,cnt)
      if (nlw.gt.nl) nlw=nl
      if (nsw.gt.ns) nsw=ns
      nlw = 2*(nlw/2) + 1		!force window to be odd
      nsw = 2*(nsw/2) + 1
      nbuf = ns + nsw - 1

      n1 = int8(2*nlw*nbuf)		!integer*2 img(nbuf,nlw)
      n2 = int8(2*ns)			!integer*2 out(ns)

      call stacka_big(9,mainmed,2,n1,n2, nl,ns,nlw,nsw,nbuf)

      call xvclose(iunit,status,' ')
      call xvclose(ounit,status,' ')
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine mainmed(img,n1,out,n2,nl,ns,nlw,nsw,nbuf)
      implicit none
      integer*4 nl,ns,nlw,nsw,nbuf
      integer*2 img(nbuf,nlw),out(ns)
      integer*8 n1,n2

      common/c1/iunit,ounit,sl,ss,mindn,maxdn
      integer*4 iunit,ounit,sl,ss,mindn,maxdn

      integer*4 status,cnt,i,i1,i2,j,j1,j2,line,oline
      integer*4 nlw2,nsw2,n50,dclev,minval,maxval
      real*4 perc,dctran
      logical*4 xvptst,high

      minval = mindn
      maxval = maxdn
      call xvp('PERCENT',perc,cnt)
      n50 = nsw*nlw*perc/100
      call xvp('DCTRAN',dctran,cnt)
      high = xvptst('HIGHPASS')
      call xvp('DCLEVEL',dclev,cnt)

      nlw2 = nlw/2
      nsw2 = nsw/2
      j = nlw2 + 1		!index to first line
      i1 = nsw2 + 1		!index to first and
      i2 = i1 + ns - 1		!last pixel of line

c     ...read initial lines into memory, reflecting at left and right margins
      do 20 line=1,nlw2+1
      call xvread(iunit,img(i1,j),status,'nsamps',ns,'samp',ss,
     &                'line',sl+line-1,' ')
      if (nsw.gt.1) then
         do i=1,nsw2
            img(i1-i,j)=img(i1+i,j)
            img(i2+i,j)=img(i2-i,j)
         enddo
      endif
   20 j = j + 1

      j1 = nlw2
      j2 = nlw2 + 2
c     ...reflect lines at top margin 
      do line=1,nlw2
         call mve(2,nbuf,img(1,j2),img(1,j1),1,1)
         j1 = j1 - 1
         j2 = j2 + 1
      enddo

      j = nlw2 + 1		!index to current line (middle of window)
      j1 = nlw			!index to bottom line of window
      line = nlw2 + 1		!image line number at bottom of window

      do 30 oline=1,nl
      call med2d(img,out,ns,nbuf,nlw,nsw,n50,mindn,maxdn,minval,maxval)
      if (high) call hp(img(i1,j),out,ns,dclev,dctran)
      call xvwrit(ounit,out,status,' ')
      j = mod(j,nlw) + 1
      j1 = mod(j1,nlw) + 1
      line = line + 1
      if (line.le.nl) then
         call xvread(iunit,img(i1,j1),status,'nsamps',ns,'samp',ss,
     &               'line',sl+line-1,' ')
         if (nsw.gt.1) then
            do i=1,nsw2
               img(i1-i,j1)=img(i1+i,j1)
               img(i2+i,j1)=img(i2-i,j1)
            enddo
         endif
      else		!reflect lines at bottom of image
          if (oline.eq.nl) goto 30
          if (line.eq.nl+1) then
             j2 = j1 - 2
          else
             j2 = j2 - 1
          endif
          if (j2.lt.1) j2=j2+nlw
          call mve(2,nbuf,img(1,j2),img(1,j1),1,1)
      endif
   30 continue

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c compute median filter for an image line.
c
      subroutine med2d(img,out,ns,nbuf,nlw,nsw,n50,mindn,maxdn,
     &		minval,maxval)
      implicit none
      integer*4 ns,nbuf,nlw,nsw,n50,mindn,maxdn,minval,maxval
      integer*2 img(nbuf,nlw),out(ns)

      integer*4 hist(-32768:32767)
      integer med,ltm,dn
      integer*4 i,il,is

      do i=minval,maxval
         hist(i)=0
      enddo

      minval = maxdn
      maxval = mindn
      do il=1,nlw
         do is=1,nsw
            dn = img(is,il)
            if (dn.lt.minval) then
               minval=dn
            elseif (dn.gt.maxval) then
               maxval=dn
            endif
            hist(dn)=hist(dn)+1
         enddo
      enddo

      med = minval
      ltm = 0		!number of samples less than median

      do 50 is=1,ns
      if (ltm.le.n50) goto 15
   10 med = med - 1
      ltm = ltm - hist(med)
      if (ltm.gt.n50) goto 10
      goto 20

   15 ltm = ltm + hist(med)
      med = med + 1
      if (ltm.le.n50) goto 15
      med = med - 1
      ltm = ltm - hist(med)

   20 out(is) = med
      if (is.eq.ns) goto 50

      do 30 il=1,nlw
      dn = img(is,il)
      if (dn.lt.med) ltm=ltm-1
      hist(dn) = hist(dn) - 1
      dn = img(is+nsw,il)
      if (dn.lt.minval) then
         minval=dn
      elseif (dn.gt.maxval) then
         maxval=dn
      endif
      if (dn.lt.med) ltm=ltm+1
      hist(dn) = hist(dn) + 1
   30 continue

   50 continue

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c perform high pass filtering on line
c
      subroutine hp(in,out,ns,dclev,dctran)
      implicit none
      integer*4 ns,dclev
      integer*2 in(ns),out(ns)
      real*4 dctran

      integer i,a

      do i=1,ns				!halfword input & output
         a = in(i) - out(i) + dclev	
         if (dctran.ne.0.0) a=a+dctran*float(out(i))
        out(i) = a
      enddo
      return
      end
