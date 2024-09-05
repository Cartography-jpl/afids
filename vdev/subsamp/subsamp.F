	include 'VICMAIN_FOR'
	subroutine main44
	implicit none
c
c	subsample sizes down images by an integer amount
c	by subsampling images at specific points much like
c	a fortran do loop or c for loop
c	
	integer*4 insamps,inlines
	parameter (insamps = 20000)		!must be reduced to 19000 in MIPL 32-bit
	parameter (inlines = 20000)		!must be reduced to 19000 in MIPL 32-bi
	integer*4 iunit,ounit,ind,icode,icnt,idef
	integer*4 i,j,nli,nsi,nlo,nso,nb,oline,osamp
	integer*4 zoom,lzoom,szoom,lz,sz,lloc,sloc
	real*4, dimension(:,:), allocatable :: oimage, iimage
	character*4 fmt(4)/'BYTE','HALF','FULL','REAL'/
	character*5 format
	character*8 org
c
	zoom=0
	call xvmessage('SUBSAMP - 07-03-2012 - (64 bit) - RJB',' ')
	call xvunit(iunit,'INP',1,ind,' ')
	call xvsignal(iunit,ind,.TRUE.)
	call xvopen(iunit,ind,'OPEN_ACT','SA','IO_ACT','SA',' ')

	call xvget(iunit,ind,'FORMAT',format,'ORG',org,' ')	

	icode = 0
	if (format.eq.'BYTE') icode=1
	if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
	if (format.eq.'FULL') icode=3
	if (format.eq.'REAL') icode=4
	if (icode.eq.0) then
		call xvmessage('??E - Unknown data format for input image',' ')
		call abend  
	endif
	call xvclose(iunit,ind,' ')

	call xvparm ('ZOOM',zoom,icnt,idef,1)
c	print *, 'zoom = ',zoom,' icnt = ',icnt,' idef = ',idef
	if (idef.eq.1) then
	    call xvparm ('LZOOM',lzoom,icnt,idef,1)
	    call xvparm ('SZOOM',szoom,icnt,idef,1)
	else
	    if (zoom.lt.-20) then
		call xvmessage('??E - Zoom cannot be less than -20',' ')
                call abend
	    endif
	    if (zoom.gt.-2) then
		call xvmessage('??E - Zoom cannot be greater than -2',' ')
		call abend
	    endif
	    lzoom=zoom
	    szoom=zoom
	endif
	call xvparm ('LLOC',lloc,icnt,idef,1)
	call xvparm ('SLOC',sloc,icnt,idef,1)
        if (lloc.gt.abs(lzoom)) then
		call xvmessage ('??E - lloc cannot be less than lzoom',' ')
		call abend
	endif
        if (sloc.gt.abs(szoom)) then
                call xvmessage ('??E - sloc cannot be less than szoom',' ')
                call abend
        endif
c	print *, 'lloc = ',lloc,' sloc = ', sloc
c	print *, 'fmt(icode) = ',fmt(icode),' ',fmt(4)
	call xvopen(iunit,ind,'OPEN_ACT','SA','IO_ACT','SA',
     &		'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')		!FMT(INCODE),' ')

	call xvget(iunit,ind,'NL',nli,'NS',nsi,'NB',nb,' ')
c	print *, 'nli = ',nli,' nsi = ',nsi
	lz=abs(lzoom)
	sz=abs(szoom)
	nlo = nli/lz
	nso = nsi/sz
c	print *,'nlo = ',nlo,' nso = ',nso
	allocate(oimage(nlo, max(nlo,nso)))
	allocate(iimage(nli, nsi))

	call xvunit(ounit,'OUT',1,ind,' ' )
	call xvopen(ounit,ind,'OP','WRITE','U_NL',nlo,'U_NS',nso,
     & 'U_NB',nb,'OPEN_ACT','SA','IO_ACT','SA','O_FORMAT',fmt(icode),
     & 'U_FORMAT',fmt(4),' ')				!,FMT(OUTCODE),' ')

c	zero buffers
c	do i = 1,inlines
c	    do j = 1,insamps
c		oimage(i,j)=0.0
c		iimage(i,j)=0.0
c	    enddo
c	enddo
        do i = 1,nli
	    call xvread (iunit,iimage(1,i),ind,' ')
	enddo
c	 print *, 'iimage (1,1-1,3) = ',iimage(1,1),iimage(1,2),iimage(1,3)

	do i=1,nlo
	    oline = lloc  + (i-1)*(lz)
	    do j=1,nso
		osamp = sloc  + (j-1)*(sz)
c		if (j.lt.5.and.i.lt.5) print *,'j = ',j,' oline,osamp = ',oline,osamp
c		if (j.lt.5.and.i.lt.5) print *,' iimage(oline,osamp) = ',iimage(oline,osamp)
		oimage(i,j) = iimage(oline,osamp)
 	    enddo
c	    print *, 'oimage(i = ',oimage(i,1),oimage(i,2),oimage(i,3),oimage(i,4),oimage(i,5)
c	1  ,oimage(i,6),oimage(i,7),oimage(i,8),oimage(i,9),oimage(i,10)
	enddo
	do i=1,nlo
		call xvwrit(ounit,oimage(1,i),ind,' ')
	enddo
c	print *, 'oimage (1,1-1,3) = ',oimage(1,1),oimage(1,2),oimage(1,3)
	call xvclose(iunit,ind,' ')
	call xvclose(ounit,ind,' ')
	return
	end
