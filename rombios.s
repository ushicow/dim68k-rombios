************************************************************
*
*	Micro Craft basic rom os system for the Dimension
*	Computer written by Mike Carpenter
*
*  Revision         Reason                    Date       Time
*     C     Initial Production Release      11/7/83      15:45
*     D        "        "         "         11/23/83     08:42 
*     E   Graphics, disk setup, config word 12/16/83     13:10
*************************************************************
	.org	$0
	.globl	rombios
istack:	.dc.l	$1fffc		; initial stack location
istart:	.dc.l	start		; initial reset start vector
	.dc.l	buserr+romstrt	; buss error
	.dc.l	adderr+romstrt	; address error (odd word access)
	.dc.l	illins+romstrt	; illegal instruction
	.dc.l	divchk+romstrt	; divide check (x/0)
	.dc.l	chkovr+romstrt	; check out of bounds
	.dc.l	trapvt+romstrt	; trap vector
	.dc.l	privlg+romstrt	; privilege instruction trap
	.dc.l	trace+romstrt	; trace interrupt vector

*****************************************************************

        .dc.b   'Copyright 1983 by Micro Craft Corporation'
   
****************************************************************
        .even

icrtmsb:=	$008005		; initial crt msb address
icrtmod:=	$008009		; initial crt mode
icrtdat:=	$008003		; initial data pointer
icrtreg:=	$008001		; initial crt register
crtmsb:	=	$ff8005		; final crt msb address
crtmod:	=	$ff8009		; final crt mode
crtreg:	=	$ff8001		; crt reg pointer address
crtdata:=	$ff8003		; crt data pointer
duartimr: =     $ffc40b         ; interrupt mask register
memswt:	=	$00dc00		; memory switch address
ramstrt:=	$10000		; effective ram start
linl:	=	80		; number of characters per line
numr:	=	25		; number of rows per screen
romstrt:=	$ff0000		; start of relocated rom
cr:	=	$0d		;Carriage Return
lf:	=	$0a		;Line Feed
*
	.org	$100
****************************************************************
*	rom bios entered with bios pointer in d0
nfuncs	=	31
rombios:
	cmpi	#nfuncs,d0
	bge	nogood
	lsl	#2,d0		; set up address
        move.l  #rombase+romstrt,a2
        move.l  0(a2,d0),a2
	jsr	(a2)
nogood:	
	rts
*	variables and constants
memptr:	.ds.l	1		; memory address pointer
mnm:	.ds.w	1		; mnemonic code
val:	.ds.l	1		; value
pres:	.ds.l	1		; present char position
cstrg:	.ds.b	12		; char string temp buffer
sln:	.ds.w	1		; number of input chars
slnm:	.ds.w	1		; max number of input chars
temp:   .dc.l   0               temp d7 value
        .org    $19c
physend: .dc.l  0               ; end of physical memory
resetv: .dc.l   mstart+romstrt
resrved: .dc.l  0
cmode:	.dc.b	$a0		; crt mode
cmsba:	.dc.b	0		; crt msb address
crttab:	.dc.b	$6f		; crt setup parm's (r0) max scan line length
	.dc.b	linl		; (r1) # of chars/line
	.dc.b	$5a		; (r2) start of h sync
	.dc.b	$06		; (r3) h sync width
	.dc.b	$1e		; (r4) v total
	.dc.b	$00		; (r5) v total adjust
	.dc.b	numr		; (r6) # of rows
	.dc.b	$1b		; (r7) v sync pos
	.dc.b	$00		; (r8) interlace mode
	.dc.b	$08		; (r9) char scan size
	.dc.b	$00		; (r10) graph-text start scan
	.dc.b	$01		; (r11) graph-text end scan
	.dc.w	crtbuf		; (r12-r13) buffer start
	.dc.w	crtbuf		; (r14-r15) graph-text start
	.dc.w	$07d0		; (r16-r17) light pen reg
	.dc.l	$50		; maxlin in longword
	.dc.l	$0000		; line length longword
	.dc.l	$0000		; present char location
	.dc.l	crtbuf		; crt buffer address longword
*	disk parms (mfm 512 b/s-10 sectors)
rtable:	.dc.b	$11		; select drive 0 (cpm unit a:)\
	.dc.b	$c2		; step rate + head unload
	.dc.b	$03		; head load + non dma
	.dc.b	$46		; cmd msb's
	.dc.b	$00		; cmd lsb's
	.dc.b	$00		; cylinder
	.dc.b	$00		; head
	.dc.b	$00		; sector
	.dc.b	$02		; 512 bytes/sector
	.dc.b	$0a		; max sector cnt
	.dc.b	$0a		; gap length
	.dc.b	$ff		; data length
	.dc.l	ramstrt		; buffer start
	.dc.b	$00		; results 1
	.dc.b	$00		; results 2
	.dc.b	$00		; results 3
	.dc.b	$00		; results 4
	.dc.b	$00		; results 5
	.dc.b	$00		; results 6
	.dc.b	$00		; results 7
	.dc.b	$00		; disk on flag
	.dc.b	$00		; steps/track factor s=2^n
	.dc.b	$28		; number of tracks
config: .dc.l   $3fffffff       ; system configuration word:
*        bit 31 = 1 is printer revision A
*        bit 31 = 0 is printer revision B
*        bit 30 = 1 is 128 character set
*        bit 30 = 0 is 256 character set
*        reserved for future expansion
scrnpt: .dc.l    cmode           text screen table pointer
keyptr: .dc.l   keytab+romstrt          keyboard table
grfchr: .dc.l   cgtabl+romstrt          graphics character table
grfchr2: .dc.l  0               graphics char set 2nd set
prescrn: .dc.l  cmode           present screen table vector
imask:  .dc.b   00              interrupt mask for 2861
auxctr:	.dc.b	$f0		
crtbuf:	.ds.b	2		; crt buffer
**********************************************************
*	interrupt routines in rom only
*	in equivalent crt buffer space
buserr:	
	move.l	#msg1+romstrt,a0	; get message
	bra.s	msgout2
adderr:	
	move.l	#msg2+romstrt,a0
	bra.s	msgout2
illins:	
	move.l	#msg3+romstrt,a0
	bra.s	msgout2
divchk:	
	move.l	#msg4+romstrt,a0
	bra.s	msgout2
chkovr:	
	move.l	#msg5+romstrt,a0
	bra.s	msgout2
trapvt:	
	move.l	#msg6+romstrt,a0
	bra.s	msgout2
privlg:	
	move.l	#msg7+romstrt,a0
	bra.s	msgout2
trace:	
	move.l	#msg8+romstrt,a0
	bra.s	msgout2
msgout2:
	bsr.s	msgout
	bra	error
msgout:	
	move.l	a0,memptr
	move	#$7,d7		; set count
msgout1:
	move.l	memptr,a1
	move.b	(a1)+,d0
	move.l	a1,memptr
	movea.l	#cmode,a0
	bsr	crtout		; output message
	tst	d7
	dbeq	d7,msgout1	; back for next char
	rts
msg1:	.dc.l	$42555353	'BUSS'
	.dc.l	$20455252	' ERR'
msg2:	.dc.l	$41444452	'ADDR'
	.dc.l	$20455252	' ERR'
msg3:	.dc.l	$494c4c20	'ILL '
	.dc.l	$494e5354	'INST'
msg4:	.dc.l	$4449562f	'DIV/'
	.dc.l	$30202020	'0   '
msg5:	.dc.l	$424f554e	'BOUN'
	.dc.l	$5320434b	'D CK'
msg6:	.dc.l	$54524150	'TRAP'
	.dc.l	$20202020	'    '
msg7:	.dc.l	$50524956	'PRIV'
	.dc.l	$4c454745	'LEGE'
msg8:	.dc.l	$54524143	'TRAC'
	.dc.l	$45202020	'E   '
*****************************************************
*       function jump table
rombase: .dc.l    nogood+romstrt   unused
         .dc.l    initcrt+romstrt  initalize crt
         .dc.l    crtous+romstrt   output char to screen
         .dc.l    initsc+romstrt   clear text screen
         .dc.l    rdkey+romstrt    read a key
         .dc.l    stkey+romstrt    get keyboard status
         .dc.l    boot+romstrt     boot disk from drive a:
         .dc.l    main+romstrt     start of monitor
         .dc.l    dvstat+romstrt   disk drive status
         .dc.l    dsmo+romstrt     disk motor on
         .dc.l    doff+romstrt     disk motor off
         .dc.l    dseek+romstrt    disk seek
         .dc.l    dinit+romstrt    disk rezero
         .dc.l    readd+romstrt    seek to and read a sector
         .dc.l    writed+romstrt   seek to and write a sector
         .dc.l    formatd+romstrt  seek to and format a track
         .dc.l    format+romstrt   format a disk
         .dc.l    dredid+romstrt   read id
         .dc.l    dstat+romstrt    disk status
         .dc.l    initmod+romstrt  initialize modem
         .dc.l    initrtc+romstrt  initialize real time clock
         .dc.l    prtsts+romstrt   printer status
         .dc.l    print+romstrt    output char to printer
         .dc.l    cursor+romstrt   position cursor
         .dc.l    readcur+romstrt  read present cursor position
         .dc.l    initgrf+romstrt  clear graphics screen
         .dc.l    setcolr+romstrt  set color nibble
         .dc.l    plotp+romstrt    plot point at x,y
         .dc.l    plotl+romstrt    plot line from x,y to x,y
         .dc.l    readgrf+romstrt  read color at x,y
         .dc.l    coutdir+romstrt  output character to crt (no lf,cr,etc)
*****************************************************
*	setup routine
*	executed in rom
start:	
	move	#$fffe,d0
startt:
	nop
	nop
	dbra	d0,startt
*
	move.b	cmsba,icrtmsb	; set msb address
	move.b	cmode,icrtmod
*
	movea.l	#crttab,a0
	movea.l	a0,a1
	clr.b	d0
start1:
	move.b	d0,icrtreg
	move.b	(a1)+,d1
	move.b	d1,icrtdat
	addq.b	#1,d0
	cmp.b	#16,d0
	bne	start1
*
	movea.l	#movins,a0
	movea.l	#ramstrt,a1	; move changeover instructions
	move.l	(a0)+,(a1)+	; 3 long words
	move.l	(a0)+,(a1)+
	move.l	(a0)+,(a1)+
	jmp	ramstrt
*	this is in high ram
*****************************************************
movins:	
	move.b	d0,memswt
	jmp	romstrt+initram
*****************************************************
*	now back to rom in ff0000 address space
initram:
	movea.l	#romstrt,a0	; move rom to ram
	movea.l	#0,a1
	move.w	#$800,d0	; set counter
mvlp1:	
	move.l	(a0)+,(a1)+
	tst	d0		; test for end
	dbeq	d0,mvlp1	; loop until complete
*************************************************
*
*	find amount of memory
*
*************************************************
*
siz_mem:
	clr.l	a0		start with address $0
	move.l	#$12345678,d0	data to store
siz_1:
	add.l	#$20000,a0	increment by 20000
	move.l	a0,physend	save top of memory
	cmp.l	#$1000000,a0	is memory maximum ?
	beq	siz_2		continue, max mem found
	move.l	d0,(a0)
	nop			wait for buss to settle
	move.l	(a0),d1		get data
	cmp.l	d0,d1
	beq.b	siz_1		ram there ?
siz_2:
	sub.l	#2,physend	correct length
	movea.l	#cmode,a0	; setup init
	bsr	initcrt		; initialize crt
	move.l	#mstart+romstrt,istart	;setup restart vector
	bsr	pinit		; initialize periphirals
	bsr	initsc          clear screen
	move.l	#header+romstrt,a1
	bsr	msg		;display banner
	move.w	#$8ffe,d7	; key board time out loop
strt1:	bsr	stkey
	tst.b	d0
	bne	strt2
	bsr	rdkey		; wait for key board to wake up
	bra	strt3
strt2:	dbra	d7,strt1
strt3:	bra	boot		;Auto Boot Drive A:
***************************************************
*	main monitor start
mstart:	
        move.l  #cmode,a0       reset crt
        bsr     initcrt
	bsr	pinit		;initialize periphirals
	bsr	initsc		; initialize screen
	move.l	#header+romstrt,a1
	bsr	msg
main:	
	move.b	#$3e,d0		; output a prompt
	bsr	crtout
	bsr	getmnm		; get a mnemonic
	move.b	d0,mnm		; save it
	subq.b	#1,d0		; branch to routine
	beq	setmem		; (sm)
	subq.b	#1,d0		; (dm)
	subq.b	#1,d0		; (mm)
	subq.b	#1,d0		; (d@)
	subq.b	#1,d0		; (gt)
	beq	gotom
	sub.b	#1,d0		; (tr)
	sub.b	#1,d0		; (tp)
	subq	#1,d0		; (bt)
	beq	boot
*****************************************************
*	support routines
error:	
	move.b	#$3f,d0		; error routine
	bsr	crtout		; output '?'
	move.b	#$0d,d0
	bsr	crtout		; output carriage return
	bra	main		; and go back
***************************************************
*	Periphiral initializition sequence
pinit:	move.b	#$29,$ffc405	; reset uart
        move.b  #$00,duartimr   ; mask all interrupts
        clr.b   imask
	move.b	#$19,$ffc405	; initialize kbd
	move.b	#$4e,$ffc403	; select clock rate
	move.b	#$f0,$ffc409	; setup aux contro;
	move.b	#$13,$ffc401	; write 1st mode byte
	move.b	#$07,$ffc401	; write 2nd mode byte
	move.b	#$00,$ffc41b	; write printer control
	rts
*****************************************************
*	Display ASCII string pointed to by a0 untill '$' is found
msg:	*Address reg a0 and Data reg d1 are destoryed
	move.b	(a1)+,d0
	ext	d0
	cmp.b	#'$',d0		;test for '$' character
	beq	msge
	move.l	#cmode,a0
	bsr	crtout		;output character to screen
	bra	msg
msge:	rts
header:	.dc.b	'                            Welcome to the Realm',cr
	.dc.b	'                                     of',cr
	.dc.b	'                            Dimension Computing',cr
	.dc.b	'                                     by',cr
	.dc.b	'                           Micro Craft Corporation',cr,cr,'$'
	.even
*****************************************************
*   direct output char to screen with no control chars
coutdir:
        move.l  prescrn,a0      get present screen pointer
        move.l  scrnpt,a5       and text screen pointer
        move.l  28(a5),a6       get char position
        move.b  d1,(a6)         store char
        eori.b  #$80,(a6)       flip cursor bit
        tst.b   (a0)            see if graphics
        bmi     croutrt         no, done
        bsr     grafchr         yes, output graphics char
croutrt:
        rts                     and return
*****************************************************
*	crt output routine
crtous:	
	move.b	d1,d0		; move byte for rom bios
crtout:	
        move.l  d7,temp          save d7
        move.l  prescrn,a0
        move.l  a0,a5           save screen pointer
        tst.b   (a0)            see if graphics
        bmi.s   crtout1         no
        move.l  scrnpt,a5       yes, set up text pointers
crtout1: movea.l	28(a5),a6	; crt output routine input char in d0
        move.l  32(a5),d7       test limits
        cmp.l   d7,a6           see if less than start
        bmi     crtstl          yes
        add.l   20(a5),d7       add length
        cmp.l   d7,a6           or gt. max
        bpl     crtsth          yes
crt01:	
        cmp.b   #$0d,d0         see if carrage return
        beq.s   crtcr           yes
        cmp.b   #$08,d0         see if backspace
        beq.s   crtbs           yes
        tst.b   (a0)            see if graphics
        bmi.s   crt03           no
        bsr     grafchr         yes, output character
crt03:
	move.b	d0,(a6)+	; store character
crt02:	
        cmp.l   d7,a6           test for scroll
	bpl	crtmv		; yes
	eor.b	#$80,(a6)	; and inverse next char
        bsr     grafput         output graf char if required
crtend:
	move.l	a6,28(a5)	; save char position
        move.l   temp,d7        restore d7
	rts			; and return
crtsth:
        sub.l   #1,d7           out of range high, set max
crtstl:
        move.l  d7,a6           out of range (low) set to limit
        bra     crt01           and output it
crtcr:	
	eor.b	#$80,(a6)	; clear cursor
        bsr     grafput         clear cursor in graphics if required
	move.l	a6,d2		; calculate amount to move
	sub.l	32(a5),d2	; -buffer address
	divs	26(a5),d2
	addq.w	#1,d2
	muls	26(a5),d2
	movea.l	d2,a6
	adda.l	32(a5),a6	; +buffer address
	bra	crt02
crtbs:	
        cmp.l   32(a5),a6       see if start of buffer
        bls     grafrtn         yes, ignore
	eor.b	#$80,(a6)	; turn off inverse flag
        bsr     grafput
	eor.b	#$80,-(a6)	; and set inverse flag
        bsr     grafput
	move.l	a6,28(a5)	; reset pointer
        move.l  temp,d7         restore d7
	rts			; and return
crtmv:	
        tst.b   (a0)            test for graphics
        bmi.s   crtmv2          no
        bsr     gfscrol         yes,scroll graphics
crtmv2:
	movea.l	32(a5),a6	; set to roll screen
	movea.l	24(a5),a4
	adda.l	a6,a4
	move.w	22(a5),d1
	sub.w	26(a5),d1
	lsr	#2,d1
	sub.w	#1,d1
crtmvl:	
	move.l	(a4)+,(a6)+	; move data
	dbra	d1,crtmvl	; back if not complete
	move.w	26(a5),d1	; fill last line with blanks
	lsr.w	#2,d1
	sub.w	#1,d1
crtmvb:	
	move.l	#$20202020,(a6)+
	dbra	d1,crtmvb	; loop back if not finished
	move.l	20(a5),d1	; reset char pointer
	sub.l	24(a5),d1
	add.l	32(a5),d1
	move.l	d1,a6
	eor.b	#$80,(a6)	;Turn on Cursor
        bsr     grafput
	bra	crtend
grafput:
        tst.b   (a0)            see if graphics
        bmi     grafrtn         no
        cmp     16(a5),a6       or in mix range
        bmi     grafrtn         no
        move.b  (a6),d0         yes, so put it out
        bsr     grafchr
grafrtn:
        rts                     return
********************************************************
*       graphic character routine
* places graphic char pointed to by contents of a6 from table grfchr
* onto graphics screen regs d0,d6,d7,a0,a4,a5,a6 are preserved
grafchr:
        move.l  32(a5),d1      get buffer start
        move.l  a6,d2          and char location
        sub.l   d1,d2          got rel char location
        move.b  3(a5),d1       get bytes/line
        ext     d1
        divu    d1,d2          ms word = remainder, ls word = line count
	move	d2,d1		;copy line count
        move.b  11(a5),d4      = scans/char box(spc)
        add.b   #1,d4          real
        ext     d4
        mulu    d1,d4          = bpl * lc * spc
        clr.l   d1
        move.b  11(a0),d1      = scans/group(spg)
        add     #1,d1          real
        divu    d1,d4          = bpl * lc * spc / spg
	swap	d4		;move scan line offset to lsw
	move	d4,d3		;store offset value
	swap	d4		;restore register
       move.b  3(a5),d1       modulo to find start of nearest line
        ext     d1
        mulu    d1,d4
        swap    d2             = bytes to start of line
        add     d2,d4          + column count = position in graf array
        clr     d1             set up for inverse chars
        move.l grfchr,a1
        move.b  d0,d2          save char
        tst.b   d2             see if inverse
        bpl     grcmv2         no
        tst.l   grfchr2
        bne     grcmv3
        not.b   d1             yes, set eor flag
grcmv2:
        and.l   #$7f,d2        strip msb (inverse flag)
grcmv6: lsl     #3,d2          make table offset
        add.l   d2,a1      set char head
        move.l  32(a0),a2      get buffer start
        add.l   d4,a2          + char start
        move.l  a2,a3          and save
	move	d3,d5		;copy scan line value
	mulu	#$4000,d5	;get offset scan line address
	adda.l	d5,a2		;add offset to start of scan address
        move.b  3(a0),d5       get line length
        ext     d5
        ext.l   d5             long
        clr     d2             get scan group cnt -1
        move.b  11(a0),d2
        move    d2,d4          save count
	sub	d3,d2		;- scan line offset
        move    #7,d3          set char box scans
grcmv1: 
        move.b  (a1)+,(a2)     move scan byte of char data
        eor.b   d1,(a2)        inverse if required
        dbra    d3,grcmv4       see if all scans done
        bra.s   grcmv9         yes
grcmv4: 
        add.l   #$4000,a2      no, so bump scan address
        dbra    d2,grcmv1      and see if scan group done
        add.l   d5,a3          for next scan group
        move.l  a3,a2
        move    d4,d2          reset counter
        bra     grcmv1         and do again
grcmv9:
	clr	d3
	move.b	11(a5),d3	;load scans per char
	sub.b	#8,d3		;subtract bytes per graphic character
	bmi	grcmv5		;if graphic = or greater return
grcmv7:
	add.l	#$4000,a2	;move ptr to next scan line
	dbra	d2,grcmv8	;if all scans per group not done continue
	add.l	d5,a3		;add offset to next line
	move.l	a3,a2		;copy value
	move	d4,d2		;load scans per group
grcmv8:
	move.b	#0,(a2)	;load a space into unused scan line
	eor.b	d1,(a2)		;inverse if required
	dbra	d3,grcmv7	;if all scan lines not blanked continue
grcmv5:
        rts
grcmv3:move.l  grfchr2,a1
       and.l   #$ff,d2
       bra     grcmv6
********************************************************
*       graphics scroll
* if scroll requested and mixed mode point (text screen table value
* in 16(a5) is less than a6 then scroll text onto graphics screen from
* bottom of screen to mixed mode point
gfscrol:
        cmp     16(a5),a6      see if we need to scroll
        bmi.s   grcmv5         no, ignore
        clr.l   d1             yes, get mixed mode point
        move.b  1(a5),d1
        swap    d1
        move    16(a5),d1
        add.l   #1,d1          start of text area
        move.l  20(a5),d7      set screen max
        add.l   32(a5),d7
        cmp.l   d7,d1          see if scroll possible
        bpl     grcmv5         no
        move.l  d7,a6          put into char pointer
	suba.l	24(a5),a6	;place ptr at front of last line

* calculate graphic pointer values
	move.l	a6,d0		;load address of last line
	sub.l	32(a5),d0	;determine offset from top
	bsr	trnslat		;translate to address in graphic buffer
	move.l	d0,d7		;save halt address
	move	16(a5),d0	;load graphic switch address
	sub	14(a5),d0	;determine offset from top
	ext.l	d0		;extend to longword value
	move.l	d0,a3		;save offset value
	bsr	trnslat		;translate to address in graphic buffer
	move.l	d0,a1		;store scroll address
	move	d2,d1		;save starting count value
	add.l	24(a5),a3	;move ptr to next line
	move.l	a3,d0		;copy offset value
	bsr	trnslat		;translate to address in graphic buffer
	move.l	d0,a2		;store move address
	move.l	#10,d3		;***** test

* scroll data up
grfscr1:
	cmp.l	a1,d7		;all data moved?
	beq	grfscr6		;yes, clear last line
	move.l	24(a5),d0	;load number of bytes per line
	lsr	#2,d0		;divide by 4 for longword move
	sub	#1,d0		;-1 for counter
grfscr2:
	move.l	(a2)+,(a1)+	;move data up to scroll point
	dbra	d0,grfscr2	;if line not done, continue
	dbra	d1,grfscr3	;if all scan lines in group not done go to next
	move.b	11(a0),d1	;load scan lines per group
	ext	d1		;extend to word length
	mulu	#$4000,d1	;get scan line offset
	sub.l	d1,a1		;move ptr back to start + 1 line
	move.b	11(a0),d1	;load number of scans per group
	ext	d1		;extend to word
	bra	grfscr4		;check other pointer
grfscr3:
	sub.l	24(a5),a1	;move pointer back to beginning of line
	add.l	#$4000,a1	;move pointer to next scan line
grfscr4:
	dbra	d2,grfscr5	;if all scan lines not done go to next
	move.b	11(a0),d2	;load scans per group
	ext	d2		;extend to word
	mulu	#$4000,d2	;get scan line offset
	sub.l	d2,a2		;move ptr back to start + 1 line
	move.b	11(a0),d2	;load scans per group
	ext	d2		;extend to word
	bra	grfscr1		;move remaining lines
grfscr5:
	sub.l	24(a5),a2	;move ptr back to start of line
	add.l	#$4000,a2	;move prt to next scan line
	bra	grfscr1		;move remaining lines

* clear last line before returning
grfscr6:
        move.l  24(a5),d7      get line length
        sub     #1,d7          -1 = number of spaces on bottom line
        move.b  #$20,d0        set to blank
grfscr7:
        bsr     grafchr        output blank
        add.l   #1,a6          bump pointer
        dbra    d7,grfscr7     and do it
        rts

***********************************************************
* This subroutine translates a position in the crt buffer *
* to a position in the graphics buffer.			  *
* Inputs: d0 - offset of location into CRT buffer	  *
*	  a5 - address of CRT table			  *
*	  a0 - address of graphics table		  *
*         d3 - scratch registers			  *
* Outputs: d0 - address of offset in graphic buffer       *
*	   d2 - starting scan line offset count		  *
***********************************************************
trnslat:
	clr.l	d3
	divu	26(a5),d0	;get number of lines from top (text)
	move.b	11(a5),d3	;load scan lines per char
	add	#1,d3		;get actual
	mulu	d3,d0		;get number of scan lines from top
	move.b	11(a0),d3	;load scan lines per group
	add	#1,d3		;get actual
	divu	d3,d0		;get number of lines from top (graphic)
	swap	d0		;move remaining lines to lsw
	move	d0,d3		;save remaining scan lines
	swap	d0		;restore register
	mulu	26(a5),d0	;get number of bytes from top
	add.l	32(a0),d0	;add buffer start to get actual address
	move	d3,d2		;copy scan line offset 
	mulu	#$4000,d2	;calculate offset bytes
	add.l	d2,d0		;add offset to scan address
	clr	d2
	move.b	11(a0),d2	;load scan lines per group
	sub.b	d3,d2		;calculate starting count value
	rts

********************************************************
*	keyboard input routine
rdkey:	bsr	stkey		; get status
	tst.b	d0
	bne	rdkey		; loop back if not ready
        clr.l   d0
	move.b	$ffc407,d0	; get data
        move.l  keyptr,a1       get translation table
        move.b  0(a1,d0),d0     ; translate the character code
	move.b	d0,d7		; save data
	rts			; and return
*******************************************************
*	keyboard status routines
stkey:
	clr	d0		; clear flag
	move.b	$ffc403,d1	; get status
	and.b	#1,d1		; trim byte
	bne.s	stkey1		; jump if ready
	move	#$ff,d0		; set busy flag
stkey1:
	rts			; and return
******************************************************
*	get string
getstr:
	clr.w	sln		; clear character counter
	move.w	d0,slnm		; save max count
getst2:
	bsr	rdkey		; get a char
	bsr	crtout		; output it
        move.b  d0,d7           save char
	cmp.b	#$61,d7		; fix if lower case
	bmi.s	getst3
	cmp.b	#$7b,d7
	bpl.s	getst3
	sub.b	#$20,d7
getst3:
	cmp.b	#$08,d7		; see if backspace
	beq.s	getbs		; yes
	cmp.b	#$0d,d7		; see if carriage return
	bne.s	getst1		; no
	bra.s	getsend		; yes, finished
getst1:
	cmp.b	#$20,d7		; see if blank
	beq.s	getsend		; yes, finished
	movea.w	sln,a6		; get char count
	adda.w	#cstrg,a6	; add base
	move.b	d7,(a6)		; save it
	addq.w	#1,sln		; increment cnt
	move.w	slnm,d0		; see if max found
	cmp	sln,d0
	beq.s	getsend		; yes, exit
	bra	getst2		; and repeat
getbs:
	subq.w	#1,sln		; backspace, decrement count
	bmi.s	getbs1		; see if empty
	bra	getst2		; no, get next
getbs1:
	clr.w	sln		; yes, clear
	bra	getst2		; and get next
getsend:
	rts			; finished
********************************************************
*	get a number
getnum:
	move.w	#8,d0		; set max count
	bsr	getstr		; get string
	clr.l	d1		; clear temp regs
	clr.l	d2
	cmp.b	#$0d,d7		; see if carriage return
	beq.s	gerend2		; yes, exit
	move.w	sln,d0		; see if empty
	beq.s	gerend
	movea.w	#cstrg,a0	; setup to string
getnm2:
	move.b	(a0)+,d1	; get char
	sub.b	#$30,d1		; convert to number
	bmi.s	gerend2		; exit if error
	cmp.b	#10,d1		; see if less than 9
	bmi.s	getnm1		; ok, digit
	sub.b	#7,d1		; get to 'A'
	bmi.s	gerend2		; no, must be error
getnm1:
	cmp.b	#16,d1		; see if too big
	bpl.s	gerend2		; yes, error
	lsl.l	#4,d2		; d2 = d2 * 16
	add.l	d1,d2		; d2 = d2 + d1
	subq.w	#1,d0		; decrement cnt
	bne	getnm2		; and loop
	move.l	d2,d0		; replace d0
	moveq.l	#1,d1		; set d1 positive
	rts			; and exit - data avail
gerend:
	clr.l	d1		; exit - empty
	rts
gerend2:
	move.l	#-1,d1		; exit - error
	rts
**********************************************************
*	convert bin to hex and output
putnum:
	move	#3,d7		; set shift count
putnuml:
	move.l	d0,d6		; svae number
	move	#7,d5		; rotate to position data
punt4:
	cmp	d5,d7
	beq.s	punt1
	rol.l	#4,d6
	subq	#1,d5
	bra	punt4
punt1:
	rol.l	#4,d6		; get ms nibble
	move	d6,d0
	and	#$f,d0
	cmp	#10,d0		; see if greater than digit
	bpl.s	punt2		; yes, must be letter
	add.b	#$30,d0		; get to 'A'
punt3:
	bsr	crtout		; and output it
	tst	d7
	dbeq	d7,punt1	; go again
	rts			; complete, exit
punt2:
	add.b	#$37,d0		; make letter
	bra	punt3
*******************************************************
*	get nmemonic
getmnm:
	move	#2,d0		; set to get 2 characters
	bsr	getstr		; get input string
	cmp.w	#2,sln		; see if right number
	bne	error		; if not 2 then error
	cmp.w	#$534d,cstrg	; compare to 'sm'
	beq.s	getm1		; yes
	cmp.w	#$444d,cstrg	; 'dm'
	beq.s	getm2
	cmp.w	#$4d4d,cstrg	; 'mm'
	beq.s	getm3
	cmp.w	#$4440,cstrg	; 'd@'
	beq.s	getm4
	cmp.w	#$4754,cstrg	; 'gt'
	beq.s	getm5
	cmp.w	#$5452,cstrg	; 'tr'
	beq.s	getm6
	cmp.w	#$5450,cstrg	; 'tp'
	beq.s	getm7
	cmp.w	#$4254,cstrg	; 'bt'
	beq.s	getm8
	bra	poperr		; none of the above - error
getm1:
	moveq	#1,d0		; set d0 to flag
	rts
getm2:
	moveq	#2,d0
	rts
getm3:
	moveq	#3,d0
	rts
getm4:
	moveq	#4,d0
	rts
getm5:
	moveq	#5,d0
	rts
getm6:
	moveq	#6,d0
	rts
getm7:
	moveq	#7,d0
	rts
getm8:
	moveq	#8,d0
	rts
poperr:
	addq.l	#4,a7		; fix stack
	bra	error
******************************************************
*	set memory
setmem:
	bsr	getnum		; get address
	tst	d1		; see results
	ble	error		; not valid, go back
	lsr.l	#1,d0		; force to even address
	lsl.l	#1,d0
	move.l	d0,memptr	; save address
setme2:
	move.b	#$0d,d0		; put out carriage return
	bsr	crtout
	move.l	memptr,d0	; output address
	move	#5,d7
	bsr	putnuml
	move.b	#$3d,d0		; output '='
	bsr	crtout
	move.l	memptr,a1	; get contents
	move.w	(a1),d0
	bsr	putnum		; and print
	move.b	#$20,d0		; output a blank
	bsr	crtout
	bsr	getnum		; get new value
	tst	d1		; see if store
	bmi	main		; go back if complete
	beq	setme1		; if 0 look at next
	move.l	memptr,a1	; otherwise replace contents
	move.w	d0,(a1)
setme1:
	add.l	#2,memptr	; bump pointer
	bra	setme2		; and try again
*******************************************************
*	go to memory location
gotom:
	bsr	getnum		; get location
	tst	d1		; see if good
	ble	main		; no, go back
	lsr.l	#1,d0		; force even address
	lsl.l	#1,d0
	move.l	d0,a0		; save location
	jmp	(a0)		; and jump to it
*******************************************************
*	initialize screen
initsc:
        move.l  prescrn,a0      get screen parms
        tst.b   (a0)            see if graphics
        bmi     init2           no, text
        move.l  scrnpt,a0       yes, get text screen and clear
init2:
	move.l	20(a0),d0	; setup max count
	lsr.l	#2,d0
	movea.l	32(a0),a1	; set buffer address
init1:
	move.l	#$20202020,(a1)+	; clear screen
	tst	d0
	dbeq	d0,init1	; branch back
	move.l	32(a0),a1	;home cursor
	move.l	32(a0),28(a0)
	eor.b	#$80,(a1)	;turn-on cursor
	rts
*********************************************************
initcrt:
	movea.l	a0,a1
        tst.b   (a0)            see if graphics
        bpl.s   initcrt1        yes
        move.l  a0,scrnpt       no, save text pointer
initcrt1:
        move.l  a0,prescrn      save present screen
        add.l   #1,a1           bump pointer
	move.b	(a1)+,crtmsb	; setup crt address
	clr.b	d0		; set pointer
intlp1:
	move.b	d0,crtreg	; write crt register number
	move.b	(a1)+,d1	; get table value
	move.b	d1,crtdata	; and store it
	addq.b	#1,d0		; increment pointer
	cmp.b	#16,d0		; see if done
	bne	intlp1		; back if not
        move.b  (a0),crtmod     turn on crt
	clr.w	d0		; setup crt max buffer size
	move.b	3(a0),d0
	clr.w	d1
	move.b	8(a0),d1
	muls	d0,d1		; max=linelength * num lines
	cmp.b	#3,10(a0)	; see if interlace
	bne	intlp2
	lsl.l	#1,d1		; yes, x 2
intlp2:
	move.l	d1,20(a0)	; save as maxlen
	clr.l	d0		; setup crtbuf addr longword
	move.b	1(a0),d0	; get msb's
	lsl.l	#8,d0		; move em over
	lsl.l	#8,d0
	add.w	14(a0),d0	; add lsb's
	move.l	d0,32(a0)	; and save as crt buf start
	clr.l	d1		; setup linelength as longword
	move.b	3(a0),d1
	move.l	d1,24(a0)	; and save as linlw
        tst.b   (a0)            see if graphics
        bmi     initcrt2        no
        move    #8,d1           yes, setup x and y max values
        move.b  (a0),d0         see if 7 bit
        and.b   #$40,d0
        beq     initcrt3        yes
        move    #7,d1           no so set char box = 7
initcrt3:
        move    26(a0),d2       get chars per line
        mulu    d2,d1           times pixels per char
        move    d1,38(a0)       and save as x max
        clr     d0
        move.b  11(a0),d0       get scans per line
        add     #1,d0           got it
        clr     d1
        move.b  8(a0),d1        and num lines
        cmp.b   #3,10(a0)       see if interlaced
        bne     initcrt4        no
        lsl     #1,d1           yes, so real lines = lines * 2
initcrt4:
        mulu    d1,d0           lines * scans per line
        move    d0,40(a0)       = y max
initcrt2:
	rts
******************************************************
*	boot sector 1 cyl 0 from drv 0 and execute
boot:
	move.l	#rtable+romstrt,a0	; refresh boot parms
	move.l	#rtable,a1
	move	#7,d0
bootlp:
	move.l	(a0)+,(a1)+
	tst	d0
	dbeq	d0,bootlp
	move.l	#rtable,a0	; boot parms
	bsr	dsmo		; motor on
	bsr	dinit		; rezero
	bsr	readd		; read sector
	bsr	dstat		; see if error
	tst	d0
	bne	boot		; try again
	bsr	doff		; motor off
	jmp	ramstrt		; jump to it
******************************************************
*	disk routines
*	motor on
dsmo:
	move.b	(a0),$ffd005	; turn on motor
dsmo2:
	bsr.s	dcmrdy		; output tags
	move.b	#3,$ffd003
	bsr.s	dcmrdy
	move.b	#1,23(a0)	; set flag
	move.b	1(a0),$ffd003
	bsr.s	dcmrdy
	move.b	2(a0),$ffd003
	rts
******************************************************
*	output command string
dcomd:
	move	#8,d1		; set 9 cmd bytes
dcmds:
	move.l	a0,a1		; setup table move
	addq.l	#3,a1
dcomd1:
	bsr.s	dcmrdy
	move.b	(a1)+,$ffd003	; output command
	dbra	d1,dcomd1	; loop if not complete
	rts
******************************************************
*	sense controller ready for cmd
dcmrdy:
	move	#10,d2		; wait 24 usec
dcmrdy1:
	dbra	d2,dcmrdy1
dcmrdy2:
	move.b	$ffd001,d2	; read status
	and.b	#$c0,d2
	cmp.b	#$80,d2
	bne	dcmrdy2		; wait if not ready
	rts
*	sense drive status
dvstat:
	bsr	dcmrdy
	move.b	#4,$ffd003	; out status request
	bsr	dcmrdy
	move.b	4(a0),d1	; out head and drive
	and.b	#7,d1
	move.b	d1,$ffd003
dvstat1:
	move.b	$ffd001,d2	; wait for ready
	and.b	#$c0,d2
	cmp.b	#$c0,d2
	bne	dvstat1
	move.b	$ffd003,d0	; read status
	rts			; and return
*******************************************************
*	set status
dstat:
	clr	d0		; clear flag
	move.b	16(a0),d0	; get status flags
	and.b	#$f8,d0		; trim off unit no
	or.b	17(a0),d0
	or.b	18(a0),d0
	rts
******************************************************
*	seek track
dseek:
	bsr	dcmrdy		; wait ready
	move.b	#$f,$ffd003	; output seek command
	bsr	dcmrdy
	move.b	(a0),d1		; get drive
	lsr.b	#2,d1
	and.b	#3,d1
	move.b	d1,$ffd003	; output drive command
	bsr	dcmrdy		; wait
	move.b	5(a0),d1	; output cylinder
	move.b	24(a0),d0	; step factor
	lsl	d0,d1		; times cylinder
	move.b	d1,$ffd003
	bsr	dskrdy		; wait for seek complete
	rts
*****************************************************
*	sector count fix up routine
slfix:
	move.b	8(a0),d0	; get sector code
	beq.s	slfix1		; < 128 bytes
	move	#$80,d3		; generate byte count
	lsl	d0,d3
slfix2:
	sub	#1,d3		; make real max
	rts
slfix1:
	clr	d3
	move.b	11(a0),d3	; get short spec record
	bra	slfix2
*****************************************************
*	setup a command
dsetup:
	btst.b	#0,6(a0)	;is head 0 selected?
	bne	dsetup1		;no, load head 1 value
	bclr	#2,4(a0)	;set bit for head 0
	bra	dsetup2
dsetup1:
	bset	#2,4(a0)	;set bit for head 1
dsetup2:
	move.b	3(a0),d1	; get command byte
	and.b	#$e0,d1		; clr old command
	or.b	d0,d1
	move.b	d1,3(a0)	; save it
	move.b	(a0),d1		; setup drive
	lsr.b	#2,d1
	and.b	#3,d1
	move.b	4(a0),d2	; leave head alone
	and.b	#$fc,d2
	or.b	d2,d1
	move.b	d1,4(a0)
	rts
*****************************************************
*	read disk
dread:
	move.l	12(a0),a1
	move.l	#$ffd001,a4
	move.l	a4,a3
	add	#2,a4
	move.l	a4,a2
	add	#2,a4
	move.b	#$20,d4		; set mask
	move.b	(a0),d5		; set tc flag
	move.b	d5,d6
	or.b	d4,d6
	bra.s	dread1		; go do it
dread2:
	and.b	d4,d0		; trim ex bit
	beq.s	dread3		; if not exec phase, get out
dread4:
	move.b	(a2),(a1)+
dread1:
	move.b	(a3),d0		; wait till ready
	bpl	dread1
	dbra	d3,dread2	; see if count complete
	move.b	d6,(a4)		; toggle tc
	move.b	d5,(a4)
	bra	dread4
dread3:
	rts			; get out
********************************************************
*	write disk
dwrite:
	move.l	12(a0),a1	; get address
	move.l	#$ffd001,a4
	move.l	a4,a3
	add	#2,a4
	move.l	a4,a2
	add	#2,a4
	move.b	#$20,d4
	move.b	(a0),d5
	move.b	(a0),d6
	or.b	d4,d6
	bra.s	dwrite1		; do it
dwrite2:
	and.b	d4,d0
	beq.s	dwrite4
dwrite3:
	move.b	(a1)+,(a2)
dwrite1:
	move.b	(a3),d0		; see if write ready
	bpl	dwrite1
	dbra	d3,dwrite2
	move.b	d6,(a4)		; toggle tc
	move.b	d5,(a4)
	bra	dwrite3
dwrite4:
	rts
********************************************************
*	get disk results
drslt:
	move	#6,d1		; set 7 bytes
dsrsh:
	move.l	a0,a1		; setup address
	add.l	#16,a1
drslt1:
	move.b	$ffd001,d2	; test ready
	and.b	#$c0,d2
	cmp.b	#$c0,d2
	bne	drslt1
	move.b	$ffd003,(a1)+	; get results
	move	#10,d0		; wait 12 usec
drslt2:
	dbra	d0,drslt2
	dbra	d1,drslt1	; no, go back
	rts
********************************************************
*	find and read sector
readd:
	move.b	#6,d0		; make read command
	bsr	dsetup		; set it up
	bsr	dseek		; move head
	bsr	slfix		; fix length
********  turn off interrupts  ************
        move.b  #0,duartimr     ; clear interrupt mask register
	bsr	dcomd		; out command
	bsr	dread		; read data
********  turn on interrupts ******************
        move.b  imask,duartimr  ; re-enable interrupts
	bsr	drslt		; get results
	bsr	dstat		; get status in d0
	rts
********************************************************
*	find and write sector
writed:
	move.b	#5,d0		; make write
	bsr	dsetup		; set it up
	bsr	dseek		; goto cylinder
	bsr	slfix		; fix length
********  turn off interrupts  *************
        move.b  #0,duartimr     ; disable mask all interrupts
	bsr	dcomd		; out cmds
	bsr	dwrite		; write data
*******   turn on interrupts  **************
        move.b  imask,duartimr  ; re-enable interrupts
	bsr	drslt		; get results
	bsr	dstat		; get status in d0
	rts
*******************************************************
*	format a track - sector interlace pointer in d3
formatd:
	move.b	#$d,d0		; make format
	bsr	dsetup
	bsr	dseek
	bsr	dcmrdy		; output cmds
	move.l	#$ffd003,a4
	move.b	3(a0),(a4)
	bsr	dcmrdy
	move.b	4(a0),(a4)
	bsr	dcmrdy
	move.b	8(a0),(a4)
	bsr	dcmrdy
	move.b	9(a0),(a4)
	bsr	dcmrdy
********  turn off interrupts  ************
        move.b  #0,duartimr     ; mask all interrupts
	move.b	10(a0),(a4)
	bsr	dcmrdy
	move.b	#$e5,(a4)
	move.l	d3,a1		; first sector
	move.b	(a1)+,7(a0)	; into table
	move.l	#$ffd000,a3
forma1:
	move.l	a0,a2		; setup pointers
	addq.l	#5,a2
	move	#3,d1
forma3:
	move	(a3),d0
	bpl	forma3
forma2:
	move	(a3),d0		; get status
	bmi	forma2		; wait on it
	and.b	#$20,d0		; see if exec
	beq	formend		; no, finished
	move.b	(a2)+,(a4)	; output wd
	dbra	d1,forma3	; see if complete
	move.b	(a1)+,7(a0)	; bump sector number
	bra	forma1
formend:
*********  turn on interrupts  ***************
        move.b  imask,duartimr
	bsr	drslt		; get results
	rts
*********************************************************
*	turn motor off
doff:
	move.b	(a0),d0		; turn off disk
	and.b	#$fe,d0		; trim off select flag
	move.b	d0,$ffd005	; and output it
	clr.b	23(a0)		; and flag
	rts
********************************************************
*	wait for execution phase
dexecw:
	move.b	$ffd001,d1	; get status
	and.b	#$20,d1
	bne	dexecw
	rts
********************************************************
*	initialize to track 0
dinit:
	move	#2,d3
dinit1:
	bsr	dcmrdy		; see if ready
	move.b	#7,$ffd003	; output rezero
	bsr	dcmrdy
	move.b	(a0),d1		; get unit
	lsr.b	#2,d1
	and.b	#3,d1
	move.b	d1,$ffd003	; output unit
	move.b	4(a0),d2	; fix command word
	and.b	#$fc,d2
	or.b	d2,d1
	move.b	d1,4(a0)
	bsr.s	dskrdy		; see if seek ready
	dbra	d3,dinit1	; do twice to be sure
	rts
********************************************************
*	see if seek complete
dskrdy:
	move	$ffd000,d1	; get status
	bmi	dskrdy		; back no intrp
	bsr	dcmrdy		; see if rdy
	move.b	#8,$ffd003	; output seek read
	move	#1,d1		; get results
	bsr	dsrsh
	rts
*******************************************************
*	read id
dredid:
	move.b	#$a,d0		; setup readid
	bsr	dsetup
	move.b	#1,d1		; out commands
	bsr	dcmds
	bsr	dexecw		; wait for end
	move.b	(a0),d1		; toggle tc
	or.b	#$20,d1
	move.b	d1,$ffd005
	and.b	#$df,d1
	move.b	d1,$ffd005
	bsr	drslt		; get results
	rts
*******************************************************
*	format a disk
format:
	clr.b	5(a0)		; clear cylinder
	bsr	dsmo
	bsr	dinit
format1:
	bsr	formatd
	addq.b	#1,5(a0)
	move.b	5(a0),d0
	cmp.b	25(a0),d0
	bne	format1
	bsr	doff		; motor off
	rts			; return
********************************************************
*	graphics screen clear
initgrf:
        move.l   prescrn,a0    get present screen
        tst.b    (a0)          see if graphics
        bmi.s    hgrin1        no, then exit
        move.l   20(a0),d0      get bytes/scan
        move.b   11(a0),d2      get # scans-1
        ext      d2
        move.l   32(a0),a1      get start
        move.l   a1,a2          save it
        lsr      #2,d0          convert # bytes to # longwords
        sub      #1,d0          and make count
hgrin2:
        move     d0,d1          set counter
hgrin3:
        clr.l    (a1)+          clear longword
        dbra     d1,hgrin3      loop to clear scan group
        add.l    #$4000,a2      set to next scan group start
        move.l   a2,a1
        dbra     d2,hgrin2      do next set of scans
        move.l   scrnpt,a5      move screen text if required
        move.l   32(a5),d0      get max
        add.l    20(a5),d0      got it
        clr.l    d1             get mix point
        move.b   1(a5),d1       msb's
        swap     d1
        move     16(a5),d1      lsb's
        add.l    #1,d1          +1 = mix point
        cmp.l    d1,d0          see if any chars possible
        bpl      hgrin1         no
        sub.l    d1,d0          length = max - mix
        move.l   d0,d6          save it
        bmi      hgrin1         exit if outside range
        move.l   d1,a6          save pointer
hgrin4:
        move.b   (a6),d0        get char
        bsr      grafchr        output it
        add.l    #1,a6          bump pointer
        dbra     d6,hgrin4      get all of the characters
hgrin1:
	rts
********************************************************
*	set color
setcolr:
        move.l  prescrn,a0     get parms
        and     #$f,d1         force to nibble
        tst.b   (a0)           make sure it is graphics
        bmi.s   setcol1        no exit
        move.b  d1,36(a0)      yes, save it
setcol1:
	rts
********************************************************
*	plot point
plotp:
        move.l  prescrn,a0     get parms
        tst.b   (a0)           see if graphics
        bmi     setcol1        no, return
        bsr     plotca         get pixel address
        move.l  d0,a1          set it
        move.b  36(a0),d1      get color
        tst.b   37(a0)         see if color on
        bne.s   hplot1         yes
        and     #1,d1          no, so lsb only significant
        lsl     #7,d1
        lsr     d4,d1          shift into position
        move    #$80,d3        make mask
        lsr     d4,d3
        not     d3
hplot2:
        tst.b   36(a0)         see if exclusive or function
        bpl     hplot3         no
        not.b   d3             set to flag xor bit(s)
        eor.b   d3,(a1)        xor it in
        rts                    and return
hplot3:
        and.b   d3,(a1)        trim old bits
        or.b    d1,(a1)        add new bit(s)
        rts                    and done
hplot1:
        lsl     #4,d1          color so use nibble
        and     #4,d4          make mask
        move    #$f,d3
        lsl     d4,d3
        lsr     d4,d1          and put color in place
        bra     hplot2         put it
plotca:
        tst     d1             address calc - test limits
        bmi.s   getpos1        neg, no good
        tst     d2             test y
        bmi.s   getpos1        ditto
        cmp     38(a0),d1      see if greater than limits
        bpl.s   getpos1        x is
        cmp     40(a0),d2      test y
        bmi.s   plotc4         go if ok
getpos1: 
        add.l   #4,a7          error, so get out
        rts                    by popping stack
plotc4:
        move    d1,d0          calculate scan point address to nearst byte
        and     #7,d0
        move    d0,d4          set nibble position
        lsr     #3,d1          get byte position
        ext.l   d1             make long calculation
        ext.l   d2
        move.l  d2,d0          get y
        cmp.b   #3,11(a0)      see if 1,2 or 4 scans
        beq.s   plotc5         4 scans
        tst.b   11(a0)
        beq.s   plotc7         1 scan
        and     #1,d0          2 scans/ set odd/even
        lsr.l   #1,d2          get real line
        bra.s   plotc6
plotc7:
        move    d1,d0
        bra.s   plotc8
plotc5:
        lsr.l   #2,d2          4 scans- set 0-3
        and.l   #3,d0
plotc6:
        move    #14,d3         set scan offset
        lsl.l   d3,d0          by mult scan by 4000h
        add.l   d1,d0          add x position
plotc8:
        move    14(a0),d1      get base
        swap    d1
        move.b  1(a0),d1
        ext     d1
        swap    d1
        move.l  24(a0),d3      num bytes per line
        mulu    d3,d2          scans * bytes/line = rel location
        add.l   d2,d0          + x position
        add.l   d1,d0          + base address
        rts                    gives abs location
********************************************************
*	plot line
plotl:
	rts
********************************************************
*	read color at point
readgrf:
        move.l  prescrn,a0    get screen
        tst.b   (a0)          see if graphics
        beq.s   rdcolr        no, get out
        bsr     plotca        get address
        move.l  d0,a1
        move.b  (a1),d0       get graphics byte
        tst.b   37(a0)        see if color on
        bne.s   rdcol1        yes
        move.b  #7,d1         no, so set up for single bit
        sub.b   d4,d1         get shift count
        lsr.b   d1,d0         move bit into position
        and     #1,d0         and get it
rdcolr:
        rts                   done
rdcol1:  
        and.b   #4,d4         trim to make shift count 0 or 4
        move.b  #4,d1
        sub.b   d4,d1
        lsr.b   d1,d0
        and     #$f,d0        mask it
        rts                   and done
********************************************************
*	read character at cursor
readcur:
        move.l  scrnpt,a5       get screen parms
	move.l	28(a5),a1	; get present location
	move.b	(a1),d0		; get character
	eor.b	#$80,d0		; toggle cursor inverse bit
	rts			; and return
*******************************************************
*	move cursor to desired location
cursor:
        move.l  prescrn,a0      get present parms
        move.l  scrnpt,a5       get screen parms
	move.l	28(a5),a6	; get present location
        move    d1,d6           save input regs
        move    d2,d7
	eor.b	#$80,(a6)	; clear present cursor
        bsr     grafput         and in graphics if required
	bsr	curtst		; see if new location in range
	move  	d7,d3
	move.l	24(a5),d4
	mulu	d4,d3		; calulate new cursor location
	add	d6,d3
	add.l	32(a5),d3	; pos=x+y*linelength+base
	movea.l	d3,a6
	move.l	d3,28(a5)	; save new cursor
	eor.b	#$80,(a6)	; turn on cursor
        bsr     grafput
	rts
*	test and fix up cursor x,y values
curtst:
	sub  	#1,d6		; set to base 0
	bmi.s	cursor1		; x < 0
	cmp	26(a5),d6
	bpl.s	cursor2		; x > max
cursor5:
	sub	#1,d7		; set base 0 y value
	bmi.s	cursor3		; y < 0
        move.l  20(a5),d3        calc max length  - get buf length
        divs    26(a5),d3        divided by line length
        cmp     d3,d7           see if limit exceeded
	bpl	cursor4		; y > max
	rts			; return
cursor1:
	clr	d6		; x<0 so set x=0
	bra	cursor5		; go test y
cursor2:
	move	26(a5),d6	; x>max so set x=max
	bra	cursor5		; go try again
cursor3:
	clr	d7		; y<0 so set y=0
	rts
cursor4:
	move	d3,d7	; y>max so set y=max
        sub     #1,d7   -1
        rts
*********************************************************
*	initialize modem
initmod:
	rts			; not implemented
*********************************************************
*	initialize real time clock
initrtc:
	rts			; not implemented
*********************************************************
*	get printer status
prtsts:
	clr.b	d0		; clear flag
	move.b	$ffc41b,d2	; get status
	and.b	#1,d2		; trim bit
	beq	prtst1		; branch ready
	move.b	#$ff,d0		; otherwise set busy
prtst1:
	rts			; and return
********************************************************
*	output character to printer
print:
	bsr	prtsts		; get status
	tst.b	d0
	bne	print		; wait till ready
        tst     config  test printer revision
        bpl     print1          rev B
	move.b	#$7f,$ffc41f	; clear all bits
	or.b	#$80,d1		; clear strobe bit
	move.b	d1,$ffc41d	; output data
	move.b	#$80,$ffc41f	; set strobe (low true)
	nop			; wait on it
	nop
	move.b	#$80,$ffc41d	; clear strobe
	rts			; and finished
print1:  
        move.b  #$ff,$ffc41f     clear printer bits
        move.b  d1,$ffc41d      output data
        move.b  $ffd801,d2      set strobe
        nop 
        nop                     wait for it
        nop
        rts
*********************************************************
keytab:
*    key code translation table
     .dc.b    $80,$81,$82,$83,$84,$85,$86,$87     ; CODES 00-07
     .dc.b    $88,$8a,$30,$2e,$31,$32,$33,$34     ; codes 08-0f
     .dc.b    $b0,$b1,$1c,$b3,$1f,$35,$1d,$b7     ; codes 10-17
     .dc.b    $1e,$b9,$35,$36,$37,$38,$39,$aa     ; codes 18-1f
     .dc.b    $20,$21,$22,$23,$24,$25,$26,$27     ; codes 20-27
     .dc.b    $28,$29,$2a,$2b,$2c,$2d,$2e,$2f     ; codes 28-2f
     .dc.b    $30,$31,$32,$33,$34,$35,$36,$37     ; codes 30-37
     .dc.b    $38,$39,$3a,$3b,$3c,$3d,$3e,$3f     ; codes 38-3f
     .dc.b    $40,$41,$42,$43,$44,$45,$46,$47     ; codes 40-47
     .dc.b    $48,$49,$4a,$4b,$4c,$4d,$4e,$4f     ; codes 48-4f
     .dc.b    $50,$51,$52,$53,$54,$55,$56,$57     ; codes 50-57
     .dc.b    $58,$59,$5a,$5b,$5c,$5d,$5e,$5f     ; codes 58-5f
     .dc.b    $60,$61,$62,$63,$64,$65,$66,$67     ; codes 60-67
     .dc.b    $68,$69,$6a,$6b,$6c,$6d,$6e,$6f     ; codes 68-6f
     .dc.b    $70,$71,$72,$73,$74,$75,$76,$77     ; codes 70-77
     .dc.b    $78,$79,$7a,$7b,$7c,$7d,$7e,$7f     ; codes 78-7f
     .dc.b    $80,$81,$82,$83,$84,$85,$86,$87     ; codes 80-87
     .dc.b    $88,$8a,$0d,$7f,$8b,$bb,$88,$8a     ; codes 88-8f
     .dc.b    $b0,$b1,$94,$b3,$92,$b5,$93,$b7     ; codes 90-97
     .dc.b    $91,$b9,$80,$81,$82,$83,$84,$85     ; codes 98-9f
     .dc.b    $80,$81,$82,$83,$84,$85,$86,$87     ; codes a0-a7
     .dc.b    $88,$8a,$b9,$09,$12,$ad,$86,$87     ; codes a8-af
     .dc.b    $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7     ; codes b0-b7
     .dc.b    $b8,$b9,$35,$13,$2a,$bd,$b1,$b3     ; codes b8-bf
     .dc.b    $00,$01,$02,$03,$04,$05,$06,$07     ; codes c0-c7
     .dc.b    $08,$09,$0a,$0b,$0c,$0d,$0e,$0f     ; codes c8-cf
     .dc.b    $10,$11,$12,$13,$14,$15,$16,$17     ; codes d0-d7
     .dc.b    $18,$19,$1a,$1b,$1c,$1d,$1e,$1f     ; codes d8-df
     .dc.b    $b0,$e1,$e2,$e3,$e4,$e5,$e6,$e7     ; codes e0-e7
     .dc.b    $e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef     ; codes e8-ef
     .dc.b    $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7     ; codes f0-f7
     .dc.b    $f8,$f9,$fa,$b7,$b9,$11,$14,$ff     ; codes f8-ff
**********************************************
*      graphics character table
cgtabl:
        .dc.l    $0010387c,$38100000      0
        .dc.l    $080808f8,$08080808      1
        .dc.l    $000000f8,$08080808      2
        .dc.l    $0808080f,$00000000      3
        .dc.l    $080808ff,$00000000      4
        .dc.l    $000000ff,$08080808      5
        .dc.l    $0808080f,$08080808      6
        .dc.l    $000000ff,$00000000      7
        .dc.l    $080808ff,$08080808      8
        .dc.l    $080808f8,$00000000      9
        .dc.l    $0000000f,$08080808      10
        .dc.l    $00225408,$22540800      11
        .dc.l    $00000000,$ffffffff      12
        .dc.l    $f0f0f0f0,$f0f0f0f0      13
        .dc.l    $00003649,$36000000      14
        .dc.l    $007e007e,$007e0000      15
        .dc.l    $88224411,$88224411      16
        .dc.l    $021c244a,$52243840      17
        .dc.l    $aa55aa55,$aa55aa55      18
        .dc.l    $00081c08,$001c0000      19
        .dc.l    $00300804,$0830003c      20
        .dc.l    $000c1020,$100c003c     21
        .dc.l    $0008003e,$00080000      22
        .dc.l    $1f909090,$50502000      23
        .dc.l    $00081c2a,$08080808      24
        .dc.l    $00080808,$2a1c0800      25
        .dc.l    $0008047e,$04080000      26
        .dc.l    $0010207e,$20100000      27
        .dc.l    $c0204080,$e0000000      28
        .dc.l    $10102840,$28101000      29
        .dc.l    $0c121038,$10382600      30
        .dc.l    $2214081c,$081c0800      31
        .dc.l    $00000000,$00000000      32
        .dc.l    $08080808,$00080000  !   33
        .dc.l    $14141400,$00000000  "   34
        .dc.l    $14143e14,$3e141400  #   35
        .dc.l    $081e281c,$123c0800  $   36
        .dc.l    $30320408,$10260600  %   37
        .dc.l    $18242810,$2a241a00  &   38
        .dc.l    $18081000,$00000000  '   39
        .dc.l    $04081010,$10080400  (   40
        .dc.l    $10080404,$04081000  )   41
        .dc.l    $082a1c1c,$2a080000  *   42
        .dc.l    $0008083e,$08080000  +   43
        .dc.l    $00000000,$18180810  ,   44
        .dc.l    $0000001e,$00000000  -   45
        .dc.l    $00000000,$00181800  .   46
        .dc.l    $00020408,$10200000  /   47
        .dc.l    $1c22262a,$32221c00  0   48
        .dc.l    $08180808,$08081c00  1   49
        .dc.l    $1c220204,$08103e00  2   50
        .dc.l    $3e040804,$02221c00  3   51
        .dc.l    $040c1424,$3e040400  4   52
        .dc.l    $3e203c02,$02221c00  5   53
        .dc.l    $0c10203c,$22221c00  6   54
        .dc.l    $3e020408,$10101000  7   55
        .dc.l    $1c22221c,$22221c00  8   56
        .dc.l    $1c22221e,$02041800  9   57
        .dc.l    $00181800,$18180000  :   58
        .dc.l    $00181800,$18180810  ;   59
        .dc.l    $04081020,$10080400  <   60
        .dc.l    $00003e00,$3e000000  =   61
        .dc.l    $10080402,$04081000  >   62
        .dc.l    $1c220204,$08000800  ?   63
        .dc.l    $1c222a2a,$2c201e00  @   64
        .dc.l    $08142222,$3e222200  A   65
        .dc.l    $3c22223c,$22223c00  B   66
        .dc.l    $1c222020,$20221c00  C   67
        .dc.l    $38242222,$22243800  D   68
        .dc.l    $3e20203c,$20203e00  E   69
        .dc.l    $3e20203c,$20202000  F   70
        .dc.l    $1c222020,$26221e00  G   71
        .dc.l    $2222223e,$22222200  H   72
        .dc.l    $1c080808,$08081c00  I   73
        .dc.l    $0e040404,$04241800  J   74
        .dc.l    $22242830,$28242200  K   75
        .dc.l    $20202020,$20203e00  L   76
        .dc.l    $22362a2a,$22222200  M   77
        .dc.l    $2222322a,$26222200  N   78
        .dc.l    $1c222222,$22221c00  O   79
        .dc.l    $3c22223c,$20202000  P   80
        .dc.l    $1c222222,$2a241a00  Q   81
        .dc.l    $3c22223c,$28242200  R   82
        .dc.l    $1c22201c,$02221c00  S   83
        .dc.l    $3e080808,$08080800  T   84
        .dc.l    $22222222,$22221c00  U   85
        .dc.l    $22222222,$22140800  V   86
        .dc.l    $22222222,$2a2a1400  W   87
        .dc.l    $22221408,$14222200  X   88
        .dc.l    $22222214,$08080800  Y   89
        .dc.l    $3e020408,$10203e00  Z   90
        .dc.l    $3e303030,$30303e00  [   91
        .dc.l    $00201008,$04020000  \   92
        .dc.l    $3e060606,$06063e00  ]   93
        .dc.l    $08142200,$00000000  ^   94
        .dc.l    $00000000,$00003e00  _   95
        .dc.l    $10080400,$00000000  `   96
        .dc.l    $00003804,$1c241e00  a   97
        .dc.l    $20203c22,$22223c00  b   98
        .dc.l    $00000e10,$10100e00  c   99
        .dc.l    $02021e22,$22221e00  d   100
        .dc.l    $00001c22,$3e201e00  e   101
        .dc.l    $06081e08,$08080800  f   102
        .dc.l    $00001e22,$221e021c  g   103
        .dc.l    $10101c12,$12121200  h   104
        .dc.l    $08001808,$08081c00  i   105
        .dc.l    $04000404,$04042418  j   106
        .dc.l    $10101214,$18141200  k   107
        .dc.l    $18080808,$08081c00  l   108
        .dc.l    $0000342a,$2a2a2a00  m   109
        .dc.l    $00001c12,$12121200  n   110
        .dc.l    $00001c22,$22221c00  o   111
        .dc.l    $00003c22,$223c2020  p   112
        .dc.l    $00001e22,$221e0202  q   113
        .dc.l    $00001618,$10101000  r   114
        .dc.l    $00001e20,$1c023c00  s   115
        .dc.l    $08081c08,$08080600  t   116
        .dc.l    $00001212,$12120e00  u   117
        .dc.l    $00002222,$22140800  v   118
        .dc.l    $00002222,$2a2a1400  w   119
        .dc.l    $00002214,$08142200  x   120
        .dc.l    $00002424,$241c0418  y   121
        .dc.l    $00003e04,$08103e00  z   122
        .dc.l    $0c101020,$10100c00  {   123
        .dc.l    $08080808,$08080808  |   124
        .dc.l    $18040402,$04041800  }   125
        .dc.l    $102a0400,$00000000  ~   126
        .dc.l    $0008103e,$10080000      127
	.org	$1FEb
	.dc.b   '83350'     ; Julian last update
	.dc.w	$0018      interrupt vectors
	.dc.w	$0019
	.dc.w	$001a
	.dc.w	$001b
	.dc.w	$001c
	.dc.w	$001d
	.dc.w	$001e
	.dc.w	$001f
	.end
