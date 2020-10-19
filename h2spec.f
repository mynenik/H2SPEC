
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ++++++++++++++++++++++++    H2SPEC    +++++++++++++++++++++++++++++++
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C	Modified 28 June 1989 for properly normalized HL factors.
C       Modified 24 July 1990 to output spectrum file with header.
C       Modified 25 July 1994 for simpler  excitation model.
C       Modified 26 June 1996 with Krishna's excitation model and
C         revised C-X Honl-London factor
C       Modified 07 Apr  1999 for g77

C	Compute the spectrum of the Werner and Lyman bands of H2:
C         Use the modern normalization of HL factors.
C	  Include rotational temperature for excitation from X.
C	  Include ortho-para population factor.
C	  Allow selection of resolution and wavelength sample.
C	  Store simulated spectrum for future use.

C	Input files:
C
C	The energy files contain levels in this format --
C	  energy (cm-1),v,j,p  F10.3,5X,F5.2,5X,F5.2,5X.F5.2
C	  These files must be ordered from v=0 upward and must include
C	  J=0 as the first entry for a given v.
C
C	  XLEVELS.DAT
C	  CLEVELS.DAT
C	  BLEVELS.DAT
C
C
C	The f-value files also contain quantum numbers in the format --
C	  fvalue,vpp,vp F10.3,5X,F5.2,5X,F5.2
C
C	  XCF.DAT
C	  XBF.DAT
C
C
C	Output file:
C
C	  H2VUV.DAT
C
C
C	Arrays:
C
C	  HLEVEL -- serial list of energy levels
C
C	  HQN    -- corresponding quantum numbers with index
C                   (x,1) = 0 for X (ground state)
C                         = 1 for B (1st excited state for Lyman bands)
C                         = 2 for C (2nd excited state for Werner bands)
C                   (x,2) = v
C                   (x,3) = J
C                   (x,4) = p (0 or 1 for para or ortho hydrogen)
C
C	  HF     -- absorption oscillator strength (x,vpp,vp)
C                   x = 1 for X-B Lyman transitions (sigma)
C                     = 2 for X-C Werner transitions (pi)
C                   vpp = ground (X) vibrational
C                   vp  = excited (B or C) vibrational number
C
C	  HLINE -- serial list of transition energies (cm-1)
C
C	  HA    -- corresponding transition probabilities (sec-1)
C
C	  HP    -- population factor for the line
C
C	  LQN   -- quantum number reference for (line,x) where
C	           x=1 for lower level and x=2 for the upper level
C
C	  HXY   -- spectrum (wavelength,intensity)
C                  wavelength = transition in angstroms
C                  intensity  = (excited population)*A*1e-6

	PROGRAM H2SPEC

	DIMENSION HLEVEL(512),HQN(512,4),HF(2,16,16)
	DIMENSION HLINE(16384),HA(16384),HP(16384),HXY(16384,2)
	DIMENSION LQN(16384,2)
	COMMON HLEVEL,HQN,HF,HLINE,HA,HP,HXY,LQN,NX,NLEVELS,NLINES,
     1   EKT,WNLOW,WNHIGH,FWHM

	CALL INITPGM
	CALL LEVELS
	CALL FVALS
	CALL LINES
	CALL EXCITE
	CALL SPECTR
	CALL WSPEC
	CALL WLINES

	END
C
C --------------------------------------------------------------------
C
	SUBROUTINE INITPGM
C
C	Initialize program variables.
C
	DIMENSION HLEVEL(512),HQN(512,4),HF(2,16,16)
	DIMENSION HLINE(16384),HA(16384),HP(16384),HXY(16384,2)
	DIMENSION LQN(16384,2)
	COMMON HLEVEL,HQN,HF,HLINE,HA,HP,HXY,LQN,NX,NLEVELS,NLINES,
     1    EKT,WNLOW,WNHIGH,FWHM

	WRITE(6,1000)
 1000	FORMAT(/' Compute the H2 spectrum with these conditions: '/)
	WRITE(6,1010)
 1010	FORMAT(' Rotational temperature (K) >> ',$)
	READ(5,*) EKT
 	EKT=EKT*0.695
	WRITE(6,1020)
 1020	FORMAT(' Lower wavenumber (cm-1) >> ',$)
	READ(5,*) WNLOW
	WRITE(6,1030)
 1030	FORMAT(' Upper wavenumber (cm-1) >> ',$)
	READ(5,*) WNHIGH
	WRITE(6,1040)
 1040	FORMAT(' FWHM (cm-1) >> ',$)
	READ(5,*) FWHM

	RETURN
	END
C
C --------------------------------------------------------------------
C
	SUBROUTINE LEVELS
C
C	Initialize the arrays HLEVEL and HQN
C
	DIMENSION HLEVEL(512),HQN(512,4),HF(2,16,16)
	DIMENSION HLINE(16384),HA(16384),HP(16384),HXY(16384,2)
	DIMENSION LQN(16384,2)
	COMMON HLEVEL,HQN,HF,HLINE,HA,HP,HXY,LQN,NX,NLEVELS,NLINES,
     1    EKT,WNLOW,WNHIGH,FWHM
C
C	Retrieve X levels
C
	OPEN(UNIT=20,FILE='xlevels.dat',STATUS='OLD')

	L=0

        DO WHILE (.TRUE.)
          READ(20, 1000, END = 200) E, AV, AJ, AP
 1000     FORMAT(F10.3,3(5X,F5.2))
          L = L + 1
          HLEVEL(L) = E
          HQN(L,1) = 0.
          HQN(L,2) = AV
          HQN(L,3) = AJ
          HQN(L,4) = AP
        END DO

 200	CONTINUE
C
C	X levels have been stored
C
        NX = L
	CLOSE(20)
C
C	Retrieve B levels
C
	OPEN(UNIT=20,FILE='blevels.dat',STATUS='OLD')

        DO WHILE (.TRUE.)
          READ(20, 1000, END = 400) E, AV, AJ, AP
          L = L + 1
          HLEVEL(L) = E
          HQN(L,1) = 1.
          HQN(L,2) = AV
          HQN(L,3) = AJ
          HQN(L,4) = AP
        END DO

 400	CONTINUE
C
C	B levels have been stored
C
	CLOSE(20)
C
C	Retrieve C levels
C
	OPEN(UNIT=20,FILE='clevels.dat',STATUS='OLD')

        DO WHILE (.TRUE.)
          READ(20, 1000, END = 600) E, AV, AJ, AP
          L = L + 1
          HLEVEL(L) = E
          HQN(L,1) = 2.
          HQN(L,2) = AV
          HQN(L,3) = AJ
          HQN(L,4) = AP
        END DO

 600	CONTINUE
C
C	C levels have been stored
C
	CLOSE(20)

        NLEVELS = L

	RETURN
	END
C
C --------------------------------------------------------------------
C
	SUBROUTINE FVALS

C	Retrieve f-values for vibrational transitions
C       Use Allison and Dalgarno f-values.
C         Their normalization agrees with Thorne-Whiting-Schadee.


	DIMENSION HLEVEL(512),HQN(512,4),HF(2,16,16)
	DIMENSION HLINE(16384),HA(16384),HP(16384),HXY(16384,2)
	DIMENSION LQN(16384,2)
	COMMON HLEVEL,HQN,HF,HLINE,HA,HP,HXY,LQN,NX,NLEVELS,NLINES,
     1    EKT,WNLOW,WNHIGH,FWHM
C
C	Retrieve the Lyman band
C
	OPEN(UNIT=20,FILE='xbf.dat',STATUS='OLD')

        DO WHILE (.TRUE.)
          READ(20, 1000, END = 200) F, AVPP, AVP
 1000     FORMAT(F10.3,2(5X,F5.2))
          IVPP = AVPP
          IVP = AVP
          HF(1,IVPP,IVP) = F
        END DO

 200	CONTINUE
	CLOSE(20)
C
C	Lyman band XB transitions have been stored
C
C	Retrieve the Werner band
C
	OPEN(UNIT=20,FILE='xcf.dat',STATUS='OLD')

        DO WHILE (.TRUE.)
          READ(20, 1000, END = 400) F, AVPP, AVP
          IVPP = AVPP
          IVP = AVP
          HF(2,IVPP,IVP) = F
        END DO

 400	CONTINUE
	CLOSE(20)
C
C	Werner band XC transitions have been stored
C
	RETURN
	END
C
C --------------------------------------------------------------------
C
	SUBROUTINE LINES
C
C	Compute the line energies and transition probabilities
C
	DIMENSION HLEVEL(512),HQN(512,4),HF(2,16,16)
	DIMENSION HLINE(16384),HA(16384),HP(16384),HXY(16384,2)
	DIMENSION LQN(16384,2)
	COMMON HLEVEL,HQN,HF,HLINE,HA,HP,HXY,LQN,NX,NLEVELS,NLINES,
     1    EKT,WNLOW,WNHIGH,FWHM

        A0 = 6.6702E15
        ILMAX = 16384
        NBC0 = NX + 1
        ILINE = 0
C
C	Each lower level 
C
        DO IX = 1, NX
          ELOW = HLEVEL(IX)
C
C	Each upper level
C
          DO IBC = NBC0, NLEVELS
            ILINE = ILINE + 1
            IF(ILINE.GT.ILMAX) THEN
              WRITE(6,1000)
 1000         FORMAT(/' Too many lines for the available memory.')
              NLINES = ILMAX
              RETURN
            END IF
C
C	Find the transition energy
C
            EHIGH = HLEVEL(IBC)
            EDIF = EHIGH - ELOW
            HLINE(ILINE) = EDIF
C
C	Label the level
C
            LQN(ILINE,1) = IX
            LQN(ILINE,2) = IBC
C
C	Find the transition probability
C
            AVPP = HQN(IX,2)
            AVP = HQN(IBC,2)
            AJPP = HQN(IX,3)
            AJP = HQN(IBC,3)
            APPP = HQN(IX,4)
            APP = HQN(IBC,4)
            AID = HQN(IBC,1)
            ID = AID
            IVPP = AVPP
            IVP = AVP
            JPP = AJPP
            JP = AJP
            IPP = APP
C
C	Hold reference wavelength for JPP=0 to JP=0 transition.
C
            IF (JP.EQ.0.AND.JPP.EQ.0) THEN
              WREF = 1.E8/EDIF
            END IF
C
C	Compute and store the transition probability.
C
            FVALUE = HF(ID,IVPP,IVP)

            CALL SJ (ID, JPP, JP, HLFACT)
            CALL GP (ID, IPP, GFACT)

            W = 1.E8/EDIF
            AVALUE = A0*(WREF/(W**3))
            HLFACT = HLFACT/(GFACT*(2.*AJP + 1))
            AVALUE = AVALUE*HLFACT*FVALUE
C
C	Set probability to zero for ortho-para transition.
C
            IF (APPP.NE.APP) AVALUE = 0.
            HA(ILINE) = AVALUE
C
C	Delete zero intensity lines from the list.
C
            IF (AVALUE.EQ.0.) ILINE = ILINE - 1

          END DO
        END DO

        NLINES = ILINE

	RETURN
	END
C
C --------------------------------------------------------------------
C
        SUBROUTINE SJ (ID, JPP, JP, HLFACT)

C       Compute the Honl-London factor:
C         Use the "modern" normalization described by Thorne in
C         Spectrophysics, 2nd edition, p. 313.
C         Values are taken from Morton and Dinerstein,
C           Ap. J. 204 (1976) 1-11, who quote Schadee's results in
C           the "old" normalization to (2J"+1).
C           There is a potential problem with the C-X case, since
C           following Thorne SJ should be modified
C           to the "new" normalization of g(2J"+1) by multiplying the
C           values for the Werner band by 2, the value of g.
C           Earlier versions of the program used twice factor given
C           here for the Werner band.


        IF (ID.EQ.1) THEN                     ! Lyman B-X band
          IF (JP.EQ.JPP+1) THEN               ! R-branch
            HLFACT = FLOAT(JPP + 1)
	  ELSE IF (JP.EQ.JPP) THEN            ! Q-branch
            HLFACT = 0.	  
          ELSE IF (JP.EQ.JPP-1) THEN          ! P-branch
            HLFACT = FLOAT(JPP)
          ELSE                                ! All other branches
            HLFACT =0.
	  END IF
 
        ELSE IF (ID.EQ.2) THEN                ! Werner C-X band
          IF (JP.EQ.JPP+1) THEN               ! R-branch 
            HLFACT = FLOAT(JPP + 2)/2.
          ELSE IF (JP.EQ.JPP) THEN            ! Q-branch
            HLFACT = FLOAT(2*JPP + 1)/2.
          ELSE IF (JP.EQ.JPP-1) THEN          ! P-branch
            HLFACT = FLOAT(JPP - 1)/2.
          ELSE
            HLFACT=0.                         ! All other branches
	  END IF
        ELSE
          HLFACT=0.                           ! All other id's
	END IF

	RETURN
	END
C
C --------------------------------------------------------------------
C
        SUBROUTINE GP (ID, IPP, GFACT)

C	Compute G-factor for this band.  The population factors for
C       parahydrogen (IPP=0) and orthohydrogen (IPP=1) are
C       are carried by the EXCITE subroutine, where the
C       statistics are 3:1 in favor of parahydrogen.

C	ID=1 for Lyman band and ID=2 for Werner band.

        IF (ID.EQ.1) THEN                     ! Lyman B-X band
          GFACT = 1.
        ELSE IF (ID.EQ.2) THEN
          GFACT = 2.                          ! Werner C-X band
        ELSE
          GFACT=0.
        END IF

	RETURN
	END
C
C --------------------------------------------------------------------
C
	SUBROUTINE EXCITE

C	Find the excited state population factors for each line.

	DIMENSION HLEVEL(512),HQN(512,4),HF(2,16,16)
	DIMENSION HLINE(16384),HA(16384),HP(16384),HXY(16384,2)
	DIMENSION LQN(16384,2)
	COMMON HLEVEL,HQN,HF,HLINE,HA,HP,HXY,LQN,NX,NLEVELS,NLINES,
     1    EKT,WNLOW,WNHIGH,FWHM

C	Impose a statistical distribution based on ground state populations.
C	  Copy the ground state rotational distribution to the excited states.
C	  Assume that electron impact excitation occurs without change in J.

C         HQN    -- corresponding quantum numbers with index
C                   (x,1) = 0 for X (ground state)
C                         = 1 for B (1st state for Lyman bands)
C                         = 2 for C (2nd state for Werner bands)
C                   (x,2) = v
C                   (x,3) = J
C                   (x,4) = p (0 or 1 for para or ortho hydrogen)

C	Do each line.

        DO ILINE = 1, NLINES

C	Identify the upper state.

          IBC = LQN(ILINE,2)                  ! Index for upper level
          IJP = HQN(IBC,3)                    ! Upper level J
          IPP = HQN(IBC,4)                    ! Upper level p 
          
          IF (IPP.EQ.0) THEN                  ! Parahydrogen
            STAT=1.
          ELSE IF (IPP.EQ.1) THEN             ! Orthohydrogen
            STAT=3.             
          ELSE
            STAT=0.                           ! No parity trap error
          END IF
          
          STAT=STAT*FLOAT(2*IJP+1)            ! Rotational degeneracy
          

!       Estimate excitation of rotation with ground state Boltzmann factor
!       Find the lowest X state which contributes to this line's upper state.

          ILOW=0
          IX=1
          DO WHILE ((IX.LE.NX).AND.(ILOW.EQ.0))
            IVPP = HQN(IX,2)                  ! Lower level v
            IJPP = HQN(IX,3)                  ! Lower level J
            IPPP = HQN(IX,4)                  ! Lower level p
            IF (IJPP.EQ.IJP) THEN
              ILOW = IX
	    END IF
            IX = IX+1
	  END DO

          IF(ILOW.EQ.0) THEN                  ! No matching J
            STAT = 0.                         ! No excitation to upper state 
          ELSE IF (IVPP.GT.0) THEN            ! Lowest state is not in v=0
            STAT = 0.                         ! No excitation to upper state
          ELSE
            ELOW = HLEVEL(ILOW)               ! Otherwise use Boltzmann factor
            STAT = STAT*EXP(-ELOW/EKT)
          END IF
          HP(ILINE) = STAT
	END DO

	RETURN
	END
C
C --------------------------------------------------------------------
C
	SUBROUTINE SPECTR
C
C	Compute a spectrum from the files HLINE and HA and store in HXY.
C	  Use sum of lorentzians for all transitions.
C
	DIMENSION HLEVEL(512),HQN(512,4),HF(2,16,16)
	DIMENSION HLINE(16384),HA(16384),HP(16384),HXY(16384,2)
	DIMENSION LQN(16384,2)
	COMMON HLEVEL,HQN,HF,HLINE,HA,HP,HXY,LQN,NX,NLEVELS,NLINES,
     1    EKT,WNLOW,WNHIGH,FWHM

        WPTS = 4.*(WNHIGH - WNLOW)/FWHM
        NPTS = WPTS
        HWHM = FWHM/2.
        HWHM2 = HWHM*HWHM
        W = WNHIGH
        WD = (WNHIGH - WNLOW)/NPTS

        DO I = 1, NPTS
          W = W - WD
          SUM = 0.

          DO J = 1, NLINES
            W0 = HLINE(J)
            Y = HWHM2/(HWHM2 + (W - W0)**2)
            Y = Y*HA(J)*HP(J)
            SUM = SUM + Y
          END DO

          HXY(I,1) = W
          HXY(I,2) = SUM
        END DO

	RETURN
	END
C
C --------------------------------------------------------------------
C
	SUBROUTINE WSPEC
C
C	Write a spectral data file.
C
	DIMENSION HLEVEL(512),HQN(512,4),HF(2,16,16)
	DIMENSION HLINE(16384),HA(16384),HP(16384),HXY(16384,2)
	DIMENSION LQN(16384,2)
	COMMON HLEVEL,HQN,HF,HLINE,HA,HP,HXY,LQN,NX,NLEVELS,NLINES,
     1    EKT,WNLOW,WNHIGH,FWHM

        T = EKT/0.695

	OPEN(UNIT=20,FILE='h2vuv.dat',STATUS='NEW')

C       Write header to file

        WRITE (20, 100)
        WRITE (20, *)
        WRITE (20, 200)
        WRITE (20, *)
        WRITE (20, 300) T
        WRITE (20, 400) FWHM
        WRITE (20, 500) WNLOW
        WRITE (20, 600) WNHIGH
        WRITE (20, *)
        WRITE (20, 700)

 100    FORMAT(' /*')
 200    FORMAT(' Computed H2 Spectrum')
 300    FORMAT('     Rotational Temperature (K)     =  ', F10.2)
 400    FORMAT('     FWHM (cm-1)                    =  ', F10.4)
 500    FORMAT('     Lower Wavenumber (cm-1)        =  ', F10.2)
 600    FORMAT('     Upper Wavenumber (cm-1)        =  ', F10.2)
 700    FORMAT(' */')

        DO I = 1, 16384
          WVNMBR = HXY(I,1)

          IF (WVNMBR.LT.WNLOW) THEN
C
C	  Terminate spectrum at long wavelength edge.
C
            CLOSE(20)
            RETURN
          END IF

          AINT = HXY(I,2)/1.E6
          WVLNGTH = 1.E8/WVNMBR
          WRITE(20, *) WVLNGTH, AINT
        END DO

	CLOSE(20)

	RETURN
	END
C
C --------------------------------------------------------------------
C
	SUBROUTINE WLINES
C
C	Write a line list file.
C
	CHARACTER*1 SIDP,SIDPP,SITP,SITPP
	DIMENSION HLEVEL(512),HQN(512,4),HF(2,16,16)
	DIMENSION HLINE(16384),HA(16384),HP(16384),HXY(16384,2)
	DIMENSION LQN(16384,2)
	COMMON HLEVEL,HQN,HF,HLINE,HA,HP,HXY,LQN,NX,NLEVELS,NLINES,
     1    EKT,WNLOW,WNHIGH,FWHM

	OPEN(UNIT=20,FILE='h2lines.dat',STATUS='NEW')

        DO I = 1, NLINES
C
C	Find the energy, wavelength, and qn's of each line.
C
          A = HA(I)
          WN = HLINE(I)
          WL = 1.E8/WN
          NPP = LQN(I,1)
          NP = LQN(I,2)
          IDP = HQN(NP,1)
          IDPP = HQN(NPP,1)
          IVP = HQN(NP,2)
          IVPP = HQN(NPP,2)
          JP = HQN(NP,3)
          JPP = HQN(NPP,3)
          ITP = HQN(NP,4)
          ITPP = HQN(NPP,4)
C
C	Label states with letters.
C	                    
          SITP = 'O'
          IF (ITP.EQ.0) SITP = 'P'
          SITPP = 'O'
          IF (ITPP.EQ.0) SITPP = 'P'
          SIDP = 'X'
          IF (IDP.EQ.1) SIDP = 'B'
          IF (IDP.EQ.2) SIDP = 'C'
          SIDPP = 'X'
          IF (IDPP.EQ.1) SIDPP = 'B'
          IF (IDPP.EQ.2) SIDPP = 'C'
C
C	Make an entry in the classified line file.
C
          WRITE(20,1000) I,WL,WN,A,SIDPP,IVPP,JPP,SITPP,SIDP,
     +      IVP,JP,SITP
 1000	FORMAT(1X,I4,2X,F8.3,1X,F10.3,1X,E10.4,3X,A1,1X,I2,1X,I2,1X,A1,2X,
     1	  A2,1X,I2,1X,I2,1X,A1)
        END DO

	CLOSE(20)

	RETURN
	END
C
C --------------------------------------------------------------------
C
