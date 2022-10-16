
module SOFA
      USE SOFAnutate

      implicit none

      contains

      SUBROUTINE iau_A2TF ( NDP, ANGLE, SIGN, IHMSF )
!*+
!*  - - - - - - - - -
!*   i a u _ A 2 T F
!*  - - - - - - - - -
!*
!*  Decompose radians into hours, minutes, seconds, fraction.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  vector/matrix support routine.
!*
!*  Given:
!*     NDP       i        resolution (Note 1)
!*     ANGLE     d        angle in radians
!*
!*  Returned:
!*     SIGN      c        '+' or '-'
!*     IHMSF     i(4)     hours, minutes, seconds, fraction
!*
!*  Called:
!*     iau_D2TF     decompose days to hms
!*
!*  Notes:
!*
!*  1) NDP is interpreted as follows:
!*
!*     NDP         resolution
!*      :      ...0000 00 00
!*     -7         1000 00 00
!*     -6          100 00 00
!*     -5           10 00 00
!*     -4            1 00 00
!*     -3            0 10 00
!*     -2            0 01 00
!*     -1            0 00 10
!*      0            0 00 01
!*      1            0 00 00.1
!*      2            0 00 00.01
!*      3            0 00 00.001
!*      :            0 00 00.000...
!*
!*  2) The largest useful value for NDP is determined by the size
!*     of ANGLE, the format of DOUBLE PRECISION floating-point numbers
!*     on the target platform, and the risk of overflowing IHMSF(4).
!*     On a typical platform, for ANGLE up to 2pi, the available
!*     floating-point precision might correspond to NDP=12.  However,
!*     the practical limit is typically NDP=9, set by the capacity of
!*     a 32-bit IHMSF(4).
!*
!*  3) The absolute value of ANGLE may exceed 2pi.  In cases where it
!*     does not, it is up to the caller to test for and handle the
!*     case where ANGLE is very nearly 2pi and rounds up to 24 hours,
!*     by testing for IHMSF(1)=24 and setting IHMSF(1-4) to zero.
!*
!*  This revision:  2000 November 25
!*
!*  SOFA release 2019-07-22
!*
!*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION ANGLE
      CHARACTER SIGN*(*)
      INTEGER IHMSF(4)

!*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Scale then use days to h,m,s routine.
      CALL iau_D2TF ( NDP, ANGLE/D2PI, SIGN, IHMSF )

!*  Finished.

!*+----------------------------------------------------------------------
      end

      SUBROUTINE iau_A2AF ( NDP, ANGLE, SIGN, IDMSF )
!*+
!*  - - - - - - - - -
!*   i a u _ A 2 A F
!*  - - - - - - - - -
!*
!*  Decompose radians into degrees, arcminutes, arcseconds, fraction.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  vector/matrix support routine.
!*
!*  Given:
!*     NDP       i        resolution (Note 1)
!*     ANGLE     d        angle in radians
!*
!*  Returned:
!*     SIGN      c        '+' or '-'
!*     IDMSF     i(4)     degrees, arcminutes, arcseconds, fraction
!*
!*  Called:
!*     iau_D2TF     decompose days to hms
!*
!*  Notes:
!*
!*  1) NDP is interpreted as follows:
!*
!*     NDP         resolution
!*      :      ...0000 00 00
!*     -7         1000 00 00
!*     -6          100 00 00
!*     -5           10 00 00
!*     -4            1 00 00
!*     -3            0 10 00
!*     -2            0 01 00
!*     -1            0 00 10
!*      0            0 00 01
!*      1            0 00 00.1
!*      2            0 00 00.01
!*      3            0 00 00.001
!*      :            0 00 00.000...
!*
!*  2) The largest positive useful value for NDP is determined by the
!*     size of ANGLE, the format of DOUBLE PRECISION floating-point
!*     numbers on the target platform, and the risk of overflowing
!*     IDMSF(4).  On a typical platform, for ANGLE up to 2pi, the
!*     available floating-point precision might correspond to NDP=12.
!*     However, the practical limit is typically NDP=9, set by the
!*     capacity of a 32-bit IDMSF(4).
!*
!*  3) The absolute value of ANGLE may exceed 2pi.  In cases where it
!*     does not, it is up to the caller to test for and handle the
!*     case where ANGLE is very nearly 2pi and rounds up to 360 degrees,
!*     by testing for IDMSF(1)=360 and setting IDMSF(1-4) to zero.
!*
!*  This revision:  2007 December 3
!*
!*  SOFA release 2019-07-22
!*
!*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION ANGLE
      CHARACTER SIGN*(*)
      INTEGER IDMSF(4)

!*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

!*  Hours to degrees * radians to turns
      DOUBLE PRECISION F
      PARAMETER ( F = 15D0/D2PI )

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Scale then use days to h,m,s routine.
      CALL iau_D2TF ( NDP, ANGLE*F, SIGN, IDMSF )

!*  Finished.

      end

!*+----------------------------------------------------------------------

      SUBROUTINE iau_D2TF ( NDP, DAYS, SIGN, IHMSF )
!*+
!*  - - - - - - - - -
!*   i a u _ D 2 T F
!*  - - - - - - - - -
!*
!*  Decompose days to hours, minutes, seconds, fraction.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  vector/matrix support routine.
!*
!*  Given:
!*     NDP       i        resolution (Note 1)
!*     DAYS      d        interval in days
!*
!*  Returned:
!*     SIGN      c        '+' or '-'
!*     IHMSF     i(4)     hours, minutes, seconds, fraction
!*
!*  Notes:
!*
!*  1) NDP is interpreted as follows:
!*
!*     NDP         resolution
!*      :      ...0000 00 00
!*     -7         1000 00 00
!*     -6          100 00 00
!*     -5           10 00 00
!*     -4            1 00 00
!*     -3            0 10 00
!*     -2            0 01 00
!*     -1            0 00 10
!*      0            0 00 01
!*      1            0 00 00.1
!*      2            0 00 00.01
!*      3            0 00 00.001
!*      :            0 00 00.000...
!*
!*  2) The largest positive useful value for NDP is determined by the
!*     size of DAYS, the format of DOUBLE PRECISION floating-point
!*     numbers on the target platform, and the risk of overflowing
!*     IHMSF(4).  On a typical platform, for DAYS up to 1D0, the
!*     available floating-point precision might correspond to NDP=12.
!*     However, the practical limit is typically NDP=9, set by the
!*     capacity of a 32-bit IHMSF(4).
!*
!*  3) The absolute value of DAYS may exceed 1D0.  In cases where it
!*     does not, it is up to the caller to test for and handle the
!*     case where DAYS is very nearly 1D0 and rounds up to 24 hours,
!*     by testing for IHMSF(1)=24 and setting IHMSF(1-4) to zero.
!*
!*  This revision:  2005 August 26
!*
!*  SOFA release 2019-07-22
!*
!*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION DAYS
      CHARACTER SIGN*(*)
      INTEGER IHMSF(4)

!*  Days to seconds
      DOUBLE PRECISION D2S
      PARAMETER ( D2S = 86400D0 )

      INTEGER NRS, N
      DOUBLE PRECISION RS, RM, RH, A, AH, AM, AS, AF

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Handle sign.
      IF ( DAYS .GE. 0D0 ) THEN
         SIGN = '+'
      ELSE
         SIGN = '-'
      END IF

!*  Interval in seconds.
      A = D2S * ABS(DAYS)

!*  Pre-round if resolution coarser than 1 second (then pretend NDP=1).
      IF ( NDP .LT. 0 ) THEN
         NRS = 1
         DO 1 N=1,-NDP
            IF ( N.EQ.2 .OR. N.EQ.4 ) THEN
               NRS = NRS * 6
            ELSE
               NRS = NRS * 10
            END IF
 1       CONTINUE
         RS = DBLE(NRS)
         A = RS * ANINT(A/RS)
      END IF

!*  Express the unit of each field in resolution units.
      NRS = 1
      DO 2 N=1,NDP
         NRS = NRS * 10
 2    CONTINUE
      RS = DBLE(NRS)
      RM = RS * 60D0
      RH = RM * 60D0

!*  Round the interval and express in resolution units.
      A = ANINT(RS*A)

!*  Break into fields.
      AH = AINT(A/RH)
      A = A - AH*RH
      AM = AINT(A/RM)
      A = A - AM*RM
      AS = AINT(A/RS)
      AF = A - AS*RS

!*  Return results.
      IHMSF(1) = NINT(AH)
      IHMSF(2) = NINT(AM)
      IHMSF(3) = NINT(AS)
      IHMSF(4) = NINT(AF)

!*  Finished.

      end
!*+----------------------------------------------------------------------

      SUBROUTINE iau_TAITT ( TAI1, TAI2, TT1, TT2, J )
!*+
!*  - - - - - - - - - -
!*   i a u _ T A I T T
!*  - - - - - - - - - -
!*
!*  Time scale transformation:  International Atomic Time, TAI, to
!*  Terrestrial Time, TT.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  canonical.
!*
!*  Given:
!*     TAI1,TAI2    d      TAI as a 2-part Julian Date
!*
!*  Returned:
!*     TT1,TT2      d      TT as a 2-part Julian Date
!*     J            i      status:  0 = OK
!*
!*  Note:
!*
!*     TAI1+TAI2 is Julian Date, apportioned in any convenient way
!*     between the two arguments, for example where TAI1 is the Julian
!*     Day Number and TAI2 is the fraction of a day.  The returned
!*     TT1,TT2 follow suit.
!*
!*  References:
!*
!*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!*     IERS Technical Note No. 32, BKG (2004)
!*
!*     Explanatory Supplement to the Astronomical Almanac,
!*     P. Kenneth Seidelmann (ed), University Science Books (1992)
!*
!*  This revision:  2010 April 16
!*
!*  SOFA release 2018-01-30
!*
!*  Copyright (C) 2018 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE
      DOUBLE PRECISION TAI1, TAI2, TT1, TT2
      INTEGER J

!*  TT minus TAI (days).
      DOUBLE PRECISION DTAT
      PARAMETER ( DTAT = 32.184D0/86400D0 )

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Result, safeguarding precision.
      IF ( TAI1.GT.TAI2 ) THEN
         TT1 = TAI1
         TT2 = TAI2 + DTAT
      ELSE
         TT1 = TAI1 + DTAT
         TT2 = TAI2
      END IF

!*  Status (always OK).
      J = 0

      end


      SUBROUTINE iau_D2DTF ( SCALE, NDP, D1, D2, IY, IM, ID, IHMSF, J )
!*+
!*  - - - - - - - - - -
!*   i a u _ D 2 D T F
!*  - - - - - - - - - -
!*
!*  Format for output a 2-part Julian Date (or in the case of UTC a
!*  quasi-JD form that includes special provision for leap seconds).
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  support routine.
!*
!*  Given:
!*     SCALE      c!*(*)  time scale ID (Note 1)
!*     NDP        i      resolution (Note 2)
!*     D1,D2      d      time as a 2-part Julian Date (Notes 3,4)
!*
!*  Returned:
!*     IY,IM,ID   i      year, month, day in Gregorian calendar (Note 5)
!*     IHMSF      i(4)   hours, minutes, seconds, fraction (Note 1)
!*     J          i      status: +1 = dubious year (Note 5)
!*                                0 = OK
!*                               -1 = unacceptable date (Note 6)
!*
!*  Notes:
!*
!*  1) SCALE identifies the time scale.  Only the value 'UTC' (in upper
!*     case) is significant, and enables handling of leap seconds (see
!*     Note 4).
!*
!*  2) NDP is the number of decimal places in the seconds field, and can
!*     have negative as well as positive values, such as:
!*
!*     NDP         resolution
!*     -4            1 00 00
!*     -3            0 10 00
!*     -2            0 01 00
!*     -1            0 00 10
!*      0            0 00 01
!*      1            0 00 00.1
!*      2            0 00 00.01
!*      3            0 00 00.001
!*
!*     The limits are platform dependent, but a safe range is -5 to +9.
!*
!*  3) D1+D2 is Julian Date, apportioned in any convenient way between
!*     the two arguments, for example where D1 is the Julian Day Number
!*     and D2 is the fraction of a day.  In the case of UTC, where the
!*     use of JD is problematical, special conventions apply:  see the
!*     next note.
!*
!*  4) JD cannot unambiguously represent UTC during a leap second unless
!*     special measures are taken.  The SOFA internal convention is that
!*     the quasi-JD day represents UTC days whether the length is 86399,
!*     86400 or 86401 SI seconds.  In the 1960-1972 era there were
!*     smaller jumps (in either direction) each time the linear UTC(TAI)
!*     expression was changed, and these "mini-leaps" are also included
!*     in the SOFA convention.
!*
!*  5) The warning status "dubious year" flags UTCs that predate the
!*     introduction of the time scale or that are too far in the future
!*     to be trusted.  See iau_DAT for further details.
!*
!*  6) For calendar conventions and limitations, see iau_CAL2JD.
!*
!*  Called:
!*     iau_JD2CAL   JD to Gregorian calendar
!*     iau_D2TF     decompose days to hms
!*     iau_DAT      delta(AT) = TAI-UTC
!*
!*  This revision:  2014 February 15
!*
!*  SOFA release 2018-01-30
!*
!*  Copyright (C) 2018 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE
      CHARACTER*(*) SCALE
      INTEGER NDP
      DOUBLE PRECISION D1, D2
      INTEGER IY, IM, ID, IHMSF(4), J

!*  Days to seconds
      DOUBLE PRECISION D2S
      PARAMETER ( D2S = 86400D0 )

      LOGICAL LEAP
      CHARACTER S
      INTEGER IY1, IM1, ID1, JS, IY2, IM2, ID2, IHMSF1(4), I
      DOUBLE PRECISION A1, B1, FD, DAT0, DAT12, W, DAT24, DLEAP

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  The two-part JD.
      A1 = D1
      B1 = D2

!*  Provisional calendar date.
      CALL iau_JD2CAL ( A1, B1, IY1, IM1, ID1, FD, JS )
      IF ( JS.NE.0 ) GO TO 9

!*  Is this a leap second day?
      LEAP = .FALSE.
      IF ( SCALE.EQ.'UTC' ) THEN

!*     TAI-UTC at 0h today.
         CALL iau_DAT ( IY1, IM1, ID1, 0D0, DAT0, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     TAI-UTC at 12h today (to detect drift).
         CALL iau_DAT ( IY1, IM1, ID1, 0.5D0, DAT12, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     TAI-UTC at 0h tomorrow (to detect jumps).
         CALL iau_JD2CAL ( A1+1.5D0, B1-FD, IY2, IM2, ID2, W, JS )
         IF ( JS.NE.0 ) GO TO 9
         CALL iau_DAT ( IY2, IM2, ID2, 0D0, DAT24, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     Any sudden change in TAI-UTC (seconds).
         DLEAP = DAT24 - ( 2D0 * DAT12 - DAT0 )

!*     If leap second day, scale the fraction of a day into SI.
         LEAP = DLEAP.NE.0D0
         IF ( LEAP ) FD = FD + FD*DLEAP/D2S

      END IF

!*  Provisional time of day.
      CALL iau_D2TF ( NDP, FD, S, IHMSF1 )

!*  Has the (rounded) time gone past 24h?
      IF ( IHMSF1(1).GT.23 ) THEN

!*     Yes.  We probably need tomorrow's calendar date.
         CALL iau_JD2CAL ( A1+1.5D0, B1-FD, IY2, IM2, ID2, W, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     Is today a leap second day?
         IF ( .NOT. LEAP ) THEN

!*        No.  Use 0h tomorrow.
            IY1 = IY2
            IM1 = IM2
            ID1 = ID2
            IHMSF1(1) = 0
            IHMSF1(2) = 0
            IHMSF1(3) = 0

         ELSE

!*        Yes.  Are we past the leap second itself?
            IF ( IHMSF1(3).GT.0 ) THEN

!*           Yes.  Use tomorrow but allow for the leap second.
               IY1 = IY2
               IM1 = IM2
               ID1 = ID2
               IHMSF1(1) = 0
               IHMSF1(2) = 0
               IHMSF1(3) = 0

            ELSE

!*           No.  Use 23 59 60... today.
               IHMSF1(1) = 23
               IHMSF1(2) = 59
               IHMSF1(3) = 60
            END IF

!*        If rounding to 10s or coarser always go up to new day.
            IF ( NDP.LT.0 .AND. IHMSF1(3).EQ.60 ) THEN
               IY1 = IY2
               IM1 = IM2
               ID1 = ID2
               IHMSF1(1) = 0
               IHMSF1(2) = 0
               IHMSF1(3) = 0
            END IF
         END IF
      END IF

!*  Results.
      IY = IY1
      IM = IM1
      ID = ID1
      DO 2 I=1,4
         IHMSF(I) = IHMSF1(I)
 2    CONTINUE

!*  Status.
 9    CONTINUE
      J = JS

      end

      SUBROUTINE iau_JD2CAL ( DJ1, DJ2, IY, IM, ID, FD, J )
!*+
!*  - - - - - - - - - - -
!*   i a u _ J D 2 C A L
!*  - - - - - - - - - - -
!*
!*  Julian Date to Gregorian year, month, day, and fraction of a day.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  support routine.
!*
!*  Given:
!*     DJ1,DJ2     d     Julian Date (Notes 1, 2)
!*
!*  Returned:
!*     IY          i     year
!*     IM          i     month
!*     ID          i     day
!*     FD          d     fraction of day
!*     J           i     status:
!*                           0 = OK
!*                          -1 = unacceptable date (Note 1)
!*
!*  Notes:
!*
!*  1) The earliest valid date is -68569.5 (-4900 March 1).  The
!*     largest value accepted is 10^9.
!*
!*  2) The Julian Date is apportioned in any convenient way between
!*     the arguments DJ1 and DJ2.  For example, JD=2450123.7 could
!*     be expressed in any of these ways, among others:
!*
!*             DJ1            DJ2
!*
!*         2450123.7D0        0D0        (JD method)
!*          2451545D0      -1421.3D0     (J2000 method)
!*         2400000.5D0     50123.2D0     (MJD method)
!*         2450123.5D0       0.2D0       (date & time method)
!*
!*  3) In early eras the conversion is from the "Proleptic Gregorian
!*     Calendar";  no account is taken of the date(s) of adoption of
!*     the Gregorian Calendar, nor is the AD/BC numbering convention
!*     observed.
!*
!*  Reference:
!*
!*     Explanatory Supplement to the Astronomical Almanac,
!*     P. Kenneth Seidelmann (ed), University Science Books (1992),
!*     Section 12.92 (p604).
!*
!*  This revision:  2017 January 12
!*
!*  SOFA release 2018-01-30
!*
!*  Copyright (C) 2018 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DJ1, DJ2
      INTEGER IY, IM, ID
      DOUBLE PRECISION FD
      INTEGER J

!*  Minimum and maximum allowed JD
      DOUBLE PRECISION DJMIN, DJMAX
      PARAMETER ( DJMIN = -68569.5D0, DJMAX = 1D9 )

      INTEGER JD, L, N, I, K
      DOUBLE PRECISION DJ, D1, D2, F1, F2, F, D

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Check if date is acceptable.
      DJ = DJ1 + DJ2
      IF ( DJ.LT.DJMIN .OR. DJ.GT.DJMAX ) THEN
         J = -1
      ELSE
         J = 0

!*     Copy the date, big then small, and re-align to midnight.
         IF ( DJ1 .GE. DJ2 ) THEN
            D1 = DJ1
            D2 = DJ2
         ELSE
            D1 = DJ2
            D2 = DJ1
         END IF
         D2 = D2 - 0.5D0

!*     Separate day and fraction.
         F1 = MOD(D1,1D0)
         F2 = MOD(D2,1D0)
         F = MOD(F1+F2,1D0)
         IF ( F .LT. 0D0 ) F = F+1D0
         D = ANINT(D1-F1) + ANINT(D2-F2) + ANINT(F1+F2-F)
         JD = NINT(D) + 1

!*     Express day in Gregorian calendar.
         L = JD + 68569
         N = ( 4*L ) / 146097
         L = L - ( 146097*N + 3 ) / 4
         I = ( 4000 * (L+1) ) / 1461001
         L = L - ( 1461*I ) / 4 + 31
         K = ( 80*L ) / 2447
         ID = L - ( 2447*K ) / 80
         L = K / 11
         IM = K + 2 - 12*L
         IY = 100 * ( N-49 ) + I + L

         FD = F
      END IF

      end

      SUBROUTINE iau_DAT ( IY, IM, ID, FD, DELTAT, J )
!*+
!*  - - - - - - - -
!*   i a u _ D A T
!*  - - - - - - - -
!*
!*  For a given UTC date, calculate Delta(AT) = TAI-UTC.
!*
!*     :------------------------------------------:
!*     :                                          :
!*     :                 IMPORTANT                :
!*     :                                          :
!*     :  A new version of this routine must be   :
!*     :  produced whenever a new leap second is  :
!*     :  announced.  There are five items to     :
!*     :  change on each such occasion:           :
!*     :                                          :
!*     :  1) The parameter NDAT must be           :
!*     :     increased by 1.                      :
!*     :                                          :
!*     :  2) The set of DATA statements that      :
!*     :     initialize the arrays IDAT and       :
!*     :     DATS must be extended by one line.   :
!*     :                                          :
!*     :  3) The parameter IYV must be set to     :
!*     :     the current year.                    :
!*     :                                          :
!*     :  4) The "Latest leap second" comment     :
!*     :     below must be set to the new leap    :
!*     :     second date.                         :
!*     :                                          :
!*     :  5) The "This revision" comment, later,  :
!*     :     must be set to the current date.     :
!*     :                                          :
!*     :  Change (3) must also be carried out     :
!*     :  whenever the routine is re-issued,      :
!*     :  even if no leap seconds have been       :
!*     :  added.                                  :
!*     :                                          :
!*     :  Latest leap second:  2016 December 31   :
!*     :                                          :
!*     :__________________________________________:
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  user-replaceable support routine.
!*
!*  Given:
!*     IY       i     UTC:  year (Notes 1 and 2)
!*     IM       i           month (Note 2)
!*     ID       i           day (Notes 2 and 3)
!*     FD       d           fraction of day (Note 4)
!*
!*  Returned:
!*     DELTAT   d     TAI minus UTC, seconds
!*     J        i     status (Note 5):
!*                       1 = dubious year (Note 1)
!*                       0 = OK
!*                      -1 = bad year
!*                      -2 = bad month
!*                      -3 = bad day (Note 3)
!*                      -4 = bad fraction (Note 4)
!*                      -5 = internal error (Note 5)
!*
!*  Notes:
!*
!*  1) UTC began at 1960 January 1.0 (JD 2436934.5) and it is improper
!*     to call the routine with an earlier date.  If this is attempted,
!*     zero is returned together with a warning status.
!*
!*     Because leap seconds cannot, in principle, be predicted in
!*     advance, a reliable check for dates beyond the valid range is
!*     impossible.  To guard against gross errors, a year five or more
!*     after the release year of the present routine (see parameter IYV)
!*     is considered dubious.  In this case a warning status is returned
!*     but the result is computed in the normal way.
!*
!*     For both too-early and too-late years, the warning status is J=+1.
!*     This is distinct from the error status J=-1, which signifies a
!*     year so early that JD could not be computed.
!*
!*  2) If the specified date is for a day which ends with a leap second,
!*     the TAI-UTC value returned is for the period leading up to the
!*     leap second.  If the date is for a day which begins as a leap
!*     second ends, the TAI-UTC returned is for the period following the
!*     leap second.
!*
!*  3) The day number must be in the normal calendar range, for example
!*     1 through 30 for April.  The "almanac" convention of allowing
!*     such dates as January 0 and December 32 is not supported in this
!*     routine, in order to avoid confusion near leap seconds.
!*
!*  4) The fraction of day is used only for dates before the introduction
!*     of leap seconds, the first of which occurred at the end of 1971.
!*     It is tested for validity (0 to 1 is the valid range) even if not
!*     used;  if invalid, zero is used and status J=-4 is returned.  For
!*     many applications, setting FD to zero is acceptable;  the
!*     resulting error is always less than 3 ms (and occurs only
!*     pre-1972).
!*
!*  5) The status value returned in the case where there are multiple
!*     errors refers to the first error detected.  For example, if the
!*     month and day are 13 and 32 respectively, J=-2 (bad month) will be
!*     returned.  The "internal error" status refers to a case that is
!*     impossible but causes some compilers to issue a warning.
!*
!*  6) In cases where a valid result is not available, zero is returned.
!*
!*  References:
!*
!*  1) For dates from 1961 January 1 onwards, the expressions from the
!*     file ftp://maia.usno.navy.mil/ser7/tai-utc.dat are used.
!*
!*  2) The 5ms timestep at 1961 January 1 is taken from 2.58.1 (p87) of
!*     the 1992 Explanatory Supplement.
!*
!*  Called:
!*     iau_CAL2JD   Gregorian calendar to JD
!*
!*  This revision:  2020 May 31
!*
!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IY, IM, ID
      DOUBLE PRECISION FD, DELTAT
      INTEGER J

!*  Release year for this version of iau_DAT
      INTEGER IYV
      PARAMETER ( IYV = 2020 )

!*  Number of Delta(AT) changes (increase by 1 for each new leap second)
      INTEGER NDAT
      PARAMETER ( NDAT = 42 )

!*  Number of Delta(AT) expressions before leap seconds were introduced
      INTEGER NERA1
      PARAMETER ( NERA1 = 14 )

!*  Dates (year, month) on which new Delta(AT) came into force
      INTEGER IDAT(2,NDAT)

!*  New Delta(AT) which came into force on the given dates
      DOUBLE PRECISION DATS(NDAT)

!*  Reference dates (MJD) and drift rates (s/day), pre leap seconds
      DOUBLE PRECISION DRIFT(2,NERA1)

!*  Miscellaneous local variables
      LOGICAL MORE
      INTEGER JS, M, N, IS
      DOUBLE PRECISION DA, DJM0, DJM

!*  Dates, Delta(AT)s, reference dates, and drift rates
      DATA ((IDAT(M,N),M=1,2),DATS(N),(DRIFT(M,N),M=1,2),N=1,14)&
     &  / 1960,  1,  1.4178180D0, 37300D0, 0.001296D0,      &
     &    1961,  1,  1.4228180D0, 37300D0, 0.001296D0,      &
     &    1961,  8,  1.3728180D0, 37300D0, 0.001296D0,      &
     &    1962,  1,  1.8458580D0, 37665D0, 0.0011232D0,     &
     &    1963, 11,  1.9458580D0, 37665D0, 0.0011232D0,     &
     &    1964,  1,  3.2401300D0, 38761D0, 0.001296D0,      &
     &    1964,  4,  3.3401300D0, 38761D0, 0.001296D0,      &
     &    1964,  9,  3.4401300D0, 38761D0, 0.001296D0,      &
     &    1965,  1,  3.5401300D0, 38761D0, 0.001296D0,      &
     &    1965,  3,  3.6401300D0, 38761D0, 0.001296D0,      &
     &    1965,  7,  3.7401300D0, 38761D0, 0.001296D0,      &
     &    1965,  9,  3.8401300D0, 38761D0, 0.001296D0,      &
     &    1966,  1,  4.3131700D0, 39126D0, 0.002592D0,      &
     &    1968,  2,  4.2131700D0, 39126D0, 0.002592D0 /

!*  Dates and Delta(AT)s
      DATA ((IDAT(M,N),M=1,2),DATS(N),N=15,30)&
     &  / 1972,  1, 10D0,     &
     &    1972,  7, 11D0,     &
     &    1973,  1, 12D0,     &
     &    1974,  1, 13D0,     &
     &    1975,  1, 14D0,     &
     &    1976,  1, 15D0,     &
     &    1977,  1, 16D0,     &
     &    1978,  1, 17D0,     &
     &    1979,  1, 18D0,     &
     &    1980,  1, 19D0,     &
     &    1981,  7, 20D0,     &
     &    1982,  7, 21D0,     &
     &    1983,  7, 22D0,     &
     &    1985,  7, 23D0,     &
     &    1988,  1, 24D0,     &
     &    1990,  1, 25D0 /

      DATA ((IDAT(M,N),M=1,2),DATS(N),N=31,NDAT)&
     &  / 1991,  1, 26D0,     &
     &    1992,  7, 27D0,     &
     &    1993,  7, 28D0,     &
     &    1994,  7, 29D0,     &
     &    1996,  1, 30D0,     &
     &    1997,  7, 31D0,     &
     &    1999,  1, 32D0,     &
     &    2006,  1, 33D0,     &
     &    2009,  1, 34D0,     &
     &    2012,  7, 35D0,     &
     &    2015,  7, 36D0,     &
     &    2017,  1, 37D0 /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Initialize the result to zero and the status to OK.
      DA = 0D0
      JS = 0

!*  If invalid fraction of a day, set error status and give up.
      IF ( FD.LT.0D0 .OR. FD.GT.1D0 ) THEN
         JS = -4
         GO TO 9000
      END IF

!*  Convert the date into an MJD.
      CALL iau_CAL2JD ( IY, IM, ID, DJM0, DJM, JS )

!*  If invalid year, month, or day, give up.
      IF ( JS .LT. 0 ) GO TO 9000

!*  If pre-UTC year, set warning status and give up.
      IF ( IY .LT. IDAT(1,1) ) THEN
         JS = 1
         GO TO 9000
      END IF

!*  If suspiciously late year, set warning status but proceed.
      IF ( IY .GT. IYV+5 ) JS = 1

!*  Combine year and month.
      M = 12*IY+IM

!*  Find the most recent table entry.
      IS = 0
      MORE = .TRUE.
      DO 1 N=NDAT,1,-1
         IF ( MORE ) THEN
            IS = N
            MORE = M .LT. ( 12*IDAT(1,N) + IDAT(2,N) )
         END IF
 1    CONTINUE

!*  Prevent underflow warnings.
      IF ( IS .LT. 1 ) THEN
         JS = -5
         GO TO 9000
      END IF

!*  Get the Delta(AT).
      DA = DATS(IS)

!*  If pre-1972, adjust for drift.
      IF ( IS .LE. NERA1 ) DA = DA +( DJM + FD - DRIFT(1,IS) ) * DRIFT(2,IS)

!*  Return the Delta(AT) value and the status.
 9000 CONTINUE
      DELTAT = DA
      J = JS

!*  Finished.

!*+----------------------------------------------------------------------

      END

!*+----------------------------------------------------------------------

      SUBROUTINE iau_CAL2JD ( IY, IM, ID, DJM0, DJM, J )
!*+
!*  - - - - - - - - - - -
!*   i a u _ C A L 2 J D
!*  - - - - - - - - - - -
!*
!*  Gregorian Calendar to Julian Date.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  support routine.
!*
!*  Given:
!*     IY,IM,ID    i     year, month, day in Gregorian calendar (Note 1)
!*
!*  Returned:
!*     DJM0        d     MJD zero-point: always 2400000.5
!*     DJM         d     Modified Julian Date for 0 hrs
!*     J           i     status:
!*                           0 = OK

!!*                          -1 = bad year   (Note 3: JD not computed)
!*                          -2 = bad month  (JD not computed)
!*                          -3 = bad day    (JD computed)
!*
!*  Notes:
!*
!*  1) The algorithm used is valid from -4800 March 1, but this
!*     implementation rejects dates before -4799 January 1.
!*
!*  2) The Julian Date is returned in two pieces, in the usual SOFA
!*     manner, which is designed to preserve time resolution.  The
!*     Julian Date is available as a single number by adding DJM0 and
!*     DJM.
!*
!*  3) In early eras the conversion is from the "Proleptic Gregorian
!*     Calendar";  no account is taken of the date(s) of adoption of
!*     the Gregorian Calendar, nor is the AD/BC numbering convention
!*     observed.
!*
!*  Reference:
!*
!*     Explanatory Supplement to the Astronomical Almanac,
!*     P. Kenneth Seidelmann (ed), University Science Books (1992),
!*     Section 12.92 (p604).
!*
!*  This revision:  2014 November 7
!*
!*  SOFA release 2018-01-30
!*
!*  Copyright (C) 2018 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IY, IM, ID
      DOUBLE PRECISION DJM0, DJM

      INTEGER J, NDAYS, MY, IYPMY

!*  Earliest year allowed (4800BC)
      INTEGER IYMIN
      PARAMETER ( IYMIN = -4799 )

!*  Month lengths in days
      INTEGER MTAB(12)
      DATA MTAB / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Preset status.
      J = 0

!*  Validate year.
      IF ( IY.LT.IYMIN ) THEN
         J = -1
      ELSE

!*     Validate month.
         IF ( IM.GE.1 .AND. IM.LE.12 ) THEN

!*        Days in current month.
            NDAYS = MTAB(IM)

!*        Allow for leap year.
            IF ( IM .EQ. 2 ) THEN
               IF ( MOD(IY,4) .EQ. 0 ) NDAYS = 29
               IF ( MOD(IY,100).EQ.0 .AND. MOD(IY,400).NE.0 ) NDAYS = 28
            END IF

!*        Validate day.
            IF ( ID.LT.1 .OR. ID.GT.NDAYS ) J = -3

!*        Result.
            MY = ( IM - 14 ) / 12
            IYPMY = IY + MY
            DJM0 = 2400000.5D0
            DJM = DBLE( ( 1461 * ( IYPMY + 4800 ) ) / 4&
                     + (  367 * ( IM-2 - 12*MY ) ) / 12&
                     - (    3 * ( ( IYPMY + 4900 ) / 100 ) ) / 4&
                     + ID - 2432076)

!*        Bad month
         ELSE
            J = -2
         END IF
      END IF
      END


      SUBROUTINE iau_DTF2D ( SCALE, IY, IM, ID, IHR, IMN, SEC, D1, D2, J )
!*+
!*  - - - - - - - - - -
!*   i a u _ D T F 2 D
!*  - - - - - - - - - -
!*
!*  Encode date and time fields into 2-part Julian Date (or in the case
!*  of UTC a quasi-JD form that includes special provision for leap
!*  seconds).
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  support routine.
!*
!*  Given:
!*     SCALE      c!*(*)  time scale ID (Note 1)
!*     IY,IM,ID   i      year, month, day in Gregorian calendar (Note 2)
!*     IHR,IMN    i      hour, minute
!*     SEC        d      seconds
!*
!*  Returned:
!*     D1,D2      d      2-part Julian Date (Notes 3,4)
!*     J          i      status: +3 = both of next two
!*                               +2 = time is after end of day (Note 5)
!*                               +1 = dubious year (Note 6)
!*                                0 = OK
!*                               -1 = bad year
!*                               -2 = bad month
!*                               -3 = bad day
!*                               -4 = bad hour
!*                               -5 = bad minute
!*                               -6 = bad second (<0)
!*
!*  Notes:
!*
!*  1) SCALE identifies the time scale.  Only the value 'UTC' (in upper
!*     case) is significant, and enables handling of leap seconds (see
!*     Note 4).
!*
!*  2) For calendar conventions and limitations, see iau_CAL2JD.
!*
!*  3) The sum of the results, D1+D2, is Julian Date, where normally D1
!*     is the Julian Day Number and D2 is the fraction of a day.  In the
!*     case of UTC, where the use of JD is problematical, special
!*     conventions apply:  see the next note.
!*
!*  4) JD cannot unambiguously represent UTC during a leap second unless
!*     special measures are taken.  The SOFA internal convention is that
!*     the quasi-JD day represents UTC days whether the length is 86399,
!*     86400 or 86401 SI seconds.  In the 1960-1972 era there were
!*     smaller jumps (in either direction) each time the linear UTC(TAI)
!*     expression was changed, and these "mini-leaps" are also included
!*     in the SOFA convention.
!*
!*  5) The warning status "time is after end of day" usually means that
!*     the SEC argument is greater than 60D0.  However, in a day ending
!*     in a leap second the limit changes to 61D0 (or 59D0 in the case of
!*     a negative leap second).
!*
!*  6) The warning status "dubious year" flags UTCs that predate the
!*     introduction of the time scale or that are too far in the future
!*     to be trusted.  See iau_DAT for further details.
!*
!*  7) Only in the case of continuous and regular time scales (TAI, TT,
!*     TCG, TCB and TDB) is the result D1+D2 a Julian Date, strictly
!*     speaking.  In the other cases (UT1 and UTC) the result must be
!*     used with circumspection;  in particular the difference between
!*     two such results cannot be interpreted as a precise time
!*     interval.
!*
!*  Called:
!*     iau_CAL2JD   Gregorian calendar to JD
!*     iau_DAT      delta(AT) = TAI-UTC
!*     iau_JD2CAL   JD to Gregorian calendar
!*
!*  This revision:  2013 July 26
!*
!*  SOFA release 2019-07-22
!*
!*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE
      CHARACTER*(*) SCALE
      INTEGER IY, IM, ID, IHR, IMN
      DOUBLE PRECISION SEC, D1, D2
      INTEGER J

!*  Days to seconds
      DOUBLE PRECISION D2S
      PARAMETER ( D2S = 86400D0 )

      INTEGER JS, IY2, IM2, ID2
      DOUBLE PRECISION DJ, W, DAY, SECLIM, DAT0, DAT12, DAT24,DLEAP, TIME

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Today's Julian Day Number.
      CALL iau_CAL2JD ( IY, IM, ID, DJ, W, JS )
      IF ( JS.NE.0 ) GO TO 9
      DJ = DJ + W

!*  Day length and final minute length in seconds (provisional).
      DAY = D2S
      SECLIM = 60D0

!*  Deal with the UTC leap second case.
      IF ( SCALE.EQ.'UTC' ) THEN

!*     TAI-UTC at 0h today.
         CALL iau_DAT ( IY, IM, ID, 0D0, DAT0, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     TAI-UTC at 12h today (to detect drift).
         CALL iau_DAT ( IY, IM, ID, 0.5D0, DAT12, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     TAI-UTC at 0h tomorrow (to detect jumps).
         CALL iau_JD2CAL ( DJ, 1.5D0, IY2, IM2, ID2, W, JS )
         IF ( JS.NE.0 ) GO TO 9
         CALL iau_DAT ( IY2, IM2, ID2, 0D0, DAT24, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     Any sudden change in TAI-UTC between today and tomorrow.
         DLEAP = DAT24 - ( 2D0 * DAT12 - DAT0 )

!*     If leap second day, correct the day and final minute lengths.
         DAY = DAY + DLEAP
         IF ( IHR.EQ.23 .AND. IMN.EQ.59 ) SECLIM = SECLIM + DLEAP

!*     End of UTC-specific actions.
      END IF

!*  Validate the time.
      IF ( IHR.GE.0 .AND. IHR.LE.23 ) THEN
         IF ( IMN.GE.0 .AND. IMN.LE.59 ) THEN
            IF ( SEC.GE.0D0 ) THEN
               IF ( SEC.GE.SECLIM ) THEN
                  JS = JS + 2
               END IF
            ELSE
               JS = -6
            END IF
         ELSE
            JS = -5
         END IF
      ELSE
         JS = -4
      END IF
      IF ( JS.LT.0 ) GO TO 9

!*  The time in days.
      TIME = (60D0*DBLE(60*IHR+IMN)+SEC) / DAY

!*  Return the date and time.
      D1 = DJ
      D2 = TIME

!*  Return the status.
 9    CONTINUE
      J = JS

!*  Finished.
      end

      DOUBLE PRECISION FUNCTION iau_GST00A ( UTA, UTB, TTA, TTB )
!*+
!*  - - - - - - - - - - -
!*   i a u _ G S T 0 0 A
!*  - - - - - - - - - - -
!*
!*  Greenwich Apparent Sidereal Time (consistent with IAU 2000
!*  resolutions).
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  canonical model.
!*
!*  Given:
!*     UTA, UTB     d      UT1 as a 2-part Julian Date (Notes 1,2)
!*     TTA, TTB     d      TT as a 2-part Julian Date (Notes 1,2)
!*
!*  Returned:
!*     iau_GST00A   d      Greenwich apparent sidereal time (radians)
!*
!*  Notes:
!*
!*  1) The UT1 and TT dates UTA+UTB and TTA+TTB respectively, are both
!*     Julian Dates, apportioned in any convenient way between the
!*     argument pairs.  For example, JD=2450123.7 could be expressed in
!*     any of these ways, among others:
!*
!*            Part A         Part B
!*
!*         2450123.7D0        0D0        (JD method)
!*          2451545D0      -1421.3D0     (J2000 method)
!*         2400000.5D0     50123.2D0     (MJD method)
!*         2450123.5D0       0.2D0       (date & time method)
!*
!*     The JD method is the most natural and convenient to use in
!*     cases where the loss of several decimal digits of resolution
!*     is acceptable (in the case of UT;  the TT is not at all critical
!*     in this respect).  The J2000 and MJD methods are good compromises
!*     between resolution and convenience.  For UT, the date & time
!*     method is best matched to the algorithm that is used by the Earth
!*     Rotation Angle routine, called internally:  maximum accuracy (or,
!*     at least, minimum noise) is delivered when the UTA argument is for
!*     0hrs UT1 on the day in question and the UTB argument lies in the
!*     range 0 to 1, or vice versa.
!*
!*  2) Both UT1 and TT are required, UT1 to predict the Earth rotation
!*     and TT to predict the effects of precession-nutation.  If UT1 is
!*     used for both purposes, errors of order 100 microarcseconds
!*     result.
!*
!*  3) This GAST is compatible with the IAU 2000 resolutions and must be
!*     used only in conjunction with other IAU 2000 compatible components
!*     such as precession-nutation.
!*
!*  4) The result is returned in the range 0 to 2pi.
!*
!*  5) The algorithm is from Capitaine et al. (2003) and IERS Conventions
!*     2003.
!*
!*  Called:
!*     iau_GMST00   Greenwich mean sidereal time, IAU 2000
!*     iau_EE00A    equation of the equinoxes, IAU 2000A
!*     iau_ANP      normalize angle into range 0 to 2pi
!*
!*  References:
!*
!*     Capitaine, N., Wallace, P.T. and McCarthy, D.D., "Expressions to
!*     implement the IAU 2000 definition of UT1", Astronomy &
!*     Astrophysics, 406, 1135-1149 (2003)
!*
!*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!*     IERS Technical Note No. 32, BKG (2004)
!*
!*  This revision:  2007 December 8
!*
!*  SOFA release 2020-07-21
!*
!*  Copyright (C) 2020 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION UTA, UTB, TTA, TTB

!      DOUBLE PRECISION iau_ANP, iau_GMST00, iau_EE00A

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      iau_GST00A = iau_ANP ( iau_GMST00 ( UTA,UTB, TTA,TTB ) + iau_EE00A ( TTA,TTB ) )

!*  Finished.

!*+----------------------------------------------------------------------

      END


      DOUBLE PRECISION FUNCTION iau_GST00B ( UTA, UTB )
!*+
!*  - - - - - - - - - - -
!*   i a u _ G S T 0 0 B
!*  - - - - - - - - - - -
!*
!*  Greenwich Apparent Sidereal Time (consistent with IAU 2000
!*  resolutions but using the truncated nutation model IAU 2000B).
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  support routine.
!*
!*  Given:
!*     UTA,UTB      d      UT1 as a 2-part Julian Date (Notes 1,2)
!*
!*  Returned:
!*     iau_GST00B   d      Greenwich apparent sidereal time (radians)
!*
!*  Notes:
!*
!*  1) The UT1 date UTA+UTB is a Julian Date, apportioned in any
!*     convenient way between the argument pair.  For example,
!*     JD(UT1)=2450123.7 could be expressed in any of these ways, among
!*     others:
!*
!*             UTA            UTB
!*
!*         2450123.7D0        0D0        (JD method)
!*          2451545D0      -1421.3D0     (J2000 method)
!*         2400000.5D0     50123.2D0     (MJD method)
!*         2450123.5D0       0.2D0       (date & time method)
!*
!*     The JD method is the most natural and convenient to use in cases
!*     where the loss of several decimal digits of resolution is
!*     acceptable.  The J2000 and MJD methods are good compromises
!*     between resolution and convenience.  For UT, the date & time
!*     method is best matched to the algorithm that is used by the Earth
!*     Rotation Angle routine, called internally:  maximum accuracy (or,
!*     at least, minimum noise) is delivered when the UTA argument is for
!*     0hrs UT1 on the day in question and the UTB argument lies in the
!*     range 0 to 1, or vice versa.
!*
!*  2) The result is compatible with the IAU 2000 resolutions, except
!*     that accuracy has been compromised for the sake of speed and
!*     convenience in two respects:
!*
!*     . UT is used instead of TDB (or TT) to compute the precession
!*       component of GMST and the equation of the equinoxes.  This
!*       results in errors of order 0.1 mas at present.
!*
!*     . The IAU 2000B abridged nutation model (McCarthy & Luzum, 2003)
!*       is used, introducing errors of up to 1 mas.
!*
!*  3) This GAST is compatible with the IAU 2000 resolutions and must be
!*     used only in conjunction with other IAU 2000 compatible components
!*     such as precession-nutation.
!*
!*  4) The result is returned in the range 0 to 2pi.
!*
!*  5) The algorithm is from Capitaine et al. (2003) and IERS Conventions
!*     2003.
!*
!*  Called:
!*     iau_GMST00   Greenwich mean sidereal time, IAU 2000
!*     iau_EE00B    equation of the equinoxes, IAU 2000B
!*     iau_ANP      normalize angle into range 0 to 2pi
!*
!*  References:
!*
!*     Capitaine, N., Wallace, P.T. and McCarthy, D.D., "Expressions to
!*     implement the IAU 2000 definition of UT1", Astronomy &
!*     Astrophysics, 406, 1135-1149 (2003)
!*
!*     McCarthy, D.D. & Luzum, B.J., "An abridged model of the
!*     precession-nutation of the celestial pole", Celestial Mechanics &
!*     Dynamical Astronomy, 85, 37-49 (2003)
!*
!*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!*     IERS Technical Note No. 32, BKG (2004)
!*
!*  This revision:  2020 November 19
!*
!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION UTA, UTB

!      DOUBLE PRECISION iau_ANP, iau_GMST00, iau_EE00B

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      iau_GST00B = iau_ANP ( iau_GMST00 ( UTA,UTB, UTA,UTB ) +  iau_EE00B ( UTA,UTB ) )

!*  Finished.

!*+----------------------------------------------------------------------

      END

      SUBROUTINE iau_UTCTAI ( UTC1, UTC2, TAI1, TAI2, J )
!*+
!*  - - - - - - - - - - -
!*   i a u _ U T C T A I
!*  - - - - - - - - - - -
!*
!*  Time scale transformation:  Coordinated Universal Time, UTC, to
!*  International Atomic Time, TAI.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  canonical.
!*
!*  Given:
!*     UTC1,UTC2    d      UTC as a 2-part quasi Julian Date (Notes 1-4)
!*
!*  Returned:
!*     TAI1,TAI2    d      TAI as a 2-part Julian Date (Note 5)
!*     J            i      status: +1 = dubious year (Note 3)
!*                                  0 = OK
!*                                 -1 = unacceptable date
!*
!*  Notes:
!*
!*  1) UTC1+UTC2 is quasi Julian Date (see Note 2), apportioned in any
!*     convenient way between the two arguments, for example where UTC1
!*     is the Julian Day Number and UTC2 is the fraction of a day.
!*
!*  2) JD cannot unambiguously represent UTC during a leap second unless
!*     special measures are taken.  The convention in the present routine
!*     is that the JD day represents UTC days whether the length is
!*     86399, 86400 or 86401 SI seconds.  In the 1960-1972 era there were
!*     smaller jumps (in either direction) each time the linear UTC(TAI)
!*     expression was changed, and these "mini-leaps" are also included
!*     in the SOFA convention.
!*
!*  3) The warning status "dubious year" flags UTCs that predate the
!*     introduction of the time scale or that are too far in the future
!*     to be trusted.  See iau_DAT for further details.
!*
!*  4) The routine iau_DTF2D converts from calendar date and time of day
!*     into 2-part Julian Date, and in the case of UTC implements the
!*     leap-second-ambiguity convention described above.
!*
!*  5) The returned TAI1,TAI2 are such that their sum is the TAI Julian
!*     Date.
!*
!*  Called:
!*     iau_JD2CAL   JD to Gregorian calendar
!*     iau_DAT      delta(AT) = TAI-UTC
!*     iau_CAL2JD   Gregorian calendar to JD
!*
!*  References:
!*
!*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!*     IERS Technical Note No. 32, BKG (2004)
!*
!*     Explanatory Supplement to the Astronomical Almanac,
!*     P. Kenneth Seidelmann (ed), University Science Books (1992)
!*
!*  This revision:  2019 June 20
!*
!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE
      DOUBLE PRECISION UTC1, UTC2, TAI1, TAI2
      INTEGER J

!*  Days to seconds
      DOUBLE PRECISION D2S
      PARAMETER ( D2S = 86400D0 )

      LOGICAL BIG1
      INTEGER IY, IM, ID, JS, IYT, IMT, IDT
      DOUBLE PRECISION U1, U2, FD, DAT0, DAT12, W, DAT24, DLOD, DLEAP,Z1, Z2, A2

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Put the two parts of the UTC into big-first order.
      BIG1 = ABS(UTC1) .GE. ABS(UTC2)
      IF ( BIG1 ) THEN
         U1 = UTC1
         U2 = UTC2
      ELSE
         U1 = UTC2
         U2 = UTC1
      END IF

!*  Get TAI-UTC at 0h today.
      CALL iau_JD2CAL ( U1, U2, IY, IM, ID, FD, JS )
      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IY, IM, ID, 0D0, DAT0, JS )
      IF ( JS.LT.0 ) GO TO 9

!*  Get TAI-UTC at 12h today (to detect drift).
      CALL iau_DAT ( IY, IM, ID, 0.5D0, DAT12, JS )
      IF ( JS.LT.0 ) GO TO 9

!*  Get TAI-UTC at 0h tomorrow (to detect jumps).
      CALL iau_JD2CAL ( U1+1.5D0, U2-FD, IYT, IMT, IDT, W, JS )
      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IYT, IMT, IDT, 0D0, DAT24, JS )
      IF ( JS.LT.0 ) GO TO 9

!*  Separate TAI-UTC change into per-day (DLOD) and any jump (DLEAP).
      DLOD = 2D0 * ( DAT12 - DAT0 )
      DLEAP = DAT24 - ( DAT0 + DLOD )

!*  Remove any scaling applied to spread leap into preceding day.
      FD = FD * (D2S+DLEAP)/D2S

!*  Scale from (pre-1972) UTC seconds to SI seconds.
      FD = FD * (D2S+DLOD)/D2S

!*  Today's calendar date to 2-part JD.
      CALL iau_CAL2JD ( IY, IM, ID, Z1, Z2, JS )
      IF ( JS.NE.0 ) GO TO 9

!*  Assemble the TAI result, preserving the UTC split and order.
      A2 = Z1 - U1
      A2 = ( A2 + Z2 ) + ( FD + DAT0/D2S )
      IF ( BIG1 ) THEN
         TAI1 = U1
         TAI2 = A2
      ELSE
         TAI1 = A2
         TAI2 = U1
      END IF

!*  Status.
 9    CONTINUE
      J = JS

!*  Finished.

!*+----------------------------------------------------------------------

      END


      DOUBLE PRECISION FUNCTION iau_GMST00 ( UTA, UTB, TTA, TTB )
!*+
!*  - - - - - - - - - - -
!*   i a u _ G M S T 0 0
!*  - - - - - - - - - - -
!*
!*  Greenwich Mean Sidereal Time (model consistent with IAU 2000
!*  resolutions).
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  canonical model.
!*
!*  Given:
!*     UTA,UTB      d      UT1 as a 2-part Julian Date (Notes 1,2)
!*     TTA,TTB      d      TT as a 2-part Julian Date (Notes 1,2)
!*
!*  Returned:
!*     iau_GMST00   d      Greenwich mean sidereal time (radians)
!*
!*  Notes:
!*
!*  1) The UT1 and TT dates UTA+UTB and TTA+TTB respectively, are both
!*     Julian Dates, apportioned in any convenient way between the
!*     argument pairs.  For example, JD(UT1)=2450123.7 could be expressed
!*     in any of these ways, among others:
!*
!*            Part A         Part B
!*
!*         2450123.7D0        0D0        (JD method)
!*          2451545D0      -1421.3D0     (J2000 method)
!*         2400000.5D0     50123.2D0     (MJD method)
!*         2450123.5D0       0.2D0       (date & time method)
!*
!*     The JD method is the most natural and convenient to use in
!*     cases where the loss of several decimal digits of resolution
!*     is acceptable (in the case of UT;  the TT is not at all critical
!*     in this respect).  The J2000 and MJD methods are good compromises
!*     between resolution and convenience.  For UT, the date & time
!*     method is best matched to the algorithm that is used by the Earth
!*     Rotation Angle routine, called internally:  maximum accuracy (or,
!*     at least, minimum noise) is delivered when the UTA argument is for
!*     0hrs UT1 on the day in question and the UTB argument lies in the
!*     range 0 to 1, or vice versa.
!*
!*  2) Both UT1 and TT are required, UT1 to predict the Earth rotation
!*     and TT to predict the effects of precession.  If UT1 is used for
!*     both purposes, errors of order 100 microarcseconds result.
!*
!*  3) This GMST is compatible with the IAU 2000 resolutions and must be
!*     used only in conjunction with other IAU 2000 compatible components
!*     such as precession-nutation and equation of the equinoxes.
!*
!*  4) The result is returned in the range 0 to 2pi.
!
!*  5) The algorithm is from Capitaine et al. (2003) and IERS Conventions
!*     2003.
!*
!*  Called:
!*     iau_ERA00    Earth rotation angle, IAU 2000
!*     iau_ANP      normalize angle into range 0 to 2pi
!*
!*  References:
!*
!*     Capitaine, N., Wallace, P.T. and McCarthy, D.D., "Expressions to
!*     implement the IAU 2000 definition of UT1", Astronomy &
!*     Astrophysics, 406, 1135-1149 (2003)
!*
!*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!*     IERS Technical Note No. 32, BKG (2004)
!*
!*  This revision:  2020 November 17
!*
!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION UTA, UTB, TTA, TTB

!*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

!*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

!*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

      DOUBLE PRECISION T

!      DOUBLE PRECISION iau_ANP, iau_ERA00

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  TT Julian centuries since J2000.0.
      T = ( ( TTA-DJ00 ) + TTB ) / DJC

!*  Greenwich Mean Sidereal Time, IAU 2000.
      iau_GMST00 = iau_ANP ( iau_ERA00 ( UTA, UTB ) +  &
     &                       (    0.014506D0   +       &
     &                       ( 4612.15739966D0 +       &
     &                       (  + 1.39667721D0 +       &
     &                       (  - 0.00009344D0 +       &
     &                       (  + 0.00001882D0 )       &
     &                                 * T ) * T ) * T ) * T ) * DAS2R )

!*  Finished.

!*+----------------------------------------------------------------------


      END
      DOUBLE PRECISION FUNCTION iau_ERA00 ( DJ1, DJ2 )
!*+
!*  - - - - - - - - - -
!*   i a u _ E R A 0 0
!*  - - - - - - - - - -
!*
!*  Earth rotation angle (IAU 2000 model).
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  canonical model.
!*
!*  Given:
!*     DJ1,DJ2     d      UT1 as a 2-part Julian Date (see note)
!*
!*  The result is the Earth rotation angle (radians), in the range 0 to
!*  2pi.
!*
!*  Notes:
!*
!*  1) The UT1 date DJ1+DJ2 is a Julian Date, apportioned in any
!*     convenient way between the arguments DJ1 and DJ2.  For example,
!*     JD(UT1)=2450123.7 could be expressed in any of these ways,
!*     among others:
!*
!*             DJ1            DJ2
!*
!*         2450123.7D0        0D0        (JD method)
!*          2451545D0      -1421.3D0     (J2000 method)
!*         2400000.5D0     50123.2D0     (MJD method)
!*         2450123.5D0       0.2D0       (date & time method)
!*
!*     The JD method is the most natural and convenient to use in
!*     cases where the loss of several decimal digits of resolution
!*     is acceptable.  The J2000 and MJD methods are good compromises
!*     between resolution and convenience.  The date & time method is
!*     best matched to the algorithm used:  maximum accuracy (or, at
!*     least, minimum noise) is delivered when the DJ1 argument is for
!*     0hrs UT1 on the day in question and the DJ2 argument lies in the
!*     range 0 to 1, or vice versa.
!*
!*  2) The algorithm is adapted from Expression 22 of Capitaine et al.
!*     2000.  The time argument has been expressed in days directly,
!*     and, to retain precision, integer contributions have been
!*     eliminated.  The same formulation is given in IERS Conventions
!*     (2003), Chap. 5, Eq. 14.
!*
!*  Called:
!*     iau_ANP      normalize angle into range 0 to 2pi
!*
!*  References:
!*
!*     Capitaine N., Guinot B. and McCarthy D.D, 2000, Astron.
!*     Astrophys., 355, 398-405.
!*
!*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!*     IERS Technical Note No. 32, BKG (2004)
!*
!*  This revision:  2009 December 15
!*
!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DJ1, DJ2

!*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

!*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

      DOUBLE PRECISION D1, D2, T, F

!      DOUBLE PRECISION iau_ANP

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Days since fundamental epoch.
      IF ( DJ1 .LT. DJ2 ) THEN
         D1 = DJ1
         D2 = DJ2
      ELSE
         D1 = DJ2
         D2 = DJ1
      END IF
      T = D1 + ( D2-DJ00 )

!*  Fractional part of T (days).
      F = MOD ( D1, 1D0 ) + MOD ( D2, 1D0 )

!*  Earth rotation angle at this UT1.
      iau_ERA00 = iau_ANP ( D2PI * ( F + 0.7790572732640D0 + 0.00273781191135448D0 * T ) )

!*  Finished.

!*+----------------------------------------------------------------------

      END

      DOUBLE PRECISION FUNCTION iau_ANP ( A )
!*+
!*  - - - - - - - -
!*   i a u _ A N P
!*  - - - - - - - -
!*
!*  Normalize angle into the range 0 <= A < 2pi.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  vector/matrix support routine.
!*
!*  Given:
!*     A          d       angle (radians)
!*
!*  Returned:
!*     iau_ANP    d       angle in range 0-2pi
!*
!*  This revision:  2000 December 15
!*
!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION A

!*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

      DOUBLE PRECISION W

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      W = MOD(A,D2PI)
      IF ( W .LT. 0D0 ) W = W + D2PI
      iau_ANP = W

!*  Finished.

!*+----------------------------------------------------------------------

      END

      DOUBLE PRECISION FUNCTION iau_EE00B ( DATE1, DATE2 )
!*+
!*  - - - - - - - - - -
!*   i a u _ E E 0 0 B
!*  - - - - - - - - - -
!*
!*  Equation of the equinoxes, compatible with IAU 2000 resolutions but
!*  using the truncated nutation model IAU 2000B.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  support routine.
!*
!*  Given:
!*     DATE1,DATE2   d     TT as a 2-part Julian Date (Note 1)
!*
!*  Returned:
!*     iau_EE00B     d     equation of the equinoxes (Note 2)
!*
!*  Notes:
!*
!*  1) The TT date DATE1+DATE2 is a Julian Date, apportioned in any
!*     convenient way between the two arguments.  For example,
!*     JD(TT)=2450123.7 could be expressed in any of these ways,
!*     among others:
!*
!*            DATE1          DATE2
!*
!*         2450123.7D0        0D0        (JD method)
!*          2451545D0      -1421.3D0     (J2000 method)
!*         2400000.5D0     50123.2D0     (MJD method)
!*         2450123.5D0       0.2D0       (date & time method)
!*
!*     The JD method is the most natural and convenient to use in
!*     cases where the loss of several decimal digits of resolution
!*     is acceptable.  The J2000 method is best matched to the way
!*     the argument is handled internally and will deliver the
!*     optimum resolution.  The MJD method and the date & time methods
!*     are both good compromises between resolution and convenience.
!*
!*  2) The result, which is in radians, operates in the following sense:
!*
!*        Greenwich apparent ST = GMST + equation of the equinoxes
!*
!*  3) The result is compatible with the IAU 2000 resolutions except that
!*     accuracy has been compromised (1 mas) for the sake of speed.  For
!*     further details, see McCarthy & Luzum (2003), IERS Conventions
!*     2003 and Capitaine et al. (2003).
!*
!*  Called:
!*     iau_PR00     IAU 2000 precession adjustments
!*     iau_OBL80    mean obliquity, IAU 1980
!*     iau_NUT00B   nutation, IAU 2000B
!*     iau_EE00     equation of the equinoxes, IAU 2000
!*
!*  References:
!*
!*     Capitaine, N., Wallace, P.T. and McCarthy, D.D., "Expressions to
!*     implement the IAU 2000 definition of UT1", Astronomy &
!*     Astrophysics, 406, 1135-1149 (2003)
!*
!*     McCarthy, D.D. & Luzum, B.J., "An abridged model of the
!*     precession-nutation of the celestial pole", Celestial Mechanics &
!*     Dynamical Astronomy, 85, 37-49 (2003)
!*
!*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!*     IERS Technical Note No. 32, BKG (2004)
!*
!*  This revision:  2020 November 3
!*
!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2

      DOUBLE PRECISION DPSIPR, DEPSPR, EPSA, DPSI, DEPS

!      DOUBLE PRECISION iau_OBL80, iau_EE00

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  IAU 2000 precession-rate adjustments.
      CALL iau_PR00 ( DATE1, DATE2, DPSIPR, DEPSPR )

!*  Mean obliquity, consistent with IAU 2000 precession-nutation.
      EPSA = iau_OBL80 ( DATE1, DATE2 ) + DEPSPR

!*  Nutation in longitude.
      CALL iau_NUT00B ( DATE1, DATE2, DPSI, DEPS )

!*  Equation of the equinoxes.
      iau_EE00B = iau_EE00 ( DATE1, DATE2, EPSA, DPSI )

!*  Finished.

!*+----------------------------------------------------------------------

      END

      DOUBLE PRECISION FUNCTION iau_EE00A ( DATE1, DATE2 )
!*+
!*  - - - - - - - - - -
!*   i a u _ E E 0 0 A
!*  - - - - - - - - - -
!*
!*  Equation of the equinoxes, compatible with IAU 2000 resolutions.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  support routine.
!*
!*  Given:
!*     DATE1,DATE2    d      TT as a 2-part Julian Date (Note 1)
!*
!*  Returned:
!*     iau_EE00A      d      equation of the equinoxes (Note 2)
!*
!*  Notes:
!*
!*  1) The TT date DATE1+DATE2 is a Julian Date, apportioned in any
!*     convenient way between the two arguments.  For example,
!*     JD(TT)=2450123.7 could be expressed in any of these ways,
!*     among others:
!*
!*            DATE1          DATE2
!*
!*         2450123.7D0        0D0        (JD method)
!*          2451545D0      -1421.3D0     (J2000 method)
!*         2400000.5D0     50123.2D0     (MJD method)
!*         2450123.5D0       0.2D0       (date & time method)
!*
!*     The JD method is the most natural and convenient to use in
!*     cases where the loss of several decimal digits of resolution
!*     is acceptable.  The J2000 method is best matched to the way
!*     the argument is handled internally and will deliver the
!*     optimum resolution.  The MJD method and the date & time methods
!*     are both good compromises between resolution and convenience.
!*
!*  2) The result, which is in radians, operates in the following sense:
!*
!*        Greenwich apparent ST = GMST + equation of the equinoxes
!*
!*  3) The result is compatible with the IAU 2000 resolutions.  For
!*     further details, see IERS Conventions 2003 and Capitaine et al.
!*     (2002).
!*
!*  Called:
!*     iau_PR00     IAU 2000 precession adjustments
!*     iau_OBL80    mean obliquity, IAU 1980
!*     iau_NUT00A   nutation, IAU 2000A
!*     iau_EE00     equation of the equinoxes, IAU 2000
!*
!*  References:
!*
!*     Capitaine, N., Wallace, P.T. and McCarthy, D.D., "Expressions to
!*     implement the IAU 2000 definition of UT1", Astronomy &
!*     Astrophysics, 406, 1135-1149 (2003)
!*
!*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!*     IERS Technical Note No. 32, BKG (2004)
!*
!*  This revision:  2006 November 13
!*
!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2

      DOUBLE PRECISION DPSIPR, DEPSPR, EPSA, DPSI, DEPS

!      DOUBLE PRECISION iau_OBL80, iau_EE00

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  IAU 2000 precession-rate adjustments.
      CALL iau_PR00 ( DATE1, DATE2, DPSIPR, DEPSPR )

!*  Mean obliquity, consistent with IAU 2000 precession-nutation.
      EPSA = iau_OBL80 ( DATE1, DATE2 ) + DEPSPR

!*  Nutation in longitude.
      CALL iau_NUT00A ( DATE1, DATE2, DPSI, DEPS )

!*  Equation of the equinoxes.
      iau_EE00A = iau_EE00 ( DATE1, DATE2, EPSA, DPSI )

!*  Finished.

!*+----------------------------------------------------------------------

      END

      DOUBLE PRECISION FUNCTION iau_EE00 ( DATE1, DATE2, EPSA, DPSI )
!*+
!*  - - - - - - - - -
!*   i a u _ E E 0 0
!*  - - - - - - - - -
!*
!*  The equation of the equinoxes, compatible with IAU 2000 resolutions,
!*  given the nutation in longitude and the mean obliquity.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  canonical model.
!*
!*  Given:
!*     DATE1,DATE2    d      TT as a 2-part Julian Date (Note 1)
!*     EPSA           d      mean obliquity (Note 2)
!*     DPSI           d      nutation in longitude (Note 3)
!*
!*  Returned:
!*     iau_EE00       d      equation of the equinoxes (Note 4)
!*
!*  Notes:
!*
!*  1) The TT date DATE1+DATE2 is a Julian Date, apportioned in any
!*     convenient way between the two arguments.  For example,
!*     JD(TT)=2450123.7 could be expressed in any of these ways,
!*     among others:
!*
!*            DATE1          DATE2
!*
!*         2450123.7D0        0D0        (JD method)
!*          2451545D0      -1421.3D0     (J2000 method)
!*         2400000.5D0     50123.2D0     (MJD method)
!*         2450123.5D0       0.2D0       (date & time method)
!*
!*     The JD method is the most natural and convenient to use in
!*     cases where the loss of several decimal digits of resolution
!*     is acceptable.  The J2000 method is best matched to the way
!*     the argument is handled internally and will deliver the
!*     optimum resolution.  The MJD method and the date & time methods
!*     are both good compromises between resolution and convenience.
!*
!*  2) The obliquity, in radians, is mean of date.
!*
!*  3) The result, which is in radians, operates in the following sense:
!*
!*        Greenwich apparent ST = GMST + equation of the equinoxes
!*
!*  4) The result is compatible with the IAU 2000 resolutions.  For
!*     further details, see IERS Conventions 2003 and Capitaine et al.
!*     (2002).
!*
!*  Called:
!*     iau_EECT00   equation of the equinoxes complementary terms
!*
!*  References:
!*
!*     Capitaine, N., Wallace, P.T. and McCarthy, D.D., "Expressions to
!*     implement the IAU 2000 definition of UT1", Astronomy &
!*     Astrophysics, 406, 1135-1149 (2003)
!*
!*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!*     IERS Technical Note No. 32, BKG (2004)
!*
!*  This revision:  2006 November 13
!*
!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2, EPSA, DPSI

!      DOUBLE PRECISION iau_EECT00

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Equation of the equinoxes.
      iau_EE00 = DPSI * COS(EPSA) + iau_EECT00 ( DATE1, DATE2 )

!*  Finished.

!*+----------------------------------------------------------------------

      END

      DOUBLE PRECISION FUNCTION iau_EECT00 ( DATE1, DATE2 )
!*+
!*  - - - - - - - - - - -
!*   i a u _ E E C T 0 0
!*  - - - - - - - - - - -
!*
!*  Equation of the equinoxes complementary terms, consistent with
!*  IAU 2000 resolutions.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  canonical model.
!*
!*  Given:
!*     DATE1,DATE2   d    TT as a 2-part Julian Date (Note 1)
!*
!*  Returned:
!*     iau_EECT00    d    complementary terms (Note 2)
!*
!*  Notes:
!*
!*  1) The TT date DATE1+DATE2 is a Julian Date, apportioned in any
!*     convenient way between the two arguments.  For example,
!*     JD(TT)=2450123.7 could be expressed in any of these ways,
!*     among others:
!*
!*            DATE1          DATE2
!*
!*         2450123.7D0        0D0        (JD method)
!*          2451545D0      -1421.3D0     (J2000 method)
!*         2400000.5D0     50123.2D0     (MJD method)
!*         2450123.5D0       0.2D0       (date & time method)
!*
!*     The JD method is the most natural and convenient to use in
!*     cases where the loss of several decimal digits of resolution
!*     is acceptable.  The J2000 method is best matched to the way
!*     the argument is handled internally and will deliver the
!*     optimum resolution.  The MJD method and the date & time methods
!*     are both good compromises between resolution and convenience.
!*
!*  2) The "complementary terms" are part of the equation of the
!*     equinoxes (EE), classically the difference between apparent and
!*     mean Sidereal Time:
!*
!*        GAST = GMST + EE
!*
!*     with:
!*
!*        EE = dpsi * cos(eps)
!*
!*     where dpsi is the nutation in longitude and eps is the obliquity
!*     of date.  However, if the rotation of the Earth were constant in
!*     an inertial frame the classical formulation would lead to apparent
!*     irregularities in the UT1 timescale traceable to side-effects of
!*     precession-nutation.  In order to eliminate these effects from
!*     UT1, "complementary terms" were introduced in 1994 (IAU, 1994) and
!*     took effect from 1997 (Capitaine and Gontier, 1993):
!*
!*        GAST = GMST + CT + EE
!*
!*     By convention, the complementary terms are included as part of the
!*     equation of the equinoxes rather than as part of the mean Sidereal
!*     Time.  This slightly compromises the "geometrical" interpretation
!*     of mean sidereal time but is otherwise inconsequential.
!*
!*     The present routine computes CT in the above expression,
!*     compatible with IAU 2000 resolutions (Capitaine et al., 2002, and
!*     IERS Conventions 2003).
!*
!*  Called:
!*     iau_FAL03    mean anomaly of the Moon
!*     iau_FALP03   mean anomaly of the Sun
!*     iau_FAF03    mean argument of the latitude of the Moon
!*     iau_FAD03    mean elongation of the Moon from the Sun
!*     iau_FAOM03   mean longitude of the Moon's ascending node
!*     iau_FAVE03   mean longitude of Venus
!*     iau_FAE03    mean longitude of Earth
!*     iau_FAPA03   general accumulated precession in longitude
!*
!*  References:
!*
!*     Capitaine, N. & Gontier, A.-M., Astron.Astrophys., 275,
!*     645-650 (1993)
!*
!*     Capitaine, N., Wallace, P.T. and McCarthy, D.D., "Expressions to
!*     implement the IAU 2000 definition of UT1", Astron.Astrophys.,
!*     406, 1135-1149 (2003)
!*
!*     IAU Resolution C7, Recommendation 3 (1994)
!*
!*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!*     IERS Technical Note No. 32, BKG (2004)
!*
!*  This revision:  2017 October 23
!*
!!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2

!*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

!*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

!*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

!*  Time since J2000.0, in Julian centuries
      DOUBLE PRECISION T

!*  Miscellaneous
      INTEGER I, J
      DOUBLE PRECISION A, S0, S1
!      DOUBLE PRECISION iau_FAL03, iau_FALP03, iau_FAF03,iau_FAD03, iau_FAOM03, iau_FAVE03, iau_FAE03,
!     :                 iau_FAPA03

!*  Fundamental arguments
      DOUBLE PRECISION FA(14)

!*  -----------------------------------------
!*  The series for the EE complementary terms
!*  -----------------------------------------

!*  Number of terms in the series
      INTEGER NE0, NE1
      PARAMETER ( NE0=33, NE1=1 )

!*  Coefficients of l,l',F,D,Om,LVe,LE,pA
      INTEGER KE0 ( 8, NE0 ),  KE1 ( 8, NE1 )

!*  Sine and cosine coefficients
      DOUBLE PRECISION SE0 ( 2, NE0 ), SE1 ( 2, NE1 )

!*  Argument coefficients for t^0
      DATA ( ( KE0(I,J), I=1,8), J=1,10 ) / &
     &  0,  0,  0,  0,  1,  0,  0,  0,      &
     &  0,  0,  0,  0,  2,  0,  0,  0,      &
     &  0,  0,  2, -2,  3,  0,  0,  0,    &
     &  0,  0,  2, -2,  1,  0,  0,  0,    &
     &  0,  0,  2, -2,  2,  0,  0,  0,    &
     &  0,  0,  2,  0,  3,  0,  0,  0,    &
     &  0,  0,  2,  0,  1,  0,  0,  0,    &
     &  0,  0,  0,  0,  3,  0,  0,  0,    &
     &  0,  1,  0,  0,  1,  0,  0,  0,    &
     &  0,  1,  0,  0, -1,  0,  0,  0 /
      DATA ( ( KE0(I,J), I=1,8), J=11,20 ) /&
     &  1,  0,  0,  0, -1,  0,  0,  0,    &
     &  1,  0,  0,  0,  1,  0,  0,  0,    &
     &  0,  1,  2, -2,  3,  0,  0,  0,    &
     &  0,  1,  2, -2,  1,  0,  0,  0,    &
     &  0,  0,  4, -4,  4,  0,  0,  0,    &
     &  0,  0,  1, -1,  1, -8, 12,  0,    &
     &  0,  0,  2,  0,  0,  0,  0,  0,    &
     &  0,  0,  2,  0,  2,  0,  0,  0,    &
     &  1,  0,  2,  0,  3,  0,  0,  0,    &
     &  1,  0,  2,  0,  1,  0,  0,  0 /
      DATA ( ( KE0(I,J), I=1,8), J=21,30 ) /&
     &  0,  0,  2, -2,  0,  0,  0,  0,    &
     &  0,  1, -2,  2, -3,  0,  0,  0,    &
     &  0,  1, -2,  2, -1,  0,  0,  0,    &
     &  0,  0,  0,  0,  0,  8,-13, -1,    &
     &  0,  0,  0,  2,  0,  0,  0,  0,    &
     &  2,  0, -2,  0, -1,  0,  0,  0,    &
     &  1,  0,  0, -2,  1,  0,  0,  0,    &
     &  0,  1,  2, -2,  2,  0,  0,  0,    &
     &  1,  0,  0, -2, -1,  0,  0,  0,    &
     &  0,  0,  4, -2,  4,  0,  0,  0 /
      DATA ( ( KE0(I,J), I=1,8), J=31,NE0 ) /&
     &  0,  0,  2, -2,  4,  0,  0,  0,    &
     &  1,  0, -2,  0, -3,  0,  0,  0,    &
     &  1,  0, -2,  0, -1,  0,  0,  0 /

!  Argument coefficients for t^1
      DATA ( ( KE1(I,J), I=1,8), J=1,NE1 ) / 0,  0,  0,  0,  1,  0,  0,  0 /

!  Sine and cosine coefficients for t^0
      DATA ( ( SE0(I,J), I=1,2), J =    1,   10 ) /&
     &            +2640.96D-6,          -0.39D-6,     &
     &              +63.52D-6,          -0.02D-6,     &
     &              +11.75D-6,          +0.01D-6,     &
     &              +11.21D-6,          +0.01D-6,     &
     &               -4.55D-6,          +0.00D-6,     &
     &               +2.02D-6,          +0.00D-6,     &
     &               +1.98D-6,          +0.00D-6,     &
     &               -1.72D-6,          +0.00D-6,     &
     &               -1.41D-6,          -0.01D-6,     &
     &               -1.26D-6,          -0.01D-6 /
      DATA ( ( SE0(I,J), I=1,2), J =   11,   20 ) /&
     &               -0.63D-6,          +0.00D-6,     &
     &               -0.63D-6,          +0.00D-6,     &
     &               +0.46D-6,          +0.00D-6,     &
     &               +0.45D-6,          +0.00D-6,     &
     &               +0.36D-6,          +0.00D-6,     &
     &               -0.24D-6,          -0.12D-6,     &
     &               +0.32D-6,          +0.00D-6,     &
     &               +0.28D-6,          +0.00D-6,     &
     &               +0.27D-6,          +0.00D-6,     &
     &               +0.26D-6,          +0.00D-6 /
      DATA ( ( SE0(I,J), I=1,2), J =   21,   30 ) /&
     &               -0.21D-6,          +0.00D-6,     &
     &               +0.19D-6,          +0.00D-6,     &
     &               +0.18D-6,          +0.00D-6,     &
     &               -0.10D-6,          +0.05D-6,     &
     &               +0.15D-6,          +0.00D-6,     &
     &               -0.14D-6,          +0.00D-6,     &
     &               +0.14D-6,          +0.00D-6,     &
     &               -0.14D-6,          +0.00D-6,     &
     &               +0.14D-6,          +0.00D-6,     &
     &               +0.13D-6,          +0.00D-6 /
      DATA ( ( SE0(I,J), I=1,2), J =   31,  NE0 ) /&
     &               -0.11D-6,          +0.00D-6,     &
     &               +0.11D-6,          +0.00D-6,     &
     &               +0.11D-6,          +0.00D-6 /

!*  Sine and cosine coefficients for t^1
      DATA ( ( SE1(I,J), I=1,2), J =    1,  NE1 ) / -0.87D-6, +0.00D-6 /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Interval between fundamental epoch J2000.0 and current date (JC).
      T = ( ( DATE1-DJ00 ) + DATE2 ) / DJC

!*  Fundamental Arguments (from IERS Conventions 2003)

!*  Mean anomaly of the Moon.
      FA(1) = iau_FAL03 ( T )

!*  Mean anomaly of the Sun.
      FA(2) = iau_FALP03 ( T )

!*  Mean longitude of the Moon minus that of the ascending node.
      FA(3) = iau_FAF03 ( T )

!*  Mean elongation of the Moon from the Sun.
      FA(4) = iau_FAD03 ( T )

!*  Mean longitude of the ascending node of the Moon.
      FA(5) = iau_FAOM03 ( T )

!*  Mean longitude of Venus.
      FA(6) = iau_FAVE03 ( T )

!*  Mean longitude of Earth.
      FA(7) = iau_FAE03 ( T )

!*  General precession in longitude.
      FA(8) = iau_FAPA03 ( T )

!*  Evaluate the EE complementary terms.
      S0 = 0D0
      S1 = 0D0

      DO 2 I = NE0,1,-1
         A = 0D0
         DO 1 J=1,8
            A = A + DBLE(KE0(J,I))*FA(J)
 1       CONTINUE
         S0 = S0 + ( SE0(1,I)*SIN(A) + SE0(2,I)*COS(A) )
 2    CONTINUE
      DO 4 I = NE1,1,-1
         A = 0D0
         DO 3 J=1,8
            A = A + DBLE(KE1(J,I))*FA(J)
 3       CONTINUE
         S1 = S1 + ( SE1(1,I)*SIN(A) + SE1(2,I)*COS(A) )
 4    CONTINUE
      iau_EECT00 = ( S0 + S1 * T ) * DAS2R

!*  Finished.

!*+----------------------------------------------------------------------

      END

      DOUBLE PRECISION FUNCTION iau_OBL80 ( DATE1, DATE2 )
!*+
!*  - - - - - - - - - -
!*   i a u _ O B L 8 0
!*  - - - - - - - - - -
!*
!*  Mean obliquity of the ecliptic, IAU 1980 model.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  canonical model.
!*
!*  Given:
!*     DATE1,DATE2     d      TT as a 2-part Julian Date (Note 1)
!*
!*  Returned:
!*     iau_OBL80       d      obliquity of the ecliptic (radians, Note 2)
!*
!*  Notes:
!*
!*  1) The date DATE1+DATE2 is a Julian Date, apportioned in any
!*     convenient way between the two arguments.  For example,
!*     JD(TDB)=2450123.7 could be expressed in any of these ways,
!*     among others:
!*
!*             DATE1         DATE2
!*
!*         2450123.7D0        0D0        (JD method)
!*          2451545D0      -1421.3D0     (J2000 method)
!*         2400000.5D0     50123.2D0     (MJD method)
!*         2450123.5D0       0.2D0       (date & time method)
!*
!*     The JD method is the most natural and convenient to use in
!*     cases where the loss of several decimal digits of resolution
!*     is acceptable.  The J2000 method is best matched to the way
!*     the argument is handled internally and will deliver the
!*     optimum resolution.  The MJD method and the date & time methods
!*     are both good compromises between resolution and convenience.
!*
!*  2) The result is the angle between the ecliptic and mean equator of
!*     date DATE1+DATE2.
!*
!*  Reference:
!*
!*     Explanatory Supplement to the Astronomical Almanac,
!*     P. Kenneth Seidelmann (ed), University Science Books (1992),
!*     Expression 3.222-1 (p114).
!*
!*  This revision:  2009 December 15
!*
!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2

!*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

!*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

!*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

      DOUBLE PRECISION T

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Interval between fundamental epoch J2000.0 and given date (JC).
      T = ( ( DATE1-DJ00 ) + DATE2 ) / DJC

!*  Mean obliquity of date.
      iau_OBL80 = DAS2R * ( 84381.448D0 +( -46.8150D0 +( -0.00059D0 +0.001813D0 * T ) * T ) * T )

!*  Finished.

!*+----------------------------------------------------------------------

      END

      SUBROUTINE iau_REFCO ( PHPA, TC, RH, WL, REFA, REFB )
!*+
!*  - - - - - - - - - -
!*   i a u _ R E F C O
!*  - - - - - - - - - -
!*
!*  Determine the constants A and B in the atmospheric refraction model
!*  dZ = A tan Z + B tan^3 Z.
!*
!*  Z is the "observed" zenith distance (i.e. affected by refraction)
!*  and dZ is what to add to Z to give the "topocentric" (i.e. in vacuo)
!*  zenith distance.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  support routine.
!*
!*  Given:
!*     PHPA     d      pressure at the observer (hPa = millibar)
!*     TC       d      ambient temperature at the observer (deg C)
!*     RH       d      relative humidity at the observer (range 0-1)
!*     WL       d      wavelength (micrometers)
!*
!*  Returned:
!*     REFA     d      tan Z coefficient (radians)
!*     REFB     d      tan^3 Z coefficient (radians)
!*
!*  Notes:
!*
!*  1) The model balances speed and accuracy to give good results in
!*     applications where performance at low altitudes is not paramount.
!*     Performance is maintained across a range of conditions, and
!*     applies to both optical/IR and radio.
!*
!*  2) The model omits the effects of (i) height above sea level (apart
!*     from the reduced pressure itself), (ii) latitude (i.e. the
!*     flattening of the Earth), (iii) variations in tropospheric lapse
!*     rate and (iv) dispersive effects in the radio.
!*
!*     The model was tested using the following range of conditions:
!*
!*       lapse rates 0.0055, 0.0065, 0.0075 deg/meter
!*       latitudes 0, 25, 50, 75 degrees
!*       heights 0, 2500, 5000 meters ASL
!*       pressures mean for height -10% to +5% in steps of 5%
!*       temperatures -10 deg to +20 deg with respect to 280 deg at SL
!*       relative humidity 0, 0.5, 1
!*       wavelengths 0.4, 0.6, ... 2 micron, + radio
!*       zenith distances 15, 45, 75 degrees
!*
!*     The accuracy with respect to raytracing through a model
!*     atmosphere was as follows:
!*
!*                            worst         RMS
!*
!*       optical/IR           62 mas       8 mas
!*       radio               319 mas      49 mas
!*
!*     For this particular set of conditions:
!*
!*       lapse rate 0.0065 K/meter
!*       latitude 50 degrees
!*       sea level
!*       pressure 1005 mb
!*       temperature 280.15 K
!*       humidity 80%
!*       wavelength 5740 Angstroms
!*
!*     the results were as follows:
!*
!*       ZD       raytrace     iau_REFCO  Saastamoinen
!*
!*       10         10.27        10.27        10.27
!*       20         21.19        21.20        21.19
!*       30         33.61        33.61        33.60
!*       40         48.82        48.83        48.81
!*       45         58.16        58.18        58.16
!*       50         69.28        69.30        69.27
!*       55         82.97        82.99        82.95
!*       60        100.51       100.54       100.50
!*       65        124.23       124.26       124.20
!*       70        158.63       158.68       158.61
!*       72        177.32       177.37       177.31
!*       74        200.35       200.38       200.32
!*       76        229.45       229.43       229.42
!*       78        267.44       267.29       267.41
!*       80        319.13       318.55       319.10
!*
!*      deg        arcsec       arcsec       arcsec
!*
!*     The values for Saastamoinen's formula (which includes terms
!*     up to tan^5) are taken from Hohenkerk and Sinclair (1985).
!*
!*  3) A WL value in the range 0-100 selects the optical/IR case and is
!*     wavelength in micrometers.  Any value outside this range selects
!*     the radio case.
!*
!*  4) Outlandish input parameters are silently limited to mathematically
!*     safe values.  Zero pressure is permissible, and causes zeroes to
!*     be returned.
!*
!*  5) The algorithm draws on several sources, as follows:
!*
!*     a) The formula for the saturation vapour pressure of water as
!*        a function of temperature and temperature is taken from
!*        Equations (A4.5-A4.7) of Gill (1982).
!*
!*     b) The formula for the water vapour pressure, given the
!*        saturation pressure and the relative humidity, is from
!*        Crane (1976), Equation (2.5.5).
!*
!*     c) The refractivity of air is a function of temperature,
!*        total pressure, water-vapour pressure and, in the case
!*        of optical/IR, wavelength.  The formulae for the two cases are
!*        developed from Hohenkerk & Sinclair (1985) and Rueger (2002).
!*        The IAG (1999) optical refractivity for dry air is used.
!*
!*     d) The formula for beta, the ratio of the scale height of the
!*        atmosphere to the geocentric distance of the observer, is
!*        an adaption of Equation (9) from Stone (1996).  The
!*        adaptations, arrived at empirically, consist of (i) a small
!*        adjustment to the coefficient and (ii) a humidity term for the
!*        radio case only.
!*
!*     e) The formulae for the refraction constants as a function of
!*        n-1 and beta are from Green (1987), Equation (4.31).
!*
!*  References:
!*
!*     Crane, R.K., Meeks, M.L. (ed), "Refraction Effects in the Neutral
!*     Atmosphere", Methods of Experimental Physics: Astrophysics 12B,
!*     Academic Press, 1976.
!*
!*     Gill, Adrian E., "Atmosphere-Ocean Dynamics", Academic Press,
!*     1982.
!*
!*     Green, R.M., "Spherical Astronomy", Cambridge University Press,
!*     1987.
!*
!*     Hohenkerk, C.Y., & Sinclair, A.T., NAO Technical Note No. 63,
!*     1985.
!*
!*     IAG Resolutions adopted at the XXIIth General Assembly in
!*     Birmingham, 1999, Resolution 3.
!*
!*     Rueger, J.M., "Refractive Index Formulae for Electronic Distance
!*     Measurement with Radio and Millimetre Waves", in Unisurv Report
!*     S-68, School of Surveying and Spatial Information Systems,
!*     University of New South Wales, Sydney, Australia, 2002.
!*
!*     Stone, Ronald C., P.A.S.P. 108, 1051-1058, 1996.
!*
!*  This revision:  2020 August 17
!*
!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE
      DOUBLE PRECISION PHPA, TC, RH, WL, REFA, REFB

      LOGICAL OPTIC
      DOUBLE PRECISION P, T, R, W, PS, PW, TK, WLSQ, GAMMA, BETA

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Decide whether optical/IR or radio case:  switch at 100 microns.
      OPTIC = WL.LE.100D0

!*  Restrict parameters to safe values.
      T = MIN(MAX(TC,-150D0),200D0)
      P = MIN(MAX(PHPA,0D0),10000D0)
      R = MIN(MAX(RH,0D0),1D0)
      W = MIN(MAX(WL,0.1D0),1D6)

!*  Water vapour pressure at the observer.
      IF (P.GT.0D0) THEN
         PS = 10D0**((0.7859D0+0.03477D0*T)/(1D0+0.00412D0*T))*(1D0+P*(4.5D-6+6D-10*T*T))
         PW = R*PS/(1D0-(1D0-R)*PS/P)
      ELSE
         PW = 0D0
      END IF

!*  Refractive index minus 1 at the observer.
      TK = T + 273.15D0
      IF (OPTIC) THEN
         WLSQ = W*W
         GAMMA = ((77.53484D-6+(4.39108D-7+3.666D-9/WLSQ)/WLSQ)*P-11.2684D-6*PW)/TK
      ELSE
         GAMMA = (77.6890D-6*P-(6.3938D-6-0.375463D0/TK)*PW)/TK
      END IF

!*  Formula for beta adapted from Stone, with empirical adjustments.
      BETA=4.4474D-6*TK
      IF (.NOT.OPTIC) BETA=BETA-0.0074D0*PW*BETA

!*  Refraction constants from Green.
      REFA = GAMMA*(1D0-BETA)
      REFB = -GAMMA*(BETA-GAMMA/2D0)

!*  Finished.

!*+----------------------------------------------------------------------

      END

      DOUBLE PRECISION FUNCTION iau_OBL06 ( DATE1, DATE2 )
!*+
!*  - - - - - - - - - -
!*   i a u _ O B L 0 6
!*  - - - - - - - - - -
!*
!*  Mean obliquity of the ecliptic, IAU 2006 precession model.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  canonical model.
!*
!*  Given:
!*     DATE1,DATE2    d     TT as a 2-part Julian Date (Note 1)
!*
!*  Returned:
!*     iau_OBL06      d     obliquity of the ecliptic (radians, Note 2)
!*
!*  Notes:
!*
!*  1) The date DATE1+DATE2 is a Julian Date, apportioned in any
!*     convenient way between the two arguments.  For example,
!*     JD(TT)=2450123.7 could be expressed in any of these ways,
!*     among others:
!*
!*             DATE1         DATE2
!*
!*         2450123.7D0        0D0        (JD method)
!*          2451545D0      -1421.3D0     (J2000 method)
!*         2400000.5D0     50123.2D0     (MJD method)
!*         2450123.5D0       0.2D0       (date & time method)
!*
!*     The JD method is the most natural and convenient to use in
!*     cases where the loss of several decimal digits of resolution
!*     is acceptable.  The J2000 method is best matched to the way
!*     the argument is handled internally and will deliver the
!*     optimum resolution.  The MJD method and the date & time methods
!*     are both good compromises between resolution and convenience.
!*
!*  2) The result is the angle between the ecliptic and mean equator of
!*     date DATE1+DATE2.
!*
!*  Reference:
!*
!*     Hilton, J. et al., 2006, Celest.Mech.Dyn.Astron. 94, 351
!*
!*  This revision:  2009 December 15
!*
!*  SOFA release 2021-01-25
!*
!*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2

!*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

!*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

!*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

      DOUBLE PRECISION T

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Interval between fundamental date J2000.0 and given date (JC).
      T = ( ( DATE1-DJ00 ) + DATE2 ) / DJC

!*  Mean obliquity.
      iau_OBL06 = ( 84381.406D0       + &
     &            (   -46.836769D0    + &
     &            (    -0.0001831D0   + &
     &            (     0.00200340D0  + &
     &            (    -0.000000576D0 + &
     &            (    -0.0000000434D0 )&
     &                             * T ) * T ) * T ) * T ) * T ) * DAS2R


!*+----------------------------------------------------------------------

      END


end module
