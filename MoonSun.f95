
module MoonSun
      USE SOFA
      implicit none

      real(kind=8), parameter ::  PI = 3.141592653589793238462643D0
      ! Degree to radian coefficient    3.14159265358979323846264338327950288419716939937510582
       real(kind=8), parameter ::  DEGRAD = PI/180.D0

      contains

      function third_order_polynomial ( a, b, c, d, x)
!     This function calculates third order polynomial
!     x variable, a,b,c,d are coefficients
      real(kind=8) :: a , b , c, d , x, third_order_polynomial
      third_order_polynomial = ((a*x + b)*x + c)*x + d
      end function

      function fourth_order_polynomial( a, b, c, d,e, x)
 !     This function calculates fourth order polynomial
 !     x variable, a,b,c,d,e are coefficients
      real(kind=8) :: a , b , c, d ,e, x, fourth_order_polynomial
      fourth_order_polynomial = (((a*x + b)*x + c)*x + d)*x + e
      end function

      !*****************************************************************************

      function I2str(k)
 !   "Convert an integer to string."
          character(len=40) :: str, I2str
          integer, intent(in) :: k
          write (str, *) k
          I2str = adjustl(str)
      end function I2str

      function D2str(d)
 !   "Convert a double to string."
          character(len=40) :: str, D2str
          real(kind=8), intent(in) :: d
          write (str,'(F9.4)') d
          D2str = adjustl(str)
      end function D2str

!******************************************************************************

subroutine Angle2DMS(Degree, D , M , S)
  !     Converts Hours in real numbers to
  !     H integer  for Hour, M integer for minute
  !     S integer for seconds
        integer :: D , M , S
        Real(kind=8) :: Degree , Rmin, Ss

        D = int(Dmod(Degree,360.D0))
        Rmin = Dmod(Degree ,1.D0)*60.D0
        M = int(mod(Rmin,60.D0))
        Ss = (Rmin - M)*60
        S = int(Dmod((Rmin - M)*60.D0,60.D0))
        if ((Ss - S)> 0.5D0) S = S + 1
        if(S == 60) then
              S = 0
              M = M +1
        end if
  end subroutine

character(LEN=10) function Angles2text(Degree)
    real(kind=8) :: Degree
    integer :: D , M , S
    call Angle2DMS(Degree, D , M , S)

    Angles2text = adjustl(trim(I2str(D))//':'//trim(I2str(M))//&
                  ':'//trim(I2str(S)))
end function

!******************************************************************************
!C***SUBROUTINE Lagrange Simple Polynomial Interpolation***********
!C***FROM BOOK APPLIED NUMERICAL METHODS WITH SOFWARE- NAKAMURA****
!C   LDA=LEADIND ARRAY
!C   N=NUMBER OF INTERVALS
!C   X(I)= ORDINATES OF FUNCTION
!C   F(I)= INPUT VALUES AT X(I)
!C   XA= ORDINATE OF NEEDED VALUE
!C   YANS = VALUE OF F(I) AT XA

      SUBROUTINE LSPI(LDA,N,X,F,XA,YANS)
      INTEGER :: LDA,N, I , J
      REAL(8) :: X(LDA),F(LDA),XA,YANS, Z

        YANS=0.D0
        DO I=1,N
            Z=1.D0
            DO J=1,N
                  IF (I.NE.J) Z=Z*(XA-X(J))/(X(I)-X(J))
            END DO
            YANS=YANS+Z*F(I)
        END DO
      RETURN
      END subroutine

!*****************************************************************************************

      SUBROUTINE polint(xa,ya,n,x,y,dy)
      INTEGER :: n,NMAX
      REAL(kind=8) :: dy,x,y,xa(0:n),ya(0:n)
      PARAMETER (NMAX=9)
 !     Largest anticipated value of n.
 !     Given arrays xa and ya, each of length n, and given a value x, this routine returns a
 !     value y, and an error estimate dy. If P(x) is the polynomial of degree N − 1 such that
 !     P(xai) = yai; i = 1; : : : ;n, then the returned value y = P(x).
      INTEGER :: i,m,ns
      REAL(kind=8) :: den,dif,dift,ho,hp,w,c(0:NMAX),d(0:NMAX)
            ns=1
            dif=dabs(x-xa(1))
            do i=1,n   ! Here we find the index ns of the closest table entry,
                  dift=abs(x-xa(i))
                  if (dift < dif) then
                        ns=i
                        dif=dift
                  endif
                  c(i)=ya(i) !and initialize the tableau of c's and d's.
                  d(i)=ya(i)
            enddo
            y=ya(ns) ! This is the initial approximation to y.
            ns=ns-1
            do m=1,n-1 ! For each column of the tableau,
                  do i=1,n-m !we loop over the current c's and d's and update them.
                        ho=xa(i)-x
                        hp=xa(i+m)-x
                        w=c(i+1)-d(i)
                        den=ho-hp
                        if(den== 0.D0) then
                              print*,'failure in point'
                              return
                        endif
                  !      This error can occur only if two input xa's are (to within roundoff identical.
                        den=w/den
                        d(i)=hp*den! Here the c's and d's are updated.
                        c(i)=ho*den
                  enddo
                  if (2*ns.lt.n-m) then
                        dy=c(ns+1)
                  else
                        dy=d(ns)
                        ns=ns-1
                  endif
                  y=y+dy
            enddo
      return
      END

!***************************************************************************

      subroutine Sort2(N,RA, RB)
                integer :: N, L , IR, J , I
                real(kind=8) :: RRA, RRB
                real(kind=8), dimension(N) :: RA, RB

                L = N/2 + 1
                IR = N

           10   continue
                        if( L > 1) then
                                L = L - 1
                                RRA = RA(L)
                                RRB = RB(L)
                        else
                                RRA = RA(IR)
                                RRB = RB(IR)
                                RA(IR) = RA(1)
                                RB(IR) = RB(1)
                                IR = IR - 1
                                if(IR==1) then
                                        RA(1) = RRA
                                        RB(1) = RRB
                                        return
                                end if
                        end if
                        I = L
                        J = L + L
           20           if(J<=IR) then
                                if(J<IR) then
                                      if(RA(J)<RA(J+1)) J = J+1
                                end if
                                if(RRA<RA(J)) then
                                        RA(I)=RA(J)
                                        RB(I)=RB(J)
                                        I=J
                                        J=J+J
                                else
                                        J=IR+1
                                end if
                        goto 20
                        end if
                        RA(I) = RRA
                        RB(I) = RRB
                goto 10
        end subroutine

        subroutine Sort2R(N,RA, RB)
        integer :: N, L , IR, J , I
        real(kind=8) :: RRA, RRB
        real(kind=8), dimension(N) :: RA, RB

        L = N/2 + 1
        IR = N

   10   continue
                if( L > 1) then
                        L = L - 1
                        RRA = RA(L)
                        RRB = RB(L)
                else
                        RRA = RA(IR)
                        RRB = RB(IR)
                        RA(IR) = RA(1)
                        RB(IR) = RB(1)
                        IR = IR - 1
                        if(IR==1) then
                                RA(1) = RRA
                                RB(1) = RRB
                                return
                        end if
                end if
                I = L
                J = L + L
   20           if(J<=IR) then
                        if(J<IR) then
                              if(RA(J)>RA(J+1)) J = J+1
                        end if
                        if(RRA>RA(J)) then
                                RA(I)=RA(J)
                                RB(I)=RB(J)
                                I=J
                                J=J+J
                        else
                                J=IR+1
                        end if
                goto 20
                end if
                RA(I) = RRA
                RB(I) = RRB
        goto 10
        end subroutine

      subroutine ChangeAngle(Angle)
!     change Angle larger then 360.0 , or less than 0.0
!     to angle less than 360.0 degrees
          real(kind=8) :: Angle
          if (Angle > 360.D0 ) then
              Angle = Dmod(Angle,360.D0)
          elseif ( Angle< 0) then
              Angle = 360.D0 + Dmod(Angle,360.D0)
          end if
      end subroutine

      subroutine AngleChange(MaxAngle,Angle)
!     change Angle larger then MaxAngle , or less than 0.0
!     to angle less than MaxAngle degrees
          real(kind=8) :: Angle, MaxAngle
          if (Angle > MaxAngle ) then
              Angle = Dmod(Angle,MaxAngle)
          elseif ( Angle< 0) then
              Angle = (MaxAngle + Dmod(Angle,MaxAngle))
          end if
      end subroutine

     subroutine ChangeAngle180(Angle)
!     change Angle larger then 180.0 , or less than 0.0
!     to angle less than 180.0 degrees
          real(kind=8) :: Angle
          if (Dabs(Angle) > 360.D0 ) then
              Angle = Dmod(Angle,360.D0)
          end if
      if ( Angle<= -180.D0) then
              Angle = 360.D0 + Angle
          elseif(Angle>= 180.D0) then
              Angle = Angle - 360.D0
          end if
      end subroutine

      subroutine change_m(m)
!     change m larger then 1.0 , or less than 0.0
!     to m less than 1.0
          real(kind=8) :: m
          if (m > 1.D0 ) then
              m = Dmod(m,1.D0)
          elseif ( m< 0) then
              m = 1.D0 + Dmod(m,1.D0)
          end if
      end subroutine

!      subroutine change_mMoon(m)
!     change m larger then 1.0 , or less than 0.0
!     to m less than 1.0
!          real(kind=8) :: m
!          m = Dmod(m,1.D0)
!          if (m<0) then
!             m = 1.D0 + m
!          end if
!      end subroutine

      subroutine stdatm(z,t,p,r,a,mu,ts,rr,pp,rm,qm,kd,kk)
           implicit none
!c
!c   *********** 1976 STANDARD ATMOSPHERE SUBROUTINE **********
!c
!c     Mason's BASIC program, converted to FORTRAN - Sept. 1, 1989
!c
!c     W.H. Mason
!c     Department of Aerospace and Ocean Engineering
!c     Virginia Tech, Blacksburg, VA 24061
!c     email: mason@aoe.vt.edu
!c
!c     kd -  = 0 - metric units
!c          <> 0 - English units
!c
!c     kk -    0 - good return
!c             1 - error: altitude out of table,
!c                 do not use output (max altitude for this
!c                 routine is 84.85 km or 282,152 ft.)
!c
!c     z  - input altitude, in feet or meters (depending on kd)
!c
!c     output:
!c                      units: metric        English
!c     t  - temp.               deg K         deg R
!c     p  - pressure            N/m^2         lb/ft^2
!c     r  - density (rho)       Kg/m^3        slug/ft^3
!c     a  - speed of sound      m/sec         ft/sec
!c     mu - viscosity           Kg/(m sec)    slug/<ft sec)
!c
!c     ts - t/t at sea level
!c     rr - rho/rho at sea level
!c     pp - p/p at sea level
!c
!c     rm - Reynolds number per Mach per unit of length
!c     qm - dynamic pressure/Mach^2
!c
      real(8) :: k, h, mu, ml,z,P,r,a,ts,pp,rm
      real(8) :: qm,AL,T,C1,PL,RL,TL,RR,BT
      integer :: kk, kd
      KK = 0
      K  = 34.163195D0
      C1 = 3.048D-04
      IF (KD .eq. 0) go to 1240
      TL = 518.67D0
      PL = 2116.22D0
      RL = .0023769D0
      AL = 1116.45D0
      ML = 3.7373D-07
      BT = 3.0450963D-08
      GO TO 1260

 1240 TL = 288.15D0
      PL = 101325.D0
      RL = 1.225D0
      C1 = .001D0
      AL = 340.294D0
      ML = 1.7894D-05
      BT = 1.458D-06

 1260 H = C1 * Z / (1 + C1 * Z / 6356.766D0)
      IF (H .gt. 11.D0) go to 1290
      T  = 288.15D0 - 6.5D0 * H
      PP = (288.15D0 / T) ** ( - K / 6.5D0)
      GO TO 1420

 1290 IF (H .gt. 20.D0) go to 1310
      T  = 216.65D0
      PP = .22336D0 *  EXP ( - K * (H - 11.D0) / 216.65D0)
      GO TO 1420

1310  IF (H .gt. 32.D0) go to 1330
      T  = 216.65D0 + (H - 20.D0)
      PP = .054032D0 * (216.65D0 / T) ** K
      GO TO 1420

1330  IF (H .gt. 47.D0) go to 1350
      T  = 228.65D0 + 2.8D0 * (H - 32.D0)
      PP = .0085666D0 * (228.65D0 / T) ** (K / 2.8D0)
      GO TO 1420

1350  IF( H .gt. 51.D0) go to 1370
      T  = 270.65D0
      PP = .0010945D0 *  EXP ( - K * (H - 47.D0) / 270.65D0)
      GO TO 1420

1370  IF (H .gt. 71.) go to 1390
      T  = 270.65D0 - 2.8 * (H - 51.D0)
      PP = .00066063D0 * (270.65D0 / T) ** ( - K / 2.8D0)
      GO TO 1420

1390  IF (H .gt. 84.852D0) THEN
                              kk = 1
                              write(6,200) H
                              return
                         END IF

      T = 214.65D0 - 2.D0 * (H - 71.D0)
      PP = 3.9046D-05 * (214.65D0 / T) ** ( - K / 2.D0)

1420  RR = PP / (T / 288.15D0)
      MU = BT * T**1.5D0 / (T + 110.4D0)
      TS = T / 288.15D0
      A  = AL *  SQRT (TS)
      T  = TL * TS
      R  = RL * RR
      P  = PL * PP
      RM = R * A / MU
      QM = .7 * P

  200 format('   Out of Table in StdAtm- too high !'// &
             4x,'H =',f12.3,'  > 84.852 km'/)

      return
      end subroutine
!************************************************************************************

    subroutine Atmos_Refrac(Elevation,POBS,TOBS,REFR)
!      This code is modified code from NOVAS for true airless calculated
!      items . Atmospheric Refraction Astronomical algorithms, Jean, Meeus!
!*
!*          Elevation  = Elevation DISTANCE of Object IN DEGREES (IN)
!*          OBSP   = OBSERVED ATMOSPHERIC PRESSURE IN MILLIBARS (IN)
!*          OBST   = OBSERVED TEMPERATURE IN DEGREES CELSIUS (IN)
!*          REFR   = ATMOSPHERIC REFRACTION IN DEGREES (OUT)
!*
!*
      real(kind=8) :: Elevation,REFR
      real(kind=8) :: POBS,TOBS
      real(kind=8) :: P,T,H,R

!*     COMPUTE REFRACTION ONLY FOR ZENITH DISTANCES
!*     BETWEEN 0.1 AND 91 DEGREES
      IF ( Elevation <= -1.D0 .OR. Elevation > 89.9D0 ) THEN
          REFR = 0.D0
          return
      END IF
       T = 0.D0
       P = 0.D0
      IF ( POBS > 1.D0 .AND. TOBS > -100.D0 ) THEN
          P  = POBS
          T  = TOBS
      END IF

      H = Elevation
      R = 0.017D0 / DTAN ( ( H +  10.3D0 / ( H + 5.11D0 ) ) * DEGRAD )
      REFR = R * ( 0.280198D0 * P / ( T + 273.15D0 ) )
      return
  end subroutine

!************************************************************************************
function IsLeapYear(Iyear)
!     checks if a Gregory year is leap year or not
!     Iyear, integer Year
!     returns TRUE or FALSE
    logical :: IsLeapYear
    integer :: Iyear
    IsLeapYear = .FALSE.
    if (mod(Iyear,4)==0 .and. mod(Iyear,100)/=0 &
         .or. (mod(Iyear,400)==0)) IsLeapYear = .True.
end  function

function dayOfGyear(Y,M,D)
!     Function returns day of Gregory year
!     from Y as year, M for month and D for day of month
!     From Jean Meeus , Astronomical Algorithms
      integer :: dayOfGyear,Y,M,D,k
      k= 1
      if(IsLeapYear(Y)) k = 2
      dayOfGyear = int(275*M/9) - k*int((M+9)/12) + D - 30
end function

function dayOfPyear(M,D)
!     Function returns day of Iranian year
!     M for month and D for day of month
!     In leap years last day of last month is 30 days
!     By Amin Khiabani
      integer :: dayOfPyear,M,D

      if(M<=6) then
         dayOfPyear = (M-1)*31 + D
      else
          dayOfPyear = (M-7)*30 + D + 186
      end if

end function

integer function GDays_in_month(Iyear, Imonth)
!    returns number of days in Gregory Month for a year
!     Iyear, input, year
!     Imonth, Input number of month in Gregory year
!     Algorithm from Internet

    integer :: Iyear , Imonth

    SELECT CASE (Imonth)     !!Find number of days in a Month

    CASE (1, 3, 5, 7:8, 10, 12)
        GDays_in_month = 31
    CASE (2)                 !!February
        GDays_in_month = 28
        if (IsLeapYear(Iyear)) GDays_in_month = 29  !!Leap year
    CASE DEFAULT                                    !!September, April, June & November
        GDays_in_month = 30    !! Thirty days hath ...^
    END SELECT

end function

 !********************************************************************

integer function IDays_in_month(Iyear, Imonth)
!    returns number of days in Iranian Month for a year
!     Iyear, input, Persian year
!     Imonth, Input number of month in Iranian year

    integer :: Iyear , Imonth
    integer :: Gyear, Marday
    integer, dimension(3) :: Equinox
    real(kind=8) :: UJD, Uhour
    logical :: isLeap

    SELECT CASE (Imonth)     !!Find number of days in a Month

    CASE (1:6)
        IDays_in_month = 31
    CASE (12)                 !!Esfand
        IDays_in_month = 29
        call IranCalendar(IYear,Gyear,UJD,isleap,Equinox,MarDay,UHour)
        if (isleap) IDays_in_month = 30  !!Leap year
    CASE DEFAULT
        IDays_in_month = 30    !! Thirty days
    END SELECT

end function


!****************************************************

subroutine Hour2HMS(Hour, H , M , S)
!     Converts Hours in real numbers to
!     H integer  for Hour, M integer for minute
!     S integer for seconds
      integer :: H , M , S
      Real(kind=8) :: Hour , Rmin, Ss
      H = int(Dmod(Hour,24.D0))
      Rmin = Dmod(Hour ,1.D0)*60.D0
      M = mod(int(Rmin),60)
      Ss = (Rmin - M)*60
      S = mod(int((Rmin - M)*60),60)
      if ((Ss - S)> 0.5D0) S = S + 1
      if(S == 60) then
            S = 0
            M = M +1
      end if
end subroutine

!*********************************************************************
character(LEN=8) function Hour2text(Hour)
    real(kind=8) :: Hour
    integer :: H , M , S
    call Hour2HMS(Hour, H , M , S)
    Hour2text = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                  ':'//trim(I2str(S)))
end function
!*********************************************************************

character(LEN=8) function HMS2text(H , M , S)
    integer :: H , M , S

    HMS2text = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                  ':'//trim(I2str(S)))
end function
!*********************************************************************

character(LEN=9) function JD2WeekDay(TJD, k)
!     Function returns week day from Julian Day
!     TJD real number , Julian Day
!     k integer, specifies week day name for k = 1
!     or k = 2 for abbreviated name week day
      integer :: idw , k
      real(8) :: TJD
      character, dimension(7) :: abweek*3 , week*9

      data week / 'Sunday   ','Monday   ','Tuesday  ', &
                  'Wednesday','Thursday ','Friday   ',&
                  'Saturday '/

      data abweek/'Sun','Mon','Tue','Wed','Thu','Fri','Sat'/

      idw = JD2WeekDayNum(TJD)
      selectcase(k)
      case(1)
            JD2WeekDay = week(idw)
      case(2)
            JD2WeekDay = abweek(idw)
      end select


end function

!**************************************************************************

 function JD2WeekDayNum(TJD)
!     Function returns week day Number from Julian Day
      integer :: JD2WeekDayNum
      real(8) :: TJD

      JD2WeekDayNum = 1 + mod(int(TJD)+2,7)

end function

!**************************************************************************
function HijriMonthName(Hm)
!     Function returns name of Hegirae  month from month number
      integer :: HM ! Month Number
      character(Len=17) :: HijriMonthName
      character(LEN=17), dimension(12),parameter :: MonthName =(/ &
      'Muharram         ', 'Safar            ', 'Rabi-al-awwal    ', &
      'Rabi-ath-thani   ', 'Jumada-al-ula    ', 'Jumada-al-akhirah', &
      'Rajab            ', 'Shaban           ', 'Ramadan          ', &
      'Shawwal          ', 'Dhual-Qadah      ', 'Dhual-Hijjah     '/)

      HijriMonthName = MonthName(HM)

end function
!***********************************************************************

function PersianMonthName(Jm)
!     Returns name of Iranian month name from month number
      integer :: Jm ! Month number
      character(LEN=11) :: PersianMonthName
      CHARACTER(LEN=11) ,dimension(12), PARAMETER :: PersianMTH = &
      (/"Farvardin  ","Ordibehesht","Khordad    ","Tir        ", &
        "Mordad     ","Shahrivar  ","Mehr       ","Aban       ", &
        "Azar       ","Dey        ","Bahman     ","Esfand     "/)

      PersianMonthName = PersianMTH(Jm)

end function
!****************************************************

function GregMonthName(Gm)
!     Returns Gregory Month name  from month number
      integer :: Gm ! Month number
      character(LEN=9) :: GregMonthName
      CHARACTER(9) ,dimension(12), PARAMETER :: MonthNAME =&
      (/"January  ","February ","March    ","April    ",&
        "May      ","June     ","July     ","August   ", &
        "September","October  ","November ","December "/)

      GregMonthName = MonthNAME(Gm)

end function

!---------------------------------------------------------------------------------------
subroutine JD2Hijri(UTJD, HY , HM , HD)
!     Returns Hejri Year , Month and day of month from
!     Julian day number, UTJD
!     UTJD, real number Julian day number
!     HY, integer , Hijri year
!     HM, integer , Hijri Month number
!     HD, integer , Hijri month day
!     Algorithm from Arithmetic or tabular Hijri
!     calendar. It differ lunar based calendar.
!     Middle of Hijri months, gives month and year
!     Translated from Java from Internet
      implicit none

      integer :: HY , HM , HD
      integer :: j , cyc

      real(8) :: UTJD, Year
      real(8) :: shift1, z, JD
      real(8) , parameter :: epochastro = 1948084.D0
  !    real(8) , parameter :: epochcivil = 1948085.D0

      jd = UTJD

      year = 10631.D0/30.D0
      shift1 = 8.01D0/60.D0

      z = jd- epochastro
      cyc = floor(z/10631.D0)
      z = z-10631.D0*cyc
      j = floor((z-shift1)/year)
      HY = 30*cyc+j
      z = z-floor(j*year+shift1)
      HM = floor((z+28.5001D0)/29.5001D0)
      if(HM==13) HM = 12
      HD = floor(z-floor(29.5001D0*HM-29.D0))
      if(HD==0) HD = 1

end subroutine

!**************************************************************************************
      subroutine JD2TradHijri(UTJD,Geo,Atmos,UT_TT,Method,Ilum,Aid,IREF,NMJD,HY,HM,HD,Hour)
      !     Returns Hejri Year , Month and day of month from
      !     Julian day number, UTJD
      !     UTJD, real number Julian day number
      !     HY, integer , Hijri year
      !     HM, integer , Hijri Month number
      !     HD, integer , Hijri month day
      !     Algorithm from Arithmetic or tabular Hijri
      !     calendar. It differ lunar based calendar.
      !     Middle of Hijri months, gives month and year
      !     Translated from Java from Internet
            implicit none

            integer :: HY , HM , HD, Method, Aid
            integer :: UT_TT, IREF
            integer :: IYear,IMonth,IDay
            real(kind=8) :: UTJD, JDE, NMJD, TJD
            real(kind=8) :: Hour, Ilum
            real(kind=8), dimension(4) :: Geo
            real(kind=8), dimension(2) :: Atmos

            call JD2CAL(UTJD,IYear,IMonth,IDay,Hour)
            call Moon_Phases(IYear,IMonth,IDay,0,JDE)
            call HijrifirstDay(JDE,Geo,Atmos,UT_TT,Method,Ilum,Aid,IREF,NMJD,HY,HM)

            TJD = UTJD
            if (NMJD > UTJD) then
                TJD = TJD - 15.D0
                call JD2CAL(TJD,IYear,IMonth,IDay,Hour)
                call Moon_Phases(IYear,IMonth,IDay,0,JDE)
                call HijrifirstDay(JDE,Geo,Atmos,UT_TT,Method,Ilum,Aid,IREF,NMJD,HY,HM)
            end if

            HD = int(dint(UTJD) - dint(NMJD))+1

      end subroutine


!********************************************************************************

function CAL2JD ( IYear,Imonth,Iday )
    integer :: Iyear, Imonth,Iday, J
    real(kind=8) :: TJD, DJM, CAL2JD
    call iau_CAL2JD (Iyear, Imonth,Iday, TJD, DJM, J )

    if(J == 0) then
        CAL2JD = TJD + DJM
    else
        CAL2JD = 0.D0
    end if
end function

!*********************************************************************************

function CAL2JDH ( IYear,Imonth,Iday, Hour )
    integer :: Iyear, Imonth,Iday, J
    real(kind=8) ::CAL2JDH, TJD, DJM, Hour
    call iau_CAL2JD (Iyear, Imonth,Iday, TJD, DJM, J )

    CAL2JDH = 0.D0
    if(J == 0) then
        TJD = TJD + DJM
        CAL2JDH = TJD + Hour/24.D0
    end if

end function

!*********************************************************************************


subroutine JD2CAL(TJD,Iyear,Imonth,Iday,Hour)
    integer :: Iyear,Imonth,Iday, J
    real(kind=8) :: TJD, FD, Hour
    call iau_JD2CAL(TJD,0.D0,Iyear,Imonth,Iday,FD,J)
    if(J == 0) then
        Hour = FD*24.D0
    else
        Iyear = 0
        Imonth = 0
        Iday = 0
        Hour = 0.D0
    end if
end subroutine

!*****************************************************************************
subroutine IranCalendar(Year,Gyear,UJD,isleap,Equinox,MarDay,UHour)
!     In Iranian calendar new year starts at spring equinox
!     <|--------------------Iranian Year-------------<
!     Next Gregory Year    Gregory Year
!     <|--------------|------------------------------<
!     Equinox        Gregory
!     20-22          New year
!     March          1January
!     For any Iranian Year as integer finds
!     UJD new year Julian Day at longitude 52.5
!     Gyear,  Gregory Years corresponding to equinoxes
!     leap, integer leap year . If none of years is leap then it is 0
!     Equinox(3), array of 3 integer of equinox year,month and day
!     MarDay,  March day of corresponding equinoxes
!     UHour, real numbers for time of equinox at 52.5 longitude

      integer :: year, Iy, leap
      integer ::  M,  I, UT_TT
      integer :: MarDay, Gyear, J
      integer, dimension(1:2) :: MDay,Gy
      integer, dimension(3) :: Equinox, Jdate
      real(kind=8), dimension(0:2) :: RTSJD, RTShour
      real(kind=8) :: JDT, JDE, UJD, Uhour, FD
      real(kind=8),dimension(2) :: EJD, Uh, noon
      real(kind=8),dimension(4) :: Geo
      real(kind=8),dimension(2) :: Atmos
      real(kind=8) :: NTJD
      logical :: isLeap

      data atmos / 898.76D0, 10.D0 /
      data Geo / 52.5D0 , 32.5D0 , 1000.D0,3.5D0 /
  !
      UT_TT = 0
      leap = 0

      Iy = Year
      Gyear = 0

      Do I=1,2
            Gy(I) = Iy +621
            JDE = trueJDEquiSolitice(Gy(I), 0) ! Julian date of equinox
            EJD(I) = JDE + Geo(4)/24.D0
            call iau_JD2CAL (EJD(I),0.D0,Gy(I),M,MDay(I),FD,J)
            Uh(I) = FD*24.D0

            JDT = dint(JDE)
            if(dmod(JDE,1.D0) <= 0.5D0) then
                JDT = JDT - 0.5D0
            else
                JDT = JDT + 0.5D0
            end if
            call SunRise_Set_Noon(JDT,JDate,Geo,Atmos,-0.2665694D0,UT_TT,1,RTSJD,RTShour)
            NTJD = RTSJD(1)

            noon(I) = RTShour(1)

            if(I > 1) then
                  if(Uh(I-1) < noon(I-1).and. Uh(I) > noon(I)) then
                        leap = Iy - 1
                  end if
            end if
            Iy = Iy + 1
      End Do

      Gyear = Gy(1)
      MarDay = MDay(1)
      Uhour = Uh(1)
      UJD = EJD(1)
      if(year == leap) then
            isLeap = .true.
        else
            isLeap = .false.
       endif
!     Equinox date in Iranian calendar
      if(Uh(1) <= noon(1)) then
            Equinox(1) = Year
            Equinox(2) = 1
            Equinox(3) = 1
      else
            Equinox(1) = year - 1
            Equinox(2) = 12
            Equinox(3) = 29
            if(Equinox(1) == leap)  Equinox(3) = 30
      end if

end subroutine
!--------------------------------------------------------------------
function IrCal2JD(Iry,Irm,Ird,Hour)
!     returns Julian day from Iranian calendar
!     Iry, integer Iranian year
!     Irm, integer Iranian Month number
!     Ird, integer Iranian Month day number

      integer Iry,Irm,Ird
      integer :: Gyear, MarDay
      integer, dimension(3) :: Equinox
      integer :: days
      real(kind=8) :: IrCal2JD, hour
      real(kind=8) :: UHour, UJD
      logical :: isLeap
!     find first day of year
      call IranCalendar(Iry,Gyear,UJD,isleap,Equinox,MarDay,UHour)
!     and find day number of year and add to Julian Day of first day
!     for first 6 months
      if(Irm <= 6) then
            days = (Irm - 1)*31 + Ird -1
      else
!     remaining months
            days = (Irm - 7)*30 + Ird + 185
      end if

      IrCal2JD = Dint(UJD) + days  + Hour/24.D0 + 0.5D0

end function

 !-------------------------------------------------------------------------

 subroutine JD2IrCal(JD, Iry, Irm, Ird, Hour)
!     returns Iranian calendar from Julian date, JD
!     Iry, integer, Iranian Year
!     Irm, Iranian month number
!     Ird, Iranian day of month
!     get gergory calendar date from Julian day

      integer :: Iry, Irm , Ird ,Gy, Gm, Gd
      integer ::  DifDays, J
      real(kind=8) :: JD, Hour, JD1, FD

 !     call RCALDAT(JD,Gy, Gm, Gd , Hour)
      call iau_JD2CAL (JD,0.D0,Gy,Gm,Gd,FD,J)
      Hour = FD*24.D0
!     Guess Iranian year
      Iry = Gy - 621
      JD1 = IrCal2JD(Iry,1,1,0.D0)
!     to find corresponding Iranian year
!     if JD less then new year Julian Day
!     then it is in Gregory previous year
      if(JD < JD1) then
            Iry = Iry - 1
            JD1 = IrCal2JD(Iry,1,1,0.D0)
      endif
      DifDays = int(JD - JD1)
!     By finding Year , and therefore first day,
!     difference of new year Julian day and JD is day number
!     By finding day number , we can find month and month of day
      if(DifDays <= 185) then
! The first 6 months
             Irm= 1+(DifDays/31)
             Ird=(MOD(DifDays,31)) + 1
             return
      else
! The remaining months
            DifDays=DifDays-186
            Irm=(7+DifDays/30)
            Ird=(MOD(DifDays,30)) + 1
      endif

 end subroutine

 !---------------------------------------------------------------------------

subroutine IrCal2GregCal(Iry,Irm,Ird,Gry,Grm,Grd)
!     coverts Iranian calendar to Gregory calendar
!     Integer, Iranian Year, Iry, Iranian Month number, Irm, and Day number, Ird
!     Integer, Gregory Year, Gry, Gregory month number, Grm, and Day number, Grd
      integer :: Iry,Irm,Ird , Gry,Grm, Grd, J
      real(kind=8) :: UJD, FD
!     find Julian day from Iranian calendar
!     and then convert Julian Day to Gregory Calendar
      UJD = IrCal2JD(Iry,Irm,Ird,0.D0)
    !  call RCALDAT(UJD,Gry,Grm,Grd,H)
      call iau_JD2CAL (UJD,0.D0,Gry,Grm,Grd,FD, J )

end subroutine

!----------------------------------------------------------------------

subroutine GregCal2IrCal(Gry,Grm,Grd,Iry,Irm,Ird)
!     Gregory calendar calendar to coverts Iranian
!     Integer, Iranian Year, Iry, Iranian Month number, Irm, and Day number, Ird
!     Integer, Gregory Year, Gry, Gregory month number, Grm, and Day number, Grd
      integer :: Iry,Irm,Ird , Gry,Grm, Grd, J
      real(kind=8) :: UJD, H, DJM
!     find Julian day from Gregory calendar
!     and then convert Julian Day to Iranian Calendar
      call iau_CAL2JD ( Gry,Grm,Grd, UJD, DJM, J )
      UJD = UJD + DJM
 !     call JULDAT(Gry,Grm,Grd,0.D0,UJD)
      call JD2IrCal(UJD,Iry,Irm,Ird,H)

end subroutine

!---------------------------------------------------------------------

Function equation_time(TJD)
!     function returns equation of time in hours
!     from Jean Meeus Astronomical Algorithms
!     TJD , real number Julian day

    real(kind=8) :: TJD , equation_time, T, L
    real(kind=8) :: R, Alfa, Delta, Landa, eqt
    real(kind=8) :: OBLT,DPSI,DEPS

!    call Pi_DEGRAD(PI, DEGRAD)
!     Ta Time measured in millennial(365250 days)
    T = (TJD - 2451545.D0)/365250.D0

!   L  Sun mean longitude, degrees
    L = 280.4664567D0 + T*(360007.6982779D0 + T*(0.03032028D0 &
  &      + T*(1.D0/49931.D0 - T*(1.D0/15300.D0 - T/2000000.D0))))

    call ChangeAngle(L)

!     find alfa, Sun right ascension
    call Sun_Apparent_Geo(TJD,R,Landa, Alfa, Delta)

!     find Nutation and obliquity
!    Call ETILT (TJD,OBLM,OBLT,EQEQ,DPSI,DEPS)
    call iau_NUT00B ( TJD, 0.D0, DPSI, DEPS )
    DPSI = DPSI/DEGRAD
    OBLT = iau_OBL06 (TJD, 0.D0) + DEPS
!     equation of time
    eqt = (L -0.0057183D0 - alfa + DPSI*Dcos(OBLT))/15.0D0

!     eqt should be less than 20 minutes
    if(Dabs(eqt) < 18.D0/60.D0) then
      equation_time = eqt
    else
      if(eqt > 0.D0) then
            equation_time = eqt - 24.D0
      else
            equation_time = eqt + 24.D0
      end if
    end if

end function

!*****************************************************************************

subroutine DNutate_DObiq_low(TJD,OBLIC,Omega,DNutate,DObliq)
!     calculate low accuracy  Nutation and Obliquity
!     From Jean Meeus Astronomical Algorithms

    real(kind=8) :: TJD, DNutate,DObliq , T , L , Lp, OBLIC
    real(kind=8) :: RL, RLp, Omega, Romega, U

!     T is time, measured in Julian Centuries
     T = ((TJD -2451545.0D0))/36525.D0
     U = T/100.D0
!     Obliquity in degrees
!    OBLIC = third_order_polynomial( 0.001813D0, - 0.00059D0, &
!        -46.8150D0, 84381.448D0, T)/3600.D0

      OBLIC = (84381.448D0+U*(-4680.93+U*(-1.55+U*(1999.25+U*(-51.38-U*(249.67+39.05*U)))))&
   &         +(U**7)*(7.12+U*(27.87+U*(5.79+2.45*U))))/3600.D0
!     Longitude of the ascending node of the Moon's mean orbit
      Omega = fourth_order_polynomial(-1.D0/60616000.D0, 1.D0/467441.D0,&
   &     +0.0020754D0, -1934.1362891D0, 125.0445479D0, T)
    call ChangeAngle(Omega)
!     Mean Longitude of Sun
    L = 280.4666D0 +T*(36000.76983D0 + 0.0003032D0*T)
    call ChangeAngle(L)
!     Mean longitude of the Moon
!    Lp = 218.3165D0 + 481267.8813D0*T
      Lp = fourth_order_polynomial(-1.D0/65194000.D0, 1.D0/538841.D0,&
   &               - 0.0015786D0, 481267.88123421D0, 218.3164477D0, T)
    call ChangeAngle(Lp)

    Romega = Omega*DEGRAD
    RLp = Lp*DEGRAD
    RL = L*DEGRAD

    DNutate = (-17.20D0*Dsin(ROmega) - 1.32D0*Dsin(2*RL) &
   &     - 0.23D0*Dsin(2*RLp) + 0.21D0*Dsin(2*ROmega))

    DObliq = (9.20D0*Dcos(ROmega) + 0.57D0*Dcos(2*RL) &
   &     + 0.10D0*Dcos(2*RLp) - 0.09D0*Dcos(2*ROmega))

end subroutine
!***************************************************************

subroutine Sun_Apparent_Geo_Low(TJD,R,Landa, Alfa, Delta)
!     From Solar Coordinates, Astronomical Algorithms, Jean Meeus
!     Sun Apparent Geocentric Lower accuracy,
!     TJD, Julian day of date real Number
!     Landa, output , real number, Sun mean longitude
!     Alfa, output , real number, sun right ascension
!     Delta, output, real number, sun obliquity of ecliptic

    real(kind=8) :: T, R, Landa, C, RLanda,M
    real(kind=8) :: L0 , Alfa, ROBLT
    real(kind=8) :: TJD,OBLIC,DNutate,DObliq ,Delta
    real(kind=8) :: e, Nu, Omega, Romega

!    call Pi_DEGRAD(PI,DEGRAD)

    T = ((TJD -2451545.0D0))/36525.D0

!     Mean Longitude of Sun
    L0 = 280.46646D0 + 36000.76983D0*T +0.0003032D0*T*T
    call ChangeAngle(L0)

!     Mean Anomaly of Sun
    M = third_order_polynomial( 1.D0/24490000.0D0, - 0.0001536D0, &
   &     35999.0502909D0, 357.5291092D0, T)
    call ChangeAngle(M)

!     Eccentricity of earth orbit
    e = 0.016708634D0 - 0.000042037D0*T - 0.0000001267D0*T*T
!     Sun equation of center
    C = (1.914602D0 - 0.004817D0*T - 0.000014D0*T*T)*Dsin(M*DEGRAD)&
   &     +(0.019993D0 - 0.000101D0*T)*Dsin(2.D0*M*DEGRAD)              &
   &     +0.000289D0*Dsin(3.D0*M*DEGRAD)
!     Sun anomaly
    Nu = M + C
!     Sun radius vector
    R = (1.000001018D0*(1.D0-e*e)) /(1.D0 + e*Dcos(Nu*DEGRAD))

    Call DNutate_DObiq_low(TJD,OBLIC,Omega,DNutate,DObliq)

    Romega = Omega*DEGRAD

    Landa = L0 + C -0.00569D0 - 0.00478D0*Dsin(Romega)
    ROBLT = (OBLIC + 0.00256D0*Dcos(ROmega))*DEGRAD
    RLanda = Landa*DEGRAD

    Alfa = Datan2(Dcos(ROBLT)*Dsin(RLanda),Dcos(RLanda))/DEGRAD
    call ChangeAngle(Alfa)

    Delta = -Dasin(Dsin(ROBLT)*Dsin(RLanda))/DEGRAD

end subroutine

!*************************************************************

function JDEquiSolitice(Year, k )

!     From Equinoxes and Solstice, Astronomical Algorithms, Jean Meeus
!     moderate accuracy
!     Year, input integer number,
!     K, input integer , Equinox or solstice selector
!           k = 0 spring equinox, new year
!           k = 1 summer solstice
!           k = 2 fall equinox
!           k = 3 winter solstice

      integer :: Year , k , I
      real(kind=8) :: JDEquiSolitice
      real(kind=8) ::  T , W , Delta_Lamda
      real(kind=8) :: S , Y , JDE0
      real(kind=8), dimension(24) :: A , B , C

      DATA A /485.D0, 203.D0, 199.D0, 182.D0, 156.D0, 136.D0, &
      &      77.D0, 74.D0, 70.D0, 58.D0, 52.D0, 50.D0, 45.D0, &
      &      44.D0, 29.D0, 18.D0, 17.D0, 16.D0, 14.D0, 12.D0, &
      &      12.D0, 12.D0, 9.D0, 8.D0 /

      DATA B / 324.96D0, 337.23D0, 342.08D0, 27.85D0, 73.14D0, &
       &    171.52D0, 222.54D0, 296.72D0, 243.58D0, 119.81D0, &
       &     297.17D0, 21.02D0, 247.54D0, 325.15D0, 60.93D0, 155.12D0, &
       &     288.79D0, 198.04D0, 199.76D0, 95.39D0, 287.11D0, 320.81D0, &
       &     227.73D0, 15.45D0 /

      DATA C / 1934.136D0, 32964.467D0, 20.186D0, 445267.112D0, 45036.886D0, &
        &    22518.443D0, 65928.934D0, 3034.906D0, 9037.513D0, 33718.147D0,150.678D0, &
        &    2281.226D0, 29929.562D0, 31555.956D0, 4443.417D0, 67555.328D0, 4562.452D0, &
        &    62894.029D0, 31436.921D0, 14577.848D0, 31931.756D0, 34777.259D0, 1222.114D0,&
        &    16859.074D0 /

      JDE0 = 0.D0
      JDEquiSolitice = 0.D0

      if(Year < 1000) then

            Y = Year/1000.D0

      select case(k)
            case(0)

                  JDE0 = fourth_order_polynomial(-0.00071D0,0.00111D0,0.06134D0,&
                  &      365242.13740D0,1721139.29189D0,Y)

            case(1)

                  JDE0 = fourth_order_polynomial(0.00025D0,0.00907D0,-0.05323D0,&
                   &     365241.72562D0,1721233.25401D0,Y)

            case(2)

                  JDE0 = fourth_order_polynomial(0.00074D0,-0.00297D0,-0.11677D0,&
                   &     365242.49558D0,1721325.70455D0,Y)

            case(3)

                  JDE0 = fourth_order_polynomial(-0.00006D0,-0.00933D0 ,-0.00769D0,&
                   &     365242.88257D0,1721414.39987D0,Y)

      end select

      elseif(Year >= 1000) then
            Y = (Year -2000)/1000.D0

      select case(k)
            case(0)

                  JDE0 = fourth_order_polynomial(-0.00057D0,-0.00411D0,0.05169D0,&
                    &    365242.37404D0,2451623.80984D0,Y)

            case(1)

                  JDE0 = fourth_order_polynomial(- 0.00030D0,0.00888D0,0.00325D0,&
                    &    365241.62603D0,2451716.56767D0,Y)

            case(2)

                  JDE0 = fourth_order_polynomial(0.00078D0,0.00337D0,-0.11575D0,&
                    &    365242.01767D0,2451810.21715D0,Y)

            case(3)

                  JDE0 = fourth_order_polynomial(0.00032D0,-0.00823D0 ,-0.06223D0,&
                    &    365242.74049D0,2451900.0592D0,Y)

      end select
      end if

      T = (JDE0 - 2451545.D0)/36525.D0

      S = 0.D0

      do I = 1 , 24
            S = S + A(I)*Dcos((B(I) + C(I)*T)*DEGRAD)
      end do

      W = (35999.373D0*T - 2.47D0)*DEGRAD

      Delta_Lamda = 1.D0 + 0.0334D0*Dcos(W) + 0.0007D0*Dcos(2.D0*W)

      JDEquiSolitice = JDE0 + 0.00001D0*S/Delta_Lamda

end function

!---------------------------------------------------------------------------

!C     This FUNCTION calculates TT-UT = DELTA_T
!C     Polynomials are from NASA eclipse website
!C     http://eclipse.gsfc.nasa.gov/SEcat5/deltatpoly.html

      function DELTAT(IYEAR,IMONTH)

      real(kind=8)  Y, DELTAT , T
      INTEGER IYEAR, IMONTH

      DELTAT = 0.D0

      Y = real(IYEAR,kind=8) + (real(IMONTH,kind=8) - 0.5D0)/12.D0

      IF (IYEAR .GE. 1600 .AND. IYEAR .LT. 1700 ) THEN

         T = Y - 1600.D0
         DELTAT = 120.D0- 0.9808D0*T - 0.01532D0*T**2 + T**3 /7129.D0

      ELSE IF (IYEAR .GE. 1700 .AND. IYEAR .LT. 1800 ) THEN

         T = Y - 1700.D0
         DELTAT = 8.83D0 + 0.1603D0*T - 0.0059285D0* T**2 + &
     &   0.00013336D0* T**3 - T**4 / 1174000.D0

      ELSE IF (IYEAR .GE. 1800 .AND. IYEAR .LT. 1860 ) THEN

         T = Y - 1800.D0
         DELTAT = 13.72D0 - 0.332447D0*T + 0.0068612D0*T**2 + &
     &   0.0041116D0*T**3 - 0.00037436D0*T**4 + 0.0000121272D0*T**5 - &
     &   0.0000001699D0*T**6 + 0.000000000875D0*T**7

       ELSE IF (IYEAR .GE. 1860 .AND. IYEAR .LT. 1900 ) THEN

         T = Y - 1860.D0
         DELTAT = 7.62D0 + 0.5737D0*T - 0.251754D0*T**2 + &
     &   0.01680668D0*T**3 -0.0004473624D0*T**4 + T**5/233174.D0

      ELSE IF (IYEAR .GE. 1900 .AND. IYEAR .LT. 1920 ) THEN

         T = Y - 1900.D0
         DELTAT = -2.79D0 + 1.494119D0*T - 0.0598939D0*T**2 + &
     &   0.0061966D0*T**3 - 0.000197D0*T**4

      ELSE IF (IYEAR .GE. 1920 .AND. IYEAR .LT. 1941 ) THEN

         T = Y - 1920.D0
         DELTAT = 21.20D0 + 0.84493D0*T - 0.076100D0*T**2 + &
     &   0.0020936D0*T**3

      ELSE IF (IYEAR .GE. 1941 .AND. IYEAR .LT. 1961 ) THEN

         T = Y - 1950.D0
         DELTAT = 29.07D0 + 0.407D0*T - T**2/233.D0 + T**3/2547.D0

      ELSE IF (IYEAR .GE. 1961 .AND. IYEAR .LT. 1986 ) THEN

         T = Y - 1975.D0
         DELTAT = 45.45D0 + 1.067D0*T - T**2/260.D0 - T**3 /718.D0

      ELSE IF (IYEAR .GE. 1986 .AND. IYEAR .LT. 2005 ) THEN

         T = Y - 2000.D0
         DELTAT = 63.86D0 + 0.3345D0*T - 0.060374D0*T**2 + &
     &   0.0017275D0*T**3 + 0.000651814D0*T**4 + 0.00002373599D0*T**5

      ELSE IF (IYEAR .GE. 2005 .AND. IYEAR .LT. 2050 ) THEN

         T = Y - 2000.D0
         DELTAT = 62.92D0 + 0.32217D0*T + 0.005589D0*T**2

      ELSE IF (IYEAR .GE. 2050 .AND. IYEAR .LT. 2150 ) THEN

         DELTAT = -20.D0 + 32.D0 * ((Y-1820.D0)/100.D0)**2 - &
     &   0.5628D0 * (2150.D0 - Y)

      END IF

      RETURN
      END function

    Real(kind=8) FUNCTION ANMP ( A )
!*  Normalize angle into the range -pi <= A < +pi.

    IMPLICIT NONE

    real(kind=8)  :: A , W

    real(kind=8), PARAMETER :: DPI = 3.141592653589793238462643D0
    real(kind=8), PARAMETER :: D2PI = 6.283185307179586476925287D0

    W = MOD(A,D2PI)
    IF ( ABS(W) >= DPI ) W = W - SIGN(D2PI,A)
    ANMP = W

    END function

!***************************************************************************************

subroutine Moon_Mean_Ecliptic(T,Omega)
    real(kind=8)  :: T, Omega !, fourth_order_polynomial
!     From Nutation and Obliquity, Astronomical Algorithms, Jean Meeus
    Omega = fourth_order_polynomial(-1.D0/60616000.D0, 1.D0/467441.D0,&
    &    +0.0020754D0, -1934.1362891D0, 125.0445479D0, T)
    call ChangeAngle(Omega)
end subroutine

!**********************************************************

subroutine Mean_Anomaly_Sun(T, M)
    real(kind=8) :: T, M !, third_order_polynomial
!     From Nutation and Obliquity, Astronomical Algorithms, Jean Meeus
    M = third_order_polynomial( 1.D0/24490000.0D0, - 0.0001536D0, &
    &    35999.0502909D0, 357.5291092D0, T)
    call ChangeAngle(M)
end subroutine
!*********************************************************
subroutine Mean_Anomaly_Moon(T, Mp)
    real(kind=8) :: T, Mp !, fourth_order_polynomial
!     From Nutation and Obliquity, Astronomical Algorithms, Jean Meeus
    Mp = fourth_order_polynomial( -1.D0/14712000.D0, 1.D0/69699.D0,&
  &  .0087414D0, 477198.8675055D0, 134.9633964D0, T)
    call ChangeAngle(Mp)
end subroutine
!**********************************************************
subroutine Moon_Argument_Latitude(T, F)
    real(kind=8) :: T, F !, fourth_order_polynomial
!     From Nutation and Obliquity, Astronomical Algorithms, Jean Meeus
    F = fourth_order_polynomial(1.D0/863310000.D0, -1.D0/3526000.D0,&
    & - 0.0036539D0, 483202.0175233D0, 93.272095D0, T)
    call ChangeAngle(F)
end subroutine
!**********************************************************

subroutine Moon_Sun_Elongation(T,D)
    real(kind=8) :: T, D !, fourth_order_polynomial
!     From Nutation and Obliquity, Astronomical Algorithms, Jean Meeus
    D = fourth_order_polynomial(-1.D0/113065000.D0, 1.D0/545868.D0,&
     - 0.0018819D0, 445267.1114034D0, 297.8501921D0, T)
    call ChangeAngle(D)
end subroutine

!***********************************************************

subroutine Moon_Mean_Longitude(T , Lp)
    real(kind=8) :: T, Lp !, fourth_order_polynomial
!    From Position of then Moon, Astronomical Algorithms, Jean Meeus
    Lp = fourth_order_polynomial(-1.D0/65194000.D0, 1.D0/538841.D0,&
     - 0.0015786D0, 481267.88123421D0, 218.3164477D0, T)
    call ChangeAngle(Lp)

end subroutine

!**********************************************************

subroutine Moon_Mean_Perigee(T , B_PI)
    real(kind=8) :: T, B_PI !, fourth_order_polynomial
!    From Nutation and Obliquity, Astronomical Algorithms, Jean Meeus
    B_PI = fourth_order_polynomial(1.D0/18999000.D0,-1.D0/80053.D0,&
    & -0.0103200D0, 4069.0137287D0,83.3532465D0, T)
    call ChangeAngle(B_PI)

end subroutine

!**********************************************************

subroutine A1_3 ( T, A1, A2, A3)
!   From Position of the Moon, Astronomical Algorithms, Jean Meeus
!   Three more arguments
    real(kind=8) T, A1, A2, A3

    A1= 119.75D0 + 131.749D0*T
    call ChangeAngle(A1)
    A2 = 53.09D0 + 479264.190D0*T
    call ChangeAngle(A2)
    A3 = 313.45D0 + 481266.484D0*T
    call ChangeAngle(A3)
end subroutine

subroutine Aberat(R,Dtau)
!    From Solar Coordinates, Astronomical Algorithms, Jean Meeus
    real(kind=8) :: R , Dtau
    Dtau = -20.4898D0/(3600.D0*R)
end subroutine

!************************************************************

subroutine ecliptic_to_equitorial(Longitude, Obliquity,latitude, Alfa, delta)
!   From Transformation of coordinates, Astronomical Algorithms, Jean Meeus
    real(kind=8) :: Longitude, Obliquity,latitude, Alfa, delta
    real(kind=8) :: Rlanda, Rtilt, Rbeta

!    call Pi_DegRad(PI, DegRad)

    Rlanda = Longitude*DegRad
    Rtilt = Obliquity*DegRad
    Rbeta = latitude*DegRad

    Alfa = datan2(Dsin(Rlanda)*Dcos(Rtilt)-Dtan(Rbeta)*Dsin(Rtilt), &
        Dcos(Rlanda))/ DegRad

   delta = Dasin(Dsin(Rbeta)*Dcos(Rtilt) + Dcos(Rbeta)*Dsin(Rtilt)*&
            Dsin(Rlanda))/DegRad

end subroutine

!************************************************************************

subroutine horizontal_coords(Geo,HourAngle,AstroLat,Atmos,Elev,Azim,Iref)
!   From Transformation of coordinates, Astronomical Algorithms, Jean Meeus
    real(kind=8) :: HourAngle,AstroLat,Azim,REFR
    real(kind=8) :: RH, R_lat, Rdel, Elev
    real(kind=8) :: x,y,Phi_p,Rearth
    real(kind=8), dimension(4) :: Geo
    real(kind=8), dimension(2) :: Atmos

    integer :: Iref

    call GeoGlob(Geo,x,y,Phi_p,Rearth)

    RH = HourAngle*DegRad
    R_lat = Phi_p*DegRad
    Rdel = AstroLat*DegRad

    Elev = (Dasin(Dsin(R_lat)*Dsin(Rdel) + &
        Dcos(R_lat)*Dcos(Rdel)*Dcos(RH)))/DegRad

    REFR = 0.D0
    if (Iref /= 0) then
        call Atmos_Refrac(Elev,Atmos(1),Atmos(2),REFR)
    end if

    Elev =  Elev +  REFR

    Azim = Datan2(Dsin(RH), Dcos(RH)*Dsin(R_lat)&
        - Dtan(Rdel)*Dcos(R_lat))/DegRad

end subroutine

subroutine parallax_correction(Dis_Delta,Geo,Hour_angle,alfa,delta)
! From Correction of parallax, Astronomical Algorithms, Jean Meeus
! Distance in astronomical unit (AU)
! Geo array of geographic location, Longitude, latitude, elevation , Time zone
! Hour_angle in degree
! alfa Equatorial longitude in degree
! delta Equatorial latitude in degree

    real(kind=8) :: Distance, Hour_angle,alfa,delta
    real(kind=8) :: Pa ,x ,y ,Rearth, Phi_p
    real(kind=8) :: RH , RD, Del_alfa, AUKM, Dis_Delta
    real(kind=8), dimension(4) :: Geo

    call GeoGlob(Geo , x , y , Phi_p, Rearth)

 !   CALL ASTCON ('AU',1.D-3,AUKM)
    AUKM = 149597870.7D0
    Distance = Dis_Delta/AUKM

    Pa = Dsin(8.794D0*DegRad/(3600.D0*Distance))

    RH = Hour_angle*DegRad
    RD = delta*DegRad

    Del_alfa = Datan2(-x*Pa*Dsin(RH),Dcos(RD)-x*Pa*Dcos(RH))

    alfa = alfa + Del_alfa/DegRad

    delta = datan2((Dsin(RD)-y*Pa)*Dcos(Del_alfa),&
        Dcos(RD)-x*Pa*dcos(RH))/DegRad

    Hour_angle = atan2(dcos(RD)*dsin(RH),dcos(RD)*dcos(RH)- &
      x*Pa)/DegRad

    Call ChangeAngle(Hour_Angle)

end subroutine
!----------------------------------------------------------------------------

subroutine Moon_LonG_Lat_Distance(TJD,Dis_Delta,Lunar_Pi,Ap_Landa,Alfa,delta)

    real(kind=8) :: TJD , Ap_Landa,Alfa, delta, Dis_delta
    real(kind=8) :: T, Landa, Beta, DEPS
    real(kind=8) :: OBLT, DPSI, Lunar_PI

!   call (PI, DegRad)

   T = ((TJD -2451545.0D0))/36525.D0

   call Moon_Mean_LonG_Lat_Dist(T, Landa, Beta , Dis_Delta, Lunar_Pi)
!   call ETILT (TJD,OBLM,OBLT,EQEQ,DPSI,DEPS)
   call iau_NUT00B ( TJD, 0.D0, DPSI, DEPS )
   Ap_Landa = Landa + DPSI/DEGRAD
   OBLT = (iau_OBL06 (TJD, 0.D0)+DEPS )/DEGRAD
   call ecliptic_to_equitorial(Ap_Landa, OBLT,Beta, Alfa, delta)
   !call ChangeAngle(delta)

end subroutine


!**************************************************************

subroutine Moon_Mean_LonG_Lat_Dist(T , Landa, Beta , Delta, Lunar_Pi)
!   From Position of the Moon, Astronomical Algorithms, Jean Meeus
!   T Julian day number in centuries
!   Landa Moon Longitude, in Degrees
!   ecliptic latitude, in degrees
!   Delta, Earth, Moon distance in Kilometers
!   Moon Equatorial horizontal parallax, in radians
    real(kind=8) :: T , Landa, Beta , Delta, Lunar_Pi
    real(kind=8) :: E , Sum_l , Sum_b , Sum_r
    real(kind=8) :: Lp , D , M , Mp , F , A1 , A2 , A3
    real(kind=8) :: Angle1, Angle2
    real(kind=8) , dimension(60,4) :: I_Terms
    real(kind=8) , dimension(60) :: L_Terms
    real(kind=8) , dimension(60) :: R_Terms
    real(kind=8) , dimension(60,5) :: B_Terms

    integer :: I, J

    DATA ( ( I_Terms(J,I), I=1,4 ), J=  1, 60) /&
    &    0.D0, 0.D0, 1.D0 ,0.D0,&
    &    2.D0, 0.D0,-1.D0 ,0.D0,&
    &    2.D0, 0.D0, 0.D0 ,0.D0,&
    &    0.D0, 0.D0, 2.D0 ,0.D0,&
    &    0.D0, 1.D0, 0.D0 ,0.D0,&
    &    0.D0, 0.D0, 0.D0 ,2.D0,&
    &    2.D0, 0.D0,-2.D0 ,0.D0,&
    &    2.D0,-1.D0,-1.D0 ,0.D0,&
    &    2.D0, 0.D0, 1.D0 ,0.D0,&
    &    2.D0,-1.D0, 0.D0 ,0.D0,&
    &    0.D0, 1.D0,-1.D0 ,0.D0,&
    &    1.D0, 0.D0, 0.D0 ,0.D0,&
    &    0.D0, 1.D0, 1.D0 ,0.D0,&
    &    2.D0, 0.D0, 0.D0 ,-2.D0,&
    &    0.D0, 0.D0, 1.D0 , 2.D0,&
    &    0.D0, 0.D0, 1.D0 ,-2.D0,&
    &    4.D0, 0.D0,-1.D0 ,0.D0,&
    &    0.D0, 0.D0, 3.D0 ,0.D0,&
    &    4.D0, 0.D0,-2.D0 ,0.D0,&
    &    2.D0, 1.D0,-1.D0 ,0.D0,&
    &    2.D0, 1.D0, 0.D0 ,0.D0,&
    &    1.D0, 0.D0,-1.D0 ,0.D0,&
    &    1.D0, 1.D0, 0.D0 ,0.D0,&
    &    2.D0,-1.D0, 1.D0 ,0.D0,&
    &    2.D0, 0.D0, 2.D0 ,0.D0,&
    &    4.D0, 0.D0, 0.D0 ,0.D0,&
    &    2.D0, 0.D0,-3.D0 ,0.D0,&
    &    0.D0, 1.D0,-2.D0 ,0.D0,&
    &    2.D0, 0.D0,-1.D0 ,2.D0,&
    &    2.D0,-1.D0,-2.D0 ,0.D0,&
    &    1.D0, 0.D0, 1.D0 ,0.D0,&
    &    2.D0,-2.D0, 0.D0 ,0.D0,&
    &    0.D0, 1.D0, 2.D0 ,0.D0,&
    &    0.D0, 2.D0, 0.D0 ,0.D0,&
    &    2.D0,-2.D0,-1.D0 ,0.D0,&
    &    2.D0, 0.D0, 1.D0 ,-2.D0,&
    &    2.D0, 0.D0, 0.D0 ,2.D0,&
    &    4.D0,-1.D0,-1.D0 ,0.D0,&
    &    0.D0, 0.D0, 2.D0 ,2.D0,&
    &    3.D0, 0.D0,-1.D0 ,0.D0,&
    &    2.D0, 1.D0, 1.D0 ,0.D0,&
    &    4.D0,-1.D0,-2.D0 ,0.D0,&
    &    0.D0, 2.D0,-1.D0 ,0.D0,&
    &    2.D0, 2.D0,-1.D0 ,0.D0,&
    &    2.D0, 1.D0,-2.D0 ,0.D0,&
    &    2.D0,-1.D0, 0.D0 ,-2.D0,&
    &    4.D0, 0.D0, 1.D0 ,0.D0,&
    &    0.D0, 0.D0, 4.D0 ,0.D0,&
    &    4.D0,-1.D0, 0.D0 ,0.D0,&
    &    1.D0, 0.D0, -2.D0 ,0.D0,&
    &    2.D0, 1.D0, 0.D0 ,-2.D0,&
    &    0.D0, 0.D0, 2.D0 ,-2.D0,&
    &    1.D0, 1.D0, 1.D0 ,0.D0,&
    &    3.D0, 0.D0,-2.D0 ,0.D0,&
    &    4.D0, 0.D0,-3.D0 ,0.D0,&
    &    2.D0,-1.D0, 2.D0 ,0.D0,&
    &    0.D0, 2.D0, 1.D0 ,0.D0,&
    &    1.D0, 1.D0,-1.D0 ,0.D0,&
    &    2.D0, 0.D0, 3.D0 ,0.D0,&
    &    2.D0, 0.D0,-1.D0 ,-2.D0/

   data L_Terms / 6288774.D0, 1274027.D0, 658314.D0, &
    &    213618.D0, -185116.D0, -114332.D0, 58793.D0, &
    &    57066.D0, 53322.D0, 45758.D0, -40923.D0,-34720.D0,&
    &    -30383.D0, 15327.D0, -12528.D0, 10980.D0,10675.D0,&
    &    10034.D0, 8548.D0, -7888.D0, -6766.D0, -5163.D0, &
    &    4987.D0, 4036.D0, 3994.D0, 3861.D0, 3665.D0,-2689.D0,&
    &    -2602.D0,2390.D0, -2348.D0, 2236.D0, -2120.D0,-2069.D0,&
    &    2048.D0, -1773.D0, -1595.D0, 1215.D0, -1110.D0,-892.D0,&
    &    -810.D0, 759.D0, -713.D0, -700.D0, 691.D0,596.D0,549.D0,&
    &    537.D0, 520.D0, -487.D0, -399.D0,-381.D0,351.D0,-340.D0,&
    &    330.D0,327.D0, -323.D0, 299.D0, 294.D0, 0.D0 /

    data R_Terms / -20905355.D0, -3699111.D0, -2955968.D0,&
    &    -569925.D0, 48888.D0, -3149.D0, 246158.D0, -152138.D0,&
    &    -170733.D0, -204586.D0, -129620.D0,108743.D0,104755.D0,&
    &    10321.D0, 0.D0, 79661.D0, -34782.D0, -23210.D0,-21636.D0,&
    &    24208.D0, 30824.D0,-8379.D0,-16675.D0,-12831.D0,-10445.D0,&
    &    -11650.D0, 14403.D0,-7003.D0,0.D0,10056.D0,6322.D0,-9884.D0,&
    &    5751.D0, 0.D0, -4950.D0, 4130.D0,0.D0,-3958.D0,0.D0,3258.D0,&
    &    2616.D0,-1897.D0,-2117.D0,2354.D0,0.D0,0.D0,-1423.D0,-1117.D0,&
    &    -1571.D0,-1739.D0,0.D0,-4421.D0,0.D0,0.D0,0.D0,0.D0,1165.D0,&
    &    0.D0,0.D0,8752.D0/

    DATA ( ( B_Terms(J,I), I=1,5), J=  1, 60) /&
    &    0.D0, 0.D0, 0.D0 ,1.D0,5128122.D0,&
    &    0.D0, 0.D0, 1.D0 ,1.D0,280602.D0,&
    &    0.D0, 0.D0, 1.D0 ,-1.D0,277693.D0,&
    &    2.D0, 0.D0, 0.D0 ,-1.D0,173237.D0,&
    &    2.D0, 0.D0,-1.D0 ,1.D0,55413.D0,&
    &    2.D0, 0.D0,-1.D0 ,-1.D0,46271.D0,&
    &    2.D0, 0.D0, 0.D0 ,1.D0,32573.D0,&
    &    0.D0, 0.D0, 2.D0 ,1.D0,17198.D0,&
    &    2.D0, 0.D0, 1.D0 ,-1.D0,9266.D0,&
    &    0.D0, 0.D0, 2.D0 ,-1.D0,8822.D0,&
    &    2.D0,-1.D0, 0.D0 ,-1.D0,8216.D0,&
    &    2.D0, 0.D0,-2.D0 ,-1.D0,4324.D0,&
    &    2.D0, 0.D0, 1.D0 ,1.D0,4200.D0,&
    &    2.D0, 1.D0, 0.D0 ,-1.D0,-3359.D0,&
    &    2.D0,-1.D0,-1.D0 , 1.D0,2463.D0,&
    &    2.D0,-1.D0, 0.D0 ,1.D0,2211.D0,&
    &    2.D0,-1.D0,-1.D0 ,-1.D0,2065.D0,&
    &    0.D0, 1.D0,-1.D0 ,-1.D0,-1870.D0,&
    &    4.D0, 0.D0,-1.D0 ,-1.D0,1828.D0,&
    &    0.D0, 1.D0, 0.D0 ,1.D0,-1794.D0,&
    &    0.D0, 0.D0, 0.D0 ,3.D0,-1749.D0,&
    &    0.D0, 1.D0,-1.D0 ,1.D0,-1565.D0,&
    &    1.D0, 0.D0, 0.D0 ,1.D0,-1491.D0,&
    &    0.D0, 1.D0, 1.D0 ,1.D0,-1475.D0,&
    &    0.D0, 1.D0, 1.D0 ,-1.D0,-1410.D0,&
    &    0.D0, 1.D0, 0.D0 ,-1.D0,-1344.D0,&
    &    1.D0, 0.D0, 0.D0 ,-1.D0,-1335.D0,&
    &    0.D0, 0.D0, 3.D0 ,1.D0,1107.D0,&
    &    4.D0, 0.D0, 0.D0 ,-1.D0,1021.D0,&
    &    4.D0, 0.D0,-1.D0 ,1.D0,833.D0,&
    &    0.D0, 0.D0, 1.D0 ,-3.D0,777.D0,&
    &    4.D0, 0.D0,-2.D0 ,1.D0,671.D0,&
    &    2.D0, 0.D0, 0.D0 ,-3.D0,607.D0,&
    &    2.D0, 0.D0, 2.D0 ,-1.D0,596.D0,&
    &    2.D0,-1.D0, 1.D0 ,-1.D0,491.D0,&
    &    2.D0, 0.D0,-2.D0 ,1.D0,-451.D0,&
    &    0.D0, 0.D0, 3.D0 ,-1.D0,439.D0,&
    &    2.D0, 0.D0, 2.D0 ,1.D0,422.D0,&
    &    2.D0, 0.D0, -3.D0 ,-1.D0,421.D0,&
    &    2.D0, 1.D0,-1.D0 ,1.D0,-366.D0,&
    &    2.D0, 1.D0, 0.D0 ,1.D0,-351.D0,&
    &    4.D0, 0.D0, 0.D0 ,1.D0,331.D0,&
    &    2.D0,-1.D0, 1.D0 ,1.D0,315.D0,&
    &    2.D0,-2.D0, 0.D0 ,-1.D0,302.D0,&
    &    0.D0, 0.D0, 1.D0 ,3.D0,-283.D0,&
    &    2.D0, 1.D0, 1.D0 ,-1.D0,-229.D0,&
    &    1.D0, 1.D0, 0.D0 ,-1.D0,223.D0,&
    &    1.D0, 1.D0, 0.D0 ,1.D0,223.D0,&
    &    0.D0, 1.D0,-2.D0 ,-1.D0,-220.D0,&
    &    2.D0, 1.D0,-1.D0 ,-1.D0,-220.D0,&
    &    1.D0, 0.D0, 1.D0 ,1.D0,-185.D0,&
    &    2.D0,-1.D0,-2.D0 ,-1.D0,181.D0,&
    &    0.D0, 1.D0, 2.D0 ,1.D0,-177.D0,&
    &    4.D0, 0.D0,-2.D0 ,-1.D0,176.D0,&
    &    4.D0,-1.D0,-1.D0 ,-1.D0,166.D0,&
    &    1.D0, 0.D0, 1.D0 ,-1.D0,-164.D0,&
    &    4.D0, 0.D0, 1.D0 ,-1.D0,132.D0,&
    &    1.D0, 0.D0,-1.D0 ,-1.D0,-119.D0,&
    &    4.D0,-1.D0, 0.D0 ,-1.D0,115.D0,&
    &    2.D0,-2.D0, 0.D0 ,1.D0,107.D0/

    E = 1.D0 -0.002516D0*T - 0.0000074D0*T*T

!    call Pi_DegRad(PI, DegRad)
    call Moon_Mean_Longitude(T , Lp)
    call Mean_Anomaly_Sun(T, M)
    call Mean_Anomaly_Moon(T, Mp)
    call Moon_Sun_Elongation(T,D)
    call Moon_Argument_Latitude(T, F)
    call A1_3 ( T, A1, A2, A3)

    Sum_l = 0.D0
    Sum_r = 0.D0
    Sum_b = 0.D0

    do I = 1 , 60

        Angle1 = (I_Terms(I,1)*D + I_Terms(I,2)*M + I_Terms(I,3)*Mp &
            + I_Terms(I,4)*F)*DegRad


        Sum_l = Sum_l + L_Terms(I)*Dsin(Angle1)*E**(dabs(I_Terms(I,2)))
        Sum_r = Sum_r + R_Terms(I)*Dcos(Angle1)*E**(dabs(I_Terms(I,2)))

        Angle2 = (B_Terms(I,1)*D + B_Terms(I,2)*M + B_Terms(I,3)*Mp &
            + B_Terms(I,4)*F)*DegRad

        Sum_b = Sum_b + B_Terms(I,5)*Dsin(Angle2)
    end do

   Sum_l = Sum_l + 3958.D0*Dsin(A1*DegRad) + 1962.D0*Dsin((Lp-F)*DegRad)&
            + 318.D0*Dsin(A2*DegRad)

   Sum_b = Sum_b -2235.D0*Dsin(Lp*DegRad) + 382.D0*Dsin(A3*DegRad) + &
            175.D0*Dsin((A1-F)*DegRad) + 175.D0*Dsin((A1+F)*DegRad)&
            + 127.D0*Dsin((Lp-Mp)*DegRad) -115.D0*Dsin((Lp+Mp)*DegRad)

    Landa = Lp + Sum_l / 1.D6
    Beta = Sum_b / 1.D6
    Delta = 385000.56 + Sum_r/1.D3
    Lunar_Pi = dasin(6378.14D0/Delta)

end subroutine

!****************************************************

subroutine Moon_Phases(Year,Month,Day,New_Full,JDE)
!   From Phases of the Moon, Astronomical Algorithms, Jean Meeus
!   Gregory Year , integer
!   Gregory Month , integer
!   New_Full , Integer
!     0 for new Moon
!     1 for first half Moon
!     2 for full Moon
!     3 for last half Moon
!   JDE , output Julian day number TT or UTC , , Real number
    integer ::  New_Full , I, J,  q , Year, month,Day, L

    real(kind=8) :: k, T
    real(kind=8):: JDE, M , M_p, F, E, Omega, A_cor, Correction
    real(kind=8):: Sum_cor, year1 , W
    real(kind=8 ), dimension(7) :: New_moon, Full_moon, CF
    real(kind=8) , dimension(18) :: NF
    real(kind=8) , dimension(25) :: QM
    real(kind=8) , dimension(14,3) :: A

      DATA ( ( A(J,q), q=1,3), J=  1, 14 ) /&
      &  299.77D0,0.107408D0,0.000325D0,&
      &  251.88D0,0.016321D0,0.000165D0,&
      &  251.83D0,26.651886D0,0.000164D0,&
      &  349.42D0,36.412478D0,0.000126D0,&
      &  84.66D0,18.206239D0,0.000110D0,&
      &  141.74D0,53.303771D0,0.000062D0,&
      &  207.14D0,2.453732D0,0.000060D0,&
      &  154.84D0,7.30686D0,0.000056D0,&
      &  34.52D0,27.261239D0,0.000047D0,&
      &  207.19D0,0.121824D0,0.000042D0,&
      &  291.34D0,1.844379D0,0.000040D0,&
      &  161.72D0,24.198154D0,0.000037D0,&
      &  239.56D0,25.513099D0,0.000035D0,&
      &  331.55D0,3.592518D0,0.000023D0/

      data New_moon /-0.4072D0,0.17241D0,0.01608D0,0.01039D0, &
     &   0.00739D0,-0.00514D0,0.00208D0/

      data Full_moon /-0.40614D0,0.17302D0,0.01614D0,0.01043D0, &
     &  0.00734D0,-0.00515D0,0.00209D0/

      data NF /  -0.00111D0,-0.00057D0,0.00056D0,-0.00042D0, &
       &        0.00042D0,0.00038D0,-0.00024D0, -0.00017D0, &
       &        -0.00007D0,0.00004D0,0.00004D0,0.00003D0, &
       &         0.00003D0,-0.00003D0,0.00003D0,-0.00002D0, &
       &         -0.00002D0, 0.00002D0/

      DATA QM / -0.62801D0, 0.17172D0 , -0.01183D0,0.00862D0 &
        &          , 0.00804D0, 0.00454D0, 0.00204D0, -0.0018D0 &
        &          , -0.0007D0, -0.00040D0, -0.00034D0, 0.00032D0 &
        &          , 0.00032D0, -0.00028D0, 0.00027D0, -0.00017D0 &
        &          , -0.00005D0, 0.00004D0, -0.00004D0, 0.00004D0 &
        &          , 0.00003D0, 0.00003D0, 0.00002D0, 0.00002D0 &
        &          , -0.00002D0 /


      Correction = 0
!     Find fraction of year
      Year1 = real(Year,kind=8) + real(Month-1,kind=8)/12.D0 &
            + real(Day,kind=8)/365.25D0

      k = (year1 -2000.D0)*12.3685D0
      k =  k - Dmod(k,1.D0)

      if (K<0) then
            K=K-1.D0
      end if

      if(New_Full == 1) then
            k = k + 0.25D0
      elseif(New_Full == 2)then
            k = k + 0.5D0
      elseif(New_Full == 3) then
            k = k + 0.75D0
      end if

      T = k/1236.85D0

      JDE = 2451550.09766D0 + 29.530588861D0*k &
    &    + (0.00015437D0 +(-0.000000150D0+0.00000000073D0*T)*T)*T*T

      A_Cor = A(1,1)+A(1,2)*k-0.009173*T*T
      !call ChangeAngle(A_cor)

      Sum_cor= Dsin(A_cor*DegRad)*A(1,3)

      do l = 2,14

            A_cor = A(l,1)+A(l,2)*k
            !call ChangeAngle(A_cor)
            Sum_cor = Sum_cor + Dsin(A_cor*DegRad)*A(l,3)
      end do

      JDE = JDE + Sum_cor

      E = 1.D0 -(0.002516D0 - 0.0000074*T)*T

      M = 2.5534D0 + 29.1053567*k -(0.0000014D0+0.00000011D0*T)*T*T

      call ChangeAngle(M)
      M = M*Degrad

      M_p = 201.5643+385.81693528*k+(0.0107582D0+(0.00001238D0-0.000000058D0*T)*T)*T*T

      call ChangeAngle(M_p)
      M_p = M_p*DegRad

      F = 160.7108D0+390.67050284D0*k+(-0.0016118D0+(-0.00000227+0.000000011D0*T)*T)*T*T

      call ChangeAngle(F)
      F = F*DegRad

      Omega = 124.7746D0-1.56375588D0*k + (0.0020672D0+0.00000215D0*T)*T*T

      call ChangeAngle(Omega)
      Omega = Omega*DegRad

      if (New_Full == 0 .or. New_Full == 2 ) then

            if (New_Full ==0 ) then

                  CF= New_moon

            elseif(New_Full == 2) then

                  CF = Full_moon
            endif

            do I = 1 , 7
                  if(I .NE. 1 .and. I .NE.3 .and. I.NE. 4) then
                        CF(I) = CF(I)*E
                  end if
            end do

            CF(7) = CF(7)*E

            do I = 1 , 7
                  if(I .NE. 1 .and. I.NE.2 .and. I .NE. 4) then
                        NF(I) = NF(I)*E
                  end if
            enddo

            Correction = CF(1)*dsin(M_p)+ &
            CF(2)*dsin(M)+ &
            CF(3)*dsin(2*M_p)+ &
            CF(4)*dsin(2*F) + &
            CF(5)*dsin(M_p-M) + &
            CF(6)*dsin(M_p+M) + &
            CF(7)*dsin(2*M) + &
            NF(1)*dsin(M_p-2*F) + &
            NF(2)*dsin(M_p+2*F)+ &
            NF(3)*dsin(2*M_p+M)+ &
            NF(4)*dsin(3*M_p)+ &
            NF(5)*dsin(M+2*F)+ &
            NF(6)*dsin(M-2*F)+ &
            NF(7)*dsin(2*M_p-M)+ &
            NF(8)*dsin(Omega)+ &
            NF(9)*dsin(M_p+2*M)+ &
            NF(10)*dsin(2*M_p-2*F)+ &
            NF(11)*dsin(3*M)+ &
            NF(12)*dsin(M_p+M-2*F)+ &
            NF(13)*dsin(2*M_p+2*F)+ &
            NF(14)*dsin(M_p+M+2*F)+ &
            NF(15)*dsin(M_p-M+2*F)+ &
            NF(16)*dsin(M_p-M-2*F)+ &
            NF(17)*dsin(3*M_p+M)+ &
            NF(18)*dsin(4*M_p)

      elseif(New_Full== 1 .or. New_Full == 3 ) then

            do I = 2 , 7
                  if(I .NE. 4 .and. I .NE.5 ) then
                        QM(I) = QM(I)*E

                  end if
            end do

            do I = 11 , 15

                  QM(I) = QM(I)*E

            end do

            QM(7) = QM(7)*E
            QM(14) = QM(14)*E

            Correction = QM(1)*dsin(M_p)+ &
            QM(2)*dsin(M)+ &
            QM(3)*dsin(M_p+M)+ &
            QM(4)*dsin(2*M_p) + &
            QM(5)*dsin(2*F) + &
            QM(6)*dsin(M_p-M) + &
            QM(7)*dsin(2*M) + &
            QM(8)*dsin(M_p-2*F) + &
            QM(9)*dsin(M_p+2*F)+ &
            QM(10)*dsin(3*M_p)+ &
            QM(11)*dsin(2*M_p-M)+ &
            QM(12)*dsin(M+2*F)+ &
            QM(13)*dsin(M-2*F)+ &
            QM(14)*dsin(M_p+2*M)+ &
            QM(15)*dsin(2*M_p+M)+ &
            QM(16)*dsin(Omega)+ &
            QM(17)*dsin(M_p-M-2*F)+ &
            QM(18)*dsin(2*M_p+2*F)+ &
            QM(19)*dsin(M_p+M+2*F)+ &
            QM(20)*dsin(M_p-2*M)+ &
            QM(21)*dsin(M_p+M-2*F)+ &
            QM(22)*dsin(3*M)+ &
            QM(23)*dsin(2*M_p-2*F)+ &
            QM(24)*dsin(M_p-M+2*F)+ &
            QM(25)*dsin(3*M_p+M)

            W= 0.00306D0 - 0.00038D0*E*Dcos(M) + 0.00026D0*dcos(M_p)&
                  -0.00002D0*Dcos(M_p-M) + 0.00002D0*Dcos(M_p+M)&
                  +0.00002D0*Dcos(2*F)

            if(New_Full== 3) W = -W
            Correction = Correction + W

      end if

      JDE = JDE + Correction

end subroutine


!*************************************************************

subroutine Moon_Illum_Fractio(Jdate,Hour,TJD,UT_TT,Ilum_Ratio)
!   From Illuminated fraction of the Moon, Astronomical Algorithms
!, Jean Meeus
!    Jdate, array of three integers corresponding to
!    Jdate(1) = year
!    Jdate(2) = Month number
!    Jdate(3) = Day number in month
!    Hour , required time in Hours, Real number
!    TJD , Julian day number, Real,
!    if TJD /= 0  Jdate and hour will not be effective
!    UT_TT integer
!     UT_TT = 0 for TT time
!     UT_TT = 1 for UTC time
!    Ilum_Ration, output, real number

    implicit none
    integer :: IHR,IMN, ISE
    integer, dimension(3) :: JDate
    real(kind=8) :: Hour, Ilum_Ratio, cos_si, AU,tan_i, Lunar_pi
    real(kind=8) :: TJD , Ap_Landa,Alfa, delta, Dis_delta, TTJD, DJM
    real(kind=8) :: R, RLanda, Alfa_0, Delta_0, cos_i,sin_si, R_si
    real(kind=8) :: R_delta, R_delta_0, Diff_Alfa,A1, A2,T1,T2, R_i

    integer :: J , UT_TT

!    call Pi_DegRad(PI, DegRad)

    if (TJD == 0D0) then
        call Hour2HMS(Hour,IHR, IMN, ISE)
        call iau_DTF2D ( '*', JDate(1),JDate(2),JDate(3), IHR, IMN, real(ISE,kind=8),TJD, DJM, J )
        TJD = TJD + DJM
    !    call JULDAT (JDate(1),JDate(2),JDate(3),Hour,TJD)
    end if
    if (UT_TT == 1) then
          CALL iau_UTCTAI ( TJD,0.D0 , A1, A2, J )
          CALL iau_TAITT ( A1, A2, T1, T2, J )
          TTJD = T1 + T2
    else
        TTJD = TJD
    end if

    call Moon_LonG_Lat_Distance(TTJD,Dis_Delta,Lunar_Pi,Ap_Landa,Alfa,delta)
    call Sun_Apparent_Geo(TTJD,R,RLanda, Alfa_0, Delta_0)

    R_delta = delta*DegRad
    R_delta_0 = delta_0*DegRad
    Diff_Alfa = (alfa_0-alfa)*DegRad

    cos_si = Dsin(R_delta_0)*Dsin(R_delta) + Dcos(R_delta_0)* &
        Dcos(R_delta)*Dcos(Diff_Alfa)

    R_si = Dacos(cos_si)/DegRad
    call AngleChange(180.D0,R_si)

    Sin_si = Dsin(R_si*DegRad)

 !   call ASTCON ("AU",0.001D0,AU)
    AU = 149597870.7D0

    tan_i = R*Sin_si/(Dis_Delta/AU-R*cos_si)
    R_i = Datan(tan_i)/DegRad
    call AngleChange(180.D0,R_i)

    cos_i = Dcos(R_i*DegRad)

    Ilum_Ratio = (1.D0 + cos_i)/2.D0

end subroutine

!************************************************************

subroutine MoonSemiDia(TJD, MoonElevation, GeoDia, TopoDia)
!     from Semidaimeters of sun, moon , planets, Astronomical
!     Algorithms, Jean Meeus
!     TJD, Julian Day number of date, input real number
!     MoonElevation , Altitude of the Moon, Input real number
!     GeoDia, output, Geocentric angular semi diameter of Moon
!     TopoDia output, Topocentric angular semi diameter of moon

      real(kind=8) :: TJD, MoonElevation, GeoDia , TopoDia
      real(kind=8) :: T, Landa, Beta , Delta, Lunar_Pi
      real(kind=8) :: SinPi


      real(kind=8) , parameter :: k_ratio = 0.272481D0

!      Call Pi_DegRad(PI, DegRad)

      T = ((TJD -2451545.0D0))/36525.D0
      call Moon_Mean_LonG_Lat_Dist(T, Landa, Beta , Delta, Lunar_Pi)

      SinPi = 6378.14D0/Delta
!     apparent Geocentric Diameter of Moon
      GeoDia = Dasin(k_ratio*SinPi)/DegRad
!     apparent Topocentric diameter of Moon
      TopoDia = GeoDia*(1.D0 + SinPi*Dsin(MoonElevation*DegRad))


end subroutine


!************************************************************
subroutine Sun_Longitude(Tc,RL,L,Lp)
!     From Solar Coordinates, Astronomical Algorithms, Jean Meeus
!     Higher accuracy, This  code is Fortran version of code from
!     NREL https://midcdmz.nrel.gov/spa/
!     TC, Julian centuries of 36525 ephemeris day from epoch 2000C
!     real Number
!     L, output , real number, Sun mean longitude


    real(kind=8) ::  Tc , RL, L, Lp
    Integer :: I , J
    real(kind=8) , dimension(0:5) :: Lt
    real(kind=8) , dimension(64,3) :: L_Terms0
    real(kind=8) , dimension(34,3) :: L_Terms1
    real(kind=8) , dimension(20,3) :: L_Terms2
    real(kind=8) , dimension(7,3) :: L_Terms3
    real(kind=8) , dimension(3,3) :: L_Terms4
    real(kind=8) , dimension(3) ::   L_Terms5


    DATA ( ( L_Terms0(J,I), I=1,3 ), J=  1, 64 ) /&

    &    175347046.D0,0.D0,0.D0,&
    &    3341656.D0,4.6692568D0,6283.0758500D0,&
    &    34894.0D0, 4.6261D0,12566.1517D0,&
    &    3497.0D0,2.7441D0,5753.3849D0,&
    &    3418.0D0,2.8289D0,3.5231D0,&
    &    3136.0D0,3.6277D0,77713.7715D0,&
    &    2676.0D0,4.4181D0,7860.4194D0,&
    &    2343.0D0,6.1352D0,3930.2097D0,&
    &    1324.0D0,0.7425D0,11506.7698D0,&
    &    1273.0D0,2.0371D0,529.691D0,&
    &    1199.0D0,1.1096D0,1577.3435D0,&
    &    990.0D0,5.233D0,5884.927D0,&
    &    902.0D0,2.045D0,26.298D0,&
    &    857.0D0,3.508D0,398.149D0,&
    &    780.0D0,1.179D0,5223.694D0,&
    &    753.0D0,2.533D0,5507.553D0,&
    &    505.0D0,4.583D0,18849.228D0,&
    &    492.0D0,4.205D0,775.523D0,&
    &    357.0D0,2.92D0,0.067D0,&
    &    317.0D0,5.849D0,11790.629D0,&
    &    284.0D0,1.899D0,796.298D0,&
    &    271.0D0,0.315D0,10977.079D0,&
    &    243.0D0,0.345D0,5486.778D0,&
    &    206.0D0,4.806D0,2544.314D0,&
    &    205.0D0,1.869D0,5573.143D0,&
    &    202.0D0,2.458D0,6069.777D0,&
    &    156.0D0,0.833D0,213.299D0,&
    &    132.0D0,3.411D0,2942.463D0,&
    &    126.0D0,1.083D0,20.775D0,&
    &    115.0D0,0.645D0,0.98D0,&
    &    103.0D0,0.636D0,4694.003D0,&
    &    102.0D0,0.976D0,15720.839D0,&
    &    102.0D0,4.267D0,7.114D0,&
    &    99.0D0,6.21D0,2146.17D0,&
    &    98.0D0,0.68D0,155.42D0,&
    &    86.0D0,5.98D0,161000.69D0,&
    &    85.0D0,1.3D0,6275.96D0,&
    &    85.0D0,3.67D0,71430.7D0,&
    &    80.0D0,1.81D0,17260.15D0,&
    &    79.0D0,3.04D0,12036.46D0,&
    &    75.0D0,1.76D0,5088.63D0,&
    &    74.0D0,3.5D0,3154.69D0,&
    &    74.0D0,4.68D0,801.82D0,&
    &    70.0D0,0.83D0,9437.76D0,&
    &    62.0D0,3.98D0,8827.39D0,&
    &    61.0D0,1.82D0,7084.9D0,&
    &    57.0D0,2.78D0,6286.6D0,&
    &    56.0D0,4.39D0,14143.5D0,&
    &    56.0D0,3.47D0,6279.55D0,&
    &    52.0D0,0.19D0,12139.55D0,&
    &    52.0D0,1.33D0,1748.02D0,&
    &    51.0D0,0.28D0,5856.48D0,&
    &    49.0D0,0.49D0,1194.45D0,&
    &    41.0D0,5.37D0,8429.24D0,&
    &    41.0D0,2.4D0,19651.05D0,&
    &    39.0D0,6.17D0,10447.39D0,&
    &    37.0D0,6.04D0,10213.29D0,&
    &    37.0D0,2.57D0,1059.38D0,&
    &    36.0D0,1.71D0,2352.87D0,&
    &    36.0D0,1.78D0,6812.77D0,&
    &    33.0D0,0.59D0,17789.85D0,&
    &    30.0D0,0.44D0,83996.85D0,&
    &    30.0D0,2.74D0,1349.87D0,&
    &    25.0D0,3.16D0,4690.48D0 /

    DATA ( ( L_Terms1(J,I), I=1,3 ), J=  1, 34) /&
     &   628331966747.0D0,0.D0,0.D0,&
     &   206059.0D0,2.678235D0,6283.07585D0,&
     &   4303.0D0,2.6351D0,12566.1517D0,&
     &   425.0D0,1.59D0,3.523D0,&
     &   119.0D0,5.796D0,26.298D0,&
     &   109.0D0,2.966D0,1577.344D0,&
     &   93.0D0,2.59D0,18849.23D0,&
     &   72.0D0,1.14D0,529.69D0,&
     &   68.0D0,1.87D0,398.15D0,&
     &   67.0D0,4.41D0,5507.55D0,&
     &   59.0D0,2.89D0,5223.69D0,&
     &   56.0D0,2.17D0,155.42D0,&
     &   45.0D0,0.4D0,796.3D0,&
     &   36.0D0,0.47D0,775.52D0,&
     &   29.0D0,2.65D0,7.11D0,&
     &   21.0D0,5.34D0,0.98D0,&
     &   19.0D0,1.85D0,5486.78D0,&
     &   19.0D0,4.97D0,213.3D0,&
     &   17.0D0,2.99D0,6275.96D0,&
     &   16.0D0,0.03D0,2544.31D0,&
     &   16.0D0,1.43D0,2146.17D0,&
     &   15.0D0,1.21D0,10977.08D0,&
     &   12.0D0,2.83D0,1748.02D0,&
     &   12.0D0,3.26D0,5088.63D0,&
     &   12.0D0,5.27D0,1194.45D0,&
     &   12.0D0,2.08D0,4694D0,&
     &   11.0D0,0.77D0,553.57D0,&
     &   10.0D0,1.3D0,6286.6D0,&
     &   10.0D0,4.24D0,1349.87D0,&
     &   9.0D0,2.7D0,242.73D0,&
     &   9.0D0,5.64D0,951.72D0,&
     &   8.0D0,5.3D0,2352.87D0,&
     &   6.0D0,2.65D0,9437.76D0,&
     &   6.0D0,4.67D0,4690.48D0 /

    DATA ( ( L_Terms2(J,I), I=1,3 ), J=  1, 20 ) /&
    &    52919.0D0,0.D0,0.D0,&
    &    8720.0D0,1.0721D0,6283.0758D0,&
    &    309.0D0,0.867D0,12566.152D0,&
    &    27.0D0,0.05D0,3.52D0,&
    &    16.0D0,5.19D0,26.3D0,&
    &    16.0D0,3.68D0,155.42D0,&
    &    10.0D0,0.76D0,18849.23D0,&
    &    9.0D0,2.06D0,77713.77D0,&
    &    7.0D0,0.83D0,775.52D0,&
    &    5.0D0,4.66D0,1577.34D0,&
    &    4.0D0,1.03D0,7.11D0,&
    &    4.0D0,3.44D0,5573.14D0,&
    &    3.0D0,5.14D0,796.3D0,&
    &    3.0D0,6.05D0,5507.55D0,&
    &    3.0D0,1.19D0,242.73D0,&
    &    3.0D0,6.12D0,529.69D0,&
    &    3.0D0,0.31D0,398.15D0,&
    &    3.0D0,2.28D0,553.57D0,&
    &    2.0D0,4.38D0,5223.69D0,&
    &    2.0D0,3.75D0,0.98D0 /

    DATA ( ( L_Terms3(J,I), I=1,3 ), J=  1, 7 ) /&
    &    289.0D0,5.844D0,6283.076D0,&
    &    35.0D0,0.D0,0.D0,&
    &    17.0D0,5.49D0,12566.15D0,&
    &    3.0D0,5.2D0,155.42D0,&
    &    1.0D0,4.72D0,3.52D0,&
    &    1.0D0,5.3D0,18849.23D0,&
    &    1.0D0,5.97D0,242.73D0 /

    DATA ( ( L_Terms4(J,I), I=1,3 ), J=  1, 3 ) /&
    &    114.0D0,3.142D0,0.D0, &
    &    8.D0,4.13D0,6283.08D0, &
    &    1.D0,3.84D0,12566.15D0 /

    DATA (L_Terms5(I), I=1,3 )/1.D0,3.14D0,0.D0 /

     Lt(:) = 0.D0

    do J = 1 , 64
        Lt(0) = Lt(0)+ L_Terms0(J,1)*Dcos(L_Terms0(J,2) + L_Terms0(J,3)*Tc)
    end do

    do J = 1 , 34
        Lt(1) = Lt(1)+ L_Terms1(J,1)*Dcos(L_Terms1(J,2) + L_Terms1(J,3)*Tc)
    end do

    do J = 1 , 20
        Lt(2) = Lt(2)+ L_Terms2(J,1)*Dcos(L_Terms2(J,2) + L_Terms2(J,3)*Tc)
    end do

    do J = 1 , 7
        Lt(3) = Lt(3)+ L_Terms3(J,1)*Dcos(L_Terms3(J,2) + L_Terms3(J,3)*Tc)
    end do

    do J = 1 , 3
        Lt(4) = Lt(4)+ L_Terms4(J,1)*Dcos(L_Terms3(J,2) + L_Terms4(J,3)*Tc)
    end do

    Lt(5) = L_Terms5(1)*Dcos(L_Terms5(2) + L_Terms5(3)*Tc)

!    L = (Lt(0) + Lt(1)*Tc + Lt(2)*Tc*Tc + Lt(3)*Tc**3 + Lt(4)*Tc**4 + Lt(5)*Tc**5)/1.D8
    RL = (Lt(0) +Tc*(Lt(1) + Tc*(Lt(2) + Tc*(Lt(3) + Tc*(Lt(4) + Tc*Lt(5))))))/1.D8

    if (RL > 2.D0*PI ) then
        RL = Dmod(RL,2.D0*PI)
    elseif ( RL < 0.D0) then
        RL = 2.D0*PI + Dmod(RL,2.D0*PI)
    end if

    L = RL/DEGRAD

    Lp = L - 1.397D0*Tc*10.D0 - 0.00031D0*Tc*Tc*100.D0


end subroutine

!***********************************************************************

subroutine Sun_Earth_Vector(Tc, R)
!     From Solar Coordinates, Astronomical Algorithms, Jean Meeus
!     Higher accuracy, This  code is Fortran version of code from
!     NREL https://midcdmz.nrel.gov/spa/
!     TC, Julian centuries of 36525 ephemeris day from epoch 2000C
!     real Number
!     R, output , real number, Sun Earth vector in AU

    real(kind=8) ::  Tc , R
    Integer :: I , J
    real(kind=8) , dimension(0:4) :: Rt
    real(kind=8) , dimension(40,3) :: R_Terms0
    real(kind=8) , dimension(10,3) :: R_Terms1
    real(kind=8) , dimension(6,3) :: R_Terms2
    real(kind=8) , dimension(2,3) :: R_Terms3
    real(kind=8) , dimension(3) ::   R_Terms4


    DATA ( ( R_Terms0(J,I), I=1,3 ), J=  1, 40 ) /&

    &    100013989.D0,0.D0,0.D0,    &
    &    1670700.D0,3.0984635D0,6283.07585D0,&
    &    13956.0D0,3.05525D0,12566.1517D0,&
    &    3084.0D0,5.1985D0,77713.7715D0,&
    &    1628.0D0,1.1739D0,5753.3849D0,&
    &    1576.0D0,2.8469D0,7860.4194D0,&
    &    925.0D0,5.453D0,11506.77D0,&
    &    542.0D0,4.564D0,3930.21D0,&
    &    472.0D0,3.661D0,5884.927D0,&
    &    346.0D0,0.964D0,5507.553D0,&
    &    329.0D0,5.9D0,5223.694D0,&
    &    307.0D0,0.299D0,5573.143D0,&
    &    243.0D0,4.273D0,11790.629D0,&
    &    212.0D0,5.847D0,1577.344D0,&
    &    186.0D0,5.022D0,10977.079D0,&
    &    175.0D0,3.012D0,18849.228D0,&
    &    110.0D0,5.055D0,5486.778D0,&
    &    98.0D0,0.89D0,6069.78D0,&
    &    86.0D0,5.69D0,15720.84D0,&
    &    86.0D0,1.27D0,161000.69D0,&
    &    65.0D0,0.27D0,17260.15D0,&
    &    63.0D0,0.92D0,529.69D0,&
    &    57.0D0,2.01D0,83996.85D0,&
    &    56.0D0,5.24D0,71430.7D0,&
    &    49.0D0,3.25D0,2544.31D0,&
    &    47.0D0,2.58D0,775.52D0,&
    &    45.0D0,5.54D0,9437.76D0,&
    &    43.0D0,6.01D0,6275.96D0,&
    &    39.0D0,5.36D0,4694D0,&
    &    38.0D0,2.39D0,8827.39D0,&
    &    37.0D0,0.83D0,19651.05D0,&
    &    37.0D0,4.9D0,12139.55D0,&
    &    36.0D0,1.67D0,12036.46D0,&
    &    35.0D0,1.84D0,2942.46D0,&
    &    33.0D0,0.24D0,7084.9D0,&
    &    32.0D0,0.18D0,5088.63D0,&
    &    32.0D0,1.78D0,398.15D0,&
    &    28.0D0,1.21D0,6286.6D0,&
    &    28.0D0,1.9D0,6279.55D0,&
    &    26.0D0,4.59D0,10447.39D0 /

    DATA ( ( R_Terms1(J,I), I=1,3 ), J=  1, 10 ) /&
    &    103019.0D0,1.10749D0,6283.07585D0,&
    &    1721.0D0,1.0644D0,12566.1517D0,&
    &    702.0D0,3.142D0,0.D0,&
    &    32.0D0,1.02D0,18849.23D0,&
    &    31.0D0,2.84D0,5507.55D0,&
    &    25.0D0,1.32D0,5223.69D0,&
    &    18.0D0,1.42D0,1577.34D0,&
    &    10.0D0,5.91D0,10977.08D0,&
    &    9.0D0,1.42D0,6275.96D0,&
    &    9.0D0,0.27D0,5486.780D0 /

    DATA ( ( R_Terms2(J,I), I=1,3 ), J=  1, 6 ) /&
    &    4359.0D0,5.7846D0,6283.0758D0,&
    &    124.0D0,5.579D0,12566.152D0,&
    &    12.0D0,3.14D0,0.D0,&
    &    9.0D0,3.63D0,77713.77D0,&
    &    6.0D0,1.87D0,5573.14D0,&
    &    3.0D0,5.47D0,18849.23D0 /

    DATA ( ( R_Terms3(J,I), I=1,3 ), J=  1, 2 ) /&
        145.0D0,4.273D0,6283.076D0, &
        7.D0,3.92D0,12566.15D0 /

    DATA (R_Terms4(I), I=1,3 )/4.D0,2.56D0,6283.08D0 /

     Rt(:) = 0.D0

    do J = 1 , 40
        Rt(0) = Rt(0)+ R_Terms0(J,1)*Dcos(R_Terms0(J,2) + R_Terms0(J,3)*Tc)
    end do

    do J = 1 , 10
        Rt(1) = Rt(1)+ R_Terms1(J,1)*Dcos(R_Terms1(J,2) + R_Terms1(J,3)*Tc)
    end do

    do J = 1 , 6
        Rt(2) = Rt(2)+ R_Terms2(J,1)*Dcos(R_Terms2(J,2) + R_Terms2(J,3)*Tc)
    end do

    do J = 1 , 2
        Rt(3) = Rt(3)+ R_Terms3(J,1)*Dcos(R_Terms3(J,2) + R_Terms3(J,3)*Tc)
    end do

    Rt(4) = R_Terms4(1)*Dcos(R_Terms4(2) + R_Terms4(3)*Tc)

!    R = (Rt(0) + Rt(1)*Tc + Rt(2)*Tc*Tc + Rt(3)*Tc**3 + Rt(4)*Tc**4)/1.D8
     R = (Rt(0) + Tc*(Rt(1) + Tc*(Rt(2) + Tc*(Rt(3) + Tc*Rt(4)))))/1.D8

end subroutine

!****************************************************************

subroutine Sun_Earth_Latitude(Tc, B)
!     From Solar Coordinates, Astronomical Algorithms, Jean Meeus
!     Higher accuracy, This  code is Fortran version of code from
!     NREL https://midcdmz.nrel.gov/spa/
!     TC, Julian centuries of 36525 ephemeris day from epoch 2000C
!     real Number
!     B, output , real number, Sun Earth latitude

    real(kind=8) ::  B0 , B1 , B , Tc
    Integer :: I , J

    real(kind=8) , dimension(5,3) :: B_Terms1
    real(kind=8) , dimension(2,3) :: B_Terms2


    DATA ( ( B_Terms1(J,I), I=1,3 ), J=  1, 5 ) /&
    &    280.0D0, 3.199D0 , 84334.662D0, &
    &    102.0D0 , 5.422D0 , 5507.553D0, &
    &    80.0D0 , 3.88D0 ,5223.69D0 ,     &
    &    44.0D0 ,3.7D0 ,2352.87D0 ,       &
    &    32.0D0 ,4.0D0 ,1577.34D0 /

    DATA ( ( B_Terms2(J,I), I=1,3 ), J=  1, 2 ) /&
    &    9.0D0 ,3.9D0 ,5507.55D0 , &
    &    6.0D0 ,1.73D0 ,5223.69D0 /

    B0 = 0.D0
    B1 = 0.D0
    do J = 1 , 5
        B0 = B0 + B_Terms1(J,1)*Dcos(B_Terms1(J,2) + B_Terms1(J,3)*Tc)
    end do

    do J = 1 , 2
        B1 = B1 + B_Terms2(J,1)*Dcos(B_Terms2(J,2) + B_Terms2(J,3)*Tc)
    end do

    B = (B0 + B1*Tc)/1.D8

end subroutine

!*************************************************************************

subroutine Sun_Apparent_Geo(TJD,R,RLanda, Alfa, Delta)
!     From Solar Coordinates, Astronomical Algorithms, Jean Meeus
!     Sun Apparent Geocentric Higher accuracy,
!     TJD, Julian day of date real Number
!     Land, output , real number, Sun mean longitude
!     Alfa, output , real number, sun right ascension
!     Delta, output, real number, sun obliquity of ecliptic

    real(kind=8) :: T, R, RLanda, Tc, B
    real(kind=8) :: Dtau, Alfa, ROBLT
    real(kind=8) :: RL , L , Lp, RLp
    real(kind=8) :: DPSI, DEPS, Landa
    real(kind=8) :: TJD,OBLM,Delta


    T = ((TJD -2451545.0D0))/36525.D0
    Tc = T/10.D0

    call  Sun_Longitude(Tc, RL, L, Lp)

    call Sun_Earth_Vector(Tc, R)

    call Sun_Earth_Latitude(Tc, B)

    Rlp = Lp*DEGRAD
    B = B + DEGRAD*(0.03916D0*(Dcos(Rlp) - Dsin(Rlp)))/3600.D0

    Call Aberat(R,Dtau)
!     ETILT from NOVAS
!    Call ETILT (TJD,OBLM,OBLT,EQEQ,DPSI,DEPS)

    call iau_NUT00B ( TJD, 0.D0, DPSI, DEPS )

    Landa = L + 180.D0 + Dtau +(DPSI*3600.D0/DEGRAD - 0.09033D0 + 0.03916D0* &
&    (Dcos(Rlp) + Dsin(Rlp))*Dtan(B))/3600.D0

    call ChangeAngle(Landa)

    RLanda = Landa*DegRad

    OBLM = iau_OBL06 ( TJD, 0.D0 )

    ROBLT = OBLM + DEPS

    Alfa = Datan2(Dcos(ROBLT)*Dsin(RLanda)-Dtan(-B)*Dsin(ROBLT)&
        ,Dcos(RLanda))/DegRad

    call ChangeAngle(Alfa)

    Delta = Dasin(Dsin(-B)*Dcos(ROBLT)+Dsin(RLanda)*Dsin(ROBLT)*Dcos(-B))&
        /DegRad

end subroutine

!*************************************************************

subroutine Solar_Position(TJD, Geo,Atmos,UT_TT,SunElev,Zenit,Azim,&
    TopoAlfa,TopoDelta,Iref)
!     From Solar Coordinates, Astronomical Algorithms, Jean Meeus
!     Higher accuracy, This  code is Fortran version of code from
!     NREL https://midcdmz.nrel.gov/spa/
!     TJD, Julian Day number of date, real number
!     Geo, Array of 4 real number, of geographic location
!           Geo(1) = Longitude, of location in degrees
!           Geo(2) = Latitude of location, in degrees
!           Geo(3) = hight of elevation from sea level in meters
!           Geo(4) = Time  zone of location
!     Atoms, array of 2 real number of atmospheric properties
!           Atmos(1) = pressure in milibars
!           Atmos(2) = temperature in Centigrade
!     SunElev, output real number, sun altitude or elevation, Degrees
!     zenit, output, sun zenit distance in degrees
!     Azim, output, sun Azimuth angle in degrees
!     TopoAlfa, output sun topocentric right ascension in degrees
!     TopoDelta, output, sun topocentric obliquity of ecliptic, degrees
!     Iref , input  integer
!            Iref = 0 , Atmospheric refraction is not  considered
!            Iref = 1 , Amospheric refraction considered

    real(kind=8) :: TJD, Zenit , Azim, ksi
    real(kind=8) :: R,RLanda, Alfa, Delta
    real(kind=8) :: AppSidTime,A1,A2,T1,T2
    real(kind=8) :: x , y , Rearth, SunElev
    real(kind=8) :: H, TopoAlfa, TJDT, Phi_p
    real(kind=8) :: Rdelta, TopoDelta, TopoH
    real(kind=8) :: RdelAlfa, RGeo, REFR,RH

    integer :: Iref , J, UT_TT
    real(kind=8), dimension(4) :: Geo
    real(kind=8), dimension(2) :: Atmos

    RGeo = Geo(2)*DegRad

    if(UT_TT == 1) then
            CALL iau_UTCTAI ( TJD,0.D0 , A1, A2, J )
            CALL iau_TAITT ( A1, A2, T1, T2, J )
            TJDT = T1+T2
      else
            TJDT = TJD
    end if

    call Sun_Apparent_Geo(TJDT, R ,RLanda , Alfa, Delta)
    ksi = (8.794*DEGRAD/(3600.D0*R))

    AppSidTime = iau_GST00B ( TJDT, 0.D0 )/DEGRAD

    H = AppSidTime + Geo(1) - Alfa

    Call ChangeAngle(H)

    RH = H*DegRad

    call GeoGlob(Geo , x , y , Phi_p, Rearth)

    RdelAlfa = Datan2(-x*Dsin(ksi)*Dsin(RH),Dcos(Delta*DegRad) &
            - x*Dsin(ksi)*Dcos(RH))

    TopoAlfa = RdelAlfa/DegRad + Alfa

    Rdelta = Delta*DegRad

    TopoDelta = Datan2(Dsin(Rdelta)-y*Dsin(ksi)*Dcos(RdelAlfa),&
    &    Dcos(RDelta) - x*Dsin(ksi)*Dcos(RH))

    TopoH = RH - RdelAlfa

    if(Geo(2)== 0D0)then
      SunElev = (Dasin(Dcos(TopoDelta)*Dcos(TopoH)))/DegRad
    else
      SunElev = (Dasin(Dsin(RGeo)*Dsin(TopoDelta) + &
        Dcos(RGeo)*Dcos(TopoDelta)*Dcos(TopoH)))/DegRad
    end if

    if (Iref == 1  .and. SunElev>=-0.8333D0) then
        call Atmos_Refrac(SunElev,Atmos(1),Atmos(2),REFR)
        SunElev = SunElev + REFR
    end if

    Zenit = 90.D0 - SunElev

    if(Geo(2)== 0D0)then
      Azim = 180.D0 + Datan2(Dsin(TopoH),-Dtan(TopoDelta))/DegRad
    else
      Azim = 180.D0 + Datan2(Dsin(TopoH),Dcos(TopoH)*Dsin(RGeo)&
        - Dtan(TopoDelta)*Dcos(RGeo))/DegRad
    end if

    TopoDelta = TopoDelta/DegRad

end subroutine

!****************************************************************

subroutine SunRise_Set_Noon(TJD,JDate,Geo,Atmos,Altitude,UT_TT,IREF,RTSJD,RTShour)
!     From Solar Coordinates, Astronomical Algorithms, Jean Meeus
!     Higher accuracy, This  code is Fortran version of code from
!     NREL https://midcdmz.nrel.gov/spa/
!     caculation based on interpolation of three consecutive dates
!     TJD, Julian Day number of date, real number
!     Geo, Array of 4 real number, of geographic location
!           Geo(1) = Longitude, of location in degrees
!           Geo(2) = Latitude of location, in degrees
!           Geo(3) = hight of elevation from sea level in meters
!           Geo(4) = Time  zone of location
!     Atoms, array of 2 real number of atmospheric properties
!           Atmos(1) = pressure in milibars
!           Atmos(2) = temperature in Centigrade
!     Altitude, real number, sun required altitude from set and rise
!     UT_TT, integer
!           UT_TT = 1 , TJD is TT
!           UT_TT = 0 , UJD need to add delta T
!     Iref , input  integer
!            Iref = 0 , Atmospheric refraction is not  considered
!            Iref = 1 , Amospheric refraction considered

    implicit none
    integer, dimension(3) :: JDate
    integer :: J, Iref , UT_TT , I
    real(kind=8), dimension(0:2) :: RTSJD, RTShour
    real(kind=8), dimension(4) :: Geo,Geo1
    real(kind=8), dimension(0:2) :: TJDcalc,Alfa,delta,m,Nu,AlfaP,Hp,H,Fr
    real(kind=8), dimension(6) :: Azim1, JD
    real(kind=8), dimension(2) :: Atmos
    real(kind=8) :: Alt1,Zenit1,Alfa1,Delta1
    real(kind=8) :: TJD,AppSidTime,Rlat,RSunalt,R
    real(kind=8) :: A1,A2, T1,T2,RLanda
    real(kind=8) :: TJD0,Arg, SunAlt, dif, Rdif
    real(kind=8) :: a,b,c,ap,bp,cp,dy
    real(kind=8) :: Altitude,Azim, Rdelta


    Geo1 = Geo
    if(Geo(2) >= 0.D0) then
        if(Geo(2)<30.D0 .or. Geo(2)> 60.D0) Geo1(2) = 45.D0
    else
        if(Geo(2)>-30.D0 .or. Geo(2)<-60.D0) Geo1(2) = -45.D0
    end if

    if (TJD == 0.D0) then
        TJD = CAL2JD ( JDate(1),JDate(2),JDate(3))
    endif

    if (UT_TT == 1) then
              CALL iau_UTCTAI ( TJD,0.D0 , A1, A2, J )
              CALL iau_TAITT ( A1, A2, T1, T2, J )
              TJD0 = T1 + T2
    else
        TJD0 = TJD
    end if

    SunAlt = Altitude
    Rsunalt = SunAlt*DEGRAD

    AppSidTime = iau_GST00B ( TJD0, 0.D0 )/DEGRAD

    do I = 0, 2
        TJDcalc(I) = TJD0 + real(I,kind=8) -1.D0
        call Sun_Apparent_Geo(TJDcalc(I),R,RLanda, Alfa(I), Delta(I))
    end do

    m(1) = (Alfa(1) - Geo(1)- AppSidTime)/360.D0
    call change_m(m(1))

    Nu(1) = AppSidTime + 360.985647D0*m(1)
    Hp(1) = Nu(1) + Geo(1) - Alfa(1)
    call ChangeAngle180(Hp(1))

    RTSJD(1) = TJD0 + m(1) - Hp(1)/360.D0
    dif = 0.D0
    call Solar_Position(RTSJD(1), Geo1,Atmos,UT_TT,Alt1,Zenit1,Azim,Alfa(1),Delta(1),IREF)

    if(Azim<= 180.D0 .and. Azim>=90.D0 .or. Azim>= 180.D0 .and. Azim<=270.D0) then
        dif = (180.D0 - Azim)
    elseif (Azim < 90.D0 .and. Azim >= 0.D0) then
        dif = -Azim
    elseif (Azim < 360.D0 .and. Azim > 270.D0) then
        dif =  360.D0 - Azim
    end if

    Rdif = dif*DEGRAD
    Rlat = dabs(Geo1(2))*DegRad
    dif  = datan(dsin(Rdif)/(dcos(Rdif)*dsin(Rlat)+dtan(Alt1*DEGRAD)*dcos(Rlat)))/DEGRAD
    RTSJD(1) = RTSJD(1)+ dif/(15.D0*24.D0)

    do I = 1,6
        JD(I) = RTSJD(1) + (real(I,kind=8) -3.D0)/(60.D0*24.D0)
        call Solar_Position(JD(I), Geo1,Atmos,UT_TT,Alt1,Zenit1,Azim1(I),Alfa1,Delta1,IREF)
        if(I>1)then
            if(Azim1(I-1)> 270.D0 .and. Azim1(I)<90.D0) then
                Azim1(I) = 360.D0 + Azim1(I)
            end if
        end if
    end do

    if(Azim<= 180.D0 .and. Azim>=90.D0 .or. Azim>= 180.D0 .and. Azim<=270.D0) then
        call polint(Azim1,JD,6,180.D0,RTSJD(1),dy)
    else
        call polint(Azim1,JD,6,360.D0,RTSJD(1),dy)
    end if

    RTShour(1) = Dmod(RTSJD(1)-0.5D0,1.D0)*24.D0 + Geo(4)

    call Solar_Position(RTSJD(1), Geo,Atmos,UT_TT,Alt1,Zenit1,Azim,Alfa(1),Delta(1),IREF)
    Rdelta = Delta(1)*DEGRAD
    Arg = (Dsin(Rsunalt)- Dsin(Rlat)*Dsin(Rdelta))/(Dcos(Rlat)*Dcos(Rdelta))

    if (Arg> 1.D0 ) then
        RTShour(0) = 0.D0
        RTShour(2) = 0.D0
        RTSJD(0) = 0.D0
        RTSJD(2) = 0.D0
        return
    elseif(Arg<-1.0D0) then
        RTShour(0) = 99.D0
        RTShour(2) = 99.D0
        RTSJD(0) = 99.D0
        RTSJD(2) = 99.D0
        return
    endif

    H(1) = dacos(Arg)/DegRad
    call AngleChange(180.D0,H(1))

    m(0) = m(1) - H(1)/360.D0
    call change_m(m(0))

    m(2) = m(1) + H(1)/360.D0
    call change_m(m(2))

    a = (Alfa(1) - Alfa(0))
    if (Dabs(a)>2.D0) then
        a=Dmod(a,1.D0)
    end if
    ap = (Delta(1) - Delta(0))
    if (Dabs(ap)>2.D0) then
        ap= Dmod(ap,1.D0)
    end if
    b = (Alfa(2) - Alfa(1))
    if (Dabs(b)>2.D0) then
        b= Dmod(b,1.D0)
    end if
    bp = (Delta(2) - Delta(1))
    if (dabs(bp)>2.D0) then
        bp= Dmod(bp,1.D0)
    end if
    c = b - a
    cp = bp - ap

    do I = 0, 2, 2
        Nu(I) = AppSidTime + 360.985647D0*m(I)
        call ChangeAngle(Nu(I))
        Alfap(I) = Alfa(1) + m(I)*(a + b + c*m(I))/2.D0
        Hp(I) = Nu(I) + Geo(1) - Alfap(I)
        call ChangeAngle180(Hp(I))
        Delta(I) = Delta(1) + m(I)*(ap + bp + cp*m(I))/2.D0
        call horizontal_coords(Geo,Hp(I),Delta(I),Atmos,H(I),Azim,Iref)
        Fr(I) = m(I)+(H(I)-SunAlt)/(360.D0*Dcos(Delta(I)*DegRad)*Dcos(Rlat)*Dsin(Hp(I)*DEGRAD))
        RTSJD(I) = TJD0 + Fr(I)
        RTShour(I) = Dmod(RTSJD(I)-0.5D0,1.D0)*24.D0 +Geo(4)
        If(RTShour(I) > 24.D0)  RTShour(I) = RTShour(I) - 24.D0
    end do

end subroutine

!**************************************************************************************************
 function TrueJDEquiSolitice(Year, k)
!     From Equinoxes and Solstice, Astronomical Algorithms, Jean Meeus
!     High accuracy, up to seconds of time
!     Year, input integer number,
!     K, input integer , Equinox or solstice selector
!           k = 0 spring equinox, new year
!           k = 1 summer solstice
!           k = 2 fall equinox
!           k = 3 winter solstice


      integer :: Year , k , J
      real(kind=8) :: TrueJDEquiSolitice, precis
      real(kind=8) :: JDE, landa, diff
      real(kind=8) :: R,RLanda, Alfa, Delta

      JDE = JDEquiSolitice(Year, k)

      precis = 5.D-7

      do J = 1, 10
            call Sun_Apparent_Geo(JDE,R,RLanda, Alfa, Delta)
            landa = Rlanda/DEGRAD
            diff = k*90.D0 - Landa

            if (Dabs(diff) <= Precis) then
                  exit
            else
                JDE = JDE + 58.D0*Dsin(diff*DEGRAD)
            endif

      end do

      TrueJDEquiSolitice = JDE

end function

!*******************************************************************************
subroutine GeoGlob(Geo , x , y ,Phi_p, Rearth)
!     from The Earth globe, Astronomical Algorithms
!     geographical longitude and latitude correction
!     for earth flattening at poles
!     Geo , array of four real number
!     Geo(1) = longitude, degree
!     Geo(2) = latitude, degree
!     Geo(3) = elevation, meter
!     Geo(4) = Time zone, hours
!     x, output corrected value
!     y, output corrected value used
!     at horizontal coordinations
!     Rearth, output, Radius of earth
!     at latitude, Km

    real(kind=8) :: x, y, Rearth
    real(kind=8) :: u , RGeo, Phi_p
    real(kind=8), dimension(4) :: Geo

!   call Pi_DegRad(Pi, DegRad)

    RGeo = Geo(2)*DegRad
!     Earth radius
    Rearth = 0.9983271D0 + 0.0016764D0*Dcos(2*RGeo) - &
            0.0000035D0*Dcos(4*RGeo)
!     Geocentric latitude
     Phi_p = Geo(2) - 692.73D0*dsin(2*RGeo)/3600.D0 + 1.16D0*dsin(4*RGeo)/3600.D0

    u = Datan(0.99664718D0*Dtan(RGeo))
    x = Dcos(u) + Geo(3)*Dcos(RGeo)/6378137D0
    y = 0.99664718D0*Dsin(u) + Geo(3)*Dsin(RGeo)/6378137D0
end subroutine

!*********************************************************************************
  subroutine Rise_Set(Geo,Atmos,TJD,UT_TT,Dangle,Iref,Dhour,DTJD)
!     finds precise time sunrise or sunset
!     Geo, Array of 4 real number, of geographic location
!           Geo(1) = Longitude, of location in degrees
!           Geo(2) = Latitude of location, in degrees
!           Geo(3) = hight of elevation from sea level in meters
!           Geo(4) = Time  zone of location
!     Atoms, array of 2 real number of atmospheric properties
!           Atmos(1) = pressure in milibars
!           Atmos(2) = temperature in Centigrade
!     TJD, Julian Day number of date, real number
!     Del, time interval to find required event,in hour (fraction)
!     Delta, Precision required in angle( fraction)
!     Dangle, angle of sun altitude at required time
!     Dhour, output required time
!     DTJD, Julian day of corresponding time
!     Iref , input  integer
!            Iref = 0 , Atmospheric refraction is not  considered
!            Iref = 1 , Atmospheric refraction considered

    implicit none

    real(kind=8) ::TJD,Dhour
    real(kind=8) :: Dangle,Azim,Zenit
    real(kind=8) :: Alfa,Delta,Azim1
    real(kind=8) :: DTJD, Alt1, Dy, Rlat
    real(kind=8),dimension(4) :: Geo
    real(kind=8),dimension(2) :: Atmos
    real(kind=8),dimension(8) :: Alt, JD
    integer :: I , UT_TT, Iref

    Rlat = Geo(2)*DEGRAD

    call Solar_Position(TJD, Geo,Atmos,UT_TT,Alt1,Zenit,Azim1,Alfa,Delta,IREF)

    if(Alt1>= Dangle-0.05D0 .and. Alt1<= Dangle+0.05D0) then
        DTJD = TJD
    else
        do I = 1 , 8
            JD(I) = TJD + (I-4.D0)/(60.D0*24.D0)
            call Solar_Position(JD(I), Geo,Atmos,UT_TT,Alt(I),Zenit,Azim,Alfa,Delta,Iref)
        end do
        call polint(Alt,JD,8,Dangle,DTJD,dy)
    end if

    Dhour = Dmod(DTJD-0.5D0,1.D0)*24.D0 + Geo(4)
    If(Dhour > 24.D0)  Dhour = Dhour - 24.D0

    end subroutine

!**********************************************************************************************

      subroutine lunar_position(TJD,UT_TT,TJDT,Geo,Atmos,Elev,Azim,alfa,delta,Iref)
!       Returns topocenteric position of moon for location Geo
!       TJD real number Julian day of date
!       UT_TT integer numer 1 for TJD is TJD is TT and no need to find TT
!       UT_TT = 0 TT will be calculated and added to TJD
!       Geo array of 4 real number
!       Geo(1) longitude of location in degrees
!       Geo(2) latitude of location in degrees
!       Geo(3) Elevation or altitude of location in meters from sea level
!       Geo(4) Time zone in hours
!       Amos array of real numbers of atmospheric properties
!       Atmos(1) pressure in milibar
!       Atmos(2) temeperature in degrees Celisius
!       Elevation out real number Altitude of the Moon from horizon in degrees
!       Azim out real number Azimuth of the Moon from north in degrees
!       delta out real number apparent equitorial longitude of the Moon
!       Iref integer input, 1 for atmospheric effect, 0 for no atmospher

    real(kind=8) :: TJD, Elev, Azim
    real(kind=8) :: Dis_Delta,Lunar_Pi,Ap_Landa,Alfa,delta
    real(kind=8) :: HourAngle,AppSidTime
    real(kind=8) :: A1, A2 , T1 , T2 , TJDT
    real(kind=8), dimension(4) :: Geo
    real(kind=8) , dimension(2) :: Atmos
    integer :: Iref, J, UT_TT


    if (UT_TT == 1) then
        CALL iau_UTCTAI ( TJD,0.D0 , A1, A2, J )
        CALL iau_TAITT ( A1, A2, T1, T2, J )
        TJDT = T1+T2
    else
        TJDT = TJD
    end if

    call Moon_LonG_Lat_Distance(TJDT,Dis_Delta,Lunar_Pi,Ap_Landa,Alfa,delta)

    AppSidTime = iau_GST00B ( TJDT, 0.D0 )/DEGRAD
    HourAngle = (AppSidTime - Alfa + Geo(1))
    Call ChangeAngle(HourAngle)

    call parallax_correction(Dis_Delta,Geo,Hourangle,alfa,delta)

    call horizontal_coords(Geo,HourAngle,delta,Atmos,Elev,Azim,Iref)
    Azim = Azim +  180.D0

    End subroutine

!************************************************************
    subroutine MoonRiseTranSet(TJD0,RTSJD,RTShour,RTSang,TimeZone, &
        moonEvents,moonRTSJD,moonRTShour,moonRTSangles)


      integer ::  J, I, N, K

      Real(kind=8) :: TJD0, TimeZone, Thour
      real(kind=8), dimension(0:2,0:2) :: RTSJD,RTShour,RTSang
      real(kind=8), dimension(4):: moonRTSJD,moonRTShour,moonRTSangles
      character(LEN=8) :: Mtime,Events
      character(LEN=10) ::Mangle
      character(28), dimension(4):: moonEvents
      logical :: isIN

        moonRTSJD = 0.D0
        moonRTShour = 0.D0
        moonRTSangles =  0.D0
        MTime =  '--------'
        Mangle = '----------'
        Events ='--------'
        moonEvents = '--------,--------,----------'

        N = 1
        do I = 0, 2
            do J = 0,2
                if(RTSJD(I,J) /= 0.D0 .and. RTSJD(I,J) /= 99.D0) then
                    if( RTSJD(I,J)+TimeZone/24.D0>= TJD0 .and. RTSJD(I,J)+TimeZone/24.D0 <= TJD0 + 1.D0) then
                        isIN = .TRUE.
                        Thour = dmod(RTSJD(I,J)+TimeZone/24.D0,1.D0)*24.D0
                        if(Thour >= 0.D0 .and. Thour <= 24.D0) then
                            do K = 1,N
                                if(Dabs(moonRTSJD(K)-RTSJD(I,J)) < 1.D-2) then
                                   isIN = .FALSE.
                                end if
                            end do
                            if(isIN ) then
                                moonRTSJD(N) = RTSJD(I,J)
                                moonRTShour(N) = RTShour(I,J)
                                moonRTSangles(N) =  RTSang(I,J)
                                Mtime = Hour2text(moonRTShour(N))
                                Mangle = ','//adjustl(trim(D2str(moonRTSangles(N))))
                                if(J == 0 ) then
                                        Events = 'Rise:'
                                elseif(J == 1) then
                                        Events = 'Transit:'
                                elseif(J == 2) then
                                        Events = 'Set:'
                                end if
                                moonEvents(N) = Events//Mtime//Mangle
                                N = N + 1
                            end if
                        end if
                    end if
                end if
            end do
        enddo

    end subroutine

 !************************************************************

    subroutine Moon_RiseTranSet(Jdate ,TJD, Geo, Atmos ,UT_TT,IREFR,moonEvents,moonRTSJD,&
        moonRTShour,moonRTSangles)

!     Returns Julian date, local hours and azimuth angle of rise and set
!     of moon in one day (24 hours)
!     Input Jdate, is an array of 3 integer represent year, month, and day
!     jDate(1) = year, jDate(2) = month , Jdate(3) = Day
!     input TJD1 real number Julian day of date. in case TJD1 = 0 ,
!     Julian date will e calculated from jDate
!     input Geo, array of 4 real number for geographic location
!     Geo(1) = Longitude, degrees, Geo(2) = Latitude , degrees
!     Geo(3) = altitude = in meters, Geo(4) = time zone in hours
!     input Atmos, array of 2 real number
!     Atmos(1) = pressure in millibars and Atmos(2) = temperature(C)
!     UT_TT integer number 0 for TT will be calculated and 1 for UTC
!     No TT calculation
!     Output RSTJDout, array of two real numbers for
!     RSTJout(1) for Moon rise in a day if exists
!     RSTJout(2) for Moon set in a day if exists
!     output RS_Hours array of two real numbers for corresponding
!     rise and set hours
!     Output RS_Azim array of two real number corresponding rise and set
!     input IREFR, integer number 1 for consider atmospheric effect
!     0 for not considering atmospheric effects

      integer, dimension(3) :: Jdate
      integer :: IREFR , UT_TT , J, I, K, N
      integer, dimension(4):: moonEvents
      real(kind=8), dimension(0:2):: TJDC
      real(kind=8) :: A1, A2, T1, T2, Thour
      Real(kind=8) :: TJD , TJD0
      real(kind=8), dimension(4) :: Geo
      real(kind=8), dimension(2) :: Atmos
      real(kind=8), dimension(4):: moonRTSJD,moonRTShour,moonRTSangles
      real(kind=8), dimension(0:2,0:2):: RTSH3,RTSJD3,RTSA3
      logical :: isIN

        if (TJD == 0.D0) TJD = CAL2JD(JDate(1),JDate(2),JDate(3))

        if (UT_TT == 1) then
                  CALL iau_UTCTAI ( TJD,0.D0 , A1, A2, J )
                  CALL iau_TAITT ( A1, A2, T1, T2, J )
                  TJD0 = T1 + T2 !- Geo(4)/24.D0
        else
            TJD0 = TJD

        endif

        moonRTShour = 0.D0
        moonRTSangles = 0.D0
        moonRTSJD = 0.D0
        RTSH3 = 0.D0
        RTSJD3 = 0.D0
        RTSA3 = 0.D0
        moonEvents = 0

        N = 1
        do I = 0, 2
            TJDC(I) = TJD0 + real(I,kind=8) -1.D0
            call Moon_RTS(Jdate,TJDC(I),Geo,Atmos,UT_TT,IREFR,RTSH3(I,:),RTSJD3(I,:),RTSA3(I,:))
            do J = 0,2
                if(RTSJD3(I,J) /= 0.D0 .and. RTSJD3(I,J) /= 99.D0) then
                    if( RTSJD3(I,J)+Geo(4)/24.D0>= TJD0 .and. RTSJD3(I,J)+Geo(4)/24.D0 <= TJD0 + 1.D0) then
                        isIN = .TRUE.
                        Thour = dmod(RTSJD3(I,J)+Geo(4)/24.D0,1.D0)*24.D0
                        if(Thour >= 0.D0 .and. Thour <= 24.D0) then
                            do K = 1,N
                                if(Dabs(moonRTSJD(K)-RTSJD3(I,J)) < 1.D-2) then
                                   isIN = .FALSE.
                                end if
                            end do
                            if(isIN ) then
                                moonRTSJD(N) = RTSJD3(I,J)
                                moonRTShour(N) = RTSH3(I,J)
                                moonRTSangles(N) =  RTSA3(I,J)
                                moonEvents(N) = J
                                N = N + 1
                            end if
                        end if
                    end if
                end if
            end do
        enddo

    end subroutine

!************************************************************
    subroutine Moon_RTS(Jdate,TJD, Geo, Atmos ,UT_TT,IREFR,RTShour,RTSJD,RTSangles)

!     Returns Julian date, local hours and azimuth angle of rise and set
!     of moon in one day (24 hours)
!     Input Jdate, is an array of 3 integer represent year, month, and day
!     jDate(1) = year, jDate(2) = month , Jdate(3) = Day
!     input TJD real number Julian day of date. in case TJD = 0 ,
!     Julian date will e calculated from jDate
!     input Geo, array of 4 real number for geographic location
!     Geo(1) = Longitude, degrees, Geo(2) = Latitude , degrees
!     Geo(3) = altitude = in meters, Geo(4) = time zone in hours
!     input Atmos, array of 2 real number
!     Atmos(1) = pressure in millibars and Atmos(2) = temperature(C)
!     UT_TT integer number 1 for TT will be calculated and 0 for UTC
!     No TT calculation
!     Output RSTJDout, array of two real numbers for
!     RSTJD(0) for Moon rise in a day if exists
!     RSTJD(1) for Moon transit in a day if exists
!     RSTJD(2) for Moon set in a day if exists
!     output RTSHours array of two real numbers for corresponding
!     rise and set hours
!     Output RTSAngles array of two real number corresponding rise and set
!     input IREFR, integer number 1 for consider atmospheric effect
!     0 for not considering atmospheric effects

      integer, dimension(3) :: Jdate
      integer :: IREFR , UT_TT , J, I
      real(kind=8), dimension(0:2):: RTShour,RTSJD,RTSangles
      real(kind=8), dimension(0:2) :: Nu,Hp,m
      real(kind=8) :: arg, Rlat, RMoonangle, H0
      real(kind=8) :: A1, A2, T1, T2, AppSidTime, Rdelta
      Real(kind=8) :: MoonAngle, TJD , TJD0, TJDT
      real(kind=8) :: alfa1, delta1 , Alt1, dy
      real(kind=8) :: GeoDia, TopoDia, dif, Rdif
      Real(kind=8) :: alfa, delta, Azim, Alt
      Real(kind=8), dimension(6) :: JD, Azim1
      real(kind=8), dimension(4) :: Geo,Geo1
      real(kind=8), dimension(2) :: Atmos


        Geo1 = Geo
        if(Geo(2) >= 0.D0) then
            if(Geo(2)<30.D0 .or. Geo(2)> 60.D0) Geo1(2) = 45.D0
        else
            if(Geo(2)>-30.D0 .or. Geo(2)<-60.D0) Geo1(2) = -45.D0
        end if

        Rlat = Geo(2)*DEGRAD
        if (TJD == 0.D0) TJD = CAL2JD(JDate(1),JDate(2),JDate(3))

        if (UT_TT == 1) then
                  CALL iau_UTCTAI ( TJD,0.D0 , A1, A2, J )
                  CALL iau_TAITT ( A1, A2, T1, T2, J )
                  TJD0 = T1 + T2
        else
            TJD0 = TJD
        end if

        AppSidTime = iau_GST00B ( TJD0, 0.D0 )/DEGRAD

        call lunar_position(TJD0,UT_TT,TJDT,Geo1,Atmos,Alt,Azim,alfa,delta,IREFR)

        m(1) = (Alfa - Geo(1)- AppSidTime)/360.D0
        call change_m(m(1))

        Nu(1) = AppSidTime + 360.985647D0*m(1)
        Hp(1) = Nu(1) + Geo(1) - Alfa
        call ChangeAngle180(Hp(1))

        RTSJD(1) = TJD0 + m(1) - Hp(1)/360.D0
        dif = 0.D0
        call lunar_position(RTSJD(1),UT_TT,TJDT,Geo1,Atmos,Alt,Azim,alfa,delta,IREFR)

        if(Azim > 270.D0 ) then
            dif = (360.D0 - Azim)
        elseif(Azim < 90.D0) then
            dif = -Azim
        else
            dif = (180.D0 - Azim)
        end if

        Rdif = dif*DEGRAD
        dif  = datan(dsin(Rdif)/(dcos(Rdif)*dsin(Rlat)+dtan(Alt*DEGRAD)*dcos(Rlat)))/DEGRAD
        RTSJD(1) = RTSJD(1)+ dif/(15.D0*24.D0)

        do I = 1,6
            JD(I) = RTSJD(1) + (real(I,kind=8) -3.D0)/(10.D0*24.D0)
            call lunar_position(JD(I),UT_TT,TJDT,Geo1,Atmos,Alt1,Azim1(I),Alfa1,Delta1,IREFR)
            if(I>1)then
                if(Azim1(I-1)> 270.D0 .and. Azim1(I)<90.D0) then
                    Azim1(I) = 360.D0 + Azim1(I)
                end if
            end if
        end do

        if(Geo(2) >=0.D0) then
            call polint(Azim1,JD,6,180.D0,RTSJD(1),dy)
        else
            call polint(Azim1,JD,6,360.D0,RTSJD(1),dy)
        end if

        call lunar_position(RTSJD(1),UT_TT,TJDT,Geo,Atmos,RTSangles(1),Azim,alfa,delta,IREFR)
        m(1) = (Alfa - Geo(1)- AppSidTime)/360.D0
        call change_m(m(1))

        RTShour(1) =  Dmod(RTSJD(1)-0.5D0,1.D0)*24.D0 + Geo(4)
        if(RTShour(1) >= 24.D0) RTShour(1) = RTShour(1) - 24.D0

        Rdelta = Delta*DEGRAD

        call MoonSemiDia(RTSJD(1), RTSangles(1), GeoDia, TopoDia)
        MoonAngle =  -TopoDia
        if(IREFR == 0) MoonAngle = MoonAngle - 0.56667D0
        RMoonAngle = MoonAngle*DEGRAD

        Arg = (Dsin(RMoonAngle)- Dsin(Rlat)*Dsin(Rdelta))/(Dcos(Rlat)*Dcos(Rdelta))

        if (Arg> 1.D0 ) then
            RTShour(0) = 0.D0
            RTShour(2) = 0.D0
            RTSJD(0) = 0.D0
            RTSJD(2) = 0.D0
            return
        elseif(Arg<-1.0D0) then
            RTShour(0) = 99.D0
            RTShour(2) = 99.D0
            RTSJD(0) = 99.D0
            RTSJD(2) = 99.D0
            return
        else
            H0 = dacos(Arg)/DegRad
            call AngleChange(180.D0,H0)

            m(0) = m(1) - H0/360.D0
            m(2) = m(1) + H0/360.D0

            do I = 0,2,2

                RTSJD(I) = TJD0 + m(I)
                call moonRS(RTSJD(I),RTSangles(I),UT_TT,Geo,Atmos,IREFR)

                RTShour(I) =  Dmod(RTSJD(I)-0.5D0,1.D0)*24.D0 + Geo(4)
                if(RTShour(I) >= 24.D0) RTShour(I) = RTShour(I) - 24.D0
            end do
        endif

    end subroutine


    !*******************************************************************************

     subroutine moonRS(RSJD,RSAzim,UT_TT,Geo,Atmos,IREF)

    integer :: UT_TT , I, IREF
    real(kind=8) :: RSJD, RSAzim, TJDT
    Real(kind=8) :: MoonAngle,  Alt1
    Real(kind=8) :: Azim , alfa, delta, dy
    real(kind=8) :: GeoDia, TopoDia
    real(kind=8), dimension(4) :: Geo
    real(kind=8), dimension(2) :: Atmos
    Real(kind=8), dimension(8) :: JD, Alt

        call lunar_position(RSJD,UT_TT,TJDT,Geo,Atmos,Alt1,RSAzim,Alfa,Delta,IREF)
        call MoonSemiDia(RSJD, Alt1, GeoDia, TopoDia)
        MoonAngle =  -TopoDia

        if(IREF == 0) MoonAngle = MoonAngle - 0.56667D0

        if(dabs(MoonAngle - Alt1) <= 0.05) return

        do I = 1,8
            JD(I) = RSJD + (real(I,kind=8)-4.D0)/(60.D0*24.D0)
            call lunar_position(JD(I),UT_TT,TJDT,Geo,Atmos,Alt(I),Azim,Alfa,Delta,IREF)
        end do
        call polint(Alt,JD,8,MoonAngle,RSJD,dy)

        call lunar_position(RSJD,UT_TT,TJDT,Geo,Atmos,Alt1,RSazim,Alfa,Delta,IREF)

    end subroutine


  !*********************************************************************************************************************************

    function Lunar_Parallax(TJD)

    real(kind=8) Lunar_Parallax
    real(kind=8) :: TJD , T , Mp , RMp , Lp , RLp

    T = ((TJD -2451545.0D0))/36525.D0

    call Mean_Anomaly_Moon(T, Mp)
    call ChangeAngle(Mp)
    RMp = Mp *  DegRad

    call Moon_Mean_Longitude(T , Lp)
    call ChangeAngle(LP)
    RLp = Lp * DegRad

    Lunar_Parallax = 0.95075D0 + 0.0518055555D0*Dcos(RMp) + 0.0078333333D0*Dcos(RLp) &
     + 0.0095277777D0*Dcos( RMp - RLp ) + 0.0028333333D0*Dcos(2*RMp) &
      + 0.0008611111D0*Dcos ( RMp + RLP )

      end function
!************************************************************************************
      subroutine MoonVisibility(NJDE,TJD,UT_TT,Geo,Atmos,Method,B_Ilum,AidAccept,IREF,&
      LStatus,Visibility)
!
!     Checks visibility of new moon crescent by different criteria
!     NJDE real number new moon Julian day
!     TJD real number , Julian day of date, which we want visibility of moon
!     UT_TT , integer number for UTC time or TT time
!           UT_TT = 0 software calculates TT time
!           UT_TT = 1 calculation based on UTC
!     Geo , array of 4 real numbers of geographic location
!           Geo(1): real number longitude of location
!           Geo(2): real number latitude of location
!           Geo(3): real number elevation above sea level
!           Geo(4): real number time zone of location
!      Method, integer number for method of visibility check
!           Method = 1 for Yallop visibility check
!           "A Method for Predicting the First Sighting of the
!           New Crescent Moon by BD Yallop"
!           Method = 2 for MOHAMMAD SH. ODEH Visibility check
!           "NEW CRITERION FOR LUNAR CRESCENT VISIBILITY"
!     B_Ilum real number minimum Moon percentage illumination
!     (or Moon phase) Young Moon Visibility Criterion Based on
!     "Crescent Illumination and Sky Brightness Contrast Model
!     by Ahmed Kamil 1 Ahmed and 2Abdul Halim Abdul Aziz
!     for Iran 0.39 % ± 0.10 %
!     AidAccept, input integer
!           AidAccept = 0 for Visibility of new Moon with aid is not accepted.
!           AidAccept = 1 for Visibility of new Moon with aid is accepted.
!     LStatus , Logical , output , TRUE if new moon is visible
!                             FALSE if new moon is not visible
!     Visibility , integer, indicating moon visibility
!     based on criteria
!     Criteria , array of 9 real numbers
!           Criteria(1) = age of new moon , hours
!           Criteria(2) = Lag of new moon , hour
!           Criteria(3) = percent of Moon phase
!           Criteria(4) = DAZ = SunAzim - MoonAzim
!           Criteria(5) = ARCV = MoonElev - SunElev
!           Criteria(6) = CosARCL = Dcos(ARCV*DegRad)*Dcos(DAZ*DegRad)
!           Criteria(7)  = TopoDia, Topocenteric semi diameter of moon
!           Criteria(8) = W_p, width of moon crescent in minutes of arc
!           Criteria(9) = q for method 1 or V for method 2
!           Criteria(10) = MoonAzim
!           Criteria(11) = MoonElev
!           Criteria(12) = Sun set time in hours
!

      implicit none
      Real(8) :: NJDE, ARCV , DAZ, V, TJD
      real(8) :: STJD
      real(8) :: GeoDia , GeoAlfa, GeoDelta
      real(8) :: MoonAzim, SunAzim, TopoDia
      real(8) :: q , W_p, Zenit, CosARCL
      real(8) :: MoonElev, SunElev, TJDT
      real(8) :: Altitude,SunSet
      real(8) :: DTJD, alfa, Delta
      real(8) :: B_Ilum, MoonIlum, Age, Lag
      real(8) :: Ho,  AgeLimit , LagLimit
      real(8),dimension(0:2) :: MRTSJD,SRTSJD,RTShour,RTSangles
      integer :: Method , UT_TT, Iref
      integer :: AidAccept
      integer, dimension(3) :: Jdate

      real(kind=8), dimension(4):: Geo
      real(kind=8), dimension(2) :: Atmos
    !  real(kind=8), dimension(12):: Criteria
      logical :: LStatus, LStatus1, LStatus2
      integer :: Visibility

      Altitude = -0.2665694D0

      q = 0.D0
      v = 0.D0
      LStatus1 = .FALSE.
      LStatus2 = .FALSE.
      LStatus  = .FALSE.

      if(AidAccept == 1) then
            LagLimit = 21.D0 !Minutes
            AgeLimit = 13.5D0 ! Hours
      else
            LagLimit = 30.D0  ! Minutes
            AgeLimit = 17.D0  ! Hours
      end if

     call SunRise_Set_Noon(TJD,JDate,Geo,Atmos,Altitude,UT_TT,IREF,SRTSJD,RTShour)
     STJD = SRTSJD(2)
     call Rise_Set(Geo,Atmos,STJD,UT_TT,Altitude,IREF,SunSet,DTJD)

     call Moon_RTS(Jdate,TJD,Geo,Atmos,UT_TT,Iref,RTShour,MRTSJD,RTSangles)

    !  Criteria(12) = SunSet
      age = (MRTSJD(2) - NJDE)*24.D0
    !  Criteria(1) = age
      if(age >= 53.02248) then
        Visibility = 24 !'Age is enough for visibility'
        LStatus = .TRUE.
        return
      end if

      Lag =  (RTShour(2)- SunSet)*60.D0
    !  Criteria(2) = Lag

      if( Lag < LagLimit .or. Age < AgeLimit ) then
            LStatus = .FALSE.
            Visibility = 1 !'Age or Lag, Moon is not Visible!'
            return
       end if

        Ho = SunSet + Lag*4.D0/9.D0
        STJD = TJD + Ho/24.D0
        call Moon_Illum_Fractio(Jdate,Ho,STJD,UT_TT,MoonIlum)
    !    Criteria(3) = MoonIlum

        if(MoonIlum < B_Ilum) then
            LStatus = .FALSE.
            Visibility = 2 !'Moon is not bright enough, Not Visible!'
        else
            call lunar_position(STJD,UT_TT,TJDT,Geo,Atmos,MoonElev,MoonAzim,alfa,delta,Iref)
        !    Criteria(10) = MoonAzim
        !    Criteria(11) = MoonElev
            call Solar_Position(STJD, Geo,Atmos,UT_TT,SunElev,Zenit,SunAzim, &
            GeoAlfa,GeoDelta,Iref)

              DAZ = SunAzim - MoonAzim
        !      Criteria(4) = DAZ
              ARCV = MoonElev - SunElev
        !      Criteria(5) = ARCV
              CosARCL = Dcos(ARCV*DegRad)*Dcos(DAZ*DegRad)
        !      Criteria(6) = CosARCL
              call MoonSemiDia(STJD, MoonElev, GeoDia, TopoDia)
        !      Criteria(7) = TopoDia*60.D0
              W_p = TopoDia*(1.D0 - CosARCL)*60.D0
        !      Criteria(8) = W_p

              if(Method==2) goto 20

              q = (ARCV - third_order_polynomial( - 0.1018D0,0.7319D0, &
              -6.3226D0, 11.8371D0, W_p))/10.D0
        !      Criteria(9) = q

              if(q > 0.216D0) then
                    LStatus1 = .TRUE.
                    Visibility = 10 !'Moon is Visible!'
              elseif(q <= 0.216D0 .and. q > -0.014D0) then
                    LStatus = .TRUE.
                    Visibility = 11 !'Moon is Visible under perfect conditions'
              elseif(q <= -0.014D0 .and. q > -0.16D0) then
                    LStatus1 = .TRUE.
                    Visibility = 12 !'May need optical aid to find crescent'
              elseif( q <= -0.16D0 .and. q > -0.232D0) then
                    if(AidAccept==1) LStatus1 = .True.
                    Visibility = 13 !'Will need Optical aid to find crescent'
              elseif( q <= -0.232D0 .and. q > -0.293D0) then
                    LStatus1 = .False.
                    Visibility = 14 !'Not visible with telescope AECL <=8.5'
              elseif(q <= 0.293) then
                   LStatus1 = .FALSE.
                    Visibility = 15 !'Moon is not Visible!'
              end if

  20          if(Method==1) goto 30

              V = ARCV - third_order_polynomial( -0.1018D0, 0.7319D0, &
                    -6.3226D0, 7.1651D0, W_p)
        !      Criteria(9) = V

              if(V >= 5.65D0) then
                    LStatus2 = .TRUE.
                    Visibility = 20 !'Crescent is visible by naked eye'
              elseif( V < 5.65D0 .and. V >= 2.D0) then
                    LStatus2 = .TRUE.
                    Visibility = 21 ! 'Crescent is visible by optical aid'
              elseif( V < 2.D0 .and. V >= -0.96D0) then
                    if(AidAccept==1) LStatus2 = .TRUE.
                    Visibility = 22 !'Crescent is visible only by optical aid'
              elseif( V < -0.96D0 ) then
                    LStatus2 = .False.
                    Visibility = 23 !'Crescent is not visible'
              end if

  30          if(Method == 1)then
                    LStatus = LStatus1
              elseif(Method == 2) then
                    Lstatus = LStatus2
              elseif(Method == 3) then
                    if(LStatus1 .or. LStatus2) then
                          LStatus = .TRUE.
                    else
                          LStatus = .FALSE.
                    end if
              end if
        endif


      end subroutine


!**********************************************************************************

      subroutine HijrifirstDay(JDE,Geo,Atmos,UT_TT,Method,B_Ilum,AidAccept,IREF,NTJD,HY,HM )
!     JDE is new moon Julian day
!     call MoonVisibility for consecutive days to find if new moon is visible
!     then find new Hijri month day Julian Day NTJD
!     Adjust is difference of first day of NTJD and tabular method of hijri month

      integer :: UT_TT, IREF
      integer :: HY , HM , HD
      integer :: Method, I, AidAccept
      integer :: Visibility
      real(8) :: JDE, B_Ilum, NTJD
      real(8), dimension(4):: Geo
      real(8), dimension(2):: Atmos
    !  real(kind=8), dimension(12) :: Criteria
      logical :: LStatus

      NTJD = dint(JDE)
      NTJD = NTJD - 0.5D0

! finding of Juliann day of first day of lunar month
! from visibility of the Moon
      do I = 1 , 3
           call MoonVisibility(JDE,NTJD,UT_TT,Geo,Atmos,Method,B_Ilum,AidAccept,IREF, &
           LStatus, Visibility)
           NTJD = NTJD + 1.D0
           if(LStatus) exit
      end do
      call JD2Hijri(NTJD , HY , HM , HD)
      if(HD >= 28) HM = HM + 1
      if(HM > 12) then
        HM = 1
        HY = HY + 1
      end if
      end subroutine
!*******************************************************************************

       subroutine YearMoonPhases(IYear,Imonth,Iday,moonPhaseJD)
!     returns array of Julian day  of moon phases for one year

        integer ::  I,k,Month,year,Iyear,Imonth,Iday, Day
        real(kind=8) :: Hour, TJD, JD
        real(kind=8), dimension(14,0:3):: MoonPhaseJD

        Year = Iyear
        Month = Imonth

        Do k  = 0, 3
            call Moon_Phases(Year,IMonth,Iday, k, MoonPhaseJD(1,k))
        End Do

        TJD = MoonPhaseJD(1,0)
        JD = CAL2JD(Year,IMonth,Iday)
        if(TJD > JD+1.D0) then
            month = Imonth - 1
            if(month < 0) then
                month = 12
                year = year - 1
            end if
        end if

        do I = 2 , 14
  10          TJD = TJD + 29.530587D0
              call JD2CAL(TJD,year,Month,day,Hour)
              Do k  = 0, 3
                    call Moon_Phases(Year,Month,day, k, MoonPhaseJD(I,k))
              End Do
              if(Dint(MoonPhaseJD(I,0))<=dint(MoonPhaseJD(I-1,0))) goto 10
        end do
      end subroutine

      subroutine HijriMonths(IYear,Imonth,Iday,Geo,Atmos,UT_TT,Method,B_Ilum,AidAccept,IREF,NewMoonJD,newMoonDate,MoonPhaseJD)
!     returns array of Julian day of first day of Hijri months in one year

            integer :: IYear,Imonth,Iday
            integer :: I,UT_TT, Method, AidAccept
            integer :: IREF
            integer , dimension(14,2):: newMoonDate
            real(kind=8) :: B_Ilum
            real(kind=8), dimension(2) :: Atmos
            real(kind=8) , dimension(4) :: Geo
            real(kind=8) , dimension(14) :: newMoonJD
            real(kind=8), dimension(14,0:3):: MoonPhaseJD


            call YearMoonPhases(IYear,Imonth,Iday,moonPhaseJD)
            newMoonJD = 0.D0
            do I = 1 , 14
                call HijrifirstDay(MoonPhaseJD(I,0),Geo,Atmos,UT_TT,Method,B_Ilum,AidAccept,IREF, &
                    newMoonJD(I),newMoonDate(I,1),newMoonDate(I,2))
            end do
      end subroutine

!*******************************************************************************
      subroutine HijriMonthCS(IYear,Imonth,Iday,Geo,Atmos,UT_TT,Method,B_Ilum,AidAccept,IREF,NewMoonJD,newMoonDate)
!     returns array of Julian day of first day of Hijri months in one year

            integer :: IYear,Imonth,Iday
            integer :: I,UT_TT, Method, AidAccept
            integer :: IREF
            integer , dimension(14,2):: newMoonDate
            real(kind=8) :: B_Ilum
            real(kind=8), dimension(2) :: Atmos
            real(kind=8) , dimension(4) :: Geo
            real(kind=8) , dimension(14) :: newMoonJD
            real(kind=8), dimension(14,0:3):: MoonPhaseJD


            call YearMoonPhases(IYear,Imonth,Iday,moonPhaseJD)
            newMoonJD = 0.D0
            do I = 1 , 14
                call HijrifirstDay(MoonPhaseJD(I,0),Geo,Atmos,UT_TT,Method,B_Ilum,AidAccept,IREF, &
                    newMoonJD(I),newMoonDate(I,1),newMoonDate(I,2))
            end do
      end subroutine

!*******************************************************************************

      FUNCTION AzanAngle(GeoLatitude)
!     returns morning prayer time from interpolation of geographic
!     latitude GeoLatitude, Data of prayer times of different cities from
!     www.azangoo.com

      integer , PARAMETER ::LDA = 10

      REAL(8) :: F(LDA),X(LDA),AzanAngle, GeoLatitude, YANS
!     N IS THE ORDER OF THE INTERPOLATION POLYNOMIAL
      INTEGER :: N, I

      DATA (X(I),I=1,6) / 25.D0,32.6D0,40.7D0,43.6D0,51.5D0,60.5D0/
      DATA (F(I),I=1,6) / -18.D0,-17.7D0,-16.D0,-15.D0,-13.2D0,-12.D0/

      N=6

      if(Dabs(GeoLatitude) < 25.D0 .and. dabs(GeoLatitude)>= 0.D0) then
            AzanAngle = -18.D0
      elseif(dabs(GeoLatitude)> 60.5D0) then
            AzanAngle = -12.D0
      else
            call LSPI(LDA,N,X,F,GeoLatitude,YANS)
            AzanAngle = YANS

      end if

      RETURN
      END function

 !---------------------------------------------------------------------------------

     function Incident(Azim, Zenith, surfAzim, tilt)

     real(kind=8) :: Incident, Azim , Zenith, surfAzim, tilt
     real(kind=8) :: Rincident, RAzim, RZenith, Rtilt, Rdif

     RAzim = DEGRAD*Azim
     RZenith = DEGRAD*Zenith
     Rtilt = DEGRAD*tilt
     Rdif = DEGRAD*(Azim - surfAzim)

     Rincident = DACOS(dcos(RZenith)*dcos(Rtilt) + dsin(RZenith)*dsin(Rtilt)*Dcos(Rdif))
     Incident = Rincident/DEGRAD

  end function Incident
!*******************************************************************************
    subroutine AstroSolarTimes(TJD,Jdate,Geo,Atmos,UT_TT,IREFR,Times,RSTJD,RTS_Angles)
!
!     caculation based on interpolation of three consecutive dates
!     TJD, Julian Day number of date, real number!
!     Geo, Array of 4 real number, of geographic location
!           Geo(1) = Longitude, of location in degrees
!           Geo(2) = Latitude of location, in degrees
!           Geo(3) = hight of elevation from sea level in meters
!           Geo(4) = Time  zone of location
!     Atoms, array of 2 real number of atmospheric properties
!           Atmos(1) = pressure in milibars
!           Atmos(2) = temperature in Centigrade
!     Iref , input  integer
!            Iref = 0 , Atmospheric refraction is not  considered
!            Iref = 1 , Amospheric refraction considered
!     Times, Array of 9 real numbers for following times
!           Times(1) = Astronomical twilight in morning
!           Times(2) = Nautical twilight in morning
!           Times(3) = Civic twilight in morning
!           Times(4) = Sunrise time
!           Times(5) = Noon , or transit time
!           Times(6) = Sunset time
!           Times(7) = Civic twilight in evening
!           Times(8) = Nautical twilight in evening
!           Times(9) = Astronomical twilight in evening

        implicit none

        real(kind=8) ::TJD, TJD0
        real(kind=8) :: Azim,Zenit,Angle
        real(kind=8) :: GeoAlfa,GeoDelta,SunElev
        real(kind=8) :: Horizon
        real(kind=8) :: A1,A2, T1,T2
        real(kind=8),dimension(4) :: Geo
        real(kind=8),dimension(2) :: Atmos
        real(kind=8),dimension(9) :: Times
        real(kind=8),dimension(0:2) :: RTSJD,RTShour
        real(kind=8),dimension(3) :: RSTJD, RTS_Angles
        integer :: I , J, IREFR, UT_TT
        integer, dimension(3) :: Jdate

        Horizon = -0.2665694D0
        RSTJD = 0.D0

        if (TJD == 0.D0) TJD = CAL2JD(JDate(1),JDate(2),JDate(3))

        if (UT_TT == 1) then
                CALL iau_UTCTAI ( TJD,0.D0 , A1, A2, J )
                CALL iau_TAITT ( A1, A2, T1, T2, J )
                TJD0 = T1 + T2
        else
            TJD0 = TJD
        end if

        call SunRise_Set_Noon(TJD0,JDate,Geo,Atmos,Horizon,UT_TT,IREFR,RTSJD,RTShour)

        RSTJD(2) = RTSJD(1)
        Times(5) = RTShour(1)
        call Solar_Position(RTSJD(1),Geo,Atmos,UT_TT,RTS_Angles(2),Zenit,Azim,&
           GeoAlfa,GeoDelta,IREFR)

        if(RTShour(0) ==0.D0 .or.RTShour(2) == 0.D0) then
            Times = 0.D0
            Times(5) = RTShour(1)
            return
        elseif(RTShour(0) == 99.D0 .or. RTShour(2)==99.D0) then
            Times = 99.D0
            Times(5) = RTShour(1)
            return
        end if

        call Rise_Set(Geo,Atmos,RTSJD(0),UT_TT,Horizon,IREFR,Times(4),RSTJD(1))
        call Rise_Set(Geo,Atmos,RTSJD(2),UT_TT,Horizon,IREFR,Times(6),RSTJD(3))

        call Solar_Position(RTSJD(0),Geo,Atmos,UT_TT,SunElev,Zenit,  &
            RTS_Angles(1),GeoAlfa,GeoDelta,IREFR)

        call Solar_Position(RTSJD(2),Geo,Atmos,UT_TT,SunElev,Zenit,  &
            RTS_Angles(3),GeoAlfa,GeoDelta,IREFR)

        call Solar_Position(RSTJD(2)-0.5D0,Geo,Atmos,UT_TT,SunElev,Zenit,Azim,&
           GeoAlfa,GeoDelta,0)

        if(SunElev <=-6.D0) then
            do I = 0, 2
                Angle = -18.D0 + (I)*6.D0

                if (SunElev <= Angle) then
                    call SunRise_Set_Noon(TJD,JDate,Geo,Atmos,Angle,UT_TT,0,RTSJD,RTShour)
                    Times(I+1) = RTShour(0)
                    Times(9-I) = RTShour(2)
                else
                    Times(I+1) = 99
                    Times(9-I) = 99
                end if

            end do
        end if

    end subroutine

!*********************************************************************************************************************

    subroutine SolarTimes(TJD,Jdate,Geo,Atmos,UT_TT,IREFR,Times,RSTJD,RTS_Angles)
!
!     caculation based on interpolation of three consecutive dates
!     TJD, Julian Day number of date, real number!   !
!     Geo, Array of 4 real number, of geographic location
!           Geo(1) = Longitude, of location in degrees
!           Geo(2) = Latitude of location, in degrees
!           Geo(3) = hight of elevation from sea level in meters
!           Geo(4) = Time  zone of location
!     Atoms, array of 2 real number of atmospheric properties
!           Atmos(1) = pressure in milibars
!           Atmos(2) = temperature in Centigrade
!     Iref , input  integer
!            Iref = 0 , Atmospheric refraction is not  considered
!            Iref = 1 , Amospheric refraction considered
!     Times, Array of 3 real numbers for following times!
!           Times(1) = Sunrise time
!           Times(2) = Noon , or transit time
!           Times(3) = Sunset time
!

        implicit none

        real(kind=8) :: TJD , TJD0, Horizon
        real(kind=8) :: Zenit,Azim,Alfa,Delta,Alt
        real(kind=8) :: A1,A2, T1,T2
        real(kind=8),dimension(4) :: Geo
        real(kind=8),dimension(2) :: Atmos
        real(kind=8),dimension(3) :: RSTJD, RTS_Angles, Times
        real(kind=8),dimension(0:2) :: RTSJD,RTShour
        integer :: IREFR, UT_TT, J
        integer, dimension(3) :: Jdate

        Horizon = -0.2665694D0
        RSTJD = 0.D0
        RTS_Angles = 0.D0

        if (TJD == 0.D0) TJD = CAL2JD(JDate(1),JDate(2),JDate(3))

        if (UT_TT == 1) then
                CALL iau_UTCTAI ( TJD,0.D0 , A1, A2, J )
                CALL iau_TAITT ( A1, A2, T1, T2, J )
                TJD0 = T1 + T2
        else
            TJD0 = TJD
        end if

        call SunRise_Set_Noon(TJD0,JDate,Geo,Atmos,Horizon,UT_TT,IREFR,RTSJD,RTShour)

        Times(2) = RTShour(1)
        RSTJD(2) = RTSJD(1)
        call Solar_Position(RTSJD(1),Geo,Atmos,UT_TT,RTS_Angles(2),Zenit,Azim,Alfa,Delta,IREFR)

        if(RTShour(0) == 99.D0 .or.RTShour(2) == 99.D0)then
            Times(1) = 99.D0
            Times(3) = 99.D0
            return
        end if

        if(RTShour(0) == 0.D0 .or.RTShour(2) == 0.D0)then
            Times(1) = 0.D0
            Times(3) = 0.D0
            return
        end if

        call Rise_Set(Geo,Atmos,RTSJD(0),UT_TT,Horizon,IREFR,Times(1),RSTJD(1))
        call Rise_Set(Geo,Atmos,RTSJD(2),UT_TT,Horizon,IREFR,Times(3),RSTJD(3))

        call Solar_Position(RSTJD(1),Geo,Atmos,UT_TT,Alt,Zenit,RTS_Angles(1),Alfa,Delta,IREFR)
        call Solar_Position(RSTJD(3),Geo,Atmos,UT_TT,Alt,Zenit,RTS_Angles(3),Alfa,Delta,IREFR)

    end subroutine
!*********************************************************

function EqIncident(Rlatitude,Rha, Rdelta, RsurfAzim, Rtilt)
    real(kind=8) :: EqIncident, Rlatitude, Cteta1, Cteta2
    real(kind=8) :: Rha, Rdelta, RsurfAzim, Rtilt
    real(kind=8) :: haCos, haSin, sAzimCos, sAzimSin
    real(kind=8) :: tiltSin, tiltCos,deltaSin, deltaCos

    haCos = dcos(Rha)
    haSin = dsin(Rha)
    deltaSin = dsin(Rdelta)
    deltaCos = dcos(Rdelta)
    sAzimCos = dcos(RsurfAzim)
    sAzimSin = dsin(RsurfAzim)
    tiltCos = dcos(Rtilt)
    tiltSin = dsin(Rtilt)

    Cteta1 = dsin(Rlatitude)*(deltaSin*tiltCos+deltaCos*sAzimCos*haCos*tiltSin)
    Cteta2 = dcos(Rlatitude)*(deltaCos*haCos*tiltCos-deltaSin*sAzimCos*tiltSin)
    EqIncident = dacos(Cteta1+Cteta2+deltaCos*sAzimSin*haSin*tiltSin)

  end function

end module

