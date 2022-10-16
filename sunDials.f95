
module SunDials

       USE MoonSun
       implicit none

       contains

       character(LEN=6) function Min2text(Minute)
            real(kind=8) :: Minute
            integer :: H, M , S
            call Hour2HMS(Minute, H , M , S)
            Min2text = adjustl(trim(I2str(M))//':'//trim(I2str(ABS(S))))
        end function

       subroutine RadHourAngle(Hours, RHangle)
            real(kind=8),dimension(13) :: Hours , RHangle
            integer :: i
            real(kind=8), parameter :: Noon = 12.D0

            do i = 1, 13
                RHangle(i) = (Hours(i) - Noon)*15.D0*DEGRAD
            end do
       end subroutine

       subroutine AllYearRdelta(NumDays,jds,Rdelta)
        integer :: NumDays, i
        real(kind=8), dimension(NumDays,3) :: jds
        real(kind=8), dimension(NumDays) :: Rdelta
        real(kind=8) :: R,RLanda, Alfa, Delta

        do i = 1,NumDays
                call Sun_Apparent_Geo(jds(i,2),R,RLanda, Alfa, Delta)
                Rdelta(i) = Delta*DEGRAD
            end do
       end subroutine

       subroutine AllYearDJD(Jyear,numOfDays,DJD)

            integer :: Jyear, numOfDays, i
            real(kind=8) :: TJD, JDSolEq
            real(kind=8), dimension(0:3) :: SolEqJD
            real(kind=8), dimension(numOfDays):: DJD

            do i = 0 , 3
                JDSolEq = JDEquiSolitice(JYear,i)
                TJD =  Dint(JDSolEq)
                if(JDSolEq - TJD >= 0.5D0) then
                     TJD = TJD + 0.5D0
                else
                     TJD = TJD - 0.5D0
                end if
                SolEqJD(i) = TJD
            end do
            if(numOfDays == 10) then
                DJD(1) = SolEqJD(1)
                DJD(2) = SolEqJD(1) + 31.D0
                DJD(3) = SolEqJD(1) + 46.D0
                DJD(4) = SolEqJD(1) + 62.D0
                DJD(5) = SolEqJD(2)
                DJD(6) = SolEqJD(3) - 75.D0
                DJD(7) = SolEqJD(3) - 59.D0
                DJD(8) = SolEqJD(3) - 44.D0
                DJD(9)= SolEqJD(3) - 29.D0
                DJD(10)= SolEqJD(3)
            elseif(numOfDays == 12) then
                DJD(1) = SolEqJD(0)
                DJD(2) = SolEqJD(0) + 15.D0
                DJD(3) = SolEqJD(0) + 31.D0
                DJD(4) = SolEqJD(0) + 46.D0
                DJD(5) = SolEqJD(0) + 62.D0
                DJD(6) = SolEqJD(1)
                DJD(7) = SolEqJD(2)
                DJD(8) = SolEqJD(3) - 75.D0
                DJD(9) = SolEqJD(3) - 59.D0
                DJD(10) = SolEqJD(3) - 44.D0
                DJD(11)= SolEqJD(3) - 29.D0
                DJD(12)= SolEqJD(3)
            elseif(numOfDays == 25) then
                DJD(1)  = SolEqJD(0)
                DJD(2)  = SolEqJD(0) + 15.D0
                DJD(3)  = SolEqJD(0) + 31.D0
                DJD(4)  = SolEqJD(0) + 46.D0
                DJD(5)  = SolEqJD(0) + 62.D0
                DJD(6)  = SolEqJD(0) + 77.D0
                DJD(7)  = SolEqJD(1)
                DJD(8)  = SolEqJD(1) + 15.D0
                DJD(9)  = SolEqJD(1) + 31.D0
                DJD(10) = SolEqJD(1) + 46.D0
                DJD(11) = SolEqJD(1) + 62.D0
                DJD(12) = SolEqJD(1) + 77.D0
                DJD(13) = SolEqJD(2)
                DJD(14) = SolEqJD(2) + 15.D0
                DJD(15) = SolEqJD(2) + 30.D0
                DJD(16) = SolEqJD(2) + 45.D0
                DJD(17) = SolEqJD(2) + 60.D0
                DJD(18) = SolEqJD(2) + 75.D0
                DJD(19) = SolEqJD(3)
                DJD(20) = SolEqJD(3) + 15.D0
                DJD(21) = SolEqJD(3) + 30.D0
                DJD(22) = SolEqJD(3) + 45.D0
                DJD(23) = SolEqJD(3) + 60.D0
                DJD(24) = SolEqJD(3) + 75.D0
                DJD(25) = SolEqJD(3) + 90.D0
            end if
       end subroutine

        subroutine AllYearDaysSunTime(Geo,atmos,UT_TT,IREF,numOfDays,DJD,times,jds)
            integer, dimension(3) :: Jdate
            integer :: UT_TT, IREF, numOfDays
            real(kind=8), dimension(2) :: Atmos
            real(kind=8), dimension(3) :: RTStimes,RTSjds,RTSangles
            real(kind=8), dimension(4) :: Geo
            real(kind=8), dimension(numOfDays):: DJD
            real(kind=8), dimension(numOfDays,3) :: times,jds
            integer :: i

            times = 0.D0
            jds = 0.D0

            do i = 1,numOfDays
                call SolarTimes(DJD(i),Jdate,Geo,atmos,UT_TT,IREF,RTStimes,RTSjds,RTSangles)
                times(i,:) = RTStimes
                jds(i,:) = RTSjds
            end do

       end subroutine

  subroutine SundialxyPoints(Jyear,Geo,atmos,UT_TT,IREF,Hours,NumberofDays,DJD, &
    &        GnomOrtho,GnomIncline,GnomDecline,PlaneDecline,PlaneIncline,dialType,timeKind,Point0,GnomLength,Psi,beta,x,y)

    integer :: UT_TT,IREF,dialType,timeKind
    integer :: NumberofDays, i , j, Jyear
    real(kind=8), dimension(2) :: Atmos, Point, Point0
    real(kind=8), dimension(4) :: Geo, Geo1
    real(kind=8), dimension(13) :: RHangle, Hours
    real(kind=8), dimension(NumberofDays) :: Rdelta, DJD
    real(kind=8), dimension(NumberofDays,3) :: times,jds
    real(kind=8),dimension(NumberofDays,13):: x,y
    real(kind=8), dimension(3) :: Pvec
    real(kind=8), dimension(3) :: Hvec
    real(kind=8), dimension(3) :: Pvec1,Hvec1
    real(kind=8) :: PlaneDecline,PlaneIncline, GnomOrtho
    real(kind=8) :: RHangle1,GnomLength,Psi,Radius
    real(kind=8) :: TJD,SunElev,Zenith,Azim,Alfa,Delta,rd
    real(kind=8) :: GnomIncline,GnomDecline,beta,Rlat, RDec

    Geo1 = Geo

    if(timeKind == 1 .or. timeKind == 2) then
      Geo1(4) = Geo(1)/15.D0
    end if

    selectcase(dialType)
    case(1:2)
      Point0 = 0.D0
      GnomLength = GnomOrtho
      Psi = 90.D0
      beta = 0.D0
   case(3)
        call HorizontalGnom(Geo(2),GnomOrtho,Point0,GnomLength,Psi)
        beta = 90.D0
    case(4)   !
        call verticalGnom(Geo(2),PlaneDecline,GnomOrtho,Point0,GnomLength,Psi,beta)
    case(5)
         call PlanarSundialGnom(Geo(2),PlaneIncline,PlaneDecline,GnomOrtho,Point0,GnomLength,Psi,beta)
    case(11 )
         call  BifilarRods(1,Geo(2) ,PlaneDecline,Rlat,RDec, GnomOrtho , GnomLength , point0 )
         Psi =PlaneDecline
    case(12 )
         call  BifilarRods(2,Geo(2) ,PlaneDecline,Rlat,RDec, GnomOrtho , GnomLength , point0 )
         Psi =PlaneDecline
    case(21) ! Equatorial Plane
        call VectorGnomon(90.D0+Geo(2),0.D0,GnomOrtho,Geo(2),0.D0,Pvec,Hvec,GnomLength,Point0,psi,beta)
        call VectorGnomon(90.D0-Geo(2),180.D0,GnomOrtho,Geo(2),0.D0,Pvec1,Hvec1,GnomLength,Point0,psi,beta)
    case(22:25)
        if(dialType == 22 ) then
            PlaneIncline = Geo(2) ! Polar Plane
            PlaneDecline = 0.D0
            GnomIncline = +90.D0 + Geo(2)
            GnomDecline = 0.D0
        elseif(dialType == 23 ) then
            PlaneIncline = 0.D0 ! Horizontal Plane
            PlaneDecline = 0.D0
            GnomIncline =  Geo(2)
            GnomDecline = 0.D0
        elseif(dialType == 24 ) then
            PlaneIncline = 90.D0 ! Vertical Plane
            GnomIncline =  Geo(2)
            GnomDecline = 0.D0
        end if
        call VectorGnomon(PlaneIncline, PlaneDecline,GnomOrtho,GnomIncline,GnomDecline,&
       &     Pvec,Hvec,GnomLength,Point0,psi,beta)
    end select

     if(timeKind == 1) then
       call RadHourAngle(Hours, RHangle)
    elseif(timeKind == 3) then
       call RadHourAngle(Hours, RHangle)
       RHangle = RHangle + (Geo(1) - Geo(4)*15.D0)*DEGRAD
    end if

    call AllYearDJD(Jyear,NumberofDays,DJD)
    call AllYearDaysSunTime(Geo1,atmos,UT_TT,IREF,NumberofDays,DJD,times,jds)

    if(dialType >= 1 .and. dialType <= 12) then
        call AllYearRdelta(NumberofDays,jds,Rdelta)
    end if

    do i = 1,NumberofDays
        do J = 1 , 13
            Point = 999999.D0
            if(timeKind == 1 .or. timeKind == 3) then
                RHangle1 = RHangle(j)
            else
                RHangle1 = (Hours(j) - times(i,2))*15.D0*DEGRAD
            end if

            if(hours(j) >= times(i,1) .and. hours(j) <= times(i,3)) then
                selectcase(dialType)
                case(1)
                    if(timeKind == 1 .or. timeKind == 3) then
                        if(I<=4) then
                              Call EquatorialPoint(GnomOrtho,RHangle1 ,Rdelta(i), point)
                        elseif( I >=6 ) then
                               Call EquatorialPoint(GnomOrtho,RHangle1 ,Rdelta(i), point)
                               Point(1) = -Point(1)
                        end if
                    elseif(timeKind == 2 .or. timeKind == 4) then
                        if(I >1 .and. I< 13) then
                              Call EquatorialPoint(GnomOrtho,RHangle1 ,Rdelta(i), point)
                        elseif( I > 13 .and. I< 25) then
                               Call EquatorialPoint(GnomOrtho,RHangle1 ,Rdelta(i), point)
                               Point(1) = -Point(1)
                        end if
                    endif
                case(2)
                    call polarPoint(GnomOrtho,RHangle1,RDelta(i),Point)
                case(3)
                    call HorizontalSundial(Geo(2),GnomOrtho,RHangle1,RDelta(i),Point)
                case(4)
                    call verticalSunDial(Geo(2),PlaneDecline,GnomOrtho,RHangle1,RDelta(i),Point)
                case(5)
                    call PlanarSundial(Geo(2),PlaneIncline,PlaneDecline,GnomOrtho,RHangle1,RDelta(i),Point)
                case(11)
                    call BifilarPoint(1,Rlat ,Rdec, GnomOrtho , GnomLength , RDelta(i), RHangle1 ,  point)
                case(12)
                    call BifilarPoint(2,Rlat ,Rdec, GnomOrtho , GnomLength ,RDelta(i), RHangle1 ,  point)
                case(21:25)
                    TJD = jds(I,2) + RHangle1/(24.D0*15.D0*DEGRAD)
                    if(timeKind == 1 .or. timeKind == 3) then
                        if(dialType == 21) then
                            if( I<5 ) then
                              call Solar_Position(TJD,Geo1,Atmos,UT_TT,SunElev,Zenith,Azim,Alfa,Delta,IREF)
                              call VectorGnomPoint(Pvec1,Hvec1,GnomOrtho,SunElev,Azim,Point)
                              Point = -Point
                            elseif( I >5) then
                              call Solar_Position(TJD,Geo1,Atmos,UT_TT,SunElev,Zenith,Azim,Alfa,Delta,IREF)
                              call VectorGnomPoint(Pvec,Hvec,GnomOrtho,SunElev,Azim,Point)
                            end if
                        else
                             call Solar_Position(TJD,Geo,Atmos,UT_TT,SunElev,Zenith,Azim,Alfa,Delta,IREF)
                             call VectorGnomPoint(Pvec,Hvec,GnomOrtho,SunElev,Azim,Point)
                        end if
                    else
                        if(dialType == 21) then
                            if(I > 1 .and. I <13) then
                              call Solar_Position(TJD,Geo,Atmos,UT_TT,SunElev,Zenith,Azim,Alfa,Delta,IREF)
                              call VectorGnomPoint(Pvec1,Hvec1,GnomOrtho,SunElev,Azim,Point)
                              Point = -Point
                            elseif( I >13 .and. I<25) then
                              call Solar_Position(TJD,Geo,Atmos,UT_TT,SunElev,Zenith,Azim,Alfa,Delta,IREF)
                              call VectorGnomPoint(Pvec,Hvec,GnomOrtho,SunElev,Azim,Point)
                            end if
                        else
                             call Solar_Position(TJD,Geo,Atmos,UT_TT,SunElev,Zenith,Azim,Alfa,Delta,IREF)
                             call VectorGnomPoint(Pvec,Hvec,GnomOrtho,SunElev,Azim,Point)
                        end if
                    end if
                 case(0)
                     TJD = jds(I,2) + RHangle1/(24.D0*15.D0*DEGRAD)
                     call Solar_Position(TJD,Geo,Atmos,UT_TT,SunElev,Zenith,Azim,Alfa,Delta,IREF)
                     rd = Azim*DEGRAD
                     point(1) = Zenith*Dsin(rd)
                     point(2) = Zenith*Dcos(rd)
                endselect
                if(dialType /= 0) then
                    Radius = dsqrt(point(1)*point(1)+point(2)*point(2))
                    if(Radius > 20.D0*GnomOrtho .or. Point(1) <-99999.D0) then
                        point = 999999.D0
                    end if
                end if
            end if
            x(i, J) = Point(1)
            Y(i, J) = Point(2)
        enddo
   end do

  end subroutine

    subroutine EquatorialPoint(GnomOrtho,RHangle,RDecline,Point)

        real(kind=8) :: GnomOrtho,RHangle,RDecline
        real(kind=8), dimension(2) :: Point
        real(kind=8) :: Q, term

        Q = dtan(RDecline)
        if(Q == 0.D0) then
                Point(1) = 999999.D0
                Point(2) = 999999.D0
                return
        else
                term = GnomOrtho/Q
                Point(1) = term*dsin(RHangle)
                Point(2) = term*dcos(RHangle)
        end if

    end subroutine

    subroutine polarPoint(GnomOrtho,RHangle,RDelta,Point)
        real(kind=8), dimension(2) :: Point
        real(kind=8) :: GnomOrtho,RHangle,RDelta,Q

        Q = dcos(RHangle)
        if(Q <= 0.D0) then
            Point(1) = 999999.D0
            Point(2) = 999999.D0
            return
        else
            Point(1) = GnomOrtho*dtan(RHangle)
            Point(2) = -GnomOrtho*dtan(RDelta)/Q
        end if

    end subroutine

      subroutine HorizontalGnom(Latitude,GnomOrtho,Point0,styLength,Psi)

        real(kind=8) :: Latitude,GnomOrtho,Stylength,Psi
        real(kind=8), dimension(2) :: Point0
        real(kind=8) :: Rlat, Slat

        Rlat = Latitude*DEGRAD
        Slat = dsin(Rlat)

        Point0(1) = 0.D0
        Point0(2) = -GnomOrtho/dtan(Rlat)

        styLength = GnomOrtho/DABS(Slat)
        Psi = DABS(Latitude)
    end subroutine

    subroutine HorizontalSundial(Latitude,GnomOrtho,RadHourangle,RadSolDecline,Point)

            real(kind=8) :: Latitude,GnomOrtho
            real(kind=8) :: Rlat, RadHourangle,RadSolDecline, TanSolDec, Chang, Clat, Slat
            real(kind=8) :: Q, term
            real(kind=8), dimension(2) :: Point
            Rlat = Latitude*DEGRAD
            Clat = dcos(Rlat)
            Slat = dsin(Rlat)
            Chang = dcos(RadHourangle)
            TanSolDec = dtan(RadSolDecline)

            Q = Clat*Chang + Slat*TanSolDec

            if(Q <= 0.D0) Then
                Point(1) = 999999.D0
                Point(2) = 999999.D0
            else
                term = GnomOrtho/Q
                Point(1) = dsin(RadHourangle)*term
                Point(2) = term*(Slat*Chang-Clat*TanSolDec)
            end if

    end subroutine

    subroutine verticalGnom(Latitude,PlaneDecline,GnomOrtho,Point0,StyLength,Psi,beta)

        real(kind=8), dimension(2) :: Point0
        real(kind=8) :: Latitude,PlaneDecline,GnomOrtho
        real(kind=8) :: stylength, Psi,beta
        real(kind=8) :: Rlat, Rdec, Cdec, Clat

        Rlat = Latitude*DEGRAD
        Rdec = PlaneDecline*DEGRAD

        Cdec = dcos(Rdec)
        Clat = dcos(Rlat)

        Point0(1) = -GnomOrtho*dtan(Rdec)
        Point0(2) = GnomOrtho* dtan(Rlat)/Cdec

        StyLength = GnomOrtho/DABS(Clat*Cdec)
        Psi = Asin(Clat*Cdec)/DEGRAD
        If (Sin(RDec) == 0.D0) Then
            beta = 90.D0
        Else
            beta = -DAtan(Tan(Rlat)/Sin(RDec)) / DEGRAD
        End If

    end subroutine

    subroutine verticalSunDial(Latitude,PlaneDecline,GnomOrtho,RHangle,Rdelta,Point)

        real(kind=8), dimension(2) :: Point
        real(kind=8) :: Latitude,PlaneDecline,GnomOrtho,RHangle,Rdelta
        real(kind=8) :: Rlat, Rdec , Q, Term
        real(kind=8) :: Sdec, Cdec, TanSdec, Slat , Clat, Shang, Chang

        Rlat = Latitude*DEGRAD
        Rdec = PlaneDecline*DEGRAD
        Sdec = dsin(Rdec)
        Cdec = dcos(Rdec)
        TanSdec= dtan(Rdelta)
        Slat = dsin(Rlat)
        Clat = dcos(Rlat)
        Shang = dsin(RHangle)
        Chang = dcos(RHangle)

        Q = Sdec*Shang + Slat*Cdec*Chang - Clat*Cdec*TanSdec

        if(Q <= 0.0D0) then
            Point(1) = 999999.D0
            Point(2) = 999999.D0
        else
            Term = GnomOrtho/Q
            Point(1) = Term*(Cdec*Shang - Slat*Sdec*Chang + Clat*Sdec*TanSdec)
            Point(2) = -Term*(Clat*Chang + Slat*TanSdec)
        end if
    end subroutine

    subroutine PlanarSundialGnom(Latitude,PlaneIncline,PlaneDecline,GnomOrtho,Point0,Stylength,Psi,beta)

        real(kind=8), dimension(2) :: Point0
        real(kind=8) :: Latitude, PlaneIncline, PlaneDecline,GnomOrtho
        real(kind=8) :: P , stylength, Psi, beta
        real(kind=8) :: Rlat, Rinc, Rdec , Gterm, Ap
        real(kind=8) :: Sinc, Cinc, Sdec, Cdec, Slat , Clat

        Rlat = Latitude*DEGRAD
        Rinc = PlaneIncline*DEGRAD
        Rdec = PlaneDecline*DEGRAD

        Sinc = dsin(Rinc)
        Cinc = dcos(Rinc)
        Sdec = dsin(Rdec)
        Cdec = dcos(Rdec)
        Slat = dsin(Rlat)
        Clat = dcos(Rlat)

        P = Slat*Cinc - Clat*Sinc*Cdec
        if(P == 0.D0) then
            Point0(1) = 0.D0
            Point0(2) = 0.D0
            StyLength = GnomOrtho
            Psi =0.D0
            beta = 0.D0
        else
            Gterm = GnomOrtho/P
            Point0(1) = Gterm*Clat*Sdec
            Point0(2) = -Gterm*(Slat*Sinc+Clat*Cinc*Cdec)
            Ap = DABS(P)
            StyLength = GnomOrtho/Ap
            Psi = dasin(Ap)/DEGRAD
            If (RDec == 0.D0) Then
                beta = 90.D0
            Else
                beta = DAtan((Tan(Rlat) * Sinc + Cinc * CDec)/SDec) / DEGRAD
            End If
        end if

    end subroutine

    subroutine PlanarSundial(Latitude,PlaneIncline,PlaneDecline,GnomOrtho,RHangle,Rdelta,Point)

        real(kind=8), dimension(2) :: Point
        real(kind=8) :: Latitude, PlaneIncline, PlaneDecline,GnomOrtho,Rdelta
        real(kind=8) :: Rlat, Rinc, Rdec , RHangle, Gterm
        real(kind=8) :: Sinc, Cinc, Sdec, Cdec, TanSdec, Slat , Clat, Shang, Chang
        real(kind=8) :: P, Q, Nx, Ny

        Rlat = Latitude*DEGRAD
        Rinc = PlaneIncline*DEGRAD
        Rdec = PlaneDecline*DEGRAD

        Sinc = dsin(Rinc)
        Cinc = dcos(Rinc)
        Sdec = dsin(Rdec)
        Cdec = dcos(Rdec)
        TanSdec= dtan(Rdelta)
        Slat = dsin(Rlat)
        Clat = dcos(Rlat)
        Shang = dsin(RHangle)
        Chang = dcos(RHangle)

        P = Slat*Cinc - Clat*Sinc*Cdec
        Q = Sdec*Sinc*Shang + (Clat*Cinc+Slat*Sinc*Cdec)*Chang + P*TanSdec

        if(Q<=0.D0) then
            Point(1) = 999999.D0
            Point(2) = 999999.D0
        else
            Gterm = GnomOrtho/Q
            Nx = Cdec*Shang - Sdec*(Slat*Chang-Clat*TanSdec)
            Ny = Cinc*Sdec*Shang - (Clat*Sinc-Slat*Cinc*Cdec)*Chang  &
            -(Slat*Sinc+Clat*Cinc*Cdec)*TanSdec
            Point(1) = Gterm*Nx
            Point(2) = Gterm*Ny
        end if

    end subroutine


     subroutine VectorGnomon(PlaneIncline, PlaneDecline,GnomonOrtho,GnomonIncline,GnomonDecline,&
   &     Pvec,Hvec,GnomonLength,Point0,psi,beta)

        real(kind=8) :: GnomonLength,PlaneIncline, PlaneDecline
        real(kind=8) :: GnomonIncline, GnomonDecline
        real(kind=8) :: GnomonOrtho,theta, psi, beta
        real(kind=8) :: Rinc, Rdec, sinInc, Arg, Sgp
        real(kind=8) :: RGInc, RGDec, cosGInc,cosTheta
        real(kind=8), dimension(2) :: Point0
        real(kind=8), dimension(3) :: Pvec,Gvec,UGvec
        real(kind=8), dimension(3) :: Kvec,Hvec, SGvec


        Rinc = PlaneIncline*DEGRAD
        Rdec = PlaneDecline*DEGRAD
        sinInc = dsin(Rinc)

        RGInc = GnomonIncline * DEGRAD
        RGDec = GnomonDecline * DEGRAD

        sinInc = Sin(RInc)
        Pvec(1) = -sinInc * Sin(RDec)
        Pvec(2) = -sinInc * Cos(RDec)
        Pvec(3) = Cos(RInc)

        Kvec(1) = 0.D0
        Kvec(2) = 0.D0
        Kvec(3) = 1.D0

        Call VecCrossP(Kvec, Pvec, Hvec, theta)
        Call VectorArg(Hvec, Arg)
        If (Arg == 0.D0) Then
            Hvec(1) = 1.D0
            Hvec(2) = 0.D0
            Hvec(3) = 0.D0
        End If

        cosGInc = DCos(RGInc)
        UGvec(1) = cosGInc * DSin(RGDec)
        UGvec(2) = cosGInc * DCos(RGDec)
        UGvec(3) = DSin(RGInc)

        Call VecDotP(Pvec, UGvec, Arg, theta)
        If (Arg < 0.D0) Then
            UGvec = -UGvec
        End If

        Psi = 90.D0 - theta / DEGRAD
        cosTheta = (DCos(theta))

        GnomonLength = GnomonOrtho / Abs(cosTheta)
        Sgp = GnomonLength*Dsin(theta)
        if(Sgp == 0.D0) then
            Point0 = 0.D0
            beta = 0.D0
        else
            Gvec = UGvec * GnomonLength
            Call VecCrossP(Gvec, Pvec, SGvec, theta)
            Call VecDotP(Hvec, SGvec, Arg, theta)

            Point0(1) = -dsign(Sgp * dSin(theta),SGvec(2))
            Point0(2) = -dsign(Sgp * dCos(theta),SGvec(1))
            beta = -90.D0 + theta / DEGRAD
            if(Hvec(2) <0.D0) beta = -beta
        end if

     end subroutine

     subroutine VectorGnomPoint(Pvec,Hvec,GnomOrtho,SolAlt,SolAz,Point)

        real(kind=8), dimension(2) :: Point
        real(kind=8), dimension(3) :: Pvec, Svec,SGvec, Hvec
        real(kind=8) :: SolAlt, SolAz,GnomOrtho,Arg
        real(kind=8) :: Ralt, Raz, cosAlt,  shadowLength,theta

        Ralt = SolAlt*DEGRAD
        Raz = SolAz*DEGRAD
        cosAlt = dcos(Ralt)

        Svec(1) = cosAlt*dsin(Raz)
        Svec(2) = cosAlt*dcos(Raz)
        Svec(3) = dsin(Ralt)

        call VecDotP(Svec,Pvec,Arg,theta)

        if(Arg < 0.D0 .or.  SolAlt <= 0.D0) then
            Point(1) = 999999.D0
            Point(2) = 999999.D0
            return
        end if

        Call VecCrossP(Pvec, Svec, SGvec, theta)
        shadowLength = GnomOrtho * dTan(theta)

        Call VecDotP(SGvec, Hvec, Arg, theta)
        point(1) = -shadowLength * dSin(theta)
        if(SGvec(3) /= 0.D0) then
            if( SGvec(3) < 0.D0 ) Point(1) = -Point(1)
        else
            if( SGvec(2) < 0.D0 ) Point(1) = -Point(1)
        end if
        point(2) = shadowLength * dCos(theta)

      end subroutine

      subroutine VecCrossP(A,B,C,theta)
      ! Calculates the cross product of vectors A and B, C = A × B
        real(kind=8), dimension(3) :: A,B,C
        real(kind=8) :: theta, ArgA, ArgB, ArgC
      ! Get the product
            C(1) = A(2)*B(3) - A(3)*B(2)
            C(2) = A(3)*B(1) - A(1)*B(3)
            C(3) = A(1)*B(2) - A(2)*B(1)
      ! Get angle between the two vectors
            call VectorArg(A,ArgA)
            call VectorArg(B,ArgB)
            call VectorArg(C,ArgC)
           theta = dasin(ArgC/(ArgA*ArgB))
      end

      subroutine VecDotP(A,B,S,theta)

        real(kind=8), dimension(3) :: A,B
        real(kind=8) :: theta, S, ArgA, ArgB

        S = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)

        call VectorArg(A,ArgA)
        call VectorArg(B,ArgB)
        theta = dacos(S/(ArgA*ArgB))

      end subroutine

      subroutine VectorArg(A,ArgA)

            real(kind=8), dimension(3) :: A
            real(kind=8) :: ArgA
            ArgA = dsqrt(A(1)*A(1)+A(2)*A(2)+A(3)*A(3))
      end

      subroutine VecUnit(A,An)

        real(kind=8), dimension(3) :: A, An
        real(kind=8) :: ArgA

        call  VectorArg(A,ArgA)
        An = A/ArgA

      end subroutine

    subroutine BifilarRods(DialType,Latitude ,PlateDec,Rlat,RDec, Rod1 , Rod2 , point0 )
        integer :: DialType
        real(kind=8) :: Latitude , PlateDec, Rlat, Rod1 , Rod2 ,RDec,TanRlat
        real(kind=8), dimension(2) :: point0

        Rlat = Latitude * DEGRAD
        RDec = PlateDec*DEGRAD
        TanRlat = DTan(RLat)

        If (DialType == 1) Then
              Rod2 = DAbs(Rod1 / DSin(Rlat))
              point0(1) = -Rod2 * DSin(RDec) / TanRlat
              point0(2) = -Rod1 * DCos(RDec) / TanRlat
         ElseIf (DialType == 2) Then
              Rod2 = DAbs(Rod1 / Dcos(Rlat))
              point0(1) = -Rod2 * Tan(RDec)
              point0(2) = Rod1 * TanRlat / Cos(RDec)
         End If

    End subroutine

    subroutine BifilarPoint(DialType,Rlat ,Rdec, Rod1 , Rod2 , Rdelta, RH ,  point)
        integer :: DialType
        real(kind=8) ::Rlat , RDec, Rod1 , Rod2 , Rdelta, RH
        real(kind=8), dimension(2) :: point
        real(kind=8) :: denumx, denumy
        real(kind=8) :: pltSin,pltCos,delTan,haSin,haCos,DecSin, DecCos

        pltSin = DSin(Rlat)
        pltCos = DCos(Rlat)
        delTan = DTan(Rdelta)
        haSin = DSin(RH)
        haCos = DCos(RH)
        DecSin = DSin(RDec)
        DecCos = DCos(RDec)

        denumx = pltSin * delTan + pltCos * haCos

        If (DialType == 1) Then
            point(1) = Rod2 * (haSin * DecCos + haCos * pltSin * DecSin - pltCos * delTan * DecSin) / denumx
            point(2) = Rod1 * (-DecSin * haSin + pltSin * haCos * DecCos - pltCos * delTan * DecCos) / denumx
        ElseIf (DialType == 2) Then
            denumy = DecSin * haSin + pltSin * haCos * DecSin - pltCos * delTan * DecSin
            point(1) = -Rod2 * (DecCos * haSin - pltSin * haSin * DecCos + pltCos * delTan * DecCos) / denumy
            point(2) = -Rod1 * denumx / denumy
        End If

    End subroutine


    subroutine ArmillaryPoint(Radius,Rdelta,point)
        real(kind=8), dimension(2) :: Point
        real(kind=8) :: Radius, Rdelta

        point(1) = 0.D0
        point(2) = -Radius*dtan(Rdelta)

    end subroutine

    subroutine Armillary(JYear,Geo,atmos,UT_TT,IREF,Hours,NumberofDays,DJD,timeKind,Radius,x,y)
        integer :: UT_TT,IREF,timeKind, Jyear
        integer :: NumberofDays, i , j
        real(kind=8), dimension(2) :: Atmos, Point
        real(kind=8), dimension(4) :: Geo
        real(kind=8), dimension(13) :: RHangle, Hours
        real(kind=8), dimension(NumberofDays) :: DJD
        real(kind=8), dimension(NumberofDays) :: Rdelta
        real(kind=8), dimension(NumberofDays,3) :: times,jds
        real(kind=8),dimension(NumberofDays,13):: x,y
        real(kind=8) :: Radius, TimeZoneAngle, AngleDif,time, RHangle1


        TimeZoneAngle = Geo(4)*15.D0

        call AllYearDJD(Jyear,NumberofDays,DJD)
        call AllYearDaysSunTime(Geo,atmos,UT_TT,IREF,NumberofDays,DJD,times,jds)
        call AllYearRdelta(NumberofDays,jds,Rdelta)

        AngleDif = (Geo(1) - TimeZoneAngle)*DEGRAD

        if(timeKind == 1) then
           call RadHourAngle(Hours, RHangle)
        elseif(timeKind == 3) then
           call RadHourAngle(Hours, RHangle)
           RHangle = RHangle + AngleDif
        end if

        do i = 1,NumberofDays
            do J = 1 , 13
                Point = 99999.9D0
                RHangle1 = 0.D0
                if(timeKind == 1 .or. timeKind == 3) then
                    RHangle1 = RHangle(j)
                else
                    RHangle1 = (Hours(j) - times(i,2))*15.D0*DEGRAD
                end if
                time = hours(j)
                if(time > times(i,1) .and. time < times(i,3)) then
                    call ArmillaryPoint(Radius,Rdelta(i),point)
                    Point(1) = Radius*RHangle1
                end if
                x(i, J) = Point(1)
                Y(i, J) = Point(2)
            end do
        end do
    end subroutine

    subroutine EquationOfTimeYear(year,hour,Calendar,eqt,minmaxEQT,minmaxEQTdays)

      integer :: year , month , day, dayInmonth
      integer :: Calendar
      integer, dimension(2,2) :: minmaxEQTdays
      real(kind=8) :: TJD, hour, eqt1
      real(kind=8), dimension(12,31) :: eqt
      real(kind=8), dimension(2) :: minmaxEQT

      eqt = 0.D0
      TJD = 0.D0
      dayInmonth = 0
      minmaxEQT = 0.D0

      if(Calendar == 1 ) then
            TJD = IrCal2JD(year,1,1,hour)
      else
            TJD = CAL2JDH ( Year,1,1, Hour )
      end if

      do month = 1,12
            if(Calendar == 1) then
                  dayInmonth = IDays_in_month(year, month)
            else
                  dayInmonth = GDays_in_month(year, month)
            end if
            do day = 1 , dayInmonth
                  eqt1 = equation_time(TJD)
                  eqt(month,Day) = eqt1
                  if(eqt1 >= minmaxEQT(1)) then
                    minmaxEQT(1) = eqt1
                    minmaxEQTdays(1,1) = month
                    minmaxEQTdays(1,2) = day
                  elseif(eqt1 <= minmaxEQT(2)) then
                    minmaxEQT(2) = eqt1
                    minmaxEQTdays(2,1) = month
                    minmaxEQTdays(2,2) = day
                  end if
                  TJD = TJD + 1.D0
            end do
      end do
      return
    end subroutine

    subroutine zeroEqTime(calendar,year,eqtZeroDays,eqtZeroJD)
        integer :: calendar
        integer :: year, I, J, k ,Nlow, Nup
        integer :: y , mo , day
        real(kind=8) :: TJD0, TJD1 , TJD2
        real(kind=8) :: eqt1, eqt2, R , Hour
        real(kind=8), dimension(4) :: eqtZeroJD
        integer, dimension(4) :: eqtZeroDays
        integer, dimension(4) :: UPL,LPL,UGL,LGL
        data UPL / 30,90,170,285/
        data LPL / 20,80,160,275/
        data UGL / 110,170,250,365/
        data LGL / 100,160,240,355/


        if(calendar == 1 ) then
            TJD0 = IrCal2JD(year,1,1,12.D0)
        else
            TJD0 = CAL2JDH ( Year,1,1, 12.D0 )
        end if

        J = 1
        do K = 1 , 4
                if(calendar == 1) then
                        Nlow = LPL(k)
                        Nup = UPL(k)
                else
                        Nlow = LGL(k)
                        Nup = UGL(k)
                end if
              do I = Nlow, Nup
                      TJD1 = TJD0 + real(I,kind=8)
                      eqt1 = equation_time(TJD1)
                      TJD2 = TJD1 + 1.D0
                      eqt2 =  equation_time(TJD2)
                      IF (DSIGN(1.D0,eqt1) /= DSIGN(1.D0,eqt2)) then

                        R = rtbis(TJD1,TJD2,1.D-4)
                        eqtZeroJD(k) = R
                        if(calendar == 1) then
                                call JD2IrCal(R,y,Mo,day,Hour)
                                eqtZeroDays(J) = dayOfPyear(Mo,day)
                        else
                                call JD2CAL(R,y,Mo,Day,Hour)
                                eqtZeroDays(J) = dayOfGyear(y,Mo,Day)
                        end if

                        J = J + 1
                        exit
                      end if
                     eqt1 = eqt2
                end do
        end do

    end subroutine

    FUNCTION rtbis(x1,x2,xacc)

        REAL(kind=8) :: rtbis,x1,x2,xacc
        integer,PARAMETER ::JMAX=40
        INTEGER j
        REAL(kind=8) :: dx,f,fmid,xmid

        fmid=equation_time(x2)
        f=equation_time(x1)
     !   if(f*fmid >= 0.D0) pause 'root must be bracketed in rtbis'
        if(f < 0.D0)then !Orient the search so that f>0 lies at x+dx.
                rtbis=x1
                dx=x2-x1
        else
                rtbis=x2
                dx=x1-x2
        endif
        do j=1,JMAX !Bisection loop.
                dx=dx*0.5D0
                xmid=rtbis+dx
                fmid=equation_time(xmid)
                if(fmid <= 0.D0)rtbis=xmid
                if(abs(dx) < xacc .or. fmid == 0.D0) return
        enddo
    !    pause 'too many bisections in rtbis'
    END

endmodule
