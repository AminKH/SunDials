
program SunDialsGraph

  USE MoonSun
  USE sunDials

  implicit none

  real(kind=8):: GnomOrtho
  real(kind=8) :: PlaneDecline,PlaneIncline
  real(kind=8), dimension(2) :: Point0, Atmos
  real(kind=8), dimension(25) :: DJD
  real(kind=8), dimension(13) :: OHours, Hours
  real(kind=8),dimension(10,13):: x,y
  real(kind=8),dimension(25,13):: x2,y2
  real(kind=8), dimension(4) :: Geo
  real(kind=8) :: styLength,Psi,Hour,beta, Radius
  real(kind=8) :: mu,Density,a,ts,pp,rm,qm,rr
  real(kind=8) :: GnomonIncline,GnomonDecline

  integer :: kk, i, j ,k
  integer :: dialSelect,timeKind, opSelect
  integer :: UT_TT, IREF, JYear, numberofDays1
  integer :: numberofDays2,Cresp
  integer :: halfHouradded, intError
  integer :: Iyear,Imonth,Iday, Kyear
  integer,dimension(3) :: today

  character(1) :: CalenSelect
  character(4) :: yearText
  character(20) :: DialTypeName
  character(30) :: LocationName
  character(65) :: fileName

  data OHours /6.D0, 7.D0, 8.D0, 9.D0, 10.D0, 11.D0, 12.D0, &
 &            13.D0, 14.D0, 15.D0, 16.D0, 17.D0, 18.D0/


    UT_TT = 0
    IREF = 1

    WRITE(*,5)
5    FORMAT(1X///4X,' UTILITY PROGRAM TO CREATE DATA for SUNDIALS',&
     & /1X,63('-')/&
     & ' THIS PROGRAM ALLOWS YOU TO PRODUCE SUNGRAPH OR SUNDIALS FOR'/&
     & ' GEOGRAPHICAL LOATIONS WITH LONGITUDE, LATITUDE, ALTITUDE AND TIMEZONE'/&
     & , 1X,63('-'))
    WRITE(*,10)
10  FORMAT('  In each of the following steps you will be asked to enter'/&
     & ' one or more values, defining the properties of your location and'/,&
     & ' desired calendar'/   &
     & ' In each step you can enter appropriate value at prompt.'/&
     & ' Location Name: name of any place in characters'/&
     & ' Longitude in decimal degrees, 0.00 to 180.00 for eastern longitudes.'/&
     & ' 0.0 to -180.00 degrees for western longitudes'/ &
     & ' Latitude in decimal degrees 0.00 to 90.00 for northern and 0.00 to -90.00'/&
     & ' southern hemispheres'/&
     & ' Altitude is elevation from sea level in meters from -29 to 8000.'/&
     & ' Time Zone in decimal hours,positive 0 to 12.0 for eastern hemisphere '/&
     & ' negative 0.0 to -12.0 for western hemisphere.'/ &
     & 1X,63('*')/)

    numberofDays1 = 10
    numberofDays2 = 25

    call idate(today)   ! today(1)=day, (2)=month, (3)=year
    Jyear = today(3)

    write(*,*)

 11 call getLocation(LocationName,Geo,intError)
    if(intError == 1) stop

    call stdatm(Geo(3),atmos(2),atmos(1),Density,a,mu,ts,rr,pp,rm,qm,0,kk)
    atmos(1) = atmos(1)/1.D2
    atmos(2) = atmos(2)- 273.15D0

    write(*,*)
    write(*,*) " Please Select Type of Calendar"
    write(*,'(A)') ' Please type I or i for Iranian Calendar: '
    write(*,'(A,$)') ' Please type G or g for IGregorian Calendar: '
    read(*,'(A)') CalenSelect
    write(*,*)

    if(CalenSelect == "I" .or. CalenSelect == "i" ) then
        call GregCal2IrCal(today(3),today(2),today(1),Iyear,Imonth,Iday)
    else
        Iyear = Jyear
    end if

    yearText = trim(I2str(Iyear))

    write(*,*)
    write(*,*) " Please Select Kind of Operation, or Enter for Sundials"
    write(*,102)
102 format(    ' Enter- Sundials or Sungraph '/&
        &      ' 2- Official Time Sun Data' /&
        &      ' 3- Equation of Time' /&
        &      ' 4- Apparent Declination of Sun' /&
        &      ' 5- Local Noon Time' )

    write(*,'(A,$)') ' Please Select: '
    read(*,'(I2)') opSelect
    write(*,*)

    if(opSelect == 3) then
        call EQTDATA(Iyear,CalenSelect)
        goto 34
    elseif( opSelect == 2) then
        write(*,22)adjustl(trim(LocationName)),'Sun Data', Geo
        write(*,*)

        write(*,'(A,A,F8.3,X,A,F7.3)')'Atmospheric properties:','&
      &   Pressure, milibar:', atmos(1),'Temperature C: ' , atmos(2)

        call SunData(Iyear,Jyear,UT_TT,IREF ,CalenSelect,Geo, Atmos)
        goto 34
    elseif(opSelect == 4 .or. opSelect == 5) then
        if(opSelect == 4) then
            write(*,22)adjustl(trim(LocationName)),'Sun Declination', Geo
            write(*,*)
        elseif( opSelect == 5) then
            write(*,22)adjustl(trim(LocationName)),'Noon Times', Geo
            write(*,*)
            write(*,'(A,A,F8.3,X,A,F7.3)')'Atmospheric properties:','&
      &   Pressure, milibar:', atmos(1),'Temperature C: ' , atmos(2)
        end if

        call  NoonDeclinaion(Iyear,Jyear,CalenSelect,opSelect,Atmos,Geo)
        goto 34
    end if

12  Hours = OHours
    write(*,*)
    write(*,*) " Please Select Type of Planar Sundial or Sungraph"
    write(*,15)
15  format(' 0 -For Sun Graph: '/&
    &      ' 1- For Equatorial dial(Polar Gnomon) : '/&
    &      ' 2- For Polar dial (Polar Gnomon) :' /&
    &      ' 3- For Horizontal dial(Polar Gnomon)' /&
    &      ' 4- For Vertical dial (Polar Gnomon)' /&
    &      ' 5- For Plane Dial(Polar Gnomon)' /&
    &      ' 10- For Bilfilar_Inclined Plane' /&
    &      ' 11- For Bifilar Horizontal' /&
    &      ' 12- For Bifilar Vertical' /&
    &      ' 21- For Equatorial dial(Vector)' /&
    &      ' 22- For Polar dial (Vector)' /&
    &      ' 23- For Horizontal dial (Vector)' /&
    &      ' 24- For Vertical dial (Vector)' /&
    &      ' 25- For Plane General Gnomon dial (Vector)' /&
    &      ' 31-For Armillary dial ')


    write(*,'(A,$)') ' Please Select: '
    read(*,'(I2)') dialSelect
    write(*,*)

    selectcase(dialSelect)
    case(1)
        DialTypeName = "_Equatorial"
        PlaneDecline = 0.D0
    case(2)
        DialTypeName = "_Polar"
        PlaneDecline = Geo(2)
        PlaneIncline = 0.D0
    case(3)
        DialTypeName = "_Horizontal"
        PlaneDecline = 0.D0
        PlaneIncline = 0.D0
    case(4)
        DialTypeName = "_Vertical"
        PlaneIncline = 90.D0
    case(5)
        DialTypeName = "_Plane"
    case(10)
        DialTypeName = "_Bifilar_Inclined"
        PlaneDecline = 0.D0
    case(11)
        DialTypeName = "_Bifilar_Horizontal"
        PlaneIncline = 0.D0
    case(12)
        DialTypeName = "_Bifilar_Vertical"
        PlaneIncline = 90.D0
    case(21)
        DialTypeName = "_Equatorial_Vector"
        PlaneDecline = 0.D0
    case(22)
        DialTypeName = "_Polar_Vector"
        PlaneDecline = 0.D0
        PlaneIncline = Geo(2)
    case(23)
        DialTypeName = "_Horizontal_Vector"
        PlaneDecline = 0.D0
        PlaneIncline = 0.D0
    case(24)
        DialTypeName = "_Vertical_Vector"
        PlaneIncline = 90.D0
    case(25)
        DialTypeName = "_Plane_Vector"
    case(31)
        DialTypeName = "_Armillary"
    case(0)
        DialTypeName = "_Sun_Path"
        GnomOrtho = 0.D0
        PlaneDecline = 0.D0
        PlaneIncline = 0.D0
        timeKind = 4
        goto 21
    endselect

    write(*,*) " Please Select Kind of Time "
    write(*,20)
20  format(' 1- For Local Solar Time : '/&
    &      ' 2- For Local Solar Time + Analema :' /&
    &      ' 3- For Solar Time considering Civil Time :' /&
    &      ' 4- For Solar Time considering Civil Time + Analema :' /&
    &      ' 5- For Egyption Solar Times : ' /)

     write(*,'(A,$)') ' Please Select: '
     read(*,'(I1)') timeKind

    GnomonIncline = 0.D0
    GnomonDecline = 0.D0

    if(dialSelect >=1 .and. dialSelect <=25) then
        write(*,*) " Enter Gnomon hight, perpendicular distance of Gnomon tip and dial Plane"
        write(*,'(A,$)') ' Please Enter: '
        read(*,'(F8.4)') GnomOrtho
    elseif(dialSelect ==31) then
        write(*,*) " Enter Radius of, dial cylinder"
        write(*,'(A,$)') ' Please Enter: '
        read(*,'(F8.4)') Radius
    end if

    if(dialSelect >= 4 .and. dialSelect <= 5 .or. dialSelect >= 11 .and. dialSelect <=12 &
 &  .or. dialSelect >= 24 .and. dialSelect <= 25) then
        write(*,*)
        write(*,*) " Enter Plane Declination, That is the angle perpendicular to Plane and"
        write(*,*) " Southern meridian toward west"
        write(*,'(A,$)') ' Please Enter: '
        read(*,'(F8.3)') PlaneDecline
        write(*,*)
    endif

    if (dialSelect == 5 .or. dialSelect == 10 .or. dialSelect == 25 ) then
        write(*,*) " Enter Plane Inclination, That is the angle perpendicular to Plane and"
        write(*,*) " Zenith, for Horizontal Sundial = 0.0 and vertical Sundial = 90.0"
        write(*,'(A,$)') ' Please Enter: '
        read(*,'(F8.3)') PlaneIncline
    end if

    if( dialSelect == 25) then
        write(*,*)
        write(*,*) " Enter Gnomon (Style) Declination, That is the angle perpendicular to Plane and"
        write(*,*) " Southern meridian toward west"
        write(*,'(A,$)') ' Please Enter: '
        read(*,'(F8.3)') GnomonDecline
        write(*,*)
        write(*,*) " Enter Gnomon (Style) Inclination, That is the angle perpendicular to Plane and"
        write(*,*) " Zenith, for Horizontal Sundial = 0.0 and vertical Sundial = 90.0"
        write(*,'(A,$)') ' Please Enter: '
        read(*,'(F8.3)') GnomonIncline
    end if

 21  if (timeKind /= 5) then
         write(*,'(A)') ' Do You want to add Half an hour lines: '
         write(*,*)' 1- Enter 1 To Add Half an Hour'
         read(*,'(I1)') halfHouradded
     end if

     if(dialSelect >= 1 .and. dialSelect <4 .or. dialSelect >= 21 .and. dialSelect <= 23) then
        fileName = trim(adjustl(trim(LocationName))//"_"//adjustl(trim(dialTypeName))&
      &  //"_G"//adjustl(trim(I2str(int(GnomOrtho))))//"_Y"//yearText//"_T"//adjustl(trim(I2str(timeKind)))//".ASC")
     elseif(dialSelect == 10 ) then
        fileName = trim(adjustl(trim(LocationName))//"_"//adjustl(trim(dialTypeName))&
      &  //"_"//adjustl(trim(I2str(int(PlaneIncline))))//"_Rod1"//adjustl(trim(I2str(int(GnomOrtho))))&
      & //"_Y"//yearText//"_T"//adjustl(trim(I2str(timeKind)))//".ASC")
     elseif(dialSelect == 11 .or. dialSelect ==12) then
        fileName = trim(adjustl(trim(LocationName))//"_"//adjustl(trim(dialTypeName))&
      &  //"_Dec"//adjustl(trim(I2str(int(PlaneDecline))))//"_Rod1"//adjustl(trim(I2str(int(GnomOrtho))))&
      & //"_Y"//yearText//"_T"//adjustl(trim(I2str(timeKind)))//".ASC")
    elseif(dialSelect == 4 .or. dialSelect == 5 .or. dialSelect ==24) then
        fileName = trim(adjustl(trim(LocationName))//"_"//adjustl(trim(dialTypeName))&
      &  //"_De"//adjustl(trim(I2str(int(PlaneDecline))))//"_In"//&
      & adjustl(trim(I2str(int(PlaneIncline))))//"_G"//adjustl(trim(I2str(int(GnomOrtho))))&
      & //"_Y"//yearText//"_T"//adjustl(trim(I2str(timeKind)))//".ASC")
    elseif(dialSelect == 25) then
        fileName = trim(adjustl(trim(LocationName))//"_"//adjustl(trim(dialTypeName))&
      &  //"_De"//adjustl(trim(I2str(int(PlaneDecline))))//"_In"//&
      & adjustl(trim(I2str(int(PlaneIncline))))//"_G"//adjustl(trim(I2str(int(GnomOrtho))))&
      &  //"_GDe"//adjustl(trim(I2str(int(GnomonDecline))))//"_GIn"//&
      adjustl(trim(I2str(int(GnomonIncline))))//"_Y"//yearText//"_T"//adjustl(trim(I2str(timeKind)))//".ASC")
    elseif(dialSelect == 0) then
        fileName = trim(adjustl(trim(LocationName))//"_"//adjustl(trim(dialTypeName))&
      &  //"_Y"//yearText//"_T"//adjustl(trim(I2str(timeKind)))//".ASC")
    elseif(dialSelect == 31) then
         fileName = trim(adjustl(trim(LocationName))//"_"//adjustl(trim(dialTypeName))&
      &  //"_R"//adjustl(trim(I2str(int(Radius))))//"_Y"//yearText//"_T"//adjustl(trim(I2str(timeKind)))//".ASC")
    end if

    open(unit = 10 ,file = fileName , form = 'formatted',status = 'unknown')

    write(*,22)adjustl(trim(LocationName)),adjustl(trim(dialTypeName)), Geo
    write(*,*)

    write(10,22)adjustl(trim(LocationName)),adjustl(trim(dialTypeName)), Geo

 22 format('Location: ',A,1X,A,2X,'Longitude',X,F9.4,2X,'Latutude',X,F9.4 &
    &   ,2X,'Elevation'X,F7.2,2X,'TimeZone',X,F5.2)

    write(*,*)
    write(*,'(A,A,F8.3,X,A,F7.3)')'Atmospheric properties:',' Pressure, milibar: ', atmos(1),'Temperature C: ' , atmos(2)

    write(10,'(A,A,F8.3,X,A,F7.3)')'Atmospheric properties:',' Pressure, milibar: ', atmos(1),'Temperature C: ' , atmos(2)
    write(*,*)

    write(*,'(A,I4)')"Year: ",Iyear
    write(10,'(A,I4)')"Year: ",Iyear
    write(*,*)


    if( timeKind <= 2) then
        if (dialSelect == 31) then
            call Armillary(Jyear,Geo,atmos,UT_TT,IREF,Hours,numberofDays1,DJD,1,Radius,x,y)
        else
            call SundialxyPoints(JYear,Geo,atmos,UT_TT,IREF,Hours,numberofDays1,DJD, &
        &        GnomOrtho,GnomonIncline,GnomonDecline,PlaneDecline,PlaneIncline, &
        &        dialSelect,1,Point0,styLength,Psi,beta,x,y)
        end if
    elseif(timeKind >= 3 .and. timeKind < 5) then
      if (dialSelect == 31) then
            call Armillary(Jyear,Geo,atmos,UT_TT,IREF,Hours,numberofDays1,DJD,3,Radius,x,y)
        else
            call SundialxyPoints(JYear,Geo,atmos,UT_TT,IREF,Hours,numberofDays1,DJD, &
        &        GnomOrtho,GnomonIncline,GnomonDecline,PlaneDecline,PlaneIncline, &
        &        dialSelect,3,Point0,styLength,Psi,beta,x,y)
        end if
    elseif( timeKind == 5) then
      if (dialSelect == 31) then
            call Armillary(Jyear,Geo,atmos,UT_TT,IREF,Hours,numberofDays1,DJD,5,Radius,x,y)
        else
            call SundialxyPoints(JYear,Geo,atmos,UT_TT,IREF,Hours,numberofDays1,DJD, &
        &        GnomOrtho,GnomonIncline,GnomonDecline,PlaneDecline,PlaneIncline, &
        &        dialSelect,5,Point0,styLength,Psi,beta,x,y)
        end if
    endif

    if(DialTypeName =="_Bifilar_Inclined" .or. DialTypeName == "_Bifilar_Horizontal" .or. DialTypeName == "_Bifilar_Vertical") then
        write(*,'(5(A,X,F10.4,X))')"Base X0",Point0(1)," Base Y0" ,Point0(2), &
        & " Rod1",GnomOrtho," Rod2", styLength," Angle with East_west",Psi
         write(10,'(5(A,X,F10.4,X))')"Base X0",Point0(1)," Base Y0" ,Point0(2), &
        & " Rod1",GnomOrtho," Rod2", styLength," Angle with East_west",Psi
        write(10,'(2(A,F12.6))')"Plate Declination:",PlaneDecline," Inclination: " ,PlaneIncline
    elseif(DialTypeName == "_Armillary") then
        write(*,'(A,F10.4)')"Radius ",Radius
        write(10,'(A,F10.4)')"Radius ",Radius
        write(10,'(2(A,F12.6))')"Plate Declination:",PlaneDecline," Inclination: " ,PlaneIncline
    else
       write(*,'(4(A,X,F14.6,X),2(A,F14.6))')"Gnom X0",Point0(1)," Y0" ,Point0(2), &
    & " Hight",GnomOrtho," Length", styLength," Angle with dial plate",Psi, &
        & " Angle with plate Vertical: ",beta
      write(*,'(2(A,F12.6))')"Plate Declination:",PlaneDecline," Inclination" ,PlaneIncline

        write(10,'(4(A,X,F14.6,X),2(A,F14.6))')"Gnom X0",Point0(1)," Y0" ,Point0(2), &
        & " Hight",GnomOrtho," Length", styLength," Angle with dial plate: ",Psi, &
        & " Angle with plate Horizon: ",beta
        write(10,'(2(A,F12.6))')"Plate Declination:",PlaneDecline," Inclination: " ,PlaneIncline
    end if

    write(*,*)

    call WriteHourBase(6,13,numberofDays1,Hours,x,y)
    call WriteHourBase(10,13,numberofDays1,Hours,x,y)

    do i = 1,numberofDays1
        if(CalenSelect == "I" .or. CalenSelect == "i") then
            call JD2IrCal(DJD(i),Kyear,Imonth,Iday,Hour)
        else
            call JD2CAL(DJD(i),Kyear,Imonth,Iday,Hour)
        end if
        write(10,'(A4,X,I2,X,I2)') "Date",Imonth , Iday

        do j = 1 , 13
            k = 0
            if(x(i,j) < 99999.0D0 ) then
                write(10,'(2(X,F10.4))') x(i,j),y(i,j)
            else
                k = k + 1
                if(k <=2) write(10,*)
            end if
        enddo
    end do

    write(*,*)

    if(timeKind == 2 .or. timeKind == 4) then
        if (dialSelect == 31) then
            call Armillary(Jyear,Geo,atmos,UT_TT,IREF,Hours,numberofDays2,DJD,timeKind,Radius,x2,y2)
        else
            call SundialxyPoints(Jyear,Geo,atmos,UT_TT,IREF,Hours,numberofDays2,DJD, &
    &        GnomOrtho,GnomonIncline,GnomonDecline,PlaneDecline,PlaneIncline, &
    &        dialSelect,timeKind,Point0,styLength,Psi,beta,x2,y2)
        end if
        call WriteHourBase(6,13,numberofDays2,Hours,x2,y2)
        call WriteHourBase(10,13,numberofDays2,Hours,x2,y2)
        write(*,*)
    end if

    if(halfHouradded == 1 ) then
        if(Geo(4)*15.D0 >= Geo(1)) then
            Hours = Hours + 0.5D0
        else
            Hours = Hours - 0.5D0
        end if
        if( timeKind <= 2) then
            if (dialSelect == 31) then
                call Armillary(Jyear,Geo,atmos,UT_TT,IREF,Hours,numberofDays1,DJD,1,Radius,x,y)
            else
                call SundialxyPoints(JYear,Geo,atmos,UT_TT,IREF,Hours,numberofDays1,DJD, &
            &        GnomOrtho,GnomonIncline,GnomonDecline,PlaneDecline,PlaneIncline, &
            &        dialSelect,1,Point0,styLength,Psi,beta,x,y)
            end if
        elseif(timeKind >= 3) then
          if (dialSelect == 31) then
                call Armillary(Jyear,Geo,atmos,UT_TT,IREF,Hours,numberofDays1,DJD,3,Radius,x,y)
            else
                call SundialxyPoints(JYear,Geo,atmos,UT_TT,IREF,Hours,numberofDays1,DJD, &
            &        GnomOrtho,GnomonIncline,GnomonDecline,PlaneDecline,PlaneIncline, &
            &        dialSelect,3,Point0,styLength,Psi,beta,x,y)
            end if
        endif
        call WriteHourBase(6,13,numberofDays1,Hours,x,y)
        call WriteHourBase(10,13,numberofDays1,Hours,x,y)
    end if

    write(*,*)'File: ', fileName, ' Saved'
    close(10)

34  write(*,35)
35  format(' 1- To start from beginning : '/&
        &  ' 2- To select different dial :' /&
        &  ' 3- To quit' /)
    print*
    write(*,'(A,$)') ' Please select '
    read(*,'(I1)') Cresp
    if(Cresp == 1) then
        goto 11
    elseif(cresp == 2) then
        k = 1
        goto 12
    else
        return
    end if

  end program

  subroutine WriteHourBase(Port,numHours,numDays,Hours,x,y)
      integer :: numHours,numDays, Port,I,J, n
      real(kind=8), dimension(numHours) :: Hours
      real(kind=8), dimension(numDays,numHours) :: x,y

      do j = 1,numHours
        write(Port,'(A4,X,I2,X,F4.1)') "Time", numDays,Hours(j)
        n = 0
        do i = 1 , numDays
            if(x(i,j) < 99999.D0 ) then
                write(Port,'(2(X,F10.4))') x(i,j),y(i,j)
             else
                n = n + 1
                if(n <=2) write(Port,*)
            end if
        enddo
      end do
   end subroutine

    subroutine getRealNum(minLimit,maxLimit,numName,realNum,intError)

        real(kind=8) :: minLimit,maxLimit,realNum
        integer :: intError
        character(*) :: numName
        character(1) :: Responce

        intError = 1

        do while(intError  == 1)
            Write(*,'(A,$)') 'Enter '//numName//' in decimal : '
            read(*,*) realNum
            if(realNum > maxLimit .or. realNum < minLimit) then
                  write(*,*) numName//' is out of range.'
                  WRITE(*,*) ' enter, to continue or, Q, or q to exit'
                  READ(*,'(A1)') Responce
                  if(Responce == 'q' .or. Responce == 'Q') then
                    stop
                  END IF
            else
                  intError  = 0
            end if
        end do
    end subroutine

      subroutine getLocation(LocatName,Geo,IntError)

      real(kind=8) :: realNum
      real(kind=8),dimension(4) :: Geo
      character(Len=*) :: LocatName
      integer :: intError

      Write(*,'(A,$)') 'Enter Name of Location: '
      read(*,'(A)') LocatName

      realNum = 0.D0

      call getRealNum(-180.D0,180.D0,"Longitude",realNum,intError)
      if(intError == 1) then
        return
      else
        Geo(1) = realNum
      end if

     call getRealNum(-90.D0,90.D0,"Latitude",realNum,intError)
      if(intError == 1) then
        return
      else
        Geo(2) = realNum
      end if

    call getRealNum(-29.D0,8000.D0,"Altitude",realNum,intError)
      if(intError == 1) then
        return
      else
        Geo(3) = realNum
      end if

    call getRealNum(-12.D0,12.D0,"Time Zone",realNum,intError)
      if(intError == 1) then
        return
      else
        Geo(4) = realNum
      end if

      end subroutine

    subroutine EQTDATA(Iyear,CalenSelect)

        USE MoonSun
        USE sunDials
        implicit none

        character(1) :: CalenSelect
        integer :: Iyear, I , J ,K
        integer :: Cyear, month, day
        integer :: DayofYear, Iresp
        real(kind=8) :: Hour
        real(kind=8), dimension(2) :: minmaxEQT
        real(kind=8), dimension(12,31) :: eqt
        real(kind=8),dimension(4) :: eqtZeroJD
        integer, dimension(2,2) :: minmaxEQTdays
        integer, dimension(4) :: eqtZeroDays
        character(LEN=8),dimension(12) :: minText

        if(CalenSelect == "I" .or. CalenSelect == "i" ) Iresp = 1
        call EquationOfTimeYear(IYear,12.D0,Iresp,eqt,minmaxEQT,minmaxEQTdays)
        call zeroEqTime(Iresp,Iyear,eqtZeroDays,eqtZeroJD)
        write(*,*)' Days which Equation of Time is maximum and minimum'
        write(*,*) "            Minutes       Year  Month  Day  Day of Year"
        if(CalenSelect == "I" .or. CalenSelect == "i" ) then
            DayofYear = dayOfPyear(minmaxEQTdays(1,1), minmaxEQTdays(1,2))
        else
            DayofYear = dayOfGyear(Iyear,minmaxEQTdays(1,1), minmaxEQTdays(1,2))
        end if
        write(*,'(X,A,2X,A8,6X,I4,2(6X,I2),6X,I3)') " Maximum: ",  &
        Min2Text(minmaxEQT(1)),Iyear, minmaxEQTdays(1,1), minmaxEQTdays(1,2) , DayofYear
        if(Iresp == 1) then
            DayofYear = dayOfPyear(minmaxEQTdays(2,1), minmaxEQTdays(2,2))
        else
            DayofYear = dayOfGyear(Iyear,minmaxEQTdays(2,1), minmaxEQTdays(2,2))
        end if
        write(*,'(X,A,2X,A8,6X,I4,2(6X,I2),6X,I3)') " Minimum: ", &
        Min2Text(minmaxEQT(2)),Iyear, minmaxEQTdays(2,1), minmaxEQTdays(2,2) , DayofYear
        write(*,*)
        write(*,*)' Days which Equation of Time = 0.0'
        write(*,*) " Year  Month  Day  Day of Year"
        do k = 1, 4
           if(CalenSelect == "I" .or. CalenSelect == "i" ) then
                call JD2IrCal(eqtZeroJD(k),Cyear,month,day,Hour)
            else
                call JD2CAL(eqtZeroJD(k),Cyear,month,day,Hour)
            end if
           write(*,'(2X,I4,2(4X,I2),2X,I3)')Cyear,month,day,eqtZeroDays(k)
        end do
        write(*,*)
        do I = 1, 31
          do J = 1, 12
            if(eqt(J,I)==0.D0)then
                minText(J) = "-------"
            else
                minText(J) = Min2Text(eqt(J,I))
            end if
          end do
          write(*,'(X,I2,12(4X,A))')I,minText
        enddo
    end subroutine

    subroutine NoonDeclinaion(Iyear,Jyear,CalenSelect, opSelect,Atmos,Geo)

        USE MoonSun
        USE sunDials
        implicit none

        character(1) :: CalenSelect
        integer :: Iyear ,Jyear ,I , J
        integer :: UT_TT, IREF, DaysinMonth
        integer :: opSelect
        integer, dimension(3) :: Jdate
        real(kind=8) :: Altitude
        real(kind=8) :: TJD,R,RLanda, Alfa
        real(kind=8), dimension(2) :: Atmos
        real(kind=8), dimension(4) :: Geo
        real(kind=8), dimension(3) :: RTSJD,RTShour
        real(kind=8), dimension(12,31) :: Arg
        character(LEN=10),dimension(12) :: minText

        TJD = 0.D0
        Arg = 0.D0

        if(CalenSelect == "I" .or. CalenSelect == "i" ) then
            TJD = IrCal2JD(Iyear,1,1,0.D0)
        else
            TJD = CAL2JD ( JYear,1,1 )
        endif

        write(*,*)
        do I = 1, 12
            if(CalenSelect == "I" .or. CalenSelect == "i" ) then
                DaysinMonth =  IDays_in_month(Iyear, I)
            else
                DaysinMonth = GDays_in_month(Iyear, I)
            endif
            do J = 1, DaysinMonth
                if(opSelect == 4) then
                    call Sun_Apparent_Geo(TJD,R,RLanda, Alfa,Arg(I,J))
                elseif(opSelect == 5) then
                    call SunRise_Set_Noon(TJD,JDate,Geo,Atmos,Altitude,UT_TT,IREF,RTSJD,RTShour)
                    Arg(I,J) = RTShour(2)
                endif
                TJD = TJD + 1
            enddo
        enddo

        do J = 1, 31
            do I = 1, 12
                if(Arg(I,J) ==0.D0)then
                    minText(I) = "-------"
                else
                    if(opSelect == 4) then
                        minText(I) =  Angles2text(Arg(I,J))
                    elseif(opSelect == 5) then
                        minText(I) = Hour2text(Arg(I,J))
                    endif
                end if
            enddo
            write(*,'(X,I2,12(4X,A))')J,minText
        end do

    end subroutine

    subroutine SunData(Iyear,Jyear,UT_TT,IREF ,CalenSelect,Geo, Atmos)

        USE MoonSun
        USE sunDials
        implicit none

        character(1) :: CalenSelect
        integer :: K1, K2, memSize
        integer :: Iyear, Imonth,Iday , I , J
        integer :: UT_TT, IREF, IHour, Jyear
        integer, dimension(3) :: Jdate
        real(kind=8) :: TJD, Hour
        real(kind=8), dimension(2) :: Atmos
        real(kind=8), dimension(4) :: Geo
        real(kind=8), dimension(12) :: DJD
        real(kind=8), dimension(3) :: times,JDS,Angles
        real(kind=8),allocatable, dimension(:) :: Time,SunElev,Zenith,Azim,Alfa,Delta


        call AllYearDJD(Jyear,12,DJD)

        k1 = 0
        k2 = 0

        Do I = 1, 12

            write(*,*)
            IHour = 0
            if(CalenSelect == "I" .or. CalenSelect == "i") then
                call JD2IrCal(DJD(i),Iyear,Imonth,Iday,Hour)
            else
                call JD2CAL(DJD(i),Iyear,Imonth,Iday,Hour)
            end if
            write(*,'(X,A,I4,A,I2,A,I2)') "Date:       ",Iyear,'/',Imonth,'/',Iday
            call SolarTimes(DJD(i),Jdate,Geo,atmos,UT_TT,IREF,times,jds,angles)
            if(times(1)== 0.D0) then
                write(*,'(X,A)') "Time:       "
                write(*,'(X,A)') "Azimuth:    "
                write(*,'(X,A)') "Altitude:   "
                write(*,'(X,A)') "Zenith:     "
                write(*,'(X,A)') "R. Asension:"
                write(*,'(X,A)') "Declination:"
            elseif(times(1) == 99.D0) then
                IHour = 11
            else
                IHour = int(times(2)- times(1)) + 1
            end if

            if(times(1) /= 0.D0) then
                memSize = IHour*2 + 1
                allocate(Time(memSize))
                allocate(SunElev(memSize))
                allocate(Azim(memSize))
                allocate(Zenith(memSize))
                allocate(Alfa(memSize))
                allocate(Delta(memSize))

                Do J = IHour, 1 ,-1
                    k1 = Ihour - J + 1
                    Time(k1) = Times(2) - J
                    TJD = jds(2) - J/24.D0
                    if(TJD < jds(1) .and. times(1) /= 99.D0) then
                        TJD = jds(1)
                        Time(k1) = times(1)
                    endif

                    call Solar_Position(TJD,Geo,Atmos,UT_TT, &
                    &    SunElev(k1),Zenith(k1),Azim(k1),Alfa(k1),Delta(k1),IREF)

                End Do

                Do J = 0 , Ihour
                    k2 = K1 + J + 1
                    Time(k2) = Times(2) + J
                    TJD = jds(2) + J/24.D0
                    if(TJD > jds(3) .and. times(1) /= 99.D0) then
                        TJD = jds(3)
                        Time(k2) = times(3)
                    endif

                    call Solar_Position(TJD,Geo,Atmos,UT_TT, &
                    &    SunElev(K2),Zenith(K2),Azim(k2),Alfa(k2),Delta(k2),IREF)
                End Do
                write(*,'(X,A,24(X,F10.6))') "Time:       ", Time(:)
                write(*,'(X,A,24(X,F10.6))') "Azimuth:    ", Azim(:)
                write(*,'(X,A,24(X,F10.6))') "Altitude:   ", SunElev(:)
                write(*,'(X,A,24(X,F10.6))') "Zenith:     ", Zenith(:)
                write(*,'(X,A,24(X,F10.6))') "R. Asension:", Alfa(:)
                write(*,'(X,A,24(X,F10.6))') "Declination:", Delta(:)
                deallocate(Time)
                deallocate(SunElev)
                deallocate(Azim)
                deallocate(Zenith)
                deallocate(Alfa)
                deallocate(Delta)
            end if

        End Do

        write(*,*)
    end subroutine


