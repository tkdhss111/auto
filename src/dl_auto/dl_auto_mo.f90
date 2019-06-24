! Last Updated: 2019-06-15 23:30:36
!===========================================================
! HTML downloader for auto
!
! Created by : Hisashi Takeda, Ph.D., 2019-06-15
!===========================================================

module dl_auto_mo

  use file_mo
  use px_mo
  
  implicit none

  integer, parameter :: METERS_LAP = 500 ! Distance of a sigle lap in meters
  integer, parameter :: METERS_ADD = 100 ! Additional distance in meters

  type(datetime), allocatable :: days(:)

  type cf_ty

    character(255) :: PLACE     = 'NA'
    character(255) :: DIR_HTML  = 'NA'
    character(255) :: DIR_CSV   = 'NA'
    character(255) :: F_LIC     = 'NA'
    integer        :: MINS_RUN  = -99999
    integer        :: SEC_SLEEP = -99999

  end type 

  type racer_ty

    character(20) :: name  = 'NA'
    character(20) :: place = 'NA'
    integer       :: year_birth  = iNA

  end type

  type racetime_ty

    real(8) :: trial       = iNA ! Minutes
    real(8) :: goal        = iNA ! Minutes
    real(8) :: start       = iNA ! Minutes
    integer :: rank_goal   = iNA
    integer :: handycup    = iNA ! Meters
    logical :: is_trouble  = .false.
    logical :: is_abnormal = .false.

  end type

  type race_ty

    type(datetime)              :: t
    type(racer_ty), allocatable :: rcrs(:)
    integer                     :: nrcrs    = iNA
    integer                     :: nlaps    = iNA
    character(20)               :: title    = 'NA'
    character(20)               :: place    = 'NA'
    character(20)               :: accident = 'NA'
    character(20)               :: weather  = 'NA'
    character(20)               :: road     = 'NA' ! Race road condition
    character(2)                :: rd_c     = 'NA'
    integer                     :: rd       = iNA ! Race round
    integer                     :: meters   = iNA ! Distance of the race
    real(8)                     :: tp       = NA
    real(8)                     :: tp_road  = NA
    real(8)                     :: hm       = NA

    contains

    procedure :: construct_race

  end type

  type, extends(race_ty) :: webpage_ty

    character(255) :: prefix   = 'http://autorace.jp/netstadium/RaceResult/'
    character(255) :: url      = 'NA'
    character(255) :: dir_html = '.'
    character(255) :: dir_csv  = '.'
    character(255) :: fn_html  = 'html'
    character(255) :: fn_csv   = 'csv'

  contains

    procedure :: set_url
    procedure :: get_html
    procedure :: get_csv_from_html
    procedure :: write_csv_files

  end type

  !
  ! Interface for submodule: init_smo.f90
  !
  interface read_config 
    module subroutine read_config (cf, cf_nml, is_print)
      type(cf_ty)                   :: cf
      character(*), intent(in)      :: cf_nml
      logical, intent(in), optional :: is_print
    end subroutine
  end interface

  interface construct_days 
    module subroutine construct_days (days, date_fr, date_to)
      type(datetime), intent(out), allocatable :: days(:)
      character(*),   intent(in)               :: date_fr
      character(*),   intent(in)               :: date_to
    end subroutine
  end interface

  !
  ! Interface for submodule: set_get_url.f90
  !
  interface set_url 
    module subroutine set_url (this)
      class(webpage_ty), intent(inout) :: this
    end subroutine
  end interface

  interface get_html 
    module subroutine get_html (this)
      class(webpage_ty), intent(inout) :: this
    end subroutine
  end interface

  !
  ! Interface for submodule: clean_lines_smo.f90
  !
  interface clean_line_race_conditions
    module subroutine clean_line_race_conditions (line)
      character(*), intent(inout) :: line
    end subroutine
  end interface

  interface clean_line_race_result
    module subroutine clean_line_race_result (line)
      character(*), intent(inout) :: line
    end subroutine
  end interface

  interface clean_line_rank_lap
    module subroutine clean_line_rank_lap (line)
      character(*), intent(inout) :: line
    end subroutine
  end interface

  !
  ! Interface for submodule: extract_tables_smo.f90
  !
  interface extract_tables
    module subroutine extract_tables (lines, lines_we, lines_rank, is_race_ok, file)
      character(*), intent(inout), allocatable :: lines(:), lines_we(:), lines_rank(:)
      logical,      intent(inout)              :: is_race_ok
      character(*), intent(in)                 :: file
    end subroutine
  end interface
  
  interface construct_race
    module subroutine construct_race (this, lines_we, lines)
      class(race_ty), intent(inout) :: this
      character(*),   intent(in)    :: lines_we(:), lines(:)
    end subroutine
  end interface

contains

  subroutine get_csv_from_html (this, year, mon, day, rd, place, dir_html, dir_csv)

    class(webpage_ty), intent(inout) :: this
    integer,           intent(in)    :: year, mon, day, rd
    character(255),    intent(in)    :: place, dir_html, dir_csv
    character(1000), allocatable     :: lines(:), lines_we(:), lines_rank(:)
    logical                          :: is_race_ok
    integer                          :: i

    is_race_ok = .true.

    this%t        = datetime(year = year, month = mon, day = day)
    this%rd       = rd
    this%place    = trim(place)
    this%dir_html = trim(dir_html)
    this%dir_csv  = trim(dir_csv)

    call this%set_url

    call this%get_html

    call extract_tables (lines, lines_we, lines_rank, is_race_ok,&
      file = trim(this%dir_html)//trim(this%place)//'/'//trim(this%fn_html)//'.html')

    if (.not. is_race_ok) then

      print *, 'skipped' 

      return

    end if

    print '(a$)', 'Cleaning data of race conditions ... '

    do i = 1, size(lines_we)

      call clean_line_race_conditions ( lines_we(i) )

    !  print *, 'lines_we: ', trim( lines_we(i) )

    end do

    print '(a)', 'done'

    print '(a$)', 'Constructing racers ... ' 

    call this%construct_race ( lines_we, lines )

    print '(a)', 'done'

    print '(a$)', 'Cleaning data of race results ... ' 

    do i = 1, size(lines)

      call clean_line_race_result ( lines(i) )

    end do

    print '(a)', 'done'

    print '(a$)', 'Cleaning data of rank of lap ... ' 

    do i = 1, size(lines_rank)

      call clean_line_rank_lap ( lines_rank(i) )

    end do

    print '(a)', 'done'

    call this%write_csv_files (lines, lines_we, lines_rank,&
      file = trim(this%dir_csv)//trim(this%place)//'/'//trim(this%fn_csv)//'.csv')

  end subroutine

  subroutine write_csv_files (this, lines, lines_we, lines_rank, file)

    class(webpage_ty), intent(inout)     :: this
    character(*),      intent(in)        :: lines(:), lines_we(:), lines_rank(:)
    character(*),      intent(in)        :: file
    character( len(lines) ), allocatable :: lines2(:) ! No empty lines
    character(1), allocatable            :: ranks_c(:, :)
    character(10), allocatable           :: ranks_lap(:)
    integer, allocatable                 :: bike(:)
    integer, allocatable                 :: bike_rank_lap(:, :) ! Value: # bike
    integer, allocatable                 :: rank_bike_lap(:, :) ! Value: rank
    integer                              :: i, j, k, u, nr, nr2
    integer                              :: rank, lap

    associate ( nrcrs => this%nrcrs, nlaps => this%nlaps )

    allocate ( ranks_c(nrcrs, nlaps), bike_rank_lap(nrcrs, nlaps), rank_bike_lap(nrcrs, nlaps) )
    allocate ( bike(nrcrs) )
    allocate ( ranks_lap(nlaps) )

    print '(a$)', 'Writing a csv file: '//trim(file)//' ... '

    !
    ! Race conditions
    !
#ifdef debug
    nr = size(lines_we)

    print *, ''

    do i = 1, nr

      print '(a, i3, a, i3, a)', 'Line: ', i, '/', nr, '; '//trim( lines_we(i) )

    end do
#endif

    !
    ! Race results
    !
    nr = size(lines)

    allocate ( lines2(nr) )

    k = 1

    do i = 1, nr

      if ( is_empty( lines(i) )) cycle

      lines2(k) = lines(i)

      k = k + 1

    end do

#ifdef debug
    nr2 = k - 1

    print *, ''

    do i = 1, nr2

      print '(a, i3, a, i3, a)', 'Line: ', i, '/', nr2, '; '//trim( lines2(i) )

    end do
#endif

    ! Check if accidents occured. If so, skip writing the CSV file.
    do i = 2, nrcrs * 11, 11

      if ( index (lines2(i), '-') > 0 ) cycle 

      print *, ''
      print *, '***********************************************************'
      print *, trim( lines2(i) )//' occured and this race has been skipped.'
      print *, '***********************************************************'

      return

    end do

    !
    ! Rank of lap 
    !
    nr = size(lines_rank)

    k    = 1
    rank = 1
    lap  = 1
    bike_rank_lap = iNA

    do i = 1, nr

!      print *, 'lines_rank: ', trim( lines_rank(i) )

      if ( is_empty( lines_rank(i) ) .or. lines_rank(i) == 'NA' ) cycle

#ifdef debug
!      print '(a, a, i1, a, i1, a, i2, a, i2)',&
!        'bike: '//trim( lines_rank(i) ), ', rank: ', rank, '/', nrcrs, ', lap: ', lap, '/', nlaps
#endif

      read (lines_rank(i), *) bike_rank_lap(rank, nlaps - lap + 1) 

!      print '(a, i1)', 'bike_rank_lap: ', bike_rank_lap(rank, nlaps - lap + 1)

      rank = rank + 1

      if (rank > nrcrs) then

        rank = 1

        lap  = lap + 1

      end if

    end do

    if (lap - 1 /= nlaps) stop 'Missing laps'

    do rank = 1, nrcrs

      do lap = 1, nlaps

        rank_bike_lap( bike_rank_lap(rank, lap), lap ) = rank

      end do

    end do

#ifdef debug
    print '(a)', repeat('=', 80)
    print '(a)', ' Lap rankings '
    print '(a)', repeat('-', 80)
    print '(a, *(i2, :, "    "))', '           lap: ', [(i, i = 1, nlaps)]
    print '(a)',  repeat('-', 80)

!    do i = 1, nrcrs 
!
!      print '(a, i1, a, *(i2, :, " -> "))', 'Bike: ', i, ', Rank: ', rank_bike_lap(i, :)
!
!    end do
#endif

    !
    ! Get bike number
    !
    k = 1

!    print *, ''

    do i = 3, nrcrs * 11, 11

      if (.not. is_numeric(lines2(i)) ) stop trim( lines2(i) )//' is NaN'

      read ( lines2(i), * ) bike(k)

      !print *, 'bike: ',  bike(k)

      k = k + 1

    end do

    ! Convert integer to character for CSV writing
    do lap = 1, nlaps

      do i = 1, nrcrs

!        print *, 'Bike: ', bike(i), 'lap: ', lap, 'Rank: ', rank_bike_lap(bike(i), lap)

        write ( ranks_c(i, lap), '(i1)' ) rank_bike_lap( bike(i), lap )

      end do

    end do

#ifdef debug
    do i = 1, nrcrs
      print '(a, i1, a, *(a2, :, " -> "))', 'Racer: ', i, ', Rank: ', ranks_c(i, :)
    end do

    print '(a)', repeat('=', 80)
#endif

    !
    ! Write race results and conditions
    !
    do i = 1, nlaps

      write ( ranks_lap(i), '(a8,  i2)' ) 'rank_lap',  i

    end do

    call execute_command_line ( 'mkdir -p '//trim( get_dirname(file) ) )

    open (newunit = u, file = file, status = 'replace')

    write (u, FMT_CSV_STR)&
      "place", "date", "rd", "meters_distance", "weather", "tp", "hm", "tp_road", "road", &
      "bike", "kanji_name_racer", &
      "name_racer", "name_bike", "meters_handycup", &
      "mins_trial", "mins_race", "mins_start", "violation", &
      ranks_lap

    k = 1

    do i = 1, nrcrs * 11, 11

      write (u, '(*(a, :, ","))')   &
        trim( this%place  ),        & ! Place
        trim( this%t%dateformat()), & ! Date
        trim( this%rd_c   ),        & ! Round
        trim( lines_we(1) ),        & ! Distance in meters 
        trim( lines_we(2) ),        & ! Weather condition
        trim( lines_we(3) ),        & ! Temperature
        trim( lines_we(4) ),        & ! Humidity
        trim( lines_we(5) ),        & ! Temperature of road
        trim( lines_we(6) ),        & ! Road condition
        [(lines2(i + j), j = 2, 10)], &  ! bike, kanji_name_racer, name_racer, name_bike, meters_handycup, mins_trial, mins_race, mins_start, violation
        ranks_c(k, :)

      k = k + 1

    end do

    close (u)

    end associate

    print '(a)', 'done'

  end subroutine

end module
