! Last Updated: 2019-06-15 23:30:36
!===========================================================
! HTML downloader for auto
!
! Created by : Hisashi Takeda, Ph.D., 2019-06-15
!===========================================================

module dl_auto_mo

  use file_mo
  use px_mo
  use csv2sqlite_mo
  
  implicit none

  integer, parameter :: METERS_LAP = 500 ! Distance of a sigle lap in meters
  integer, parameter :: METERS_ADD = 100 ! Additional distance in meters

  type(datetime), allocatable :: days(:)

  type payout_ty

    character(20) :: exacta   ! 二連単
    character(20) :: quinella ! 二連複
    character(20) :: trifecta ! 三連単
    character(20) :: trio     ! 三連複
    character(20) :: win      ! 単勝
    character(20) :: place(3) ! 複勝
    character(20) :: wide(3)  ! ワイド 

  end type

  type(payout_ty) :: payout

  type cf_ty

    character(255) :: PLACE     = 'NA'
    character(255) :: DB_AUTO   = 'NA'
    character(255) :: TB_RACE   = 'NA'
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
    procedure :: dl_auto
    procedure :: construct_race
    procedure :: extract_tables
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

  interface clean_line_pay
    module subroutine clean_line_pay (line)
      character(*), intent(inout) :: line
    end subroutine
  end interface

  !
  ! Interface for submodule: extract_tables_smo.f90
  !
  interface extract_tables
    module subroutine extract_tables &
        (this, lines, lines_cd, lines_rank, lines_pay, skipped, file)
      class(webpage_ty), intent(inout)              :: this
      character(*),      intent(inout), allocatable :: lines(:), lines_cd(:)
      character(*),      intent(inout), allocatable :: lines_rank(:), lines_pay(:)
      logical,           intent(inout)              :: skipped
      character(*),      intent(in)                 :: file
    end subroutine
  end interface
  
  interface construct_race
    module subroutine construct_race (this, lines_cd, lines)
      class(webpage_ty), intent(inout) :: this
      character(*),      intent(in)    :: lines_cd(:), lines(:)
    end subroutine
  end interface

  !
  ! Interface for submodule: write_csv_files_smo.f90
  !
  interface write_csv_files
    module subroutine write_csv_files (this, lines, lines_cd, lines_rank, skipped, file)
      class(webpage_ty), intent(inout) :: this
      character(*),      intent(in)    :: lines(:), lines_cd(:), lines_rank(:)
      logical,           intent(inout) :: skipped
      character(*),      intent(in)    :: file
    end subroutine
  end interface

contains

  subroutine dl_auto (this, cf, year, mon, day, rd, place, dir_html, dir_csv)

    class(webpage_ty), intent(inout) :: this
    type(cf_ty),       intent(in)    :: cf
    integer,           intent(in)    :: year, mon, day, rd
    character(*),      intent(in)    :: place, dir_html, dir_csv
    character(1000), allocatable     :: lines(:), lines_cd(:)
    character(1000), allocatable     :: lines_rank(:), lines_pay(:)
    logical                          :: skipped
    character(20)                    :: ctys(33) ! N.B. Need to change if you increase columns

    this%t        = datetime(year = year, month = mon, day = day)
    this%rd       = rd
    this%place    = trim(place)
    this%dir_html = trim(dir_html)
    this%dir_csv  = trim(dir_csv)

    !
    ! Skipped date and round list
    !
!    if ( this%place == 'kawaguchi' ) then
!
!      if ( this%t%dateformat() == '2011-03-12') return
!      if ( this%t%dateformat() == '2011-03-13') return
!      if ( this%t%dateformat() == '2011-03-14') return
!
!    end if

!    if ( this%place == 'isesaki' ) then
!
!      if ( this%t%dateformat() == '2011-03-15') return
!      if ( this%t%dateformat() == '2011-03-16') return
!      if ( this%t%dateformat() == '2011-03-17') return
!      if ( this%t%dateformat() == '2011-03-26') return
!      if ( this%t%dateformat() == '2011-03-27') return
!      if ( this%t%dateformat() == '2011-03-28') return
!      if ( this%t%dateformat() == '2011-03-29') return
!
!    end if

    call this%set_url

    call this%get_html

    skipped = .false.

    call this%extract_tables ( lines, lines_cd, lines_rank, lines_pay, skipped, &
      file = trim(this%dir_html)//trim(this%place)//'/'//trim(this%fn_html)//'.html' )

    if (skipped) return

    call this%write_csv_files ( lines, lines_cd, lines_rank, skipped, &
      file = trim(this%dir_csv)//trim(this%place)//'/'//trim(this%fn_csv)//'.csv' )

    if (skipped) return

    ctys( 1) = 'char(80)' ! key
    ctys( 2) = 'char(30)' ! place
    ctys( 3) = 'char(10)' ! date
    ctys( 4) = 'integer'  ! round
    ctys( 5) = 'integer'  ! distance
    ctys( 6) = 'char(10)' ! we
    ctys( 7) = 'real'     ! tp
    ctys( 8) = 'real'     ! hm
    ctys( 9) = 'real'     ! tp_road
    ctys(10) = 'char(30)' ! road
    ctys(11) = 'integer'  ! nlaps
    ctys(12) = 'integer'  ! rank_goal
    ctys(13) = 'integer'  ! bike
    ctys(14) = 'char(50)' ! kanji_name_racer
    ctys(14) = 'char(50)' ! name_racer
    ctys(15) = 'char(50)' ! name_biki
    ctys(16) = 'integer'  ! handycup
    ctys(17) = 'real'     ! sec_trial
    ctys(18) = 'real'     ! sec_race
    ctys(19) = 'real'     ! sec_start
    ctys(20) = 'char(50)' ! violation
    ctys(21) = 'integer'  ! payout_win
    ctys(22) = 'integer'  ! payout_place
    ctys(23:)= 'integer'  ! rank_lap 

    call csv2sqlite ( dbnm    = cf%DB_AUTO, &
                      tbnm    = cf%TB_RACE, &
                      primary = 'key',      &
                      ctys    = ctys,       &
                      csv     = trim(this%dir_csv)//trim(this%place)//'/'//trim(this%fn_csv)//'.csv')


  end subroutine

end module
