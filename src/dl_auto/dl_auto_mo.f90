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

  type payout_ty

    character(20) :: exacta   ! 二連単
    character(20) :: quinella ! 二連複
    character(20) :: trifecta ! 三連単
    character(20) :: trio     ! 三連複
    character(20) :: win      ! 単勝
    character(20) :: place(3) ! 複勝
    character(20) :: wide(3)  ! ワイド 

    !integer :: exacta   ! 二連単
    !integer :: quinella ! 二連複
    !integer :: trifecta ! 三連単
    !integer :: trio     ! 三連複
    !integer :: win      ! 単勝
    !integer :: place(3) ! 複勝
    !integer :: wide(3)  ! ワイド 
  end type

  type(payout_ty) :: payout

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
        (this, lines, lines_cd, lines_rank, lines_pay, is_race_ok, file)
      class(webpage_ty), intent(inout)         :: this
      character(*), intent(inout), allocatable :: lines(:), lines_cd(:)
      character(*), intent(inout), allocatable :: lines_rank(:), lines_pay(:)
      logical,      intent(inout)              :: is_race_ok
      character(*), intent(in)                 :: file
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
    module subroutine write_csv_files (this, lines, lines_cd, lines_rank, file)
      class(webpage_ty), intent(inout)     :: this
      character(*),      intent(in)        :: lines(:), lines_cd(:), lines_rank(:)
      character(*),      intent(in)        :: file
    end subroutine
  end interface

contains

  subroutine dl_auto (this, year, mon, day, rd, place, dir_html, dir_csv)

    class(webpage_ty), intent(inout) :: this
    integer,           intent(in)    :: year, mon, day, rd
    character(255),    intent(in)    :: place, dir_html, dir_csv
    character(1000), allocatable     :: lines(:), lines_cd(:)
    character(1000), allocatable     :: lines_rank(:), lines_pay(:)
    logical                          :: is_race_ok

    is_race_ok = .true.

    this%t        = datetime(year = year, month = mon, day = day)
    this%rd       = rd
    this%place    = trim(place)
    this%dir_html = trim(dir_html)
    this%dir_csv  = trim(dir_csv)

    call this%set_url

    call this%get_html

    call this%extract_tables (lines, lines_cd, lines_rank, lines_pay, is_race_ok, &
      file = trim(this%dir_html)//trim(this%place)//'/'//trim(this%fn_html)//'.html')

    if (.not. is_race_ok) then

#ifdef debug
      print *, 'skipped' 
#endif

      return

    end if

    call this%write_csv_files (lines, lines_cd, lines_rank, &
      file = trim(this%dir_csv)//trim(this%place)//'/'//trim(this%fn_csv)//'.csv')

  end subroutine

end module
