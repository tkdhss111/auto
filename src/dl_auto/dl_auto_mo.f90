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

    character(20)  :: title   = 'NA'
    character(20)  :: place   = 'NA'
    character(20)  :: weather = 'NA'
    character(20)  :: road    = 'NA' ! Race road condition
    integer        :: year    = iNA
    integer        :: mon     = iNA
    integer        :: day     = iNA
    integer        :: rd      = iNA ! Race round
    integer        :: dist    = iNA ! Distance of the race
    real(8)        :: tp      = NA
    real(8)        :: tp_road = NA
    real(8)        :: hm      = NA

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

  end type

contains

  subroutine read_config (cf, cf_nml, is_print)
    
    type(cf_ty)                   :: cf
    character(*), intent(in)      :: cf_nml
    logical, intent(in), optional :: is_print
    integer u

    namelist /config/ cf 

    print '(a$)', 'Reading configuration file ... '

    open (newunit = u, file = cf_nml, status = 'old')

    read (u, nml = config)

    close (u)

    print '(a)', 'done'

    if (present (is_print)) then

      if (is_print) then

        print '(a)', 'PLACEk   : '//trim(cf%PLACE   ) 
        print '(a)', 'DIR_HTML : '//trim(cf%DIR_HTML) 
        print '(a)', 'DIR_CSV  : '//trim(cf%DIR_CSV ) 
        print '(a)', 'F_LIC    : '//trim(cf%F_LIC   ) 
        print *,     'MINS_RUN : ', cf%MINS_RUN      
        print *,     'SEC_SLEEP: ', cf%SEC_SLEEP     

      end if

    end if

  end subroutine

  subroutine construct_days (days, date_fr, date_to)

    type(datetime), intent(inout), allocatable :: days(:)
    character(*),   intent(in)                 :: date_fr
    character(*),   intent(in)                 :: date_to
    type(datetime)                             :: t_fr, t_to
    integer                                    :: ndays

    t_fr = strptime (date_fr//' 00:00:00', "%Y-%m-%d %H:%M:%S")
    t_to = strptime (date_to//' 00:00:00', "%Y-%m-%d %H:%M:%S")
    days = datetimeRange(t_fr, t_to, timedelta(days = 7))
    ndays = size(days)

  end subroutine

  subroutine get_csv_from_html (this, year, mon, day, rd, place, dir_html, dir_csv)

    class(webpage_ty), intent(inout) :: this
    integer,           intent(in)    :: year, mon, day, rd
    character(255),    intent(in)    :: place, dir_html, dir_csv
    character(1000), allocatable     :: lines(:)
    integer                          :: i

    this%year     = year
    this%mon      = mon
    this%day      = day
    this%rd       = rd
    this%place    = trim(place)
    this%dir_html = trim(dir_html)
    this%dir_csv  = trim(dir_csv)

    call this%set_url

    call this%get_html

    call read_html_files (lines, file = trim(this%dir_html)//trim(this%fn_html)//'.html')

    do concurrent ( i = 1:size(lines) )

      call clean_line ( lines(i) )

    end do

    call write_csv_files (lines, file = trim(this%dir_csv)//trim(this%fn_csv)//'.csv')

  end subroutine

  subroutine set_url (this)

    class(webpage_ty), intent(inout) :: this
    character(4)                     :: year_c
    character(2)                     :: mon_c
    character(2)                     :: day_c
    character(1)                     :: rd_c

    write(year_c, '(i4.0)') this%year
    write(mon_c,  '(i2.2)') this%mon
    write(day_c,  '(i2.2)') this%day
    write(rd_c,   '(i0)'  ) this%rd

    this%url = trim(this%prefix)//trim(this%place)//'/'//&
               trim(year_c)//'-'//trim(mon_c)//'-'//trim(day_c)//'_'//trim(rd_c)

    print '(a)', 'URL: ', trim(this%url)

    write (this%fn_html,&
      '(i4.0, "-", i2.2, "-", i2.2, "_", i0, "_", a)') &
      this%year, this%mon, this%day, this%rd, trim(this%place)

  end subroutine

  subroutine get_html (this)

    class(webpage_ty), intent(inout) :: this
    character(255)                   :: cmd

    print '(a)', 'Downloading htm: ', trim(this%url), ' to ', trim(this%dir_html)

    cmd = trim('curl "'//trim(this%url)//'" -o "'//trim(this%dir_html)//&
          trim(this%fn_html)//'.html"')

    print *, trim(cmd)

    call execute_command_line ( 'mkdir -p '//trim(this%dir_html) )

    call execute_command_line(cmd)

  end subroutine

  subroutine read_html_files (lines, file)

    character(*), allocatable, intent(inout) :: lines(:)
    character( len(lines) ), allocatable     :: lines_(:)
    character(*), intent(in)                 :: file
    logical                                  :: is_table
    integer                                  :: i_fr, i_to
    integer                                  :: i, u, nr

    print '(a$)', 'Opening a html file: '//trim(file)//' ... '

    is_table = .false.

    open (newunit = u, file = file, status = 'old')

    nr = count_rows (u)

    allocate ( lines_(nr) )
    
    i_fr = 1
    i_to = nr

    do i = 1, nr

      read (u, '(a)') lines_(i)

      ! Begining of table
      if ( index(lines_(i), '<td class="light f16">') > 0 ) then

        i_fr = i

        is_table = .true.

      end if

      ! End of the table
      if ( index(lines_(i), '</table>') > 0 .and. is_table) i_to = i - 1

    end do

    close (u)

    allocate ( lines(i_to - i_fr + 1) )

    lines = lines_(i_fr:i_to)

    print '(a)', 'done'

  end subroutine

  subroutine write_csv_files (lines, file)

    character(*), intent(in) :: lines(:)
    character(*), intent(in) :: file
    integer                  :: i, u, nr

    print '(a$)', 'Opening a csv file: '//trim(file)//' ... '

    call execute_command_line ( 'mkdir -p '//trim( get_dirname(file) ) )

    open (newunit = u, file = file, status = 'replace')

    nr = size(lines)

    write (u, FMT_CSV_STR) "race_title", "year", "mon", "day", "rd", "place", "name", "handycup"

    do i = 1, nr

      print '(a)', trim( lines(i) )

      write (u, '(a)') lines(i)

    end do

    close (u)

    print '(a)', 'done'

  end subroutine

  pure subroutine clean_line (line)

    character(*), intent(inout) :: line
    character(len(line))        :: line2
    logical                     :: is_in_blacket, is_j_block_nolocked(5000)
    integer                     :: i_fr, i_to
    integer                     :: i, k

    is_in_blacket       = .false.
    is_j_block_nolocked = .false.

    i_fr = 0
    i_to = 0

    line2 = ''

    call string_replace (line, ' class="light f16"',        '')
    call string_replace (line, ' class="f16"',              '')
    call string_replace (line, ' class="td_blue_center"',   '')
    call string_replace (line, ' class="td_orange_center"', '')

    line = line( index(line, '<td'):index(line, '</td>', back = .true.) - 1 ) ! Omit <tr> tags

    i_fr = index(line, 'alt') + 5

    if (i_fr > 5) then

      i_to = i_fr + index(line(i_fr:), '"') - 2

      is_j_block_nolocked(i_fr:i_to) = .true.

      !print *, 'Word: ', line(i_fr:i_to), ' has been blocked.'

    end if

    !
    ! Letter by letter process: delete unwanted letters
    !
    do i = 1, len_trim(line)

      if (line(i:i) == '<') then

        is_in_blacket = .true.

      end if

      if (line(i:i) == '>') then

        is_in_blacket = .false.

        line(i:i) = ''

      end if

      if (line(i:i+2) == '/td') then

        line(i-1:i-1) = ','

      end if

      if (is_in_blacket .and. line(i:i) /= ',' .and. .not. is_j_block_nolocked(i)) then

        line(i:i) = ''

      end if

    end do

    !
    ! Delete white space in the line
    !
    k = 1

    do i = 1, len(line)

      if (line(i:i) == ' ') cycle

      line2(k:k) = line(i:i)

      k = k + 1

    end do

  end subroutine

end module
