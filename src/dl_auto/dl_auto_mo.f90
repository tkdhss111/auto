! Last Updated: 2019-06-15 23:30:36
!===========================================================
! HTML downloader for auto
!
! Created by : Hisashi Takeda, Ph.D., 2019-06-15
!===========================================================

module dl_auto_mo

  use file_mo
  use px_mo
  use string_helpers
  
  implicit none

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

    character(20)  :: title   = 'NA'
    character(20)  :: place   = 'NA'
    character(20)  :: weather = 'NA'
    character(20)  :: road    = 'NA' ! Race road condition
    character(4)   :: year_c  = 'NA'
    character(2)   :: mon_c   = 'NA'
    character(2)   :: day_c   = 'NA'
    character(2)   :: rd_c    = 'NA'
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
    procedure :: write_csv_files

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

    type(datetime), intent(out), allocatable :: days(:)
    character(*),   intent(in)               :: date_fr
    character(*),   intent(in)               :: date_to
    type(datetime)                           :: t_fr, t_to
    integer                                  :: ndays

    t_fr = strptime (date_fr//' 00:00:00', "%Y-%m-%d %H:%M:%S")
    t_to = strptime (date_to//' 00:00:00', "%Y-%m-%d %H:%M:%S")
    days = datetimeRange(t_fr, t_to, timedelta(days = 1))
    ndays = size(days)

  end subroutine

  subroutine get_csv_from_html (this, year, mon, day, rd, place, dir_html, dir_csv)

    class(webpage_ty), intent(inout) :: this
    integer,           intent(in)    :: year, mon, day, rd
    character(255),    intent(in)    :: place, dir_html, dir_csv
    character(1000), allocatable     :: lines(:), lines_we(:)
    logical                          :: is_race_open
    integer                          :: i

    is_race_open = .true.

    this%year     = year
    this%mon      = mon
    this%day      = day
    this%rd       = rd
    this%place    = trim(place)
    this%dir_html = trim(dir_html)
    this%dir_csv  = trim(dir_csv)

    call this%set_url

    call this%get_html

    call read_lines_html (lines, lines_we, is_race_open, &
      file = trim(this%dir_html)//trim(this%place)//'/'//trim(this%fn_html)//'.html')

    if (.not. is_race_open) return

    do i = 1, size(lines)

      call clean_line_race_result ( lines(i) )

    end do

    do i = 1, size(lines_we)

      call clean_line_race_conditions ( lines_we(i) )

    end do

    call this%write_csv_files (lines, lines_we,&
      file = trim(this%dir_csv)//trim(this%place)//'/'//trim(this%fn_csv)//'.csv')

  end subroutine

  subroutine set_url (this)

    class(webpage_ty), intent(inout) :: this

    write(this%year_c, '(i4.0)') this%year
    write(this%mon_c,  '(i2.2)') this%mon
    write(this%day_c,  '(i2.2)') this%day
    write(this%rd_c,   '(i0)'  ) this%rd

    this%url = trim(this%prefix)//trim(this%place)//'/'//&
               trim(this%year_c)//'-'//trim(this%mon_c)//'-'//trim(this%day_c)//'_'//trim(this%rd_c)
#ifdef debug
    print '(a)', 'URL: '//trim(this%url)
#endif

    write (this%fn_html,&
      '(i4.0, "-", i2.2, "-", i2.2, "_", i0)') &
      this%year, this%mon, this%day, this%rd

    this%fn_csv = this%fn_html

  end subroutine

  subroutine get_html (this)

    class(webpage_ty), intent(inout) :: this
    character(255)                   :: cmd, outfile
    logical                          :: exist

#ifdef debug
    print '(a)', 'Downloading htm: '//trim(this%url)//' to '//trim(this%dir_html)
#endif

    outfile = trim(this%dir_html)//trim(this%place)//'/'//trim(this%fn_html)//'.html'

    inquire (file = outfile, exist = exist)

    if (exist) return

    cmd = trim('curl "'//trim(this%url)//'" -o "'//trim(outfile)//'"')

#ifdef debug
    print *, trim(cmd)
#endif

    call execute_command_line ( 'mkdir -p '//trim(this%dir_html)//trim(this%place)//'/' )

    call execute_command_line(cmd)

  end subroutine

  subroutine read_lines_html (lines, lines_we, is_race_open, file)

    character(*), allocatable, intent(inout) :: lines(:), lines_we(:)
    character( len(lines) ), allocatable     :: lines_(:)
    character(*), intent(in)                 :: file
    logical,      intent(inout)              :: is_race_open
    logical                                  :: is_table, is_table_we
    integer                                  :: i_fr, i_to
    integer                                  :: i_fr_we, i_to_we
    integer                                  :: i, u, nr

    print '(a$)', 'Opening a html file: '//trim(file)//' ... '

    is_table = .false.

    open (newunit = u, file = file, status = 'old')

    nr = count_rows (u)

    allocate ( lines_(nr) )
    
    i_fr        = 1
    i_fr_we     = 1
    i_to        = nr
    i_to_we     = nr
    is_table    = .false.
    is_table_we = .false.

    !
    ! Check if race is open
    !
    do i = 1, nr

      read (u, '(a)') lines_(i)

      if ( index(lines_(i), '本日の開催情報はありません') > 0 ) then

        print *, 'Skipped since no race on that day'
        is_race_open = .false.

        return

      end if

    end do

    rewind (u)

    !
    ! Transaction for race results
    !
    do i = 1, nr

      read (u, '(a)') lines_(i)

      ! Begining of table
      if ( index(lines_(i), '<td class="light f16">1</td>') > 0 .and. .not. is_table ) then

        i_fr = i

        is_table = .true.

      end if

      ! End of the table
      if ( index(lines_(i), '</table>') > 0 .and. is_table) then

        i_to = i - 2

        exit

      end if

    end do

    lines = lines_(i_fr:i_to)

    rewind (u)

    !
    ! Transaction for race conditions 
    !
    do i = 1, nr

      read (u, '(a)') lines_(i)

      ! Begining of table
      if ( index(lines_(i), 'ｍ</td>') > 0 .and. .not. is_table_we ) then

        i_fr_we = i

        is_table_we = .true.

      end if

      ! End of the table
      if ( index(lines_(i), '</table>') > 0 .and. is_table_we) then

        i_to_we = i - 3

        exit

      end if

    end do

    lines_we = lines_(i_fr_we:i_to_we)

    close (u)

    print '(a)', 'done'

  end subroutine

  subroutine clean_line_race_result (line)

    character(*), intent(inout) :: line
    integer                     :: i_a_fr, n_to

    call string_replace (line, ' class="light f16"',        '')
    call string_replace (line, ' class="f8"',               '')
    call string_replace (line, ' class="f16"',              '')
    call string_replace (line, ' class="td_white_center"',  '')
    call string_replace (line, ' class="td_black_center"',  '')
    call string_replace (line, ' class="td_blue_center"',   '')
    call string_replace (line, ' class="td_orange_center"', '')
    call string_replace (line, ' class="td_green_center"',  '')
    call string_replace (line, ' class="td_yellow_center"', '')
    call string_replace (line, ' class="td_pink_center"',   '')
    call string_replace (line, ' class="td_red_center"',    '')
    call string_replace (line, '</a>',                      '')
    call string_replace (line, '<tr>',                      '')
    call string_replace (line, '</tr>',                     '')
    call string_replace (line, '<td></td>',               'NA')
    call string_replace (line, '<td>',                      '')
    call string_replace (line, '</td>',                     '')
    call string_replace (line, '　',                       ' ')

    if (index(line, '<a') > 0) then

      i_a_fr = index(line, '<a')
      n_to = index(line(i_a_fr:), '>') - 1

      line(1:i_a_fr + n_to) = ''
      
    end if

    line = adjustl(line)

  end subroutine

  subroutine clean_line_race_conditions (line)

    character(*), intent(inout) :: line

    call string_replace (line, '<td>',  '')
    call string_replace (line, '</td>', '')
    call string_replace (line, 'ｍ',    '')
    call string_replace (line, '℃',     '')
    call string_replace (line, '％',    '')

    line = adjustl(line)

  end subroutine

  subroutine write_csv_files (this, lines, lines_we, file)

    class(webpage_ty), intent(inout)     :: this
    character(*), intent(in)             :: lines(:), lines_we(:)
    character(*), intent(in)             :: file
    character( len(lines) ), allocatable :: lines2(:) ! No empty lines
    integer                              :: i, j, k, u, nr

    !print '(a$)', 'Opening a csv file: '//trim(file)//' ... '

    call execute_command_line ( 'mkdir -p '//trim( get_dirname(file) ) )

    open (newunit = u, file = file, status = 'replace')

    nr = size(lines)

    allocate ( lines2(nr) )

    write (u, FMT_CSV_STR)&
      "place", "year", "mon", "day", "rd",&
      "meters_distance", "weather", "tp", "hm", "tp_road", "road",&
      "rank_goal", "accident", "no_motercycle", "kanji_name_racer",&
      "name_racer", "name_motercycle", "meters_handycup",&
      "mins_trial", "mins_race", "mins_start", "violation"

    k = 1

    do i = 1, nr

      if ( is_empty( lines(i) )) cycle

#ifdef debug
      print '(a, i3, a, i3, a, i0)', 'Line: ', i, '/', nr, '; '//trim( lines(i) )//', len:', len_trim( lines(i) )
#endif

      lines2(k) = lines(i)

      k = k + 1

    end do

    nr = size(lines_we)

    do i = 1, nr 

      print '(a, i3, a, i3, a)', 'Line: ', i, '/', nr, '; '//trim( lines_we(i) )

    end do

    !
    ! Write race results and conditions
    !
    do i = 1, k - 11, 11

      write (u, '(*(a, :, ","))') &
        trim( this%place  ),      &
        trim( this%year_c ),      &
        trim( this%mon_c  ),      &
        trim( this%day_c  ),      &
        trim( this%rd_c   ),      &
        trim( lines_we(1) ),      &
        trim( lines_we(2) ),      &
        trim( lines_we(3) ),      &
        trim( lines_we(4) ),      &
        trim( lines_we(5) ),      &
        trim( lines_we(6) ),      &
        [(lines2(i + j), j = 0, 10)]

    end do

    close (u)

    !print '(a)', 'done'

  end subroutine

end module
