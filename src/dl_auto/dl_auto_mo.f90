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

  integer, parameter :: NRCRS      = 8   ! Number of racers
  integer, parameter :: METERS_RUN = 400 ! Distance of a sigle run in meters
  integer, parameter :: METERS_ADD = 300 ! Additional distance in meters

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

    type(datetime) :: t
    character(20)  :: title    = 'NA'
    character(20)  :: place    = 'NA'
    character(20)  :: accident = 'NA'
    character(20)  :: weather  = 'NA'
    character(20)  :: road     = 'NA' ! Race road condition
    character(2)   :: rd_c     = 'NA'
    integer        :: rd       = iNA ! Race round
    integer        :: meters   = iNA ! Distance of the race
    real(8)        :: tp       = NA
    real(8)        :: tp_road  = NA
    real(8)        :: hm       = NA

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
    character(1000), allocatable     :: lines(:), lines_we(:), lines_rank(:)
    logical                          :: is_race_open
    integer                          :: i

    is_race_open = .true.

    this%t        = datetime(year = year, month = mon, day = day)
    this%rd       = rd
    this%place    = trim(place)
    this%dir_html = trim(dir_html)
    this%dir_csv  = trim(dir_csv)

    call this%set_url

    call this%get_html

    call read_lines_html (lines, lines_we, lines_rank, is_race_open, &
      file = trim(this%dir_html)//trim(this%place)//'/'//trim(this%fn_html)//'.html')

    if (.not. is_race_open) then

      print *, 'Skipped since no race' 

      return

    end if

    print '(a$)', 'Cleaning data of race conditions ... '

    do i = 1, size(lines_we)

      call clean_line_race_conditions ( lines_we(i) )

    end do

    print '(a)', 'done'

    print '(a$)', 'Cleaning data of race results ... ' 

    do i = 1, size(lines)

      call clean_line_race_result ( lines(i) )

    end do

    print '(a)', 'done'

    print '(a$)', 'Cleaning data of rank of run ... ' 

    do i = 1, size(lines_rank)

      call clean_line_rank_run ( lines_rank(i) )

    end do

    print '(a)', 'done'

    call this%write_csv_files (lines, lines_we, lines_rank,&
      file = trim(this%dir_csv)//trim(this%place)//'/'//trim(this%fn_csv)//'.csv')

  end subroutine

  subroutine set_url (this)

    class(webpage_ty), intent(inout) :: this

    write(this%rd_c, '(i0)') this%rd

    write (this%fn_html, '(a)') trim(this%t%strftime('%Y-%m-%d'))//'_'//trim(this%rd_c)

#ifdef debug
    print '(a)', 'URL: '//trim(this%url)
#endif

    this%url = trim(this%prefix)//trim(this%place)//'/'//trim(this%fn_html)
    
    this%fn_csv = this%fn_html

  end subroutine

  subroutine get_html (this)

    class(webpage_ty), intent(inout) :: this
    character(255)                   :: cmd, outfile
    logical                          :: exist

#ifdef debug
    print '(a$)', 'Downloading htm: '//trim(this%url)//' ... '
#endif

    outfile = trim(this%dir_html)//trim(this%place)//'/'//trim(this%fn_html)//'.html'

    inquire (file = outfile, exist = exist)

    if (exist) then

      print '(a)', 'skipped' ! Since the HTML file already exists

      return

    end if

    cmd = trim('curl "'//trim(this%url)//'" -o "'//trim(outfile)//'"')

#ifdef debug
    print *, trim(cmd)
#endif

    call execute_command_line ( 'mkdir -p '//trim(this%dir_html)//trim(this%place)//'/' )

    call execute_command_line(cmd)

    print '(a)', 'done'

    call sleep ( int( rand() * 3 ) )

  end subroutine

  subroutine read_lines_html (lines, lines_we, lines_rank, is_race_open, file)

    character(*), allocatable, intent(inout) :: lines(:), lines_we(:), lines_rank(:)
    character( len(lines) ), allocatable     :: lines_(:)
    character(*), intent(in)                 :: file
    logical,      intent(inout)              :: is_race_open
    logical                                  :: is_table, is_table_we, is_table_rank
    integer                                  :: i_fr, i_to
    integer                                  :: i_fr_we, i_to_we
    integer                                  :: i_fr_rank, i_to_rank
    integer                                  :: i, u, nr

    print '(a$)', 'Opening a html file: '//trim(file)//' ... '

    is_table = .false.

    open (newunit = u, file = file, status = 'old')

    nr = count_rows (u)

    allocate ( lines_(nr) )
    
    i_fr          = 1
    i_fr_we       = 1
    i_fr_rank     = 1
    i_to          = nr
    i_to_we       = nr
    i_to_rank     = nr
    is_table      = .false.
    is_table_we   = .false.
    is_table_rank = .false.

    !
    ! Check if race is open
    !
    do i = 1, nr

      read (u, '(a)') lines_(i)

      if ( index(lines_(i), '本日の開催情報はありません') > 0 ) then

        print *, 'Skipped since no race on that day'

        is_race_open = .false.

        close (u)

        return

      end if

    end do

    !
    ! Transaction for race conditions 
    !
    rewind (u)

    lines_ = ''

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

    !
    ! Transaction for race results
    !
    rewind (u)

    lines_ = ''

    do i = 1, nr

      read (u, '(a)') lines_(i)

      ! Begining of table
      if ( index(lines_(i), '<td class="light f16">1</td>') > 0 .and. .not. is_table ) then

        i_fr = i

        is_table = .true.

      end if

      ! End of the table
      if ( index(lines_(i), '</table>') > 0 .and. is_table) then

        i_to = i - 3

        exit

      end if

    end do

    lines = lines_(i_fr:i_to)


    !
    ! Transaction for rank of run 
    !
    rewind (u)

    lines_ = ''

    do i = 1, nr

      read (u, '(a)') lines_(i)

      ! Begining of table
      if ( index(lines_(i), 'ゴール線通過') > 0 .and. .not. is_table_rank ) then

        i_fr_rank = i + 1

        is_table_rank = .true.

      end if

      ! End of the table
      if ( index(lines_(i), '</table>') > 0 .and. is_table_rank) then

        i_to_rank = i - 3

        exit

      end if

    end do

    lines_rank = lines_(i_fr_rank:i_to_rank)

    close (u)

    print '(a)', 'done'

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

  subroutine clean_line_race_result (line)

    character(*), intent(inout) :: line
    integer                     :: i_a_fr, i_a_to

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
    call string_replace (line, '<font class="bold" color="red">再</font>', '')
    call string_replace (line, '－',                      'NA')
    call string_replace (line, '<tr>',                      '')
    call string_replace (line, '</tr>',                     '')
    call string_replace (line, '<td></td>',                '-')
    call string_replace (line, '<td>',                      '')
    call string_replace (line, '</td>',                     '')
    call string_replace (line, '　',                       ' ')
    call string_replace (line, achar(9),                    '')

    if (index(line, '<a') > 0) then

      i_a_fr = index(line, '<a')
      i_a_to = index(line, '>')

      line = line(1:i_a_fr + 1)//line( i_a_to:len_trim(line) )

    end if

    call string_replace (line, '<a></a>', 'NA')
    call string_replace (line, '<a>',  '')
    call string_replace (line, '</a>', '')

    line = adjustl(line)

  end subroutine

  subroutine clean_line_rank_run (line)

    character(*), intent(inout) :: line

    call string_replace (line, '<td class="td_orange_center">', '')
    call string_replace (line, '<td class="td_blue_center">',   '')
    call string_replace (line, '<td class="td_black_center">',  '')
    call string_replace (line, '<td class="td_yellow_center">', '')
    call string_replace (line, '<td class="td_green_center">',  '')
    call string_replace (line, '<td class="td_red_center">',    '')
    call string_replace (line, '<td class="td_white_center">',  '')
    call string_replace (line, '<td class="td_pink_center">',   '')
    call string_replace (line, '<tr class="td_white_center">',        '')
    call string_replace (line, '<td class="light txtArea">1周回</td>', '')
    call string_replace (line, '<td class="light txtArea">2周回</td>', '')
    call string_replace (line, '<td class="light txtArea">3周回</td>', '')
    call string_replace (line, '<td class="light txtArea">4周回</td>', '')
    call string_replace (line, '<td class="light txtArea">5周回</td>', '')
    call string_replace (line, '<td class="light txtArea">6周回</td>', '')
    call string_replace (line, '<td class="light txtArea">7周回</td>', '')
    call string_replace (line, '<td class="light txtArea">8周回</td>', '')
    call string_replace (line, '<td class=""></td>', '8')
    call string_replace (line, '</tr>', '')
    call string_replace (line, '<td>',  '')
    call string_replace (line, '</td>', '')

    line = adjustl(line)

  end subroutine

  subroutine write_csv_files (this, lines, lines_we, lines_rank, file)

    class(webpage_ty), intent(inout)     :: this
    character(*),      intent(in)        :: lines(:), lines_we(:), lines_rank(:)
    character(*),      intent(in)        :: file
    character( len(lines) ), allocatable :: lines2(:) ! No empty lines
    character(1), allocatable            :: ranks_c(:, :)
    character(9), allocatable            :: ranks_run(:)
    integer                              :: bike(NRCRS)
    integer, allocatable                 :: no_rank_run(:, :) ! Value: # bike
    integer, allocatable                 :: rank_no_run(:, :) ! Value: rank
    integer                              :: i, j, k, u, nr, nr2
    integer                              :: rank, run, nruns

    read (lines_we(1), '(i4)') this%meters
    nruns = (this%meters - METERS_ADD) / METERS_RUN 

    allocate ( ranks_c(NRCRS, nruns), no_rank_run(NRCRS, nruns), rank_no_run(NRCRS, nruns) )
    allocate ( ranks_run(nruns) )

    print '(a$)', 'Opening a csv file: '//trim(file)//' ... '

    call execute_command_line ( 'mkdir -p '//trim( get_dirname(file) ) )

    open (newunit = u, file = file, status = 'replace')

    do i = 1, nruns

      write ( ranks_run(i), '(a8,  i1)' ) 'rank_run',  i

    end do

    write (u, FMT_CSV_STR)&
      "place", "date", "rd", "meters_distance", "weather", "tp", "hm", "tp_road", "road", &
      "accident", "bike", "kanji_name_racer", &
      "name_racer", "name_bike", "meters_handycup", &
      "mins_trial", "mins_race", "mins_start", "violation", &
      ranks_run

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

    !
    ! Rank of run 
    !
    nr = size(lines_rank)

    k    = 1
    rank = 1
    run  = 1

    do i = 1, nr

      if ( is_empty( lines_rank(i) )) cycle

!      print '(a$)', 'lines_rank: '//trim( lines_rank(i) )

      read (lines_rank(i), '(i1)') no_rank_run(rank, NRUNS - run + 1) 

!      print '(a, i1)', '; no_rank_run: ', no_rank_run(rank, NRUNS - run + 1)

      rank = rank + 1

      if (rank > NRCRS) then

        rank = 1

        run  = run + 1

      end if

    end do

    do rank = 1, NRCRS

      do run = 1, NRUNS

        rank_no_run( no_rank_run(rank, run), run ) = rank

      end do

    end do

#ifdef debug
    print '(a)',  '-----------------------------------------------------------'
    print '(a, *(i1, :, "    "))', '           Run: ', [(i, i = 1, NRUNS)]
    print '(a)',  '-----------------------------------------------------------'

    do i = 1, NRCRS 

      print '(a, i1, a, *(i1, :, " -> "))', 'Bike: ', i, ', Ranks: ', rank_no_run(i, :)

    end do
#endif

    !
    ! Check if accidents occured.
    ! If so, skip writing the CSV file.
    !
    do i = 2, 88, 11

      if ( index (lines2(i), '-') > 0 ) cycle 

      print *, ''
      print *, '***********************************************************'
      print *, trim( lines2(i) )//' occured and this race has been skipped.'
      print *, '***********************************************************'

      return

    end do

    !
    ! Get motercycle(bike) number
    !
    k = 1

    print *, ''

    do i = 3, 88, 11

      if (.not. is_numeric(lines2(i)) ) stop trim( lines2(i) )//' is NaN'

      read ( lines2(i), * ) bike(k)

!      print *, 'bike: ',  bike(k)

      k = k + 1

    end do

    ! Convert integer to character for CSV writing
    do run = 1, NRUNS

      do i = 1, NRCRS

!        print *, 'Bike: ', bike(i), 'Run: ', run, 'Rank: ', rank_no_run(bike(i), run)

        write ( ranks_c(i, run), '(i1)' ) rank_no_run( bike(i), run )

      end do

    end do

    do i = 1, NRCRS

      print '(a, i1, a, *(a, :, " -> "))', 'Racer: ', i, ', Rank: ', ranks_c(i, :)

    end do

    !
    ! Write race results and conditions
    !
    k = 1

    do i = 1, nr2, 11

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
        [(lines2(i + j), j = 1, 10)], &  ! accident, bike, kanji_name_racer, name_racer, name_bike, meters_handycup, mins_trial, mins_race, mins_start, violation
        ranks_c(k, :)

      k = k + 1

    end do

    close (u)

    print '(a)', 'done'

  end subroutine

end module
