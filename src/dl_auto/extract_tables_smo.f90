! Last Updated: 2019-06-24 09:51:45
!=================================================================================
! Submodule for extracting tables from downloaded HTML file
!               constructing race such as circle meters, number of laps and racers
!
! Created by : Hisashi Takeda, Ph.D., 2019-06-24
!=================================================================================

submodule (dl_auto_mo) extract_tables_smo

  implicit none

contains

  module subroutine extract_tables (lines, lines_we, lines_rank, is_race_ok, file)

    character(*), allocatable, intent(inout) :: lines(:), lines_we(:), lines_rank(:)
    character( len(lines) ), allocatable     :: lines_(:)
    logical,      intent(inout)              :: is_race_ok
    character(*), intent(in)                 :: file
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
    ! Check if race is valid
    !
    do i = 1, nr

      read (u, '(a)') lines_(i)

      if ( index(lines_(i), '本日の開催情報はありません') > 0 .or. &
           index(lines_(i), 'ページが見つかりません') > 0 ) then

        print *, 'skipped'

        is_race_ok = .false.

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
    ! Transaction for rank of lap 
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

  module subroutine construct_race (this, lines_we, lines)

    class(race_ty), intent(inout) :: this
    character(*),   intent(in)    :: lines_we(:), lines(:)
    character(len(lines))         :: line
    integer                       :: i

    !
    ! Get circle meters and calculate number of laps
    !
    read (lines_we(1), '(i4)') this%meters

    this%nlaps = (this%meters - METERS_ADD) / METERS_LAP + 1 ! NB: +1 

#ifdef debug
    print '(a, i2)', 'Number of laps: ', this%nlaps, ' as the last 100m is counted one lap.'
#endif

    !
    ! Count number of racers and construct racer object (rcrs)
    !
    associate ( n => this%nrcrs )

    n = 0

    do i = 1, size(lines)

      line = trim( lines(i) )

      if ( index (line, 'td_white_center' ) > 0 ) n = n + 1
      if ( index (line, 'td_black_center' ) > 0 ) n = n + 1
      if ( index (line, 'td_blue_center'  ) > 0 ) n = n + 1
      if ( index (line, 'td_orange_center') > 0 ) n = n + 1
      if ( index (line, 'td_green_center' ) > 0 ) n = n + 1
      if ( index (line, 'td_yellow_center') > 0 ) n = n + 1
      if ( index (line, 'td_pink_center'  ) > 0 ) n = n + 1
      if ( index (line, 'td_red_center'   ) > 0 ) n = n + 1

    end do

#ifdef debug
    print '(a, i0$)', 'Number of racers: ', n
#endif

    if ( allocated (this%rcrs) ) deallocate (this%rcrs)

    allocate ( this%rcrs(n) )

    end associate

  end subroutine

end submodule
