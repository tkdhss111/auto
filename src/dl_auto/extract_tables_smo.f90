! Last Updated: 2019-06-24 09:51:45
!=================================================================================
! Submodule for extracting tables from downloaded HTML file
!               constructing race such as circle meters, number of laps and racers
!
! Depends: clean_lines_smo.f90 
!
! Created by : Hisashi Takeda, Ph.D., 2019-06-24
!=================================================================================

submodule (dl_auto_mo) extract_tables_smo

  implicit none

contains

  module subroutine extract_tables &
      (this, lines, lines_cd, lines_rank, lines_pay, is_race_ok, file)

    class(webpage_ty), intent(inout)           :: this
    character(*),   intent(inout), allocatable :: lines(:), lines_cd(:)
    character(*),   intent(inout), allocatable :: lines_rank(:), lines_pay(:)
    logical,        intent(inout)              :: is_race_ok
    character(*),   intent(in)                 :: file

    character( len(lines) ), allocatable :: lines_(:)
    logical                              :: is_table
    integer                              :: i_fr, i_to
    integer                              :: nr, nr_cd, nr_result, nr_pay, nr_rank
    integer                              :: i, k, u

    print '(a$)', 'Opening a html file: '//trim(file)//' ... '

    is_table = .false.

    open (newunit = u, file = file, status = 'old')

    nr = count_rows (u)

    allocate ( lines_(nr) )
    
    is_table = .false.

    !
    ! Check if race is valid
    !
    do i = 1, nr

      read (u, '(a)') lines_(i)

      if ( index(lines_(i), '本日の開催情報はありません') > 0 .or. &
           index(lines_(i), 'ページが見つかりません') > 0 ) then

        print *, '/kipped'

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
      if ( index(lines_(i), 'ｍ</td>') > 0 .and. .not. is_table ) then

        i_fr = i

        is_table = .true.

      end if

      ! End of the table
      if ( index(lines_(i), '</table>') > 0 .and. is_table ) then

        i_to = i - 3

        exit

      end if

    end do

    !print *, ''
    !print '(a$)', 'Cleaning data of race conditions ... '

    do i = i_fr, i_to

      call clean_line_race_conditions ( lines_(i) )

    !  print *, 'lines_cd: ', trim( lines_cd(i) )

    end do

    lines_cd = lines_(i_fr:i_to)

    !print '(a)', 'done'

#ifdef debug
    print *, ''

    nr_cd = i_to - i_fr + 1

    do i = 1, nr_cd

      print '(a, i3, a, i3, a)', 'Line: ', i, '/', nr_cd, '; '//trim( lines_cd(i) )

    end do
#endif

    !
    ! Transaction for race results
    !
    rewind (u)

    is_table = .false.

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

    !print *, ''
    !print '(a$)', 'Constructing race ... ' 

    call this%construct_race ( lines_cd, lines_ )

    !print '(a)', 'done'

    !print *, ''
    !print '(a$)', 'Cleaning data of race results ... ' 

    do i = i_fr, i_to

      call clean_line_race_result ( lines_(i) )

    end do

    !print '(a)', 'done'

    nr_result = 0

    do i = i_fr, i_to

      if ( is_empty( lines_(i) )) cycle

      nr_result = nr_result + 1

    end do

    allocate ( lines(nr_result) )

    k = 1

    do i = i_fr, i_to

      if ( is_empty( lines_(i) )) cycle

      lines(k) = lines_(i)

      k = k + 1

    end do

#ifdef debug
    print *, ''
    do i = 1, nr_result
      print '(a, i3, a, i3, a)', 'Line(result): ', i, '/', nr_result, '; '//trim( lines(i) )
    end do
#endif

    !
    ! Transaction for rank of lap 
    !
    rewind (u)

    is_table = .false.

    lines_ = ''

    do i = 1, nr

      read (u, '(a)') lines_(i)

      ! Begining of table
      if ( index(lines_(i), 'ゴール線通過') > 0 .and. .not. is_table ) then

        i_fr = i

        is_table = .true.

      end if

      ! End of the table
      if ( index(lines_(i), '</table>') > 0 .and. is_table ) then

        i_to = i - 3

        exit

      end if

    end do

    !print *, ''
    !print '(a$)', 'Cleaning data of rank of lap ... ' 

    do i = i_fr, i_to

      call clean_line_rank_lap ( lines_(i) )

    end do

    !print '(a)', 'done'

    lines_rank = lines_(i_fr:i_to)

#ifdef debug
    nr_rank = i_to - i_fr + 1
    print *, ''
    do i = 1, nr_rank
      print '(a, i3, a, i3, a)', 'Line(rank): ', i, '/', nr_rank, '; '//trim( lines_rank(i) )
    end do
#endif

    !
    ! Transaction for pay 
    !
    rewind (u)

    is_table = .false.

    lines_ = ''

    do i = 1, nr

      read (u, '(a)') lines_(i)

      ! Begining of table
      if ( index(lines_(i), '2連単') > 0 .and. .not. is_table ) then

        i_fr = i

        is_table = .true.

      end if

      ! End of the table
      if ( index(lines_(i), '</table>') > 0 .and. is_table ) then

        i_to = i - 9

        exit

      end if

    end do

    !print *, ''
    !print '(a$)', 'Cleaning data of payment ... ' 

    do i = i_fr, i_to

      call clean_line_pay ( lines_(i) )

    end do

    !print '(a)', 'done'

    lines_pay = lines_(i_fr:i_to)

#ifdef debug
    nr_pay = i_to - i_fr + 1
    print *, ''
    do i = 1, nr_pay
      print '(a, i3, a, i3, a)', 'Line(pay): ', i, '/', nr_pay, '; '//trim( lines_pay(i) )
    end do
#endif

    !-------------------------------------------------------------

    close (u)

    print '(a)', 'done'

  end subroutine

  module subroutine construct_race (this, lines_cd, lines)

    class(webpage_ty), intent(inout) :: this
    character(*),   intent(in)       :: lines_cd(:), lines(:)
    character(len(lines))            :: line
    integer                          :: i

#ifdef debug
    print '(a$)', 'Constructing racers ... ' 
#endif

    !
    ! Get circle meters and calculate number of laps
    !
    read (lines_cd(1), '(i4)') this%meters

    this%nlaps = (this%meters - METERS_ADD) / METERS_LAP + 1 ! NB: +1 

#ifdef debug
    print '(a, i2$)', 'Number of laps: ', this%nlaps ! N.B. the last 100m is counted one lap
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
    print '(a, i2, a$)', ', Number of racers: ', n, ' ... '
#endif

    if ( allocated (this%rcrs) ) deallocate (this%rcrs)

    allocate ( this%rcrs(n) )

    end associate

#ifdef debug
    print '(a)', 'done'
#endif

  end subroutine

end submodule
