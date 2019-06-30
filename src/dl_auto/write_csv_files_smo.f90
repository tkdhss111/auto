! Last Updated: 2019-06-24 09:51:45
!================================================
! Submodule for writing CSV file from clean data 
!
! Created by : Hisashi Takeda, Ph.D., 2019-06-24
!================================================

submodule (dl_auto_mo) write_csv_files_smo

  implicit none

contains

  module subroutine write_csv_files (this, lines, lines_cd, lines_rank, skipped, file)

    class(webpage_ty), intent(inout) :: this
    character(*),      intent(in)    :: lines(:), lines_cd(:), lines_rank(:)
    logical,           intent(inout) :: skipped
    character(*),      intent(in)    :: file

    integer, parameter           :: MAX_NLAPS = 10
    character(2)                 :: nlaps_wo_goal
    character(1),  allocatable   :: ranks_c(:, :)
    character(11)                :: ranks_lap(MAX_NLAPS)
    character(10), allocatable   :: payout_win(:)
    character(10), allocatable   :: payout_place(:)
    integer, allocatable         :: bike(:)
    integer, allocatable         :: bike_rank_lap(:, :) ! Value: # bike
    integer, allocatable         :: rank_bike_lap(:, :) ! Value: rank
    integer                      :: i, k, u, nr
    integer                      :: rank, lap
    character(1000), allocatable :: lines_out(:)
    character(255)               :: key

    associate ( nrcrs => this%nrcrs, nlaps => this%nlaps )

    allocate ( ranks_c(nrcrs, MAX_NLAPS), bike_rank_lap(nrcrs, nlaps), rank_bike_lap(nrcrs, nlaps) )
    allocate ( bike(nrcrs), payout_win(nrcrs), payout_place(nrcrs) )
    allocate ( lines_out(0:nrcrs) )

    payout_win = '0'
    payout_win(1) = trim( payout%win )

    payout_place = '0'
    payout_place(1) = trim( payout%place(1) )
    payout_place(2) = trim( payout%place(2) )
    payout_place(3) = trim( payout%place(3) )

!#ifdef debug
!    print '(a$)', 'Writing a csv file: '//trim(file)//' ... '
!#endif

    ! Check if accidents occured. If so, skip writing the CSV file.
    skipped = .false.

    do i = 2, nrcrs * 11, 11

      if ( index (lines(i), '-') > 0 ) cycle 

      print *, ''
      print *, '***********************************************************'
      print *, trim( lines(i) )//' occured and this race has been skipped.'
      print *, '***********************************************************'

      skipped = .true.

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

      if ( is_empty( lines_rank(i) ) .or. .not. is_numeric( lines_rank(i) ) .or.&
           lines_rank(i) == 'NA' ) cycle

!#ifdef debug
!      print '(a, a, i1, a, i1, a, i2, a, i2)',&
!        'bike: '//trim( lines_rank(i) ), ', rank: ', rank, '/', nrcrs, ', lap: ', lap, '/', nlaps
!#endif

      read (lines_rank(i), *) bike_rank_lap(rank, nlaps - lap + 1) 

!      print '(a, i1)', 'bike_rank_lap: ', bike_rank_lap(rank, nlaps - lap + 1)

      rank = rank + 1

      if (rank > nrcrs) then

        rank = 1

        lap  = lap + 1

      end if

    end do

    skipped = .false.

    if (lap - 1 /= nlaps) then

      print *, ''
      print *, '***********************************************************'
      print *, ' Missing laps, so skipped.'
      print *, '***********************************************************'

      skipped = .true.

      return

    end if

    do rank = 1, nrcrs

      do lap = 1, nlaps

        rank_bike_lap( bike_rank_lap(rank, lap), lap ) = rank

      end do

    end do

#ifdef debug
    print *, ''
    print '(a)', repeat('=', 80)
    print '(a)', '  Lap rankings '
    print '(a)', repeat('-', 80)
    print '(a, *(i2, :, "    "))', '           lap: ', [(i, i = 1, nlaps)]
    print '(a)',  repeat('-', 80)

    do i = 1, nrcrs 

      print '(a, i1, a, *(i2, :, " -> "))', 'Bike : ', i, ', Rank: ', rank_bike_lap(i, :)

    end do

    print '(a)',  repeat('-', 80)
#endif

    !
    ! Get bike number
    !
    k = 1

!    print *, ''

    do i = 3, nrcrs * 11, 11

      if (.not. is_numeric(lines(i)) ) stop trim( lines(i) )//' is NaN'

      read ( lines(i), * ) bike(k)

      !print *, 'bike: ',  bike(k)

      k = k + 1

    end do

    ! Convert integer to character for CSV writing
    ranks_c = ''
    do lap = 1, nlaps

      do i = 1, nrcrs

!        print *, 'Bike: ', bike(i), 'lap: ', lap, 'Rank: ', rank_bike_lap(bike(i), lap)

        write ( ranks_c(i, lap), '(i1)' ) rank_bike_lap( bike(i), lap )

      end do

    end do

#ifdef debug
    do i = 1, nrcrs
      print '(a, i1, a, *(a2, :, " -> "))', 'Racer: ', i, ', Rank: ', ranks_c(i, :nlaps)
    end do

    print '(a)', repeat('=', 80)
#endif

    !
    ! Write race results and conditions
    !
    do i = 1, MAX_NLAPS

      write ( ranks_lap(i), '(a9, i0)' ) 'rank_lap_',  i

    end do

    write (nlaps_wo_goal, '(i0)') nlaps - 1

    !write (u, FMT_CSV_STR)&
    write (lines_out(0), '( *(a, :, ",") )')&
      "key", "place", "date", "round", "distance", "we", "tp", "hm", "tp_road", "road", "nlaps", &
      "rank_goal", "bike", "kanji_name_racer", "name_racer", "name_bike", "handycup", &
      "sec_trial", "sec_race", "sec_start", "violation", "payout_win", "payout_place", &
      ranks_lap

    k = 1

    do i = 1, nrcrs * 11, 11

      write (key, '(a)') this%place(1:2)//'_'//this%t%strftime('%Y%m%d')//'_'//trim(this%rd_c)//'_'//trim(ranks_c(k, nlaps))

      write (lines_out(k), '( *(a, :, ",") )') &
        trim( key                ), & ! Key
        trim( this%place         ), & ! Place
        trim( this%t%dateformat()), & ! Date
        trim( this%rd_c          ), & ! Round
        trim( lines_cd(1)        ), & ! Distance in meters
        trim( lines_cd(2)        ), & ! Weather condition
        trim( lines_cd(3)        ), & ! Temperature
        trim( lines_cd(4)        ), & ! Humidity
        trim( lines_cd(5)        ), & ! Temperature of road
        trim( lines_cd(6)        ), & ! Road condition
        trim( nlaps_wo_goal      ), & ! Number of laps (not including the last 100m lap)
        trim( ranks_c(k, nlaps)  ), & ! Goal ranking
        trim( lines(i + 2)       ), & ! bike
        trim( lines(i + 3)       ), & ! kanji_name_racer
        trim( lines(i + 4)       ), & ! name_racer
        trim( lines(i + 5)       ), & ! name_bike
        trim( lines(i + 6)       ), & ! handycup
        trim( lines(i + 7)       ), & ! sec_trial
        trim( lines(i + 8)       ), & ! sec_race
        trim( lines(i + 9)       ), & ! sec_start
        trim( lines(i + 10)      ), & ! violation
        trim( payout_win(k)      ), & ! Payout for win wager
        trim( payout_place(k)    ), & ! Payout for place wager
        ranks_c(k, :)                 ! Lap rankings

      k = k + 1

    end do

    call execute_command_line ( 'mkdir -p '//trim( get_dirname(file) ) )

    open (newunit = u, file = file, status = 'replace')

    do i = 0, nrcrs

!      print '(a)', trim( lines_out(i) )
      write (u, '(a)') trim( lines_out(i) )

    end do

    close (u)
    
    end associate

#ifdef debug
    print '(a)', 'done'
#endif

  end subroutine

end submodule
