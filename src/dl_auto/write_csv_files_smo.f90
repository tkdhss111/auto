! Last Updated: 2019-06-24 09:51:45
!================================================
! Submodule for writing CSV file from clean data 
!
! Created by : Hisashi Takeda, Ph.D., 2019-06-24
!================================================

submodule (dl_auto_mo) write_csv_files_smo

  implicit none

contains

  module subroutine write_csv_files (this, lines, lines_cd, lines_rank, file)

    class(webpage_ty), intent(inout)     :: this
    character(*),      intent(in)        :: lines(:), lines_cd(:), lines_rank(:)
    character(*),      intent(in)        :: file
    character(1), allocatable            :: ranks_c(:, :)
    character(10), allocatable           :: ranks_lap(:)
    integer, allocatable                 :: bike(:)
    integer, allocatable                 :: bike_rank_lap(:, :) ! Value: # bike
    integer, allocatable                 :: rank_bike_lap(:, :) ! Value: rank
    integer                              :: i, j, k, u, nr
    integer                              :: rank, lap

    associate ( nrcrs => this%nrcrs, nlaps => this%nlaps )

    allocate ( ranks_c(nrcrs, nlaps), bike_rank_lap(nrcrs, nlaps), rank_bike_lap(nrcrs, nlaps) )
    allocate ( bike(nrcrs) )
    allocate ( ranks_lap(nlaps) )

    print '(a$)', 'Writing a csv file: '//trim(file)//' ... '

    ! Check if accidents occured. If so, skip writing the CSV file.
    do i = 2, nrcrs * 11, 11

      if ( index (lines(i), '-') > 0 ) cycle 

      print *, ''
      print *, '***********************************************************'
      print *, trim( lines(i) )//' occured and this race has been skipped.'
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

      if ( is_empty( lines_rank(i) ) .or. .not. is_numeric( lines_rank(i) ) .or.&
           lines_rank(i) == 'NA' ) cycle

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
    print *, ''
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

      if (.not. is_numeric(lines(i)) ) stop trim( lines(i) )//' is NaN'

      read ( lines(i), * ) bike(k)

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
        trim( lines_cd(1) ),        & ! Distance in meters 
        trim( lines_cd(2) ),        & ! Weather condition
        trim( lines_cd(3) ),        & ! Temperature
        trim( lines_cd(4) ),        & ! Humidity
        trim( lines_cd(5) ),        & ! Temperature of road
        trim( lines_cd(6) ),        & ! Road condition
        [(lines(i + j), j = 2, 10)], &  ! bike, kanji_name_racer, name_racer, name_bike, meters_handycup, mins_trial, mins_race, mins_start, violation
        ranks_c(k, :)

      k = k + 1

    end do

    close (u)

    end associate

    print '(a)', 'done'

  end subroutine

end submodule
