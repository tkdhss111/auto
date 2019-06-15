! Last Updated: 2019-06-15 16:49:44.

!===========================================================
! Auto Race Data Fetcher
!
! Created by : Hisashi Takeda, Ph.D., 2018-05-05
!===========================================================

module fetch_auto_mo

  use my_tools_mo
  
  implicit none

  type racer_ty
    character(20) :: name  = 'NA'
    character(20) :: place = 'NA'
    integer       :: year_birth  = iNA
    integer       :: rank_goal   = iNA
    integer       :: handycup    = iNA ! Meters
    real(8)       :: time_trial  = iNA ! Minutes
    real(8)       :: time_goal   = iNA ! Minutes
    real(8)       :: time_start  = iNA ! Minutes
    logical       :: is_trouble  = .false.
    logical       :: is_abnormal = .false.
  end type

  type race_ty
    character(20) :: title   = 'NA'
    character(20) :: place   = 'NA'
    character(20) :: weather = 'NA'
    character(20) :: road    = 'NA' ! Race road condition
    integer       :: year    = iNA
    integer       :: mon     = iNA
    integer       :: day     = iNA
    integer       :: rd      = iNA ! Race round
    integer       :: dist    = iNA ! Distance of the race
    real(8)       :: tp      = NA
    real(8)       :: tp_road = NA
    real(8)       :: hm      = NA

  end type

  type, extends(race_ty) :: webpage_ty
    character(255) :: prefix        = 'http://autorace.jp/netstadium/RaceResult/'
    character(255) :: url           = 'NA'
    character(255) :: dir_save_html = '.'
    character(255) :: dir_save_csv  = '.'
    character(255) :: fn_save_html  = 'html'
    character(255) :: fn_save_csv   = 'csv'
  contains
    procedure :: set_url
    procedure :: get_html
    procedure :: get_csv_from_html
  end type

contains

  subroutine set_url (this)
    class(webpage_ty), intent(inout) :: this
    character(4) :: year_c
    character(2) :: mon_c
    character(2) :: day_c
    character(2) :: rd_c

    write(year_c, '("year=", i4.0)') this%year
    write(mon_c,  '("mon=",  i2.2)') this%mon
    write(day_c,  '("day=",  i2.2)') this%day
    write(rd_c,   '("rd=",   i0)'  ) this%rd

    this%url = trim(this%prefix)//trim(this%place)//'/'//trim(year_c)//'-'//trim(mon_c)//'-'//trim(day_c)//'_'//trim(this%rd_c)
    print '(a)', 'URL: ', trim(this%url)

    write(this%filename_save_html, '(i4.0, "-", i2.2, "-", i2.2, "_", i0, "_", a)') this%year, this%mon, this%day, this%rd, trim(this%place)
  end subroutine

  subroutine get_html (this)
    class(webpage_ty), intent(inout) :: this
    print '(a)', 'Downloading htm: ', trim(this%url), ' to ', trim(this%dir_save_html)
    print *, trim('curl "'//trim(this%url)//'" -o '//trim(this%dir_save_html)//'/"'//trim(this%filename_save_html)//'".htm')
    call execute_command_line(trim('curl "'//trim(this%url)//'" -o '//trim(this%dir_save_html)//'/"'//trim(this%filename_save_html)//'".htm'))
  end subroutine

  subroutine get_csv_from_html (this)
    use Posix_mo
    class(webpage_ty), intent(inout) :: this
    type(file_ty)   :: f_htmll, f_csv
    character(1000) :: line = '', line2 = ''
    character(19)   :: date_time(1)
    type(posix_ty)  :: px
    logical         :: is_table, is_in_blacket, is_j_block_nolocked(5000)
    integer         :: i, j, k, nrow, j_we_fr = 0, j_we_to = 0

    this%savename_csv = this%savename
    print '(a)', 'Extracting csv data from the downloaded html: ', trim(this%fn_save_html), &
                 ' and save them as csv: ', trim(this%dir_save_csv)//'/'//trim(this%fn_save_csv)
    call f_html%Construct_File (path_op=trim(this%dir_save_html)//'\'//trim(this%fn_save_html)//'.html')
    call f_csv %Construct_File (path_op=trim(this%dir_save_csv )//'\'//trim(this%fn_save_csv )//'.csv' )

    print '(2a, a$)', 'Opening a html file: ', trim(f_html%path), '...'
    open(newunit=f_html%unit, file=trim(f_html%path), status='old', iostat=f_html%stat, iomsg=f_html%msg)
    if (f_html%stat /= 0) then
      print *, ''
      print *, '************************************************************'
      print *, 'Error occured upon opening the html file: ', trim(f_html%msg)
      print *, '************************************************************'
      return
    else
      print '(a)', 'done'
    end if

    print '(2a, a$)', 'Opening a csv file: ', trim(f_csv%path), '...'
    open(newunit=f_csv%unit, file=trim(f_csv%path), status='replace', iostat=f_csv%stat, iomsg=f_csv%msg)
    if (f_csv%stat /= 0) then
      print *, 'Error occured upon opening the html file: ', trim(f_csv%msg)
      stop
    else
      write(f_csv%unit, FMT_CSV_STR) "race_title", "year", "mon", "day", "rd", "place", "name", "handycup"
      print '(a)', 'done'
    end if

    date_time = Get_Date_Time_From_Values (this%year, this%mon, this%day, 1)
    call px%Construct_Posix_From_Time_Stamp (date_time)

    is_table = .false.
    is_in_blacket = .false.
    nrow = 0

    do while (nrow < 1000)
      line  = ''
      line2 = ''
      is_j_block_nolocked = .false.
      j_we_fr = 0
      j_we_to = 0

      read(f_html%unit, '(a)', end=10) line

      if (index(line, "<table id='tablefix1' class='data2_s'>") > 0) then
        is_table = .true.
        do i = 1, 2
          read(f_html%unit, '()', end=10) ! Skip headers
        end do
        cycle
      end if

      if (index(line, '</table>') > 0 .and. is_table) then
        is_table = .false.
        exit
      end if

      if (is_table) then
        line = line(index(line, '<td'):index(line, '</td>', back=.true.)-1) ! Omit <tr> tags
        j_we_fr = index(line, 'alt') + 5
        if (j_we_fr > 5) then
          j_we_to = j_we_fr + index(line(j_we_fr:), '"') - 2
          is_j_block_nolocked(j_we_fr:j_we_to) = .true.
          print *, 'Word: ', line(j_we_fr:j_we_to), ' has been blocked.'
        end if
        nrow = nrow + 1
      end if

      ! letter by letter process: delete unwanted letters
      if (nrow > 0) then
        do j = 1, len_trim(line)

          if (line(j:j) == '<') then
            is_in_blacket = .true.
          end if

          if (line(j:j) == '>') then
            is_in_blacket = .false.
            line(j:j) = ''
          end if

          if (line(j:j+2) == '/td') then
            line(j-1:j-1) = ','
          end if

          if (is_in_blacket .and. line(j:j) /= ',' .and. .not. is_j_block_nolocked(j)) then
            line(j:j) = ''
          end if

        end do

        ! Delete white space in the line
        k = 1
        do i = 1, len(line)
          if (line(i:i) == ' ') cycle
          line2(k:k) = line(i:i)
          k = k + 1
        end do

        ! Write clean data
        print '(a)', trim(line2)
        write(f_csv%unit, '(a, a, i5, a, f7.4, a, f8.4, a, i1, a, i1, a, a19, a, i0, a, i0, a, i0, a, a)') &
          trim(this%name), ',', this%block_no, ',', this%lat, ',', this%lon, ',', this%has_ra_mj, ',', this%is_island, ',', px%date_time, ',', this%year, ',', this%mon, ',', this%day, ',', trim(line2)
        px = px + 60 * 60
      end if
    end do
    10 continue
    close(f_html%unit)
    close(f_csv%unit)
  end subroutine

end module

program main
  use fetch_weather_from_jma_mo
  use my_tools_mo
  implicit none
  integer          :: n_files = 0, n = 1
  character(10)    :: c, d
  character(10000) :: line
  character(255)   :: str(100)
  type(file_ty)    :: f(2)
  type(cmd_ty)     :: cmd
  type(obstry_ty)  :: obstry
  integer          :: j_name, j_prec_no, j_block_no, j_lat, j_lon, j_has_ra_mj, j_is_island

  cmd%exe       = 'fetch_weather_from_jma'
  cmd%version   = '1.0'
  cmd%usage(1)  = 'Usage: '//trim(cmd%exe)//' [OPTIONS]'
  cmd%usage(2)  = ''
  cmd%usage(3)  = 'Example: '//trim(cmd%exe)//'.exe -f info_obstry.csv -d 2016-01-01 -s1 html -s2 csv -n 1'
  cmd%usage(4)  = 'NB: Max number of types of condition is 100'
  cmd%usage(5)  = 'Program options:'
  cmd%usage(6)  = ''
  cmd%usage(7)  = '  -f,  --filename    followed by path of a file regarding observatory information'
  cmd%usage(8)  = '  -d,  --date        followed by date in format as yyyy-mm-dd'
  cmd%usage(8)  = '  -s1, --savedir     followed by html file directory for download'
  cmd%usage(8)  = '  -s2, --savedir_csv followed by directory of csv data obtained from html data'
  cmd%usage(8)  = '  -n                 1 for download html and 0 for skipping downlond (only to convert html to csv)'
  cmd%usage(9)  = '  -v,  --version print version information and exit'
  cmd%usage(10) = '  -h,  --help    print usage information and exit'

  call cmd%cmdline(f, n_files, n, c, d, obstry%savedir, obstry%savedir_csv)
  print *, "info_obstry: ", trim(f(1)%path)
  print *, "Date: ",        trim(d)

  call f(1)%Open_File (status='old', has_rownames=.false., is_for_writing=.false.)
  read(f(1)%unit, '(a)') line
  call Get_Strings_From_Line (line, str)
  j_name      = f(1)%Get_Target_Column_Index ('name')
  j_prec_no   = f(1)%Get_Target_Column_Index ('prec_no')
  j_block_no  = f(1)%Get_Target_Column_Index ('block_no')
  j_lat       = f(1)%Get_Target_Column_Index ('lat')
  j_lon       = f(1)%Get_Target_Column_Index ('lon')
  j_has_ra_mj = f(1)%Get_Target_Column_Index ('has_ra_mj')
  j_is_island = f(1)%Get_Target_Column_Index ('is_island')

  do
    read(f(1)%unit, '(a)', end=10) line
    call Get_Strings_From_Line (line, str)

    ! Skip records that contain NAs
    if (trim(str(j_prec_no)) == 'NA' .or. trim(str(j_block_no)) == 'NA') cycle

    obstry%name = trim(str(j_name))
    read(str(j_prec_no  ), *) obstry%prec_no
    read(str(j_block_no ), *) obstry%block_no
    read(str(j_lat      ), *) obstry%lat
    read(str(j_lon      ), *) obstry%lon
    read(str(j_has_ra_mj), *) obstry%has_ra_mj
    read(str(j_is_island), *) obstry%is_island
    read(d(1:4),           *) obstry%year
    read(d(6:7),           *) obstry%mon
    read(d(9:10),          *) obstry%day

    call obstry%set_url
    if (n == 1) call obstry%get_html
    call obstry%get_csv_from_html
  end do
  10 close(f(1)%unit)
end program
