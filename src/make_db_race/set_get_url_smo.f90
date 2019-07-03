! Last Updated: 2019-07-03 22:02:31
!===========================================================
! Submodule for seting and getting URLs
!
! Created by : Hisashi Takeda, Ph.D., 2019-06-24
!===========================================================

submodule (make_db_race_mo) set_get_url_smo

  implicit none

contains

  module subroutine set_url (this)

    class(webpage_ty), intent(inout) :: this

    write (this%rd_c, '(i0)') this%rd

    write (this%fn_html, '(a)') trim(this%t%strftime('%Y-%m-%d'))//'_'//trim(this%rd_c)

    this%url = trim(this%prefix)//trim(this%place)//'/'//trim(this%fn_html)

!#ifdef debug
!    print '(a)', 'URL: '//trim(this%url)
!#endif
    
    this%fn_csv = this%fn_html

    this%htmlfile = trim(this%dir_html)//trim(this%place)//'/'//trim(this%fn_html)//'.html'
    this%csvfile  = trim(this%dir_csv)//trim(this%place)//'/'//trim(this%fn_csv)//'.csv'

  end subroutine

  module subroutine get_html (this)

    class(webpage_ty), intent(inout) :: this
    character(255)                   :: cmd, cmdmsg = ''
    logical                          :: exist
    integer                          :: exitstat

#ifdef debug
    print '(a$)', 'Downloading html ... '
#endif

    inquire (file = this%htmlfile, exist = exist)

    if (exist) then

      print '(a)', 'Skipped since the HTML file already exists.'

      return

    end if

    cmd = trim('curl --max-time 2 --retry 2 "'//trim(this%url)//'" -o "'//trim(this%htmlfile)//'"')

!#ifdef debug
!    print *, trim(cmd)
!#endif

    call execute_command_line ( 'mkdir -p '//trim(this%dir_html)//trim(this%place)//'/' )

    call execute_command_line(cmd, exitstat = exitstat, cmdmsg = cmdmsg)

    if (exitstat == 7) stop 'Failed to connect to host' 

    call sleep( int(10 * rand()) )

#ifdef debug
    print '(a)', 'done'
#endif

  end subroutine

end submodule
