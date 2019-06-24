! Last Updated: 2019-06-24 09:51:45
!===========================================================
! Submodule for seting and getting URLs
!
! Created by : Hisashi Takeda, Ph.D., 2019-06-24
!===========================================================

submodule (dl_auto_mo) set_get_url_smo

  implicit none

contains

  module subroutine set_url (this)

    class(webpage_ty), intent(inout) :: this

    write (this%rd_c, '(i0)') this%rd

    write (this%fn_html, '(a)') trim(this%t%strftime('%Y-%m-%d'))//'_'//trim(this%rd_c)

    this%url = trim(this%prefix)//trim(this%place)//'/'//trim(this%fn_html)

#ifdef debug
    print '(a)', 'URL: '//trim(this%url)
#endif
    
    this%fn_csv = this%fn_html

  end subroutine

  module subroutine get_html (this)

    class(webpage_ty), intent(inout) :: this
    character(255)                   :: cmd, outfile
    logical                          :: exist

!#ifdef debug
    print '(a$)', 'Downloading htm: '//trim(this%url)//' ... '
!#endif

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

  end subroutine

end submodule
