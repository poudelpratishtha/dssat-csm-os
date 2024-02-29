module dssat_csm_cli_mod

  implicit none

  character(len=:), allocatable :: cmd

  public :: is_cmd_arg_present, get_cmd_arg

contains

  ! Subroutine to initialize full command as called
  subroutine init_cmd()

    implicit none

    integer cmd_len, cmd_status

    allocate(character(len=100)::cmd)

    call get_command(&
         command = cmd, &
         length = cmd_len, &
         status = cmd_status)

    ! Check if cmd was long enough to get the full command.
    ! If not, reallocate cmd and call get_command() again
    ! to get the full command.
    if(cmd_status < 0)then
       deallocate(cmd)
       allocate(character(len=cmd_len)::cmd)
       call get_command(&
            command = cmd, &
            length = cmd_len, &
            status = cmd_status)
    end if

  end subroutine init_cmd

  ! Subroutine to check whether command line call has been
  ! initialized. If not, then initialize
  subroutine check_cmd_init()

    implicit none

    if(.not.allocated(cmd)) call init_cmd()

  end subroutine check_cmd_init

  ! Function to find the start position of a given command argument
  function cmd_arg_start(arg_name) result(start_pos)

    implicit none

    integer start_pos

    character(len=*) :: arg_name

    call check_cmd_init()

    start_pos = index(cmd, "--"//trim(adjustl(arg_name))//"=")

    if(start_pos /= 0) start_pos = start_pos + index(cmd(start_pos:), "=")

  end function cmd_arg_start

  ! Function to find the end position of a given command argument
  function cmd_arg_end(start_pos) result(end_pos)

    implicit none

    integer start_pos, end_pos

    call check_cmd_init()    

    end_pos = index(cmd(start_pos:), " ")

    if(end_pos == 0)then
       end_pos = len(cmd)
    else
       end_pos = start_pos + end_pos - 1
    end if

  end function cmd_arg_end

  ! Function to determine if a command argument is present in command
  function is_cmd_arg_present(arg_name) result(is_present)

    implicit none

    logical :: is_present

    character(len=*) :: arg_name

    is_present = cmd_arg_start(arg_name) /= 0

  end function is_cmd_arg_present

  ! Subroutine to retrieve command line argument from command
  ! with fixed length arg_val
  subroutine get_cmd_arg_fixed(arg_name, arg_val)

    implicit none

    character(len=*) :: arg_name, arg_val

    integer :: pos1, pos2

    call check_cmd_init()

    if(is_cmd_arg_present(arg_name))then

      pos1 = cmd_arg_start(arg_name)

      pos2 = cmd_arg_end(pos1) 

      arg_val = cmd(pos1:pos2)

   else

      arg_val = " "

    end if

  end subroutine get_cmd_arg_fixed


  ! Subroutine to retrieve command line argument from command
  ! with deferred length character (arg_val) or allocatable
  ! length character (arg_alloc)
  subroutine get_cmd_arg(arg_name, arg_val, arg_alloc)

    implicit none

    character(len=*) :: arg_name
    character(len=*), optional :: arg_val
    character(len=:), allocatable, optional :: arg_alloc

    integer :: pos1, pos2

    call check_cmd_init()

    if(is_cmd_arg_present(arg_name))then

      pos1 = cmd_arg_start(arg_name)

      pos2 = cmd_arg_end(pos1)

      if(present(arg_val))then

         arg_val = cmd(pos1:pos2)

      end if

      if(present(arg_alloc)) then

         if(allocated(arg_alloc)) deallocate(arg_alloc)

         allocate(character(len=pos2 - pos1 + 1)::arg_alloc)

         arg_alloc = cmd(pos1:pos2)

      end if

      arg_val = cmd(pos1:pos2)

   else

      if(present(arg_val))then

         arg_val = " "

      end if

      if(present(arg_alloc)) then

         if(allocated(arg_alloc)) deallocate(arg_alloc)

         allocate(character(len=1)::arg_alloc)

         arg_alloc = " "

      end if

    end if    

  end subroutine get_cmd_arg

end module dssat_csm_cli_mod
