module stdlib_error
    !! Provides support for catching and handling errors
    !! ([Specification](../page/specs/stdlib_error.html))
use, intrinsic :: iso_fortran_env, only: stderr => error_unit
use stdlib_optval, only: optval
implicit none
private

public :: check, error_stop

contains

subroutine check(condition, msg, code, warn)
    !! version: experimental
    !!
    !! Checks the value of a logical condition
    !! ([Specification](../page/specs/stdlib_error.html#description))
    !!
    !!##### Behavior
    !!
    !! If `condition == .false.` and:
    !!
    !!   * No other arguments are provided, it stops the program with the default
    !!     message and exit code `1`;
    !!   * `msg` is provided, it prints the value of `msg`;
    !!   * `code` is provided, it stops the program with the given exit code;
    !!   * `warn` is provided and `.true.`, it doesn't stop the program and prints
    !!     the message.
    !!
    !!##### Examples
    !!
    !!* If `a /= 5`, stops the program with exit code `1`
    !!  and prints `Check failed.`
    !!``` fortran
    !!  call check(a == 5)
    !!```
    !!
    !!* As above, but prints `a == 5 failed`.
    !!``` fortran
    !!  call check(a == 5, msg='a == 5 failed.')
    !!```
    !!
    !!* As above, but doesn't stop the program.
    !!``` fortran
    !!  call check(a == 5, msg='a == 5 failed.', warn=.true.)
    !!```
    !!
    !!* As example #2, but stops the program with exit code `77`
    !!``` fortran
    !!  call check(a == 5, msg='a == 5 failed.', code=77)
    !!```

    !
    ! Arguments
    ! ---------

    logical, intent(in) :: condition
    character(*), intent(in), optional :: msg
    integer, intent(in), optional :: code
    logical, intent(in), optional :: warn
    character(*), parameter :: msg_default = 'Check failed.'

    if (.not. condition) then
        if (optval(warn, .false.)) then
            write(stderr,*) optval(msg, msg_default)
        else
            call error_stop(optval(msg, msg_default), optval(code, 1))
        end if
    end if

end subroutine check

subroutine error_stop(msg, code)
! Aborts the program with nonzero exit code
! this is a fallback for Fortran 2008 error stop (e.g. Intel 19.1/2020 compiler)
!
! The "stop <character>" statement generally has return code 0.
! To allow non-zero return code termination with character message,
! error_stop() uses the statement "error stop", which by default
! has exit code 1 and prints the message to stderr.
! An optional integer return code "code" may be specified.
!
! Example
! -------
!
! call error_stop("Invalid argument")
character(*), intent(in) :: msg
integer, intent(in), optional :: code

write(stderr,*) msg

if(present(code)) then
  select case (code)
  case (1)
    error stop 1
  case (2)
    error stop 2
  case (77)
    error stop 77
  case default
    write(stderr,*) 'ERROR: code ',code,' was specified.'
    error stop
  end select
else
  error stop
endif
end subroutine

end module stdlib_error
