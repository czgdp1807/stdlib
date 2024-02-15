module test_sorting

    use, intrinsic :: iso_fortran_env, only: compiler_version, error_unit
    use stdlib_kinds, only: int32, int64, dp, sp
    use stdlib_sorting
    use stdlib_bitsets, only: bitset_64, bitset_large, &
        assignment(=), operator(>), operator(<)
    use testdrive, only: new_unittest, unittest_type, error_type, check

    implicit none

    integer(int32), parameter :: bitset_size = char_set_size**3

    type(bitset_large) ::                  &
        bitsetl_decrease(0:bitset_size-1), &
        bitsetl_increase(0:bitset_size-1), &
        bitsetl_rand(0:bitset_size-1)
    type(bitset_64) ::                  &
        bitset64_decrease(0:bitset_size-1), &
        bitset64_increase(0:bitset_size-1), &
        bitset64_rand(0:bitset_size-1)

    type(bitset_large)      :: bitsetl_dummy(0:bitset_size-1)
    type(bitset_64)         :: bitset64_dummy(0:bitset_size-1)
    type(bitset_large)      :: bitsetl_work(0:bitset_size/2-1)
    type(bitset_64)         :: bitset64_work(0:bitset_size/2-1)
    type(bitset_large)      :: bitsetl_temp
    type(bitset_64)         :: bitset64_temp

contains

    !> Collect all exported unit tests
    subroutine collect_sorting(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('bitset_large_ord_sorts', test_bitsetl_ord_sorts), &
            new_unittest('bitset_64_ord_sorts', test_bitset64_ord_sorts), &
            new_unittest('bitset_large_sorts', test_bitsetl_sorts), &
            new_unittest('bitset_64_sorts', test_bitset64_sorts), &
            new_unittest('bitset_large_sort_indexes', test_bitsetl_sort_indexes), &
            new_unittest('bitset_64_sort_indexes', test_bitset64_sort_indexes) &
        ]

    end subroutine collect_sorting


    subroutine initialize_tests()

        do i = 0, bitset_size-1
            write(bin32,'(b32.32)') i
            call bitsetl_increase(i)%from_string(bin32)
        end do
        do i=0, bitset_size-1
            bitsetl_decrease(bitset_size-1-i) = bitsetl_increase(i)
        end do

        bitsetl_rand(:) = bitsetl_increase(:)
        do i=0, bitset_size-1
            call random_number( arand )
            index1 = int( floor( arand * bitset_size ), kind=int32 )
            bitsetl_temp = bitsetl_rand(i)
            bitsetl_rand(i) = bitsetl_rand(index1)
            bitsetl_rand(index1) = bitsetl_temp
        end do

        do i = 0, bitset_size-1
            write(bin64,'(b64.64)') i
            call bitset64_increase(i)%from_string(bin64)
        end do
        do i=0, bitset_size-1
            bitset64_decrease(bitset_size-1-i) = bitset64_increase(i)
        end do

        bitset64_rand(:) = bitset64_increase(:)
        do i=0, bitset_size-1
            call random_number( arand )
            index1 = int( floor( arand * bitset_size ), kind=int32 )
            bitset64_temp = bitset64_rand(i)
            bitset64_rand(i) = bitset64_rand(index1)
            bitset64_rand(index1) = bitset64_temp
        end do

        ! Create and intialize file to report the results of the sortings
        open( newunit=lun, file=filename, access='sequential', action='write', &
            form='formatted', status='replace' )
        write( lun, '(a)' ) trim(compiler_version())
        write( lun, * )
        write( lun, '("|     Type     | Elements |    Array Name   |    Method ' // &
            '  |  Time (s) |")' )
        write( lun, '("|--------------|----------|-----------------|-----------' // &
            '--|-----------|")' )

    end subroutine initialize_tests

    subroutine test_bitsetl_ord_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical:: ltest

        call test_bitsetl_ord_sort( bitsetl_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_ord_sort( bitsetl_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_ord_sort( bitsetl_rand, "Bitset Random" , ltest)
        call check(error, ltest)

    end subroutine test_bitsetl_ord_sorts

    subroutine test_bitsetl_ord_sort( a, a_name, ltest )
        type(bitset_large), intent(in) :: a(0:)
        character(*), intent(in)       :: a_name
        logical, intent(out)           :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitsetl_dummy = a
            call system_clock( t0, rate )
            call ord_sort( bitsetl_dummy, bitsetl_work )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitsetl_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("| Bitset_large |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Ord_Sort", tdiff/rate

        !reverse
        bitsetl_dummy = a
        call ord_sort( bitsetl_dummy, bitsetl_work, reverse = .true. )

        call verify_bitsetl_reverse_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse + work ORD_SORT did not sort " // a_name // &
                "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if

        bitsetl_dummy = a
        call ord_sort( bitsetl_dummy, reverse = .true. )

        call verify_bitsetl_reverse_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if

    end subroutine test_bitsetl_ord_sort

    subroutine test_bitset64_ord_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical:: ltest

        call test_bitset64_ord_sort( bitset64_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_ord_sort( bitset64_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_ord_sort( bitset64_rand, "Bitset Random" , ltest)
        call check(error, ltest)

    end subroutine test_bitset64_ord_sorts

    subroutine test_bitset64_ord_sort( a, a_name, ltest )
        type(bitset_64), intent(in) :: a(0:)
        character(*), intent(in)    :: a_name
        logical, intent(out)        :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitset64_dummy = a
            call system_clock( t0, rate )
            call ord_sort( bitset64_dummy, bitset64_work )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitset64_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("|    Bitset_64 |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Ord_Sort", tdiff/rate

        !reverse
        bitset64_dummy = a
        call ord_sort( bitset64_dummy, bitset64_work, reverse = .true. )

        call verify_bitset64_reverse_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse + work ORD_SORT did not sort " // a_name // &
                "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if

        bitset64_dummy = a
        call ord_sort( bitset64_dummy, reverse = .true. )

        call verify_bitset64_reverse_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse ORD_SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if

    end subroutine test_bitset64_ord_sort

    subroutine test_bitsetl_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitsetl_sort( bitsetl_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort( bitsetl_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort( bitsetl_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitsetl_sorts

    subroutine test_bitsetl_sort( a, a_name, ltest )
        type(bitset_large), intent(in) :: a(0:)
        character(*), intent(in)       :: a_name
        logical, intent(out)           :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitsetl_dummy = a
            call system_clock( t0, rate )
            call sort( bitsetl_dummy )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitsetl_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("| Bitset_large |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort", tdiff/rate

        ! reverse
        bitsetl_dummy = a
        call sort( bitsetl_dummy, .true.)
        call verify_bitsetl_reverse_sort(bitsetl_dummy, valid, i)
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
    end subroutine test_bitsetl_sort

    subroutine test_bitset64_sorts(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitset64_sort( bitset64_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort( bitset64_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort( bitset64_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitset64_sorts

    subroutine test_bitset64_sort( a, a_name, ltest )
        type(bitset_64), intent(in) :: a(0:)
        character(*), intent(in)    :: a_name
        logical, intent(out)        :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitset64_dummy = a
            call system_clock( t0, rate )
            call sort( bitset64_dummy )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitset64_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("|    Bitset_64 |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort", tdiff/rate

        ! reverse
        bitset64_dummy = a
        call sort( bitset64_dummy, .true.)
        call verify_bitset64_reverse_sort(bitset64_dummy, valid, i)
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "reverse SORT did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
    end subroutine test_bitset64_sort

    subroutine test_bitsetl_sort_indexes(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitsetl_sort_index( bitsetl_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_index( bitsetl_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitsetl_sort_index( bitsetl_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitsetl_sort_indexes

    subroutine test_bitsetl_sort_index( a, a_name, ltest )
        type(bitset_large), intent(in) :: a(0:)
        character(*), intent(in)       :: a_name
        logical, intent(out)           :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitsetl_dummy = a
            call system_clock( t0, rate )
            call sort_index( bitsetl_dummy, index, bitsetl_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitsetl_sort( bitsetl_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitsetl_dummy(i-1)%to_string(bin_im1)
            call bitsetl_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitsetl_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("| Bitset_large |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_Index", tdiff/rate

    end subroutine test_bitsetl_sort_index

    subroutine test_bitset64_sort_indexes(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        logical :: ltest

        call test_bitset64_sort_index( bitset64_decrease, "Bitset Decrease", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_index( bitset64_increase, "Bitset Increase", ltest )
        call check(error, ltest)
        if (allocated(error)) return

        call test_bitset64_sort_index( bitset64_rand, "Bitset Random", ltest )
        call check(error, ltest)

    end subroutine test_bitset64_sort_indexes

    subroutine test_bitset64_sort_index( a, a_name, ltest )
        type(bitset_64), intent(in) :: a(0:)
        character(*), intent(in)    :: a_name
        logical, intent(out)        :: ltest

        integer(int64) :: t0, t1, tdiff
        real(dp)       :: rate
        integer(int64) :: i
        logical        :: valid
        character(:), allocatable :: bin_im1, bin_i

        ltest = .true.

        tdiff = 0
        do i = 1, repeat
            bitset64_dummy = a
            call system_clock( t0, rate )
            call sort_index( bitset64_dummy, index, bitset64_work, iwork )
            call system_clock( t1, rate )
            tdiff = tdiff + t1 - t0
        end do
        tdiff = tdiff/repeat

        call verify_bitset64_sort( bitset64_dummy, valid, i )
        ltest = (ltest .and. valid)
        if ( .not. valid ) then
            write( *, * ) "SORT_INDEX did not sort " // a_name // "."
            write(*,*) 'i = ', i
            call bitset64_dummy(i-1)%to_string(bin_im1)
            call bitset64_dummy(i)%to_string(bin_i)
            write(*,'(a, 2(a:,1x))') 'bitset64_dummy(i-1:i) = ', &
                bin_im1, bin_i
        end if
        write( lun, '("|    Bitset_64 |", 1x, i7, 2x, "|", 1x, a15, " |", ' // &
            'a12, " |",  F10.6, " |" )' ) &
            bitset_size, a_name, "Sort_Index", tdiff/rate

    end subroutine test_bitset64_sort_index

    subroutine verify_bitsetl_sort( a, valid, i )
        type(bitset_large), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) > a(i) ) return
        end do
        valid = .true.

    end subroutine verify_bitsetl_sort

    subroutine verify_bitset64_sort( a, valid, i )
        type(bitset_64), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) > a(i) ) return
        end do
        valid = .true.

    end subroutine verify_bitset64_sort

    subroutine verify_bitsetl_reverse_sort( a, valid, i )
        type(bitset_large), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) < a(i) ) return
        end do
        valid = .true.

    end subroutine verify_bitsetl_reverse_sort

    subroutine verify_bitset64_reverse_sort( a, valid, i )
        type(bitset_64), intent(in) :: a(0:)
        logical, intent(out) :: valid
        integer(int64), intent(out) :: i

        integer(int64) :: n

        n = size( a, kind=int64 )
        valid = .false.
        do i=1, n-1
            if ( a(i-1) < a(i) ) return
        end do
        valid = .true.

    end subroutine verify_bitset64_reverse_sort
end module test_sorting


program tester
    use, intrinsic :: iso_fortran_env, only: compiler_version, error_unit
    use testdrive, only: new_testsuite, run_testsuite, testsuite_type
    use test_sorting, only: initialize_tests, collect_sorting

    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    call initialize_tests()

    stat = 0

    testsuites = [ &
        new_testsuite("sorting", collect_sorting) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester
