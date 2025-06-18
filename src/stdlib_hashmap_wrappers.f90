!! The module STDLIB_HASHMAP_WRAPPERS provides wrappers for various
!! entities used by the hash map procedures. These include wrappers for the
!! `key` and `other` data, and hashing procedures to operate on entities of
!! the `key_type`.

module stdlib_hashmap_wrappers

    use, intrinsic :: iso_fortran_env, only : &
        character_storage_size

    use stdlib_hash_32bit

    use stdlib_kinds, only : &
        int8,                &
        int16,               &
        int32,               &
        int64,               &
        dp

    implicit none

    private

!! Public procedures
    public ::                    &
        copy_key,                &
        copy_other,              &
        fibonacci_hash,          &
        fnv_1_hasher,            &
        fnv_1a_hasher,           &
        free_key,                &
        free_other,              &
        get,                     &
        hasher_fun,              &
        operator(==),            &
        seeded_nmhash32_hasher,  &
        seeded_nmhash32x_hasher, &
        seeded_water_hasher,     &
        set

    integer(4), parameter ::                  &
        offset_basis = int( z'811C9DC5', 4 ), &
        prime        = int( z'01000193', 4 )

!! Public types
    public ::      &
        key_type,  &
        other_type

!! Public integers
    public ::   &
        int_hash

    integer, parameter ::               &
! Should be 8
        bits_int8  = bit_size(0_int8)

    integer, parameter ::                   &
        bits_char = character_storage_size, &
        bytes_char = bits_char/bits_int8

    integer(int32), parameter :: nmh_prime32_1 = int( Z'9E3779B1', int32 )
    integer(int32), parameter :: nmh_prime32_2 = int( Z'85EBCA77', int32 )
    integer(int32), parameter :: nmh_prime32_3 = int( Z'C2B2AE3D', int32 )
    integer(int32), parameter :: nmh_prime32_4 = int( Z'27D4EB2F', int32 )

    integer(int32), parameter :: nmh_m1 = int(z'F0D9649B', int32 )
    integer(int32), parameter :: nmh_m2 = int(z'29A7935D', int32 )
    integer(int32), parameter :: nmh_m3 = int(z'55D35831', int32 )

    integer(int32), parameter :: nmh_m1_v(0:31) = nmh_m1
    integer(int32), parameter :: nmh_m2_v(0:31) = nmh_m2
    integer(int32), parameter :: nmh_m3_v(0:31) = nmh_m3

    logical, parameter :: nmh_short32_without_seed2=.false.
    logical, parameter :: nmh_short32_with_seed2=.true.

    integer, parameter :: init_size = 32

    integer(int32), parameter :: nmh_acc_init(0:init_size-1) = [ &
        int( z'B8FE6C39', int32 ), int( z'23A44BBE', int32 ), &
        int( z'7C01812C', int32 ), int( z'F721AD1C', int32 ), &
        int( z'DED46DE9', int32 ), int( z'839097DB', int32 ), &
        int( z'7240A4A4', int32 ), int( z'B7B3671F', int32 ), &
        int( z'CB79E64E', int32 ), int( z'CCC0E578', int32 ), &
        int( z'825AD07D', int32 ), int( z'CCFF7221', int32 ), &
        int( z'B8084674', int32 ), int( z'F743248E', int32 ), &
        int( z'E03590E6', int32 ), int( z'813A264C', int32 ), &

        int( z'3C2852BB', int32 ), int( z'91C300CB', int32 ), &
        int( z'88D0658B', int32 ), int( z'1B532EA3', int32 ), &
        int( z'71644897', int32 ), int( z'A20DF94E', int32 ), &
        int( z'3819EF46', int32 ), int( z'A9DEACD8', int32 ), &
        int( z'A8FA763F', int32 ), int( z'E39C343F', int32 ), &
        int( z'F9DCBBC7', int32 ), int( z'C70B4F1D', int32 ), &
        int( z'8A51E04B', int32 ), int( z'CDB45931', int32 ), &
        int( z'C89F7EC9', int32 ), int( z'D9787364', int32 ) ]

    character(*), parameter :: module_name = "STDLIB_HASHMAP_WRAPPERS"

    type :: key_type
!! Version: Experimental
!!
!! A wrapper type for the key's true type
!        private
        integer(int8), allocatable :: value(:)
    end type key_type

    abstract interface
!! Version: Experimental
!!
!! Abstract interface to a 64 bit hash function operating on a KEY_TYPE
        pure function hasher_fun( key )  result(hash_value)
            import key_type, int_hash
            type(key_type), intent(in)    :: key
            integer(int_hash)             :: hash_value
        end function hasher_fun
    end interface

    type :: other_type
!! Version: Experimental
!!
!! A wrapper type for the other data's true type
!        private
        class(*), allocatable :: value
    end type other_type

    interface get

        module procedure get_char_key,   &
                         get_int8_key,   &
                         get_other

    end interface get


    interface operator(==)
        module procedure equal_keys
    end interface operator(==)

    interface set

        module procedure set_char_key,   &
                         set_int8_key,   &
                         set_other

    end interface set

    interface fnv_1_hash_wrappers
        module procedure int8_fnv_1_wrappers,    &
                         int16_fnv_1_wrappers,   &
                         int32_fnv_1_wrappers,   &
                         int64_fnv_1_wrappers,   &
                         character_fnv_1_wrappers
    end interface fnv_1_hash_wrappers

    interface fnv_1a_hash_wrappers
        module procedure int8_fnv_1a_wrappers,   &
                         int16_fnv_1a_wrappers,  &
                         int32_fnv_1a_wrappers,  &
                         int64_fnv_1a_wrappers,  &
                         character_fnv_1a_wrappers
    end interface fnv_1a_hash_wrappers

    interface nmhash32_wrappers
        module procedure int8_nmhash32_wrappers,   &
                         int16_nmhash32_wrappers,  &
                         int32_nmhash32_wrappers,  &
                         int64_nmhash32_wrappers,  &
                         character_nmhash32_wrappers
    end interface nmhash32_wrappers

    interface nmhash32x_wrappers
        module procedure int8_nmhash32x_wrappers,   &
                         int16_nmhash32x_wrappers,  &
                         int32_nmhash32x_wrappers,  &
                         int64_nmhash32x_wrappers,  &
                         character_nmhash32x_wrappers
    end interface nmhash32x_wrappers

    interface water_hash_wrappers
        module procedure int8_water_hash_wrappers,   &
                         int16_water_hash_wrappers,  &
                         int32_water_hash_wrappers,  &
                         int64_water_hash_wrappers,  &
                         character_water_hash_wrappers
    end interface water_hash_wrappers

contains

    pure module function int8_fnv_1_wrappers( key ) result(hash_code)
        integer(int8), intent(in)     :: key(:)
        integer(int_hash)             :: hash_code

        integer(int64) :: i

        hash_code = offset_basis
        do i=1_int64, size(key, kind=int64)
            hash_code = hash_code * prime
            if ( little_endian ) then
                hash_code = ieor( hash_code, &
                                  transfer( [key(i), 0_int8, 0_int8, 0_int8], &
                                            0_int_hash ) )
            else
                hash_code = ieor( hash_code, &
                                  transfer( [0_int8, 0_int8, 0_int8, key(i)], &
                                            0_int_hash ) )
            end if
        end do

    end function int8_fnv_1_wrappers


    pure module function int16_fnv_1_wrappers( key ) result(hash_code)
        integer(int16), intent(in) :: key(:)
        integer(int_hash)           :: hash_code

        hash_code = int8_fnv_1_wrappers( transfer( key, 0_int8,                      &
                                          2*                     &
                                          size( key, kind=int64 ) ) )

    end function int16_fnv_1_wrappers

    pure module function int32_fnv_1_wrappers( key ) result(hash_code)
        integer(int32), intent(in) :: key(:)
        integer(int_hash)           :: hash_code

        hash_code = int8_fnv_1_wrappers( transfer( key, 0_int8,                      &
                                          4*                     &
                                          size( key, kind=int64 ) ) )

    end function int32_fnv_1_wrappers

    pure module function int64_fnv_1_wrappers( key ) result(hash_code)
        integer(int64), intent(in) :: key(:)
        integer(int_hash)           :: hash_code

        hash_code = int8_fnv_1_wrappers( transfer( key, 0_int8,                      &
                                          8*                     &
                                          size( key, kind=int64 ) ) )

    end function int64_fnv_1_wrappers



    elemental module function character_fnv_1_wrappers( key ) result(hash_code)
        character(*), intent(in)      :: key
        integer(int_hash)             :: hash_code

        hash_code = int8_fnv_1_wrappers( transfer( key,                           &
                                          0_int8,                        &
                                          1*                    &
                                          len(key, kind=int64) ) )

    end function character_fnv_1_wrappers

    pure module function int8_fnv_1a_wrappers( key ) result(hash_code)
        integer(int8), intent(in)     :: key(:)
        integer(int_hash)             :: hash_code

        integer(int64) :: i

        hash_code = offset_basis
        do i=1_int64, size(key, kind=int64)
            if ( little_endian ) then
                hash_code = ieor( hash_code, &
                                  transfer( [key(i), 0_int8, 0_int8, 0_int8],  &
                                            0_int_hash ) )
            else
                hash_code = ieor( hash_code, &
                                  transfer( [0_int8, 0_int8, 0_int8, key(i)], &
                                            0_int_hash ) )
            end if
            hash_code = hash_code * prime
        end do

    end function int8_fnv_1a_wrappers

    pure module function int16_fnv_1a_wrappers( key ) result(hash_code)
        integer(int16), intent(in)   :: key(:)
        integer(int_hash)             :: hash_code

        hash_code = int8_fnv_1a_wrappers( transfer( key, 0_int8,                   &
                                           2*                  &
                                           size(key, kind=int64)) )

    end function int16_fnv_1a_wrappers

    pure module function int32_fnv_1a_wrappers( key ) result(hash_code)
        integer(int32), intent(in)   :: key(:)
        integer(int_hash)             :: hash_code

        hash_code = int8_fnv_1a_wrappers( transfer( key, 0_int8,                   &
                                           4*                  &
                                           size(key, kind=int64)) )

    end function int32_fnv_1a_wrappers

    pure module function int64_fnv_1a_wrappers( key ) result(hash_code)
        integer(int64), intent(in)   :: key(:)
        integer(int_hash)             :: hash_code

        hash_code = int8_fnv_1a_wrappers( transfer( key, 0_int8,                   &
                                           8*                  &
                                           size(key, kind=int64)) )

    end function int64_fnv_1a_wrappers


    elemental module function character_fnv_1a_wrappers( key ) result(hash_code)
        character(*), intent(in)      :: key
        integer(int_hash)             :: hash_code

        hash_code = int8_fnv_1a_wrappers( transfer( key, 0_int8,                   &
                                           (bits_char/bits_int8)*         &
                                           len(key, kind=int64) ) )

    end function character_fnv_1a_wrappers

    pure function nmh_readle32_wrappers( p ) result( v )
        integer(int32) :: v
        integer(int8), intent(in) :: p(:)

        if ( little_endian ) then
            v = transfer( p(1:4), 0_int32 )
        else
            v = transfer( [ p(4), p(3), p(2), p(1) ], 0_int32 )
        end if

    end function nmh_readle32_wrappers

    pure function nmh_readle16_wrappers( p ) result( v )
        integer(int16) :: v
        integer(int8), intent(in) :: p(:)

        if ( little_endian ) then
            v = transfer( p(1:2), 0_int16 )
        else
            v = transfer( [ p(2), p(1) ], 0_int16 )
        end if

    end function nmh_readle16_wrappers

    pure function nmhash32_0to8_wrappers( x, seed ) result( vx32 )
        integer(int32), intent(in) :: x
        integer(int32), intent(in) :: seed
        integer(int32) :: vx32
        integer(int32), dimension(1) :: arr_vx32
        integer(int32), parameter :: m1 = int(z'776BF593', int32)
        integer(int32), parameter :: m2 = int(z'3FB39C65', int32)
        integer(int32), parameter :: m3 = int(z'E9139917', int32)
        integer(int32), dimension(1) :: arr_m1
        integer(int32), dimension(1) :: arr_m2
        integer(int32), dimension(1) :: arr_m3

        integer(int16) :: vx16(2)

        vx32 = x
        vx32 = ieor( vx32, ieor( ishft( vx32, -12 ), ishft( vx32, -6 ) ) )
        arr_vx32 = [vx32]
        vx16 = transfer( arr_vx32, 0_int16, 2 )
        arr_m1 = [m1]
        vx16 = vx16 * transfer( arr_m1, 0_int16, 2 )
        vx32 = transfer( vx16, 0_int32 )
        vx32 = ieor( vx32, ieor( ishft( vx32, 11 ), ishft( vx32, -19 ) ) )
        arr_vx32 = [vx32]
        vx16 = transfer( arr_vx32, 0_int16, 2 )
        arr_m2 = [m2]
        vx16 = vx16 * transfer( arr_m2, 0_int16, 2 )
        vx32 = transfer( vx16, 0_int32 )
        vx32 = ieor( vx32, seed )
        vx32 = ieor( vx32, ieor( ishft( vx32, -15 ), ishft( vx32, -9 ) ) )
        arr_vx32 = [vx32]
        vx16 = transfer( arr_vx32, 0_int16, 2 )
        arr_m3 = [m3]
        vx16 = vx16 * transfer( arr_m3, 0_int16, 2 )
        vx32 = transfer( vx16, 0_int32 )
        vx32 = ieor( vx32, ieor( ishft(vx32, 16), ishft(vx32, -11) ) )

    end function nmhash32_0to8_wrappers

    pure function nmhash32_9to255_wrappers( p, seed, full_avalanche ) result( result )
        integer(int8), intent(in)  :: p(0:)
        integer(int32), intent(in) :: seed
        logical, intent(in)        :: full_avalanche
        integer(int32) :: result

        integer(int32) :: xu32(0:3), yu32(0:3)
        integer(int16) :: xu16(0:1)
        integer(int16) :: nmh_m1_16(0:1), nmh_m2_16(0:1), nmh_m3_16(0:1)
        integer(int32), dimension(1) :: arr_nmh_m1
        integer(int32), dimension(1) :: arr_nmh_m2
        integer(int32), dimension(1) :: arr_nmh_m3
        integer(int32) :: s1
        integer(int64) :: length
        integer(int64), dimension(1) :: arr_length
        integer(int32), dimension(1) :: arr_xu32
        integer(int32) :: length32(0:1)
        integer(int64) :: i, j, r

        arr_nmh_m1 = [nmh_m1]
        nmh_m1_16(0:1) = transfer( arr_nmh_m1, 0_int16, 2 )
        arr_nmh_m2 = [nmh_m2]
        nmh_m2_16(0:1) = transfer( arr_nmh_m2, 0_int16, 2 )
        arr_nmh_m3 = [nmh_m3]
        nmh_m3_16(0:1) = transfer( arr_nmh_m3, 0_int16, 2 )

        result = 0
        length = size( p, kind=int64 )
        arr_length = [length]
        length32 = transfer(arr_length, 0_int32, 2)
        if (little_endian) then
            s1 = seed + length32(0)
        else
            s1 = seed + length32(1)
        end if
        xu32(0) = nmh_prime32_1
        xu32(1) = nmh_prime32_2
        xu32(2) = nmh_prime32_3
        xu32(3) = nmh_prime32_4
        yu32(:) = s1

        if (full_avalanche) then
            r = (length - 1 ) /32
            do i=0, r-1
                do j=0, 3
                    xu32(j) = ieor( xu32(j), nmh_readle32_wrappers( p(i*32 + j*4: ) ) )
                    yu32(j) = ieor( yu32(j), &
                                    nmh_readle32_wrappers( p(i*32 + j*4 + 16: ) ) )
                    xu32(j) = xu32(j) + yu32(j)
                    arr_xu32 = [xu32(i)]
                    xu16 = transfer( arr_xu32, 0_int16, 2 )
                    xu16 = xu16 * nmh_m1_16
                    xu32(j) = transfer( xu16, 0_int32 )
                    xu32(j) = ieor( xu32(j), &
                                    ieor( ishft(xu32(j), 5), &
                                          ishft(xu32(j), -13)) )
                    arr_xu32 = [xu32(j)]
                    xu16 = transfer( arr_xu32, 0_int16, 2 )
                    xu16 = xu16 * nmh_m2_16
                    xu32(j) = transfer( xu16, 0_int32 )
                    xu32(j) = ieor( xu32(j), yu32(j) )
                    xu32(j) = ieor( xu32(j), &
                                    ieor( ishft(xu32(j), 11), &
                                          ishft(xu32(j), -9) ) )
                    arr_xu32 = [xu32(j)]
                    xu16 = transfer( arr_xu32, 0_int16, 2 )
                    xu16 = xu16 * nmh_m3_16
                    xu32(j) = transfer( xu16, 0_int32 )
                    xu32(j) = ieor( xu32(j), &
                                    ieor( ishft(xu32(j),-10), &
                                          ishft(xu32(j), -20) ) )
                end do
            end do
            do j=0, 3
                xu32(j) = ieor( xu32(j), &
                                nmh_readle32_wrappers( p(length - 32 + j*4: ) ) )
                yu32(j) = ieor( yu32(j), &
                                nmh_readle32_wrappers( p(length - 16 + j*4: ) ) )
            end do
        else
            xu32(0) = ieor(xu32(0), nmh_readle32_wrappers(p(0:)))
            xu32(1) = ieor(xu32(1), nmh_readle32_wrappers(p(ishft(ishft(length,-4),3):)))
            xu32(2) = ieor(xu32(2), nmh_readle32_wrappers(p(length-8:)))
            xu32(3) = ieor(xu32(3), &
                           nmh_readle32_wrappers(p(length-8-ishft(ishft(length,-4),3):)))
            yu32(0) = ieor(yu32(0), nmh_readle32_wrappers(p(4:)))
            yu32(1) = ieor(yu32(1), &
                      nmh_readle32_wrappers(p(ishft(ishft(length,-4),3)+4:)))
            yu32(2) = ieor(yu32(2), nmh_readle32_wrappers(p(length-8+4:)))
            yu32(3) = ieor(yu32(3), &
                           nmh_readle32_wrappers(p(length - 8 - &
                                        ishft(ishft(length,-4),3)+4:)))
        end if
        do j=0, 3
            xu32(j) = xu32(j) + yu32(j)
            yu32(j) = ieor( yu32(j), ieor(ishft(yu32(j), 17), &
                                          ishft(yu32(j), -6) ) )
            arr_xu32 = [xu32(j)]
            xu16 = transfer( arr_xu32, 0_int16, 2 )
            xu16 = xu16 * nmh_m1_16
            xu32(j) = transfer( xu16, 0_int32 )
            xu32(j) = ieor( xu32(j), ieor(ishft(xu32(j), 5), &
                                          ishft(xu32(j), -13) ) )
            arr_xu32 = [xu32(j)]
            xu16 = transfer( arr_xu32, 0_int16, 2 )
            xu16 = xu16 * nmh_m2_16
            xu32(j) = transfer( xu16, 0_int32 )
            xu32(j) = ieor( xu32(j), yu32(j) )
            xu32(j) = ieor( xu32(j), ieor(ishft(xu32(j), 11), &
                                          ishft(xu32(j), -9) ) )
            arr_xu32 = [xu32(j)]
            xu16 = transfer( arr_xu32, 0_int16, 2 )
            xu16 = xu16 * nmh_m3_16
            xu32(j) = transfer( xu16, 0_int32 )
            xu32(j) = ieor( xu32(j), ieor(ishft(xu32(j), -10), &
                                          ishft(xu32(j), -20) ) )
        end do
        xu32(0) = ieor( xu32(0), nmh_prime32_1 )
        xu32(1) = ieor( xu32(1), nmh_prime32_2 )
        xu32(2) = ieor( xu32(2), nmh_prime32_3 )
        xu32(3) = ieor( xu32(3), nmh_prime32_4 )
        do j=1, 3
            xu32(0) = xu32(0) + xu32(j)
        end do
        xu32(0) = ieor(xu32(0), s1 + ishft(s1, -5) )
        arr_xu32 = [xu32(0)]
        xu16 = transfer( arr_xu32, 0_int16, 2 )
        xu16 = xu16 * nmh_m3_16
        xu32(0) = transfer( xu16, 0_int32 )
        xu32(0) = ieor(xu32(0), &
                       ieor(ishft(xu32(0), -10), ishft(xu32(0), -20) ) )
        result = xu32(0)

    end function nmhash32_9to255_wrappers

    pure function nmhash32_9to32_wrappers( p, seed ) result( result )
        integer(int8), intent(in)  :: p(0:)
        integer(int32), intent(in) :: seed
        integer(int32) :: result

        result = nmhash32_9to255_wrappers( p, seed, .false. )

    end function nmhash32_9to32_wrappers

    pure function nmhash32_33to255_wrappers( p, seed ) result( result )
        integer(int8), intent(in)  :: p(0:)
        integer(int32), intent(in) :: seed
        integer(int32) :: result

        result = nmhash32_9to255_wrappers( p, seed, .true. )

    end function nmhash32_33to255_wrappers

    pure subroutine nmhash32_long_round_wrappers( accx, accy, p )
        integer(int32), intent(inout) :: accx(0:)
        integer(int32), dimension(1) :: arr_accx_i
        integer(int32), dimension(1) :: arr_nmh_m1_v_i
        integer(int32), dimension(1) :: arr_nmh_m2_v_i
        integer(int32), dimension(1) :: arr_nmh_m3_v_i
        integer(int32), intent(inout) :: accy(0:)
        integer(int8), intent(in)     :: p(0:)

        integer(int64), parameter :: nbgroups = init_size
        integer(int64) :: i
        integer(int16) :: dummy1(0:1)
        integer(int16) :: dummy2(0:1)

        do i = 0, nbgroups-1
            accx(i) = ieor( accx(i), nmh_readle32_wrappers( p(i*4:) ) )
            accy(i) = ieor( accy(i), nmh_readle32_wrappers( p(i*4+nbgroups*4:) ) )
            accx(i) = accx(i) + accy(i)
            accy(i) = ieor( accy(i), ishft(accx(i),  -1) )
            arr_accx_i = [accx(i)]
            dummy1 = transfer( arr_accx_i, 0_int16, 2 )
            arr_nmh_m1_v_i = [nmh_m1_v(i)]
            dummy2 = transfer( arr_nmh_m1_v_i, 0_int16, 2 )
            dummy1 = dummy1 * dummy2
            accx(i) = transfer( dummy1, 0_int32 )
            accx(i) = ieor( accx(i), ieor( ishft(accx(i), 5), &
                                           ishft(accx(i),-13) ) )
            arr_accx_i = [accx(i)]
            dummy1 = transfer( arr_accx_i, 0_int16, 2 )
            arr_nmh_m2_v_i = [nmh_m2_v(i)]
            dummy2 = transfer( arr_nmh_m2_v_i, 0_int16, 2 )
            dummy1 = dummy1 * dummy2
            accx(i) = transfer( dummy1, 0_int32 )
            accx(i) = ieor( accx(i), accy(i) )
            accx(i) = ieor( accx(i), ieor( ishft(accx(i), 11), &
                                           ishft(accx(i),-9) ) )
            arr_accx_i = [accx(i)]
            dummy1 = transfer( arr_accx_i, 0_int16, 2 )
            arr_nmh_m3_v_i = [nmh_m3_v(i)]
            dummy2 = transfer( arr_nmh_m3_v_i, 0_int16, 2 )
            dummy1 = dummy1 * dummy2
            accx(i) = transfer( dummy1, 0_int32 )
            accx(i) = ieor( accx(i), ieor( ishft(accx(i),-10), &
                                           ishft(accx(i),-20) ) )
        end do

    end subroutine nmhash32_long_round_wrappers

    pure function nmhash32_long_wrappers( p, seed ) result( sum )
        integer(int32) :: sum
        integer(int8), intent(in) :: p(0:)
        integer(int32), intent(in) :: seed

        integer(int32) :: accx(0:size(nmh_acc_init)-1)
        integer(int32) :: accy(0:size(nmh_acc_init)-1)
        integer(int64) :: nbrounds
        integer(int64) :: len
        integer(int64), dimension(1) :: arr_len
        integer(int32) :: len32(0:1)
        integer(int64) :: i

        len  = size( p, kind=int64 )
        nbrounds = (len-1) / ( 4*size(accx, kind=int64) * 2 )
        sum = 0

        do i=0_int64, size(nmh_acc_init, kind=int64)-1
            accx(i) = nmh_acc_init(i)
            accy(i) = seed
        end do

        ! init
        do i=0_int64, nbrounds-1
            call nmhash32_long_round_wrappers( accx, accy, &
                                      p(i*8*size(accx, kind=int64):) )
        end do
        call nmhash32_long_round_wrappers( accx, accy, &
                                  p(len-8*size(accx, kind=int64):) )

        ! merge acc
        do i=0, size( accx, kind=int64 )-1
            accx(i) = ieor( accx(i), nmh_acc_init(i) )
            sum = sum + accx(i)
        end do

        arr_len = [len]
        len32 = transfer(arr_len, 0_int32, 2)
        if ( little_endian ) then
            sum = sum + len32(1)
            sum = ieor(sum, len32(0))
        else
            sum = sum + len32(0)
            sum = ieor(sum, len32(1))
        end if

    end function nmhash32_long_wrappers

    pure function nmhash32_avalanche32_wrappers( x ) result( u32 )
        integer(int32) :: u32
        integer(int32), intent(in) :: x

        integer(int16) :: u16(0:1)
        integer(int32), parameter:: m1 = int(z'CCE5196D', int32)
        integer(int32), parameter:: m2 = int(z'464BE229', int32)
        integer(int32), dimension(1) :: arr_m1
        integer(int32), dimension(1) :: arr_m2
        integer(int32), dimension(1) :: arr_u32
        integer(int16) :: m1_16(0:1), m2_16(0:1)

        arr_m1 = [m1]
        m1_16(0:1) = transfer(arr_m1, 0_int16, 2)
        arr_m2 = [m2]
        m2_16(0:1) = transfer(arr_m2, 0_int16, 2)

        u32 = x
        u32 = ieor( u32, ieor( ishft( u32, -8 ), ishft( u32, -21 ) ) )
        arr_u32 = [u32]
        u16 = transfer( arr_u32, 0_int16, 2 )
        u16(0) = u16(0) * m1_16(0)
        u16(1) = u16(1) * m1_16(1)
        u32 = transfer( u16, 0_int32 )
        u32 = ieor( u32, ieor( ishft( u32, 12 ), ishft( u32, -7 ) ) )
        arr_u32 = [u32]
        u16 = transfer( arr_u32, 0_int16, 2 )
        u16(0) = u16(0) * m2_16(0)
        u16(1) = u16(1) * m2_16(1)
        u32 = transfer( u16, 0_int32 )
        u32 = ieor( u32, ieor( ishft( u32, -8 ), ishft( u32, -21 ) ) )

    end function nmhash32_avalanche32_wrappers

    pure module function int8_nmhash32_wrappers( key, seed ) result( hash )
        integer(int32) :: hash
        integer(int8), intent(in) :: key(0:)
        integer(int32), intent(in) :: seed
        integer(int64) :: len
        integer(int32) :: u32
        integer(int16) :: u16(0:1)
        integer(int32) :: x, y
        integer(int32) :: new_seed

        len = size( key, kind=int64 )
        if ( len <= 32 ) then
            if ( len > 8 ) then
                hash = nmhash32_9to32_wrappers( key, seed )
                return
            else if ( len > 4 ) then
                x = nmh_readle32_wrappers(key)
                y = ieor( nmh_readle32_wrappers(key(len-4:)), nmh_prime32_4 + 2 + seed )
                x = x + y
                x = ieor( x, ishft(x, len + 7 ) )
                hash = nmhash32_0to8_wrappers( x, ishftc(y, 5) )
                return
            else
                select case(len)
                case(0)
                    new_seed = seed + nmh_prime32_2
                    u32 = 0
                case(1)
                    new_seed = seed + nmh_prime32_2 + ishft(1_int32, 24) + &
                               2_int32
                    if ( little_endian ) then
                        u32 = transfer( [key(0), 0_int8, 0_int8, 0_int8], &
                                        0_int32 )
                    else
                        u32 = transfer( [0_int8, 0_int8, 0_int8, key(0)], &
                                        0_int32 )
                    end if
                case(2)
                    new_seed = seed + nmh_prime32_2 + ishft(2_int32, 24) + &
                               4_int32
                    if (little_endian) then
                        u32 = transfer( [nmh_readle16_wrappers(key), 0_int16], 0_int32 )
                    else
                        u32 = transfer( [0_int16, nmh_readle16_wrappers(key)], 0_int32 )
                    end if
                case(3)
                    new_seed = seed + nmh_prime32_2 + ishft(3_int32, 24) + &
                               6_int32
                    if ( little_endian ) then
                        u16(1) = transfer( [key(2), 0_int8], 0_int16 )
                        u16(0) = nmh_readle16_wrappers( key )
                    else
                        u16(0) = transfer( [0_int8, key(2)], 0_int16 )
                        u16(1) = nmh_readle16_wrappers( key )
                    end if
                    u32 = transfer( u16, 0_int32 )
                case(4)
                    new_seed = seed + nmh_prime32_3
                    u32 = nmh_readle32_wrappers(key)
                case default
                    hash = 0
                    return
                end select
                hash = nmhash32_0to8_wrappers(u32+new_seed, ishftc(new_seed, 5) )
                return
            end if
        else if ( len < 256_int64 ) then
            hash = nmhash32_33to255_wrappers( key, seed )
            return
        else
            hash = nmhash32_avalanche32_wrappers( nmhash32_long_wrappers(key, seed ))
            return
        end if

    end function int8_nmhash32_wrappers

    pure module function int16_nmhash32_wrappers( key, seed ) result(hash_code)
        integer(int16), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int32)           :: hash_code

        hash_code = int8_nmhash32_wrappers( transfer( key, 0_int8, &
                     2*size(key, kind=int64) ), seed)

    end function int16_nmhash32_wrappers

    pure module function int32_nmhash32_wrappers( key, seed ) result(hash_code)
        integer(int32), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int32)           :: hash_code

        hash_code = int8_nmhash32_wrappers( transfer( key, 0_int8, &
                     4*size(key, kind=int64) ), seed)

    end function int32_nmhash32_wrappers

    pure module function int64_nmhash32_wrappers( key, seed ) result(hash_code)
        integer(int64), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int32)           :: hash_code

        hash_code = int8_nmhash32_wrappers( transfer( key, 0_int8, &
                     8*size(key, kind=int64) ), seed)

    end function int64_nmhash32_wrappers


    elemental module function character_nmhash32_wrappers( key, seed ) result(hash_code)
        character(*), intent(in)   :: key
        integer(int32), intent(in) :: seed
        integer(int32)             :: hash_code

        hash_code = int8_nmhash32_wrappers( transfer( key, 0_int8, &
                     bytes_char*len(key, kind=int64) ), seed)

    end function character_nmhash32_wrappers

    pure function nmhash32x_0to4_wrappers( x, seed ) result( hash )
        integer(int32), intent(in) :: x
        integer(int32), intent(in) :: seed
        integer(int32) :: hash

        hash = x
        hash = ieor( hash, seed )
        hash = hash * int(z'BDAB1EA9', int32)
        hash = hash + ishftc(seed, 31)
        hash = ieor( hash, ishft(hash, -18) )
        hash = hash * int(z'A7896A1B', int32)
        hash = ieor( hash, ishft(hash, -12) )
        hash = hash * int(z'83796A2D', int32)
        hash = ieor( hash, ishft(hash, -16) )

    end function nmhash32x_0to4_wrappers

    pure function nmhash32x_5to8_wrappers( p, seed ) result( x )
        integer(int8), intent(in) :: p(0:)
        integer(int32), intent(in) :: seed
        integer(int32) :: x

        integer(int64) :: len
        integer(int32) :: y

        len = size(p, kind=int64)
        x = ieor( nmh_readle32_wrappers(p), nmh_prime32_3 )
        y = ieor( nmh_readle32_wrappers(p(len-4:)), seed )
        x  = x + y
        x = ieor( x, ishft(x, -len) )
        x = x * int(z'11049A7D', int32)
        x = ieor( x, ishft(x, -23) )
        x = x * int(z'BCCCDC7B', int32)
        x = ieor( x, ishftc(y, 3) )
        x = ieor( x, ishft(x, -12) )
        x = x * int(z'065E9DAD', int32)
        x = ieor( x, ishft(x, -12) )

    end function nmhash32x_5to8_wrappers

    pure function nmhash32x_9to255_wrappers( p, seed ) result( x )
        integer(int8), intent(in) :: p(0:)
        integer(int32), intent(in) :: seed
        integer(int32) :: x

        integer(int64) :: len
        integer(int64), dimension(1) :: arr_len
        integer(int32) :: len32(0:1), len_base
        integer(int32) :: y
        integer(int32) :: a, b
        integer(int64) :: i, r

        len = size(p, kind=int64)
        arr_len = [len]
        len32 = transfer(arr_len, 0_int32, 2)
        if (little_endian) then
            len_base = len32(0)
        else
            len_base = len32(1)
        end if
        x = nmh_prime32_3
        y = seed
        a = nmh_prime32_4
        b = seed
        r = (len - 1)/16

        do i=0, r-1
            x = ieor(x, nmh_readle32_wrappers( p(i*16 + 0:) ) )
            y = ieor(y, nmh_readle32_wrappers( p(i*16 + 4:) ) )
            x = ieor(x, y)
            x = x * int(z'11049A7D', int32)
            x = ieor(x, ishft(x, -23) )
            x = x * int(z'BCCCDC7B', int32)
            y = ishftc(y, 4)
            x = ieor(x, y)
            x = ieor(x, ishft(x, -12) )
            x = x * int(z'065E9DAD', int32)
            x = ieor(x, ishft(x, -12) )

            a = ieor(a, nmh_readle32_wrappers(p(i*16 + 8:)))
            b = ieor(b, nmh_readle32_wrappers(p(i*16 + 12:)))
            a = ieor(a, b)
            a = a * int(z'11049A7D', int32)
            a = ieor(a, ishft(a, -23) )
            a = a * int(z'BCCCDC7B', int32)
            b = ishftc(b, 3)
            a = ieor(a, b)
            a = ieor(a, ishft(a, -12) )
            a = a * int(z'065E9DAD', int32)
            a = ieor(a, ishft(a, -12) )
        end do

        if ( iand(len_base-1_int32, 8_int32) /= 0 ) then
            if ( iand(len_base-1_int32, 4_int32) /= 0 ) then
                a = ieor( a, nmh_readle32_wrappers( p(r*16 + 0:) ) )
                b = ieor( b, nmh_readle32_wrappers( p(r*16 + 4:) ) )
                a = ieor(a, b)
                a = a * int(z'11049A7D', int32)
                a = ieor(a, ishft(a, -23) )
                a = a * int(z'BCCCDC7B', int32)
                a = ieor(a, ishftc(b, 4))
                a = ieor(a, ishft(a, -12))
                a = a * int(z'065E9DAD', int32)
            else
                a = ieor( a, nmh_readle32_wrappers( p(r*16:) ) + b )
                a = ieor( a, ishft(a, -16) )
                a = a * int(z'A52FB2CD', int32)
                a = ieor( a, ishft(a, -15) )
                a = a * int(z'551E4D49', int32)
            end if
            x = ieor( x, nmh_readle32_wrappers( p(len - 8:) ) )
            y = ieor( y, nmh_readle32_wrappers( p(len - 4:) ) )
            x = ieor( x, y )
            x = x * int(z'11049A7D', int32)
            x = ieor( x, ishft(x, -23) )
            x = x * int(z'BCCCDC7B', int32);
            x = ieor( x, ishftc(y, 3) )
            x = ieor( x, ishft(x, -12) )
            x = x * int(z'065E9DAD', int32)
        else
            if ( iand(len_base-1_int32, 4_int32) /= 0) then
                a = ieor(a, nmh_readle32_wrappers(p( r * 16:) ) + b )
                a = ieor( a, ishft(a,-16) )
                a = a * int(z'A52FB2CD', int32)
                a = ieor( a, ishft(a,-15) )
                a = a * int(z'551E4D49', int32)
            end if
            x = ieor( x, nmh_readle32_wrappers(p( len - 4:) ) + y )
            x = ieor( x, ishft(x,-16) )
            x = x * int(z'A52FB2CD', int32)
            x = ieor( x, ishft(x,-15) )
            x = x * int(z'551E4D49', int32)
        end if

        x = ieor(x, len_base )
        x = ieor(x, ishftc(a, 27)) ! rotate one lane to pass Diff test
        x = ieor(x, ishft(x,-14))
        x = x * int(z'141CC535', int32 )

    end function nmhash32x_9to255_wrappers

    pure function nmhash32x_avalanche32_wrappers( x ) result(hash)
        integer(int32) :: hash
        integer(int32), intent(in) :: x

        hash = x
        hash = ieor( hash, ishft( hash, -15 ) )
        hash = hash * int( z'D168AAAD', int32 )
        hash = ieor( hash, ishft( hash, -15 ) )
        hash = hash * int( z'AF723597', int32 )
        hash = ieor( hash, ishft( hash, -15 ) )

    end function nmhash32x_avalanche32_wrappers

    pure module function int8_nmhash32x_wrappers( key, seed ) result(hash)
        integer(int32) :: hash
        integer(int8), intent(in) :: key(0:)
        integer(int32), intent(in) :: seed

        integer(int64) :: len
        integer(int32) :: seed2
        integer(int32) :: u32
        integer(int16) :: u16(0:1)

        len = size( key, kind=int64 )
        if ( len <= 8 ) then
            if ( len > 4 ) then
                hash = nmhash32x_5to8_wrappers( key, seed )
                return
            else ! 0 to 4 bytes
                select case (len)
                case(0)
                    seed2 = seed + nmh_prime32_2
                    u32 = 0
                case(1)
                    seed2 = seed + nmh_prime32_2 + ishft(1_int32, 24) + &
                        ishft(1_int32, 1)
                    if (little_endian) then
                        u32 = transfer( [key(0), 0_int8, 0_int8, 0_int8], &
                                        0_int32 )
                    else
                        u32 = transfer( [0_int8, 0_int8, 0_int8, key(0)], &
                                        0_int32 )
                    end if
                case(2)
                    seed2 = seed + nmh_prime32_2 + ishft(2_int32, 24) + &
                        ishft(2_int32, 1)
                    if (little_endian) then
                        u32 = transfer( [nmh_readle16_wrappers(key), 0_int16], 0_int32 )
                    else
                        u32 = transfer( [0_int16, nmh_readle16_wrappers(key)], 0_int32 )
                    end if
                case(3)
                    seed2 = seed + nmh_prime32_2 + ishft(3_int32, 24) + &
                        ishft(3_int32, 1)
                    if (little_endian ) then
                        u16(1) = transfer( [ key(2), 0_int8 ], 0_int16 )
                        u16(0) = nmh_readle16_wrappers(key)
                    else
                        u16(0) = transfer( [ 0_int8, key(2) ], 0_int16 )
                        u16(1) = nmh_readle16_wrappers(key)
                    end if
                    u32 = transfer( u16, 0_int32 )
                case(4)
                    seed2 = seed + nmh_prime32_1
                    u32 = nmh_readle32_wrappers(key)
                case default
                    hash = 0
                    return
                end select
                hash = nmhash32x_0to4_wrappers(u32, seed2)
                return
            end if
        end if
        if (len < 256) then
            hash = nmhash32x_9to255_wrappers(key, seed)
            return
        end if
        hash = nmhash32x_avalanche32_wrappers(nmhash32_long_wrappers(key, seed))

    end function int8_nmhash32x_wrappers

    pure module function int16_nmhash32x_wrappers( key, seed ) result(hash_code)
        integer(int16), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int32)           :: hash_code

        hash_code = int8_nmhash32x_wrappers( transfer( key, 0_int8, &
                     2*size(key, kind=int64) ), seed)

    end function int16_nmhash32x_wrappers

    pure module function int32_nmhash32x_wrappers( key, seed ) result(hash_code)
        integer(int32), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int32)           :: hash_code

        hash_code = int8_nmhash32x_wrappers( transfer( key, 0_int8, &
                     4*size(key, kind=int64) ), seed)

    end function int32_nmhash32x_wrappers

    pure module function int64_nmhash32x_wrappers( key, seed ) result(hash_code)
        integer(int64), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int32)           :: hash_code

        hash_code = int8_nmhash32x_wrappers( transfer( key, 0_int8, &
                     8*size(key, kind=int64) ), seed)

    end function int64_nmhash32x_wrappers


    elemental module function character_nmhash32x_wrappers( key, seed ) result(hash_code)
        character(*), intent(in)   :: key
        integer(int32), intent(in) :: seed
        integer(int32)             :: hash_code

        hash_code = int8_nmhash32x_wrappers( transfer( key, 0_int8, &
                     bytes_char*len(key, kind=int64) ), seed)

    end function character_nmhash32x_wrappers

    pure module function int8_water_hash_wrappers( key, seed ) result(hash_code)
        integer(int32)             :: hash_code
        integer(int8), intent(in)  :: key(0:)
        integer(int64), intent(in) :: seed

        integer(int32) :: dummy(2)
        integer(int64) :: h
        integer(int64) :: i
        integer(int64) :: len
        integer(int64), parameter ::                &
            waterp0 = int(z'a0761d65', kind=int64), &
            waterp1 = int(z'e7037ed1', kind=int64), &
            waterp2 = int(z'8ebc6af1', kind=int64), &
            waterp3 = int(z'589965cd', kind=int64), &
            waterp4 = int(z'1d8e4e27', kind=int64), &
            waterp5 = int(z'eb44accb', kind=int64)

        len = size(key, kind=int64)
        h = seed
        do i = 0_int64, len-16, 16
            h = watermum_wrappers(watermum_wrappers(ieor(waterr32_wrappers(key(i:)),waterp1),        &
                                  ieor(waterr32_wrappers(key(i+4:)),waterp2)) + h, &
                         watermum_wrappers(ieor(waterr32_wrappers(key(i+8:)),waterp3),      &
                                  ieor(waterr32_wrappers(key(i+12:)),waterp4)))
        end do
        h = h + waterp5

        select case( iand(len, 15_int64) )
        case(1)
            h = watermum_wrappers(ieor(waterp2, h),               &
                         ieor(waterr08_wrappers(key(i:)), waterp1))
        case(2)
            h = watermum_wrappers(ieor(waterp3, h),               &
                         ieor(waterr16_wrappers(key(i:)), waterp4))
        case(3)
            h = watermum_wrappers(ieor(waterr16_wrappers(key(i:)), h),        &
                         ieor(waterr08_wrappers(key(i+2:)), waterp2))
        case(4)
            h = watermum_wrappers(ieor(waterr16_wrappers(key(i:)), h),        &
                         ieor(waterr16_wrappers(key(i+2:)), waterp3))
        case(5)
            h = watermum_wrappers(ieor(waterr32_wrappers(key(i:)), h),        &
                         ieor(waterr08_wrappers(key(i+4:)), waterp1))
        case(6)
            h = watermum_wrappers(ieor(waterr32_wrappers(key(i:)), h),        &
                         ieor(waterr16_wrappers(key(i+4:)), waterp1))
        case(7)
            h = watermum_wrappers(ieor(waterr32_wrappers(key(i:)), h),             &
                         ieor(ior(ishft(waterr16_wrappers(key(i+4:)), 8), &
                                  waterr08_wrappers(key(i+6:))), waterp1))
        case(8)
            h = watermum_wrappers(ieor(waterr32_wrappers(key(i:)), h),        &
                         ieor(waterr32_wrappers(key(i+4:)), waterp0))
        case(9)
            h = ieor(watermum_wrappers(ieor(waterr32_wrappers(key(i:)), h),          &
                              ieor(waterr32_wrappers(key(i+4:)), waterp2)), &
                     watermum_wrappers(ieor(h, waterp4),                    &
                              ieor(waterr08_wrappers(key(i+8:)), waterp3)))
        case(10)
            h = ieor(watermum_wrappers(ieor(waterr32_wrappers(key(i:)), h),            &
                              ieor(waterr32_wrappers(key(i+4:)), waterp2)),   &
                     watermum_wrappers(h, ieor(waterr16_wrappers(key(i+8:)), waterp3)))
        case(11)
            h = ieor(watermum_wrappers(ieor(waterr32_wrappers(key(i:)), h),            &
                              ieor(waterr32_wrappers(key(i+4:)), waterp2)),   &
                     watermum_wrappers(h,                                     &
                              ieor(ior(ishft(waterr16_wrappers(key(i+8:)),8), &
                                       waterr08_wrappers(key(i+10:))),        &
                                   waterp3)))
        case(12)
            h = ieor(watermum_wrappers(ieor(waterr32_wrappers(key(i:)), h),          &
                              ieor(waterr32_wrappers(key(i+4:)), waterp2)), &
                     watermum_wrappers(ieor(h, waterr32_wrappers(key(i+8:))),        &
                                      waterp4))
        case(13)
            h = ieor(watermum_wrappers(ieor(waterr32_wrappers(key(i:)), h),            &
                              ieor(waterr32_wrappers(key(i+4:)), waterp2)),   &
                     watermum_wrappers(ieor(h, waterr32_wrappers(key(i+8:))),          &
                              ieor(waterr08_wrappers(key(i+12:)), waterp4)))
        case(14)
            h = ieor(watermum_wrappers(ieor(waterr32_wrappers(key(i:)), h),            &
                              ieor(waterr32_wrappers(key(i+4:)), waterp2)),   &
                     watermum_wrappers(ieor(h, waterr32_wrappers(key(i+8:))),          &
                              ieor(waterr16_wrappers(key(i+12:)), waterp4)))
        case(15)
            h = ieor(watermum_wrappers(ieor(waterr32_wrappers(key(i:)), h),             &
                              ieor(waterr32_wrappers(key(i+4:)), waterp2)),    &
                     watermum_wrappers(ieor(h, waterr32_wrappers(key(i+8:))),           &
                              ieor(ior(ishft(waterr16_wrappers(key(i+12:)),8), &
                                       waterr08_wrappers(key(i+14:))),         &
                                   waterp4)))
        end select

        h = ieor( h, ishft(h,16) ) * ieor( len, waterp0 )
        h = h - ishft( h, -32 )
        dummy(1:2) = transfer(h, dummy, 2)
        if (little_endian) then
            hash_code = dummy(1)
        else
            hash_code = dummy(2)
        end if

    contains

        pure function watermum_wrappers( a, b ) result(r)
            integer(int64)             :: r
            integer(int64), intent(in) :: a, b

            r = a * b
            r = r - ishft(r, -32)

        end function watermum_wrappers

        pure function waterr08_wrappers( p ) result(v)
            integer(int64)            :: v
            integer(int8), intent(in) :: p(:)

            if (little_endian) then
                v = transfer( [ p(1), 0_int8, 0_int8, 0_int8,       &
                                0_int8, 0_int8, 0_int8, 0_int8 ], v )
            else
                v = transfer( [ 0_int8, 0_int8, 0_int8, 0_int8,   &
                                0_int8, 0_int8, 0_int8, p(1) ], v )
            end if

        end function waterr08_wrappers

        pure function waterr16_wrappers( p ) result(v)
            integer(int64)            :: v
            integer(int8), intent(in) :: p(:)

            if (little_endian) then
                v = transfer( [ p(1), p(2), 0_int8, 0_int8,         &
                                0_int8, 0_int8, 0_int8, 0_int8 ], v )
            else
                v = transfer( [ 0_int8, 0_int8, 0_int8, 0_int8,  &
                                0_int8, 0_int8, p(2), p(1) ], v )
            end if

        end function waterr16_wrappers

        pure function waterr32_wrappers( p ) result(v)
            integer(int64)            :: v
            integer(int8), intent(in) :: p(:)

            if (little_endian) then
                v = transfer( [ p(1), p(2), p(3), p(4),             &
                                0_int8, 0_int8, 0_int8, 0_int8 ], v )
            else
                v = transfer( [ 0_int8, 0_int8, 0_int8, 0_int8, &
                                p(4), p(3), p(2), p(1) ], v )
            end if

        end function waterr32_wrappers

    end function int8_water_hash_wrappers


    pure module function int16_water_hash_wrappers( key, seed ) result(hash_code)
        integer(int16), intent(in) :: key(:)
        integer(int64), intent(in)  :: seed
        integer(int_hash)           :: hash_code

        hash_code = int8_water_hash_wrappers( transfer( key, 0_int8, &
                     2*size(key, kind=int64) ), seed)

    end function int16_water_hash_wrappers

    pure module function int32_water_hash_wrappers( key, seed ) result(hash_code)
        integer(int32), intent(in) :: key(:)
        integer(int64), intent(in)  :: seed
        integer(int_hash)           :: hash_code

        hash_code = int8_water_hash_wrappers( transfer( key, 0_int8, &
                     4*size(key, kind=int64) ), seed)

    end function int32_water_hash_wrappers

    pure module function int64_water_hash_wrappers( key, seed ) result(hash_code)
        integer(int64), intent(in) :: key(:)
        integer(int64), intent(in)  :: seed
        integer(int_hash)           :: hash_code

        hash_code = int8_water_hash_wrappers( transfer( key, 0_int8, &
                     8*size(key, kind=int64) ), seed)

    end function int64_water_hash_wrappers


    elemental module function character_water_hash_wrappers( key, seed ) &
        result(hash_code)
        character(*), intent(in)   :: key
        integer(int64), intent(in) :: seed
        integer(int_hash)          :: hash_code

        hash_code = int8_water_hash_wrappers( transfer( key, 0_int8, &
                     bytes_char*len(key, kind=int64) ), seed)

    end function character_water_hash_wrappers

    pure subroutine copy_key( old_key, new_key )
!! Version: Experimental
!!
!! Copies the contents of the key, old_key, to the key, new_key
!! ([Specifications](../page/specs/stdlib_hashmaps.html#copy_key-returns-a-copy-of-the-key))
!!
!! Arguments:
!!     old_key - the input key
!!     new_key - the output copy of old_key
        type(key_type), intent(in)  :: old_key
        type(key_type), intent(out) :: new_key

        new_key % value = old_key % value

    end subroutine copy_key


    subroutine copy_other( other_in, other_out )
!! Version: Experimental
!!
!! Copies the other data, other_in, to the variable, other_out
!! ([Specifications](../page/specs/stdlib_hashmaps.html#copy_other-returns-a-copy-of-the-other-data))
!!
!! Arguments:
!!     other_in  - the input data
!!     other_out - the output data
        type(other_type), intent(in)  :: other_in
        type(other_type), intent(out) :: other_out

        allocate(other_out % value, source = other_in % value )

    end subroutine copy_other


    function equal_keys( key1, key2 ) result(test) ! Chase's tester
!! Version: Experimental
!!
!! Compares two keys for equality
!! ([Specifications](../page/specs/stdlib_hashmaps.html#operator(==)-compares-two-keys-for-equality))
!!
!! Arguments:
!!     key1 - the first key
!!     key2 - the second key
        logical                    :: test
        type(key_type), intent(in) :: key1
        type(key_type), intent(in) :: key2

        if ( size(key1 % value, kind=int64) /= &
             size(key2 % value, kind=int64) ) then
            test = .false.
            return
        end if

        if ( all( key1 % value == key2 % value ) ) then
            test = .true.
        else
            test = .false.
        end if

    end function equal_keys


    subroutine free_key( key )
!! Version: Experimental
!!
!! Frees the memory in a key
!! ([Specifications](../page/specs/stdlib_hashmaps.html#free_key-frees-the-memory-associated-with-a-key))
!!
!! Arguments:
!!     key  - the key
        type(key_type), intent(inout) :: key

        if ( allocated( key % value ) ) deallocate( key % value )

    end subroutine free_key


    subroutine free_other( other )
!! Version: Experimental
!!
!! Frees the memory in the other data
!! ([Specifications](../page/specs/stdlib_hashmaps.html#free_other-frees-the-memory-associated-with-other-data))
!!
!! Arguments:
!!     other  - the other data
        type(other_type), intent(inout) :: other

        if ( allocated( other % value) ) deallocate( other % value )

    end subroutine free_other


    subroutine get_char_key( key, value )
!! Version: Experimental
!!
!! Gets the contents of the key as a CHARACTER string
!! Arguments:
!!     key   - the input key
!!     value - the contents of key mapped to a CHARACTER string
        type(key_type), intent(in)             :: key
        character(:), allocatable, intent(out) :: value
        character(*), parameter :: procedure = "GET"

        integer(int64) :: key_as_char
        integer(int64) :: key_size

        key_size = size( key % value, kind=int64 )
        select case( bytes_char )
        case(1)
            key_as_char = key_size
        case(2)
            if ( iand( key_size, 1_int64 ) > 0 ) then
                error stop module_name // " % " // procedure // &
                          ": Internal Error at stdlib_hashmaps: " // &
                           "System uses 2 bytes per character, so " // &
                           "key_size can't be an odd number."
            end if
            key_as_char = ishft( key_size, -1 )
        case(4)
            if ( iand( key_size, 3_int64) > 0 ) then
                error stop module_name // " % " // procedure // &
                          ": Internal Error at stdlib_hashmaps: " // &
                           "System uses 4 bytes per character, and " // &
                           "key_size is not a multiple of four."
            end if
            key_as_char = ishft( key_size, -2 )
        case default
            error stop module_name // " % " // procedure // &
                       ": Internal Error: " // &
                       "System doesn't use a power of two for its " // &
                       "character size as expected by stdlib_hashmaps."
        end select

        allocate( character( len=key_as_char ) :: value )

        value(1:key_as_char) = transfer( key % value, value )

    end subroutine get_char_key

    subroutine get_other( other, value )
!! Version: Experimental
!!
!! Gets the contents of the other as a CLASS(*) string
!! Arguments:
!!     other - the input other data
!!     value - the contents of other mapped to a CLASS(*) variable
        type(other_type), intent(in)       :: other
        class(*), allocatable, intent(out) :: value

        allocate(value, source=other % value)

    end subroutine get_other


    subroutine get_int8_key( key, value )
!! Version: Experimental
!!
!! Gets the contents of the key as an INTEGER(INT8) vector
!! Arguments:
!!     key   - the input key
!!     value - the contents of key mapped to an INTEGER(INT8) vector
        type(key_type), intent(in)              :: key
        integer(int8), allocatable, intent(out) :: value(:)

        value = key % value

    end subroutine get_int8_key


    subroutine set_char_key( key, value )
!! Version: Experimental
!!
!! Sets the contents of the key from a CHARACTER string
!! Arguments:
!!     key   - the output key
!!     value - the input CHARACTER string
        type(key_type), intent(out) :: key
        character(*), intent(in)    :: value

        allocate(key % value(bytes_char * len(value)))
        key % value = transfer( value, key % value, &
                                bytes_char * len( value ) )

    end subroutine set_char_key


    subroutine set_other( other, value )
!! Version: Experimental
!!
!! Sets the contents of the other data from a CLASS(*) variable
!! Arguments:
!!     other - the output other data
!!     value - the input CLASS(*) variable
        type(other_type), intent(out) :: other
        class(*), intent(in)          :: value

        allocate(other % value, source=value)

    end subroutine set_other


    subroutine set_int8_key( key, value )
!! Version: Experimental
!!
!! Sets the contents of the key from an INTEGER(INT8) vector
!! Arguments:
!!     key   - the output key
!!     value - the input INTEGER(INT8) vector
        type(key_type), intent(out) :: key
        integer(int8), intent(in)   :: value(:)

        key % value = value

    end subroutine set_int8_key


    pure function fnv_1_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the FNV_1 algorithm
!! Arguments:
!!     key  - the key to be hashed
        type(key_type), intent(in)    :: key
        integer(int_hash)             :: fnv_1_hasher

        fnv_1_hasher = fnv_1_hash_wrappers( key % value )

    end function fnv_1_hasher


    pure function fnv_1a_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the FNV_1a algorithm
!! ([Specifications](../page/specs/stdlib_hashmaps.html#fnv_1a_hasher-calculates-a-hash-code-from-a-key))
!!
!! Arguments:
!!     key  - the key to be hashed
        type(key_type), intent(in)    :: key
        integer(int_hash)             :: fnv_1a_hasher

        fnv_1a_hasher = fnv_1a_hash_wrappers( key % value )

    end function fnv_1a_hasher


    pure function seeded_nmhash32_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the NMHASH32 hash algorithm
!! ([Specifications](../page/specs/stdlib_hashmaps.html#seeded_nmhash32_hasher-calculates-a-hash-code-from-a-key))
!!
!! Arguments:
!!     key  - the key to be hashed
!!     seed - the seed (unused) for the hashing algorithm
        type(key_type), intent(in)    :: key
        integer(int_hash)             :: seeded_nmhash32_hasher

        seeded_nmhash32_hasher = nmhash32_wrappers( key % value, &
            int( z'DEADBEEF', int32 ) )

    end function seeded_nmhash32_hasher


    pure function seeded_nmhash32x_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the NMHASH32X hash algorithm
!! ([Specifications](../page/specs/stdlib_hashmaps.html#seeded_nmhash32x_hasher-calculates-a-hash-code-from-a-key))
!! Arguments:
!!     key  - the key to be hashed
!!     seed - the seed (unused) for the hashing algorithm
        type(key_type), intent(in)    :: key
        integer(int_hash)             :: seeded_nmhash32x_hasher

        seeded_nmhash32x_hasher = nmhash32x_wrappers( key % value, &
            int( z'DEADBEEF', int32 ) )

    end function seeded_nmhash32x_hasher


    pure function seeded_water_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the waterhash algorithm
!! ([Specifications](../page/specs/stdlib_hashmaps.html#seeded_water_hasher-calculates-a-hash-code-from-a-key))
!!
!! Arguments:
!!     key  - the key to be hashed
        type(key_type), intent(in)  :: key
        integer(int_hash)           :: seeded_water_hasher

        seeded_water_hasher = water_hash_wrappers( key % value, &
            int( z'DEADBEEF1EADBEEF', int64 ) )

    end function seeded_water_hasher


end module stdlib_hashmap_wrappers
