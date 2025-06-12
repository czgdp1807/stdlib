!! The module, STDLIB_HASH_MAPS, implements two hash maps:
!! CHAINING_HASH_MAP_TYPE, a separate chaining hash map; and OPEN_HASH_MAP_TYPE,
!! an open addressing hash map using linear addressing. The two hash maps are
!! implementations of the abstract type, HASH_MAP_TYPE.

module stdlib_hashmaps

    use, intrinsic :: iso_fortran_env, only: &
        character_storage_size,              &
        error_unit

    use stdlib_kinds, only: &
        dp,                 &
        int8,               &
        int16,              &
        int32,              &
        int64

    use stdlib_hashmap_wrappers

    implicit none

    !private

!! Public data_types
    public ::                  &
        chaining_hashmap_type, &
        hashmap_type,          &
        open_hashmap_type

!! Values that parameterize David Chase's empirical SLOT expansion code
    integer, parameter ::        &
        inmap_probe_factor = 10, &
        map_probe_factor   =  5

!! Values that parameterize the SLOTS table size
    integer, parameter, public :: &
        default_bits =  6,        &
        max_bits     = 30

!! KIND values used to parameterixe the hash map and its procedures
    integer, parameter, public :: &
        int_calls  = int64,       &
        int_depth  = int64,       &
        int_index  = int32,       &
        int_probes = int64

!! Error codes returned by the hash map procedures
    integer, parameter, public ::  &
        success = 0,               &
        alloc_fault = 1,           &
        array_size_error = 2

! The number of bits used by various types
    integer, parameter ::             &
! Should be 8
        int8_bits = bit_size(0_int8), &
        char_bits = character_storage_size

!! The hash map load factor
    real, parameter, public ::      &
        load_factor = 0.5625

!! The size of the pools of allocated map entries
    integer(int32), parameter :: pool_size = 64

    character(*), parameter, private :: module_name = 'STDLIB_HASHMAPS'

    abstract interface
        pure function hasher_fun_temp( key )  result(hash_value)
            import key_type, int_hash
            type(key_type), intent(in)    :: key
            integer(int_hash)             :: hash_value
        end function hasher_fun_temp
    end interface

    type, abstract :: hashmap_type
!! Version: Experimental
!!
!! Type implementing an abstract hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-hashmap_type-abstract-type))
        !private
        integer(int_calls) :: call_count = 0
!! Number of calls
        integer(int_calls) :: probe_count = 0
!! Number of probes since last expansion
        integer(int_calls) :: total_probes = 0
!! Cumulative number of probes
        integer(int_index) :: num_entries = 0
!! Number of entries
        integer(int_index) :: num_free = 0
!! Number of elements in the free_list
        integer(int32)     :: nbits = default_bits
!! Number of bits used to address the slots
        procedure(hasher_fun_temp), pointer, nopass :: hasher => fnv_1_hasher
!! Hash function

    contains

        procedure, non_overridable, pass(map) :: calls
        procedure, non_overridable, pass(map) :: entries
        procedure, non_overridable, pass(map) :: map_probes
        procedure, non_overridable, pass(map) :: num_slots
        procedure, non_overridable, pass(map) :: slots_bits
    end type hashmap_type

!! API for the chaining_hashmap_type

    type :: chaining_map_entry_type  ! Hash entry
!! Version: Experimental
!!
!! Chaining hash map entry type
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-chaining_map_entry_type-derived-type))
        !private
        integer(int_hash)  :: hash_val
!! Full hash value
        type(key_type)     :: key
!! The entry's key
        type(other_type)   :: other
!! Other entry data
        integer(int_index) :: inmap
!! Index into inverse table
        type(chaining_map_entry_type), pointer :: next => null()
!! Next bucket
    end type chaining_map_entry_type


    type chaining_map_entry_ptr
!! Version: Experimental
!!
!! Wrapper for a pointer to a chaining map entry type object
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-chaining_map_entry_type_ptr-derived-type))
        type(chaining_map_entry_type), pointer :: target => null()
    end type chaining_map_entry_ptr


    type :: chaining_map_entry_pool
!! Version: Experimental
!!
!! Type implementing a pool of allocated `chaining_map_entry_type`
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-chaining_map_entry_pool-derived-type))
        !private
! Index of next bucket
        integer(int_index)                         :: next = 0
        type(chaining_map_entry_type), allocatable :: more_map_entries(:)
        type(chaining_map_entry_pool), pointer     :: lastpool => null()
    end type chaining_map_entry_pool


    type, extends(hashmap_type) :: chaining_hashmap_type
!! Version: Experimental
!!
!! Type implementing the `chaining_hashmap_type` types
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-chaining_hashmap_type-derived-type))
        !private
        type(chaining_map_entry_pool), pointer    :: cache => null()
!! Pool of allocated chaining_map_entry_type objects
        type(chaining_map_entry_type), pointer    :: free_list => null()
!! free list of map entries
        type(chaining_map_entry_ptr), allocatable :: inverse(:)
!! Array of bucket lists (inverses) Note max_elts=size(inverse)
        type(chaining_map_entry_ptr), allocatable :: slots(:)
!! Array of bucket lists Note # slots=size(slots)
    end type chaining_hashmap_type


!! API for the open_hashmap_type

    type :: open_map_entry_type
!! Version: Experimental
!!
!! Open hash map entry type
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-open_map_entry_type-derived-type))
        !private
        integer(int_hash) :: hash_val
!! Full hash value
        type(key_type)    :: key
!! Hash entry key
        type(other_type)  :: other
!! Other entry data
        integer(int_index) :: inmap
!! Index into inverse table
    end type open_map_entry_type

    type :: open_map_entry_list
!! Version: Experimental
!!
!! Open hash map entry type
        !private
        type(open_map_entry_type), pointer :: target => null()
        type(open_map_entry_list), pointer :: next => null()
    end type open_map_entry_list


    type open_map_entry_ptr
!! Version: Experimental
!!
!! Wrapper for a pointer to an open hash map entry type object
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-open_map_entry_ptr-derived-type))
        type(open_map_entry_type), pointer :: target => null()
    end type open_map_entry_ptr


    type :: open_map_entry_pool
!! Version: Experimental
!!
!! Type implementing a pool of allocated `open_map_entry_type`
        !private
        integer(int_index)                     :: next = 0
!! Index of next bucket
        type(open_map_entry_type), allocatable :: more_map_entries(:)
        type(open_map_entry_pool), pointer     :: lastpool => null()
    end type open_map_entry_pool


    type, extends(hashmap_type) :: open_hashmap_type
!! Version: Experimental
!!
!! Type implementing an "open" hash map
        !private
        integer(int_index) :: index_mask = 2_int_index**default_bits-1
!! Mask used in linear addressing
        type(open_map_entry_pool), pointer    :: cache => null()
!! Pool of allocated open_map_entry_type objects
        type(open_map_entry_list), pointer    :: free_list => null()
!! free list of map entries
        type(open_map_entry_ptr), allocatable  :: inverse(:)
!! Array of bucket lists (inverses) Note max_elts=size(inverse)
        integer(int_index), allocatable        :: slots(:)
!! Array of indices to the inverse Note # slots=size(slots)
    end type open_hashmap_type

contains

    pure function calls( map )
!! Version: Experimental
!!
!! Returns the number of subroutine calls on an open hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#calls-returns-the-number-of-calls-on-the-hash-map))
!!
!! Arguments:
!!     map - an open hash map
        class(hashmap_type), intent(in) :: map
        integer(int_calls)              :: calls

        calls = map % call_count

    end function calls

    pure function entries( map )
!! Version: Experimental
!!
!! Returns the number of entries in a hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#entries-returns-the-number-of-entries-in-the-hash-map))
!!
!! Arguments:
!!     map - an open hash map
        class(hashmap_type), intent(in) :: map
        integer(int_index) :: entries

        entries = map % num_entries

    end function entries


    pure function map_probes( map )
!! Version: Experimental
!!
!! Returns the total number of table probes on a hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#map_probes-returns-the-number-of-hash-map-probes))
!!
!! Arguments:
!!     map - an open hash map
        class(hashmap_type), intent(in) :: map
        integer(int_calls) :: map_probes

        map_probes = map % total_probes + map % probe_count

    end function map_probes


    pure function num_slots( map )
!! Version: Experimental
!!
!! Returns the number of allocated slots in a hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#num_slots-returns-the-number-of-hash-map-slots))
!!
!! Arguments:
!!     map - an open hash map
        class(hashmap_type), intent(in) :: map
        integer(int_index)              :: num_slots

        num_slots = 2**map % nbits

    end function num_slots


    pure function slots_bits( map )
!! Version: Experimental
!!
!! Returns the number of bits used to specify the number of allocated
!! slots in a hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#slots_bits-returns-the-number-of-bits-used-to-address-the-hash-map-slots))
!!
!! Arguments:
!!     map - an open hash map
        class(hashmap_type), intent(in) :: map
        integer                              :: slots_bits

        slots_bits = map % nbits

    end function slots_bits


end module stdlib_hashmaps
