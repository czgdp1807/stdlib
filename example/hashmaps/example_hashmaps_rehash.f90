program example_rehash
  use stdlib_kinds, only: int8
  use stdlib_hashmaps, only: open_hashmap_type
  use stdlib_hashmap_open
  use stdlib_hashmap_wrappers, only: fnv_1_hasher, fnv_1a_hasher, &
                                     key_type, other_type, set
  implicit none
  type(open_hashmap_type) :: map
  type(key_type)      :: key
  type(other_type)    :: other
  integer :: i
  class(*), allocatable :: dummy
  allocate (dummy, source='a dummy value')
  call init(map, fnv_1_hasher, slots_bits=10)
  call set(key, [5_int8, 7_int8, 4_int8, 13_int8])
  call set(other, dummy)
  do i= 1, 576
    allocate(prev_keys(i) % value(4))
  end do
  call map_open_entry(map, key, other)
  call rehash_open_map(map, fnv_1a_hasher)
end program example_rehash
