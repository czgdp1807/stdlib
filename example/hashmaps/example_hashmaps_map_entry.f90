program example_map_entry
  use, intrinsic:: iso_fortran_env, only: int8
  use stdlib_hashmaps, only: chaining_hashmap_type
  use stdlib_hashmap_chaining
  use stdlib_hashmap_wrappers, only: fnv_1_hasher, key_type, other_type, set
  type(chaining_hashmap_type) :: map
  type(key_type)      :: key
  logical             :: conflict
  type(other_type)    :: other
  integer :: i
  class(*), allocatable :: dummy
  allocate (dummy, source=4)
  call init(map, fnv_1_hasher, slots_bits=10)
  call set(key, [5_int8, 7_int8, 4_int8, 13_int8])
  call set(other, dummy)
  do i= 1, 1024
    allocate(prev_keys_inverse(i) % value(4))
  end do
  call map_chain_entry(map, key, other, conflict)
  print *, 'CONFLICT = ', conflict
end program example_map_entry
