program example_loading
  use stdlib_hashmaps, only: open_hashmap_type
  use stdlib_hashmap_wrappers, only: fnv_1_hasher
  use stdlib_hashmap_open
  implicit none
  type(open_hashmap_type) :: map
  real :: ratio
  call init(map, fnv_1_hasher)
  ratio = open_loading(map)
  print *, "Initial loading =  ", ratio
end program example_loading
