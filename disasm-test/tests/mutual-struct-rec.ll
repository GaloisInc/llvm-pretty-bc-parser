
%struct.A = type { i32, %struct.B* }
%struct.B = type { i32, %struct.A* }

define %struct.A* @f(%struct.A* %ptr) {
  ret %struct.A* %ptr
}
