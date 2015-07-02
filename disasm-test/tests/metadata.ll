
; Some unnamed metadata nodes, which are referenced by the named metadata.
!0 = !{ !"zero" }
!1 = !{ !{ !"three" }, !2 }
!2 = !{ !"one"  }

; A named metadata.
!thinger = !{ !0, !1, !2 }

@val = global i32 10
