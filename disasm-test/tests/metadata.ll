
; Some unnamed metadata nodes, which are referenced by the named metadata.
!0 = metadata !{ metadata !"zero" }
!1 = metadata !{ metadata !{ metadata !"three" }, metadata !2 }
!2 = metadata !{ metadata !"one"  }

; A named metadata.
!thinger = !{ !0, !1, !2 }

@val = global i32 10
