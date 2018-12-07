; ModuleID = 'merge.bc'
source_filename = "merge.cpp"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%"class.std::ios_base::Init" = type { i8 }
%"class.std::basic_ostream" = type { i32 (...)**, %"class.std::basic_ios" }
%"class.std::basic_ios" = type { %"class.std::ios_base", %"class.std::basic_ostream"*, i8, i8, %"class.std::basic_streambuf"*, %"class.std::ctype"*, %"class.std::num_put"*, %"class.std::num_get"* }
%"class.std::ios_base" = type { i32 (...)**, i64, i64, i32, i32, i32, %"struct.std::ios_base::_Callback_list"*, %"struct.std::ios_base::_Words", [8 x %"struct.std::ios_base::_Words"], i32, %"struct.std::ios_base::_Words"*, %"class.std::locale" }
%"struct.std::ios_base::_Callback_list" = type { %"struct.std::ios_base::_Callback_list"*, void (i32, %"class.std::ios_base"*, i32)*, i32, i32 }
%"struct.std::ios_base::_Words" = type { i8*, i64 }
%"class.std::locale" = type { %"class.std::locale::_Impl"* }
%"class.std::locale::_Impl" = type { i32, %"class.std::locale::facet"**, i64, %"class.std::locale::facet"**, i8** }
%"class.std::locale::facet" = type <{ i32 (...)**, i32, [4 x i8] }>
%"class.std::basic_streambuf" = type { i32 (...)**, i8*, i8*, i8*, i8*, i8*, i8*, %"class.std::locale" }
%"class.std::ctype" = type <{ %"class.std::locale::facet.base", [4 x i8], %struct.__locale_struct*, i8, [7 x i8], i32*, i32*, i16*, i8, [256 x i8], [256 x i8], i8, [6 x i8] }>
%"class.std::locale::facet.base" = type <{ i32 (...)**, i32 }>
%struct.__locale_struct = type { [13 x %struct.__locale_data*], i16*, i32*, i32*, [13 x i8*] }
%struct.__locale_data = type opaque
%"class.std::num_put" = type { %"class.std::locale::facet.base", [4 x i8] }
%"class.std::num_get" = type { %"class.std::locale::facet.base", [4 x i8] }
%"class.std::vector" = type { %"struct.std::_Vector_base" }
%"struct.std::_Vector_base" = type { %"struct.std::_Vector_base<int, std::allocator<int> >::_Vector_impl" }
%"struct.std::_Vector_base<int, std::allocator<int> >::_Vector_impl" = type { i32*, i32*, i32* }
%"class.std::allocator" = type { i8 }
%"class.__gnu_cxx::__normal_iterator" = type { i32* }

$_ZNSt6vectorIiSaIiEE17_M_realloc_insertIJRKiEEEvN9__gnu_cxx17__normal_iteratorIPiS1_EEDpOT_ = comdat any

@_ZStL8__ioinit = internal global %"class.std::ios_base::Init" zeroinitializer, align 1, !dbg !0
@__dso_handle = external hidden global i8
@_ZSt4cout = external global %"class.std::basic_ostream", align 8
@llvm.global_ctors = appending global [1 x { i32, void ()*, i8* }] [{ i32, void ()*, i8* } { i32 65535, void ()* @_GLOBAL__sub_I_merge.cpp, i8* null }]

declare void @_ZNSt8ios_base4InitC1Ev(%"class.std::ios_base::Init"*) unnamed_addr #0

; Function Attrs: nounwind
declare void @_ZNSt8ios_base4InitD1Ev(%"class.std::ios_base::Init"*) unnamed_addr #1

; Function Attrs: nounwind
declare i32 @__cxa_atexit(void (i8*)*, i8*, i8*) local_unnamed_addr #2

; Function Attrs: sspstrong uwtable
define void @_Z5mergeSt6vectorIiSaIiEES1_(%"class.std::vector"* noalias sret, %"class.std::vector"* nocapture readonly, %"class.std::vector"* nocapture readonly) local_unnamed_addr #3 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) !dbg !1544 {
  call void @llvm.dbg.declare(metadata %"class.std::vector"* %1, metadata !1548, metadata !DIExpression()), !dbg !1555
  call void @llvm.dbg.declare(metadata %"class.std::vector"* %2, metadata !1549, metadata !DIExpression()), !dbg !1556
  call void @llvm.dbg.declare(metadata %"class.std::vector"* %0, metadata !1550, metadata !DIExpression()), !dbg !1557
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1558, metadata !DIExpression()) #2, !dbg !1562
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1564, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !1568
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1570, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !1574
  %4 = bitcast %"class.std::vector"* %0 to i8*, !dbg !1576
  tail call void @llvm.memset.p0i8.i64(i8* %4, i8 0, i64 24, i32 8, i1 false) #2, !dbg !1577
  call void @llvm.dbg.value(metadata i32 0, metadata !1551, metadata !DIExpression()), !dbg !1578
  call void @llvm.dbg.value(metadata i32 0, metadata !1552, metadata !DIExpression()), !dbg !1579
  call void @llvm.dbg.value(metadata i64 0, metadata !1553, metadata !DIExpression()), !dbg !1580
  call void @llvm.dbg.value(metadata %"class.std::vector"* %1, metadata !1581, metadata !DIExpression()), !dbg !1585
  %5 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %1, i64 0, i32 0, i32 0, i32 1, !dbg !1588
  %6 = bitcast i32** %5 to i64*, !dbg !1588
  %7 = load i64, i64* %6, align 8, !dbg !1588, !tbaa !1589
  %8 = bitcast %"class.std::vector"* %1 to i64*, !dbg !1595
  %9 = load i64, i64* %8, align 8, !dbg !1595, !tbaa !1596
  %10 = sub i64 %7, %9, !dbg !1597
  %11 = ashr exact i64 %10, 2, !dbg !1597
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1581, metadata !DIExpression()), !dbg !1598
  %12 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %2, i64 0, i32 0, i32 0, i32 1, !dbg !1600
  %13 = bitcast i32** %12 to i64*, !dbg !1600
  %14 = load i64, i64* %13, align 8, !dbg !1600, !tbaa !1589
  %15 = bitcast %"class.std::vector"* %2 to i64*, !dbg !1601
  %16 = load i64, i64* %15, align 8, !dbg !1601, !tbaa !1596
  %17 = sub i64 %14, %16, !dbg !1602
  %18 = ashr exact i64 %17, 2, !dbg !1602
  %19 = sub nsw i64 0, %11, !dbg !1603
  %20 = icmp eq i64 %18, %19, !dbg !1603
  br i1 %20, label %79, label %21, !dbg !1604

; <label>:21:                                     ; preds = %3
  %22 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, i32 0, i32 1
  %23 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, i32 0, i32 2
  br label %24, !dbg !1604

; <label>:24:                                     ; preds = %76, %21
  %25 = phi i32* [ null, %21 ], [ %78, %76 ], !dbg !1605
  %26 = phi i32* [ null, %21 ], [ %77, %76 ], !dbg !1615
  %27 = phi i64 [ %16, %21 ], [ %71, %76 ]
  %28 = phi i64 [ %9, %21 ], [ %67, %76 ]
  %29 = phi i32 [ 0, %21 ], [ %64, %76 ]
  %30 = phi i64 [ 0, %21 ], [ %65, %76 ]
  %31 = phi i32 [ 0, %21 ], [ %63, %76 ]
  %32 = inttoptr i64 %28 to i32*, !dbg !1616
  %33 = inttoptr i64 %27 to i32*, !dbg !1622
  call void @llvm.dbg.value(metadata i32 %31, metadata !1552, metadata !DIExpression()), !dbg !1579
  call void @llvm.dbg.value(metadata i64 %30, metadata !1553, metadata !DIExpression()), !dbg !1580
  call void @llvm.dbg.value(metadata i32 %29, metadata !1551, metadata !DIExpression()), !dbg !1578
  %34 = sext i32 %29 to i64, !dbg !1624
  call void @llvm.dbg.value(metadata %"class.std::vector"* %1, metadata !1619, metadata !DIExpression()), !dbg !1625
  call void @llvm.dbg.value(metadata i64 %34, metadata !1620, metadata !DIExpression()), !dbg !1626
  %35 = getelementptr i32, i32* %32, i64 %34, !dbg !1627
  %36 = load i32, i32* %35, align 4, !dbg !1628, !tbaa !1629
  %37 = sext i32 %31 to i64, !dbg !1631
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1619, metadata !DIExpression()), !dbg !1632
  call void @llvm.dbg.value(metadata i64 %37, metadata !1620, metadata !DIExpression()), !dbg !1633
  %38 = getelementptr i32, i32* %33, i64 %37, !dbg !1634
  %39 = load i32, i32* %38, align 4, !dbg !1635, !tbaa !1629
  %40 = icmp sgt i32 %36, %39, !dbg !1636
  %41 = icmp eq i32* %26, %25, !dbg !1637
  br i1 %40, label %56, label %42, !dbg !1638

; <label>:42:                                     ; preds = %24
  call void @llvm.dbg.value(metadata %"class.std::vector"* %1, metadata !1619, metadata !DIExpression()), !dbg !1639
  call void @llvm.dbg.value(metadata i64 %34, metadata !1620, metadata !DIExpression()), !dbg !1642
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1609, metadata !DIExpression()), !dbg !1643
  call void @llvm.dbg.value(metadata i32* %35, metadata !1610, metadata !DIExpression()), !dbg !1645
  br i1 %41, label %45, label %43, !dbg !1646

; <label>:43:                                     ; preds = %42
  call void @llvm.dbg.value(metadata i32* %26, metadata !1647, metadata !DIExpression()), !dbg !1660
  call void @llvm.dbg.value(metadata i32* %35, metadata !1659, metadata !DIExpression()), !dbg !1663
  call void @llvm.dbg.value(metadata i32* %26, metadata !1664, metadata !DIExpression()), !dbg !1673
  call void @llvm.dbg.value(metadata i32* %35, metadata !1672, metadata !DIExpression()), !dbg !1675
  store i32 %36, i32* %26, align 4, !dbg !1676, !tbaa !1629
  %44 = getelementptr i32, i32* %26, i64 1, !dbg !1677
  store i32* %44, i32** %22, align 8, !dbg !1677, !tbaa !1589
  br label %46, !dbg !1678

; <label>:45:                                     ; preds = %42
  invoke void @_ZNSt6vectorIiSaIiEE17_M_realloc_insertIJRKiEEEvN9__gnu_cxx17__normal_iteratorIPiS1_EEDpOT_(%"class.std::vector"* nonnull %0, i32* %25, i32* nonnull dereferenceable(4) %35)
          to label %46 unwind label %48, !dbg !1679

; <label>:46:                                     ; preds = %43, %45
  %47 = add i32 %29, 1, !dbg !1680
  call void @llvm.dbg.value(metadata i32 %47, metadata !1551, metadata !DIExpression()), !dbg !1578
  br label %62, !dbg !1681

; <label>:48:                                     ; preds = %45, %59
  %49 = landingpad { i8*, i32 }
          cleanup, !dbg !1682
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1683, metadata !DIExpression()) #2, !dbg !1686
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !1691
  %50 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, i32 0, i32 0, !dbg !1694
  %51 = load i32*, i32** %50, align 8, !dbg !1694, !tbaa !1596
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !1701
  call void @llvm.dbg.value(metadata i32* %51, metadata !1699, metadata !DIExpression()) #2, !dbg !1703
  %52 = icmp eq i32* %51, null, !dbg !1704
  br i1 %52, label %55, label %53, !dbg !1706

; <label>:53:                                     ; preds = %48
  call void @llvm.dbg.value(metadata i32* %51, metadata !1707, metadata !DIExpression()) #2, !dbg !1712
  call void @llvm.dbg.value(metadata i32* %51, metadata !1714, metadata !DIExpression()) #2, !dbg !1719
  %54 = bitcast i32* %51 to i8*, !dbg !1721
  tail call void @_ZdlPv(i8* %54) #2, !dbg !1722
  br label %55, !dbg !1723

; <label>:55:                                     ; preds = %48, %53
  resume { i8*, i32 } %49, !dbg !1724

; <label>:56:                                     ; preds = %24
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1619, metadata !DIExpression()), !dbg !1725
  call void @llvm.dbg.value(metadata i64 %37, metadata !1620, metadata !DIExpression()), !dbg !1727
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1609, metadata !DIExpression()), !dbg !1728
  call void @llvm.dbg.value(metadata i32* %38, metadata !1610, metadata !DIExpression()), !dbg !1729
  br i1 %41, label %59, label %57, !dbg !1730

; <label>:57:                                     ; preds = %56
  call void @llvm.dbg.value(metadata i32* %26, metadata !1647, metadata !DIExpression()), !dbg !1731
  call void @llvm.dbg.value(metadata i32* %38, metadata !1659, metadata !DIExpression()), !dbg !1733
  call void @llvm.dbg.value(metadata i32* %26, metadata !1664, metadata !DIExpression()), !dbg !1734
  call void @llvm.dbg.value(metadata i32* %38, metadata !1672, metadata !DIExpression()), !dbg !1736
  store i32 %39, i32* %26, align 4, !dbg !1737, !tbaa !1629
  %58 = getelementptr i32, i32* %26, i64 1, !dbg !1738
  store i32* %58, i32** %22, align 8, !dbg !1738, !tbaa !1589
  br label %60, !dbg !1739

; <label>:59:                                     ; preds = %56
  invoke void @_ZNSt6vectorIiSaIiEE17_M_realloc_insertIJRKiEEEvN9__gnu_cxx17__normal_iteratorIPiS1_EEDpOT_(%"class.std::vector"* nonnull %0, i32* %25, i32* nonnull dereferenceable(4) %38)
          to label %60 unwind label %48, !dbg !1740

; <label>:60:                                     ; preds = %57, %59
  %61 = add i32 %31, 1, !dbg !1741
  call void @llvm.dbg.value(metadata i32 %61, metadata !1552, metadata !DIExpression()), !dbg !1579
  br label %62

; <label>:62:                                     ; preds = %46, %60
  %63 = phi i32 [ %31, %46 ], [ %61, %60 ]
  %64 = phi i32 [ %47, %46 ], [ %29, %60 ]
  %65 = add nuw i64 %30, 1, !dbg !1742
  call void @llvm.dbg.value(metadata i32 %64, metadata !1551, metadata !DIExpression()), !dbg !1578
  call void @llvm.dbg.value(metadata i64 %65, metadata !1553, metadata !DIExpression()), !dbg !1580
  call void @llvm.dbg.value(metadata i32 %63, metadata !1552, metadata !DIExpression()), !dbg !1579
  call void @llvm.dbg.value(metadata %"class.std::vector"* %1, metadata !1581, metadata !DIExpression()), !dbg !1585
  %66 = load i64, i64* %6, align 8, !dbg !1588, !tbaa !1589
  %67 = load i64, i64* %8, align 8, !dbg !1595, !tbaa !1596
  %68 = sub i64 %66, %67, !dbg !1597
  %69 = ashr exact i64 %68, 2, !dbg !1597
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1581, metadata !DIExpression()), !dbg !1598
  %70 = load i64, i64* %13, align 8, !dbg !1600, !tbaa !1589
  %71 = load i64, i64* %15, align 8, !dbg !1601, !tbaa !1596
  %72 = sub i64 %70, %71, !dbg !1602
  %73 = ashr exact i64 %72, 2, !dbg !1602
  %74 = add nsw i64 %73, %69, !dbg !1743
  %75 = icmp ult i64 %65, %74, !dbg !1603
  br i1 %75, label %76, label %79, !dbg !1604, !llvm.loop !1744

; <label>:76:                                     ; preds = %62
  %77 = load i32*, i32** %22, align 8, !dbg !1615, !tbaa !1589
  %78 = load i32*, i32** %23, align 8, !dbg !1605, !tbaa !1746
  br label %24, !dbg !1604

; <label>:79:                                     ; preds = %62, %3
  ret void, !dbg !1724
}

; Function Attrs: nounwind readnone speculatable
declare void @llvm.dbg.declare(metadata, metadata, metadata) #4

declare i32 @__gxx_personality_v0(...)

; Function Attrs: norecurse sspstrong uwtable
define i32 @main() local_unnamed_addr #5 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) !dbg !1747 {
  %1 = alloca %"class.std::vector", align 8
  %2 = alloca %"class.std::vector", align 8
  %3 = alloca %"class.std::vector", align 8
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1751, metadata !DIExpression()), !dbg !1756
  call void @llvm.dbg.value(metadata i64 2, metadata !1754, metadata !DIExpression()), !dbg !1758
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !1755, metadata !DIExpression()), !dbg !1759
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1760, metadata !DIExpression(DW_OP_stack_value)), !dbg !1765
  call void @llvm.dbg.value(metadata i64 2, metadata !1763, metadata !DIExpression()), !dbg !1767
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !1764, metadata !DIExpression()), !dbg !1768
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1769, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !1773
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !1772, metadata !DIExpression()) #2, !dbg !1775
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1776, metadata !DIExpression(DW_OP_stack_value)), !dbg !1780
  call void @llvm.dbg.value(metadata i64 2, metadata !1779, metadata !DIExpression()), !dbg !1783
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1784, metadata !DIExpression(DW_OP_stack_value)), !dbg !1788
  call void @llvm.dbg.value(metadata i64 2, metadata !1787, metadata !DIExpression()), !dbg !1790
  call void @llvm.dbg.value(metadata i64 2, metadata !1791, metadata !DIExpression()), !dbg !1795
  call void @llvm.dbg.value(metadata i64 2, metadata !1797, metadata !DIExpression()), !dbg !1802
  call void @llvm.dbg.value(metadata i8* null, metadata !1801, metadata !DIExpression()), !dbg !1804
  %4 = invoke i8* @_Znwm(i64 8)
          to label %5 unwind label %59, !dbg !1805

; <label>:5:                                      ; preds = %0
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1806, metadata !DIExpression()), !dbg !1810
  call void @llvm.dbg.value(metadata i64 2, metadata !1809, metadata !DIExpression()), !dbg !1813
  call void @llvm.dbg.value(metadata i8* %4, metadata !1814, metadata !DIExpression()), !dbg !1825
  call void @llvm.dbg.value(metadata i64 2, metadata !1820, metadata !DIExpression()), !dbg !1827
  call void @llvm.dbg.value(metadata i8* %4, metadata !1828, metadata !DIExpression()), !dbg !1836
  call void @llvm.dbg.value(metadata i64 2, metadata !1833, metadata !DIExpression()), !dbg !1838
  call void @llvm.dbg.value(metadata i8 1, metadata !1834, metadata !DIExpression()), !dbg !1839
  call void @llvm.dbg.value(metadata i8* %4, metadata !1840, metadata !DIExpression()), !dbg !1848
  call void @llvm.dbg.value(metadata i64 2, metadata !1847, metadata !DIExpression()), !dbg !1850
  call void @llvm.dbg.value(metadata i8* %4, metadata !1851, metadata !DIExpression()), !dbg !1861
  call void @llvm.dbg.value(metadata i64 2, metadata !1857, metadata !DIExpression()), !dbg !1863
  call void @llvm.dbg.value(metadata i8* %4, metadata !1864, metadata !DIExpression()), !dbg !1882
  call void @llvm.dbg.value(metadata i64 2, metadata !1875, metadata !DIExpression()), !dbg !1884
  call void @llvm.dbg.value(metadata i32 0, metadata !1877, metadata !DIExpression()), !dbg !1885
  call void @llvm.dbg.value(metadata i64 2, metadata !1878, metadata !DIExpression()), !dbg !1886
  call void @llvm.dbg.value(metadata i8* %4, metadata !1864, metadata !DIExpression()), !dbg !1882
  %6 = bitcast i8* %4 to i64*, !dbg !1887
  store i64 0, i64* %6, align 4, !dbg !1887
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1889, metadata !DIExpression()), !dbg !1893
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1895, metadata !DIExpression()), !dbg !1906
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1908, metadata !DIExpression()), !dbg !1921
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1924, metadata !DIExpression()), !dbg !1930
  call void @llvm.dbg.value(metadata i64 1, metadata !1927, metadata !DIExpression()), !dbg !1932
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1581, metadata !DIExpression()), !dbg !1933
  call void @llvm.dbg.value(metadata i64 1, metadata !1927, metadata !DIExpression()), !dbg !1932
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1581, metadata !DIExpression()), !dbg !1936
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1581, metadata !DIExpression()), !dbg !1938
  call void @llvm.dbg.value(metadata i64 4, metadata !1929, metadata !DIExpression()), !dbg !1940
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1581, metadata !DIExpression()), !dbg !1941
  call void @llvm.dbg.value(metadata i64 4, metadata !1916, metadata !DIExpression()), !dbg !1943
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !1914, metadata !DIExpression()), !dbg !1944
  call void @llvm.dbg.value(metadata i64 2, metadata !1918, metadata !DIExpression()), !dbg !1945
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1784, metadata !DIExpression(DW_OP_stack_value)), !dbg !1946
  call void @llvm.dbg.value(metadata i64 4, metadata !1787, metadata !DIExpression()), !dbg !1948
  call void @llvm.dbg.value(metadata i64 4, metadata !1791, metadata !DIExpression()), !dbg !1949
  call void @llvm.dbg.value(metadata i64 4, metadata !1797, metadata !DIExpression()), !dbg !1951
  call void @llvm.dbg.value(metadata i8* null, metadata !1801, metadata !DIExpression()), !dbg !1953
  %7 = invoke i8* @_Znwm(i64 16)
          to label %8 unwind label %63, !dbg !1954

; <label>:8:                                      ; preds = %5
  call void @llvm.dbg.value(metadata i8* %7, metadata !1919, metadata !DIExpression()), !dbg !1955
  call void @llvm.dbg.value(metadata i8* %7, metadata !1920, metadata !DIExpression()), !dbg !1956
  %9 = getelementptr i8, i8* %7, i64 8, !dbg !1957
  %10 = bitcast i8* %9 to i32*, !dbg !1957
  call void @llvm.dbg.value(metadata i32* %10, metadata !1959, metadata !DIExpression()), !dbg !1968
  call void @llvm.dbg.value(metadata i32* %10, metadata !1970, metadata !DIExpression()), !dbg !1978
  store i32 0, i32* %10, align 4, !dbg !1980, !tbaa !1629
  call void @llvm.dbg.value(metadata i32* null, metadata !1920, metadata !DIExpression()), !dbg !1956
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !1914, metadata !DIExpression()), !dbg !1944
  call void @llvm.dbg.value(metadata i8* %4, metadata !1981, metadata !DIExpression()), !dbg !1992
  call void @llvm.dbg.value(metadata i8* %4, metadata !1986, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !1994
  call void @llvm.dbg.value(metadata i8* %7, metadata !1987, metadata !DIExpression()), !dbg !1995
  call void @llvm.dbg.value(metadata i8* %4, metadata !1996, metadata !DIExpression()), !dbg !2006
  call void @llvm.dbg.value(metadata i8* %4, metadata !2001, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2008
  call void @llvm.dbg.value(metadata i8* %7, metadata !2002, metadata !DIExpression()), !dbg !2009
  call void @llvm.dbg.value(metadata i8* %4, metadata !2010, metadata !DIExpression()), !dbg !2019
  call void @llvm.dbg.value(metadata i8* %4, metadata !2015, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2021
  call void @llvm.dbg.value(metadata i8* %7, metadata !2016, metadata !DIExpression()), !dbg !2022
  call void @llvm.dbg.value(metadata i8 1, metadata !2017, metadata !DIExpression()), !dbg !2023
  call void @llvm.dbg.value(metadata i8* %4, metadata !2024, metadata !DIExpression()), !dbg !2033
  call void @llvm.dbg.value(metadata i8* %4, metadata !2031, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2035
  call void @llvm.dbg.value(metadata i8* %7, metadata !2032, metadata !DIExpression()), !dbg !2036
  call void @llvm.dbg.value(metadata i8* %4, metadata !2037, metadata !DIExpression()), !dbg !2044
  call void @llvm.dbg.value(metadata i8* %4, metadata !2040, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2046
  call void @llvm.dbg.value(metadata i8* %7, metadata !2041, metadata !DIExpression()), !dbg !2047
  call void @llvm.dbg.value(metadata i8* %4, metadata !2048, metadata !DIExpression()), !dbg !2058
  call void @llvm.dbg.value(metadata i8* %4, metadata !2053, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2060
  call void @llvm.dbg.value(metadata i8* %7, metadata !2054, metadata !DIExpression()), !dbg !2061
  call void @llvm.dbg.value(metadata i8* %4, metadata !2062, metadata !DIExpression()), !dbg !2068
  call void @llvm.dbg.value(metadata i8* %4, metadata !2065, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2070
  call void @llvm.dbg.value(metadata i8* %7, metadata !2066, metadata !DIExpression()), !dbg !2071
  call void @llvm.dbg.value(metadata i8 1, metadata !2067, metadata !DIExpression()), !dbg !2072
  call void @llvm.dbg.value(metadata i8* %4, metadata !2073, metadata !DIExpression()) #2, !dbg !2096
  call void @llvm.dbg.value(metadata i8* %4, metadata !2092, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)) #2, !dbg !2098
  call void @llvm.dbg.value(metadata i8* %7, metadata !2093, metadata !DIExpression()) #2, !dbg !2099
  call void @llvm.dbg.value(metadata i64 2, metadata !2094, metadata !DIExpression()) #2, !dbg !2100
  %11 = bitcast i8* %7 to i64*, !dbg !2101
  %12 = load i64, i64* %6, align 4, !dbg !2101
  store i64 %12, i64* %11, align 4, !dbg !2101
  call void @llvm.dbg.value(metadata i8* %7, metadata !1920, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !1956
  call void @llvm.dbg.value(metadata i32* %15, metadata !1920, metadata !DIExpression()), !dbg !1956
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !1914, metadata !DIExpression()), !dbg !1944
  call void @llvm.dbg.value(metadata i8* %4, metadata !1981, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2103
  call void @llvm.dbg.value(metadata i32** undef, metadata !1986, metadata !DIExpression(DW_OP_deref)), !dbg !2105
  call void @llvm.dbg.value(metadata i32* %15, metadata !1987, metadata !DIExpression()), !dbg !2106
  call void @llvm.dbg.value(metadata i8* %4, metadata !1996, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2107
  call void @llvm.dbg.value(metadata i32** undef, metadata !2001, metadata !DIExpression(DW_OP_deref)), !dbg !2109
  call void @llvm.dbg.value(metadata i32* %15, metadata !2002, metadata !DIExpression()), !dbg !2110
  call void @llvm.dbg.value(metadata i8* %4, metadata !2010, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2111
  call void @llvm.dbg.value(metadata i32** undef, metadata !2015, metadata !DIExpression(DW_OP_deref)), !dbg !2113
  call void @llvm.dbg.value(metadata i32* %15, metadata !2016, metadata !DIExpression()), !dbg !2114
  call void @llvm.dbg.value(metadata i8 1, metadata !2017, metadata !DIExpression()), !dbg !2115
  call void @llvm.dbg.value(metadata i8* %4, metadata !2024, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2116
  call void @llvm.dbg.value(metadata i32** undef, metadata !2031, metadata !DIExpression(DW_OP_deref)), !dbg !2118
  call void @llvm.dbg.value(metadata i32* %15, metadata !2032, metadata !DIExpression()), !dbg !2119
  call void @llvm.dbg.value(metadata i8* %4, metadata !2037, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2120
  call void @llvm.dbg.value(metadata i32** undef, metadata !2040, metadata !DIExpression(DW_OP_deref)), !dbg !2122
  call void @llvm.dbg.value(metadata i32* %15, metadata !2041, metadata !DIExpression()), !dbg !2123
  call void @llvm.dbg.value(metadata i8* %4, metadata !2048, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2124
  call void @llvm.dbg.value(metadata i32** undef, metadata !2053, metadata !DIExpression(DW_OP_deref)), !dbg !2126
  call void @llvm.dbg.value(metadata i32* %15, metadata !2054, metadata !DIExpression()), !dbg !2127
  call void @llvm.dbg.value(metadata i8* %4, metadata !2062, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)), !dbg !2128
  call void @llvm.dbg.value(metadata i32** undef, metadata !2065, metadata !DIExpression(DW_OP_deref)), !dbg !2130
  call void @llvm.dbg.value(metadata i32* %15, metadata !2066, metadata !DIExpression()), !dbg !2131
  call void @llvm.dbg.value(metadata i8 1, metadata !2067, metadata !DIExpression()), !dbg !2132
  call void @llvm.dbg.value(metadata i8* %4, metadata !2073, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)) #2, !dbg !2133
  call void @llvm.dbg.value(metadata i32** undef, metadata !2092, metadata !DIExpression(DW_OP_deref)) #2, !dbg !2135
  call void @llvm.dbg.value(metadata i32* %15, metadata !2093, metadata !DIExpression()) #2, !dbg !2136
  call void @llvm.dbg.value(metadata i64 0, metadata !2094, metadata !DIExpression()) #2, !dbg !2137
  call void @llvm.dbg.value(metadata i32* %15, metadata !1920, metadata !DIExpression()), !dbg !1956
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1696, metadata !DIExpression(DW_OP_stack_value)), !dbg !2138
  call void @llvm.dbg.value(metadata i8* %4, metadata !1699, metadata !DIExpression()), !dbg !2140
  call void @llvm.dbg.value(metadata i8* %4, metadata !1707, metadata !DIExpression()), !dbg !2141
  call void @llvm.dbg.value(metadata i8* %4, metadata !1714, metadata !DIExpression()) #2, !dbg !2143
  tail call void @_ZdlPv(i8* nonnull %4) #2, !dbg !2145
  %13 = ptrtoint i8* %7 to i64, !dbg !2146
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1889, metadata !DIExpression()), !dbg !2147
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1895, metadata !DIExpression()), !dbg !2149
  %14 = getelementptr i8, i8* %7, i64 12, !dbg !2151
  %15 = bitcast i8* %14 to i32*, !dbg !2151
  call void @llvm.dbg.value(metadata i32* %15, metadata !1959, metadata !DIExpression()), !dbg !2152
  call void @llvm.dbg.value(metadata i32* %15, metadata !1970, metadata !DIExpression()), !dbg !2155
  store i32 1, i32* %15, align 4, !dbg !2157, !tbaa !1629
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1751, metadata !DIExpression()), !dbg !2158
  call void @llvm.dbg.value(metadata i64 2, metadata !1754, metadata !DIExpression()), !dbg !2160
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !1755, metadata !DIExpression()), !dbg !2161
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1760, metadata !DIExpression(DW_OP_stack_value)), !dbg !2162
  call void @llvm.dbg.value(metadata i64 2, metadata !1763, metadata !DIExpression()), !dbg !2164
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !1764, metadata !DIExpression()), !dbg !2165
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1769, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2166
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !1772, metadata !DIExpression()) #2, !dbg !2168
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1776, metadata !DIExpression(DW_OP_stack_value)), !dbg !2169
  call void @llvm.dbg.value(metadata i64 2, metadata !1779, metadata !DIExpression()), !dbg !2171
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1784, metadata !DIExpression(DW_OP_stack_value)), !dbg !2172
  call void @llvm.dbg.value(metadata i64 2, metadata !1787, metadata !DIExpression()), !dbg !2174
  call void @llvm.dbg.value(metadata i64 2, metadata !1791, metadata !DIExpression()), !dbg !2175
  call void @llvm.dbg.value(metadata i64 2, metadata !1797, metadata !DIExpression()), !dbg !2177
  call void @llvm.dbg.value(metadata i8* null, metadata !1801, metadata !DIExpression()), !dbg !2179
  %16 = invoke i8* @_Znwm(i64 8)
          to label %17 unwind label %68, !dbg !2180

; <label>:17:                                     ; preds = %8
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1806, metadata !DIExpression()), !dbg !2181
  call void @llvm.dbg.value(metadata i64 2, metadata !1809, metadata !DIExpression()), !dbg !2183
  call void @llvm.dbg.value(metadata i8* %16, metadata !1814, metadata !DIExpression()), !dbg !2184
  call void @llvm.dbg.value(metadata i64 2, metadata !1820, metadata !DIExpression()), !dbg !2186
  call void @llvm.dbg.value(metadata i8* %16, metadata !1828, metadata !DIExpression()), !dbg !2187
  call void @llvm.dbg.value(metadata i64 2, metadata !1833, metadata !DIExpression()), !dbg !2189
  call void @llvm.dbg.value(metadata i8 1, metadata !1834, metadata !DIExpression()), !dbg !2190
  call void @llvm.dbg.value(metadata i8* %16, metadata !1840, metadata !DIExpression()), !dbg !2191
  call void @llvm.dbg.value(metadata i64 2, metadata !1847, metadata !DIExpression()), !dbg !2193
  call void @llvm.dbg.value(metadata i8* %16, metadata !1851, metadata !DIExpression()), !dbg !2194
  call void @llvm.dbg.value(metadata i64 2, metadata !1857, metadata !DIExpression()), !dbg !2196
  call void @llvm.dbg.value(metadata i8* %16, metadata !1864, metadata !DIExpression()), !dbg !2197
  call void @llvm.dbg.value(metadata i64 2, metadata !1875, metadata !DIExpression()), !dbg !2199
  call void @llvm.dbg.value(metadata i32 0, metadata !1877, metadata !DIExpression()), !dbg !2200
  call void @llvm.dbg.value(metadata i64 2, metadata !1878, metadata !DIExpression()), !dbg !2201
  call void @llvm.dbg.value(metadata i8* %16, metadata !1864, metadata !DIExpression()), !dbg !2197
  %18 = bitcast i8* %16 to i64*, !dbg !2202
  store i64 0, i64* %18, align 4, !dbg !2202
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1889, metadata !DIExpression()), !dbg !2203
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1895, metadata !DIExpression()), !dbg !2205
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1908, metadata !DIExpression()), !dbg !2207
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1924, metadata !DIExpression()), !dbg !2209
  call void @llvm.dbg.value(metadata i64 1, metadata !1927, metadata !DIExpression()), !dbg !2211
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1581, metadata !DIExpression()), !dbg !2212
  call void @llvm.dbg.value(metadata i64 1, metadata !1927, metadata !DIExpression()), !dbg !2211
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1581, metadata !DIExpression()), !dbg !2214
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1581, metadata !DIExpression()), !dbg !2216
  call void @llvm.dbg.value(metadata i64 8, metadata !1929, metadata !DIExpression()), !dbg !2218
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1581, metadata !DIExpression()), !dbg !2219
  call void @llvm.dbg.value(metadata i64 8, metadata !1916, metadata !DIExpression()), !dbg !2221
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !1914, metadata !DIExpression()), !dbg !2222
  call void @llvm.dbg.value(metadata i64 4, metadata !1918, metadata !DIExpression()), !dbg !2223
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1784, metadata !DIExpression(DW_OP_stack_value)), !dbg !2224
  call void @llvm.dbg.value(metadata i64 8, metadata !1787, metadata !DIExpression()), !dbg !2226
  call void @llvm.dbg.value(metadata i64 8, metadata !1791, metadata !DIExpression()), !dbg !2227
  call void @llvm.dbg.value(metadata i64 8, metadata !1797, metadata !DIExpression()), !dbg !2229
  call void @llvm.dbg.value(metadata i8* null, metadata !1801, metadata !DIExpression()), !dbg !2231
  %19 = invoke i8* @_Znwm(i64 32)
          to label %20 unwind label %72, !dbg !2232

; <label>:20:                                     ; preds = %17
  call void @llvm.dbg.value(metadata i8* %19, metadata !1919, metadata !DIExpression()), !dbg !2233
  call void @llvm.dbg.value(metadata i8* %19, metadata !1920, metadata !DIExpression()), !dbg !2234
  %21 = getelementptr i8, i8* %19, i64 16, !dbg !2235
  %22 = bitcast i8* %21 to i32*, !dbg !2235
  call void @llvm.dbg.value(metadata i32* %22, metadata !1959, metadata !DIExpression()), !dbg !2236
  call void @llvm.dbg.value(metadata i32* %22, metadata !1970, metadata !DIExpression()), !dbg !2238
  store i32 0, i32* %22, align 4, !dbg !2240, !tbaa !1629
  call void @llvm.dbg.value(metadata i32* null, metadata !1920, metadata !DIExpression()), !dbg !2234
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !1914, metadata !DIExpression()), !dbg !2222
  call void @llvm.dbg.value(metadata i64 %13, metadata !1981, metadata !DIExpression()), !dbg !2241
  call void @llvm.dbg.value(metadata i8* %19, metadata !1987, metadata !DIExpression()), !dbg !2243
  call void @llvm.dbg.value(metadata i64 %13, metadata !1996, metadata !DIExpression()), !dbg !2244
  call void @llvm.dbg.value(metadata i8* %19, metadata !2002, metadata !DIExpression()), !dbg !2246
  call void @llvm.dbg.value(metadata i64 %13, metadata !2010, metadata !DIExpression()), !dbg !2247
  call void @llvm.dbg.value(metadata i8* %19, metadata !2016, metadata !DIExpression()), !dbg !2249
  call void @llvm.dbg.value(metadata i8 1, metadata !2017, metadata !DIExpression()), !dbg !2250
  call void @llvm.dbg.value(metadata i64 %13, metadata !2024, metadata !DIExpression()), !dbg !2251
  call void @llvm.dbg.value(metadata i8* %19, metadata !2032, metadata !DIExpression()), !dbg !2253
  call void @llvm.dbg.value(metadata i64 %13, metadata !2037, metadata !DIExpression()), !dbg !2254
  call void @llvm.dbg.value(metadata i8* %19, metadata !2041, metadata !DIExpression()), !dbg !2256
  call void @llvm.dbg.value(metadata i64 %13, metadata !2048, metadata !DIExpression()), !dbg !2257
  call void @llvm.dbg.value(metadata i8* %19, metadata !2054, metadata !DIExpression()), !dbg !2259
  call void @llvm.dbg.value(metadata i64 %13, metadata !2062, metadata !DIExpression()), !dbg !2260
  call void @llvm.dbg.value(metadata i8* %19, metadata !2066, metadata !DIExpression()), !dbg !2262
  call void @llvm.dbg.value(metadata i8 1, metadata !2067, metadata !DIExpression()), !dbg !2263
  call void @llvm.dbg.value(metadata i64 %13, metadata !2073, metadata !DIExpression()) #2, !dbg !2264
  call void @llvm.dbg.value(metadata i8* %19, metadata !2093, metadata !DIExpression()) #2, !dbg !2266
  call void @llvm.dbg.value(metadata i64 4, metadata !2094, metadata !DIExpression()) #2, !dbg !2267
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* nonnull %19, i8* nonnull %7, i64 16, i32 4, i1 false) #2, !dbg !2268
  call void @llvm.dbg.value(metadata i32* %22, metadata !1920, metadata !DIExpression()), !dbg !2234
  call void @llvm.dbg.value(metadata i32* %22, metadata !1920, metadata !DIExpression(DW_OP_plus_uconst, 4, DW_OP_stack_value)), !dbg !2234
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !1914, metadata !DIExpression()), !dbg !2222
  call void @llvm.dbg.value(metadata i32** undef, metadata !1986, metadata !DIExpression(DW_OP_deref)), !dbg !2269
  call void @llvm.dbg.value(metadata i32* %22, metadata !1987, metadata !DIExpression(DW_OP_plus_uconst, 4, DW_OP_stack_value)), !dbg !2271
  call void @llvm.dbg.value(metadata i32** undef, metadata !2001, metadata !DIExpression(DW_OP_deref)), !dbg !2272
  call void @llvm.dbg.value(metadata i32* %22, metadata !2002, metadata !DIExpression(DW_OP_plus_uconst, 4, DW_OP_stack_value)), !dbg !2274
  call void @llvm.dbg.value(metadata i32** undef, metadata !2015, metadata !DIExpression(DW_OP_deref)), !dbg !2275
  call void @llvm.dbg.value(metadata i32* %22, metadata !2016, metadata !DIExpression(DW_OP_plus_uconst, 4, DW_OP_stack_value)), !dbg !2277
  call void @llvm.dbg.value(metadata i8 1, metadata !2017, metadata !DIExpression()), !dbg !2278
  call void @llvm.dbg.value(metadata i32** undef, metadata !2031, metadata !DIExpression(DW_OP_deref)), !dbg !2279
  call void @llvm.dbg.value(metadata i32* %22, metadata !2032, metadata !DIExpression(DW_OP_plus_uconst, 4, DW_OP_stack_value)), !dbg !2281
  call void @llvm.dbg.value(metadata i32** undef, metadata !2040, metadata !DIExpression(DW_OP_deref)), !dbg !2282
  call void @llvm.dbg.value(metadata i32* %22, metadata !2041, metadata !DIExpression(DW_OP_plus_uconst, 4, DW_OP_stack_value)), !dbg !2284
  call void @llvm.dbg.value(metadata i32** undef, metadata !2053, metadata !DIExpression(DW_OP_deref)), !dbg !2285
  call void @llvm.dbg.value(metadata i32* %22, metadata !2054, metadata !DIExpression(DW_OP_plus_uconst, 4, DW_OP_stack_value)), !dbg !2287
  call void @llvm.dbg.value(metadata i32** undef, metadata !2065, metadata !DIExpression(DW_OP_deref)), !dbg !2288
  call void @llvm.dbg.value(metadata i32* %22, metadata !2066, metadata !DIExpression(DW_OP_plus_uconst, 4, DW_OP_stack_value)), !dbg !2290
  call void @llvm.dbg.value(metadata i8 1, metadata !2067, metadata !DIExpression()), !dbg !2291
  call void @llvm.dbg.value(metadata i32** undef, metadata !2092, metadata !DIExpression(DW_OP_deref)) #2, !dbg !2292
  call void @llvm.dbg.value(metadata i32* %22, metadata !2093, metadata !DIExpression(DW_OP_plus_uconst, 4, DW_OP_stack_value)) #2, !dbg !2294
  call void @llvm.dbg.value(metadata i64 0, metadata !2094, metadata !DIExpression()) #2, !dbg !2295
  call void @llvm.dbg.value(metadata i32* %22, metadata !1920, metadata !DIExpression(DW_OP_plus_uconst, 4, DW_OP_stack_value)), !dbg !2234
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1696, metadata !DIExpression(DW_OP_stack_value)), !dbg !2296
  call void @llvm.dbg.value(metadata i64 %13, metadata !1699, metadata !DIExpression()), !dbg !2298
  call void @llvm.dbg.value(metadata i64 %13, metadata !1707, metadata !DIExpression()), !dbg !2299
  call void @llvm.dbg.value(metadata i64 %13, metadata !1714, metadata !DIExpression()) #2, !dbg !2301
  tail call void @_ZdlPv(i8* nonnull %7) #2, !dbg !2303
  %23 = ptrtoint i8* %19 to i64, !dbg !2304
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1889, metadata !DIExpression()), !dbg !2305
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1895, metadata !DIExpression()), !dbg !2307
  %24 = getelementptr i8, i8* %19, i64 20
  %25 = bitcast i8* %24 to i32*
  call void @llvm.dbg.value(metadata i32* %25, metadata !1959, metadata !DIExpression()), !dbg !2309
  call void @llvm.dbg.value(metadata i32* %25, metadata !1970, metadata !DIExpression()), !dbg !2311
  store i32 3, i32* %25, align 4, !dbg !2313, !tbaa !1629
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !2314, metadata !DIExpression()), !dbg !2318
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !2317, metadata !DIExpression()), !dbg !2320
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1581, metadata !DIExpression()), !dbg !2321
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1760, metadata !DIExpression(DW_OP_stack_value)), !dbg !2323
  call void @llvm.dbg.value(metadata i64 6, metadata !1763, metadata !DIExpression()), !dbg !2325
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !1764, metadata !DIExpression()), !dbg !2326
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1769, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2327
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !1772, metadata !DIExpression()) #2, !dbg !2329
  %26 = bitcast %"class.std::vector"* %2 to i8*, !dbg !2330
  call void @llvm.memset.p0i8.i64(i8* nonnull %26, i8 0, i64 24, i32 8, i1 false) #2, !dbg !2331
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1776, metadata !DIExpression(DW_OP_stack_value)), !dbg !2332
  call void @llvm.dbg.value(metadata i64 6, metadata !1779, metadata !DIExpression()), !dbg !2334
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1784, metadata !DIExpression(DW_OP_stack_value)), !dbg !2335
  call void @llvm.dbg.value(metadata i64 6, metadata !1787, metadata !DIExpression()), !dbg !2337
  call void @llvm.dbg.value(metadata i64 6, metadata !1791, metadata !DIExpression()), !dbg !2338
  call void @llvm.dbg.value(metadata i64 6, metadata !1797, metadata !DIExpression()), !dbg !2340
  call void @llvm.dbg.value(metadata i8* null, metadata !1801, metadata !DIExpression()), !dbg !2342
  %27 = invoke i8* @_Znwm(i64 24)
          to label %28 unwind label %76, !dbg !2343

; <label>:28:                                     ; preds = %20
  %29 = bitcast %"class.std::vector"* %2 to i8**, !dbg !2344
  store i8* %27, i8** %29, align 8, !dbg !2344, !tbaa !1596
  %30 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %2, i64 0, i32 0, i32 0, i32 1, !dbg !2345
  %31 = bitcast i32** %30 to i8**, !dbg !2346
  store i8* %27, i8** %31, align 8, !dbg !2346, !tbaa !1589
  %32 = getelementptr i8, i8* %27, i64 24, !dbg !2347
  %33 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %2, i64 0, i32 0, i32 0, i32 2, !dbg !2348
  %34 = bitcast i32** %33 to i8**, !dbg !2349
  store i8* %32, i8** %34, align 8, !dbg !2349, !tbaa !1746
  call void @llvm.dbg.value(metadata i64 %23, metadata !2350, metadata !DIExpression()), !dbg !2360
  call void @llvm.dbg.value(metadata i32** undef, metadata !2355, metadata !DIExpression(DW_OP_deref)), !dbg !2363
  call void @llvm.dbg.value(metadata i8* %27, metadata !2356, metadata !DIExpression()), !dbg !2364
  call void @llvm.dbg.value(metadata i64 %23, metadata !2365, metadata !DIExpression()), !dbg !2374
  call void @llvm.dbg.value(metadata i32** undef, metadata !2370, metadata !DIExpression(DW_OP_deref)), !dbg !2376
  call void @llvm.dbg.value(metadata i8* %27, metadata !2371, metadata !DIExpression()), !dbg !2377
  call void @llvm.dbg.value(metadata i8 1, metadata !2372, metadata !DIExpression()), !dbg !2378
  call void @llvm.dbg.value(metadata i64 %23, metadata !2379, metadata !DIExpression()), !dbg !2385
  call void @llvm.dbg.value(metadata i32** undef, metadata !2383, metadata !DIExpression(DW_OP_deref)), !dbg !2387
  call void @llvm.dbg.value(metadata i8* %27, metadata !2384, metadata !DIExpression()), !dbg !2388
  call void @llvm.dbg.value(metadata i64 %23, metadata !2389, metadata !DIExpression()), !dbg !2396
  call void @llvm.dbg.value(metadata i32** undef, metadata !2392, metadata !DIExpression(DW_OP_deref)), !dbg !2398
  call void @llvm.dbg.value(metadata i8* %27, metadata !2393, metadata !DIExpression()), !dbg !2399
  call void @llvm.dbg.value(metadata i64 %23, metadata !2400, metadata !DIExpression()), !dbg !2407
  call void @llvm.dbg.value(metadata i32** undef, metadata !2403, metadata !DIExpression(DW_OP_deref)), !dbg !2409
  call void @llvm.dbg.value(metadata i8* %27, metadata !2404, metadata !DIExpression()), !dbg !2410
  call void @llvm.dbg.value(metadata i64 %23, metadata !2411, metadata !DIExpression()), !dbg !2419
  call void @llvm.dbg.value(metadata i32** undef, metadata !2414, metadata !DIExpression(DW_OP_deref)), !dbg !2421
  call void @llvm.dbg.value(metadata i8* %27, metadata !2415, metadata !DIExpression()), !dbg !2422
  call void @llvm.dbg.value(metadata i8 1, metadata !2416, metadata !DIExpression()), !dbg !2423
  call void @llvm.dbg.value(metadata i64 %23, metadata !2424, metadata !DIExpression()) #2, !dbg !2434
  call void @llvm.dbg.value(metadata i32** undef, metadata !2431, metadata !DIExpression(DW_OP_deref)) #2, !dbg !2436
  call void @llvm.dbg.value(metadata i8* %27, metadata !2432, metadata !DIExpression()) #2, !dbg !2437
  call void @llvm.dbg.value(metadata i64 6, metadata !2433, metadata !DIExpression()) #2, !dbg !2438
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* nonnull %27, i8* nonnull %19, i64 24, i32 4, i1 false) #2, !dbg !2439
  %35 = bitcast i32** %30 to i8**, !dbg !2441
  store i8* %32, i8** %35, align 8, !dbg !2441, !tbaa !1589
  call void @llvm.dbg.value(metadata %"class.std::vector"* %3, metadata !2314, metadata !DIExpression()), !dbg !2442
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !2317, metadata !DIExpression()), !dbg !2444
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1581, metadata !DIExpression()), !dbg !2445
  call void @llvm.dbg.value(metadata %"class.std::vector"* %3, metadata !1760, metadata !DIExpression(DW_OP_stack_value)), !dbg !2447
  call void @llvm.dbg.value(metadata i64 2, metadata !1763, metadata !DIExpression()), !dbg !2449
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !1764, metadata !DIExpression()), !dbg !2450
  call void @llvm.dbg.value(metadata %"class.std::vector"* %3, metadata !1769, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2451
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !1772, metadata !DIExpression()) #2, !dbg !2453
  %36 = bitcast %"class.std::vector"* %3 to i8*, !dbg !2454
  call void @llvm.memset.p0i8.i64(i8* nonnull %36, i8 0, i64 24, i32 8, i1 false) #2, !dbg !2455
  call void @llvm.dbg.value(metadata %"class.std::vector"* %3, metadata !1776, metadata !DIExpression(DW_OP_stack_value)), !dbg !2456
  call void @llvm.dbg.value(metadata i64 2, metadata !1779, metadata !DIExpression()), !dbg !2458
  call void @llvm.dbg.value(metadata %"class.std::vector"* %3, metadata !1784, metadata !DIExpression(DW_OP_stack_value)), !dbg !2459
  call void @llvm.dbg.value(metadata i64 2, metadata !1787, metadata !DIExpression()), !dbg !2461
  call void @llvm.dbg.value(metadata i64 2, metadata !1791, metadata !DIExpression()), !dbg !2462
  call void @llvm.dbg.value(metadata i64 2, metadata !1797, metadata !DIExpression()), !dbg !2464
  call void @llvm.dbg.value(metadata i8* null, metadata !1801, metadata !DIExpression()), !dbg !2466
  %37 = invoke i8* @_Znwm(i64 8)
          to label %38 unwind label %80, !dbg !2467

; <label>:38:                                     ; preds = %28
  %39 = bitcast %"class.std::vector"* %3 to i8**, !dbg !2468
  store i8* %37, i8** %39, align 8, !dbg !2468, !tbaa !1596
  %40 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %3, i64 0, i32 0, i32 0, i32 1, !dbg !2469
  %41 = bitcast i32** %40 to i8**, !dbg !2470
  %42 = getelementptr i8, i8* %37, i64 8, !dbg !2471
  %43 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %3, i64 0, i32 0, i32 0, i32 2, !dbg !2472
  %44 = bitcast i32** %43 to i8**, !dbg !2473
  store i8* %42, i8** %44, align 8, !dbg !2473, !tbaa !1746
  call void @llvm.dbg.value(metadata i8* %16, metadata !2350, metadata !DIExpression()), !dbg !2474
  call void @llvm.dbg.value(metadata i32** undef, metadata !2355, metadata !DIExpression(DW_OP_deref)), !dbg !2476
  call void @llvm.dbg.value(metadata i8* %37, metadata !2356, metadata !DIExpression()), !dbg !2477
  call void @llvm.dbg.value(metadata i8* %16, metadata !2365, metadata !DIExpression()), !dbg !2478
  call void @llvm.dbg.value(metadata i32** undef, metadata !2370, metadata !DIExpression(DW_OP_deref)), !dbg !2480
  call void @llvm.dbg.value(metadata i8* %37, metadata !2371, metadata !DIExpression()), !dbg !2481
  call void @llvm.dbg.value(metadata i8 1, metadata !2372, metadata !DIExpression()), !dbg !2482
  call void @llvm.dbg.value(metadata i8* %16, metadata !2379, metadata !DIExpression()), !dbg !2483
  call void @llvm.dbg.value(metadata i32** undef, metadata !2383, metadata !DIExpression(DW_OP_deref)), !dbg !2485
  call void @llvm.dbg.value(metadata i8* %37, metadata !2384, metadata !DIExpression()), !dbg !2486
  call void @llvm.dbg.value(metadata i8* %16, metadata !2389, metadata !DIExpression()), !dbg !2487
  call void @llvm.dbg.value(metadata i32** undef, metadata !2392, metadata !DIExpression(DW_OP_deref)), !dbg !2489
  call void @llvm.dbg.value(metadata i8* %37, metadata !2393, metadata !DIExpression()), !dbg !2490
  call void @llvm.dbg.value(metadata i8* %16, metadata !2400, metadata !DIExpression()), !dbg !2491
  call void @llvm.dbg.value(metadata i32** undef, metadata !2403, metadata !DIExpression(DW_OP_deref)), !dbg !2493
  call void @llvm.dbg.value(metadata i8* %37, metadata !2404, metadata !DIExpression()), !dbg !2494
  call void @llvm.dbg.value(metadata i8* %16, metadata !2411, metadata !DIExpression()), !dbg !2495
  call void @llvm.dbg.value(metadata i32** undef, metadata !2414, metadata !DIExpression(DW_OP_deref)), !dbg !2497
  call void @llvm.dbg.value(metadata i8* %37, metadata !2415, metadata !DIExpression()), !dbg !2498
  call void @llvm.dbg.value(metadata i8 1, metadata !2416, metadata !DIExpression()), !dbg !2499
  call void @llvm.dbg.value(metadata i8* %16, metadata !2424, metadata !DIExpression()) #2, !dbg !2500
  call void @llvm.dbg.value(metadata i32** undef, metadata !2431, metadata !DIExpression(DW_OP_deref)) #2, !dbg !2502
  call void @llvm.dbg.value(metadata i8* %37, metadata !2432, metadata !DIExpression()) #2, !dbg !2503
  call void @llvm.dbg.value(metadata i64 2, metadata !2433, metadata !DIExpression()) #2, !dbg !2504
  %45 = bitcast i8* %37 to i64*, !dbg !2505
  %46 = load i64, i64* %18, align 4, !dbg !2505
  store i64 %46, i64* %45, align 4, !dbg !2505
  store i8* %42, i8** %41, align 8, !dbg !2506, !tbaa !1589
  invoke void @_Z5mergeSt6vectorIiSaIiEES1_(%"class.std::vector"* nonnull sret %1, %"class.std::vector"* nonnull %2, %"class.std::vector"* nonnull %3)
          to label %47 unwind label %84, !dbg !2507

; <label>:47:                                     ; preds = %38
  call void @llvm.dbg.value(metadata %"class.std::vector"* %1, metadata !1619, metadata !DIExpression()), !dbg !2508
  call void @llvm.dbg.value(metadata i64 2, metadata !1620, metadata !DIExpression()), !dbg !2510
  %48 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %1, i64 0, i32 0, i32 0, i32 0, !dbg !2511
  %49 = load i32*, i32** %48, align 8, !dbg !2511, !tbaa !1596
  %50 = getelementptr i32, i32* %49, i64 2, !dbg !2512
  %51 = load i32, i32* %50, align 4, !dbg !2507, !tbaa !1629
  %52 = invoke dereferenceable(272) %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"* nonnull @_ZSt4cout, i32 %51)
          to label %53 unwind label %88, !dbg !2513

; <label>:53:                                     ; preds = %47
  call void @llvm.dbg.value(metadata %"class.std::vector"* %1, metadata !1683, metadata !DIExpression()) #2, !dbg !2514
  call void @llvm.dbg.value(metadata %"class.std::vector"* %1, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2516
  %54 = load i32*, i32** %48, align 8, !dbg !2518, !tbaa !1596
  call void @llvm.dbg.value(metadata %"class.std::vector"* %1, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2519
  call void @llvm.dbg.value(metadata i32* %54, metadata !1699, metadata !DIExpression()) #2, !dbg !2521
  %55 = icmp eq i32* %54, null, !dbg !2522
  br i1 %55, label %58, label %56, !dbg !2523

; <label>:56:                                     ; preds = %53
  call void @llvm.dbg.value(metadata i32* %54, metadata !1707, metadata !DIExpression()) #2, !dbg !2524
  call void @llvm.dbg.value(metadata i32* %54, metadata !1714, metadata !DIExpression()) #2, !dbg !2526
  %57 = bitcast i32* %54 to i8*, !dbg !2528
  call void @_ZdlPv(i8* %57) #2, !dbg !2529
  br label %58, !dbg !2530

; <label>:58:                                     ; preds = %53, %56
  call void @llvm.dbg.value(metadata %"class.std::vector"* %3, metadata !1683, metadata !DIExpression()) #2, !dbg !2531
  call void @llvm.dbg.value(metadata %"class.std::vector"* %3, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2533
  call void @llvm.dbg.value(metadata %"class.std::vector"* %3, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2535
  call void @llvm.dbg.value(metadata i8* %37, metadata !1699, metadata !DIExpression()) #2, !dbg !2537
  call void @llvm.dbg.value(metadata i8* %37, metadata !1707, metadata !DIExpression()) #2, !dbg !2538
  call void @llvm.dbg.value(metadata i8* %37, metadata !1714, metadata !DIExpression()) #2, !dbg !2540
  call void @_ZdlPv(i8* nonnull %37) #2, !dbg !2542
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1683, metadata !DIExpression()) #2, !dbg !2543
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2545
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2547
  call void @llvm.dbg.value(metadata i8* %27, metadata !1699, metadata !DIExpression()) #2, !dbg !2549
  call void @llvm.dbg.value(metadata i8* %27, metadata !1707, metadata !DIExpression()) #2, !dbg !2550
  call void @llvm.dbg.value(metadata i8* %27, metadata !1714, metadata !DIExpression()) #2, !dbg !2552
  call void @_ZdlPv(i8* %27) #2, !dbg !2554
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1750, metadata !DIExpression()), !dbg !2555
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1683, metadata !DIExpression()) #2, !dbg !2556
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2558
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2560
  call void @llvm.dbg.value(metadata i8* %16, metadata !1699, metadata !DIExpression()) #2, !dbg !2562
  call void @llvm.dbg.value(metadata i8* %16, metadata !1707, metadata !DIExpression()) #2, !dbg !2563
  call void @llvm.dbg.value(metadata i8* %16, metadata !1714, metadata !DIExpression()) #2, !dbg !2565
  call void @_ZdlPv(i8* nonnull %16) #2, !dbg !2567
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1749, metadata !DIExpression()), !dbg !2568
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1683, metadata !DIExpression()) #2, !dbg !2569
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2571
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2573
  call void @llvm.dbg.value(metadata i64 %23, metadata !1699, metadata !DIExpression()) #2, !dbg !2575
  call void @llvm.dbg.value(metadata i64 %23, metadata !1707, metadata !DIExpression()) #2, !dbg !2576
  call void @llvm.dbg.value(metadata i64 %23, metadata !1714, metadata !DIExpression()) #2, !dbg !2578
  call void @_ZdlPv(i8* %19) #2, !dbg !2580
  ret i32 0, !dbg !2581

; <label>:59:                                     ; preds = %0
  %60 = landingpad { i8*, i32 }
          cleanup, !dbg !2581
  %61 = extractvalue { i8*, i32 } %60, 0, !dbg !2581
  %62 = extractvalue { i8*, i32 } %60, 1, !dbg !2581
  br label %110, !dbg !2568

; <label>:63:                                     ; preds = %5
  %64 = landingpad { i8*, i32 }
          cleanup, !dbg !2581
  %65 = ptrtoint i8* %4 to i64, !dbg !2582
  %66 = extractvalue { i8*, i32 } %64, 0, !dbg !2581
  %67 = extractvalue { i8*, i32 } %64, 1, !dbg !2581
  br label %105, !dbg !2583

; <label>:68:                                     ; preds = %8
  %69 = landingpad { i8*, i32 }
          cleanup, !dbg !2581
  %70 = extractvalue { i8*, i32 } %69, 0, !dbg !2581
  %71 = extractvalue { i8*, i32 } %69, 1, !dbg !2581
  br label %105, !dbg !2555

; <label>:72:                                     ; preds = %17
  %73 = landingpad { i8*, i32 }
          cleanup, !dbg !2581
  %74 = extractvalue { i8*, i32 } %73, 1, !dbg !2581
  %75 = extractvalue { i8*, i32 } %73, 0, !dbg !2581
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1750, metadata !DIExpression()), !dbg !2555
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1683, metadata !DIExpression()) #2, !dbg !2584
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2586
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2588
  call void @llvm.dbg.value(metadata i8* %16, metadata !1699, metadata !DIExpression()) #2, !dbg !2590
  call void @llvm.dbg.value(metadata i8* %16, metadata !1707, metadata !DIExpression()) #2, !dbg !2591
  call void @llvm.dbg.value(metadata i8* %16, metadata !1714, metadata !DIExpression()) #2, !dbg !2593
  call void @_ZdlPv(i8* nonnull %16) #2, !dbg !2595
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1749, metadata !DIExpression()), !dbg !2568
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1683, metadata !DIExpression()) #2, !dbg !2596
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2598
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2600
  call void @llvm.dbg.value(metadata i64 %23, metadata !1699, metadata !DIExpression()) #2, !dbg !2602
  br label %105, !dbg !2603

; <label>:76:                                     ; preds = %20
  %77 = landingpad { i8*, i32 }
          cleanup, !dbg !2581
  %78 = extractvalue { i8*, i32 } %77, 0, !dbg !2581
  %79 = extractvalue { i8*, i32 } %77, 1, !dbg !2581
  br label %102, !dbg !2581

; <label>:80:                                     ; preds = %28
  %81 = landingpad { i8*, i32 }
          cleanup, !dbg !2581
  %82 = extractvalue { i8*, i32 } %81, 0, !dbg !2581
  %83 = extractvalue { i8*, i32 } %81, 1, !dbg !2581
  br label %99, !dbg !2581

; <label>:84:                                     ; preds = %38
  %85 = landingpad { i8*, i32 }
          cleanup, !dbg !2581
  %86 = extractvalue { i8*, i32 } %85, 0, !dbg !2581
  %87 = extractvalue { i8*, i32 } %85, 1, !dbg !2581
  br label %96, !dbg !2581

; <label>:88:                                     ; preds = %47
  %89 = landingpad { i8*, i32 }
          cleanup, !dbg !2581
  %90 = extractvalue { i8*, i32 } %89, 0, !dbg !2581
  %91 = extractvalue { i8*, i32 } %89, 1, !dbg !2581
  call void @llvm.dbg.value(metadata %"class.std::vector"* %1, metadata !1683, metadata !DIExpression()) #2, !dbg !2604
  call void @llvm.dbg.value(metadata %"class.std::vector"* %1, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2606
  %92 = load i32*, i32** %48, align 8, !dbg !2608, !tbaa !1596
  call void @llvm.dbg.value(metadata %"class.std::vector"* %1, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2609
  call void @llvm.dbg.value(metadata i32* %92, metadata !1699, metadata !DIExpression()) #2, !dbg !2611
  %93 = icmp eq i32* %92, null, !dbg !2612
  br i1 %93, label %96, label %94, !dbg !2613

; <label>:94:                                     ; preds = %88
  call void @llvm.dbg.value(metadata i32* %92, metadata !1707, metadata !DIExpression()) #2, !dbg !2614
  call void @llvm.dbg.value(metadata i32* %92, metadata !1714, metadata !DIExpression()) #2, !dbg !2616
  %95 = bitcast i32* %92 to i8*, !dbg !2618
  call void @_ZdlPv(i8* %95) #2, !dbg !2619
  br label %96, !dbg !2620

; <label>:96:                                     ; preds = %84, %88, %94
  %97 = phi i8* [ %86, %84 ], [ %90, %88 ], [ %90, %94 ]
  %98 = phi i32 [ %87, %84 ], [ %91, %88 ], [ %91, %94 ]
  call void @llvm.dbg.value(metadata %"class.std::vector"* %3, metadata !1683, metadata !DIExpression()) #2, !dbg !2621
  call void @llvm.dbg.value(metadata %"class.std::vector"* %3, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2623
  call void @llvm.dbg.value(metadata %"class.std::vector"* %3, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2625
  call void @llvm.dbg.value(metadata i8* %37, metadata !1699, metadata !DIExpression()) #2, !dbg !2627
  call void @llvm.dbg.value(metadata i8* %37, metadata !1707, metadata !DIExpression()) #2, !dbg !2628
  call void @llvm.dbg.value(metadata i8* %37, metadata !1714, metadata !DIExpression()) #2, !dbg !2630
  call void @_ZdlPv(i8* nonnull %37) #2, !dbg !2632
  br label %99, !dbg !2633

; <label>:99:                                     ; preds = %80, %96
  %100 = phi i8* [ %82, %80 ], [ %97, %96 ]
  %101 = phi i32 [ %83, %80 ], [ %98, %96 ]
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1683, metadata !DIExpression()) #2, !dbg !2634
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2636
  call void @llvm.dbg.value(metadata %"class.std::vector"* %2, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2638
  call void @llvm.dbg.value(metadata i8* %27, metadata !1699, metadata !DIExpression()) #2, !dbg !2640
  call void @llvm.dbg.value(metadata i8* %27, metadata !1707, metadata !DIExpression()) #2, !dbg !2641
  call void @llvm.dbg.value(metadata i8* %27, metadata !1714, metadata !DIExpression()) #2, !dbg !2643
  call void @_ZdlPv(i8* %27) #2, !dbg !2645
  br label %102, !dbg !2646

; <label>:102:                                    ; preds = %76, %99
  %103 = phi i8* [ %78, %76 ], [ %100, %99 ]
  %104 = phi i32 [ %79, %76 ], [ %101, %99 ]
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1750, metadata !DIExpression()), !dbg !2555
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1683, metadata !DIExpression()) #2, !dbg !2584
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2586
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2588
  call void @llvm.dbg.value(metadata i8* %16, metadata !1699, metadata !DIExpression()) #2, !dbg !2590
  call void @llvm.dbg.value(metadata i8* %16, metadata !1707, metadata !DIExpression()) #2, !dbg !2591
  call void @llvm.dbg.value(metadata i8* %16, metadata !1714, metadata !DIExpression()) #2, !dbg !2593
  call void @_ZdlPv(i8* nonnull %16) #2, !dbg !2595
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1749, metadata !DIExpression()), !dbg !2568
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1683, metadata !DIExpression()) #2, !dbg !2596
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1688, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2598
  call void @llvm.dbg.value(metadata %"class.std::vector"* undef, metadata !1696, metadata !DIExpression(DW_OP_stack_value)) #2, !dbg !2600
  call void @llvm.dbg.value(metadata i64 %23, metadata !1699, metadata !DIExpression()) #2, !dbg !2602
  br label %105, !dbg !2603

; <label>:105:                                    ; preds = %102, %68, %63, %72
  %106 = phi i32 [ %104, %102 ], [ %74, %72 ], [ %71, %68 ], [ %67, %63 ]
  %107 = phi i8* [ %103, %102 ], [ %75, %72 ], [ %70, %68 ], [ %66, %63 ]
  %108 = phi i64 [ %23, %102 ], [ %13, %72 ], [ %13, %68 ], [ %65, %63 ]
  call void @llvm.dbg.value(metadata i64 %23, metadata !1707, metadata !DIExpression()) #2, !dbg !2647
  call void @llvm.dbg.value(metadata i64 %23, metadata !1714, metadata !DIExpression()) #2, !dbg !2649
  %109 = inttoptr i64 %108 to i8*, !dbg !2651
  call void @_ZdlPv(i8* %109) #2, !dbg !2652
  br label %110, !dbg !2653

; <label>:110:                                    ; preds = %105, %59
  %111 = phi i8* [ %61, %59 ], [ %107, %105 ]
  %112 = phi i32 [ %62, %59 ], [ %106, %105 ]
  %113 = insertvalue { i8*, i32 } undef, i8* %111, 0, !dbg !2581
  %114 = insertvalue { i8*, i32 } %113, i32 %112, 1, !dbg !2581
  resume { i8*, i32 } %114, !dbg !2581
}

declare dereferenceable(272) %"class.std::basic_ostream"* @_ZNSolsEi(%"class.std::basic_ostream"*, i32) local_unnamed_addr #0

; Function Attrs: nobuiltin nounwind
declare void @_ZdlPv(i8*) local_unnamed_addr #6

; Function Attrs: sspstrong uwtable
define linkonce_odr void @_ZNSt6vectorIiSaIiEE17_M_realloc_insertIJRKiEEEvN9__gnu_cxx17__normal_iteratorIPiS1_EEDpOT_(%"class.std::vector"*, i32*, i32* dereferenceable(4)) local_unnamed_addr #3 comdat align 2 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) !dbg !2654 {
  %4 = ptrtoint i32* %1 to i64
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !2660, metadata !DIExpression()), !dbg !2667
  call void @llvm.dbg.value(metadata i32* %2, metadata !2662, metadata !DIExpression()), !dbg !2668
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1924, metadata !DIExpression()), !dbg !2669
  call void @llvm.dbg.value(metadata i64 1, metadata !1927, metadata !DIExpression()), !dbg !2671
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1581, metadata !DIExpression()), !dbg !2672
  %5 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, i32 0, i32 1, !dbg !2674
  %6 = bitcast i32** %5 to i64*, !dbg !2674
  %7 = load i64, i64* %6, align 8, !dbg !2674, !tbaa !1589
  %8 = bitcast %"class.std::vector"* %0 to i64*, !dbg !2675
  %9 = load i64, i64* %8, align 8, !dbg !2675, !tbaa !2676
  %10 = sub i64 %7, %9, !dbg !2677
  %11 = ashr exact i64 %10, 2, !dbg !2677
  call void @llvm.dbg.value(metadata i64 1, metadata !1927, metadata !DIExpression()), !dbg !2671
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1581, metadata !DIExpression()), !dbg !2678
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1581, metadata !DIExpression()), !dbg !2680
  %12 = icmp eq i64 %10, 0, !dbg !2682
  %13 = select i1 %12, i64 1, i64 %11, !dbg !2695
  %14 = add nsw i64 %13, %11, !dbg !2696
  call void @llvm.dbg.value(metadata i64 %14, metadata !1929, metadata !DIExpression()), !dbg !2697
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1581, metadata !DIExpression()), !dbg !2698
  %15 = icmp ult i64 %14, %11, !dbg !2700
  %16 = icmp ugt i64 %14, 4611686018427387903, !dbg !2701
  %17 = or i1 %15, %16, !dbg !2702
  %18 = select i1 %17, i64 4611686018427387903, i64 %14, !dbg !2702
  call void @llvm.dbg.value(metadata i64 %18, metadata !2663, metadata !DIExpression()), !dbg !2703
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !2704, metadata !DIExpression()), !dbg !2707
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !2709, metadata !DIExpression()), !dbg !2714
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !2712, metadata !DIExpression(DW_OP_stack_value)), !dbg !2716
  %19 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, i32 0, i32 0, !dbg !2717
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !2661, metadata !DIExpression()), !dbg !2718
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !2719, metadata !DIExpression()), !dbg !2726
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !2725, metadata !DIExpression()), !dbg !2728
  %20 = sub i64 %4, %9, !dbg !2729
  %21 = ashr exact i64 %20, 2, !dbg !2729
  call void @llvm.dbg.value(metadata i64 %21, metadata !2664, metadata !DIExpression()), !dbg !2730
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1784, metadata !DIExpression(DW_OP_stack_value)), !dbg !2731
  call void @llvm.dbg.value(metadata i64 %18, metadata !1787, metadata !DIExpression()), !dbg !2733
  %22 = icmp eq i64 %18, 0, !dbg !2734
  %23 = inttoptr i64 %9 to i32*, !dbg !2735
  br i1 %22, label %32, label %24, !dbg !2735

; <label>:24:                                     ; preds = %3
  call void @llvm.dbg.value(metadata i64 %18, metadata !1791, metadata !DIExpression()), !dbg !2736
  call void @llvm.dbg.value(metadata i64 %18, metadata !1797, metadata !DIExpression()), !dbg !2738
  call void @llvm.dbg.value(metadata i8* null, metadata !1801, metadata !DIExpression()), !dbg !2740
  %25 = icmp ugt i64 %18, 4611686018427387903, !dbg !2741
  br i1 %25, label %26, label %27, !dbg !2743

; <label>:26:                                     ; preds = %24
  tail call void @_ZSt17__throw_bad_allocv() #10, !dbg !2744
  unreachable, !dbg !2744

; <label>:27:                                     ; preds = %24
  %28 = shl i64 %18, 2, !dbg !2745
  %29 = tail call i8* @_Znwm(i64 %28), !dbg !2746
  %30 = bitcast i8* %29 to i32*, !dbg !2747
  %31 = load i32*, i32** %19, align 8, !dbg !2748, !tbaa !1596
  br label %32, !dbg !2735

; <label>:32:                                     ; preds = %27, %3
  %33 = phi i32* [ %31, %27 ], [ %23, %3 ], !dbg !2748
  %34 = phi i8* [ %29, %27 ], [ null, %3 ], !dbg !2735
  %35 = phi i32* [ %30, %27 ], [ null, %3 ], !dbg !2735
  call void @llvm.dbg.value(metadata i32* %35, metadata !2665, metadata !DIExpression()), !dbg !2750
  call void @llvm.dbg.value(metadata i32* %35, metadata !2666, metadata !DIExpression()), !dbg !2751
  %36 = getelementptr i32, i32* %35, i64 %21, !dbg !2752
  call void @llvm.dbg.value(metadata i32* %36, metadata !1647, metadata !DIExpression()), !dbg !2753
  call void @llvm.dbg.value(metadata i32* %2, metadata !1659, metadata !DIExpression()), !dbg !2755
  call void @llvm.dbg.value(metadata i32* %36, metadata !1664, metadata !DIExpression()), !dbg !2756
  call void @llvm.dbg.value(metadata i32* %2, metadata !1672, metadata !DIExpression()), !dbg !2758
  %37 = load i32, i32* %2, align 4, !dbg !2759, !tbaa !1629
  store i32 %37, i32* %36, align 4, !dbg !2760, !tbaa !1629
  call void @llvm.dbg.value(metadata i32* null, metadata !2666, metadata !DIExpression()), !dbg !2751
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !2661, metadata !DIExpression()), !dbg !2718
  call void @llvm.dbg.value(metadata i32* %33, metadata !1981, metadata !DIExpression()), !dbg !2761
  call void @llvm.dbg.value(metadata i32* %1, metadata !1986, metadata !DIExpression()), !dbg !2763
  call void @llvm.dbg.value(metadata i32* %35, metadata !1987, metadata !DIExpression()), !dbg !2764
  call void @llvm.dbg.value(metadata i32* %33, metadata !1996, metadata !DIExpression()), !dbg !2765
  call void @llvm.dbg.value(metadata i32* %1, metadata !2001, metadata !DIExpression()), !dbg !2767
  call void @llvm.dbg.value(metadata i32* %35, metadata !2002, metadata !DIExpression()), !dbg !2768
  call void @llvm.dbg.value(metadata i32* %33, metadata !2010, metadata !DIExpression()), !dbg !2769
  call void @llvm.dbg.value(metadata i32* %1, metadata !2015, metadata !DIExpression()), !dbg !2771
  call void @llvm.dbg.value(metadata i32* %35, metadata !2016, metadata !DIExpression()), !dbg !2772
  call void @llvm.dbg.value(metadata i8 1, metadata !2017, metadata !DIExpression()), !dbg !2773
  call void @llvm.dbg.value(metadata i32* %33, metadata !2024, metadata !DIExpression()), !dbg !2774
  call void @llvm.dbg.value(metadata i32* %1, metadata !2031, metadata !DIExpression()), !dbg !2776
  call void @llvm.dbg.value(metadata i32* %35, metadata !2032, metadata !DIExpression()), !dbg !2777
  call void @llvm.dbg.value(metadata i32* %33, metadata !2037, metadata !DIExpression()), !dbg !2778
  call void @llvm.dbg.value(metadata i32* %1, metadata !2040, metadata !DIExpression()), !dbg !2780
  call void @llvm.dbg.value(metadata i32* %35, metadata !2041, metadata !DIExpression()), !dbg !2781
  call void @llvm.dbg.value(metadata i32* %33, metadata !2048, metadata !DIExpression()), !dbg !2782
  call void @llvm.dbg.value(metadata i32* %1, metadata !2053, metadata !DIExpression()), !dbg !2784
  call void @llvm.dbg.value(metadata i32* %35, metadata !2054, metadata !DIExpression()), !dbg !2785
  call void @llvm.dbg.value(metadata i32* %33, metadata !2062, metadata !DIExpression()), !dbg !2786
  call void @llvm.dbg.value(metadata i32* %1, metadata !2065, metadata !DIExpression()), !dbg !2788
  call void @llvm.dbg.value(metadata i32* %35, metadata !2066, metadata !DIExpression()), !dbg !2789
  call void @llvm.dbg.value(metadata i8 1, metadata !2067, metadata !DIExpression()), !dbg !2790
  call void @llvm.dbg.value(metadata i32* %33, metadata !2073, metadata !DIExpression()) #2, !dbg !2791
  call void @llvm.dbg.value(metadata i32* %1, metadata !2092, metadata !DIExpression()) #2, !dbg !2793
  call void @llvm.dbg.value(metadata i32* %35, metadata !2093, metadata !DIExpression()) #2, !dbg !2794
  %38 = ptrtoint i32* %33 to i64, !dbg !2795
  %39 = sub i64 %4, %38, !dbg !2795
  %40 = ashr exact i64 %39, 2, !dbg !2795
  call void @llvm.dbg.value(metadata i64 %40, metadata !2094, metadata !DIExpression()) #2, !dbg !2796
  %41 = icmp eq i64 %39, 0, !dbg !2797
  br i1 %41, label %44, label %42, !dbg !2798

; <label>:42:                                     ; preds = %32
  %43 = bitcast i32* %33 to i8*, !dbg !2799
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %34, i8* %43, i64 %39, i32 4, i1 false) #2, !dbg !2799
  br label %44, !dbg !2799

; <label>:44:                                     ; preds = %42, %32
  %45 = getelementptr i32, i32* %35, i64 %40, !dbg !2800
  call void @llvm.dbg.value(metadata i32* %45, metadata !2666, metadata !DIExpression()), !dbg !2751
  %46 = getelementptr i32, i32* %45, i64 1, !dbg !2801
  call void @llvm.dbg.value(metadata i32* %46, metadata !2666, metadata !DIExpression()), !dbg !2751
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !2661, metadata !DIExpression()), !dbg !2718
  %47 = load i64, i64* %6, align 8, !dbg !2802, !tbaa !1589
  call void @llvm.dbg.value(metadata i32* %1, metadata !1981, metadata !DIExpression()), !dbg !2803
  call void @llvm.dbg.value(metadata i32** %5, metadata !1986, metadata !DIExpression(DW_OP_deref)), !dbg !2805
  call void @llvm.dbg.value(metadata i32* %46, metadata !1987, metadata !DIExpression()), !dbg !2806
  call void @llvm.dbg.value(metadata i32* %1, metadata !1996, metadata !DIExpression()), !dbg !2807
  call void @llvm.dbg.value(metadata i32** %5, metadata !2001, metadata !DIExpression(DW_OP_deref)), !dbg !2809
  call void @llvm.dbg.value(metadata i32* %46, metadata !2002, metadata !DIExpression()), !dbg !2810
  call void @llvm.dbg.value(metadata i32* %1, metadata !2010, metadata !DIExpression()), !dbg !2811
  call void @llvm.dbg.value(metadata i32** %5, metadata !2015, metadata !DIExpression(DW_OP_deref)), !dbg !2813
  call void @llvm.dbg.value(metadata i32* %46, metadata !2016, metadata !DIExpression()), !dbg !2814
  call void @llvm.dbg.value(metadata i8 1, metadata !2017, metadata !DIExpression()), !dbg !2815
  call void @llvm.dbg.value(metadata i32* %1, metadata !2024, metadata !DIExpression()), !dbg !2816
  call void @llvm.dbg.value(metadata i32** %5, metadata !2031, metadata !DIExpression(DW_OP_deref)), !dbg !2818
  call void @llvm.dbg.value(metadata i32* %46, metadata !2032, metadata !DIExpression()), !dbg !2819
  call void @llvm.dbg.value(metadata i32* %1, metadata !2037, metadata !DIExpression()), !dbg !2820
  call void @llvm.dbg.value(metadata i32** %5, metadata !2040, metadata !DIExpression(DW_OP_deref)), !dbg !2822
  call void @llvm.dbg.value(metadata i32* %46, metadata !2041, metadata !DIExpression()), !dbg !2823
  call void @llvm.dbg.value(metadata i32* %1, metadata !2048, metadata !DIExpression()), !dbg !2824
  call void @llvm.dbg.value(metadata i32** %5, metadata !2053, metadata !DIExpression(DW_OP_deref)), !dbg !2826
  call void @llvm.dbg.value(metadata i32* %46, metadata !2054, metadata !DIExpression()), !dbg !2827
  call void @llvm.dbg.value(metadata i32* %1, metadata !2062, metadata !DIExpression()), !dbg !2828
  call void @llvm.dbg.value(metadata i32** %5, metadata !2065, metadata !DIExpression(DW_OP_deref)), !dbg !2830
  call void @llvm.dbg.value(metadata i32* %46, metadata !2066, metadata !DIExpression()), !dbg !2831
  call void @llvm.dbg.value(metadata i8 1, metadata !2067, metadata !DIExpression()), !dbg !2832
  call void @llvm.dbg.value(metadata i32* %1, metadata !2073, metadata !DIExpression()) #2, !dbg !2833
  call void @llvm.dbg.value(metadata i32** %5, metadata !2092, metadata !DIExpression(DW_OP_deref)) #2, !dbg !2835
  call void @llvm.dbg.value(metadata i32* %46, metadata !2093, metadata !DIExpression()) #2, !dbg !2836
  %48 = sub i64 %47, %4, !dbg !2837
  %49 = ashr exact i64 %48, 2, !dbg !2837
  call void @llvm.dbg.value(metadata i64 %49, metadata !2094, metadata !DIExpression()) #2, !dbg !2838
  %50 = icmp eq i64 %48, 0, !dbg !2839
  br i1 %50, label %54, label %51, !dbg !2840

; <label>:51:                                     ; preds = %44
  %52 = bitcast i32* %46 to i8*, !dbg !2841
  %53 = bitcast i32* %1 to i8*, !dbg !2841
  tail call void @llvm.memmove.p0i8.p0i8.i64(i8* %52, i8* %53, i64 %48, i32 4, i1 false) #2, !dbg !2841
  br label %54, !dbg !2841

; <label>:54:                                     ; preds = %51, %44
  %55 = getelementptr i32, i32* %46, i64 %49, !dbg !2842
  call void @llvm.dbg.value(metadata i32* %55, metadata !2666, metadata !DIExpression()), !dbg !2751
  %56 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, i32 0, i32 2, !dbg !2843
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !1696, metadata !DIExpression(DW_OP_stack_value)), !dbg !2844
  call void @llvm.dbg.value(metadata i32* %33, metadata !1699, metadata !DIExpression()), !dbg !2846
  %57 = icmp eq i32* %33, null, !dbg !2847
  br i1 %57, label %60, label %58, !dbg !2848

; <label>:58:                                     ; preds = %54
  call void @llvm.dbg.value(metadata i32* %33, metadata !1707, metadata !DIExpression()), !dbg !2849
  call void @llvm.dbg.value(metadata i32* %33, metadata !1714, metadata !DIExpression()) #2, !dbg !2851
  %59 = bitcast i32* %33 to i8*, !dbg !2853
  tail call void @_ZdlPv(i8* %59) #2, !dbg !2854
  br label %60, !dbg !2855

; <label>:60:                                     ; preds = %54, %58
  %61 = bitcast %"class.std::vector"* %0 to i8**, !dbg !2856
  store i8* %34, i8** %61, align 8, !dbg !2856, !tbaa !1596
  store i32* %55, i32** %5, align 8, !dbg !2857, !tbaa !1589
  %62 = getelementptr i32, i32* %35, i64 %18, !dbg !2858
  store i32* %62, i32** %56, align 8, !dbg !2859, !tbaa !1746
  ret void, !dbg !2860
}

; Function Attrs: noreturn
declare void @_ZSt17__throw_bad_allocv() local_unnamed_addr #7

; Function Attrs: nobuiltin
declare noalias nonnull i8* @_Znwm(i64) local_unnamed_addr #8

; Function Attrs: argmemonly nounwind
declare void @llvm.memmove.p0i8.p0i8.i64(i8* nocapture, i8* nocapture readonly, i64, i32, i1) #9

; Function Attrs: sspstrong uwtable
define internal void @_GLOBAL__sub_I_merge.cpp() #3 section ".text.startup" !dbg !2861 {
  tail call void @_ZNSt8ios_base4InitC1Ev(%"class.std::ios_base::Init"* nonnull @_ZStL8__ioinit), !dbg !2863
  %1 = tail call i32 @__cxa_atexit(void (i8*)* bitcast (void (%"class.std::ios_base::Init"*)* @_ZNSt8ios_base4InitD1Ev to void (i8*)*), i8* getelementptr inbounds (%"class.std::ios_base::Init", %"class.std::ios_base::Init"* @_ZStL8__ioinit, i64 0, i32 0), i8* nonnull @__dso_handle) #2, !dbg !2863
  ret void
}

; Function Attrs: nounwind readnone speculatable
declare void @llvm.dbg.value(metadata, metadata, metadata) #4

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i32, i1) #9

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i32, i1) #9

attributes #0 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }
attributes #3 = { sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind readnone speculatable }
attributes #5 = { norecurse sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { nobuiltin nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { noreturn "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #8 = { nobuiltin "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #9 = { argmemonly nounwind }
attributes #10 = { noreturn }

!llvm.dbg.cu = !{!19}
!llvm.module.flags = !{!1539, !1540, !1541, !1542}
!llvm.ident = !{!1543}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "__ioinit", linkageName: "_ZStL8__ioinit", scope: !2, file: !3, line: 74, type: !4, isLocal: true, isDefinition: true)
!2 = !DINamespace(name: "std", scope: null)
!3 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/iostream", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!4 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "Init", scope: !6, file: !5, line: 601, size: 8, elements: !7, identifier: "_ZTSNSt8ios_base4InitE")
!5 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/bits/ios_base.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!6 = !DICompositeType(tag: DW_TAG_class_type, name: "ios_base", scope: !2, file: !5, line: 228, flags: DIFlagFwdDecl, identifier: "_ZTSSt8ios_base")
!7 = !{!8, !12, !14, !18}
!8 = !DIDerivedType(tag: DW_TAG_member, name: "_S_refcount", scope: !4, file: !5, line: 609, baseType: !9, flags: DIFlagStaticMember)
!9 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Atomic_word", file: !10, line: 32, baseType: !11)
!10 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/x86_64-unknown-linux-gnu/bits/atomic_word.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!11 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!12 = !DIDerivedType(tag: DW_TAG_member, name: "_S_synced_with_stdio", scope: !4, file: !5, line: 610, baseType: !13, flags: DIFlagStaticMember)
!13 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!14 = !DISubprogram(name: "Init", scope: !4, file: !5, line: 605, type: !15, isLocal: false, isDefinition: false, scopeLine: 605, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!15 = !DISubroutineType(types: !16)
!16 = !{null, !17}
!17 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!18 = !DISubprogram(name: "~Init", scope: !4, file: !5, line: 606, type: !15, isLocal: false, isDefinition: false, scopeLine: 606, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!19 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus, file: !20, producer: "clang version 6.0.1 (tags/RELEASE_601/final)", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !21, retainedTypes: !30, globals: !615, imports: !616)
!20 = !DIFile(filename: "merge.cpp", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!21 = !{!22}
!22 = !DICompositeType(tag: DW_TAG_enumeration_type, scope: !24, file: !23, line: 104, size: 32, elements: !28, identifier: "_ZTSNSt10__are_sameIiiEUt_E")
!23 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/bits/cpp_type_traits.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!24 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__are_same<int, int>", scope: !2, file: !23, line: 102, size: 8, elements: !25, templateParams: !26, identifier: "_ZTSSt10__are_sameIiiE")
!25 = !{}
!26 = !{!27, !27}
!27 = !DITemplateTypeParameter(type: !11)
!28 = !{!29}
!29 = !DIEnumerator(name: "__value", value: 1)
!30 = !{!31, !221, !222, !96, !223, !224, !99, !55, !563, !290, !480}
!31 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !32, size: 64)
!32 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Tp_alloc_type", scope: !34, file: !33, line: 77, baseType: !218)
!33 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/bits/stl_vector.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!34 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_Vector_base<int, std::allocator<int> >", scope: !2, file: !33, line: 74, size: 192, elements: !35, templateParams: !217, identifier: "_ZTSSt12_Vector_baseIiSaIiEE")
!35 = !{!36, !171, !176, !181, !185, !188, !193, !196, !199, !202, !206, !209, !210, !213, !216}
!36 = !DIDerivedType(tag: DW_TAG_member, name: "_M_impl", scope: !34, file: !33, line: 166, baseType: !37, size: 192)
!37 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_Vector_impl", scope: !34, file: !33, line: 81, size: 192, elements: !38, identifier: "_ZTSNSt12_Vector_baseIiSaIiEE12_Vector_implE")
!38 = !{!39, !40, !152, !153, !154, !158, !163, !167}
!39 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !37, baseType: !32)
!40 = !DIDerivedType(tag: DW_TAG_member, name: "_M_start", scope: !37, file: !33, line: 84, baseType: !41, size: 64)
!41 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !34, file: !33, line: 79, baseType: !42)
!42 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !44, file: !43, line: 59, baseType: !54)
!43 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/ext/alloc_traits.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!44 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__alloc_traits<std::allocator<int> >", scope: !45, file: !43, line: 50, size: 8, elements: !46, templateParams: !136, identifier: "_ZTSN9__gnu_cxx14__alloc_traitsISaIiEEE")
!45 = !DINamespace(name: "__gnu_cxx", scope: null)
!46 = !{!47, !138, !141, !145, !148, !149, !150, !151}
!47 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !44, baseType: !48)
!48 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "allocator_traits<std::allocator<int> >", scope: !2, file: !49, line: 384, size: 8, elements: !50, templateParams: !136, identifier: "_ZTSSt16allocator_traitsISaIiEE")
!49 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/bits/alloc_traits.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!50 = !{!51, !120, !124, !127, !133}
!51 = !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaIiEE8allocateERS0_m", scope: !48, file: !49, line: 435, type: !52, isLocal: false, isDefinition: false, scopeLine: 435, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!52 = !DISubroutineType(types: !53)
!53 = !{!54, !56, !119}
!54 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !48, file: !49, line: 392, baseType: !55)
!55 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !11, size: 64)
!56 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !57, size: 64)
!57 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !48, file: !49, line: 387, baseType: !58)
!58 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "allocator<int>", scope: !2, file: !59, line: 108, size: 8, elements: !60, templateParams: !107, identifier: "_ZTSSaIiE")
!59 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/bits/allocator.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!60 = !{!61, !109, !113, !118}
!61 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !58, baseType: !62, flags: DIFlagPublic)
!62 = !DIDerivedType(tag: DW_TAG_typedef, name: "__allocator_base<int>", scope: !2, file: !63, line: 48, baseType: !64)
!63 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/x86_64-unknown-linux-gnu/bits/c++allocator.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!64 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "new_allocator<int>", scope: !45, file: !65, line: 58, size: 8, elements: !66, templateParams: !107, identifier: "_ZTSN9__gnu_cxx13new_allocatorIiEE")
!65 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/ext/new_allocator.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!66 = !{!67, !71, !76, !77, !84, !92, !101, !104}
!67 = !DISubprogram(name: "new_allocator", scope: !64, file: !65, line: 79, type: !68, isLocal: false, isDefinition: false, scopeLine: 79, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!68 = !DISubroutineType(types: !69)
!69 = !{null, !70}
!70 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !64, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!71 = !DISubprogram(name: "new_allocator", scope: !64, file: !65, line: 81, type: !72, isLocal: false, isDefinition: false, scopeLine: 81, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!72 = !DISubroutineType(types: !73)
!73 = !{null, !70, !74}
!74 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !75, size: 64)
!75 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !64)
!76 = !DISubprogram(name: "~new_allocator", scope: !64, file: !65, line: 86, type: !68, isLocal: false, isDefinition: false, scopeLine: 86, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!77 = !DISubprogram(name: "address", linkageName: "_ZNK9__gnu_cxx13new_allocatorIiE7addressERi", scope: !64, file: !65, line: 89, type: !78, isLocal: false, isDefinition: false, scopeLine: 89, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!78 = !DISubroutineType(types: !79)
!79 = !{!80, !81, !82}
!80 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !64, file: !65, line: 63, baseType: !55)
!81 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !75, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!82 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !64, file: !65, line: 65, baseType: !83)
!83 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !11, size: 64)
!84 = !DISubprogram(name: "address", linkageName: "_ZNK9__gnu_cxx13new_allocatorIiE7addressERKi", scope: !64, file: !65, line: 93, type: !85, isLocal: false, isDefinition: false, scopeLine: 93, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!85 = !DISubroutineType(types: !86)
!86 = !{!87, !81, !90}
!87 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_pointer", scope: !64, file: !65, line: 64, baseType: !88)
!88 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !89, size: 64)
!89 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !11)
!90 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reference", scope: !64, file: !65, line: 66, baseType: !91)
!91 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !89, size: 64)
!92 = !DISubprogram(name: "allocate", linkageName: "_ZN9__gnu_cxx13new_allocatorIiE8allocateEmPKv", scope: !64, file: !65, line: 99, type: !93, isLocal: false, isDefinition: false, scopeLine: 99, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!93 = !DISubroutineType(types: !94)
!94 = !{!80, !70, !95, !99}
!95 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", file: !65, line: 61, baseType: !96)
!96 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_t", scope: !2, file: !97, line: 231, baseType: !98)
!97 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/x86_64-unknown-linux-gnu/bits/c++config.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!98 = !DIBasicType(name: "long unsigned int", size: 64, encoding: DW_ATE_unsigned)
!99 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !100, size: 64)
!100 = !DIDerivedType(tag: DW_TAG_const_type, baseType: null)
!101 = !DISubprogram(name: "deallocate", linkageName: "_ZN9__gnu_cxx13new_allocatorIiE10deallocateEPim", scope: !64, file: !65, line: 116, type: !102, isLocal: false, isDefinition: false, scopeLine: 116, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!102 = !DISubroutineType(types: !103)
!103 = !{null, !70, !80, !95}
!104 = !DISubprogram(name: "max_size", linkageName: "_ZNK9__gnu_cxx13new_allocatorIiE8max_sizeEv", scope: !64, file: !65, line: 129, type: !105, isLocal: false, isDefinition: false, scopeLine: 129, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!105 = !DISubroutineType(types: !106)
!106 = !{!95, !81}
!107 = !{!108}
!108 = !DITemplateTypeParameter(name: "_Tp", type: !11)
!109 = !DISubprogram(name: "allocator", scope: !58, file: !59, line: 131, type: !110, isLocal: false, isDefinition: false, scopeLine: 131, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!110 = !DISubroutineType(types: !111)
!111 = !{null, !112}
!112 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !58, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!113 = !DISubprogram(name: "allocator", scope: !58, file: !59, line: 133, type: !114, isLocal: false, isDefinition: false, scopeLine: 133, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!114 = !DISubroutineType(types: !115)
!115 = !{null, !112, !116}
!116 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !117, size: 64)
!117 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !58)
!118 = !DISubprogram(name: "~allocator", scope: !58, file: !59, line: 139, type: !110, isLocal: false, isDefinition: false, scopeLine: 139, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!119 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", file: !49, line: 407, baseType: !96)
!120 = !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaIiEE8allocateERS0_mPKv", scope: !48, file: !49, line: 449, type: !121, isLocal: false, isDefinition: false, scopeLine: 449, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!121 = !DISubroutineType(types: !122)
!122 = !{!54, !56, !119, !123}
!123 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_void_pointer", file: !49, line: 401, baseType: !99)
!124 = !DISubprogram(name: "deallocate", linkageName: "_ZNSt16allocator_traitsISaIiEE10deallocateERS0_Pim", scope: !48, file: !49, line: 461, type: !125, isLocal: false, isDefinition: false, scopeLine: 461, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!125 = !DISubroutineType(types: !126)
!126 = !{null, !56, !54, !119}
!127 = !DISubprogram(name: "max_size", linkageName: "_ZNSt16allocator_traitsISaIiEE8max_sizeERKS0_", scope: !48, file: !49, line: 495, type: !128, isLocal: false, isDefinition: false, scopeLine: 495, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!128 = !DISubroutineType(types: !129)
!129 = !{!130, !131}
!130 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !48, file: !49, line: 407, baseType: !96)
!131 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !132, size: 64)
!132 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !57)
!133 = !DISubprogram(name: "select_on_container_copy_construction", linkageName: "_ZNSt16allocator_traitsISaIiEE37select_on_container_copy_constructionERKS0_", scope: !48, file: !49, line: 504, type: !134, isLocal: false, isDefinition: false, scopeLine: 504, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!134 = !DISubroutineType(types: !135)
!135 = !{!57, !131}
!136 = !{!137}
!137 = !DITemplateTypeParameter(name: "_Alloc", type: !58)
!138 = !DISubprogram(name: "_S_select_on_copy", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIiEE17_S_select_on_copyERKS1_", scope: !44, file: !43, line: 94, type: !139, isLocal: false, isDefinition: false, scopeLine: 94, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!139 = !DISubroutineType(types: !140)
!140 = !{!58, !116}
!141 = !DISubprogram(name: "_S_on_swap", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIiEE10_S_on_swapERS1_S3_", scope: !44, file: !43, line: 97, type: !142, isLocal: false, isDefinition: false, scopeLine: 97, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!142 = !DISubroutineType(types: !143)
!143 = !{null, !144, !144}
!144 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !58, size: 64)
!145 = !DISubprogram(name: "_S_propagate_on_copy_assign", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIiEE27_S_propagate_on_copy_assignEv", scope: !44, file: !43, line: 100, type: !146, isLocal: false, isDefinition: false, scopeLine: 100, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!146 = !DISubroutineType(types: !147)
!147 = !{!13}
!148 = !DISubprogram(name: "_S_propagate_on_move_assign", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIiEE27_S_propagate_on_move_assignEv", scope: !44, file: !43, line: 103, type: !146, isLocal: false, isDefinition: false, scopeLine: 103, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!149 = !DISubprogram(name: "_S_propagate_on_swap", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIiEE20_S_propagate_on_swapEv", scope: !44, file: !43, line: 106, type: !146, isLocal: false, isDefinition: false, scopeLine: 106, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!150 = !DISubprogram(name: "_S_always_equal", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIiEE15_S_always_equalEv", scope: !44, file: !43, line: 109, type: !146, isLocal: false, isDefinition: false, scopeLine: 109, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!151 = !DISubprogram(name: "_S_nothrow_move", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIiEE15_S_nothrow_moveEv", scope: !44, file: !43, line: 112, type: !146, isLocal: false, isDefinition: false, scopeLine: 112, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!152 = !DIDerivedType(tag: DW_TAG_member, name: "_M_finish", scope: !37, file: !33, line: 85, baseType: !41, size: 64, offset: 64)
!153 = !DIDerivedType(tag: DW_TAG_member, name: "_M_end_of_storage", scope: !37, file: !33, line: 86, baseType: !41, size: 64, offset: 128)
!154 = !DISubprogram(name: "_Vector_impl", scope: !37, file: !33, line: 88, type: !155, isLocal: false, isDefinition: false, scopeLine: 88, flags: DIFlagPrototyped, isOptimized: true)
!155 = !DISubroutineType(types: !156)
!156 = !{null, !157}
!157 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !37, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!158 = !DISubprogram(name: "_Vector_impl", scope: !37, file: !33, line: 92, type: !159, isLocal: false, isDefinition: false, scopeLine: 92, flags: DIFlagPrototyped, isOptimized: true)
!159 = !DISubroutineType(types: !160)
!160 = !{null, !157, !161}
!161 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !162, size: 64)
!162 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !32)
!163 = !DISubprogram(name: "_Vector_impl", scope: !37, file: !33, line: 97, type: !164, isLocal: false, isDefinition: false, scopeLine: 97, flags: DIFlagPrototyped, isOptimized: true)
!164 = !DISubroutineType(types: !165)
!165 = !{null, !157, !166}
!166 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !32, size: 64)
!167 = !DISubprogram(name: "_M_swap_data", linkageName: "_ZNSt12_Vector_baseIiSaIiEE12_Vector_impl12_M_swap_dataERS2_", scope: !37, file: !33, line: 103, type: !168, isLocal: false, isDefinition: false, scopeLine: 103, flags: DIFlagPrototyped, isOptimized: true)
!168 = !DISubroutineType(types: !169)
!169 = !{null, !157, !170}
!170 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !37, size: 64)
!171 = !DISubprogram(name: "_M_get_Tp_allocator", linkageName: "_ZNSt12_Vector_baseIiSaIiEE19_M_get_Tp_allocatorEv", scope: !34, file: !33, line: 115, type: !172, isLocal: false, isDefinition: false, scopeLine: 115, flags: DIFlagPrototyped, isOptimized: true)
!172 = !DISubroutineType(types: !173)
!173 = !{!174, !175}
!174 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !32, size: 64)
!175 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !34, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!176 = !DISubprogram(name: "_M_get_Tp_allocator", linkageName: "_ZNKSt12_Vector_baseIiSaIiEE19_M_get_Tp_allocatorEv", scope: !34, file: !33, line: 119, type: !177, isLocal: false, isDefinition: false, scopeLine: 119, flags: DIFlagPrototyped, isOptimized: true)
!177 = !DISubroutineType(types: !178)
!178 = !{!161, !179}
!179 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !180, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!180 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !34)
!181 = !DISubprogram(name: "get_allocator", linkageName: "_ZNKSt12_Vector_baseIiSaIiEE13get_allocatorEv", scope: !34, file: !33, line: 123, type: !182, isLocal: false, isDefinition: false, scopeLine: 123, flags: DIFlagPrototyped, isOptimized: true)
!182 = !DISubroutineType(types: !183)
!183 = !{!184, !179}
!184 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !34, file: !33, line: 112, baseType: !58)
!185 = !DISubprogram(name: "_Vector_base", scope: !34, file: !33, line: 126, type: !186, isLocal: false, isDefinition: false, scopeLine: 126, flags: DIFlagPrototyped, isOptimized: true)
!186 = !DISubroutineType(types: !187)
!187 = !{null, !175}
!188 = !DISubprogram(name: "_Vector_base", scope: !34, file: !33, line: 129, type: !189, isLocal: false, isDefinition: false, scopeLine: 129, flags: DIFlagPrototyped, isOptimized: true)
!189 = !DISubroutineType(types: !190)
!190 = !{null, !175, !191}
!191 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !192, size: 64)
!192 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !184)
!193 = !DISubprogram(name: "_Vector_base", scope: !34, file: !33, line: 132, type: !194, isLocal: false, isDefinition: false, scopeLine: 132, flags: DIFlagPrototyped, isOptimized: true)
!194 = !DISubroutineType(types: !195)
!195 = !{null, !175, !96}
!196 = !DISubprogram(name: "_Vector_base", scope: !34, file: !33, line: 136, type: !197, isLocal: false, isDefinition: false, scopeLine: 136, flags: DIFlagPrototyped, isOptimized: true)
!197 = !DISubroutineType(types: !198)
!198 = !{null, !175, !96, !191}
!199 = !DISubprogram(name: "_Vector_base", scope: !34, file: !33, line: 141, type: !200, isLocal: false, isDefinition: false, scopeLine: 141, flags: DIFlagPrototyped, isOptimized: true)
!200 = !DISubroutineType(types: !201)
!201 = !{null, !175, !166}
!202 = !DISubprogram(name: "_Vector_base", scope: !34, file: !33, line: 144, type: !203, isLocal: false, isDefinition: false, scopeLine: 144, flags: DIFlagPrototyped, isOptimized: true)
!203 = !DISubroutineType(types: !204)
!204 = !{null, !175, !205}
!205 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !34, size: 64)
!206 = !DISubprogram(name: "_Vector_base", scope: !34, file: !33, line: 148, type: !207, isLocal: false, isDefinition: false, scopeLine: 148, flags: DIFlagPrototyped, isOptimized: true)
!207 = !DISubroutineType(types: !208)
!208 = !{null, !175, !205, !191}
!209 = !DISubprogram(name: "~_Vector_base", scope: !34, file: !33, line: 161, type: !186, isLocal: false, isDefinition: false, scopeLine: 161, flags: DIFlagPrototyped, isOptimized: true)
!210 = !DISubprogram(name: "_M_allocate", linkageName: "_ZNSt12_Vector_baseIiSaIiEE11_M_allocateEm", scope: !34, file: !33, line: 169, type: !211, isLocal: false, isDefinition: false, scopeLine: 169, flags: DIFlagPrototyped, isOptimized: true)
!211 = !DISubroutineType(types: !212)
!212 = !{!41, !175, !96}
!213 = !DISubprogram(name: "_M_deallocate", linkageName: "_ZNSt12_Vector_baseIiSaIiEE13_M_deallocateEPim", scope: !34, file: !33, line: 176, type: !214, isLocal: false, isDefinition: false, scopeLine: 176, flags: DIFlagPrototyped, isOptimized: true)
!214 = !DISubroutineType(types: !215)
!215 = !{null, !175, !41, !96}
!216 = !DISubprogram(name: "_M_create_storage", linkageName: "_ZNSt12_Vector_baseIiSaIiEE17_M_create_storageEm", scope: !34, file: !33, line: 185, type: !194, isLocal: false, isDefinition: false, scopeLine: 185, flags: DIFlagPrivate | DIFlagPrototyped, isOptimized: true)
!217 = !{!108, !137}
!218 = !DIDerivedType(tag: DW_TAG_typedef, name: "other", scope: !219, file: !43, line: 117, baseType: !220)
!219 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "rebind<int>", scope: !44, file: !43, line: 116, size: 8, elements: !25, templateParams: !107, identifier: "_ZTSN9__gnu_cxx14__alloc_traitsISaIiEE6rebindIiEE")
!220 = !DIDerivedType(tag: DW_TAG_typedef, name: "rebind_alloc<int>", scope: !48, file: !49, line: 422, baseType: !58)
!221 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", file: !33, line: 242, baseType: !96)
!222 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!223 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !162, size: 64)
!224 = !DIDerivedType(tag: DW_TAG_typedef, name: "iterator", scope: !225, file: !33, line: 237, baseType: !510)
!225 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "vector<int, std::allocator<int> >", scope: !2, file: !33, line: 216, size: 192, elements: !226, templateParams: !217, identifier: "_ZTSSt6vectorIiSaIiEE")
!226 = !{!227, !228, !232, !238, !241, !247, !252, !256, !259, !262, !267, !268, !272, !275, !278, !281, !284, !287, !350, !351, !352, !357, !362, !363, !364, !365, !366, !367, !368, !371, !372, !375, !376, !377, !378, !381, !382, !390, !397, !400, !401, !402, !405, !408, !409, !410, !413, !416, !419, !423, !424, !427, !430, !433, !436, !439, !442, !445, !446, !447, !448, !449, !452, !453, !456, !457, !458, !465, !469, !472, !475, !494}
!227 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !225, baseType: !34, flags: DIFlagProtected)
!228 = !DISubprogram(name: "vector", scope: !225, file: !33, line: 259, type: !229, isLocal: false, isDefinition: false, scopeLine: 259, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!229 = !DISubroutineType(types: !230)
!230 = !{null, !231}
!231 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !225, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!232 = !DISubprogram(name: "vector", scope: !225, file: !33, line: 270, type: !233, isLocal: false, isDefinition: false, scopeLine: 270, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!233 = !DISubroutineType(types: !234)
!234 = !{null, !231, !235}
!235 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !236, size: 64)
!236 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !237)
!237 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !225, file: !33, line: 244, baseType: !58)
!238 = !DISubprogram(name: "vector", scope: !225, file: !33, line: 283, type: !239, isLocal: false, isDefinition: false, scopeLine: 283, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!239 = !DISubroutineType(types: !240)
!240 = !{null, !231, !221, !235}
!241 = !DISubprogram(name: "vector", scope: !225, file: !33, line: 295, type: !242, isLocal: false, isDefinition: false, scopeLine: 295, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!242 = !DISubroutineType(types: !243)
!243 = !{null, !231, !221, !244, !235}
!244 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !245, size: 64)
!245 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !246)
!246 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !225, file: !33, line: 232, baseType: !11)
!247 = !DISubprogram(name: "vector", scope: !225, file: !33, line: 326, type: !248, isLocal: false, isDefinition: false, scopeLine: 326, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!248 = !DISubroutineType(types: !249)
!249 = !{null, !231, !250}
!250 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !251, size: 64)
!251 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !225)
!252 = !DISubprogram(name: "vector", scope: !225, file: !33, line: 344, type: !253, isLocal: false, isDefinition: false, scopeLine: 344, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!253 = !DISubroutineType(types: !254)
!254 = !{null, !231, !255}
!255 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !225, size: 64)
!256 = !DISubprogram(name: "vector", scope: !225, file: !33, line: 348, type: !257, isLocal: false, isDefinition: false, scopeLine: 348, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!257 = !DISubroutineType(types: !258)
!258 = !{null, !231, !250, !235}
!259 = !DISubprogram(name: "vector", scope: !225, file: !33, line: 358, type: !260, isLocal: false, isDefinition: false, scopeLine: 358, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!260 = !DISubroutineType(types: !261)
!261 = !{null, !231, !255, !235}
!262 = !DISubprogram(name: "vector", scope: !225, file: !33, line: 383, type: !263, isLocal: false, isDefinition: false, scopeLine: 383, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!263 = !DISubroutineType(types: !264)
!264 = !{null, !231, !265, !235}
!265 = !DICompositeType(tag: DW_TAG_class_type, name: "initializer_list<int>", scope: !2, file: !266, line: 47, flags: DIFlagFwdDecl, identifier: "_ZTSSt16initializer_listIiE")
!266 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/initializer_list", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!267 = !DISubprogram(name: "~vector", scope: !225, file: !33, line: 433, type: !229, isLocal: false, isDefinition: false, scopeLine: 433, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!268 = !DISubprogram(name: "operator=", linkageName: "_ZNSt6vectorIiSaIiEEaSERKS1_", scope: !225, file: !33, line: 447, type: !269, isLocal: false, isDefinition: false, scopeLine: 447, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!269 = !DISubroutineType(types: !270)
!270 = !{!271, !231, !250}
!271 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !225, size: 64)
!272 = !DISubprogram(name: "operator=", linkageName: "_ZNSt6vectorIiSaIiEEaSEOS1_", scope: !225, file: !33, line: 461, type: !273, isLocal: false, isDefinition: false, scopeLine: 461, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!273 = !DISubroutineType(types: !274)
!274 = !{!271, !231, !255}
!275 = !DISubprogram(name: "operator=", linkageName: "_ZNSt6vectorIiSaIiEEaSESt16initializer_listIiE", scope: !225, file: !33, line: 482, type: !276, isLocal: false, isDefinition: false, scopeLine: 482, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!276 = !DISubroutineType(types: !277)
!277 = !{!271, !231, !265}
!278 = !DISubprogram(name: "assign", linkageName: "_ZNSt6vectorIiSaIiEE6assignEmRKi", scope: !225, file: !33, line: 501, type: !279, isLocal: false, isDefinition: false, scopeLine: 501, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!279 = !DISubroutineType(types: !280)
!280 = !{null, !231, !221, !244}
!281 = !DISubprogram(name: "assign", linkageName: "_ZNSt6vectorIiSaIiEE6assignESt16initializer_listIiE", scope: !225, file: !33, line: 546, type: !282, isLocal: false, isDefinition: false, scopeLine: 546, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!282 = !DISubroutineType(types: !283)
!283 = !{null, !231, !265}
!284 = !DISubprogram(name: "begin", linkageName: "_ZNSt6vectorIiSaIiEE5beginEv", scope: !225, file: !33, line: 563, type: !285, isLocal: false, isDefinition: false, scopeLine: 563, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!285 = !DISubroutineType(types: !286)
!286 = !{!224, !231}
!287 = !DISubprogram(name: "begin", linkageName: "_ZNKSt6vectorIiSaIiEE5beginEv", scope: !225, file: !33, line: 572, type: !288, isLocal: false, isDefinition: false, scopeLine: 572, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!288 = !DISubroutineType(types: !289)
!289 = !{!290, !349}
!290 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_iterator", scope: !225, file: !33, line: 239, baseType: !291)
!291 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "__normal_iterator<const int *, std::vector<int, std::allocator<int> > >", scope: !45, file: !292, line: 760, size: 64, elements: !293, templateParams: !347, identifier: "_ZTSN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEE")
!292 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/bits/stl_iterator.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!293 = !{!294, !295, !299, !304, !315, !320, !324, !327, !328, !329, !336, !339, !342, !343, !344}
!294 = !DIDerivedType(tag: DW_TAG_member, name: "_M_current", scope: !291, file: !292, line: 763, baseType: !88, size: 64, flags: DIFlagProtected)
!295 = !DISubprogram(name: "__normal_iterator", scope: !291, file: !292, line: 775, type: !296, isLocal: false, isDefinition: false, scopeLine: 775, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!296 = !DISubroutineType(types: !297)
!297 = !{null, !298}
!298 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !291, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!299 = !DISubprogram(name: "__normal_iterator", scope: !291, file: !292, line: 779, type: !300, isLocal: false, isDefinition: false, scopeLine: 779, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!300 = !DISubroutineType(types: !301)
!301 = !{null, !298, !302}
!302 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !303, size: 64)
!303 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !88)
!304 = !DISubprogram(name: "operator*", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEdeEv", scope: !291, file: !292, line: 792, type: !305, isLocal: false, isDefinition: false, scopeLine: 792, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!305 = !DISubroutineType(types: !306)
!306 = !{!307, !313}
!307 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !291, file: !292, line: 772, baseType: !308)
!308 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !310, file: !309, line: 195, baseType: !91)
!309 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/bits/stl_iterator_base_types.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!310 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "iterator_traits<const int *>", scope: !2, file: !309, line: 189, size: 8, elements: !25, templateParams: !311, identifier: "_ZTSSt15iterator_traitsIPKiE")
!311 = !{!312}
!312 = !DITemplateTypeParameter(name: "_Iterator", type: !88)
!313 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !314, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!314 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !291)
!315 = !DISubprogram(name: "operator->", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEptEv", scope: !291, file: !292, line: 796, type: !316, isLocal: false, isDefinition: false, scopeLine: 796, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!316 = !DISubroutineType(types: !317)
!317 = !{!318, !313}
!318 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !291, file: !292, line: 773, baseType: !319)
!319 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !310, file: !309, line: 194, baseType: !88)
!320 = !DISubprogram(name: "operator++", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEppEv", scope: !291, file: !292, line: 800, type: !321, isLocal: false, isDefinition: false, scopeLine: 800, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!321 = !DISubroutineType(types: !322)
!322 = !{!323, !298}
!323 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !291, size: 64)
!324 = !DISubprogram(name: "operator++", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEppEi", scope: !291, file: !292, line: 807, type: !325, isLocal: false, isDefinition: false, scopeLine: 807, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!325 = !DISubroutineType(types: !326)
!326 = !{!291, !298, !11}
!327 = !DISubprogram(name: "operator--", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEmmEv", scope: !291, file: !292, line: 812, type: !321, isLocal: false, isDefinition: false, scopeLine: 812, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!328 = !DISubprogram(name: "operator--", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEmmEi", scope: !291, file: !292, line: 819, type: !325, isLocal: false, isDefinition: false, scopeLine: 819, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!329 = !DISubprogram(name: "operator[]", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEixEl", scope: !291, file: !292, line: 824, type: !330, isLocal: false, isDefinition: false, scopeLine: 824, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!330 = !DISubroutineType(types: !331)
!331 = !{!307, !313, !332}
!332 = !DIDerivedType(tag: DW_TAG_typedef, name: "difference_type", scope: !291, file: !292, line: 771, baseType: !333)
!333 = !DIDerivedType(tag: DW_TAG_typedef, name: "difference_type", scope: !310, file: !309, line: 193, baseType: !334)
!334 = !DIDerivedType(tag: DW_TAG_typedef, name: "ptrdiff_t", scope: !2, file: !97, line: 232, baseType: !335)
!335 = !DIBasicType(name: "long int", size: 64, encoding: DW_ATE_signed)
!336 = !DISubprogram(name: "operator+=", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEpLEl", scope: !291, file: !292, line: 828, type: !337, isLocal: false, isDefinition: false, scopeLine: 828, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!337 = !DISubroutineType(types: !338)
!338 = !{!323, !298, !332}
!339 = !DISubprogram(name: "operator+", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEplEl", scope: !291, file: !292, line: 832, type: !340, isLocal: false, isDefinition: false, scopeLine: 832, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!340 = !DISubroutineType(types: !341)
!341 = !{!291, !313, !332}
!342 = !DISubprogram(name: "operator-=", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEmIEl", scope: !291, file: !292, line: 836, type: !337, isLocal: false, isDefinition: false, scopeLine: 836, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!343 = !DISubprogram(name: "operator-", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEmiEl", scope: !291, file: !292, line: 840, type: !340, isLocal: false, isDefinition: false, scopeLine: 840, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!344 = !DISubprogram(name: "base", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEE4baseEv", scope: !291, file: !292, line: 844, type: !345, isLocal: false, isDefinition: false, scopeLine: 844, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!345 = !DISubroutineType(types: !346)
!346 = !{!302, !313}
!347 = !{!312, !348}
!348 = !DITemplateTypeParameter(name: "_Container", type: !225)
!349 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !251, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!350 = !DISubprogram(name: "end", linkageName: "_ZNSt6vectorIiSaIiEE3endEv", scope: !225, file: !33, line: 581, type: !285, isLocal: false, isDefinition: false, scopeLine: 581, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!351 = !DISubprogram(name: "end", linkageName: "_ZNKSt6vectorIiSaIiEE3endEv", scope: !225, file: !33, line: 590, type: !288, isLocal: false, isDefinition: false, scopeLine: 590, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!352 = !DISubprogram(name: "rbegin", linkageName: "_ZNSt6vectorIiSaIiEE6rbeginEv", scope: !225, file: !33, line: 599, type: !353, isLocal: false, isDefinition: false, scopeLine: 599, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!353 = !DISubroutineType(types: !354)
!354 = !{!355, !231}
!355 = !DIDerivedType(tag: DW_TAG_typedef, name: "reverse_iterator", scope: !225, file: !33, line: 241, baseType: !356)
!356 = !DICompositeType(tag: DW_TAG_class_type, name: "reverse_iterator<__gnu_cxx::__normal_iterator<int *, std::vector<int, std::allocator<int> > > >", scope: !2, file: !292, line: 101, flags: DIFlagFwdDecl, identifier: "_ZTSSt16reverse_iteratorIN9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEEE")
!357 = !DISubprogram(name: "rbegin", linkageName: "_ZNKSt6vectorIiSaIiEE6rbeginEv", scope: !225, file: !33, line: 608, type: !358, isLocal: false, isDefinition: false, scopeLine: 608, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!358 = !DISubroutineType(types: !359)
!359 = !{!360, !349}
!360 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reverse_iterator", scope: !225, file: !33, line: 240, baseType: !361)
!361 = !DICompositeType(tag: DW_TAG_class_type, name: "reverse_iterator<__gnu_cxx::__normal_iterator<const int *, std::vector<int, std::allocator<int> > > >", scope: !2, file: !292, line: 101, flags: DIFlagFwdDecl, identifier: "_ZTSSt16reverse_iteratorIN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEEE")
!362 = !DISubprogram(name: "rend", linkageName: "_ZNSt6vectorIiSaIiEE4rendEv", scope: !225, file: !33, line: 617, type: !353, isLocal: false, isDefinition: false, scopeLine: 617, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!363 = !DISubprogram(name: "rend", linkageName: "_ZNKSt6vectorIiSaIiEE4rendEv", scope: !225, file: !33, line: 626, type: !358, isLocal: false, isDefinition: false, scopeLine: 626, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!364 = !DISubprogram(name: "cbegin", linkageName: "_ZNKSt6vectorIiSaIiEE6cbeginEv", scope: !225, file: !33, line: 636, type: !288, isLocal: false, isDefinition: false, scopeLine: 636, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!365 = !DISubprogram(name: "cend", linkageName: "_ZNKSt6vectorIiSaIiEE4cendEv", scope: !225, file: !33, line: 645, type: !288, isLocal: false, isDefinition: false, scopeLine: 645, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!366 = !DISubprogram(name: "crbegin", linkageName: "_ZNKSt6vectorIiSaIiEE7crbeginEv", scope: !225, file: !33, line: 654, type: !358, isLocal: false, isDefinition: false, scopeLine: 654, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!367 = !DISubprogram(name: "crend", linkageName: "_ZNKSt6vectorIiSaIiEE5crendEv", scope: !225, file: !33, line: 663, type: !358, isLocal: false, isDefinition: false, scopeLine: 663, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!368 = !DISubprogram(name: "size", linkageName: "_ZNKSt6vectorIiSaIiEE4sizeEv", scope: !225, file: !33, line: 670, type: !369, isLocal: false, isDefinition: false, scopeLine: 670, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!369 = !DISubroutineType(types: !370)
!370 = !{!221, !349}
!371 = !DISubprogram(name: "max_size", linkageName: "_ZNKSt6vectorIiSaIiEE8max_sizeEv", scope: !225, file: !33, line: 675, type: !369, isLocal: false, isDefinition: false, scopeLine: 675, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!372 = !DISubprogram(name: "resize", linkageName: "_ZNSt6vectorIiSaIiEE6resizeEm", scope: !225, file: !33, line: 689, type: !373, isLocal: false, isDefinition: false, scopeLine: 689, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!373 = !DISubroutineType(types: !374)
!374 = !{null, !231, !221}
!375 = !DISubprogram(name: "resize", linkageName: "_ZNSt6vectorIiSaIiEE6resizeEmRKi", scope: !225, file: !33, line: 709, type: !279, isLocal: false, isDefinition: false, scopeLine: 709, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!376 = !DISubprogram(name: "shrink_to_fit", linkageName: "_ZNSt6vectorIiSaIiEE13shrink_to_fitEv", scope: !225, file: !33, line: 741, type: !229, isLocal: false, isDefinition: false, scopeLine: 741, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!377 = !DISubprogram(name: "capacity", linkageName: "_ZNKSt6vectorIiSaIiEE8capacityEv", scope: !225, file: !33, line: 750, type: !369, isLocal: false, isDefinition: false, scopeLine: 750, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!378 = !DISubprogram(name: "empty", linkageName: "_ZNKSt6vectorIiSaIiEE5emptyEv", scope: !225, file: !33, line: 759, type: !379, isLocal: false, isDefinition: false, scopeLine: 759, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!379 = !DISubroutineType(types: !380)
!380 = !{!13, !349}
!381 = !DISubprogram(name: "reserve", linkageName: "_ZNSt6vectorIiSaIiEE7reserveEm", scope: !225, file: !33, line: 780, type: !373, isLocal: false, isDefinition: false, scopeLine: 780, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!382 = !DISubprogram(name: "operator[]", linkageName: "_ZNSt6vectorIiSaIiEEixEm", scope: !225, file: !33, line: 795, type: !383, isLocal: false, isDefinition: false, scopeLine: 795, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!383 = !DISubroutineType(types: !384)
!384 = !{!385, !231, !221}
!385 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !225, file: !33, line: 235, baseType: !386)
!386 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !44, file: !43, line: 64, baseType: !387)
!387 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !388, size: 64)
!388 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !44, file: !43, line: 58, baseType: !389)
!389 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !48, file: !49, line: 389, baseType: !11)
!390 = !DISubprogram(name: "operator[]", linkageName: "_ZNKSt6vectorIiSaIiEEixEm", scope: !225, file: !33, line: 813, type: !391, isLocal: false, isDefinition: false, scopeLine: 813, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!391 = !DISubroutineType(types: !392)
!392 = !{!393, !349, !221}
!393 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reference", scope: !225, file: !33, line: 236, baseType: !394)
!394 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reference", scope: !44, file: !43, line: 65, baseType: !395)
!395 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !396, size: 64)
!396 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !388)
!397 = !DISubprogram(name: "_M_range_check", linkageName: "_ZNKSt6vectorIiSaIiEE14_M_range_checkEm", scope: !225, file: !33, line: 822, type: !398, isLocal: false, isDefinition: false, scopeLine: 822, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!398 = !DISubroutineType(types: !399)
!399 = !{null, !349, !221}
!400 = !DISubprogram(name: "at", linkageName: "_ZNSt6vectorIiSaIiEE2atEm", scope: !225, file: !33, line: 844, type: !383, isLocal: false, isDefinition: false, scopeLine: 844, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!401 = !DISubprogram(name: "at", linkageName: "_ZNKSt6vectorIiSaIiEE2atEm", scope: !225, file: !33, line: 862, type: !391, isLocal: false, isDefinition: false, scopeLine: 862, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!402 = !DISubprogram(name: "front", linkageName: "_ZNSt6vectorIiSaIiEE5frontEv", scope: !225, file: !33, line: 873, type: !403, isLocal: false, isDefinition: false, scopeLine: 873, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!403 = !DISubroutineType(types: !404)
!404 = !{!385, !231}
!405 = !DISubprogram(name: "front", linkageName: "_ZNKSt6vectorIiSaIiEE5frontEv", scope: !225, file: !33, line: 884, type: !406, isLocal: false, isDefinition: false, scopeLine: 884, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!406 = !DISubroutineType(types: !407)
!407 = !{!393, !349}
!408 = !DISubprogram(name: "back", linkageName: "_ZNSt6vectorIiSaIiEE4backEv", scope: !225, file: !33, line: 895, type: !403, isLocal: false, isDefinition: false, scopeLine: 895, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!409 = !DISubprogram(name: "back", linkageName: "_ZNKSt6vectorIiSaIiEE4backEv", scope: !225, file: !33, line: 906, type: !406, isLocal: false, isDefinition: false, scopeLine: 906, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!410 = !DISubprogram(name: "data", linkageName: "_ZNSt6vectorIiSaIiEE4dataEv", scope: !225, file: !33, line: 920, type: !411, isLocal: false, isDefinition: false, scopeLine: 920, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!411 = !DISubroutineType(types: !412)
!412 = !{!55, !231}
!413 = !DISubprogram(name: "data", linkageName: "_ZNKSt6vectorIiSaIiEE4dataEv", scope: !225, file: !33, line: 924, type: !414, isLocal: false, isDefinition: false, scopeLine: 924, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!414 = !DISubroutineType(types: !415)
!415 = !{!88, !349}
!416 = !DISubprogram(name: "push_back", linkageName: "_ZNSt6vectorIiSaIiEE9push_backERKi", scope: !225, file: !33, line: 939, type: !417, isLocal: false, isDefinition: false, scopeLine: 939, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!417 = !DISubroutineType(types: !418)
!418 = !{null, !231, !244}
!419 = !DISubprogram(name: "push_back", linkageName: "_ZNSt6vectorIiSaIiEE9push_backEOi", scope: !225, file: !33, line: 953, type: !420, isLocal: false, isDefinition: false, scopeLine: 953, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!420 = !DISubroutineType(types: !421)
!421 = !{null, !231, !422}
!422 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !246, size: 64)
!423 = !DISubprogram(name: "pop_back", linkageName: "_ZNSt6vectorIiSaIiEE8pop_backEv", scope: !225, file: !33, line: 975, type: !229, isLocal: false, isDefinition: false, scopeLine: 975, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!424 = !DISubprogram(name: "insert", linkageName: "_ZNSt6vectorIiSaIiEE6insertEN9__gnu_cxx17__normal_iteratorIPKiS1_EERS4_", scope: !225, file: !33, line: 1012, type: !425, isLocal: false, isDefinition: false, scopeLine: 1012, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!425 = !DISubroutineType(types: !426)
!426 = !{!224, !231, !290, !244}
!427 = !DISubprogram(name: "insert", linkageName: "_ZNSt6vectorIiSaIiEE6insertEN9__gnu_cxx17__normal_iteratorIPKiS1_EEOi", scope: !225, file: !33, line: 1042, type: !428, isLocal: false, isDefinition: false, scopeLine: 1042, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!428 = !DISubroutineType(types: !429)
!429 = !{!224, !231, !290, !422}
!430 = !DISubprogram(name: "insert", linkageName: "_ZNSt6vectorIiSaIiEE6insertEN9__gnu_cxx17__normal_iteratorIPKiS1_EESt16initializer_listIiE", scope: !225, file: !33, line: 1059, type: !431, isLocal: false, isDefinition: false, scopeLine: 1059, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!431 = !DISubroutineType(types: !432)
!432 = !{!224, !231, !290, !265}
!433 = !DISubprogram(name: "insert", linkageName: "_ZNSt6vectorIiSaIiEE6insertEN9__gnu_cxx17__normal_iteratorIPKiS1_EEmRS4_", scope: !225, file: !33, line: 1084, type: !434, isLocal: false, isDefinition: false, scopeLine: 1084, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!434 = !DISubroutineType(types: !435)
!435 = !{!224, !231, !290, !221, !244}
!436 = !DISubprogram(name: "erase", linkageName: "_ZNSt6vectorIiSaIiEE5eraseEN9__gnu_cxx17__normal_iteratorIPKiS1_EE", scope: !225, file: !33, line: 1179, type: !437, isLocal: false, isDefinition: false, scopeLine: 1179, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!437 = !DISubroutineType(types: !438)
!438 = !{!224, !231, !290}
!439 = !DISubprogram(name: "erase", linkageName: "_ZNSt6vectorIiSaIiEE5eraseEN9__gnu_cxx17__normal_iteratorIPKiS1_EES6_", scope: !225, file: !33, line: 1206, type: !440, isLocal: false, isDefinition: false, scopeLine: 1206, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!440 = !DISubroutineType(types: !441)
!441 = !{!224, !231, !290, !290}
!442 = !DISubprogram(name: "swap", linkageName: "_ZNSt6vectorIiSaIiEE4swapERS1_", scope: !225, file: !33, line: 1229, type: !443, isLocal: false, isDefinition: false, scopeLine: 1229, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!443 = !DISubroutineType(types: !444)
!444 = !{null, !231, !271}
!445 = !DISubprogram(name: "clear", linkageName: "_ZNSt6vectorIiSaIiEE5clearEv", scope: !225, file: !33, line: 1247, type: !229, isLocal: false, isDefinition: false, scopeLine: 1247, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!446 = !DISubprogram(name: "_M_fill_initialize", linkageName: "_ZNSt6vectorIiSaIiEE18_M_fill_initializeEmRKi", scope: !225, file: !33, line: 1334, type: !279, isLocal: false, isDefinition: false, scopeLine: 1334, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!447 = !DISubprogram(name: "_M_default_initialize", linkageName: "_ZNSt6vectorIiSaIiEE21_M_default_initializeEm", scope: !225, file: !33, line: 1344, type: !373, isLocal: false, isDefinition: false, scopeLine: 1344, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!448 = !DISubprogram(name: "_M_fill_assign", linkageName: "_ZNSt6vectorIiSaIiEE14_M_fill_assignEmRKi", scope: !225, file: !33, line: 1386, type: !279, isLocal: false, isDefinition: false, scopeLine: 1386, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!449 = !DISubprogram(name: "_M_fill_insert", linkageName: "_ZNSt6vectorIiSaIiEE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPiS1_EEmRKi", scope: !225, file: !33, line: 1425, type: !450, isLocal: false, isDefinition: false, scopeLine: 1425, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!450 = !DISubroutineType(types: !451)
!451 = !{null, !231, !224, !221, !244}
!452 = !DISubprogram(name: "_M_default_append", linkageName: "_ZNSt6vectorIiSaIiEE17_M_default_appendEm", scope: !225, file: !33, line: 1430, type: !373, isLocal: false, isDefinition: false, scopeLine: 1430, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!453 = !DISubprogram(name: "_M_shrink_to_fit", linkageName: "_ZNSt6vectorIiSaIiEE16_M_shrink_to_fitEv", scope: !225, file: !33, line: 1433, type: !454, isLocal: false, isDefinition: false, scopeLine: 1433, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!454 = !DISubroutineType(types: !455)
!455 = !{!13, !231}
!456 = !DISubprogram(name: "_M_insert_rval", linkageName: "_ZNSt6vectorIiSaIiEE14_M_insert_rvalEN9__gnu_cxx17__normal_iteratorIPKiS1_EEOi", scope: !225, file: !33, line: 1482, type: !428, isLocal: false, isDefinition: false, scopeLine: 1482, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!457 = !DISubprogram(name: "_M_emplace_aux", linkageName: "_ZNSt6vectorIiSaIiEE14_M_emplace_auxEN9__gnu_cxx17__normal_iteratorIPKiS1_EEOi", scope: !225, file: !33, line: 1491, type: !428, isLocal: false, isDefinition: false, scopeLine: 1491, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!458 = !DISubprogram(name: "_M_check_len", linkageName: "_ZNKSt6vectorIiSaIiEE12_M_check_lenEmPKc", scope: !225, file: !33, line: 1497, type: !459, isLocal: false, isDefinition: false, scopeLine: 1497, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!459 = !DISubroutineType(types: !460)
!460 = !{!461, !349, !221, !462}
!461 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !225, file: !33, line: 242, baseType: !96)
!462 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !463, size: 64)
!463 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !464)
!464 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!465 = !DISubprogram(name: "_M_erase_at_end", linkageName: "_ZNSt6vectorIiSaIiEE15_M_erase_at_endEPi", scope: !225, file: !33, line: 1511, type: !466, isLocal: false, isDefinition: false, scopeLine: 1511, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!466 = !DISubroutineType(types: !467)
!467 = !{null, !231, !468}
!468 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !225, file: !33, line: 233, baseType: !41)
!469 = !DISubprogram(name: "_M_erase", linkageName: "_ZNSt6vectorIiSaIiEE8_M_eraseEN9__gnu_cxx17__normal_iteratorIPiS1_EE", scope: !225, file: !33, line: 1518, type: !470, isLocal: false, isDefinition: false, scopeLine: 1518, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!470 = !DISubroutineType(types: !471)
!471 = !{!224, !231, !224}
!472 = !DISubprogram(name: "_M_erase", linkageName: "_ZNSt6vectorIiSaIiEE8_M_eraseEN9__gnu_cxx17__normal_iteratorIPiS1_EES5_", scope: !225, file: !33, line: 1521, type: !473, isLocal: false, isDefinition: false, scopeLine: 1521, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!473 = !DISubroutineType(types: !474)
!474 = !{!224, !231, !224, !224}
!475 = !DISubprogram(name: "_M_move_assign", linkageName: "_ZNSt6vectorIiSaIiEE14_M_move_assignEOS1_St17integral_constantIbLb1EE", scope: !225, file: !33, line: 1529, type: !476, isLocal: false, isDefinition: false, scopeLine: 1529, flags: DIFlagPrototyped, isOptimized: true)
!476 = !DISubroutineType(types: !477)
!477 = !{null, !231, !255, !478}
!478 = !DIDerivedType(tag: DW_TAG_typedef, name: "true_type", scope: !2, file: !479, line: 87, baseType: !480)
!479 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/type_traits", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!480 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "integral_constant<bool, true>", scope: !2, file: !479, line: 69, size: 8, elements: !481, templateParams: !491, identifier: "_ZTSSt17integral_constantIbLb1EE")
!481 = !{!482, !484, !490}
!482 = !DIDerivedType(tag: DW_TAG_member, name: "value", scope: !480, file: !479, line: 71, baseType: !483, flags: DIFlagStaticMember, extraData: i1 true)
!483 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !13)
!484 = !DISubprogram(name: "operator bool", linkageName: "_ZNKSt17integral_constantIbLb1EEcvbEv", scope: !480, file: !479, line: 74, type: !485, isLocal: false, isDefinition: false, scopeLine: 74, flags: DIFlagPrototyped, isOptimized: true)
!485 = !DISubroutineType(types: !486)
!486 = !{!487, !488}
!487 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !480, file: !479, line: 72, baseType: !13)
!488 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !489, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!489 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !480)
!490 = !DISubprogram(name: "operator()", linkageName: "_ZNKSt17integral_constantIbLb1EEclEv", scope: !480, file: !479, line: 79, type: !485, isLocal: false, isDefinition: false, scopeLine: 79, flags: DIFlagPrototyped, isOptimized: true)
!491 = !{!492, !493}
!492 = !DITemplateTypeParameter(name: "_Tp", type: !13)
!493 = !DITemplateValueParameter(name: "__v", type: !13, value: i8 1)
!494 = !DISubprogram(name: "_M_move_assign", linkageName: "_ZNSt6vectorIiSaIiEE14_M_move_assignEOS1_St17integral_constantIbLb0EE", scope: !225, file: !33, line: 1540, type: !495, isLocal: false, isDefinition: false, scopeLine: 1540, flags: DIFlagPrototyped, isOptimized: true)
!495 = !DISubroutineType(types: !496)
!496 = !{null, !231, !255, !497}
!497 = !DIDerivedType(tag: DW_TAG_typedef, name: "false_type", scope: !2, file: !479, line: 90, baseType: !498)
!498 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "integral_constant<bool, false>", scope: !2, file: !479, line: 69, size: 8, elements: !499, templateParams: !508, identifier: "_ZTSSt17integral_constantIbLb0EE")
!499 = !{!500, !501, !507}
!500 = !DIDerivedType(tag: DW_TAG_member, name: "value", scope: !498, file: !479, line: 71, baseType: !483, flags: DIFlagStaticMember, extraData: i1 false)
!501 = !DISubprogram(name: "operator bool", linkageName: "_ZNKSt17integral_constantIbLb0EEcvbEv", scope: !498, file: !479, line: 74, type: !502, isLocal: false, isDefinition: false, scopeLine: 74, flags: DIFlagPrototyped, isOptimized: true)
!502 = !DISubroutineType(types: !503)
!503 = !{!504, !505}
!504 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !498, file: !479, line: 72, baseType: !13)
!505 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !506, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!506 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !498)
!507 = !DISubprogram(name: "operator()", linkageName: "_ZNKSt17integral_constantIbLb0EEclEv", scope: !498, file: !479, line: 79, type: !502, isLocal: false, isDefinition: false, scopeLine: 79, flags: DIFlagPrototyped, isOptimized: true)
!508 = !{!492, !509}
!509 = !DITemplateValueParameter(name: "__v", type: !13, value: i8 0)
!510 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "__normal_iterator<int *, std::vector<int, std::allocator<int> > >", scope: !45, file: !292, line: 760, size: 64, elements: !511, templateParams: !562, identifier: "_ZTSN9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEE")
!511 = !{!512, !513, !517, !522, !532, !537, !541, !544, !545, !546, !551, !554, !557, !558, !559}
!512 = !DIDerivedType(tag: DW_TAG_member, name: "_M_current", scope: !510, file: !292, line: 763, baseType: !55, size: 64, flags: DIFlagProtected)
!513 = !DISubprogram(name: "__normal_iterator", scope: !510, file: !292, line: 775, type: !514, isLocal: false, isDefinition: false, scopeLine: 775, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!514 = !DISubroutineType(types: !515)
!515 = !{null, !516}
!516 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !510, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!517 = !DISubprogram(name: "__normal_iterator", scope: !510, file: !292, line: 779, type: !518, isLocal: false, isDefinition: false, scopeLine: 779, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!518 = !DISubroutineType(types: !519)
!519 = !{null, !516, !520}
!520 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !521, size: 64)
!521 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !55)
!522 = !DISubprogram(name: "operator*", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEdeEv", scope: !510, file: !292, line: 792, type: !523, isLocal: false, isDefinition: false, scopeLine: 792, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!523 = !DISubroutineType(types: !524)
!524 = !{!525, !530}
!525 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !510, file: !292, line: 772, baseType: !526)
!526 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !527, file: !309, line: 184, baseType: !83)
!527 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "iterator_traits<int *>", scope: !2, file: !309, line: 178, size: 8, elements: !25, templateParams: !528, identifier: "_ZTSSt15iterator_traitsIPiE")
!528 = !{!529}
!529 = !DITemplateTypeParameter(name: "_Iterator", type: !55)
!530 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !531, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!531 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !510)
!532 = !DISubprogram(name: "operator->", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEptEv", scope: !510, file: !292, line: 796, type: !533, isLocal: false, isDefinition: false, scopeLine: 796, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!533 = !DISubroutineType(types: !534)
!534 = !{!535, !530}
!535 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !510, file: !292, line: 773, baseType: !536)
!536 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !527, file: !309, line: 183, baseType: !55)
!537 = !DISubprogram(name: "operator++", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEppEv", scope: !510, file: !292, line: 800, type: !538, isLocal: false, isDefinition: false, scopeLine: 800, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!538 = !DISubroutineType(types: !539)
!539 = !{!540, !516}
!540 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !510, size: 64)
!541 = !DISubprogram(name: "operator++", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEppEi", scope: !510, file: !292, line: 807, type: !542, isLocal: false, isDefinition: false, scopeLine: 807, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!542 = !DISubroutineType(types: !543)
!543 = !{!510, !516, !11}
!544 = !DISubprogram(name: "operator--", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEmmEv", scope: !510, file: !292, line: 812, type: !538, isLocal: false, isDefinition: false, scopeLine: 812, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!545 = !DISubprogram(name: "operator--", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEmmEi", scope: !510, file: !292, line: 819, type: !542, isLocal: false, isDefinition: false, scopeLine: 819, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!546 = !DISubprogram(name: "operator[]", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEixEl", scope: !510, file: !292, line: 824, type: !547, isLocal: false, isDefinition: false, scopeLine: 824, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!547 = !DISubroutineType(types: !548)
!548 = !{!525, !530, !549}
!549 = !DIDerivedType(tag: DW_TAG_typedef, name: "difference_type", scope: !510, file: !292, line: 771, baseType: !550)
!550 = !DIDerivedType(tag: DW_TAG_typedef, name: "difference_type", scope: !527, file: !309, line: 182, baseType: !334)
!551 = !DISubprogram(name: "operator+=", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEpLEl", scope: !510, file: !292, line: 828, type: !552, isLocal: false, isDefinition: false, scopeLine: 828, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!552 = !DISubroutineType(types: !553)
!553 = !{!540, !516, !549}
!554 = !DISubprogram(name: "operator+", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEplEl", scope: !510, file: !292, line: 832, type: !555, isLocal: false, isDefinition: false, scopeLine: 832, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!555 = !DISubroutineType(types: !556)
!556 = !{!510, !530, !549}
!557 = !DISubprogram(name: "operator-=", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEmIEl", scope: !510, file: !292, line: 836, type: !552, isLocal: false, isDefinition: false, scopeLine: 836, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!558 = !DISubprogram(name: "operator-", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEmiEl", scope: !510, file: !292, line: 840, type: !555, isLocal: false, isDefinition: false, scopeLine: 840, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!559 = !DISubprogram(name: "base", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEE4baseEv", scope: !510, file: !292, line: 844, type: !560, isLocal: false, isDefinition: false, scopeLine: 844, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!560 = !DISubroutineType(types: !561)
!561 = !{!520, !530}
!562 = !{!529, !348}
!563 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "move_iterator<int *>", scope: !2, file: !292, line: 1010, size: 64, elements: !564, templateParams: !528, identifier: "_ZTSSt13move_iteratorIPiE")
!564 = !{!565, !566, !570, !574, !579, !590, !594, !598, !601, !602, !603, !607, !610, !611, !612}
!565 = !DIDerivedType(tag: DW_TAG_member, name: "_M_current", scope: !563, file: !292, line: 1013, baseType: !55, size: 64, flags: DIFlagProtected)
!566 = !DISubprogram(name: "move_iterator", scope: !563, file: !292, line: 1032, type: !567, isLocal: false, isDefinition: false, scopeLine: 1032, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!567 = !DISubroutineType(types: !568)
!568 = !{null, !569}
!569 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !563, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!570 = !DISubprogram(name: "move_iterator", scope: !563, file: !292, line: 1036, type: !571, isLocal: false, isDefinition: false, scopeLine: 1036, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!571 = !DISubroutineType(types: !572)
!572 = !{null, !569, !573}
!573 = !DIDerivedType(tag: DW_TAG_typedef, name: "iterator_type", scope: !563, file: !292, line: 1019, baseType: !55)
!574 = !DISubprogram(name: "base", linkageName: "_ZNKSt13move_iteratorIPiE4baseEv", scope: !563, file: !292, line: 1045, type: !575, isLocal: false, isDefinition: false, scopeLine: 1045, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!575 = !DISubroutineType(types: !576)
!576 = !{!573, !577}
!577 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !578, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!578 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !563)
!579 = !DISubprogram(name: "operator*", linkageName: "_ZNKSt13move_iteratorIPiEdeEv", scope: !563, file: !292, line: 1049, type: !580, isLocal: false, isDefinition: false, scopeLine: 1049, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!580 = !DISubroutineType(types: !581)
!581 = !{!582, !577}
!582 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !563, file: !292, line: 1029, baseType: !583)
!583 = !DIDerivedType(tag: DW_TAG_typedef, name: "type", scope: !584, file: !479, line: 2166, baseType: !588)
!584 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "conditional<true, int &&, int &>", scope: !2, file: !479, line: 2165, size: 8, elements: !25, templateParams: !585, identifier: "_ZTSSt11conditionalILb1EOiRiE")
!585 = !{!586, !587, !589}
!586 = !DITemplateValueParameter(name: "_Cond", type: !13, value: i8 1)
!587 = !DITemplateTypeParameter(name: "_Iftrue", type: !588)
!588 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !11, size: 64)
!589 = !DITemplateTypeParameter(name: "_Iffalse", type: !83)
!590 = !DISubprogram(name: "operator->", linkageName: "_ZNKSt13move_iteratorIPiEptEv", scope: !563, file: !292, line: 1053, type: !591, isLocal: false, isDefinition: false, scopeLine: 1053, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!591 = !DISubroutineType(types: !592)
!592 = !{!593, !577}
!593 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !563, file: !292, line: 1024, baseType: !55)
!594 = !DISubprogram(name: "operator++", linkageName: "_ZNSt13move_iteratorIPiEppEv", scope: !563, file: !292, line: 1057, type: !595, isLocal: false, isDefinition: false, scopeLine: 1057, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!595 = !DISubroutineType(types: !596)
!596 = !{!597, !569}
!597 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !563, size: 64)
!598 = !DISubprogram(name: "operator++", linkageName: "_ZNSt13move_iteratorIPiEppEi", scope: !563, file: !292, line: 1064, type: !599, isLocal: false, isDefinition: false, scopeLine: 1064, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!599 = !DISubroutineType(types: !600)
!600 = !{!563, !569, !11}
!601 = !DISubprogram(name: "operator--", linkageName: "_ZNSt13move_iteratorIPiEmmEv", scope: !563, file: !292, line: 1072, type: !595, isLocal: false, isDefinition: false, scopeLine: 1072, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!602 = !DISubprogram(name: "operator--", linkageName: "_ZNSt13move_iteratorIPiEmmEi", scope: !563, file: !292, line: 1079, type: !599, isLocal: false, isDefinition: false, scopeLine: 1079, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!603 = !DISubprogram(name: "operator+", linkageName: "_ZNKSt13move_iteratorIPiEplEl", scope: !563, file: !292, line: 1087, type: !604, isLocal: false, isDefinition: false, scopeLine: 1087, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!604 = !DISubroutineType(types: !605)
!605 = !{!563, !577, !606}
!606 = !DIDerivedType(tag: DW_TAG_typedef, name: "difference_type", scope: !563, file: !292, line: 1022, baseType: !550)
!607 = !DISubprogram(name: "operator+=", linkageName: "_ZNSt13move_iteratorIPiEpLEl", scope: !563, file: !292, line: 1091, type: !608, isLocal: false, isDefinition: false, scopeLine: 1091, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!608 = !DISubroutineType(types: !609)
!609 = !{!597, !569, !606}
!610 = !DISubprogram(name: "operator-", linkageName: "_ZNKSt13move_iteratorIPiEmiEl", scope: !563, file: !292, line: 1098, type: !604, isLocal: false, isDefinition: false, scopeLine: 1098, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!611 = !DISubprogram(name: "operator-=", linkageName: "_ZNSt13move_iteratorIPiEmIEl", scope: !563, file: !292, line: 1102, type: !608, isLocal: false, isDefinition: false, scopeLine: 1102, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!612 = !DISubprogram(name: "operator[]", linkageName: "_ZNKSt13move_iteratorIPiEixEl", scope: !563, file: !292, line: 1109, type: !613, isLocal: false, isDefinition: false, scopeLine: 1109, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!613 = !DISubroutineType(types: !614)
!614 = !{!582, !577, !606}
!615 = !{!0}
!616 = !{!617, !635, !638, !643, !701, !710, !714, !721, !725, !729, !731, !733, !737, !744, !748, !754, !760, !762, !766, !770, !774, !778, !789, !791, !795, !799, !803, !805, !810, !814, !818, !820, !822, !826, !834, !838, !842, !846, !848, !854, !856, !863, !868, !872, !876, !880, !884, !888, !890, !892, !896, !900, !904, !906, !910, !914, !916, !918, !922, !927, !932, !937, !938, !939, !940, !941, !942, !943, !944, !945, !946, !947, !1001, !1005, !1009, !1014, !1018, !1021, !1024, !1027, !1029, !1031, !1033, !1035, !1037, !1039, !1041, !1044, !1046, !1051, !1054, !1057, !1060, !1062, !1064, !1066, !1068, !1070, !1072, !1074, !1076, !1079, !1081, !1085, !1089, !1094, !1100, !1102, !1104, !1106, !1108, !1110, !1112, !1114, !1116, !1118, !1120, !1122, !1124, !1126, !1127, !1128, !1132, !1136, !1142, !1146, !1151, !1153, !1158, !1162, !1166, !1175, !1179, !1183, !1187, !1191, !1195, !1199, !1203, !1207, !1211, !1216, !1220, !1224, !1226, !1230, !1234, !1238, !1244, !1248, !1252, !1254, !1258, !1262, !1268, !1270, !1274, !1278, !1282, !1286, !1290, !1294, !1298, !1299, !1300, !1301, !1303, !1304, !1305, !1306, !1307, !1308, !1309, !1313, !1319, !1324, !1328, !1330, !1332, !1334, !1336, !1343, !1348, !1352, !1356, !1360, !1364, !1369, !1373, !1375, !1379, !1385, !1389, !1394, !1396, !1399, !1403, !1407, !1409, !1411, !1413, !1415, !1419, !1421, !1423, !1427, !1431, !1435, !1439, !1443, !1447, !1449, !1453, !1457, !1461, !1465, !1467, !1469, !1473, !1477, !1478, !1479, !1480, !1481, !1482, !1488, !1491, !1492, !1494, !1496, !1498, !1500, !1504, !1506, !1508, !1510, !1512, !1514, !1516, !1518, !1520, !1524, !1528, !1530, !1534, !1538}
!617 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !618, file: !634, line: 64)
!618 = !DIDerivedType(tag: DW_TAG_typedef, name: "mbstate_t", file: !619, line: 6, baseType: !620)
!619 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/types/mbstate_t.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!620 = !DIDerivedType(tag: DW_TAG_typedef, name: "__mbstate_t", file: !621, line: 21, baseType: !622)
!621 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/types/__mbstate_t.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!622 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !621, line: 13, size: 64, elements: !623, identifier: "_ZTS11__mbstate_t")
!623 = !{!624, !625}
!624 = !DIDerivedType(tag: DW_TAG_member, name: "__count", scope: !622, file: !621, line: 15, baseType: !11, size: 32)
!625 = !DIDerivedType(tag: DW_TAG_member, name: "__value", scope: !622, file: !621, line: 20, baseType: !626, size: 32, offset: 32)
!626 = distinct !DICompositeType(tag: DW_TAG_union_type, scope: !622, file: !621, line: 16, size: 32, elements: !627, identifier: "_ZTSN11__mbstate_tUt_E")
!627 = !{!628, !630}
!628 = !DIDerivedType(tag: DW_TAG_member, name: "__wch", scope: !626, file: !621, line: 18, baseType: !629, size: 32)
!629 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!630 = !DIDerivedType(tag: DW_TAG_member, name: "__wchb", scope: !626, file: !621, line: 19, baseType: !631, size: 32)
!631 = !DICompositeType(tag: DW_TAG_array_type, baseType: !464, size: 32, elements: !632)
!632 = !{!633}
!633 = !DISubrange(count: 4)
!634 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/cwchar", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!635 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !636, file: !634, line: 139)
!636 = !DIDerivedType(tag: DW_TAG_typedef, name: "wint_t", file: !637, line: 20, baseType: !629)
!637 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/types/wint_t.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!638 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !639, file: !634, line: 141)
!639 = !DISubprogram(name: "btowc", scope: !640, file: !640, line: 318, type: !641, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!640 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/wchar.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!641 = !DISubroutineType(types: !642)
!642 = !{!636, !11}
!643 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !644, file: !634, line: 142)
!644 = !DISubprogram(name: "fgetwc", scope: !640, file: !640, line: 660, type: !645, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!645 = !DISubroutineType(types: !646)
!646 = !{!636, !647}
!647 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !648, size: 64)
!648 = !DIDerivedType(tag: DW_TAG_typedef, name: "__FILE", file: !649, line: 5, baseType: !650)
!649 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/types/__FILE.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!650 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_IO_FILE", file: !651, line: 241, size: 1728, elements: !652, identifier: "_ZTS8_IO_FILE")
!651 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/libio.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!652 = !{!653, !654, !656, !657, !658, !659, !660, !661, !662, !663, !664, !665, !666, !669, !671, !672, !673, !676, !678, !680, !684, !687, !689, !690, !691, !692, !693, !696, !697}
!653 = !DIDerivedType(tag: DW_TAG_member, name: "_flags", scope: !650, file: !651, line: 242, baseType: !11, size: 32)
!654 = !DIDerivedType(tag: DW_TAG_member, name: "_IO_read_ptr", scope: !650, file: !651, line: 247, baseType: !655, size: 64, offset: 64)
!655 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !464, size: 64)
!656 = !DIDerivedType(tag: DW_TAG_member, name: "_IO_read_end", scope: !650, file: !651, line: 248, baseType: !655, size: 64, offset: 128)
!657 = !DIDerivedType(tag: DW_TAG_member, name: "_IO_read_base", scope: !650, file: !651, line: 249, baseType: !655, size: 64, offset: 192)
!658 = !DIDerivedType(tag: DW_TAG_member, name: "_IO_write_base", scope: !650, file: !651, line: 250, baseType: !655, size: 64, offset: 256)
!659 = !DIDerivedType(tag: DW_TAG_member, name: "_IO_write_ptr", scope: !650, file: !651, line: 251, baseType: !655, size: 64, offset: 320)
!660 = !DIDerivedType(tag: DW_TAG_member, name: "_IO_write_end", scope: !650, file: !651, line: 252, baseType: !655, size: 64, offset: 384)
!661 = !DIDerivedType(tag: DW_TAG_member, name: "_IO_buf_base", scope: !650, file: !651, line: 253, baseType: !655, size: 64, offset: 448)
!662 = !DIDerivedType(tag: DW_TAG_member, name: "_IO_buf_end", scope: !650, file: !651, line: 254, baseType: !655, size: 64, offset: 512)
!663 = !DIDerivedType(tag: DW_TAG_member, name: "_IO_save_base", scope: !650, file: !651, line: 256, baseType: !655, size: 64, offset: 576)
!664 = !DIDerivedType(tag: DW_TAG_member, name: "_IO_backup_base", scope: !650, file: !651, line: 257, baseType: !655, size: 64, offset: 640)
!665 = !DIDerivedType(tag: DW_TAG_member, name: "_IO_save_end", scope: !650, file: !651, line: 258, baseType: !655, size: 64, offset: 704)
!666 = !DIDerivedType(tag: DW_TAG_member, name: "_markers", scope: !650, file: !651, line: 260, baseType: !667, size: 64, offset: 768)
!667 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !668, size: 64)
!668 = !DICompositeType(tag: DW_TAG_structure_type, name: "_IO_marker", file: !651, line: 156, flags: DIFlagFwdDecl, identifier: "_ZTS10_IO_marker")
!669 = !DIDerivedType(tag: DW_TAG_member, name: "_chain", scope: !650, file: !651, line: 262, baseType: !670, size: 64, offset: 832)
!670 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !650, size: 64)
!671 = !DIDerivedType(tag: DW_TAG_member, name: "_fileno", scope: !650, file: !651, line: 264, baseType: !11, size: 32, offset: 896)
!672 = !DIDerivedType(tag: DW_TAG_member, name: "_flags2", scope: !650, file: !651, line: 268, baseType: !11, size: 32, offset: 928)
!673 = !DIDerivedType(tag: DW_TAG_member, name: "_old_offset", scope: !650, file: !651, line: 270, baseType: !674, size: 64, offset: 960)
!674 = !DIDerivedType(tag: DW_TAG_typedef, name: "__off_t", file: !675, line: 140, baseType: !335)
!675 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/types.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!676 = !DIDerivedType(tag: DW_TAG_member, name: "_cur_column", scope: !650, file: !651, line: 274, baseType: !677, size: 16, offset: 1024)
!677 = !DIBasicType(name: "unsigned short", size: 16, encoding: DW_ATE_unsigned)
!678 = !DIDerivedType(tag: DW_TAG_member, name: "_vtable_offset", scope: !650, file: !651, line: 275, baseType: !679, size: 8, offset: 1040)
!679 = !DIBasicType(name: "signed char", size: 8, encoding: DW_ATE_signed_char)
!680 = !DIDerivedType(tag: DW_TAG_member, name: "_shortbuf", scope: !650, file: !651, line: 276, baseType: !681, size: 8, offset: 1048)
!681 = !DICompositeType(tag: DW_TAG_array_type, baseType: !464, size: 8, elements: !682)
!682 = !{!683}
!683 = !DISubrange(count: 1)
!684 = !DIDerivedType(tag: DW_TAG_member, name: "_lock", scope: !650, file: !651, line: 280, baseType: !685, size: 64, offset: 1088)
!685 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !686, size: 64)
!686 = !DIDerivedType(tag: DW_TAG_typedef, name: "_IO_lock_t", file: !651, line: 150, baseType: null)
!687 = !DIDerivedType(tag: DW_TAG_member, name: "_offset", scope: !650, file: !651, line: 289, baseType: !688, size: 64, offset: 1152)
!688 = !DIDerivedType(tag: DW_TAG_typedef, name: "__off64_t", file: !675, line: 141, baseType: !335)
!689 = !DIDerivedType(tag: DW_TAG_member, name: "__pad1", scope: !650, file: !651, line: 297, baseType: !222, size: 64, offset: 1216)
!690 = !DIDerivedType(tag: DW_TAG_member, name: "__pad2", scope: !650, file: !651, line: 298, baseType: !222, size: 64, offset: 1280)
!691 = !DIDerivedType(tag: DW_TAG_member, name: "__pad3", scope: !650, file: !651, line: 299, baseType: !222, size: 64, offset: 1344)
!692 = !DIDerivedType(tag: DW_TAG_member, name: "__pad4", scope: !650, file: !651, line: 300, baseType: !222, size: 64, offset: 1408)
!693 = !DIDerivedType(tag: DW_TAG_member, name: "__pad5", scope: !650, file: !651, line: 302, baseType: !694, size: 64, offset: 1472)
!694 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_t", file: !695, line: 62, baseType: !98)
!695 = !DIFile(filename: "/nix/store/cn1rr6ayq4n9ybp808yrfwdwsldp4yvy-clang-6.0.1/lib/clang/6.0.1/include/stddef.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!696 = !DIDerivedType(tag: DW_TAG_member, name: "_mode", scope: !650, file: !651, line: 303, baseType: !11, size: 32, offset: 1536)
!697 = !DIDerivedType(tag: DW_TAG_member, name: "_unused2", scope: !650, file: !651, line: 305, baseType: !698, size: 160, offset: 1568)
!698 = !DICompositeType(tag: DW_TAG_array_type, baseType: !464, size: 160, elements: !699)
!699 = !{!700}
!700 = !DISubrange(count: 20)
!701 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !702, file: !634, line: 143)
!702 = !DISubprogram(name: "fgetws", scope: !703, file: !703, line: 384, type: !704, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!703 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/wchar2.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!704 = !DISubroutineType(types: !705)
!705 = !{!706, !708, !11, !709}
!706 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !707, size: 64)
!707 = !DIBasicType(name: "wchar_t", size: 32, encoding: DW_ATE_signed)
!708 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !706)
!709 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !647)
!710 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !711, file: !634, line: 144)
!711 = !DISubprogram(name: "fputwc", scope: !640, file: !640, line: 674, type: !712, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!712 = !DISubroutineType(types: !713)
!713 = !{!636, !707, !647}
!714 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !715, file: !634, line: 145)
!715 = !DISubprogram(name: "fputws", scope: !640, file: !640, line: 696, type: !716, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!716 = !DISubroutineType(types: !717)
!717 = !{!11, !718, !709}
!718 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !719)
!719 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !720, size: 64)
!720 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !707)
!721 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !722, file: !634, line: 146)
!722 = !DISubprogram(name: "fwide", scope: !640, file: !640, line: 506, type: !723, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!723 = !DISubroutineType(types: !724)
!724 = !{!11, !647, !11}
!725 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !726, file: !634, line: 147)
!726 = !DISubprogram(name: "fwprintf", scope: !640, file: !640, line: 513, type: !727, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!727 = !DISubroutineType(types: !728)
!728 = !{!11, !709, !718, null}
!729 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !730, file: !634, line: 148)
!730 = !DISubprogram(name: "fwscanf", scope: !640, file: !640, line: 554, type: !727, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!731 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !732, file: !634, line: 149)
!732 = !DISubprogram(name: "getwc", scope: !640, file: !640, line: 661, type: !645, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!733 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !734, file: !634, line: 150)
!734 = !DISubprogram(name: "getwchar", scope: !640, file: !640, line: 667, type: !735, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!735 = !DISubroutineType(types: !736)
!736 = !{!636}
!737 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !738, file: !634, line: 151)
!738 = !DISubprogram(name: "mbrlen", scope: !640, file: !640, line: 329, type: !739, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!739 = !DISubroutineType(types: !740)
!740 = !{!694, !741, !694, !742}
!741 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !462)
!742 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !743)
!743 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !618, size: 64)
!744 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !745, file: !634, line: 152)
!745 = !DISubprogram(name: "mbrtowc", scope: !640, file: !640, line: 296, type: !746, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!746 = !DISubroutineType(types: !747)
!747 = !{!694, !708, !741, !694, !742}
!748 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !749, file: !634, line: 153)
!749 = !DISubprogram(name: "mbsinit", scope: !640, file: !640, line: 292, type: !750, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!750 = !DISubroutineType(types: !751)
!751 = !{!11, !752}
!752 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !753, size: 64)
!753 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !618)
!754 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !755, file: !634, line: 154)
!755 = !DISubprogram(name: "mbsrtowcs", scope: !703, file: !703, line: 474, type: !756, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!756 = !DISubroutineType(types: !757)
!757 = !{!694, !708, !758, !694, !742}
!758 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !759)
!759 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !462, size: 64)
!760 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !761, file: !634, line: 155)
!761 = !DISubprogram(name: "putwc", scope: !640, file: !640, line: 675, type: !712, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!762 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !763, file: !634, line: 156)
!763 = !DISubprogram(name: "putwchar", scope: !640, file: !640, line: 681, type: !764, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!764 = !DISubroutineType(types: !765)
!765 = !{!636, !707}
!766 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !767, file: !634, line: 158)
!767 = !DISubprogram(name: "swprintf", scope: !640, file: !640, line: 523, type: !768, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!768 = !DISubroutineType(types: !769)
!769 = !{!11, !708, !694, !718, null}
!770 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !771, file: !634, line: 160)
!771 = !DISubprogram(name: "swscanf", scope: !640, file: !640, line: 564, type: !772, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!772 = !DISubroutineType(types: !773)
!773 = !{!11, !718, !718, null}
!774 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !775, file: !634, line: 161)
!775 = !DISubprogram(name: "ungetwc", scope: !640, file: !640, line: 704, type: !776, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!776 = !DISubroutineType(types: !777)
!777 = !{!636, !636, !647}
!778 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !779, file: !634, line: 162)
!779 = !DISubprogram(name: "vfwprintf", scope: !703, file: !703, line: 364, type: !780, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!780 = !DISubroutineType(types: !781)
!781 = !{!11, !709, !718, !782}
!782 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !783, size: 64)
!783 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__va_list_tag", file: !20, size: 192, elements: !784, identifier: "_ZTS13__va_list_tag")
!784 = !{!785, !786, !787, !788}
!785 = !DIDerivedType(tag: DW_TAG_member, name: "gp_offset", scope: !783, file: !20, baseType: !629, size: 32)
!786 = !DIDerivedType(tag: DW_TAG_member, name: "fp_offset", scope: !783, file: !20, baseType: !629, size: 32, offset: 32)
!787 = !DIDerivedType(tag: DW_TAG_member, name: "overflow_arg_area", scope: !783, file: !20, baseType: !222, size: 64, offset: 64)
!788 = !DIDerivedType(tag: DW_TAG_member, name: "reg_save_area", scope: !783, file: !20, baseType: !222, size: 64, offset: 128)
!789 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !790, file: !634, line: 164)
!790 = !DISubprogram(name: "vfwscanf", scope: !640, file: !640, line: 606, type: !780, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!791 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !792, file: !634, line: 167)
!792 = !DISubprogram(name: "vswprintf", scope: !703, file: !703, line: 315, type: !793, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!793 = !DISubroutineType(types: !794)
!794 = !{!11, !708, !694, !718, !782}
!795 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !796, file: !634, line: 170)
!796 = !DISubprogram(name: "vswscanf", scope: !640, file: !640, line: 618, type: !797, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!797 = !DISubroutineType(types: !798)
!798 = !{!11, !718, !718, !782}
!799 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !800, file: !634, line: 172)
!800 = !DISubprogram(name: "vwprintf", scope: !703, file: !703, line: 358, type: !801, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!801 = !DISubroutineType(types: !802)
!802 = !{!11, !718, !782}
!803 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !804, file: !634, line: 174)
!804 = !DISubprogram(name: "vwscanf", scope: !640, file: !640, line: 614, type: !801, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!805 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !806, file: !634, line: 176)
!806 = !DISubprogram(name: "wcrtomb", scope: !703, file: !703, line: 440, type: !807, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!807 = !DISubroutineType(types: !808)
!808 = !{!694, !809, !707, !742}
!809 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !655)
!810 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !811, file: !634, line: 177)
!811 = !DISubprogram(name: "wcscat", scope: !703, file: !703, line: 246, type: !812, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!812 = !DISubroutineType(types: !813)
!813 = !{!706, !708, !718}
!814 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !815, file: !634, line: 178)
!815 = !DISubprogram(name: "wcscmp", scope: !640, file: !640, line: 106, type: !816, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!816 = !DISubroutineType(types: !817)
!817 = !{!11, !719, !719}
!818 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !819, file: !634, line: 179)
!819 = !DISubprogram(name: "wcscoll", scope: !640, file: !640, line: 131, type: !816, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!820 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !821, file: !634, line: 180)
!821 = !DISubprogram(name: "wcscpy", scope: !703, file: !703, line: 152, type: !812, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!822 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !823, file: !634, line: 181)
!823 = !DISubprogram(name: "wcscspn", scope: !640, file: !640, line: 187, type: !824, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!824 = !DISubroutineType(types: !825)
!825 = !{!694, !719, !719}
!826 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !827, file: !634, line: 182)
!827 = !DISubprogram(name: "wcsftime", scope: !640, file: !640, line: 768, type: !828, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!828 = !DISubroutineType(types: !829)
!829 = !{!694, !708, !694, !718, !830}
!830 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !831)
!831 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !832, size: 64)
!832 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !833)
!833 = !DICompositeType(tag: DW_TAG_structure_type, name: "tm", file: !640, line: 83, flags: DIFlagFwdDecl, identifier: "_ZTS2tm")
!834 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !835, file: !634, line: 183)
!835 = !DISubprogram(name: "wcslen", scope: !640, file: !640, line: 222, type: !836, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!836 = !DISubroutineType(types: !837)
!837 = !{!694, !719}
!838 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !839, file: !634, line: 184)
!839 = !DISubprogram(name: "wcsncat", scope: !703, file: !703, line: 263, type: !840, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!840 = !DISubroutineType(types: !841)
!841 = !{!706, !708, !718, !694}
!842 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !843, file: !634, line: 185)
!843 = !DISubprogram(name: "wcsncmp", scope: !640, file: !640, line: 109, type: !844, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!844 = !DISubroutineType(types: !845)
!845 = !{!11, !719, !719, !694}
!846 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !847, file: !634, line: 186)
!847 = !DISubprogram(name: "wcsncpy", scope: !703, file: !703, line: 191, type: !840, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!848 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !849, file: !634, line: 187)
!849 = !DISubprogram(name: "wcsrtombs", scope: !703, file: !703, line: 508, type: !850, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!850 = !DISubroutineType(types: !851)
!851 = !{!694, !809, !852, !694, !742}
!852 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !853)
!853 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !719, size: 64)
!854 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !855, file: !634, line: 188)
!855 = !DISubprogram(name: "wcsspn", scope: !640, file: !640, line: 191, type: !824, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!856 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !857, file: !634, line: 189)
!857 = !DISubprogram(name: "wcstod", scope: !640, file: !640, line: 377, type: !858, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!858 = !DISubroutineType(types: !859)
!859 = !{!860, !718, !861}
!860 = !DIBasicType(name: "double", size: 64, encoding: DW_ATE_float)
!861 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !862)
!862 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !706, size: 64)
!863 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !864, file: !634, line: 191)
!864 = !DISubprogram(name: "wcstof", scope: !640, file: !640, line: 382, type: !865, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!865 = !DISubroutineType(types: !866)
!866 = !{!867, !718, !861}
!867 = !DIBasicType(name: "float", size: 32, encoding: DW_ATE_float)
!868 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !869, file: !634, line: 193)
!869 = !DISubprogram(name: "wcstok", scope: !640, file: !640, line: 217, type: !870, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!870 = !DISubroutineType(types: !871)
!871 = !{!706, !708, !718, !861}
!872 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !873, file: !634, line: 194)
!873 = !DISubprogram(name: "wcstol", scope: !640, file: !640, line: 397, type: !874, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!874 = !DISubroutineType(types: !875)
!875 = !{!335, !718, !861, !11}
!876 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !877, file: !634, line: 195)
!877 = !DISubprogram(name: "wcstoul", scope: !640, file: !640, line: 402, type: !878, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!878 = !DISubroutineType(types: !879)
!879 = !{!98, !718, !861, !11}
!880 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !881, file: !634, line: 196)
!881 = !DISubprogram(name: "wcsxfrm", scope: !640, file: !640, line: 135, type: !882, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!882 = !DISubroutineType(types: !883)
!883 = !{!694, !708, !718, !694}
!884 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !885, file: !634, line: 197)
!885 = !DISubprogram(name: "wctob", scope: !640, file: !640, line: 324, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!886 = !DISubroutineType(types: !887)
!887 = !{!11, !636}
!888 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !889, file: !634, line: 198)
!889 = !DISubprogram(name: "wmemcmp", scope: !640, file: !640, line: 258, type: !844, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!890 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !891, file: !634, line: 199)
!891 = !DISubprogram(name: "wmemcpy", scope: !703, file: !703, line: 39, type: !840, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!892 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !893, file: !634, line: 200)
!893 = !DISubprogram(name: "wmemmove", scope: !703, file: !703, line: 68, type: !894, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!894 = !DISubroutineType(types: !895)
!895 = !{!706, !706, !719, !694}
!896 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !897, file: !634, line: 201)
!897 = !DISubprogram(name: "wmemset", scope: !703, file: !703, line: 129, type: !898, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!898 = !DISubroutineType(types: !899)
!899 = !{!706, !706, !707, !694}
!900 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !901, file: !634, line: 202)
!901 = !DISubprogram(name: "wprintf", scope: !640, file: !640, line: 520, type: !902, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!902 = !DISubroutineType(types: !903)
!903 = !{!11, !718, null}
!904 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !905, file: !634, line: 203)
!905 = !DISubprogram(name: "wscanf", scope: !640, file: !640, line: 561, type: !902, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!906 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !907, file: !634, line: 204)
!907 = !DISubprogram(name: "wcschr", scope: !640, file: !640, line: 164, type: !908, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!908 = !DISubroutineType(types: !909)
!909 = !{!706, !719, !707}
!910 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !911, file: !634, line: 205)
!911 = !DISubprogram(name: "wcspbrk", scope: !640, file: !640, line: 201, type: !912, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!912 = !DISubroutineType(types: !913)
!913 = !{!706, !719, !719}
!914 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !915, file: !634, line: 206)
!915 = !DISubprogram(name: "wcsrchr", scope: !640, file: !640, line: 174, type: !908, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!916 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !917, file: !634, line: 207)
!917 = !DISubprogram(name: "wcsstr", scope: !640, file: !640, line: 212, type: !912, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!918 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !919, file: !634, line: 208)
!919 = !DISubprogram(name: "wmemchr", scope: !640, file: !640, line: 253, type: !920, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!920 = !DISubroutineType(types: !921)
!921 = !{!706, !719, !707, !694}
!922 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !923, file: !634, line: 248)
!923 = !DISubprogram(name: "wcstold", scope: !640, file: !640, line: 384, type: !924, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!924 = !DISubroutineType(types: !925)
!925 = !{!926, !718, !861}
!926 = !DIBasicType(name: "long double", size: 128, encoding: DW_ATE_float)
!927 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !928, file: !634, line: 257)
!928 = !DISubprogram(name: "wcstoll", scope: !640, file: !640, line: 410, type: !929, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!929 = !DISubroutineType(types: !930)
!930 = !{!931, !718, !861, !11}
!931 = !DIBasicType(name: "long long int", size: 64, encoding: DW_ATE_signed)
!932 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !933, file: !634, line: 258)
!933 = !DISubprogram(name: "wcstoull", scope: !640, file: !640, line: 417, type: !934, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!934 = !DISubroutineType(types: !935)
!935 = !{!936, !718, !861, !11}
!936 = !DIBasicType(name: "long long unsigned int", size: 64, encoding: DW_ATE_unsigned)
!937 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !923, file: !634, line: 264)
!938 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !928, file: !634, line: 265)
!939 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !933, file: !634, line: 266)
!940 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !864, file: !634, line: 280)
!941 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !790, file: !634, line: 283)
!942 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !796, file: !634, line: 286)
!943 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !804, file: !634, line: 289)
!944 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !923, file: !634, line: 293)
!945 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !928, file: !634, line: 294)
!946 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !933, file: !634, line: 295)
!947 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !948, file: !949, line: 57)
!948 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "exception_ptr", scope: !950, file: !949, line: 79, size: 64, elements: !951, identifier: "_ZTSNSt15__exception_ptr13exception_ptrE")
!949 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/bits/exception_ptr.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!950 = !DINamespace(name: "__exception_ptr", scope: !2)
!951 = !{!952, !953, !957, !960, !961, !966, !967, !971, !976, !980, !984, !987, !988, !991, !994}
!952 = !DIDerivedType(tag: DW_TAG_member, name: "_M_exception_object", scope: !948, file: !949, line: 81, baseType: !222, size: 64)
!953 = !DISubprogram(name: "exception_ptr", scope: !948, file: !949, line: 83, type: !954, isLocal: false, isDefinition: false, scopeLine: 83, flags: DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!954 = !DISubroutineType(types: !955)
!955 = !{null, !956, !222}
!956 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !948, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!957 = !DISubprogram(name: "_M_addref", linkageName: "_ZNSt15__exception_ptr13exception_ptr9_M_addrefEv", scope: !948, file: !949, line: 85, type: !958, isLocal: false, isDefinition: false, scopeLine: 85, flags: DIFlagPrototyped, isOptimized: true)
!958 = !DISubroutineType(types: !959)
!959 = !{null, !956}
!960 = !DISubprogram(name: "_M_release", linkageName: "_ZNSt15__exception_ptr13exception_ptr10_M_releaseEv", scope: !948, file: !949, line: 86, type: !958, isLocal: false, isDefinition: false, scopeLine: 86, flags: DIFlagPrototyped, isOptimized: true)
!961 = !DISubprogram(name: "_M_get", linkageName: "_ZNKSt15__exception_ptr13exception_ptr6_M_getEv", scope: !948, file: !949, line: 88, type: !962, isLocal: false, isDefinition: false, scopeLine: 88, flags: DIFlagPrototyped, isOptimized: true)
!962 = !DISubroutineType(types: !963)
!963 = !{!222, !964}
!964 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !965, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!965 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !948)
!966 = !DISubprogram(name: "exception_ptr", scope: !948, file: !949, line: 96, type: !958, isLocal: false, isDefinition: false, scopeLine: 96, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!967 = !DISubprogram(name: "exception_ptr", scope: !948, file: !949, line: 98, type: !968, isLocal: false, isDefinition: false, scopeLine: 98, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!968 = !DISubroutineType(types: !969)
!969 = !{null, !956, !970}
!970 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !965, size: 64)
!971 = !DISubprogram(name: "exception_ptr", scope: !948, file: !949, line: 101, type: !972, isLocal: false, isDefinition: false, scopeLine: 101, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!972 = !DISubroutineType(types: !973)
!973 = !{null, !956, !974}
!974 = !DIDerivedType(tag: DW_TAG_typedef, name: "nullptr_t", scope: !2, file: !97, line: 235, baseType: !975)
!975 = !DIBasicType(tag: DW_TAG_unspecified_type, name: "decltype(nullptr)")
!976 = !DISubprogram(name: "exception_ptr", scope: !948, file: !949, line: 105, type: !977, isLocal: false, isDefinition: false, scopeLine: 105, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!977 = !DISubroutineType(types: !978)
!978 = !{null, !956, !979}
!979 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !948, size: 64)
!980 = !DISubprogram(name: "operator=", linkageName: "_ZNSt15__exception_ptr13exception_ptraSERKS0_", scope: !948, file: !949, line: 118, type: !981, isLocal: false, isDefinition: false, scopeLine: 118, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!981 = !DISubroutineType(types: !982)
!982 = !{!983, !956, !970}
!983 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !948, size: 64)
!984 = !DISubprogram(name: "operator=", linkageName: "_ZNSt15__exception_ptr13exception_ptraSEOS0_", scope: !948, file: !949, line: 122, type: !985, isLocal: false, isDefinition: false, scopeLine: 122, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!985 = !DISubroutineType(types: !986)
!986 = !{!983, !956, !979}
!987 = !DISubprogram(name: "~exception_ptr", scope: !948, file: !949, line: 129, type: !958, isLocal: false, isDefinition: false, scopeLine: 129, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!988 = !DISubprogram(name: "swap", linkageName: "_ZNSt15__exception_ptr13exception_ptr4swapERS0_", scope: !948, file: !949, line: 132, type: !989, isLocal: false, isDefinition: false, scopeLine: 132, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!989 = !DISubroutineType(types: !990)
!990 = !{null, !956, !983}
!991 = !DISubprogram(name: "operator bool", linkageName: "_ZNKSt15__exception_ptr13exception_ptrcvbEv", scope: !948, file: !949, line: 144, type: !992, isLocal: false, isDefinition: false, scopeLine: 144, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!992 = !DISubroutineType(types: !993)
!993 = !{!13, !964}
!994 = !DISubprogram(name: "__cxa_exception_type", linkageName: "_ZNKSt15__exception_ptr13exception_ptr20__cxa_exception_typeEv", scope: !948, file: !949, line: 153, type: !995, isLocal: false, isDefinition: false, scopeLine: 153, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!995 = !DISubroutineType(types: !996)
!996 = !{!997, !964}
!997 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !998, size: 64)
!998 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !999)
!999 = !DICompositeType(tag: DW_TAG_class_type, name: "type_info", scope: !2, file: !1000, line: 88, flags: DIFlagFwdDecl, identifier: "_ZTSSt9type_info")
!1000 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/typeinfo", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1001 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !950, entity: !1002, file: !949, line: 73)
!1002 = !DISubprogram(name: "rethrow_exception", linkageName: "_ZSt17rethrow_exceptionNSt15__exception_ptr13exception_ptrE", scope: !2, file: !949, line: 69, type: !1003, isLocal: false, isDefinition: false, flags: DIFlagPrototyped | DIFlagNoReturn, isOptimized: true)
!1003 = !DISubroutineType(types: !1004)
!1004 = !{null, !948}
!1005 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1006, entity: !1007, file: !1008, line: 58)
!1006 = !DINamespace(name: "__gnu_debug", scope: null)
!1007 = !DINamespace(name: "__debug", scope: !2)
!1008 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/debug/debug.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1009 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1010, file: !1013, line: 48)
!1010 = !DIDerivedType(tag: DW_TAG_typedef, name: "int8_t", file: !1011, line: 24, baseType: !1012)
!1011 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/stdint-intn.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1012 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int8_t", file: !675, line: 36, baseType: !679)
!1013 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/cstdint", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1014 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1015, file: !1013, line: 49)
!1015 = !DIDerivedType(tag: DW_TAG_typedef, name: "int16_t", file: !1011, line: 25, baseType: !1016)
!1016 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int16_t", file: !675, line: 38, baseType: !1017)
!1017 = !DIBasicType(name: "short", size: 16, encoding: DW_ATE_signed)
!1018 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1019, file: !1013, line: 50)
!1019 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !1011, line: 26, baseType: !1020)
!1020 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int32_t", file: !675, line: 40, baseType: !11)
!1021 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1022, file: !1013, line: 51)
!1022 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !1011, line: 27, baseType: !1023)
!1023 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !675, line: 43, baseType: !335)
!1024 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1025, file: !1013, line: 53)
!1025 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast8_t", file: !1026, line: 68, baseType: !679)
!1026 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/stdint.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1027 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1028, file: !1013, line: 54)
!1028 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast16_t", file: !1026, line: 70, baseType: !335)
!1029 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1030, file: !1013, line: 55)
!1030 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast32_t", file: !1026, line: 71, baseType: !335)
!1031 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1032, file: !1013, line: 56)
!1032 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast64_t", file: !1026, line: 72, baseType: !335)
!1033 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1034, file: !1013, line: 58)
!1034 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least8_t", file: !1026, line: 43, baseType: !679)
!1035 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1036, file: !1013, line: 59)
!1036 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least16_t", file: !1026, line: 44, baseType: !1017)
!1037 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1038, file: !1013, line: 60)
!1038 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least32_t", file: !1026, line: 45, baseType: !11)
!1039 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1040, file: !1013, line: 61)
!1040 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least64_t", file: !1026, line: 47, baseType: !335)
!1041 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1042, file: !1013, line: 63)
!1042 = !DIDerivedType(tag: DW_TAG_typedef, name: "intmax_t", file: !1026, line: 111, baseType: !1043)
!1043 = !DIDerivedType(tag: DW_TAG_typedef, name: "__intmax_t", file: !675, line: 61, baseType: !335)
!1044 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1045, file: !1013, line: 64)
!1045 = !DIDerivedType(tag: DW_TAG_typedef, name: "intptr_t", file: !1026, line: 97, baseType: !335)
!1046 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1047, file: !1013, line: 66)
!1047 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !1048, line: 24, baseType: !1049)
!1048 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/stdint-uintn.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1049 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint8_t", file: !675, line: 37, baseType: !1050)
!1050 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!1051 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1052, file: !1013, line: 67)
!1052 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint16_t", file: !1048, line: 25, baseType: !1053)
!1053 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint16_t", file: !675, line: 39, baseType: !677)
!1054 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1055, file: !1013, line: 68)
!1055 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !1048, line: 26, baseType: !1056)
!1056 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint32_t", file: !675, line: 41, baseType: !629)
!1057 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1058, file: !1013, line: 69)
!1058 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !1048, line: 27, baseType: !1059)
!1059 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint64_t", file: !675, line: 44, baseType: !98)
!1060 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1061, file: !1013, line: 71)
!1061 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast8_t", file: !1026, line: 81, baseType: !1050)
!1062 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1063, file: !1013, line: 72)
!1063 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast16_t", file: !1026, line: 83, baseType: !98)
!1064 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1065, file: !1013, line: 73)
!1065 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast32_t", file: !1026, line: 84, baseType: !98)
!1066 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1067, file: !1013, line: 74)
!1067 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast64_t", file: !1026, line: 85, baseType: !98)
!1068 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1069, file: !1013, line: 76)
!1069 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least8_t", file: !1026, line: 54, baseType: !1050)
!1070 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1071, file: !1013, line: 77)
!1071 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least16_t", file: !1026, line: 55, baseType: !677)
!1072 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1073, file: !1013, line: 78)
!1073 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least32_t", file: !1026, line: 56, baseType: !629)
!1074 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1075, file: !1013, line: 79)
!1075 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least64_t", file: !1026, line: 58, baseType: !98)
!1076 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1077, file: !1013, line: 81)
!1077 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintmax_t", file: !1026, line: 112, baseType: !1078)
!1078 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uintmax_t", file: !675, line: 62, baseType: !98)
!1079 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1080, file: !1013, line: 82)
!1080 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !1026, line: 100, baseType: !98)
!1081 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1082, file: !1084, line: 53)
!1082 = !DICompositeType(tag: DW_TAG_structure_type, name: "lconv", file: !1083, line: 51, flags: DIFlagFwdDecl, identifier: "_ZTS5lconv")
!1083 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/locale.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1084 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/clocale", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1085 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1086, file: !1084, line: 54)
!1086 = !DISubprogram(name: "setlocale", scope: !1083, file: !1083, line: 122, type: !1087, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1087 = !DISubroutineType(types: !1088)
!1088 = !{!655, !11, !462}
!1089 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1090, file: !1084, line: 55)
!1090 = !DISubprogram(name: "localeconv", scope: !1083, file: !1083, line: 125, type: !1091, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1091 = !DISubroutineType(types: !1092)
!1092 = !{!1093}
!1093 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1082, size: 64)
!1094 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1095, file: !1099, line: 64)
!1095 = !DISubprogram(name: "isalnum", scope: !1096, file: !1096, line: 108, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1096 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/ctype.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1097 = !DISubroutineType(types: !1098)
!1098 = !{!11, !11}
!1099 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/cctype", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1100 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1101, file: !1099, line: 65)
!1101 = !DISubprogram(name: "isalpha", scope: !1096, file: !1096, line: 109, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1102 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1103, file: !1099, line: 66)
!1103 = !DISubprogram(name: "iscntrl", scope: !1096, file: !1096, line: 110, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1104 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1105, file: !1099, line: 67)
!1105 = !DISubprogram(name: "isdigit", scope: !1096, file: !1096, line: 111, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1106 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1107, file: !1099, line: 68)
!1107 = !DISubprogram(name: "isgraph", scope: !1096, file: !1096, line: 113, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1108 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1109, file: !1099, line: 69)
!1109 = !DISubprogram(name: "islower", scope: !1096, file: !1096, line: 112, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1110 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1111, file: !1099, line: 70)
!1111 = !DISubprogram(name: "isprint", scope: !1096, file: !1096, line: 114, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1112 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1113, file: !1099, line: 71)
!1113 = !DISubprogram(name: "ispunct", scope: !1096, file: !1096, line: 115, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1114 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1115, file: !1099, line: 72)
!1115 = !DISubprogram(name: "isspace", scope: !1096, file: !1096, line: 116, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1116 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1117, file: !1099, line: 73)
!1117 = !DISubprogram(name: "isupper", scope: !1096, file: !1096, line: 117, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1118 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1119, file: !1099, line: 74)
!1119 = !DISubprogram(name: "isxdigit", scope: !1096, file: !1096, line: 118, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1120 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1121, file: !1099, line: 75)
!1121 = !DISubprogram(name: "tolower", scope: !1096, file: !1096, line: 122, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1122 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1123, file: !1099, line: 76)
!1123 = !DISubprogram(name: "toupper", scope: !1096, file: !1096, line: 125, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1124 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1125, file: !1099, line: 87)
!1125 = !DISubprogram(name: "isblank", scope: !1096, file: !1096, line: 130, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1126 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !96, file: !65, line: 44)
!1127 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !334, file: !65, line: 45)
!1128 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1129, file: !1131, line: 52)
!1129 = !DISubprogram(name: "abs", scope: !1130, file: !1130, line: 722, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1130 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/stdlib.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1131 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/bits/std_abs.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1132 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1133, file: !1135, line: 124)
!1133 = !DIDerivedType(tag: DW_TAG_typedef, name: "div_t", file: !1130, line: 62, baseType: !1134)
!1134 = !DICompositeType(tag: DW_TAG_structure_type, file: !1130, line: 58, flags: DIFlagFwdDecl, identifier: "_ZTS5div_t")
!1135 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/cstdlib", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1136 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1137, file: !1135, line: 125)
!1137 = !DIDerivedType(tag: DW_TAG_typedef, name: "ldiv_t", file: !1130, line: 70, baseType: !1138)
!1138 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !1130, line: 66, size: 128, elements: !1139, identifier: "_ZTS6ldiv_t")
!1139 = !{!1140, !1141}
!1140 = !DIDerivedType(tag: DW_TAG_member, name: "quot", scope: !1138, file: !1130, line: 68, baseType: !335, size: 64)
!1141 = !DIDerivedType(tag: DW_TAG_member, name: "rem", scope: !1138, file: !1130, line: 69, baseType: !335, size: 64, offset: 64)
!1142 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1143, file: !1135, line: 127)
!1143 = !DISubprogram(name: "abort", scope: !1130, file: !1130, line: 473, type: !1144, isLocal: false, isDefinition: false, flags: DIFlagPrototyped | DIFlagNoReturn, isOptimized: true)
!1144 = !DISubroutineType(types: !1145)
!1145 = !{null}
!1146 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1147, file: !1135, line: 128)
!1147 = !DISubprogram(name: "atexit", scope: !1130, file: !1130, line: 477, type: !1148, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1148 = !DISubroutineType(types: !1149)
!1149 = !{!11, !1150}
!1150 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1144, size: 64)
!1151 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1152, file: !1135, line: 131)
!1152 = !DISubprogram(name: "at_quick_exit", scope: !1130, file: !1130, line: 482, type: !1148, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1153 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1154, file: !1135, line: 134)
!1154 = !DISubprogram(name: "atof", scope: !1155, file: !1155, line: 25, type: !1156, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1155 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/stdlib-float.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1156 = !DISubroutineType(types: !1157)
!1157 = !{!860, !462}
!1158 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1159, file: !1135, line: 135)
!1159 = !DISubprogram(name: "atoi", scope: !1130, file: !1130, line: 246, type: !1160, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1160 = !DISubroutineType(types: !1161)
!1161 = !{!11, !462}
!1162 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1163, file: !1135, line: 136)
!1163 = !DISubprogram(name: "atol", scope: !1130, file: !1130, line: 251, type: !1164, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1164 = !DISubroutineType(types: !1165)
!1165 = !{!335, !462}
!1166 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1167, file: !1135, line: 137)
!1167 = !DISubprogram(name: "bsearch", scope: !1168, file: !1168, line: 20, type: !1169, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1168 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/stdlib-bsearch.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1169 = !DISubroutineType(types: !1170)
!1170 = !{!222, !99, !99, !694, !694, !1171}
!1171 = !DIDerivedType(tag: DW_TAG_typedef, name: "__compar_fn_t", file: !1130, line: 690, baseType: !1172)
!1172 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1173, size: 64)
!1173 = !DISubroutineType(types: !1174)
!1174 = !{!11, !99, !99}
!1175 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1176, file: !1135, line: 138)
!1176 = !DISubprogram(name: "calloc", scope: !1130, file: !1130, line: 426, type: !1177, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1177 = !DISubroutineType(types: !1178)
!1178 = !{!222, !694, !694}
!1179 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1180, file: !1135, line: 139)
!1180 = !DISubprogram(name: "div", scope: !1130, file: !1130, line: 734, type: !1181, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1181 = !DISubroutineType(types: !1182)
!1182 = !{!1133, !11, !11}
!1183 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1184, file: !1135, line: 140)
!1184 = !DISubprogram(name: "exit", scope: !1130, file: !1130, line: 499, type: !1185, isLocal: false, isDefinition: false, flags: DIFlagPrototyped | DIFlagNoReturn, isOptimized: true)
!1185 = !DISubroutineType(types: !1186)
!1186 = !{null, !11}
!1187 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1188, file: !1135, line: 141)
!1188 = !DISubprogram(name: "free", scope: !1130, file: !1130, line: 448, type: !1189, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1189 = !DISubroutineType(types: !1190)
!1190 = !{null, !222}
!1191 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1192, file: !1135, line: 142)
!1192 = !DISubprogram(name: "getenv", scope: !1130, file: !1130, line: 516, type: !1193, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1193 = !DISubroutineType(types: !1194)
!1194 = !{!655, !462}
!1195 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1196, file: !1135, line: 143)
!1196 = !DISubprogram(name: "labs", scope: !1130, file: !1130, line: 723, type: !1197, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1197 = !DISubroutineType(types: !1198)
!1198 = !{!335, !335}
!1199 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1200, file: !1135, line: 144)
!1200 = !DISubprogram(name: "ldiv", scope: !1130, file: !1130, line: 736, type: !1201, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1201 = !DISubroutineType(types: !1202)
!1202 = !{!1137, !335, !335}
!1203 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1204, file: !1135, line: 145)
!1204 = !DISubprogram(name: "malloc", scope: !1130, file: !1130, line: 424, type: !1205, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1205 = !DISubroutineType(types: !1206)
!1206 = !{!222, !694}
!1207 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1208, file: !1135, line: 147)
!1208 = !DISubprogram(name: "mblen", scope: !1130, file: !1130, line: 804, type: !1209, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1209 = !DISubroutineType(types: !1210)
!1210 = !{!11, !462, !694}
!1211 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1212, file: !1135, line: 148)
!1212 = !DISubprogram(name: "mbstowcs", scope: !1213, file: !1213, line: 113, type: !1214, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1213 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/stdlib.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1214 = !DISubroutineType(types: !1215)
!1215 = !{!694, !708, !741, !694}
!1216 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1217, file: !1135, line: 149)
!1217 = !DISubprogram(name: "mbtowc", scope: !1130, file: !1130, line: 807, type: !1218, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1218 = !DISubroutineType(types: !1219)
!1219 = !{!11, !708, !741, !694}
!1220 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1221, file: !1135, line: 151)
!1221 = !DISubprogram(name: "qsort", scope: !1130, file: !1130, line: 712, type: !1222, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1222 = !DISubroutineType(types: !1223)
!1223 = !{null, !222, !694, !694, !1171}
!1224 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1225, file: !1135, line: 154)
!1225 = !DISubprogram(name: "quick_exit", scope: !1130, file: !1130, line: 505, type: !1185, isLocal: false, isDefinition: false, flags: DIFlagPrototyped | DIFlagNoReturn, isOptimized: true)
!1226 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1227, file: !1135, line: 157)
!1227 = !DISubprogram(name: "rand", scope: !1130, file: !1130, line: 338, type: !1228, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1228 = !DISubroutineType(types: !1229)
!1229 = !{!11}
!1230 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1231, file: !1135, line: 158)
!1231 = !DISubprogram(name: "realloc", scope: !1130, file: !1130, line: 434, type: !1232, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1232 = !DISubroutineType(types: !1233)
!1233 = !{!222, !222, !694}
!1234 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1235, file: !1135, line: 159)
!1235 = !DISubprogram(name: "srand", scope: !1130, file: !1130, line: 340, type: !1236, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1236 = !DISubroutineType(types: !1237)
!1237 = !{null, !629}
!1238 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1239, file: !1135, line: 160)
!1239 = !DISubprogram(name: "strtod", scope: !1130, file: !1130, line: 117, type: !1240, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1240 = !DISubroutineType(types: !1241)
!1241 = !{!860, !741, !1242}
!1242 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !1243)
!1243 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !655, size: 64)
!1244 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1245, file: !1135, line: 161)
!1245 = !DISubprogram(name: "strtol", scope: !1130, file: !1130, line: 139, type: !1246, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1246 = !DISubroutineType(types: !1247)
!1247 = !{!335, !741, !1242, !11}
!1248 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1249, file: !1135, line: 162)
!1249 = !DISubprogram(name: "strtoul", scope: !1130, file: !1130, line: 143, type: !1250, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1250 = !DISubroutineType(types: !1251)
!1251 = !{!98, !741, !1242, !11}
!1252 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1253, file: !1135, line: 163)
!1253 = !DISubprogram(name: "system", scope: !1130, file: !1130, line: 666, type: !1160, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1254 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1255, file: !1135, line: 165)
!1255 = !DISubprogram(name: "wcstombs", scope: !1213, file: !1213, line: 144, type: !1256, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1256 = !DISubroutineType(types: !1257)
!1257 = !{!694, !809, !718, !694}
!1258 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1259, file: !1135, line: 166)
!1259 = !DISubprogram(name: "wctomb", scope: !1213, file: !1213, line: 83, type: !1260, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1260 = !DISubroutineType(types: !1261)
!1261 = !{!11, !655, !707}
!1262 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1263, file: !1135, line: 194)
!1263 = !DIDerivedType(tag: DW_TAG_typedef, name: "lldiv_t", file: !1130, line: 80, baseType: !1264)
!1264 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !1130, line: 76, size: 128, elements: !1265, identifier: "_ZTS7lldiv_t")
!1265 = !{!1266, !1267}
!1266 = !DIDerivedType(tag: DW_TAG_member, name: "quot", scope: !1264, file: !1130, line: 78, baseType: !931, size: 64)
!1267 = !DIDerivedType(tag: DW_TAG_member, name: "rem", scope: !1264, file: !1130, line: 79, baseType: !931, size: 64, offset: 64)
!1268 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1269, file: !1135, line: 200)
!1269 = !DISubprogram(name: "_Exit", scope: !1130, file: !1130, line: 511, type: !1185, isLocal: false, isDefinition: false, flags: DIFlagPrototyped | DIFlagNoReturn, isOptimized: true)
!1270 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1271, file: !1135, line: 204)
!1271 = !DISubprogram(name: "llabs", scope: !1130, file: !1130, line: 726, type: !1272, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1272 = !DISubroutineType(types: !1273)
!1273 = !{!931, !931}
!1274 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1275, file: !1135, line: 210)
!1275 = !DISubprogram(name: "lldiv", scope: !1130, file: !1130, line: 740, type: !1276, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1276 = !DISubroutineType(types: !1277)
!1277 = !{!1263, !931, !931}
!1278 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1279, file: !1135, line: 221)
!1279 = !DISubprogram(name: "atoll", scope: !1130, file: !1130, line: 258, type: !1280, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1280 = !DISubroutineType(types: !1281)
!1281 = !{!931, !462}
!1282 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1283, file: !1135, line: 222)
!1283 = !DISubprogram(name: "strtoll", scope: !1130, file: !1130, line: 163, type: !1284, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1284 = !DISubroutineType(types: !1285)
!1285 = !{!931, !741, !1242, !11}
!1286 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1287, file: !1135, line: 223)
!1287 = !DISubprogram(name: "strtoull", scope: !1130, file: !1130, line: 168, type: !1288, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1288 = !DISubroutineType(types: !1289)
!1289 = !{!936, !741, !1242, !11}
!1290 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1291, file: !1135, line: 225)
!1291 = !DISubprogram(name: "strtof", scope: !1130, file: !1130, line: 123, type: !1292, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1292 = !DISubroutineType(types: !1293)
!1293 = !{!867, !741, !1242}
!1294 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1295, file: !1135, line: 226)
!1295 = !DISubprogram(name: "strtold", scope: !1130, file: !1130, line: 126, type: !1296, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1296 = !DISubroutineType(types: !1297)
!1297 = !{!926, !741, !1242}
!1298 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1263, file: !1135, line: 234)
!1299 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1269, file: !1135, line: 236)
!1300 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1271, file: !1135, line: 238)
!1301 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1302, file: !1135, line: 239)
!1302 = !DISubprogram(name: "div", linkageName: "_ZN9__gnu_cxx3divExx", scope: !45, file: !1135, line: 207, type: !1276, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1303 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1275, file: !1135, line: 240)
!1304 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1279, file: !1135, line: 242)
!1305 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1291, file: !1135, line: 243)
!1306 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1283, file: !1135, line: 244)
!1307 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1287, file: !1135, line: 245)
!1308 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1295, file: !1135, line: 246)
!1309 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1310, file: !1312, line: 98)
!1310 = !DIDerivedType(tag: DW_TAG_typedef, name: "FILE", file: !1311, line: 7, baseType: !650)
!1311 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/types/FILE.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1312 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/cstdio", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1313 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1314, file: !1312, line: 99)
!1314 = !DIDerivedType(tag: DW_TAG_typedef, name: "fpos_t", file: !1315, line: 78, baseType: !1316)
!1315 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/stdio.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1316 = !DIDerivedType(tag: DW_TAG_typedef, name: "_G_fpos_t", file: !1317, line: 26, baseType: !1318)
!1317 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/_G_config.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1318 = !DICompositeType(tag: DW_TAG_structure_type, file: !1317, line: 22, flags: DIFlagFwdDecl, identifier: "_ZTS9_G_fpos_t")
!1319 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1320, file: !1312, line: 101)
!1320 = !DISubprogram(name: "clearerr", scope: !1315, file: !1315, line: 757, type: !1321, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1321 = !DISubroutineType(types: !1322)
!1322 = !{null, !1323}
!1323 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1310, size: 64)
!1324 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1325, file: !1312, line: 102)
!1325 = !DISubprogram(name: "fclose", scope: !1315, file: !1315, line: 199, type: !1326, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1326 = !DISubroutineType(types: !1327)
!1327 = !{!11, !1323}
!1328 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1329, file: !1312, line: 103)
!1329 = !DISubprogram(name: "feof", scope: !1315, file: !1315, line: 759, type: !1326, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1330 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1331, file: !1312, line: 104)
!1331 = !DISubprogram(name: "ferror", scope: !1315, file: !1315, line: 761, type: !1326, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1332 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1333, file: !1312, line: 105)
!1333 = !DISubprogram(name: "fflush", scope: !1315, file: !1315, line: 204, type: !1326, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1334 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1335, file: !1312, line: 106)
!1335 = !DISubprogram(name: "fgetc", scope: !1315, file: !1315, line: 477, type: !1326, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1336 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1337, file: !1312, line: 107)
!1337 = !DISubprogram(name: "fgetpos", scope: !1315, file: !1315, line: 731, type: !1338, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1338 = !DISubroutineType(types: !1339)
!1339 = !{!11, !1340, !1341}
!1340 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !1323)
!1341 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !1342)
!1342 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1314, size: 64)
!1343 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1344, file: !1312, line: 108)
!1344 = !DISubprogram(name: "fgets", scope: !1345, file: !1345, line: 252, type: !1346, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1345 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/stdio2.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1346 = !DISubroutineType(types: !1347)
!1347 = !{!655, !809, !11, !1340}
!1348 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1349, file: !1312, line: 109)
!1349 = !DISubprogram(name: "fopen", scope: !1315, file: !1315, line: 232, type: !1350, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1350 = !DISubroutineType(types: !1351)
!1351 = !{!1323, !741, !741}
!1352 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1353, file: !1312, line: 110)
!1353 = !DISubprogram(name: "fprintf", scope: !1315, file: !1315, line: 312, type: !1354, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1354 = !DISubroutineType(types: !1355)
!1355 = !{!11, !1340, !741, null}
!1356 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1357, file: !1312, line: 111)
!1357 = !DISubprogram(name: "fputc", scope: !1315, file: !1315, line: 517, type: !1358, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1358 = !DISubroutineType(types: !1359)
!1359 = !{!11, !11, !1323}
!1360 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1361, file: !1312, line: 112)
!1361 = !DISubprogram(name: "fputs", scope: !1315, file: !1315, line: 626, type: !1362, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1362 = !DISubroutineType(types: !1363)
!1363 = !{!11, !741, !1340}
!1364 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1365, file: !1312, line: 113)
!1365 = !DISubprogram(name: "fread", scope: !1345, file: !1345, line: 281, type: !1366, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1366 = !DISubroutineType(types: !1367)
!1367 = !{!694, !1368, !694, !694, !1340}
!1368 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !222)
!1369 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1370, file: !1312, line: 114)
!1370 = !DISubprogram(name: "freopen", scope: !1315, file: !1315, line: 238, type: !1371, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1371 = !DISubroutineType(types: !1372)
!1372 = !{!1323, !741, !741, !1340}
!1373 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1374, file: !1312, line: 115)
!1374 = !DISubprogram(name: "fscanf", scope: !1315, file: !1315, line: 377, type: !1354, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1375 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1376, file: !1312, line: 116)
!1376 = !DISubprogram(name: "fseek", scope: !1315, file: !1315, line: 684, type: !1377, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1377 = !DISubroutineType(types: !1378)
!1378 = !{!11, !1323, !335, !11}
!1379 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1380, file: !1312, line: 117)
!1380 = !DISubprogram(name: "fsetpos", scope: !1315, file: !1315, line: 736, type: !1381, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1381 = !DISubroutineType(types: !1382)
!1382 = !{!11, !1323, !1383}
!1383 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1384, size: 64)
!1384 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1314)
!1385 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1386, file: !1312, line: 118)
!1386 = !DISubprogram(name: "ftell", scope: !1315, file: !1315, line: 689, type: !1387, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1387 = !DISubroutineType(types: !1388)
!1388 = !{!335, !1323}
!1389 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1390, file: !1312, line: 119)
!1390 = !DISubprogram(name: "fwrite", scope: !1315, file: !1315, line: 652, type: !1391, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1391 = !DISubroutineType(types: !1392)
!1392 = !{!694, !1393, !694, !694, !1340}
!1393 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !99)
!1394 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1395, file: !1312, line: 120)
!1395 = !DISubprogram(name: "getc", scope: !1315, file: !1315, line: 478, type: !1326, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1396 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1397, file: !1312, line: 121)
!1397 = !DISubprogram(name: "getchar", scope: !1398, file: !1398, line: 44, type: !1228, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1398 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/stdio.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1399 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1400, file: !1312, line: 126)
!1400 = !DISubprogram(name: "perror", scope: !1315, file: !1315, line: 775, type: !1401, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1401 = !DISubroutineType(types: !1402)
!1402 = !{null, !462}
!1403 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1404, file: !1312, line: 127)
!1404 = !DISubprogram(name: "printf", scope: !1315, file: !1315, line: 318, type: !1405, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1405 = !DISubroutineType(types: !1406)
!1406 = !{!11, !741, null}
!1407 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1408, file: !1312, line: 128)
!1408 = !DISubprogram(name: "putc", scope: !1315, file: !1315, line: 518, type: !1358, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1409 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1410, file: !1312, line: 129)
!1410 = !DISubprogram(name: "putchar", scope: !1398, file: !1398, line: 79, type: !1097, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1411 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1412, file: !1312, line: 130)
!1412 = !DISubprogram(name: "puts", scope: !1315, file: !1315, line: 632, type: !1160, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1413 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1414, file: !1312, line: 131)
!1414 = !DISubprogram(name: "remove", scope: !1315, file: !1315, line: 144, type: !1160, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1415 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1416, file: !1312, line: 132)
!1416 = !DISubprogram(name: "rename", scope: !1315, file: !1315, line: 146, type: !1417, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1417 = !DISubroutineType(types: !1418)
!1418 = !{!11, !462, !462}
!1419 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1420, file: !1312, line: 133)
!1420 = !DISubprogram(name: "rewind", scope: !1315, file: !1315, line: 694, type: !1321, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1421 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1422, file: !1312, line: 134)
!1422 = !DISubprogram(name: "scanf", scope: !1315, file: !1315, line: 383, type: !1405, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1423 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1424, file: !1312, line: 135)
!1424 = !DISubprogram(name: "setbuf", scope: !1315, file: !1315, line: 290, type: !1425, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1425 = !DISubroutineType(types: !1426)
!1426 = !{null, !1340, !809}
!1427 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1428, file: !1312, line: 136)
!1428 = !DISubprogram(name: "setvbuf", scope: !1315, file: !1315, line: 294, type: !1429, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1429 = !DISubroutineType(types: !1430)
!1430 = !{!11, !1340, !809, !11, !694}
!1431 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1432, file: !1312, line: 137)
!1432 = !DISubprogram(name: "sprintf", scope: !1315, file: !1315, line: 320, type: !1433, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1433 = !DISubroutineType(types: !1434)
!1434 = !{!11, !809, !741, null}
!1435 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1436, file: !1312, line: 138)
!1436 = !DISubprogram(name: "sscanf", scope: !1315, file: !1315, line: 385, type: !1437, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1437 = !DISubroutineType(types: !1438)
!1438 = !{!11, !741, !741, null}
!1439 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1440, file: !1312, line: 139)
!1440 = !DISubprogram(name: "tmpfile", scope: !1315, file: !1315, line: 159, type: !1441, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1441 = !DISubroutineType(types: !1442)
!1442 = !{!1323}
!1443 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1444, file: !1312, line: 141)
!1444 = !DISubprogram(name: "tmpnam", scope: !1315, file: !1315, line: 173, type: !1445, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1445 = !DISubroutineType(types: !1446)
!1446 = !{!655, !655}
!1447 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1448, file: !1312, line: 143)
!1448 = !DISubprogram(name: "ungetc", scope: !1315, file: !1315, line: 639, type: !1358, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1449 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1450, file: !1312, line: 144)
!1450 = !DISubprogram(name: "vfprintf", scope: !1345, file: !1345, line: 124, type: !1451, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1451 = !DISubroutineType(types: !1452)
!1452 = !{!11, !1340, !741, !782}
!1453 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1454, file: !1312, line: 145)
!1454 = !DISubprogram(name: "vprintf", scope: !1345, file: !1345, line: 114, type: !1455, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1455 = !DISubroutineType(types: !1456)
!1456 = !{!11, !741, !782}
!1457 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1458, file: !1312, line: 146)
!1458 = !DISubprogram(name: "vsprintf", scope: !1345, file: !1345, line: 43, type: !1459, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1459 = !DISubroutineType(types: !1460)
!1460 = !{!11, !809, !741, !782}
!1461 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1462, file: !1312, line: 175)
!1462 = !DISubprogram(name: "snprintf", scope: !1315, file: !1315, line: 340, type: !1463, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1463 = !DISubroutineType(types: !1464)
!1464 = !{!11, !809, !694, !741, null}
!1465 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1466, file: !1312, line: 176)
!1466 = !DISubprogram(name: "vfscanf", scope: !1315, file: !1315, line: 420, type: !1451, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1467 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1468, file: !1312, line: 177)
!1468 = !DISubprogram(name: "vscanf", scope: !1315, file: !1315, line: 428, type: !1455, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1469 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1470, file: !1312, line: 178)
!1470 = !DISubprogram(name: "vsnprintf", scope: !1345, file: !1345, line: 74, type: !1471, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1471 = !DISubroutineType(types: !1472)
!1472 = !{!11, !809, !694, !741, !782}
!1473 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !45, entity: !1474, file: !1312, line: 179)
!1474 = !DISubprogram(name: "vsscanf", scope: !1315, file: !1315, line: 432, type: !1475, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1475 = !DISubroutineType(types: !1476)
!1476 = !{!11, !741, !741, !782}
!1477 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1462, file: !1312, line: 185)
!1478 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1466, file: !1312, line: 186)
!1479 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1468, file: !1312, line: 187)
!1480 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1470, file: !1312, line: 188)
!1481 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1474, file: !1312, line: 189)
!1482 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1483, file: !1487, line: 82)
!1483 = !DIDerivedType(tag: DW_TAG_typedef, name: "wctrans_t", file: !1484, line: 48, baseType: !1485)
!1484 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/wctype.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1485 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1486, size: 64)
!1486 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1020)
!1487 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/cwctype", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1488 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1489, file: !1487, line: 83)
!1489 = !DIDerivedType(tag: DW_TAG_typedef, name: "wctype_t", file: !1490, line: 38, baseType: !98)
!1490 = !DIFile(filename: "/nix/store/8hagj3k5q9vkyxnrl8431r4kpn4gf46l-glibc-2.26-131-dev/include/bits/wctype-wchar.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1491 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !636, file: !1487, line: 84)
!1492 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1493, file: !1487, line: 86)
!1493 = !DISubprogram(name: "iswalnum", scope: !1490, file: !1490, line: 95, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1494 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1495, file: !1487, line: 87)
!1495 = !DISubprogram(name: "iswalpha", scope: !1490, file: !1490, line: 101, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1496 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1497, file: !1487, line: 89)
!1497 = !DISubprogram(name: "iswblank", scope: !1490, file: !1490, line: 146, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1498 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1499, file: !1487, line: 91)
!1499 = !DISubprogram(name: "iswcntrl", scope: !1490, file: !1490, line: 104, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1500 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1501, file: !1487, line: 92)
!1501 = !DISubprogram(name: "iswctype", scope: !1490, file: !1490, line: 159, type: !1502, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1502 = !DISubroutineType(types: !1503)
!1503 = !{!11, !636, !1489}
!1504 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1505, file: !1487, line: 93)
!1505 = !DISubprogram(name: "iswdigit", scope: !1490, file: !1490, line: 108, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1506 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1507, file: !1487, line: 94)
!1507 = !DISubprogram(name: "iswgraph", scope: !1490, file: !1490, line: 112, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1508 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1509, file: !1487, line: 95)
!1509 = !DISubprogram(name: "iswlower", scope: !1490, file: !1490, line: 117, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1510 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1511, file: !1487, line: 96)
!1511 = !DISubprogram(name: "iswprint", scope: !1490, file: !1490, line: 120, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1512 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1513, file: !1487, line: 97)
!1513 = !DISubprogram(name: "iswpunct", scope: !1490, file: !1490, line: 125, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1514 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1515, file: !1487, line: 98)
!1515 = !DISubprogram(name: "iswspace", scope: !1490, file: !1490, line: 130, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1516 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1517, file: !1487, line: 99)
!1517 = !DISubprogram(name: "iswupper", scope: !1490, file: !1490, line: 135, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1518 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1519, file: !1487, line: 100)
!1519 = !DISubprogram(name: "iswxdigit", scope: !1490, file: !1490, line: 140, type: !886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1520 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1521, file: !1487, line: 101)
!1521 = !DISubprogram(name: "towctrans", scope: !1484, file: !1484, line: 55, type: !1522, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1522 = !DISubroutineType(types: !1523)
!1523 = !{!636, !636, !1483}
!1524 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1525, file: !1487, line: 102)
!1525 = !DISubprogram(name: "towlower", scope: !1490, file: !1490, line: 166, type: !1526, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1526 = !DISubroutineType(types: !1527)
!1527 = !{!636, !636}
!1528 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1529, file: !1487, line: 103)
!1529 = !DISubprogram(name: "towupper", scope: !1490, file: !1490, line: 169, type: !1526, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1530 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1531, file: !1487, line: 104)
!1531 = !DISubprogram(name: "wctrans", scope: !1484, file: !1484, line: 52, type: !1532, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1532 = !DISubroutineType(types: !1533)
!1533 = !{!1483, !462}
!1534 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2, entity: !1535, file: !1487, line: 105)
!1535 = !DISubprogram(name: "wctype", scope: !1490, file: !1490, line: 155, type: !1536, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1536 = !DISubroutineType(types: !1537)
!1537 = !{!1489, !462}
!1538 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !19, entity: !2, file: !20, line: 4)
!1539 = !{i32 2, !"Dwarf Version", i32 4}
!1540 = !{i32 2, !"Debug Info Version", i32 3}
!1541 = !{i32 1, !"wchar_size", i32 4}
!1542 = !{i32 7, !"PIC Level", i32 2}
!1543 = !{!"clang version 6.0.1 (tags/RELEASE_601/final)"}
!1544 = distinct !DISubprogram(name: "merge", linkageName: "_Z5mergeSt6vectorIiSaIiEES1_", scope: !20, file: !20, line: 8, type: !1545, isLocal: false, isDefinition: true, scopeLine: 8, flags: DIFlagPrototyped, isOptimized: true, unit: !19, variables: !1547)
!1545 = !DISubroutineType(types: !1546)
!1546 = !{!225, !225, !225}
!1547 = !{!1548, !1549, !1550, !1551, !1552, !1553}
!1548 = !DILocalVariable(name: "x", arg: 1, scope: !1544, file: !20, line: 8, type: !225)
!1549 = !DILocalVariable(name: "y", arg: 2, scope: !1544, file: !20, line: 8, type: !225)
!1550 = !DILocalVariable(name: "result", scope: !1544, file: !20, line: 9, type: !225)
!1551 = !DILocalVariable(name: "xi", scope: !1544, file: !20, line: 10, type: !11)
!1552 = !DILocalVariable(name: "yi", scope: !1544, file: !20, line: 10, type: !11)
!1553 = !DILocalVariable(name: "j", scope: !1554, file: !20, line: 11, type: !98)
!1554 = distinct !DILexicalBlock(scope: !1544, file: !20, line: 11, column: 3)
!1555 = !DILocation(line: 8, column: 31, scope: !1544)
!1556 = !DILocation(line: 8, column: 46, scope: !1544)
!1557 = !DILocation(line: 9, column: 15, scope: !1544)
!1558 = !DILocalVariable(name: "this", arg: 1, scope: !1559, type: !1561, flags: DIFlagArtificial | DIFlagObjectPointer)
!1559 = distinct !DISubprogram(name: "vector", linkageName: "_ZNSt6vectorIiSaIiEEC2Ev", scope: !225, file: !33, line: 259, type: !229, isLocal: false, isDefinition: true, scopeLine: 263, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !228, variables: !1560)
!1560 = !{!1558}
!1561 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !225, size: 64)
!1562 = !DILocation(line: 0, scope: !1559, inlinedAt: !1563)
!1563 = distinct !DILocation(line: 9, column: 15, scope: !1544)
!1564 = !DILocalVariable(name: "this", arg: 1, scope: !1565, type: !1567, flags: DIFlagArtificial | DIFlagObjectPointer)
!1565 = distinct !DISubprogram(name: "_Vector_base", linkageName: "_ZNSt12_Vector_baseIiSaIiEEC2Ev", scope: !34, file: !33, line: 126, type: !186, isLocal: false, isDefinition: true, scopeLine: 127, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !185, variables: !1566)
!1566 = !{!1564}
!1567 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !34, size: 64)
!1568 = !DILocation(line: 0, scope: !1565, inlinedAt: !1569)
!1569 = distinct !DILocation(line: 263, column: 9, scope: !1559, inlinedAt: !1563)
!1570 = !DILocalVariable(name: "this", arg: 1, scope: !1571, type: !1573, flags: DIFlagArtificial | DIFlagObjectPointer)
!1571 = distinct !DISubprogram(name: "_Vector_impl", linkageName: "_ZNSt12_Vector_baseIiSaIiEE12_Vector_implC2Ev", scope: !37, file: !33, line: 88, type: !155, isLocal: false, isDefinition: true, scopeLine: 90, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !154, variables: !1572)
!1572 = !{!1570}
!1573 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !37, size: 64)
!1574 = !DILocation(line: 0, scope: !1571, inlinedAt: !1575)
!1575 = distinct !DILocation(line: 127, column: 9, scope: !1565, inlinedAt: !1569)
!1576 = !DILocation(line: 90, column: 4, scope: !1571, inlinedAt: !1575)
!1577 = !DILocation(line: 89, column: 34, scope: !1571, inlinedAt: !1575)
!1578 = !DILocation(line: 10, column: 7, scope: !1544)
!1579 = !DILocation(line: 10, column: 15, scope: !1544)
!1580 = !DILocation(line: 11, column: 22, scope: !1554)
!1581 = !DILocalVariable(name: "this", arg: 1, scope: !1582, type: !1584, flags: DIFlagArtificial | DIFlagObjectPointer)
!1582 = distinct !DISubprogram(name: "size", linkageName: "_ZNKSt6vectorIiSaIiEE4sizeEv", scope: !225, file: !33, line: 670, type: !369, isLocal: false, isDefinition: true, scopeLine: 671, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !368, variables: !1583)
!1583 = !{!1581}
!1584 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !251, size: 64)
!1585 = !DILocation(line: 0, scope: !1582, inlinedAt: !1586)
!1586 = distinct !DILocation(line: 11, column: 35, scope: !1587)
!1587 = distinct !DILexicalBlock(scope: !1554, file: !20, line: 11, column: 3)
!1588 = !DILocation(line: 671, column: 40, scope: !1582, inlinedAt: !1586)
!1589 = !{!1590, !1592, i64 8}
!1590 = !{!"_ZTSSt12_Vector_baseIiSaIiEE", !1591, i64 0}
!1591 = !{!"_ZTSNSt12_Vector_baseIiSaIiEE12_Vector_implE", !1592, i64 0, !1592, i64 8, !1592, i64 16}
!1592 = !{!"any pointer", !1593, i64 0}
!1593 = !{!"omnipotent char", !1594, i64 0}
!1594 = !{!"Simple C++ TBAA"}
!1595 = !DILocation(line: 671, column: 66, scope: !1582, inlinedAt: !1586)
!1596 = !{!1590, !1592, i64 0}
!1597 = !DILocation(line: 671, column: 50, scope: !1582, inlinedAt: !1586)
!1598 = !DILocation(line: 0, scope: !1582, inlinedAt: !1599)
!1599 = distinct !DILocation(line: 11, column: 46, scope: !1587)
!1600 = !DILocation(line: 671, column: 40, scope: !1582, inlinedAt: !1599)
!1601 = !DILocation(line: 671, column: 66, scope: !1582, inlinedAt: !1599)
!1602 = !DILocation(line: 671, column: 50, scope: !1582, inlinedAt: !1599)
!1603 = !DILocation(line: 11, column: 31, scope: !1587)
!1604 = !DILocation(line: 11, column: 3, scope: !1554)
!1605 = !DILocation(line: 941, column: 47, scope: !1606, inlinedAt: !1611)
!1606 = distinct !DILexicalBlock(scope: !1607, file: !33, line: 941, column: 6)
!1607 = distinct !DISubprogram(name: "push_back", linkageName: "_ZNSt6vectorIiSaIiEE9push_backERKi", scope: !225, file: !33, line: 939, type: !417, isLocal: false, isDefinition: true, scopeLine: 940, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !416, variables: !1608)
!1608 = !{!1609, !1610}
!1609 = !DILocalVariable(name: "this", arg: 1, scope: !1607, type: !1561, flags: DIFlagArtificial | DIFlagObjectPointer)
!1610 = !DILocalVariable(name: "__x", arg: 2, scope: !1607, file: !33, line: 939, type: !244)
!1611 = distinct !DILocation(line: 16, column: 14, scope: !1612)
!1612 = distinct !DILexicalBlock(scope: !1613, file: !20, line: 15, column: 12)
!1613 = distinct !DILexicalBlock(scope: !1614, file: !20, line: 12, column: 9)
!1614 = distinct !DILexicalBlock(scope: !1587, file: !20, line: 11, column: 59)
!1615 = !DILocation(line: 941, column: 20, scope: !1606, inlinedAt: !1611)
!1616 = !DILocation(line: 798, column: 25, scope: !1617, inlinedAt: !1621)
!1617 = distinct !DISubprogram(name: "operator[]", linkageName: "_ZNSt6vectorIiSaIiEEixEm", scope: !225, file: !33, line: 795, type: !383, isLocal: false, isDefinition: true, scopeLine: 796, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !382, variables: !1618)
!1618 = !{!1619, !1620}
!1619 = !DILocalVariable(name: "this", arg: 1, scope: !1617, type: !1561, flags: DIFlagArtificial | DIFlagObjectPointer)
!1620 = !DILocalVariable(name: "__n", arg: 2, scope: !1617, file: !33, line: 795, type: !221)
!1621 = distinct !DILocation(line: 12, column: 9, scope: !1613)
!1622 = !DILocation(line: 798, column: 25, scope: !1617, inlinedAt: !1623)
!1623 = distinct !DILocation(line: 12, column: 18, scope: !1613)
!1624 = !DILocation(line: 12, column: 11, scope: !1613)
!1625 = !DILocation(line: 0, scope: !1617, inlinedAt: !1621)
!1626 = !DILocation(line: 795, column: 28, scope: !1617, inlinedAt: !1621)
!1627 = !DILocation(line: 798, column: 34, scope: !1617, inlinedAt: !1621)
!1628 = !DILocation(line: 12, column: 9, scope: !1613)
!1629 = !{!1630, !1630, i64 0}
!1630 = !{!"int", !1593, i64 0}
!1631 = !DILocation(line: 12, column: 20, scope: !1613)
!1632 = !DILocation(line: 0, scope: !1617, inlinedAt: !1623)
!1633 = !DILocation(line: 795, column: 28, scope: !1617, inlinedAt: !1623)
!1634 = !DILocation(line: 798, column: 34, scope: !1617, inlinedAt: !1623)
!1635 = !DILocation(line: 12, column: 18, scope: !1613)
!1636 = !DILocation(line: 12, column: 15, scope: !1613)
!1637 = !DILocation(line: 941, column: 30, scope: !1606, inlinedAt: !1611)
!1638 = !DILocation(line: 12, column: 9, scope: !1614)
!1639 = !DILocation(line: 0, scope: !1617, inlinedAt: !1640)
!1640 = distinct !DILocation(line: 13, column: 24, scope: !1641)
!1641 = distinct !DILexicalBlock(scope: !1613, file: !20, line: 12, column: 25)
!1642 = !DILocation(line: 795, column: 28, scope: !1617, inlinedAt: !1640)
!1643 = !DILocation(line: 0, scope: !1607, inlinedAt: !1644)
!1644 = distinct !DILocation(line: 13, column: 14, scope: !1641)
!1645 = !DILocation(line: 939, column: 35, scope: !1607, inlinedAt: !1644)
!1646 = !DILocation(line: 941, column: 6, scope: !1607, inlinedAt: !1644)
!1647 = !DILocalVariable(name: "__p", arg: 2, scope: !1648, file: !49, line: 474, type: !55)
!1648 = distinct !DISubprogram(name: "construct<int, const int &>", linkageName: "_ZNSt16allocator_traitsISaIiEE9constructIiJRKiEEEvRS0_PT_DpOT0_", scope: !48, file: !49, line: 474, type: !1649, isLocal: false, isDefinition: true, scopeLine: 475, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !1652, declaration: !1651, variables: !1657)
!1649 = !DISubroutineType(types: !1650)
!1650 = !{null, !56, !55, !91}
!1651 = !DISubprogram(name: "construct<int, const int &>", linkageName: "_ZNSt16allocator_traitsISaIiEE9constructIiJRKiEEEvRS0_PT_DpOT0_", scope: !48, file: !49, line: 474, type: !1649, isLocal: false, isDefinition: false, scopeLine: 474, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true, templateParams: !1652)
!1652 = !{!1653, !1654}
!1653 = !DITemplateTypeParameter(name: "_Up", type: !11)
!1654 = !DITemplateValueParameter(tag: DW_TAG_GNU_template_parameter_pack, name: "_Args", value: !1655)
!1655 = !{!1656}
!1656 = !DITemplateTypeParameter(type: !91)
!1657 = !{!1658, !1647, !1659}
!1658 = !DILocalVariable(name: "__a", arg: 1, scope: !1648, file: !49, line: 474, type: !56)
!1659 = !DILocalVariable(name: "__args", arg: 3, scope: !1648, file: !49, line: 474, type: !91)
!1660 = !DILocation(line: 474, column: 38, scope: !1648, inlinedAt: !1661)
!1661 = distinct !DILocation(line: 943, column: 6, scope: !1662, inlinedAt: !1644)
!1662 = distinct !DILexicalBlock(scope: !1606, file: !33, line: 942, column: 4)
!1663 = !DILocation(line: 474, column: 54, scope: !1648, inlinedAt: !1661)
!1664 = !DILocalVariable(name: "__p", arg: 2, scope: !1665, file: !65, line: 135, type: !55)
!1665 = distinct !DISubprogram(name: "construct<int, const int &>", linkageName: "_ZN9__gnu_cxx13new_allocatorIiE9constructIiJRKiEEEvPT_DpOT0_", scope: !64, file: !65, line: 135, type: !1666, isLocal: false, isDefinition: true, scopeLine: 136, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !1652, declaration: !1668, variables: !1669)
!1666 = !DISubroutineType(types: !1667)
!1667 = !{null, !70, !55, !91}
!1668 = !DISubprogram(name: "construct<int, const int &>", linkageName: "_ZN9__gnu_cxx13new_allocatorIiE9constructIiJRKiEEEvPT_DpOT0_", scope: !64, file: !65, line: 135, type: !1666, isLocal: false, isDefinition: false, scopeLine: 135, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true, templateParams: !1652)
!1669 = !{!1670, !1664, !1672}
!1670 = !DILocalVariable(name: "this", arg: 1, scope: !1665, type: !1671, flags: DIFlagArtificial | DIFlagObjectPointer)
!1671 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !64, size: 64)
!1672 = !DILocalVariable(name: "__args", arg: 3, scope: !1665, file: !65, line: 135, type: !91)
!1673 = !DILocation(line: 135, column: 17, scope: !1665, inlinedAt: !1674)
!1674 = distinct !DILocation(line: 475, column: 8, scope: !1648, inlinedAt: !1661)
!1675 = !DILocation(line: 135, column: 33, scope: !1665, inlinedAt: !1674)
!1676 = !DILocation(line: 136, column: 4, scope: !1665, inlinedAt: !1674)
!1677 = !DILocation(line: 945, column: 6, scope: !1662, inlinedAt: !1644)
!1678 = !DILocation(line: 946, column: 4, scope: !1662, inlinedAt: !1644)
!1679 = !DILocation(line: 948, column: 4, scope: !1606, inlinedAt: !1644)
!1680 = !DILocation(line: 14, column: 9, scope: !1641)
!1681 = !DILocation(line: 15, column: 5, scope: !1641)
!1682 = !DILocation(line: 21, column: 1, scope: !1641)
!1683 = !DILocalVariable(name: "this", arg: 1, scope: !1684, type: !1561, flags: DIFlagArtificial | DIFlagObjectPointer)
!1684 = distinct !DISubprogram(name: "~vector", linkageName: "_ZNSt6vectorIiSaIiEED2Ev", scope: !225, file: !33, line: 433, type: !229, isLocal: false, isDefinition: true, scopeLine: 434, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !267, variables: !1685)
!1685 = !{!1683}
!1686 = !DILocation(line: 0, scope: !1684, inlinedAt: !1687)
!1687 = distinct !DILocation(line: 21, column: 1, scope: !1544)
!1688 = !DILocalVariable(name: "this", arg: 1, scope: !1689, type: !1567, flags: DIFlagArtificial | DIFlagObjectPointer)
!1689 = distinct !DISubprogram(name: "~_Vector_base", linkageName: "_ZNSt12_Vector_baseIiSaIiEED2Ev", scope: !34, file: !33, line: 161, type: !186, isLocal: false, isDefinition: true, scopeLine: 162, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !209, variables: !1690)
!1690 = !{!1688}
!1691 = !DILocation(line: 0, scope: !1689, inlinedAt: !1692)
!1692 = distinct !DILocation(line: 435, column: 33, scope: !1693, inlinedAt: !1687)
!1693 = distinct !DILexicalBlock(scope: !1684, file: !33, line: 434, column: 7)
!1694 = !DILocation(line: 162, column: 37, scope: !1695, inlinedAt: !1692)
!1695 = distinct !DILexicalBlock(scope: !1689, file: !33, line: 162, column: 7)
!1696 = !DILocalVariable(name: "this", arg: 1, scope: !1697, type: !1567, flags: DIFlagArtificial | DIFlagObjectPointer)
!1697 = distinct !DISubprogram(name: "_M_deallocate", linkageName: "_ZNSt12_Vector_baseIiSaIiEE13_M_deallocateEPim", scope: !34, file: !33, line: 176, type: !214, isLocal: false, isDefinition: true, scopeLine: 177, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !213, variables: !1698)
!1698 = !{!1696, !1699, !1700}
!1699 = !DILocalVariable(name: "__p", arg: 2, scope: !1697, file: !33, line: 176, type: !41)
!1700 = !DILocalVariable(name: "__n", arg: 3, scope: !1697, file: !33, line: 176, type: !96)
!1701 = !DILocation(line: 0, scope: !1697, inlinedAt: !1702)
!1702 = distinct !DILocation(line: 162, column: 9, scope: !1695, inlinedAt: !1692)
!1703 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !1702)
!1704 = !DILocation(line: 179, column: 6, scope: !1705, inlinedAt: !1702)
!1705 = distinct !DILexicalBlock(scope: !1697, file: !33, line: 179, column: 6)
!1706 = !DILocation(line: 179, column: 6, scope: !1697, inlinedAt: !1702)
!1707 = !DILocalVariable(name: "__p", arg: 2, scope: !1708, file: !49, line: 461, type: !54)
!1708 = distinct !DISubprogram(name: "deallocate", linkageName: "_ZNSt16allocator_traitsISaIiEE10deallocateERS0_Pim", scope: !48, file: !49, line: 461, type: !125, isLocal: false, isDefinition: true, scopeLine: 462, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !124, variables: !1709)
!1709 = !{!1710, !1707, !1711}
!1710 = !DILocalVariable(name: "__a", arg: 1, scope: !1708, file: !49, line: 461, type: !56)
!1711 = !DILocalVariable(name: "__n", arg: 3, scope: !1708, file: !49, line: 461, type: !119)
!1712 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !1713)
!1713 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !1702)
!1714 = !DILocalVariable(name: "__p", arg: 2, scope: !1715, file: !65, line: 116, type: !80)
!1715 = distinct !DISubprogram(name: "deallocate", linkageName: "_ZN9__gnu_cxx13new_allocatorIiE10deallocateEPim", scope: !64, file: !65, line: 116, type: !102, isLocal: false, isDefinition: true, scopeLine: 117, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !101, variables: !1716)
!1716 = !{!1717, !1714, !1718}
!1717 = !DILocalVariable(name: "this", arg: 1, scope: !1715, type: !1671, flags: DIFlagArtificial | DIFlagObjectPointer)
!1718 = !DILocalVariable(arg: 3, scope: !1715, file: !65, line: 116, type: !95)
!1719 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !1720)
!1720 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !1713)
!1721 = !DILocation(line: 125, column: 20, scope: !1715, inlinedAt: !1720)
!1722 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !1720)
!1723 = !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !1702)
!1724 = !DILocation(line: 21, column: 1, scope: !1544)
!1725 = !DILocation(line: 0, scope: !1617, inlinedAt: !1726)
!1726 = distinct !DILocation(line: 16, column: 24, scope: !1612)
!1727 = !DILocation(line: 795, column: 28, scope: !1617, inlinedAt: !1726)
!1728 = !DILocation(line: 0, scope: !1607, inlinedAt: !1611)
!1729 = !DILocation(line: 939, column: 35, scope: !1607, inlinedAt: !1611)
!1730 = !DILocation(line: 941, column: 6, scope: !1607, inlinedAt: !1611)
!1731 = !DILocation(line: 474, column: 38, scope: !1648, inlinedAt: !1732)
!1732 = distinct !DILocation(line: 943, column: 6, scope: !1662, inlinedAt: !1611)
!1733 = !DILocation(line: 474, column: 54, scope: !1648, inlinedAt: !1732)
!1734 = !DILocation(line: 135, column: 17, scope: !1665, inlinedAt: !1735)
!1735 = distinct !DILocation(line: 475, column: 8, scope: !1648, inlinedAt: !1732)
!1736 = !DILocation(line: 135, column: 33, scope: !1665, inlinedAt: !1735)
!1737 = !DILocation(line: 136, column: 4, scope: !1665, inlinedAt: !1735)
!1738 = !DILocation(line: 945, column: 6, scope: !1662, inlinedAt: !1611)
!1739 = !DILocation(line: 946, column: 4, scope: !1662, inlinedAt: !1611)
!1740 = !DILocation(line: 948, column: 4, scope: !1606, inlinedAt: !1611)
!1741 = !DILocation(line: 17, column: 9, scope: !1612)
!1742 = !DILocation(line: 11, column: 55, scope: !1587)
!1743 = !DILocation(line: 11, column: 42, scope: !1587)
!1744 = distinct !{!1744, !1604, !1745}
!1745 = !DILocation(line: 19, column: 3, scope: !1554)
!1746 = !{!1590, !1592, i64 16}
!1747 = distinct !DISubprogram(name: "main", scope: !20, file: !20, line: 23, type: !1228, isLocal: false, isDefinition: true, scopeLine: 23, flags: DIFlagPrototyped, isOptimized: true, unit: !19, variables: !1748)
!1748 = !{!1749, !1750}
!1749 = !DILocalVariable(name: "x", scope: !1747, file: !20, line: 24, type: !225)
!1750 = !DILocalVariable(name: "y", scope: !1747, file: !20, line: 27, type: !225)
!1751 = !DILocalVariable(name: "this", arg: 1, scope: !1752, type: !1561, flags: DIFlagArtificial | DIFlagObjectPointer)
!1752 = distinct !DISubprogram(name: "vector", linkageName: "_ZNSt6vectorIiSaIiEEC2EmRKS0_", scope: !225, file: !33, line: 283, type: !239, isLocal: false, isDefinition: true, scopeLine: 285, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !238, variables: !1753)
!1753 = !{!1751, !1754, !1755}
!1754 = !DILocalVariable(name: "__n", arg: 2, scope: !1752, file: !33, line: 283, type: !221)
!1755 = !DILocalVariable(name: "__a", arg: 3, scope: !1752, file: !33, line: 283, type: !235)
!1756 = !DILocation(line: 0, scope: !1752, inlinedAt: !1757)
!1757 = distinct !DILocation(line: 24, column: 15, scope: !1747)
!1758 = !DILocation(line: 283, column: 24, scope: !1752, inlinedAt: !1757)
!1759 = !DILocation(line: 283, column: 51, scope: !1752, inlinedAt: !1757)
!1760 = !DILocalVariable(name: "this", arg: 1, scope: !1761, type: !1567, flags: DIFlagArtificial | DIFlagObjectPointer)
!1761 = distinct !DISubprogram(name: "_Vector_base", linkageName: "_ZNSt12_Vector_baseIiSaIiEEC2EmRKS0_", scope: !34, file: !33, line: 136, type: !197, isLocal: false, isDefinition: true, scopeLine: 138, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !196, variables: !1762)
!1762 = !{!1760, !1763, !1764}
!1763 = !DILocalVariable(name: "__n", arg: 2, scope: !1761, file: !33, line: 136, type: !96)
!1764 = !DILocalVariable(name: "__a", arg: 3, scope: !1761, file: !33, line: 136, type: !191)
!1765 = !DILocation(line: 0, scope: !1761, inlinedAt: !1766)
!1766 = distinct !DILocation(line: 284, column: 9, scope: !1752, inlinedAt: !1757)
!1767 = !DILocation(line: 136, column: 27, scope: !1761, inlinedAt: !1766)
!1768 = !DILocation(line: 136, column: 54, scope: !1761, inlinedAt: !1766)
!1769 = !DILocalVariable(name: "this", arg: 1, scope: !1770, type: !1573, flags: DIFlagArtificial | DIFlagObjectPointer)
!1770 = distinct !DISubprogram(name: "_Vector_impl", linkageName: "_ZNSt12_Vector_baseIiSaIiEE12_Vector_implC2ERKS0_", scope: !37, file: !33, line: 92, type: !159, isLocal: false, isDefinition: true, scopeLine: 94, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !158, variables: !1771)
!1771 = !{!1769, !1772}
!1772 = !DILocalVariable(name: "__a", arg: 2, scope: !1770, file: !33, line: 92, type: !161)
!1773 = !DILocation(line: 0, scope: !1770, inlinedAt: !1774)
!1774 = distinct !DILocation(line: 137, column: 9, scope: !1761, inlinedAt: !1766)
!1775 = !DILocation(line: 92, column: 37, scope: !1770, inlinedAt: !1774)
!1776 = !DILocalVariable(name: "this", arg: 1, scope: !1777, type: !1567, flags: DIFlagArtificial | DIFlagObjectPointer)
!1777 = distinct !DISubprogram(name: "_M_create_storage", linkageName: "_ZNSt12_Vector_baseIiSaIiEE17_M_create_storageEm", scope: !34, file: !33, line: 185, type: !194, isLocal: false, isDefinition: true, scopeLine: 186, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !216, variables: !1778)
!1778 = !{!1776, !1779}
!1779 = !DILocalVariable(name: "__n", arg: 2, scope: !1777, file: !33, line: 185, type: !96)
!1780 = !DILocation(line: 0, scope: !1777, inlinedAt: !1781)
!1781 = distinct !DILocation(line: 138, column: 9, scope: !1782, inlinedAt: !1766)
!1782 = distinct !DILexicalBlock(scope: !1761, file: !33, line: 138, column: 7)
!1783 = !DILocation(line: 185, column: 32, scope: !1777, inlinedAt: !1781)
!1784 = !DILocalVariable(name: "this", arg: 1, scope: !1785, type: !1567, flags: DIFlagArtificial | DIFlagObjectPointer)
!1785 = distinct !DISubprogram(name: "_M_allocate", linkageName: "_ZNSt12_Vector_baseIiSaIiEE11_M_allocateEm", scope: !34, file: !33, line: 169, type: !211, isLocal: false, isDefinition: true, scopeLine: 170, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !210, variables: !1786)
!1786 = !{!1784, !1787}
!1787 = !DILocalVariable(name: "__n", arg: 2, scope: !1785, file: !33, line: 169, type: !96)
!1788 = !DILocation(line: 0, scope: !1785, inlinedAt: !1789)
!1789 = distinct !DILocation(line: 187, column: 33, scope: !1777, inlinedAt: !1781)
!1790 = !DILocation(line: 169, column: 26, scope: !1785, inlinedAt: !1789)
!1791 = !DILocalVariable(name: "__n", arg: 2, scope: !1792, file: !49, line: 435, type: !119)
!1792 = distinct !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaIiEE8allocateERS0_m", scope: !48, file: !49, line: 435, type: !52, isLocal: false, isDefinition: true, scopeLine: 436, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !51, variables: !1793)
!1793 = !{!1794, !1791}
!1794 = !DILocalVariable(name: "__a", arg: 1, scope: !1792, file: !49, line: 435, type: !56)
!1795 = !DILocation(line: 435, column: 47, scope: !1792, inlinedAt: !1796)
!1796 = distinct !DILocation(line: 172, column: 20, scope: !1785, inlinedAt: !1789)
!1797 = !DILocalVariable(name: "__n", arg: 2, scope: !1798, file: !65, line: 99, type: !95)
!1798 = distinct !DISubprogram(name: "allocate", linkageName: "_ZN9__gnu_cxx13new_allocatorIiE8allocateEmPKv", scope: !64, file: !65, line: 99, type: !93, isLocal: false, isDefinition: true, scopeLine: 100, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !92, variables: !1799)
!1799 = !{!1800, !1797, !1801}
!1800 = !DILocalVariable(name: "this", arg: 1, scope: !1798, type: !1671, flags: DIFlagArtificial | DIFlagObjectPointer)
!1801 = !DILocalVariable(arg: 3, scope: !1798, file: !65, line: 99, type: !99)
!1802 = !DILocation(line: 99, column: 26, scope: !1798, inlinedAt: !1803)
!1803 = distinct !DILocation(line: 436, column: 20, scope: !1792, inlinedAt: !1796)
!1804 = !DILocation(line: 99, column: 43, scope: !1798, inlinedAt: !1803)
!1805 = !DILocation(line: 111, column: 27, scope: !1798, inlinedAt: !1803)
!1806 = !DILocalVariable(name: "this", arg: 1, scope: !1807, type: !1561, flags: DIFlagArtificial | DIFlagObjectPointer)
!1807 = distinct !DISubprogram(name: "_M_default_initialize", linkageName: "_ZNSt6vectorIiSaIiEE21_M_default_initializeEm", scope: !225, file: !33, line: 1344, type: !373, isLocal: false, isDefinition: true, scopeLine: 1345, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !447, variables: !1808)
!1808 = !{!1806, !1809}
!1809 = !DILocalVariable(name: "__n", arg: 2, scope: !1807, file: !33, line: 1344, type: !221)
!1810 = !DILocation(line: 0, scope: !1807, inlinedAt: !1811)
!1811 = distinct !DILocation(line: 285, column: 9, scope: !1812, inlinedAt: !1757)
!1812 = distinct !DILexicalBlock(scope: !1752, file: !33, line: 285, column: 7)
!1813 = !DILocation(line: 1344, column: 39, scope: !1807, inlinedAt: !1811)
!1814 = !DILocalVariable(name: "__first", arg: 1, scope: !1815, file: !1816, line: 643, type: !55)
!1815 = distinct !DISubprogram(name: "__uninitialized_default_n_a<int *, unsigned long, int>", linkageName: "_ZSt27__uninitialized_default_n_aIPimiET_S1_T0_RSaIT1_E", scope: !2, file: !1816, line: 643, type: !1817, isLocal: false, isDefinition: true, scopeLine: 645, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !1822, variables: !1819)
!1816 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/bits/stl_uninitialized.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1817 = !DISubroutineType(types: !1818)
!1818 = !{!55, !55, !98, !144}
!1819 = !{!1814, !1820, !1821}
!1820 = !DILocalVariable(name: "__n", arg: 2, scope: !1815, file: !1816, line: 643, type: !98)
!1821 = !DILocalVariable(arg: 3, scope: !1815, file: !1816, line: 644, type: !144)
!1822 = !{!1823, !1824, !108}
!1823 = !DITemplateTypeParameter(name: "_ForwardIterator", type: !55)
!1824 = !DITemplateTypeParameter(name: "_Size", type: !98)
!1825 = !DILocation(line: 643, column: 50, scope: !1815, inlinedAt: !1826)
!1826 = distinct !DILocation(line: 1347, column: 4, scope: !1807, inlinedAt: !1811)
!1827 = !DILocation(line: 643, column: 65, scope: !1815, inlinedAt: !1826)
!1828 = !DILocalVariable(name: "__first", arg: 1, scope: !1829, file: !1816, line: 574, type: !55)
!1829 = distinct !DISubprogram(name: "__uninitialized_default_n<int *, unsigned long>", linkageName: "_ZSt25__uninitialized_default_nIPimET_S1_T0_", scope: !2, file: !1816, line: 574, type: !1830, isLocal: false, isDefinition: true, scopeLine: 575, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !1835, variables: !1832)
!1830 = !DISubroutineType(types: !1831)
!1831 = !{!55, !55, !98}
!1832 = !{!1828, !1833, !1834}
!1833 = !DILocalVariable(name: "__n", arg: 2, scope: !1829, file: !1816, line: 574, type: !98)
!1834 = !DILocalVariable(name: "__assignable", scope: !1829, file: !1816, line: 579, type: !483)
!1835 = !{!1823, !1824}
!1836 = !DILocation(line: 574, column: 48, scope: !1829, inlinedAt: !1837)
!1837 = distinct !DILocation(line: 645, column: 14, scope: !1815, inlinedAt: !1826)
!1838 = !DILocation(line: 574, column: 63, scope: !1829, inlinedAt: !1837)
!1839 = !DILocation(line: 579, column: 18, scope: !1829, inlinedAt: !1837)
!1840 = !DILocalVariable(name: "__first", arg: 1, scope: !1841, file: !1816, line: 543, type: !55)
!1841 = distinct !DISubprogram(name: "__uninit_default_n<int *, unsigned long>", linkageName: "_ZNSt27__uninitialized_default_n_1ILb1EE18__uninit_default_nIPimEET_S3_T0_", scope: !1842, file: !1816, line: 543, type: !1830, isLocal: false, isDefinition: true, scopeLine: 544, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !1835, declaration: !1845, variables: !1846)
!1842 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__uninitialized_default_n_1<true>", scope: !2, file: !1816, line: 539, size: 8, elements: !25, templateParams: !1843, identifier: "_ZTSSt27__uninitialized_default_n_1ILb1EE")
!1843 = !{!1844}
!1844 = !DITemplateValueParameter(name: "_TrivialValueType", type: !13, value: i8 1)
!1845 = !DISubprogram(name: "__uninit_default_n<int *, unsigned long>", linkageName: "_ZNSt27__uninitialized_default_n_1ILb1EE18__uninit_default_nIPimEET_S3_T0_", scope: !1842, file: !1816, line: 543, type: !1830, isLocal: false, isDefinition: false, scopeLine: 543, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true, templateParams: !1835)
!1846 = !{!1840, !1847}
!1847 = !DILocalVariable(name: "__n", arg: 2, scope: !1841, file: !1816, line: 543, type: !98)
!1848 = !DILocation(line: 543, column: 45, scope: !1841, inlinedAt: !1849)
!1849 = distinct !DILocation(line: 581, column: 14, scope: !1829, inlinedAt: !1837)
!1850 = !DILocation(line: 543, column: 60, scope: !1841, inlinedAt: !1849)
!1851 = !DILocalVariable(name: "__first", arg: 1, scope: !1852, file: !1853, line: 784, type: !55)
!1852 = distinct !DISubprogram(name: "fill_n<int *, unsigned long, int>", linkageName: "_ZSt6fill_nIPimiET_S1_T0_RKT1_", scope: !2, file: !1853, line: 784, type: !1854, isLocal: false, isDefinition: true, scopeLine: 785, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !1859, variables: !1856)
!1853 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/bits/stl_algobase.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1854 = !DISubroutineType(types: !1855)
!1855 = !{!55, !55, !98, !91}
!1856 = !{!1851, !1857, !1858}
!1857 = !DILocalVariable(name: "__n", arg: 2, scope: !1852, file: !1853, line: 784, type: !98)
!1858 = !DILocalVariable(name: "__value", arg: 3, scope: !1852, file: !1853, line: 784, type: !91)
!1859 = !{!1860, !1824, !108}
!1860 = !DITemplateTypeParameter(name: "_OI", type: !55)
!1861 = !DILocation(line: 784, column: 16, scope: !1852, inlinedAt: !1862)
!1862 = distinct !DILocation(line: 548, column: 11, scope: !1841, inlinedAt: !1849)
!1863 = !DILocation(line: 784, column: 31, scope: !1852, inlinedAt: !1862)
!1864 = !DILocalVariable(name: "__first", arg: 1, scope: !1865, file: !1853, line: 749, type: !55)
!1865 = distinct !DISubprogram(name: "__fill_n_a<int *, unsigned long, int>", linkageName: "_ZSt10__fill_n_aIPimiEN9__gnu_cxx11__enable_ifIXsr11__is_scalarIT1_EE7__valueET_E6__typeES4_T0_RKS3_", scope: !2, file: !1853, line: 749, type: !1866, isLocal: false, isDefinition: true, scopeLine: 750, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !1880, variables: !1874)
!1866 = !DISubroutineType(types: !1867)
!1867 = !{!1868, !55, !98, !91}
!1868 = !DIDerivedType(tag: DW_TAG_typedef, name: "__type", scope: !1870, file: !1869, line: 50, baseType: !55)
!1869 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/ext/type_traits.h", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1870 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__enable_if<true, int *>", scope: !45, file: !1869, line: 49, size: 8, elements: !25, templateParams: !1871, identifier: "_ZTSN9__gnu_cxx11__enable_ifILb1EPiEE")
!1871 = !{!1872, !1873}
!1872 = !DITemplateValueParameter(type: !13, value: i8 1)
!1873 = !DITemplateTypeParameter(type: !55)
!1874 = !{!1864, !1875, !1876, !1877, !1878}
!1875 = !DILocalVariable(name: "__n", arg: 2, scope: !1865, file: !1853, line: 749, type: !98)
!1876 = !DILocalVariable(name: "__value", arg: 3, scope: !1865, file: !1853, line: 749, type: !91)
!1877 = !DILocalVariable(name: "__tmp", scope: !1865, file: !1853, line: 751, type: !89)
!1878 = !DILocalVariable(name: "__niter", scope: !1879, file: !1853, line: 752, type: !98)
!1879 = distinct !DILexicalBlock(scope: !1865, file: !1853, line: 752, column: 7)
!1880 = !{!1881, !1824, !108}
!1881 = !DITemplateTypeParameter(name: "_OutputIterator", type: !55)
!1882 = !DILocation(line: 749, column: 32, scope: !1865, inlinedAt: !1883)
!1883 = distinct !DILocation(line: 789, column: 18, scope: !1852, inlinedAt: !1862)
!1884 = !DILocation(line: 749, column: 47, scope: !1865, inlinedAt: !1883)
!1885 = !DILocation(line: 751, column: 17, scope: !1865, inlinedAt: !1883)
!1886 = !DILocation(line: 752, column: 32, scope: !1879, inlinedAt: !1883)
!1887 = !DILocation(line: 754, column: 11, scope: !1888, inlinedAt: !1883)
!1888 = distinct !DILexicalBlock(scope: !1879, file: !1853, line: 752, column: 7)
!1889 = !DILocalVariable(name: "this", arg: 1, scope: !1890, type: !1561, flags: DIFlagArtificial | DIFlagObjectPointer)
!1890 = distinct !DISubprogram(name: "push_back", linkageName: "_ZNSt6vectorIiSaIiEE9push_backEOi", scope: !225, file: !33, line: 953, type: !420, isLocal: false, isDefinition: true, scopeLine: 954, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !419, variables: !1891)
!1891 = !{!1889, !1892}
!1892 = !DILocalVariable(name: "__x", arg: 2, scope: !1890, file: !33, line: 953, type: !422)
!1893 = !DILocation(line: 0, scope: !1890, inlinedAt: !1894)
!1894 = distinct !DILocation(line: 25, column: 5, scope: !1747)
!1895 = !DILocalVariable(name: "this", arg: 1, scope: !1896, type: !1561, flags: DIFlagArtificial | DIFlagObjectPointer)
!1896 = distinct !DISubprogram(name: "emplace_back<int>", linkageName: "_ZNSt6vectorIiSaIiEE12emplace_backIJiEEEvDpOT_", scope: !225, file: !1897, line: 96, type: !1898, isLocal: false, isDefinition: true, scopeLine: 97, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !1901, declaration: !1900, variables: !1904)
!1897 = !DIFile(filename: "/nix/store/czxcii58v6d3yhrq9r667zw91192rcgk-gcc-7.3.0/include/c++/7.3.0/bits/vector.tcc", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!1898 = !DISubroutineType(types: !1899)
!1899 = !{null, !231, !588}
!1900 = !DISubprogram(name: "emplace_back<int>", linkageName: "_ZNSt6vectorIiSaIiEE12emplace_backIJiEEEvDpOT_", scope: !225, file: !33, line: 962, type: !1898, isLocal: false, isDefinition: false, scopeLine: 962, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true, templateParams: !1901)
!1901 = !{!1902}
!1902 = !DITemplateValueParameter(tag: DW_TAG_GNU_template_parameter_pack, name: "_Args", value: !1903)
!1903 = !{!27}
!1904 = !{!1895, !1905}
!1905 = !DILocalVariable(name: "__args", arg: 2, scope: !1896, file: !33, line: 962, type: !588)
!1906 = !DILocation(line: 0, scope: !1896, inlinedAt: !1907)
!1907 = distinct !DILocation(line: 954, column: 9, scope: !1890, inlinedAt: !1894)
!1908 = !DILocalVariable(name: "this", arg: 1, scope: !1909, type: !1561, flags: DIFlagArtificial | DIFlagObjectPointer)
!1909 = distinct !DISubprogram(name: "_M_realloc_insert<int>", linkageName: "_ZNSt6vectorIiSaIiEE17_M_realloc_insertIJiEEEvN9__gnu_cxx17__normal_iteratorIPiS1_EEDpOT_", scope: !225, file: !1897, line: 395, type: !1910, isLocal: false, isDefinition: true, scopeLine: 402, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !1901, declaration: !1912, variables: !1913)
!1910 = !DISubroutineType(types: !1911)
!1911 = !{null, !231, !224, !588}
!1912 = !DISubprogram(name: "_M_realloc_insert<int>", linkageName: "_ZNSt6vectorIiSaIiEE17_M_realloc_insertIJiEEEvN9__gnu_cxx17__normal_iteratorIPiS1_EEDpOT_", scope: !225, file: !33, line: 1478, type: !1910, isLocal: false, isDefinition: false, scopeLine: 1478, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true, templateParams: !1901)
!1913 = !{!1908, !1914, !1915, !1916, !1918, !1919, !1920}
!1914 = !DILocalVariable(name: "__position", arg: 2, scope: !1909, file: !33, line: 1478, type: !224)
!1915 = !DILocalVariable(name: "__args", arg: 3, scope: !1909, file: !33, line: 1478, type: !588)
!1916 = !DILocalVariable(name: "__len", scope: !1909, file: !1897, line: 403, type: !1917)
!1917 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !221)
!1918 = !DILocalVariable(name: "__elems_before", scope: !1909, file: !1897, line: 405, type: !1917)
!1919 = !DILocalVariable(name: "__new_start", scope: !1909, file: !1897, line: 406, type: !468)
!1920 = !DILocalVariable(name: "__new_finish", scope: !1909, file: !1897, line: 407, type: !468)
!1921 = !DILocation(line: 0, scope: !1909, inlinedAt: !1922)
!1922 = distinct !DILocation(line: 105, column: 4, scope: !1923, inlinedAt: !1907)
!1923 = distinct !DILexicalBlock(scope: !1896, file: !1897, line: 98, column: 6)
!1924 = !DILocalVariable(name: "this", arg: 1, scope: !1925, type: !1584, flags: DIFlagArtificial | DIFlagObjectPointer)
!1925 = distinct !DISubprogram(name: "_M_check_len", linkageName: "_ZNKSt6vectorIiSaIiEE12_M_check_lenEmPKc", scope: !225, file: !33, line: 1497, type: !459, isLocal: false, isDefinition: true, scopeLine: 1498, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !458, variables: !1926)
!1926 = !{!1924, !1927, !1928, !1929}
!1927 = !DILocalVariable(name: "__n", arg: 2, scope: !1925, file: !33, line: 1497, type: !221)
!1928 = !DILocalVariable(name: "__s", arg: 3, scope: !1925, file: !33, line: 1497, type: !462)
!1929 = !DILocalVariable(name: "__len", scope: !1925, file: !33, line: 1502, type: !1917)
!1930 = !DILocation(line: 0, scope: !1925, inlinedAt: !1931)
!1931 = distinct !DILocation(line: 404, column: 2, scope: !1909, inlinedAt: !1922)
!1932 = !DILocation(line: 1497, column: 30, scope: !1925, inlinedAt: !1931)
!1933 = !DILocation(line: 0, scope: !1582, inlinedAt: !1934)
!1934 = distinct !DILocation(line: 1499, column: 19, scope: !1935, inlinedAt: !1931)
!1935 = distinct !DILexicalBlock(scope: !1925, file: !33, line: 1499, column: 6)
!1936 = !DILocation(line: 0, scope: !1582, inlinedAt: !1937)
!1937 = distinct !DILocation(line: 1502, column: 26, scope: !1925, inlinedAt: !1931)
!1938 = !DILocation(line: 0, scope: !1582, inlinedAt: !1939)
!1939 = distinct !DILocation(line: 1502, column: 44, scope: !1925, inlinedAt: !1931)
!1940 = !DILocation(line: 1502, column: 18, scope: !1925, inlinedAt: !1931)
!1941 = !DILocation(line: 0, scope: !1582, inlinedAt: !1942)
!1942 = distinct !DILocation(line: 1503, column: 18, scope: !1925, inlinedAt: !1931)
!1943 = !DILocation(line: 403, column: 23, scope: !1909, inlinedAt: !1922)
!1944 = !DILocation(line: 1478, column: 29, scope: !1909, inlinedAt: !1922)
!1945 = !DILocation(line: 405, column: 23, scope: !1909, inlinedAt: !1922)
!1946 = !DILocation(line: 0, scope: !1785, inlinedAt: !1947)
!1947 = distinct !DILocation(line: 406, column: 33, scope: !1909, inlinedAt: !1922)
!1948 = !DILocation(line: 169, column: 26, scope: !1785, inlinedAt: !1947)
!1949 = !DILocation(line: 435, column: 47, scope: !1792, inlinedAt: !1950)
!1950 = distinct !DILocation(line: 172, column: 20, scope: !1785, inlinedAt: !1947)
!1951 = !DILocation(line: 99, column: 26, scope: !1798, inlinedAt: !1952)
!1952 = distinct !DILocation(line: 436, column: 20, scope: !1792, inlinedAt: !1950)
!1953 = !DILocation(line: 99, column: 43, scope: !1798, inlinedAt: !1952)
!1954 = !DILocation(line: 111, column: 27, scope: !1798, inlinedAt: !1952)
!1955 = !DILocation(line: 406, column: 15, scope: !1909, inlinedAt: !1922)
!1956 = !DILocation(line: 407, column: 15, scope: !1909, inlinedAt: !1922)
!1957 = !DILocation(line: 416, column: 20, scope: !1958, inlinedAt: !1922)
!1958 = distinct !DILexicalBlock(scope: !1909, file: !1897, line: 409, column: 2)
!1959 = !DILocalVariable(name: "__p", arg: 2, scope: !1960, file: !49, line: 474, type: !55)
!1960 = distinct !DISubprogram(name: "construct<int, int>", linkageName: "_ZNSt16allocator_traitsISaIiEE9constructIiJiEEEvRS0_PT_DpOT0_", scope: !48, file: !49, line: 474, type: !1961, isLocal: false, isDefinition: true, scopeLine: 475, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !1964, declaration: !1963, variables: !1965)
!1961 = !DISubroutineType(types: !1962)
!1962 = !{null, !56, !55, !588}
!1963 = !DISubprogram(name: "construct<int, int>", linkageName: "_ZNSt16allocator_traitsISaIiEE9constructIiJiEEEvRS0_PT_DpOT0_", scope: !48, file: !49, line: 474, type: !1961, isLocal: false, isDefinition: false, scopeLine: 474, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true, templateParams: !1964)
!1964 = !{!1653, !1902}
!1965 = !{!1966, !1959, !1967}
!1966 = !DILocalVariable(name: "__a", arg: 1, scope: !1960, file: !49, line: 474, type: !56)
!1967 = !DILocalVariable(name: "__args", arg: 3, scope: !1960, file: !49, line: 474, type: !588)
!1968 = !DILocation(line: 474, column: 38, scope: !1960, inlinedAt: !1969)
!1969 = distinct !DILocation(line: 415, column: 4, scope: !1958, inlinedAt: !1922)
!1970 = !DILocalVariable(name: "__p", arg: 2, scope: !1971, file: !65, line: 135, type: !55)
!1971 = distinct !DISubprogram(name: "construct<int, int>", linkageName: "_ZN9__gnu_cxx13new_allocatorIiE9constructIiJiEEEvPT_DpOT0_", scope: !64, file: !65, line: 135, type: !1972, isLocal: false, isDefinition: true, scopeLine: 136, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !1964, declaration: !1974, variables: !1975)
!1972 = !DISubroutineType(types: !1973)
!1973 = !{null, !70, !55, !588}
!1974 = !DISubprogram(name: "construct<int, int>", linkageName: "_ZN9__gnu_cxx13new_allocatorIiE9constructIiJiEEEvPT_DpOT0_", scope: !64, file: !65, line: 135, type: !1972, isLocal: false, isDefinition: false, scopeLine: 135, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true, templateParams: !1964)
!1975 = !{!1976, !1970, !1977}
!1976 = !DILocalVariable(name: "this", arg: 1, scope: !1971, type: !1671, flags: DIFlagArtificial | DIFlagObjectPointer)
!1977 = !DILocalVariable(name: "__args", arg: 3, scope: !1971, file: !65, line: 135, type: !588)
!1978 = !DILocation(line: 135, column: 17, scope: !1971, inlinedAt: !1979)
!1979 = distinct !DILocation(line: 475, column: 8, scope: !1960, inlinedAt: !1969)
!1980 = !DILocation(line: 136, column: 4, scope: !1971, inlinedAt: !1979)
!1981 = !DILocalVariable(name: "__first", arg: 1, scope: !1982, file: !1816, line: 305, type: !55)
!1982 = distinct !DISubprogram(name: "__uninitialized_move_if_noexcept_a<int *, int *, std::allocator<int> >", linkageName: "_ZSt34__uninitialized_move_if_noexcept_aIPiS0_SaIiEET0_T_S3_S2_RT1_", scope: !2, file: !1816, line: 305, type: !1983, isLocal: false, isDefinition: true, scopeLine: 309, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !1989, variables: !1985)
!1983 = !DISubroutineType(types: !1984)
!1984 = !{!55, !55, !55, !55, !144}
!1985 = !{!1981, !1986, !1987, !1988}
!1986 = !DILocalVariable(name: "__last", arg: 2, scope: !1982, file: !1816, line: 306, type: !55)
!1987 = !DILocalVariable(name: "__result", arg: 3, scope: !1982, file: !1816, line: 307, type: !55)
!1988 = !DILocalVariable(name: "__alloc", arg: 4, scope: !1982, file: !1816, line: 308, type: !144)
!1989 = !{!1990, !1823, !1991}
!1990 = !DITemplateTypeParameter(name: "_InputIterator", type: !55)
!1991 = !DITemplateTypeParameter(name: "_Allocator", type: !58)
!1992 = !DILocation(line: 305, column: 55, scope: !1982, inlinedAt: !1993)
!1993 = distinct !DILocation(line: 425, column: 8, scope: !1958, inlinedAt: !1922)
!1994 = !DILocation(line: 306, column: 27, scope: !1982, inlinedAt: !1993)
!1995 = !DILocation(line: 307, column: 29, scope: !1982, inlinedAt: !1993)
!1996 = !DILocalVariable(name: "__first", arg: 1, scope: !1997, file: !1816, line: 287, type: !563)
!1997 = distinct !DISubprogram(name: "__uninitialized_copy_a<std::move_iterator<int *>, int *, int>", linkageName: "_ZSt22__uninitialized_copy_aISt13move_iteratorIPiES1_iET0_T_S4_S3_RSaIT1_E", scope: !2, file: !1816, line: 287, type: !1998, isLocal: false, isDefinition: true, scopeLine: 289, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2004, variables: !2000)
!1998 = !DISubroutineType(types: !1999)
!1999 = !{!55, !563, !563, !55, !144}
!2000 = !{!1996, !2001, !2002, !2003}
!2001 = !DILocalVariable(name: "__last", arg: 2, scope: !1997, file: !1816, line: 287, type: !563)
!2002 = !DILocalVariable(name: "__result", arg: 3, scope: !1997, file: !1816, line: 288, type: !55)
!2003 = !DILocalVariable(arg: 4, scope: !1997, file: !1816, line: 288, type: !144)
!2004 = !{!2005, !1823, !108}
!2005 = !DITemplateTypeParameter(name: "_InputIterator", type: !563)
!2006 = !DILocation(line: 287, column: 43, scope: !1997, inlinedAt: !2007)
!2007 = distinct !DILocation(line: 310, column: 14, scope: !1982, inlinedAt: !1993)
!2008 = !DILocation(line: 287, column: 67, scope: !1997, inlinedAt: !2007)
!2009 = !DILocation(line: 288, column: 24, scope: !1997, inlinedAt: !2007)
!2010 = !DILocalVariable(name: "__first", arg: 1, scope: !2011, file: !1816, line: 115, type: !563)
!2011 = distinct !DISubprogram(name: "uninitialized_copy<std::move_iterator<int *>, int *>", linkageName: "_ZSt18uninitialized_copyISt13move_iteratorIPiES1_ET0_T_S4_S3_", scope: !2, file: !1816, line: 115, type: !2012, isLocal: false, isDefinition: true, scopeLine: 117, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2018, variables: !2014)
!2012 = !DISubroutineType(types: !2013)
!2013 = !{!55, !563, !563, !55}
!2014 = !{!2010, !2015, !2016, !2017}
!2015 = !DILocalVariable(name: "__last", arg: 2, scope: !2011, file: !1816, line: 115, type: !563)
!2016 = !DILocalVariable(name: "__result", arg: 3, scope: !2011, file: !1816, line: 116, type: !55)
!2017 = !DILocalVariable(name: "__assignable", scope: !2011, file: !1816, line: 128, type: !483)
!2018 = !{!2005, !1823}
!2019 = !DILocation(line: 115, column: 39, scope: !2011, inlinedAt: !2020)
!2020 = distinct !DILocation(line: 289, column: 14, scope: !1997, inlinedAt: !2007)
!2021 = !DILocation(line: 115, column: 63, scope: !2011, inlinedAt: !2020)
!2022 = !DILocation(line: 116, column: 27, scope: !2011, inlinedAt: !2020)
!2023 = !DILocation(line: 128, column: 18, scope: !2011, inlinedAt: !2020)
!2024 = !DILocalVariable(name: "__first", arg: 1, scope: !2025, file: !1816, line: 99, type: !563)
!2025 = distinct !DISubprogram(name: "__uninit_copy<std::move_iterator<int *>, int *>", linkageName: "_ZNSt20__uninitialized_copyILb1EE13__uninit_copyISt13move_iteratorIPiES3_EET0_T_S6_S5_", scope: !2026, file: !1816, line: 99, type: !2012, isLocal: false, isDefinition: true, scopeLine: 101, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2018, declaration: !2029, variables: !2030)
!2026 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__uninitialized_copy<true>", scope: !2, file: !1816, line: 95, size: 8, elements: !25, templateParams: !2027, identifier: "_ZTSSt20__uninitialized_copyILb1EE")
!2027 = !{!2028}
!2028 = !DITemplateValueParameter(name: "_TrivialValueTypes", type: !13, value: i8 1)
!2029 = !DISubprogram(name: "__uninit_copy<std::move_iterator<int *>, int *>", linkageName: "_ZNSt20__uninitialized_copyILb1EE13__uninit_copyISt13move_iteratorIPiES3_EET0_T_S6_S5_", scope: !2026, file: !1816, line: 99, type: !2012, isLocal: false, isDefinition: false, scopeLine: 99, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true, templateParams: !2018)
!2030 = !{!2024, !2031, !2032}
!2031 = !DILocalVariable(name: "__last", arg: 2, scope: !2025, file: !1816, line: 99, type: !563)
!2032 = !DILocalVariable(name: "__result", arg: 3, scope: !2025, file: !1816, line: 100, type: !55)
!2033 = !DILocation(line: 99, column: 38, scope: !2025, inlinedAt: !2034)
!2034 = distinct !DILocation(line: 131, column: 14, scope: !2011, inlinedAt: !2020)
!2035 = !DILocation(line: 99, column: 62, scope: !2025, inlinedAt: !2034)
!2036 = !DILocation(line: 100, column: 26, scope: !2025, inlinedAt: !2034)
!2037 = !DILocalVariable(name: "__first", arg: 1, scope: !2038, file: !1853, line: 446, type: !563)
!2038 = distinct !DISubprogram(name: "copy<std::move_iterator<int *>, int *>", linkageName: "_ZSt4copyISt13move_iteratorIPiES1_ET0_T_S4_S3_", scope: !2, file: !1853, line: 446, type: !2012, isLocal: false, isDefinition: true, scopeLine: 447, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2042, variables: !2039)
!2039 = !{!2037, !2040, !2041}
!2040 = !DILocalVariable(name: "__last", arg: 2, scope: !2038, file: !1853, line: 446, type: !563)
!2041 = !DILocalVariable(name: "__result", arg: 3, scope: !2038, file: !1853, line: 446, type: !55)
!2042 = !{!2043, !1860}
!2043 = !DITemplateTypeParameter(name: "_II", type: !563)
!2044 = !DILocation(line: 446, column: 14, scope: !2038, inlinedAt: !2045)
!2045 = distinct !DILocation(line: 101, column: 18, scope: !2025, inlinedAt: !2034)
!2046 = !DILocation(line: 446, column: 27, scope: !2038, inlinedAt: !2045)
!2047 = !DILocation(line: 446, column: 39, scope: !2038, inlinedAt: !2045)
!2048 = !DILocalVariable(name: "__first", arg: 1, scope: !2049, file: !1853, line: 420, type: !55)
!2049 = distinct !DISubprogram(name: "__copy_move_a2<true, int *, int *>", linkageName: "_ZSt14__copy_move_a2ILb1EPiS0_ET1_T0_S2_S1_", scope: !2, file: !1853, line: 420, type: !2050, isLocal: false, isDefinition: true, scopeLine: 421, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2055, variables: !2052)
!2050 = !DISubroutineType(types: !2051)
!2051 = !{!55, !55, !55, !55}
!2052 = !{!2048, !2053, !2054}
!2053 = !DILocalVariable(name: "__last", arg: 2, scope: !2049, file: !1853, line: 420, type: !55)
!2054 = !DILocalVariable(name: "__result", arg: 3, scope: !2049, file: !1853, line: 420, type: !55)
!2055 = !{!2056, !2057, !1860}
!2056 = !DITemplateValueParameter(name: "_IsMove", type: !13, value: i8 1)
!2057 = !DITemplateTypeParameter(name: "_II", type: !55)
!2058 = !DILocation(line: 420, column: 24, scope: !2049, inlinedAt: !2059)
!2059 = distinct !DILocation(line: 454, column: 15, scope: !2038, inlinedAt: !2045)
!2060 = !DILocation(line: 420, column: 37, scope: !2049, inlinedAt: !2059)
!2061 = !DILocation(line: 420, column: 49, scope: !2049, inlinedAt: !2059)
!2062 = !DILocalVariable(name: "__first", arg: 1, scope: !2063, file: !1853, line: 375, type: !55)
!2063 = distinct !DISubprogram(name: "__copy_move_a<true, int *, int *>", linkageName: "_ZSt13__copy_move_aILb1EPiS0_ET1_T0_S2_S1_", scope: !2, file: !1853, line: 375, type: !2050, isLocal: false, isDefinition: true, scopeLine: 376, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2055, variables: !2064)
!2064 = !{!2062, !2065, !2066, !2067}
!2065 = !DILocalVariable(name: "__last", arg: 2, scope: !2063, file: !1853, line: 375, type: !55)
!2066 = !DILocalVariable(name: "__result", arg: 3, scope: !2063, file: !1853, line: 375, type: !55)
!2067 = !DILocalVariable(name: "__simple", scope: !2063, file: !1853, line: 380, type: !483)
!2068 = !DILocation(line: 375, column: 23, scope: !2063, inlinedAt: !2069)
!2069 = distinct !DILocation(line: 422, column: 18, scope: !2049, inlinedAt: !2059)
!2070 = !DILocation(line: 375, column: 36, scope: !2063, inlinedAt: !2069)
!2071 = !DILocation(line: 375, column: 48, scope: !2063, inlinedAt: !2069)
!2072 = !DILocation(line: 380, column: 18, scope: !2063, inlinedAt: !2069)
!2073 = !DILocalVariable(name: "__first", arg: 1, scope: !2074, file: !1853, line: 357, type: !88)
!2074 = distinct !DISubprogram(name: "__copy_m<int>", linkageName: "_ZNSt11__copy_moveILb1ELb1ESt26random_access_iterator_tagE8__copy_mIiEEPT_PKS3_S6_S4_", scope: !2075, file: !1853, line: 357, type: !2088, isLocal: false, isDefinition: true, scopeLine: 358, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !107, declaration: !2090, variables: !2091)
!2075 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__copy_move<true, true, std::random_access_iterator_tag>", scope: !2, file: !1853, line: 353, size: 8, elements: !25, templateParams: !2076, identifier: "_ZTSSt11__copy_moveILb1ELb1ESt26random_access_iterator_tagE")
!2076 = !{!1872, !1872, !2077}
!2077 = !DITemplateTypeParameter(type: !2078)
!2078 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "random_access_iterator_tag", scope: !2, file: !309, line: 103, size: 8, elements: !2079, identifier: "_ZTSSt26random_access_iterator_tag")
!2079 = !{!2080}
!2080 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !2078, baseType: !2081)
!2081 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "bidirectional_iterator_tag", scope: !2, file: !309, line: 99, size: 8, elements: !2082, identifier: "_ZTSSt26bidirectional_iterator_tag")
!2082 = !{!2083}
!2083 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !2081, baseType: !2084)
!2084 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "forward_iterator_tag", scope: !2, file: !309, line: 95, size: 8, elements: !2085, identifier: "_ZTSSt20forward_iterator_tag")
!2085 = !{!2086}
!2086 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !2084, baseType: !2087)
!2087 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "input_iterator_tag", scope: !2, file: !309, line: 89, size: 8, elements: !25, identifier: "_ZTSSt18input_iterator_tag")
!2088 = !DISubroutineType(types: !2089)
!2089 = !{!55, !88, !88, !55}
!2090 = !DISubprogram(name: "__copy_m<int>", linkageName: "_ZNSt11__copy_moveILb1ELb1ESt26random_access_iterator_tagE8__copy_mIiEEPT_PKS3_S6_S4_", scope: !2075, file: !1853, line: 357, type: !2088, isLocal: false, isDefinition: false, scopeLine: 357, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true, templateParams: !107)
!2091 = !{!2073, !2092, !2093, !2094}
!2092 = !DILocalVariable(name: "__last", arg: 2, scope: !2074, file: !1853, line: 357, type: !88)
!2093 = !DILocalVariable(name: "__result", arg: 3, scope: !2074, file: !1853, line: 357, type: !55)
!2094 = !DILocalVariable(name: "_Num", scope: !2074, file: !1853, line: 366, type: !2095)
!2095 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !334)
!2096 = !DILocation(line: 357, column: 29, scope: !2074, inlinedAt: !2097)
!2097 = distinct !DILocation(line: 385, column: 14, scope: !2063, inlinedAt: !2069)
!2098 = !DILocation(line: 357, column: 49, scope: !2074, inlinedAt: !2097)
!2099 = !DILocation(line: 357, column: 62, scope: !2074, inlinedAt: !2097)
!2100 = !DILocation(line: 366, column: 20, scope: !2074, inlinedAt: !2097)
!2101 = !DILocation(line: 368, column: 6, scope: !2102, inlinedAt: !2097)
!2102 = distinct !DILexicalBlock(scope: !2074, file: !1853, line: 367, column: 8)
!2103 = !DILocation(line: 305, column: 55, scope: !1982, inlinedAt: !2104)
!2104 = distinct !DILocation(line: 432, column: 8, scope: !1958, inlinedAt: !1922)
!2105 = !DILocation(line: 306, column: 27, scope: !1982, inlinedAt: !2104)
!2106 = !DILocation(line: 307, column: 29, scope: !1982, inlinedAt: !2104)
!2107 = !DILocation(line: 287, column: 43, scope: !1997, inlinedAt: !2108)
!2108 = distinct !DILocation(line: 310, column: 14, scope: !1982, inlinedAt: !2104)
!2109 = !DILocation(line: 287, column: 67, scope: !1997, inlinedAt: !2108)
!2110 = !DILocation(line: 288, column: 24, scope: !1997, inlinedAt: !2108)
!2111 = !DILocation(line: 115, column: 39, scope: !2011, inlinedAt: !2112)
!2112 = distinct !DILocation(line: 289, column: 14, scope: !1997, inlinedAt: !2108)
!2113 = !DILocation(line: 115, column: 63, scope: !2011, inlinedAt: !2112)
!2114 = !DILocation(line: 116, column: 27, scope: !2011, inlinedAt: !2112)
!2115 = !DILocation(line: 128, column: 18, scope: !2011, inlinedAt: !2112)
!2116 = !DILocation(line: 99, column: 38, scope: !2025, inlinedAt: !2117)
!2117 = distinct !DILocation(line: 131, column: 14, scope: !2011, inlinedAt: !2112)
!2118 = !DILocation(line: 99, column: 62, scope: !2025, inlinedAt: !2117)
!2119 = !DILocation(line: 100, column: 26, scope: !2025, inlinedAt: !2117)
!2120 = !DILocation(line: 446, column: 14, scope: !2038, inlinedAt: !2121)
!2121 = distinct !DILocation(line: 101, column: 18, scope: !2025, inlinedAt: !2117)
!2122 = !DILocation(line: 446, column: 27, scope: !2038, inlinedAt: !2121)
!2123 = !DILocation(line: 446, column: 39, scope: !2038, inlinedAt: !2121)
!2124 = !DILocation(line: 420, column: 24, scope: !2049, inlinedAt: !2125)
!2125 = distinct !DILocation(line: 454, column: 15, scope: !2038, inlinedAt: !2121)
!2126 = !DILocation(line: 420, column: 37, scope: !2049, inlinedAt: !2125)
!2127 = !DILocation(line: 420, column: 49, scope: !2049, inlinedAt: !2125)
!2128 = !DILocation(line: 375, column: 23, scope: !2063, inlinedAt: !2129)
!2129 = distinct !DILocation(line: 422, column: 18, scope: !2049, inlinedAt: !2125)
!2130 = !DILocation(line: 375, column: 36, scope: !2063, inlinedAt: !2129)
!2131 = !DILocation(line: 375, column: 48, scope: !2063, inlinedAt: !2129)
!2132 = !DILocation(line: 380, column: 18, scope: !2063, inlinedAt: !2129)
!2133 = !DILocation(line: 357, column: 29, scope: !2074, inlinedAt: !2134)
!2134 = distinct !DILocation(line: 385, column: 14, scope: !2063, inlinedAt: !2129)
!2135 = !DILocation(line: 357, column: 49, scope: !2074, inlinedAt: !2134)
!2136 = !DILocation(line: 357, column: 62, scope: !2074, inlinedAt: !2134)
!2137 = !DILocation(line: 366, column: 20, scope: !2074, inlinedAt: !2134)
!2138 = !DILocation(line: 0, scope: !1697, inlinedAt: !2139)
!2139 = distinct !DILocation(line: 448, column: 7, scope: !1909, inlinedAt: !1922)
!2140 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2139)
!2141 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2142)
!2142 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2139)
!2143 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2144)
!2144 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2142)
!2145 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2144)
!2146 = !DILocation(line: 451, column: 30, scope: !1909, inlinedAt: !1922)
!2147 = !DILocation(line: 0, scope: !1890, inlinedAt: !2148)
!2148 = distinct !DILocation(line: 26, column: 5, scope: !1747)
!2149 = !DILocation(line: 0, scope: !1896, inlinedAt: !2150)
!2150 = distinct !DILocation(line: 954, column: 9, scope: !1890, inlinedAt: !2148)
!2151 = !DILocation(line: 429, column: 4, scope: !1958, inlinedAt: !1922)
!2152 = !DILocation(line: 474, column: 38, scope: !1960, inlinedAt: !2153)
!2153 = distinct !DILocation(line: 100, column: 6, scope: !2154, inlinedAt: !2150)
!2154 = distinct !DILexicalBlock(scope: !1923, file: !1897, line: 99, column: 4)
!2155 = !DILocation(line: 135, column: 17, scope: !1971, inlinedAt: !2156)
!2156 = distinct !DILocation(line: 475, column: 8, scope: !1960, inlinedAt: !2153)
!2157 = !DILocation(line: 136, column: 4, scope: !1971, inlinedAt: !2156)
!2158 = !DILocation(line: 0, scope: !1752, inlinedAt: !2159)
!2159 = distinct !DILocation(line: 27, column: 15, scope: !1747)
!2160 = !DILocation(line: 283, column: 24, scope: !1752, inlinedAt: !2159)
!2161 = !DILocation(line: 283, column: 51, scope: !1752, inlinedAt: !2159)
!2162 = !DILocation(line: 0, scope: !1761, inlinedAt: !2163)
!2163 = distinct !DILocation(line: 284, column: 9, scope: !1752, inlinedAt: !2159)
!2164 = !DILocation(line: 136, column: 27, scope: !1761, inlinedAt: !2163)
!2165 = !DILocation(line: 136, column: 54, scope: !1761, inlinedAt: !2163)
!2166 = !DILocation(line: 0, scope: !1770, inlinedAt: !2167)
!2167 = distinct !DILocation(line: 137, column: 9, scope: !1761, inlinedAt: !2163)
!2168 = !DILocation(line: 92, column: 37, scope: !1770, inlinedAt: !2167)
!2169 = !DILocation(line: 0, scope: !1777, inlinedAt: !2170)
!2170 = distinct !DILocation(line: 138, column: 9, scope: !1782, inlinedAt: !2163)
!2171 = !DILocation(line: 185, column: 32, scope: !1777, inlinedAt: !2170)
!2172 = !DILocation(line: 0, scope: !1785, inlinedAt: !2173)
!2173 = distinct !DILocation(line: 187, column: 33, scope: !1777, inlinedAt: !2170)
!2174 = !DILocation(line: 169, column: 26, scope: !1785, inlinedAt: !2173)
!2175 = !DILocation(line: 435, column: 47, scope: !1792, inlinedAt: !2176)
!2176 = distinct !DILocation(line: 172, column: 20, scope: !1785, inlinedAt: !2173)
!2177 = !DILocation(line: 99, column: 26, scope: !1798, inlinedAt: !2178)
!2178 = distinct !DILocation(line: 436, column: 20, scope: !1792, inlinedAt: !2176)
!2179 = !DILocation(line: 99, column: 43, scope: !1798, inlinedAt: !2178)
!2180 = !DILocation(line: 111, column: 27, scope: !1798, inlinedAt: !2178)
!2181 = !DILocation(line: 0, scope: !1807, inlinedAt: !2182)
!2182 = distinct !DILocation(line: 285, column: 9, scope: !1812, inlinedAt: !2159)
!2183 = !DILocation(line: 1344, column: 39, scope: !1807, inlinedAt: !2182)
!2184 = !DILocation(line: 643, column: 50, scope: !1815, inlinedAt: !2185)
!2185 = distinct !DILocation(line: 1347, column: 4, scope: !1807, inlinedAt: !2182)
!2186 = !DILocation(line: 643, column: 65, scope: !1815, inlinedAt: !2185)
!2187 = !DILocation(line: 574, column: 48, scope: !1829, inlinedAt: !2188)
!2188 = distinct !DILocation(line: 645, column: 14, scope: !1815, inlinedAt: !2185)
!2189 = !DILocation(line: 574, column: 63, scope: !1829, inlinedAt: !2188)
!2190 = !DILocation(line: 579, column: 18, scope: !1829, inlinedAt: !2188)
!2191 = !DILocation(line: 543, column: 45, scope: !1841, inlinedAt: !2192)
!2192 = distinct !DILocation(line: 581, column: 14, scope: !1829, inlinedAt: !2188)
!2193 = !DILocation(line: 543, column: 60, scope: !1841, inlinedAt: !2192)
!2194 = !DILocation(line: 784, column: 16, scope: !1852, inlinedAt: !2195)
!2195 = distinct !DILocation(line: 548, column: 11, scope: !1841, inlinedAt: !2192)
!2196 = !DILocation(line: 784, column: 31, scope: !1852, inlinedAt: !2195)
!2197 = !DILocation(line: 749, column: 32, scope: !1865, inlinedAt: !2198)
!2198 = distinct !DILocation(line: 789, column: 18, scope: !1852, inlinedAt: !2195)
!2199 = !DILocation(line: 749, column: 47, scope: !1865, inlinedAt: !2198)
!2200 = !DILocation(line: 751, column: 17, scope: !1865, inlinedAt: !2198)
!2201 = !DILocation(line: 752, column: 32, scope: !1879, inlinedAt: !2198)
!2202 = !DILocation(line: 754, column: 11, scope: !1888, inlinedAt: !2198)
!2203 = !DILocation(line: 0, scope: !1890, inlinedAt: !2204)
!2204 = distinct !DILocation(line: 28, column: 5, scope: !1747)
!2205 = !DILocation(line: 0, scope: !1896, inlinedAt: !2206)
!2206 = distinct !DILocation(line: 954, column: 9, scope: !1890, inlinedAt: !2204)
!2207 = !DILocation(line: 0, scope: !1909, inlinedAt: !2208)
!2208 = distinct !DILocation(line: 105, column: 4, scope: !1923, inlinedAt: !2206)
!2209 = !DILocation(line: 0, scope: !1925, inlinedAt: !2210)
!2210 = distinct !DILocation(line: 404, column: 2, scope: !1909, inlinedAt: !2208)
!2211 = !DILocation(line: 1497, column: 30, scope: !1925, inlinedAt: !2210)
!2212 = !DILocation(line: 0, scope: !1582, inlinedAt: !2213)
!2213 = distinct !DILocation(line: 1499, column: 19, scope: !1935, inlinedAt: !2210)
!2214 = !DILocation(line: 0, scope: !1582, inlinedAt: !2215)
!2215 = distinct !DILocation(line: 1502, column: 26, scope: !1925, inlinedAt: !2210)
!2216 = !DILocation(line: 0, scope: !1582, inlinedAt: !2217)
!2217 = distinct !DILocation(line: 1502, column: 44, scope: !1925, inlinedAt: !2210)
!2218 = !DILocation(line: 1502, column: 18, scope: !1925, inlinedAt: !2210)
!2219 = !DILocation(line: 0, scope: !1582, inlinedAt: !2220)
!2220 = distinct !DILocation(line: 1503, column: 18, scope: !1925, inlinedAt: !2210)
!2221 = !DILocation(line: 403, column: 23, scope: !1909, inlinedAt: !2208)
!2222 = !DILocation(line: 1478, column: 29, scope: !1909, inlinedAt: !2208)
!2223 = !DILocation(line: 405, column: 23, scope: !1909, inlinedAt: !2208)
!2224 = !DILocation(line: 0, scope: !1785, inlinedAt: !2225)
!2225 = distinct !DILocation(line: 406, column: 33, scope: !1909, inlinedAt: !2208)
!2226 = !DILocation(line: 169, column: 26, scope: !1785, inlinedAt: !2225)
!2227 = !DILocation(line: 435, column: 47, scope: !1792, inlinedAt: !2228)
!2228 = distinct !DILocation(line: 172, column: 20, scope: !1785, inlinedAt: !2225)
!2229 = !DILocation(line: 99, column: 26, scope: !1798, inlinedAt: !2230)
!2230 = distinct !DILocation(line: 436, column: 20, scope: !1792, inlinedAt: !2228)
!2231 = !DILocation(line: 99, column: 43, scope: !1798, inlinedAt: !2230)
!2232 = !DILocation(line: 111, column: 27, scope: !1798, inlinedAt: !2230)
!2233 = !DILocation(line: 406, column: 15, scope: !1909, inlinedAt: !2208)
!2234 = !DILocation(line: 407, column: 15, scope: !1909, inlinedAt: !2208)
!2235 = !DILocation(line: 416, column: 20, scope: !1958, inlinedAt: !2208)
!2236 = !DILocation(line: 474, column: 38, scope: !1960, inlinedAt: !2237)
!2237 = distinct !DILocation(line: 415, column: 4, scope: !1958, inlinedAt: !2208)
!2238 = !DILocation(line: 135, column: 17, scope: !1971, inlinedAt: !2239)
!2239 = distinct !DILocation(line: 475, column: 8, scope: !1960, inlinedAt: !2237)
!2240 = !DILocation(line: 136, column: 4, scope: !1971, inlinedAt: !2239)
!2241 = !DILocation(line: 305, column: 55, scope: !1982, inlinedAt: !2242)
!2242 = distinct !DILocation(line: 425, column: 8, scope: !1958, inlinedAt: !2208)
!2243 = !DILocation(line: 307, column: 29, scope: !1982, inlinedAt: !2242)
!2244 = !DILocation(line: 287, column: 43, scope: !1997, inlinedAt: !2245)
!2245 = distinct !DILocation(line: 310, column: 14, scope: !1982, inlinedAt: !2242)
!2246 = !DILocation(line: 288, column: 24, scope: !1997, inlinedAt: !2245)
!2247 = !DILocation(line: 115, column: 39, scope: !2011, inlinedAt: !2248)
!2248 = distinct !DILocation(line: 289, column: 14, scope: !1997, inlinedAt: !2245)
!2249 = !DILocation(line: 116, column: 27, scope: !2011, inlinedAt: !2248)
!2250 = !DILocation(line: 128, column: 18, scope: !2011, inlinedAt: !2248)
!2251 = !DILocation(line: 99, column: 38, scope: !2025, inlinedAt: !2252)
!2252 = distinct !DILocation(line: 131, column: 14, scope: !2011, inlinedAt: !2248)
!2253 = !DILocation(line: 100, column: 26, scope: !2025, inlinedAt: !2252)
!2254 = !DILocation(line: 446, column: 14, scope: !2038, inlinedAt: !2255)
!2255 = distinct !DILocation(line: 101, column: 18, scope: !2025, inlinedAt: !2252)
!2256 = !DILocation(line: 446, column: 39, scope: !2038, inlinedAt: !2255)
!2257 = !DILocation(line: 420, column: 24, scope: !2049, inlinedAt: !2258)
!2258 = distinct !DILocation(line: 454, column: 15, scope: !2038, inlinedAt: !2255)
!2259 = !DILocation(line: 420, column: 49, scope: !2049, inlinedAt: !2258)
!2260 = !DILocation(line: 375, column: 23, scope: !2063, inlinedAt: !2261)
!2261 = distinct !DILocation(line: 422, column: 18, scope: !2049, inlinedAt: !2258)
!2262 = !DILocation(line: 375, column: 48, scope: !2063, inlinedAt: !2261)
!2263 = !DILocation(line: 380, column: 18, scope: !2063, inlinedAt: !2261)
!2264 = !DILocation(line: 357, column: 29, scope: !2074, inlinedAt: !2265)
!2265 = distinct !DILocation(line: 385, column: 14, scope: !2063, inlinedAt: !2261)
!2266 = !DILocation(line: 357, column: 62, scope: !2074, inlinedAt: !2265)
!2267 = !DILocation(line: 366, column: 20, scope: !2074, inlinedAt: !2265)
!2268 = !DILocation(line: 368, column: 6, scope: !2102, inlinedAt: !2265)
!2269 = !DILocation(line: 306, column: 27, scope: !1982, inlinedAt: !2270)
!2270 = distinct !DILocation(line: 432, column: 8, scope: !1958, inlinedAt: !2208)
!2271 = !DILocation(line: 307, column: 29, scope: !1982, inlinedAt: !2270)
!2272 = !DILocation(line: 287, column: 67, scope: !1997, inlinedAt: !2273)
!2273 = distinct !DILocation(line: 310, column: 14, scope: !1982, inlinedAt: !2270)
!2274 = !DILocation(line: 288, column: 24, scope: !1997, inlinedAt: !2273)
!2275 = !DILocation(line: 115, column: 63, scope: !2011, inlinedAt: !2276)
!2276 = distinct !DILocation(line: 289, column: 14, scope: !1997, inlinedAt: !2273)
!2277 = !DILocation(line: 116, column: 27, scope: !2011, inlinedAt: !2276)
!2278 = !DILocation(line: 128, column: 18, scope: !2011, inlinedAt: !2276)
!2279 = !DILocation(line: 99, column: 62, scope: !2025, inlinedAt: !2280)
!2280 = distinct !DILocation(line: 131, column: 14, scope: !2011, inlinedAt: !2276)
!2281 = !DILocation(line: 100, column: 26, scope: !2025, inlinedAt: !2280)
!2282 = !DILocation(line: 446, column: 27, scope: !2038, inlinedAt: !2283)
!2283 = distinct !DILocation(line: 101, column: 18, scope: !2025, inlinedAt: !2280)
!2284 = !DILocation(line: 446, column: 39, scope: !2038, inlinedAt: !2283)
!2285 = !DILocation(line: 420, column: 37, scope: !2049, inlinedAt: !2286)
!2286 = distinct !DILocation(line: 454, column: 15, scope: !2038, inlinedAt: !2283)
!2287 = !DILocation(line: 420, column: 49, scope: !2049, inlinedAt: !2286)
!2288 = !DILocation(line: 375, column: 36, scope: !2063, inlinedAt: !2289)
!2289 = distinct !DILocation(line: 422, column: 18, scope: !2049, inlinedAt: !2286)
!2290 = !DILocation(line: 375, column: 48, scope: !2063, inlinedAt: !2289)
!2291 = !DILocation(line: 380, column: 18, scope: !2063, inlinedAt: !2289)
!2292 = !DILocation(line: 357, column: 49, scope: !2074, inlinedAt: !2293)
!2293 = distinct !DILocation(line: 385, column: 14, scope: !2063, inlinedAt: !2289)
!2294 = !DILocation(line: 357, column: 62, scope: !2074, inlinedAt: !2293)
!2295 = !DILocation(line: 366, column: 20, scope: !2074, inlinedAt: !2293)
!2296 = !DILocation(line: 0, scope: !1697, inlinedAt: !2297)
!2297 = distinct !DILocation(line: 448, column: 7, scope: !1909, inlinedAt: !2208)
!2298 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2297)
!2299 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2300)
!2300 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2297)
!2301 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2302)
!2302 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2300)
!2303 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2302)
!2304 = !DILocation(line: 451, column: 30, scope: !1909, inlinedAt: !2208)
!2305 = !DILocation(line: 0, scope: !1890, inlinedAt: !2306)
!2306 = distinct !DILocation(line: 29, column: 5, scope: !1747)
!2307 = !DILocation(line: 0, scope: !1896, inlinedAt: !2308)
!2308 = distinct !DILocation(line: 954, column: 9, scope: !1890, inlinedAt: !2306)
!2309 = !DILocation(line: 474, column: 38, scope: !1960, inlinedAt: !2310)
!2310 = distinct !DILocation(line: 100, column: 6, scope: !2154, inlinedAt: !2308)
!2311 = !DILocation(line: 135, column: 17, scope: !1971, inlinedAt: !2312)
!2312 = distinct !DILocation(line: 475, column: 8, scope: !1960, inlinedAt: !2310)
!2313 = !DILocation(line: 136, column: 4, scope: !1971, inlinedAt: !2312)
!2314 = !DILocalVariable(name: "this", arg: 1, scope: !2315, type: !1561, flags: DIFlagArtificial | DIFlagObjectPointer)
!2315 = distinct !DISubprogram(name: "vector", linkageName: "_ZNSt6vectorIiSaIiEEC2ERKS1_", scope: !225, file: !33, line: 326, type: !248, isLocal: false, isDefinition: true, scopeLine: 329, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !247, variables: !2316)
!2316 = !{!2314, !2317}
!2317 = !DILocalVariable(name: "__x", arg: 2, scope: !2315, file: !33, line: 326, type: !250)
!2318 = !DILocation(line: 0, scope: !2315, inlinedAt: !2319)
!2319 = distinct !DILocation(line: 30, column: 17, scope: !1747)
!2320 = !DILocation(line: 326, column: 28, scope: !2315, inlinedAt: !2319)
!2321 = !DILocation(line: 0, scope: !1582, inlinedAt: !2322)
!2322 = distinct !DILocation(line: 327, column: 19, scope: !2315, inlinedAt: !2319)
!2323 = !DILocation(line: 0, scope: !1761, inlinedAt: !2324)
!2324 = distinct !DILocation(line: 327, column: 9, scope: !2315, inlinedAt: !2319)
!2325 = !DILocation(line: 136, column: 27, scope: !1761, inlinedAt: !2324)
!2326 = !DILocation(line: 136, column: 54, scope: !1761, inlinedAt: !2324)
!2327 = !DILocation(line: 0, scope: !1770, inlinedAt: !2328)
!2328 = distinct !DILocation(line: 137, column: 9, scope: !1761, inlinedAt: !2324)
!2329 = !DILocation(line: 92, column: 37, scope: !1770, inlinedAt: !2328)
!2330 = !DILocation(line: 94, column: 4, scope: !1770, inlinedAt: !2328)
!2331 = !DILocation(line: 93, column: 37, scope: !1770, inlinedAt: !2328)
!2332 = !DILocation(line: 0, scope: !1777, inlinedAt: !2333)
!2333 = distinct !DILocation(line: 138, column: 9, scope: !1782, inlinedAt: !2324)
!2334 = !DILocation(line: 185, column: 32, scope: !1777, inlinedAt: !2333)
!2335 = !DILocation(line: 0, scope: !1785, inlinedAt: !2336)
!2336 = distinct !DILocation(line: 187, column: 33, scope: !1777, inlinedAt: !2333)
!2337 = !DILocation(line: 169, column: 26, scope: !1785, inlinedAt: !2336)
!2338 = !DILocation(line: 435, column: 47, scope: !1792, inlinedAt: !2339)
!2339 = distinct !DILocation(line: 172, column: 20, scope: !1785, inlinedAt: !2336)
!2340 = !DILocation(line: 99, column: 26, scope: !1798, inlinedAt: !2341)
!2341 = distinct !DILocation(line: 436, column: 20, scope: !1792, inlinedAt: !2339)
!2342 = !DILocation(line: 99, column: 43, scope: !1798, inlinedAt: !2341)
!2343 = !DILocation(line: 111, column: 27, scope: !1798, inlinedAt: !2341)
!2344 = !DILocation(line: 187, column: 25, scope: !1777, inlinedAt: !2333)
!2345 = !DILocation(line: 188, column: 16, scope: !1777, inlinedAt: !2333)
!2346 = !DILocation(line: 188, column: 26, scope: !1777, inlinedAt: !2333)
!2347 = !DILocation(line: 189, column: 59, scope: !1777, inlinedAt: !2333)
!2348 = !DILocation(line: 189, column: 16, scope: !1777, inlinedAt: !2333)
!2349 = !DILocation(line: 189, column: 34, scope: !1777, inlinedAt: !2333)
!2350 = !DILocalVariable(name: "__first", arg: 1, scope: !2351, file: !1816, line: 287, type: !291)
!2351 = distinct !DISubprogram(name: "__uninitialized_copy_a<__gnu_cxx::__normal_iterator<const int *, std::vector<int, std::allocator<int> > >, int *, int>", linkageName: "_ZSt22__uninitialized_copy_aIN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEEPiiET0_T_SA_S9_RSaIT1_E", scope: !2, file: !1816, line: 287, type: !2352, isLocal: false, isDefinition: true, scopeLine: 289, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2358, variables: !2354)
!2352 = !DISubroutineType(types: !2353)
!2353 = !{!55, !291, !291, !55, !144}
!2354 = !{!2350, !2355, !2356, !2357}
!2355 = !DILocalVariable(name: "__last", arg: 2, scope: !2351, file: !1816, line: 287, type: !291)
!2356 = !DILocalVariable(name: "__result", arg: 3, scope: !2351, file: !1816, line: 288, type: !55)
!2357 = !DILocalVariable(arg: 4, scope: !2351, file: !1816, line: 288, type: !144)
!2358 = !{!2359, !1823, !108}
!2359 = !DITemplateTypeParameter(name: "_InputIterator", type: !291)
!2360 = !DILocation(line: 287, column: 43, scope: !2351, inlinedAt: !2361)
!2361 = distinct !DILocation(line: 331, column: 4, scope: !2362, inlinedAt: !2319)
!2362 = distinct !DILexicalBlock(scope: !2315, file: !33, line: 329, column: 7)
!2363 = !DILocation(line: 287, column: 67, scope: !2351, inlinedAt: !2361)
!2364 = !DILocation(line: 288, column: 24, scope: !2351, inlinedAt: !2361)
!2365 = !DILocalVariable(name: "__first", arg: 1, scope: !2366, file: !1816, line: 115, type: !291)
!2366 = distinct !DISubprogram(name: "uninitialized_copy<__gnu_cxx::__normal_iterator<const int *, std::vector<int, std::allocator<int> > >, int *>", linkageName: "_ZSt18uninitialized_copyIN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEEPiET0_T_SA_S9_", scope: !2, file: !1816, line: 115, type: !2367, isLocal: false, isDefinition: true, scopeLine: 117, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2373, variables: !2369)
!2367 = !DISubroutineType(types: !2368)
!2368 = !{!55, !291, !291, !55}
!2369 = !{!2365, !2370, !2371, !2372}
!2370 = !DILocalVariable(name: "__last", arg: 2, scope: !2366, file: !1816, line: 115, type: !291)
!2371 = !DILocalVariable(name: "__result", arg: 3, scope: !2366, file: !1816, line: 116, type: !55)
!2372 = !DILocalVariable(name: "__assignable", scope: !2366, file: !1816, line: 128, type: !483)
!2373 = !{!2359, !1823}
!2374 = !DILocation(line: 115, column: 39, scope: !2366, inlinedAt: !2375)
!2375 = distinct !DILocation(line: 289, column: 14, scope: !2351, inlinedAt: !2361)
!2376 = !DILocation(line: 115, column: 63, scope: !2366, inlinedAt: !2375)
!2377 = !DILocation(line: 116, column: 27, scope: !2366, inlinedAt: !2375)
!2378 = !DILocation(line: 128, column: 18, scope: !2366, inlinedAt: !2375)
!2379 = !DILocalVariable(name: "__first", arg: 1, scope: !2380, file: !1816, line: 99, type: !291)
!2380 = distinct !DISubprogram(name: "__uninit_copy<__gnu_cxx::__normal_iterator<const int *, std::vector<int, std::allocator<int> > >, int *>", linkageName: "_ZNSt20__uninitialized_copyILb1EE13__uninit_copyIN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEEPiEET0_T_SC_SB_", scope: !2026, file: !1816, line: 99, type: !2367, isLocal: false, isDefinition: true, scopeLine: 101, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2373, declaration: !2381, variables: !2382)
!2381 = !DISubprogram(name: "__uninit_copy<__gnu_cxx::__normal_iterator<const int *, std::vector<int, std::allocator<int> > >, int *>", linkageName: "_ZNSt20__uninitialized_copyILb1EE13__uninit_copyIN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEEPiEET0_T_SC_SB_", scope: !2026, file: !1816, line: 99, type: !2367, isLocal: false, isDefinition: false, scopeLine: 99, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true, templateParams: !2373)
!2382 = !{!2379, !2383, !2384}
!2383 = !DILocalVariable(name: "__last", arg: 2, scope: !2380, file: !1816, line: 99, type: !291)
!2384 = !DILocalVariable(name: "__result", arg: 3, scope: !2380, file: !1816, line: 100, type: !55)
!2385 = !DILocation(line: 99, column: 38, scope: !2380, inlinedAt: !2386)
!2386 = distinct !DILocation(line: 131, column: 14, scope: !2366, inlinedAt: !2375)
!2387 = !DILocation(line: 99, column: 62, scope: !2380, inlinedAt: !2386)
!2388 = !DILocation(line: 100, column: 26, scope: !2380, inlinedAt: !2386)
!2389 = !DILocalVariable(name: "__first", arg: 1, scope: !2390, file: !1853, line: 446, type: !291)
!2390 = distinct !DISubprogram(name: "copy<__gnu_cxx::__normal_iterator<const int *, std::vector<int, std::allocator<int> > >, int *>", linkageName: "_ZSt4copyIN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEEPiET0_T_SA_S9_", scope: !2, file: !1853, line: 446, type: !2367, isLocal: false, isDefinition: true, scopeLine: 447, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2394, variables: !2391)
!2391 = !{!2389, !2392, !2393}
!2392 = !DILocalVariable(name: "__last", arg: 2, scope: !2390, file: !1853, line: 446, type: !291)
!2393 = !DILocalVariable(name: "__result", arg: 3, scope: !2390, file: !1853, line: 446, type: !55)
!2394 = !{!2395, !1860}
!2395 = !DITemplateTypeParameter(name: "_II", type: !291)
!2396 = !DILocation(line: 446, column: 14, scope: !2390, inlinedAt: !2397)
!2397 = distinct !DILocation(line: 101, column: 18, scope: !2380, inlinedAt: !2386)
!2398 = !DILocation(line: 446, column: 27, scope: !2390, inlinedAt: !2397)
!2399 = !DILocation(line: 446, column: 39, scope: !2390, inlinedAt: !2397)
!2400 = !DILocalVariable(name: "__first", arg: 1, scope: !2401, file: !1853, line: 420, type: !291)
!2401 = distinct !DISubprogram(name: "__copy_move_a2<false, __gnu_cxx::__normal_iterator<const int *, std::vector<int, std::allocator<int> > >, int *>", linkageName: "_ZSt14__copy_move_a2ILb0EN9__gnu_cxx17__normal_iteratorIPKiSt6vectorIiSaIiEEEEPiET1_T0_SA_S9_", scope: !2, file: !1853, line: 420, type: !2367, isLocal: false, isDefinition: true, scopeLine: 421, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2405, variables: !2402)
!2402 = !{!2400, !2403, !2404}
!2403 = !DILocalVariable(name: "__last", arg: 2, scope: !2401, file: !1853, line: 420, type: !291)
!2404 = !DILocalVariable(name: "__result", arg: 3, scope: !2401, file: !1853, line: 420, type: !55)
!2405 = !{!2406, !2395, !1860}
!2406 = !DITemplateValueParameter(name: "_IsMove", type: !13, value: i8 0)
!2407 = !DILocation(line: 420, column: 24, scope: !2401, inlinedAt: !2408)
!2408 = distinct !DILocation(line: 454, column: 15, scope: !2390, inlinedAt: !2397)
!2409 = !DILocation(line: 420, column: 37, scope: !2401, inlinedAt: !2408)
!2410 = !DILocation(line: 420, column: 49, scope: !2401, inlinedAt: !2408)
!2411 = !DILocalVariable(name: "__first", arg: 1, scope: !2412, file: !1853, line: 375, type: !88)
!2412 = distinct !DISubprogram(name: "__copy_move_a<false, const int *, int *>", linkageName: "_ZSt13__copy_move_aILb0EPKiPiET1_T0_S4_S3_", scope: !2, file: !1853, line: 375, type: !2088, isLocal: false, isDefinition: true, scopeLine: 376, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2417, variables: !2413)
!2413 = !{!2411, !2414, !2415, !2416}
!2414 = !DILocalVariable(name: "__last", arg: 2, scope: !2412, file: !1853, line: 375, type: !88)
!2415 = !DILocalVariable(name: "__result", arg: 3, scope: !2412, file: !1853, line: 375, type: !55)
!2416 = !DILocalVariable(name: "__simple", scope: !2412, file: !1853, line: 380, type: !483)
!2417 = !{!2406, !2418, !1860}
!2418 = !DITemplateTypeParameter(name: "_II", type: !88)
!2419 = !DILocation(line: 375, column: 23, scope: !2412, inlinedAt: !2420)
!2420 = distinct !DILocation(line: 422, column: 18, scope: !2401, inlinedAt: !2408)
!2421 = !DILocation(line: 375, column: 36, scope: !2412, inlinedAt: !2420)
!2422 = !DILocation(line: 375, column: 48, scope: !2412, inlinedAt: !2420)
!2423 = !DILocation(line: 380, column: 18, scope: !2412, inlinedAt: !2420)
!2424 = !DILocalVariable(name: "__first", arg: 1, scope: !2425, file: !1853, line: 357, type: !88)
!2425 = distinct !DISubprogram(name: "__copy_m<int>", linkageName: "_ZNSt11__copy_moveILb0ELb1ESt26random_access_iterator_tagE8__copy_mIiEEPT_PKS3_S6_S4_", scope: !2426, file: !1853, line: 357, type: !2088, isLocal: false, isDefinition: true, scopeLine: 358, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !107, declaration: !2429, variables: !2430)
!2426 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__copy_move<false, true, std::random_access_iterator_tag>", scope: !2, file: !1853, line: 353, size: 8, elements: !25, templateParams: !2427, identifier: "_ZTSSt11__copy_moveILb0ELb1ESt26random_access_iterator_tagE")
!2427 = !{!2428, !1872, !2077}
!2428 = !DITemplateValueParameter(type: !13, value: i8 0)
!2429 = !DISubprogram(name: "__copy_m<int>", linkageName: "_ZNSt11__copy_moveILb0ELb1ESt26random_access_iterator_tagE8__copy_mIiEEPT_PKS3_S6_S4_", scope: !2426, file: !1853, line: 357, type: !2088, isLocal: false, isDefinition: false, scopeLine: 357, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true, templateParams: !107)
!2430 = !{!2424, !2431, !2432, !2433}
!2431 = !DILocalVariable(name: "__last", arg: 2, scope: !2425, file: !1853, line: 357, type: !88)
!2432 = !DILocalVariable(name: "__result", arg: 3, scope: !2425, file: !1853, line: 357, type: !55)
!2433 = !DILocalVariable(name: "_Num", scope: !2425, file: !1853, line: 366, type: !2095)
!2434 = !DILocation(line: 357, column: 29, scope: !2425, inlinedAt: !2435)
!2435 = distinct !DILocation(line: 385, column: 14, scope: !2412, inlinedAt: !2420)
!2436 = !DILocation(line: 357, column: 49, scope: !2425, inlinedAt: !2435)
!2437 = !DILocation(line: 357, column: 62, scope: !2425, inlinedAt: !2435)
!2438 = !DILocation(line: 366, column: 20, scope: !2425, inlinedAt: !2435)
!2439 = !DILocation(line: 368, column: 6, scope: !2440, inlinedAt: !2435)
!2440 = distinct !DILexicalBlock(scope: !2425, file: !1853, line: 367, column: 8)
!2441 = !DILocation(line: 330, column: 26, scope: !2362, inlinedAt: !2319)
!2442 = !DILocation(line: 0, scope: !2315, inlinedAt: !2443)
!2443 = distinct !DILocation(line: 30, column: 20, scope: !1747)
!2444 = !DILocation(line: 326, column: 28, scope: !2315, inlinedAt: !2443)
!2445 = !DILocation(line: 0, scope: !1582, inlinedAt: !2446)
!2446 = distinct !DILocation(line: 327, column: 19, scope: !2315, inlinedAt: !2443)
!2447 = !DILocation(line: 0, scope: !1761, inlinedAt: !2448)
!2448 = distinct !DILocation(line: 327, column: 9, scope: !2315, inlinedAt: !2443)
!2449 = !DILocation(line: 136, column: 27, scope: !1761, inlinedAt: !2448)
!2450 = !DILocation(line: 136, column: 54, scope: !1761, inlinedAt: !2448)
!2451 = !DILocation(line: 0, scope: !1770, inlinedAt: !2452)
!2452 = distinct !DILocation(line: 137, column: 9, scope: !1761, inlinedAt: !2448)
!2453 = !DILocation(line: 92, column: 37, scope: !1770, inlinedAt: !2452)
!2454 = !DILocation(line: 94, column: 4, scope: !1770, inlinedAt: !2452)
!2455 = !DILocation(line: 93, column: 37, scope: !1770, inlinedAt: !2452)
!2456 = !DILocation(line: 0, scope: !1777, inlinedAt: !2457)
!2457 = distinct !DILocation(line: 138, column: 9, scope: !1782, inlinedAt: !2448)
!2458 = !DILocation(line: 185, column: 32, scope: !1777, inlinedAt: !2457)
!2459 = !DILocation(line: 0, scope: !1785, inlinedAt: !2460)
!2460 = distinct !DILocation(line: 187, column: 33, scope: !1777, inlinedAt: !2457)
!2461 = !DILocation(line: 169, column: 26, scope: !1785, inlinedAt: !2460)
!2462 = !DILocation(line: 435, column: 47, scope: !1792, inlinedAt: !2463)
!2463 = distinct !DILocation(line: 172, column: 20, scope: !1785, inlinedAt: !2460)
!2464 = !DILocation(line: 99, column: 26, scope: !1798, inlinedAt: !2465)
!2465 = distinct !DILocation(line: 436, column: 20, scope: !1792, inlinedAt: !2463)
!2466 = !DILocation(line: 99, column: 43, scope: !1798, inlinedAt: !2465)
!2467 = !DILocation(line: 111, column: 27, scope: !1798, inlinedAt: !2465)
!2468 = !DILocation(line: 187, column: 25, scope: !1777, inlinedAt: !2457)
!2469 = !DILocation(line: 188, column: 16, scope: !1777, inlinedAt: !2457)
!2470 = !DILocation(line: 188, column: 26, scope: !1777, inlinedAt: !2457)
!2471 = !DILocation(line: 189, column: 59, scope: !1777, inlinedAt: !2457)
!2472 = !DILocation(line: 189, column: 16, scope: !1777, inlinedAt: !2457)
!2473 = !DILocation(line: 189, column: 34, scope: !1777, inlinedAt: !2457)
!2474 = !DILocation(line: 287, column: 43, scope: !2351, inlinedAt: !2475)
!2475 = distinct !DILocation(line: 331, column: 4, scope: !2362, inlinedAt: !2443)
!2476 = !DILocation(line: 287, column: 67, scope: !2351, inlinedAt: !2475)
!2477 = !DILocation(line: 288, column: 24, scope: !2351, inlinedAt: !2475)
!2478 = !DILocation(line: 115, column: 39, scope: !2366, inlinedAt: !2479)
!2479 = distinct !DILocation(line: 289, column: 14, scope: !2351, inlinedAt: !2475)
!2480 = !DILocation(line: 115, column: 63, scope: !2366, inlinedAt: !2479)
!2481 = !DILocation(line: 116, column: 27, scope: !2366, inlinedAt: !2479)
!2482 = !DILocation(line: 128, column: 18, scope: !2366, inlinedAt: !2479)
!2483 = !DILocation(line: 99, column: 38, scope: !2380, inlinedAt: !2484)
!2484 = distinct !DILocation(line: 131, column: 14, scope: !2366, inlinedAt: !2479)
!2485 = !DILocation(line: 99, column: 62, scope: !2380, inlinedAt: !2484)
!2486 = !DILocation(line: 100, column: 26, scope: !2380, inlinedAt: !2484)
!2487 = !DILocation(line: 446, column: 14, scope: !2390, inlinedAt: !2488)
!2488 = distinct !DILocation(line: 101, column: 18, scope: !2380, inlinedAt: !2484)
!2489 = !DILocation(line: 446, column: 27, scope: !2390, inlinedAt: !2488)
!2490 = !DILocation(line: 446, column: 39, scope: !2390, inlinedAt: !2488)
!2491 = !DILocation(line: 420, column: 24, scope: !2401, inlinedAt: !2492)
!2492 = distinct !DILocation(line: 454, column: 15, scope: !2390, inlinedAt: !2488)
!2493 = !DILocation(line: 420, column: 37, scope: !2401, inlinedAt: !2492)
!2494 = !DILocation(line: 420, column: 49, scope: !2401, inlinedAt: !2492)
!2495 = !DILocation(line: 375, column: 23, scope: !2412, inlinedAt: !2496)
!2496 = distinct !DILocation(line: 422, column: 18, scope: !2401, inlinedAt: !2492)
!2497 = !DILocation(line: 375, column: 36, scope: !2412, inlinedAt: !2496)
!2498 = !DILocation(line: 375, column: 48, scope: !2412, inlinedAt: !2496)
!2499 = !DILocation(line: 380, column: 18, scope: !2412, inlinedAt: !2496)
!2500 = !DILocation(line: 357, column: 29, scope: !2425, inlinedAt: !2501)
!2501 = distinct !DILocation(line: 385, column: 14, scope: !2412, inlinedAt: !2496)
!2502 = !DILocation(line: 357, column: 49, scope: !2425, inlinedAt: !2501)
!2503 = !DILocation(line: 357, column: 62, scope: !2425, inlinedAt: !2501)
!2504 = !DILocation(line: 366, column: 20, scope: !2425, inlinedAt: !2501)
!2505 = !DILocation(line: 368, column: 6, scope: !2440, inlinedAt: !2501)
!2506 = !DILocation(line: 330, column: 26, scope: !2362, inlinedAt: !2443)
!2507 = !DILocation(line: 30, column: 11, scope: !1747)
!2508 = !DILocation(line: 0, scope: !1617, inlinedAt: !2509)
!2509 = distinct !DILocation(line: 30, column: 11, scope: !1747)
!2510 = !DILocation(line: 795, column: 28, scope: !1617, inlinedAt: !2509)
!2511 = !DILocation(line: 798, column: 25, scope: !1617, inlinedAt: !2509)
!2512 = !DILocation(line: 798, column: 34, scope: !1617, inlinedAt: !2509)
!2513 = !DILocation(line: 30, column: 8, scope: !1747)
!2514 = !DILocation(line: 0, scope: !1684, inlinedAt: !2515)
!2515 = distinct !DILocation(line: 30, column: 3, scope: !1747)
!2516 = !DILocation(line: 0, scope: !1689, inlinedAt: !2517)
!2517 = distinct !DILocation(line: 435, column: 33, scope: !1693, inlinedAt: !2515)
!2518 = !DILocation(line: 162, column: 37, scope: !1695, inlinedAt: !2517)
!2519 = !DILocation(line: 0, scope: !1697, inlinedAt: !2520)
!2520 = distinct !DILocation(line: 162, column: 9, scope: !1695, inlinedAt: !2517)
!2521 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2520)
!2522 = !DILocation(line: 179, column: 6, scope: !1705, inlinedAt: !2520)
!2523 = !DILocation(line: 179, column: 6, scope: !1697, inlinedAt: !2520)
!2524 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2525)
!2525 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2520)
!2526 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2527)
!2527 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2525)
!2528 = !DILocation(line: 125, column: 20, scope: !1715, inlinedAt: !2527)
!2529 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2527)
!2530 = !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2520)
!2531 = !DILocation(line: 0, scope: !1684, inlinedAt: !2532)
!2532 = distinct !DILocation(line: 30, column: 3, scope: !1747)
!2533 = !DILocation(line: 0, scope: !1689, inlinedAt: !2534)
!2534 = distinct !DILocation(line: 435, column: 33, scope: !1693, inlinedAt: !2532)
!2535 = !DILocation(line: 0, scope: !1697, inlinedAt: !2536)
!2536 = distinct !DILocation(line: 162, column: 9, scope: !1695, inlinedAt: !2534)
!2537 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2536)
!2538 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2539)
!2539 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2536)
!2540 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2541)
!2541 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2539)
!2542 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2541)
!2543 = !DILocation(line: 0, scope: !1684, inlinedAt: !2544)
!2544 = distinct !DILocation(line: 30, column: 3, scope: !1747)
!2545 = !DILocation(line: 0, scope: !1689, inlinedAt: !2546)
!2546 = distinct !DILocation(line: 435, column: 33, scope: !1693, inlinedAt: !2544)
!2547 = !DILocation(line: 0, scope: !1697, inlinedAt: !2548)
!2548 = distinct !DILocation(line: 162, column: 9, scope: !1695, inlinedAt: !2546)
!2549 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2548)
!2550 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2551)
!2551 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2548)
!2552 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2553)
!2553 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2551)
!2554 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2553)
!2555 = !DILocation(line: 27, column: 15, scope: !1747)
!2556 = !DILocation(line: 0, scope: !1684, inlinedAt: !2557)
!2557 = distinct !DILocation(line: 31, column: 1, scope: !1747)
!2558 = !DILocation(line: 0, scope: !1689, inlinedAt: !2559)
!2559 = distinct !DILocation(line: 435, column: 33, scope: !1693, inlinedAt: !2557)
!2560 = !DILocation(line: 0, scope: !1697, inlinedAt: !2561)
!2561 = distinct !DILocation(line: 162, column: 9, scope: !1695, inlinedAt: !2559)
!2562 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2561)
!2563 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2564)
!2564 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2561)
!2565 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2566)
!2566 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2564)
!2567 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2566)
!2568 = !DILocation(line: 24, column: 15, scope: !1747)
!2569 = !DILocation(line: 0, scope: !1684, inlinedAt: !2570)
!2570 = distinct !DILocation(line: 31, column: 1, scope: !1747)
!2571 = !DILocation(line: 0, scope: !1689, inlinedAt: !2572)
!2572 = distinct !DILocation(line: 435, column: 33, scope: !1693, inlinedAt: !2570)
!2573 = !DILocation(line: 0, scope: !1697, inlinedAt: !2574)
!2574 = distinct !DILocation(line: 162, column: 9, scope: !1695, inlinedAt: !2572)
!2575 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2574)
!2576 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2577)
!2577 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2574)
!2578 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2579)
!2579 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2577)
!2580 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2579)
!2581 = !DILocation(line: 31, column: 1, scope: !1747)
!2582 = !DILocation(line: 187, column: 25, scope: !1777, inlinedAt: !1781)
!2583 = !DILocation(line: 25, column: 3, scope: !1747)
!2584 = !DILocation(line: 0, scope: !1684, inlinedAt: !2585)
!2585 = distinct !DILocation(line: 31, column: 1, scope: !1747)
!2586 = !DILocation(line: 0, scope: !1689, inlinedAt: !2587)
!2587 = distinct !DILocation(line: 435, column: 33, scope: !1693, inlinedAt: !2585)
!2588 = !DILocation(line: 0, scope: !1697, inlinedAt: !2589)
!2589 = distinct !DILocation(line: 162, column: 9, scope: !1695, inlinedAt: !2587)
!2590 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2589)
!2591 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2592)
!2592 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2589)
!2593 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2594)
!2594 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2592)
!2595 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2594)
!2596 = !DILocation(line: 0, scope: !1684, inlinedAt: !2597)
!2597 = distinct !DILocation(line: 31, column: 1, scope: !1747)
!2598 = !DILocation(line: 0, scope: !1689, inlinedAt: !2599)
!2599 = distinct !DILocation(line: 435, column: 33, scope: !1693, inlinedAt: !2597)
!2600 = !DILocation(line: 0, scope: !1697, inlinedAt: !2601)
!2601 = distinct !DILocation(line: 162, column: 9, scope: !1695, inlinedAt: !2599)
!2602 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2601)
!2603 = !DILocation(line: 179, column: 6, scope: !1697, inlinedAt: !2601)
!2604 = !DILocation(line: 0, scope: !1684, inlinedAt: !2605)
!2605 = distinct !DILocation(line: 30, column: 3, scope: !1747)
!2606 = !DILocation(line: 0, scope: !1689, inlinedAt: !2607)
!2607 = distinct !DILocation(line: 435, column: 33, scope: !1693, inlinedAt: !2605)
!2608 = !DILocation(line: 162, column: 37, scope: !1695, inlinedAt: !2607)
!2609 = !DILocation(line: 0, scope: !1697, inlinedAt: !2610)
!2610 = distinct !DILocation(line: 162, column: 9, scope: !1695, inlinedAt: !2607)
!2611 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2610)
!2612 = !DILocation(line: 179, column: 6, scope: !1705, inlinedAt: !2610)
!2613 = !DILocation(line: 179, column: 6, scope: !1697, inlinedAt: !2610)
!2614 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2615)
!2615 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2610)
!2616 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2617)
!2617 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2615)
!2618 = !DILocation(line: 125, column: 20, scope: !1715, inlinedAt: !2617)
!2619 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2617)
!2620 = !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2610)
!2621 = !DILocation(line: 0, scope: !1684, inlinedAt: !2622)
!2622 = distinct !DILocation(line: 30, column: 3, scope: !1747)
!2623 = !DILocation(line: 0, scope: !1689, inlinedAt: !2624)
!2624 = distinct !DILocation(line: 435, column: 33, scope: !1693, inlinedAt: !2622)
!2625 = !DILocation(line: 0, scope: !1697, inlinedAt: !2626)
!2626 = distinct !DILocation(line: 162, column: 9, scope: !1695, inlinedAt: !2624)
!2627 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2626)
!2628 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2629)
!2629 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2626)
!2630 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2631)
!2631 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2629)
!2632 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2631)
!2633 = !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2626)
!2634 = !DILocation(line: 0, scope: !1684, inlinedAt: !2635)
!2635 = distinct !DILocation(line: 30, column: 3, scope: !1747)
!2636 = !DILocation(line: 0, scope: !1689, inlinedAt: !2637)
!2637 = distinct !DILocation(line: 435, column: 33, scope: !1693, inlinedAt: !2635)
!2638 = !DILocation(line: 0, scope: !1697, inlinedAt: !2639)
!2639 = distinct !DILocation(line: 162, column: 9, scope: !1695, inlinedAt: !2637)
!2640 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2639)
!2641 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2642)
!2642 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2639)
!2643 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2644)
!2644 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2642)
!2645 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2644)
!2646 = !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2639)
!2647 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2648)
!2648 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2601)
!2649 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2650)
!2650 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2648)
!2651 = !DILocation(line: 125, column: 20, scope: !1715, inlinedAt: !2650)
!2652 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2650)
!2653 = !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2601)
!2654 = distinct !DISubprogram(name: "_M_realloc_insert<const int &>", linkageName: "_ZNSt6vectorIiSaIiEE17_M_realloc_insertIJRKiEEEvN9__gnu_cxx17__normal_iteratorIPiS1_EEDpOT_", scope: !225, file: !1897, line: 395, type: !2655, isLocal: false, isDefinition: true, scopeLine: 402, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2658, declaration: !2657, variables: !2659)
!2655 = !DISubroutineType(types: !2656)
!2656 = !{null, !231, !224, !91}
!2657 = !DISubprogram(name: "_M_realloc_insert<const int &>", linkageName: "_ZNSt6vectorIiSaIiEE17_M_realloc_insertIJRKiEEEvN9__gnu_cxx17__normal_iteratorIPiS1_EEDpOT_", scope: !225, file: !33, line: 1478, type: !2655, isLocal: false, isDefinition: false, scopeLine: 1478, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true, templateParams: !2658)
!2658 = !{!1654}
!2659 = !{!2660, !2661, !2662, !2663, !2664, !2665, !2666}
!2660 = !DILocalVariable(name: "this", arg: 1, scope: !2654, type: !1561, flags: DIFlagArtificial | DIFlagObjectPointer)
!2661 = !DILocalVariable(name: "__position", arg: 2, scope: !2654, file: !33, line: 1478, type: !224)
!2662 = !DILocalVariable(name: "__args", arg: 3, scope: !2654, file: !33, line: 1478, type: !91)
!2663 = !DILocalVariable(name: "__len", scope: !2654, file: !1897, line: 403, type: !1917)
!2664 = !DILocalVariable(name: "__elems_before", scope: !2654, file: !1897, line: 405, type: !1917)
!2665 = !DILocalVariable(name: "__new_start", scope: !2654, file: !1897, line: 406, type: !468)
!2666 = !DILocalVariable(name: "__new_finish", scope: !2654, file: !1897, line: 407, type: !468)
!2667 = !DILocation(line: 0, scope: !2654)
!2668 = !DILocation(line: 1478, column: 52, scope: !2654)
!2669 = !DILocation(line: 0, scope: !1925, inlinedAt: !2670)
!2670 = distinct !DILocation(line: 404, column: 2, scope: !2654)
!2671 = !DILocation(line: 1497, column: 30, scope: !1925, inlinedAt: !2670)
!2672 = !DILocation(line: 0, scope: !1582, inlinedAt: !2673)
!2673 = distinct !DILocation(line: 1499, column: 19, scope: !1935, inlinedAt: !2670)
!2674 = !DILocation(line: 671, column: 40, scope: !1582, inlinedAt: !2673)
!2675 = !DILocation(line: 671, column: 66, scope: !1582, inlinedAt: !2673)
!2676 = !{!1592, !1592, i64 0}
!2677 = !DILocation(line: 671, column: 50, scope: !1582, inlinedAt: !2673)
!2678 = !DILocation(line: 0, scope: !1582, inlinedAt: !2679)
!2679 = distinct !DILocation(line: 1502, column: 26, scope: !1925, inlinedAt: !2670)
!2680 = !DILocation(line: 0, scope: !1582, inlinedAt: !2681)
!2681 = distinct !DILocation(line: 1502, column: 44, scope: !1925, inlinedAt: !2670)
!2682 = !DILocation(line: 224, column: 15, scope: !2683, inlinedAt: !2694)
!2683 = distinct !DILexicalBlock(scope: !2684, file: !1853, line: 224, column: 11)
!2684 = distinct !DISubprogram(name: "max<unsigned long>", linkageName: "_ZSt3maxImERKT_S2_S2_", scope: !2, file: !1853, line: 219, type: !2685, isLocal: false, isDefinition: true, scopeLine: 220, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !2692, variables: !2689)
!2685 = !DISubroutineType(types: !2686)
!2686 = !{!2687, !2687, !2687}
!2687 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !2688, size: 64)
!2688 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !98)
!2689 = !{!2690, !2691}
!2690 = !DILocalVariable(name: "__a", arg: 1, scope: !2684, file: !1853, line: 219, type: !2687)
!2691 = !DILocalVariable(name: "__b", arg: 2, scope: !2684, file: !1853, line: 219, type: !2687)
!2692 = !{!2693}
!2693 = !DITemplateTypeParameter(name: "_Tp", type: !98)
!2694 = distinct !DILocation(line: 1502, column: 35, scope: !1925, inlinedAt: !2670)
!2695 = !DILocation(line: 1502, column: 35, scope: !1925, inlinedAt: !2670)
!2696 = !DILocation(line: 1502, column: 33, scope: !1925, inlinedAt: !2670)
!2697 = !DILocation(line: 1502, column: 18, scope: !1925, inlinedAt: !2670)
!2698 = !DILocation(line: 0, scope: !1582, inlinedAt: !2699)
!2699 = distinct !DILocation(line: 1503, column: 18, scope: !1925, inlinedAt: !2670)
!2700 = !DILocation(line: 1503, column: 16, scope: !1925, inlinedAt: !2670)
!2701 = !DILocation(line: 1503, column: 34, scope: !1925, inlinedAt: !2670)
!2702 = !DILocation(line: 1503, column: 25, scope: !1925, inlinedAt: !2670)
!2703 = !DILocation(line: 403, column: 23, scope: !2654)
!2704 = !DILocalVariable(name: "this", arg: 1, scope: !2705, type: !1561, flags: DIFlagArtificial | DIFlagObjectPointer)
!2705 = distinct !DISubprogram(name: "begin", linkageName: "_ZNSt6vectorIiSaIiEE5beginEv", scope: !225, file: !33, line: 563, type: !285, isLocal: false, isDefinition: true, scopeLine: 564, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !284, variables: !2706)
!2706 = !{!2704}
!2707 = !DILocation(line: 0, scope: !2705, inlinedAt: !2708)
!2708 = distinct !DILocation(line: 405, column: 53, scope: !2654)
!2709 = !DILocalVariable(name: "this", arg: 1, scope: !2710, type: !2713, flags: DIFlagArtificial | DIFlagObjectPointer)
!2710 = distinct !DISubprogram(name: "__normal_iterator", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPiSt6vectorIiSaIiEEEC2ERKS1_", scope: !510, file: !292, line: 779, type: !518, isLocal: false, isDefinition: true, scopeLine: 780, flags: DIFlagPrototyped, isOptimized: true, unit: !19, declaration: !517, variables: !2711)
!2711 = !{!2709, !2712}
!2712 = !DILocalVariable(name: "__i", arg: 2, scope: !2710, file: !292, line: 779, type: !520)
!2713 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !510, size: 64)
!2714 = !DILocation(line: 0, scope: !2710, inlinedAt: !2715)
!2715 = distinct !DILocation(line: 564, column: 16, scope: !2705, inlinedAt: !2708)
!2716 = !DILocation(line: 779, column: 42, scope: !2710, inlinedAt: !2715)
!2717 = !DILocation(line: 780, column: 20, scope: !2710, inlinedAt: !2715)
!2718 = !DILocation(line: 1478, column: 29, scope: !2654)
!2719 = !DILocalVariable(name: "__lhs", arg: 1, scope: !2720, file: !292, line: 962, type: !2723)
!2720 = distinct !DISubprogram(name: "operator-<int *, std::vector<int, std::allocator<int> > >", linkageName: "_ZN9__gnu_cxxmiIPiSt6vectorIiSaIiEEEENS_17__normal_iteratorIT_T0_E15difference_typeERKS8_SB_", scope: !45, file: !292, line: 962, type: !2721, isLocal: false, isDefinition: true, scopeLine: 965, flags: DIFlagPrototyped, isOptimized: true, unit: !19, templateParams: !562, variables: !2724)
!2721 = !DISubroutineType(types: !2722)
!2722 = !{!549, !2723, !2723}
!2723 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !531, size: 64)
!2724 = !{!2719, !2725}
!2725 = !DILocalVariable(name: "__rhs", arg: 2, scope: !2720, file: !292, line: 963, type: !2723)
!2726 = !DILocation(line: 962, column: 63, scope: !2720, inlinedAt: !2727)
!2727 = distinct !DILocation(line: 405, column: 51, scope: !2654)
!2728 = !DILocation(line: 963, column: 56, scope: !2720, inlinedAt: !2727)
!2729 = !DILocation(line: 965, column: 27, scope: !2720, inlinedAt: !2727)
!2730 = !DILocation(line: 405, column: 23, scope: !2654)
!2731 = !DILocation(line: 0, scope: !1785, inlinedAt: !2732)
!2732 = distinct !DILocation(line: 406, column: 33, scope: !2654)
!2733 = !DILocation(line: 169, column: 26, scope: !1785, inlinedAt: !2732)
!2734 = !DILocation(line: 172, column: 13, scope: !1785, inlinedAt: !2732)
!2735 = !DILocation(line: 172, column: 9, scope: !1785, inlinedAt: !2732)
!2736 = !DILocation(line: 435, column: 47, scope: !1792, inlinedAt: !2737)
!2737 = distinct !DILocation(line: 172, column: 20, scope: !1785, inlinedAt: !2732)
!2738 = !DILocation(line: 99, column: 26, scope: !1798, inlinedAt: !2739)
!2739 = distinct !DILocation(line: 436, column: 20, scope: !1792, inlinedAt: !2737)
!2740 = !DILocation(line: 99, column: 43, scope: !1798, inlinedAt: !2739)
!2741 = !DILocation(line: 101, column: 10, scope: !2742, inlinedAt: !2739)
!2742 = distinct !DILexicalBlock(scope: !1798, file: !65, line: 101, column: 6)
!2743 = !DILocation(line: 101, column: 6, scope: !1798, inlinedAt: !2739)
!2744 = !DILocation(line: 102, column: 4, scope: !2742, inlinedAt: !2739)
!2745 = !DILocation(line: 111, column: 46, scope: !1798, inlinedAt: !2739)
!2746 = !DILocation(line: 111, column: 27, scope: !1798, inlinedAt: !2739)
!2747 = !DILocation(line: 111, column: 9, scope: !1798, inlinedAt: !2739)
!2748 = !DILocation(line: 426, column: 21, scope: !2749)
!2749 = distinct !DILexicalBlock(scope: !2654, file: !1897, line: 409, column: 2)
!2750 = !DILocation(line: 406, column: 15, scope: !2654)
!2751 = !DILocation(line: 407, column: 15, scope: !2654)
!2752 = !DILocation(line: 416, column: 20, scope: !2749)
!2753 = !DILocation(line: 474, column: 38, scope: !1648, inlinedAt: !2754)
!2754 = distinct !DILocation(line: 415, column: 4, scope: !2749)
!2755 = !DILocation(line: 474, column: 54, scope: !1648, inlinedAt: !2754)
!2756 = !DILocation(line: 135, column: 17, scope: !1665, inlinedAt: !2757)
!2757 = distinct !DILocation(line: 475, column: 8, scope: !1648, inlinedAt: !2754)
!2758 = !DILocation(line: 135, column: 33, scope: !1665, inlinedAt: !2757)
!2759 = !DILocation(line: 136, column: 27, scope: !1665, inlinedAt: !2757)
!2760 = !DILocation(line: 136, column: 4, scope: !1665, inlinedAt: !2757)
!2761 = !DILocation(line: 305, column: 55, scope: !1982, inlinedAt: !2762)
!2762 = distinct !DILocation(line: 425, column: 8, scope: !2749)
!2763 = !DILocation(line: 306, column: 27, scope: !1982, inlinedAt: !2762)
!2764 = !DILocation(line: 307, column: 29, scope: !1982, inlinedAt: !2762)
!2765 = !DILocation(line: 287, column: 43, scope: !1997, inlinedAt: !2766)
!2766 = distinct !DILocation(line: 310, column: 14, scope: !1982, inlinedAt: !2762)
!2767 = !DILocation(line: 287, column: 67, scope: !1997, inlinedAt: !2766)
!2768 = !DILocation(line: 288, column: 24, scope: !1997, inlinedAt: !2766)
!2769 = !DILocation(line: 115, column: 39, scope: !2011, inlinedAt: !2770)
!2770 = distinct !DILocation(line: 289, column: 14, scope: !1997, inlinedAt: !2766)
!2771 = !DILocation(line: 115, column: 63, scope: !2011, inlinedAt: !2770)
!2772 = !DILocation(line: 116, column: 27, scope: !2011, inlinedAt: !2770)
!2773 = !DILocation(line: 128, column: 18, scope: !2011, inlinedAt: !2770)
!2774 = !DILocation(line: 99, column: 38, scope: !2025, inlinedAt: !2775)
!2775 = distinct !DILocation(line: 131, column: 14, scope: !2011, inlinedAt: !2770)
!2776 = !DILocation(line: 99, column: 62, scope: !2025, inlinedAt: !2775)
!2777 = !DILocation(line: 100, column: 26, scope: !2025, inlinedAt: !2775)
!2778 = !DILocation(line: 446, column: 14, scope: !2038, inlinedAt: !2779)
!2779 = distinct !DILocation(line: 101, column: 18, scope: !2025, inlinedAt: !2775)
!2780 = !DILocation(line: 446, column: 27, scope: !2038, inlinedAt: !2779)
!2781 = !DILocation(line: 446, column: 39, scope: !2038, inlinedAt: !2779)
!2782 = !DILocation(line: 420, column: 24, scope: !2049, inlinedAt: !2783)
!2783 = distinct !DILocation(line: 454, column: 15, scope: !2038, inlinedAt: !2779)
!2784 = !DILocation(line: 420, column: 37, scope: !2049, inlinedAt: !2783)
!2785 = !DILocation(line: 420, column: 49, scope: !2049, inlinedAt: !2783)
!2786 = !DILocation(line: 375, column: 23, scope: !2063, inlinedAt: !2787)
!2787 = distinct !DILocation(line: 422, column: 18, scope: !2049, inlinedAt: !2783)
!2788 = !DILocation(line: 375, column: 36, scope: !2063, inlinedAt: !2787)
!2789 = !DILocation(line: 375, column: 48, scope: !2063, inlinedAt: !2787)
!2790 = !DILocation(line: 380, column: 18, scope: !2063, inlinedAt: !2787)
!2791 = !DILocation(line: 357, column: 29, scope: !2074, inlinedAt: !2792)
!2792 = distinct !DILocation(line: 385, column: 14, scope: !2063, inlinedAt: !2787)
!2793 = !DILocation(line: 357, column: 49, scope: !2074, inlinedAt: !2792)
!2794 = !DILocation(line: 357, column: 62, scope: !2074, inlinedAt: !2792)
!2795 = !DILocation(line: 366, column: 34, scope: !2074, inlinedAt: !2792)
!2796 = !DILocation(line: 366, column: 20, scope: !2074, inlinedAt: !2792)
!2797 = !DILocation(line: 367, column: 8, scope: !2102, inlinedAt: !2792)
!2798 = !DILocation(line: 367, column: 8, scope: !2074, inlinedAt: !2792)
!2799 = !DILocation(line: 368, column: 6, scope: !2102, inlinedAt: !2792)
!2800 = !DILocation(line: 369, column: 20, scope: !2074, inlinedAt: !2792)
!2801 = !DILocation(line: 429, column: 4, scope: !2749)
!2802 = !DILocation(line: 433, column: 40, scope: !2749)
!2803 = !DILocation(line: 305, column: 55, scope: !1982, inlinedAt: !2804)
!2804 = distinct !DILocation(line: 432, column: 8, scope: !2749)
!2805 = !DILocation(line: 306, column: 27, scope: !1982, inlinedAt: !2804)
!2806 = !DILocation(line: 307, column: 29, scope: !1982, inlinedAt: !2804)
!2807 = !DILocation(line: 287, column: 43, scope: !1997, inlinedAt: !2808)
!2808 = distinct !DILocation(line: 310, column: 14, scope: !1982, inlinedAt: !2804)
!2809 = !DILocation(line: 287, column: 67, scope: !1997, inlinedAt: !2808)
!2810 = !DILocation(line: 288, column: 24, scope: !1997, inlinedAt: !2808)
!2811 = !DILocation(line: 115, column: 39, scope: !2011, inlinedAt: !2812)
!2812 = distinct !DILocation(line: 289, column: 14, scope: !1997, inlinedAt: !2808)
!2813 = !DILocation(line: 115, column: 63, scope: !2011, inlinedAt: !2812)
!2814 = !DILocation(line: 116, column: 27, scope: !2011, inlinedAt: !2812)
!2815 = !DILocation(line: 128, column: 18, scope: !2011, inlinedAt: !2812)
!2816 = !DILocation(line: 99, column: 38, scope: !2025, inlinedAt: !2817)
!2817 = distinct !DILocation(line: 131, column: 14, scope: !2011, inlinedAt: !2812)
!2818 = !DILocation(line: 99, column: 62, scope: !2025, inlinedAt: !2817)
!2819 = !DILocation(line: 100, column: 26, scope: !2025, inlinedAt: !2817)
!2820 = !DILocation(line: 446, column: 14, scope: !2038, inlinedAt: !2821)
!2821 = distinct !DILocation(line: 101, column: 18, scope: !2025, inlinedAt: !2817)
!2822 = !DILocation(line: 446, column: 27, scope: !2038, inlinedAt: !2821)
!2823 = !DILocation(line: 446, column: 39, scope: !2038, inlinedAt: !2821)
!2824 = !DILocation(line: 420, column: 24, scope: !2049, inlinedAt: !2825)
!2825 = distinct !DILocation(line: 454, column: 15, scope: !2038, inlinedAt: !2821)
!2826 = !DILocation(line: 420, column: 37, scope: !2049, inlinedAt: !2825)
!2827 = !DILocation(line: 420, column: 49, scope: !2049, inlinedAt: !2825)
!2828 = !DILocation(line: 375, column: 23, scope: !2063, inlinedAt: !2829)
!2829 = distinct !DILocation(line: 422, column: 18, scope: !2049, inlinedAt: !2825)
!2830 = !DILocation(line: 375, column: 36, scope: !2063, inlinedAt: !2829)
!2831 = !DILocation(line: 375, column: 48, scope: !2063, inlinedAt: !2829)
!2832 = !DILocation(line: 380, column: 18, scope: !2063, inlinedAt: !2829)
!2833 = !DILocation(line: 357, column: 29, scope: !2074, inlinedAt: !2834)
!2834 = distinct !DILocation(line: 385, column: 14, scope: !2063, inlinedAt: !2829)
!2835 = !DILocation(line: 357, column: 49, scope: !2074, inlinedAt: !2834)
!2836 = !DILocation(line: 357, column: 62, scope: !2074, inlinedAt: !2834)
!2837 = !DILocation(line: 366, column: 34, scope: !2074, inlinedAt: !2834)
!2838 = !DILocation(line: 366, column: 20, scope: !2074, inlinedAt: !2834)
!2839 = !DILocation(line: 367, column: 8, scope: !2102, inlinedAt: !2834)
!2840 = !DILocation(line: 367, column: 8, scope: !2074, inlinedAt: !2834)
!2841 = !DILocation(line: 368, column: 6, scope: !2102, inlinedAt: !2834)
!2842 = !DILocation(line: 369, column: 20, scope: !2074, inlinedAt: !2834)
!2843 = !DILocation(line: 449, column: 21, scope: !2654)
!2844 = !DILocation(line: 0, scope: !1697, inlinedAt: !2845)
!2845 = distinct !DILocation(line: 448, column: 7, scope: !2654)
!2846 = !DILocation(line: 176, column: 29, scope: !1697, inlinedAt: !2845)
!2847 = !DILocation(line: 179, column: 6, scope: !1705, inlinedAt: !2845)
!2848 = !DILocation(line: 179, column: 6, scope: !1697, inlinedAt: !2845)
!2849 = !DILocation(line: 461, column: 47, scope: !1708, inlinedAt: !2850)
!2850 = distinct !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2845)
!2851 = !DILocation(line: 116, column: 26, scope: !1715, inlinedAt: !2852)
!2852 = distinct !DILocation(line: 462, column: 13, scope: !1708, inlinedAt: !2850)
!2853 = !DILocation(line: 125, column: 20, scope: !1715, inlinedAt: !2852)
!2854 = !DILocation(line: 125, column: 2, scope: !1715, inlinedAt: !2852)
!2855 = !DILocation(line: 180, column: 4, scope: !1705, inlinedAt: !2845)
!2856 = !DILocation(line: 451, column: 30, scope: !2654)
!2857 = !DILocation(line: 452, column: 31, scope: !2654)
!2858 = !DILocation(line: 453, column: 53, scope: !2654)
!2859 = !DILocation(line: 453, column: 39, scope: !2654)
!2860 = !DILocation(line: 454, column: 5, scope: !2654)
!2861 = distinct !DISubprogram(linkageName: "_GLOBAL__sub_I_merge.cpp", scope: !20, file: !20, type: !2862, isLocal: true, isDefinition: true, flags: DIFlagArtificial, isOptimized: true, unit: !19, variables: !25)
!2862 = !DISubroutineType(types: !25)
!2863 = !DILocation(line: 74, column: 25, scope: !2864, inlinedAt: !2865)
!2864 = distinct !DISubprogram(name: "__cxx_global_var_init", scope: !3, file: !3, line: 74, type: !1144, isLocal: true, isDefinition: true, scopeLine: 74, flags: DIFlagPrototyped, isOptimized: true, unit: !19, variables: !25)
!2865 = distinct !DILocation(line: 0, scope: !2861)
