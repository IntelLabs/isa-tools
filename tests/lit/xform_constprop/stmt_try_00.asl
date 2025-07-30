// RUN: %asli --batchmode --exec=:xform_lower --exec=:xform_constprop --exec=":show --format=raw FUT" %s | filecheck --check-prefix=XFORM %s

// Copyright (C) 2025-2025 Intel Corporation

func FUT?() => integer
begin
    let y = 0;
    var z = 1;
    try
        z = z + y;
    catch
    end
    return y;
end

// XFORM:       func FUT.0?{}() => integer
// XFORM:       begin
// XFORM-NEXT:      let y : integer{0} = 0;
// XFORM-NEXT:      var z : integer = 1;
// XFORM-NEXT:      try
// XFORM-NEXT:          z = 1;
// XFORM-NEXT:      catch
// XFORM:           end
// XFORM-NEXT:      return 0;
// XFORM-NEXT:  end

func main() => integer
begin
    print_int_dec(FUT?());
    return 0;
end
