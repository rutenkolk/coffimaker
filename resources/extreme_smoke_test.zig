pub const __zigclj_fn_param_names_CLITERAL = .{"v"};
pub fn CLITERAL(v: anytype) switch (@TypeOf(v)) {
    else => @compileError("should be ignored"),
} {}

pub const __zigclj_fn_param_names_genericArgRet = .{"v"};
pub fn genericArgRet(v: anytype) @TypeOf(v) {}

pub const __zigclj_fn_param_names_genericArg = .{"_"};
pub fn genericArg(_: anytype) void {}

pub const __zigclj_fn_param_names_knownVoid = .{ "_", "_" };
//old zig (before https://github.com/ziglang/zig/pull/22547): The `@TypeOf` here are resolved to void.
//pub fn knownVoid(v: void, w: @TypeOf(v)) @TypeOf(w) {}
//new zig: arguments and results with @TypeOf are treated as generic.
pub fn knownVoid(_: void, _: void) void {}

pub const __zigclj_fn_param_names_genericRet = .{"v"};
pub fn genericRet(v: void) @TypeOf(v) {}

pub const EmptyStruct = extern struct {};
pub const EmptyEnum = enum {};
pub const EmptyUnionBare = extern union {};
pub const FooBarStruct = extern struct { foo: void, bar: u8 };
//pub const FooBarEnum = enum {foo, bar};
pub const FooBarUnionBare = extern union { foo: void, bar: u8 };
pub const Anyopaque = anyopaque;
pub const SomeOpaque = opaque {};
pub const VoidAlias = void;

pub const EmptyVoidArray = [0]void;
pub const some_u16_value: u16 = 888;
pub const some_u8_value: u8 = 1 << 7;
pub var some_i8_var: i8 = (-1) << 7;
pub extern const extern_u8_const: u8;
pub extern var extern_i8_var: i8;

comptime {
    // old zig (before ziglang/zig/pull/21206): @export takes lvalue expression
    //@export(some_u8_value, .{ .name = "extern_u8_const" });
    //@export(some_i8_var, .{ .name = "extern_i8_var" });
    // new zig: @export takes pointer
    @export(&some_u8_value, .{ .name = "extern_u8_const" });
    @export(&some_i8_var, .{ .name = "extern_i8_var" });
}

pub const empty_struct = EmptyStruct{};
pub const foo_bar_struct = FooBarStruct{ .foo = {}, .bar = 2 };
pub const ad_hoc_struct = .{ .foo = {}, .bar = 2 };

pub const array = [2]*const u8{ &some_u8_value, &extern_u8_const };
pub const string = "abc";
pub const null_pointer: ?*void = null;
pub const null_pointer_pointer = &null_pointer;
pub const pointer_that_is_null_illegally: *void = @as(*const *void, @ptrCast(null_pointer_pointer)).*;
pub const u8_pointer = &some_u8_value;
pub const u16_pointer = &some_u16_value;

pub const @"f16": f16 = 0.3;
pub const void_value = {};
pub const optional_void_value_null: ?void = null;
pub const optional_void_value_present: ?void = {};
pub const empty_void_array: EmptyVoidArray = .{};
pub const empty_void_array_pointer: *const EmptyVoidArray = &empty_void_array;

pub const optional_anyopaque_pointer: ?*const anyopaque = @ptrFromInt(0x800);
pub const optional_function_pointer: ?*const fn (void, void) void = &knownVoid;
pub const optional_u8_pointer: ?*const u8 = &some_u8_value;

pub const @"NameWithDots.A.B" = struct {};
pub const @"name_with_dots.A.B": @"NameWithDots.A.B" = .{};
